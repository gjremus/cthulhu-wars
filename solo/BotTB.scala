package cws

import hrf.colmat._

// ============================================================================
// THE BURROWERS BENEATH (TB) BOT
// Mirrors the BotXSS structure (object extends BotX, class extends GameEvaluation).
// Scoring priorities (working defaults -- tune via simulation):
//  (1) Tentacle spread for Power generation / Ensnare coverage.
//  (2) Gate construction via Cadavolytes (Tentacles cannot control gates).
//  (3) Shudde M'ell awaken timing (cost 8, Mantle must be in play).
//  (4) Autotomy Elder Sign farming via Unlimited Battles.
//  (5) Overlay the Mantle (SBR-1) as early as possible for mobility.
//  (6) 2026-11-01: creator-specified early-game Tentacle-rush weighting (10-
//      Tentacle SBR-2, Thousand Writhing Maws type/placement, Setup glyph
//      priority, Stalk tentacle-spread, and a Spellbook pick order -- Stalk
//      first, Psychic Shriek second) -- see TB Current Tasks doc. SUPERSEDED
//      by (9) below: the first Spellbook pick is now Subterrane, not Stalk.
//  (7) 2026-11-02: creator-specified late-game sequencing from the same
//      instructions -- Subterrane pick once the Mantle is placed, Ensnare
//      pick once Shudde M'ell is awakened, moving Head/Segments onto the
//      three Glyph Areas and paying for the alt Three-Glyphs SBR once 4 of
//      the other 5 SBRs are fulfilled, Grasp pick once Three-Glyphs is
//      satisfied, the Gates-at-every-GOO SBR once 5 Spellbooks + 8 Power,
//      Psychic Shriek once all Spellbooks earned and an enemy exceeds 6
//      Power, and Thousand Writhing Maws raw-count thresholds (8 or fewer
//      Tentacles / 2 or fewer Cadavolytes) alongside the existing SBR gates.
//  (8) 2026-11-05: creator follow-up -- Gates-at-every-GOO-Area's baseline
//      (non-aggressive) score now stays deprioritized until 4 Spellbooks are
//      earned (was only until Overlay the Mantle, 1 Spellbook), so it can no
//      longer compete with the early-game opening in the 1-3 Spellbook window.
//  (9) 2026-11-07: creator reversal of (6)'s Spellbook order -- Subterrane
//      must be the first Spellbook picked (it's earned from placing 2 Gates
//      and placing the Mantle), not Stalk. Also: Remove-Gate-Place-Chthonian
//      (SBR-3) is now withheld from TB's menu entirely (not just deprioritized)
//      until Shudde M'ell has been awakened at least once -- a -1000 eval
//      score is not an absolute block under BotX's lexicographic compare, and
//      live play (Death for Rupture, game 946) proved it could still win.
//  (10) 2026-11-07 (later same day): creator reversal of (9)'s order again --
//      Psychic Shriek, not Subterrane, must be the very first Spellbook
//      picked. New post-Mantle sequencing: while Shudde M'ell is unawakened
//      and TB holds 10+ Power, spam Psychic Shriek (targeting an enemy with
//      more than 4 Power) at top priority instead of awakening immediately;
//      once Power drops to 9, awakening Shudde M'ell (already top priority
//      below) takes over and the next Spellbook picked should be (only)
//      Stalk. After Shudde M'ell is awakened, whenever TB is at 0 Power,
//      Remove-Gate-Place-Chthonian (to bank 2 Power) becomes top priority,
//      the next Spellbook picked should be (only) Subterrane, and that
//      banked Power should go toward a Thousand Writhing Maws double
//      Tentacle recruit.
//  (11) 2026-11-08: creator report (game "Torment through Tentacles") --
//      sequencing from (10) still didn't fire correctly. Two bugs found:
//      (a) the Psychic Shriek window's own "TB holds 10+ Power" gate meant
//      it could never fire in a game where TB's power never climbed above
//      9 pre-awaken (confirmed: it never fired this game at all) -- removed
//      that self-power requirement, matching the creator's latest wording
//      ("as soon as action phase 2 starts... a faction who has more than 3
//      power", no TB-side power threshold). (b) Stalk's and Subterrane's
//      pick-order bonuses were keyed on live game state (Mantle/Awakened/
//      Power) that both become simultaneously true right after Awaken
//      (TB's Power often lands at exactly 0 the instant Awaken's 8-Power
//      cost is paid), so Subterrane's bonus could fire ahead of Stalk's
//      intended second-pick slot -- reworked both to key on
//      self.spellbooks.num (the count of Spellbooks already picked) so
//      Stalk is unambiguously the 2nd pick and Subterrane the 3rd,
//      independent of the Power/Awaken race.
// ============================================================================
object BotTB extends BotX(implicit g => new GameEvaluationTB)

class GameEvaluationTB(implicit game : Game) extends GameEvaluation(TB)(game) {
    def eval(a : Action) : $[Evaluation] = {
        var result : $[Evaluation] = $

        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        if (game.battle.none) {
            // Creator priority (2026-11-02): count of the 6 Spellbook Requirements,
            // excluding the Three-Glyphs one itself, already satisfied -- used to
            // time the alt-payment / glyph-placement push for Three Glyphs + Grasp.
            val sbReqsDoneExclGlyphs = $(OverlayMantleReq, TenTentaclesReq, RemoveGatePlaceChthonianReq,
                GatesAtGOOsReq, AwakenShuddeMellReq).%(req => !self.needs(req)).num

            // Creator priority (2026-11-02): TB should never take a single, plain
            // Recruit/Summon when Thousand Writhing Maws' double recruit/summon is
            // legal this turn -- same eligibility check the game itself uses to offer
            // the action (2+ Power, an eligible up-to-2-cost pool type available).
            val twmAvailable = self.power >= 2 && TBExpansion.tbMawsEligibleTypes(self).any

            // Creator priority (2026-11-02, "different opening" trial): the opening
            // Cadavolyte pair should land as far as possible from any enemy-occupied
            // Area -- used by the TWM Cadavolyte-opening scoring below.
            // Creator fix (2026-11-06): this used to exclude r itself from the enemy
            // list before measuring distance, so an Area that was itself enemy-occupied
            // got scored as if no enemy were anywhere nearby (falling through to the
            // 99 default) instead of distance 0 -- making the bot treat an enemy's own
            // Area as the single safest placement. Now r's own enemy occupancy counts.
            val enemyRegions = game.board.regions.%(_.foes.any)
            def distFromEnemies(r : Region) : Int = enemyRegions./(e => game.board.distance(r, e)).minOr(99)

            a match {

                case FirstPlayerAction(_, f) =>
                    f == self && allSB                          |=> 100  -> "play first all SB"
                    f == self && others.%(ofinale).any           |=> 200  -> "play first someone near win"
                    f == self                                    |=> -50  -> "stall"

                case PlayDirectionAction(_, order) =>
                    (order == game.factions)                     |=> 20   -> "natural order"

                // Spellbook ability pick order: Psychic Shriek 1st, Stalk 2nd,
                // Subterrane 3rd (see note (10)/(11) above for history). 2026-11-08
                // fix: Stalk's and Subterrane's exclusive-pick bonuses used to key on
                // live game state (Mantle placed / Shudde M'ell awakened / Power == 0)
                // instead of pick order directly -- but Awaken can make all three of
                // those conditions flip true in the same instant (Power often lands at
                // exactly 0 right when Awaken's 8-Power cost is paid), so Subterrane's
                // bonus could outscore Stalk's intended 2nd-pick slot. Now both key on
                // self.spellbooks.num (how many Spellbooks already picked) instead,
                // which is unambiguous and independent of the Power/Awaken race. Both
                // windows out-rank every flat weight below so they are exclusive
                // within their trigger condition.
                case SpellbookAction(_, sb, _) => sb match {
                    // 2026-11-07 fix: the very first Spellbook decision offers all 6
                    // unclaimed library books at once, and Stalk's conditional 2400
                    // (mantle just placed, pre-awaken, below the shriek-spam threshold)
                    // was scoring above this flat 1900 -- so the bot kept picking Stalk
                    // first exactly as the creator reported (live in "Gate for Time").
                    // 2600 safely exceeds every other book's ceiling (Stalk's 2400,
                    // Subterrane's 2500) only on the first pick; PsychicShriek is
                    // removed from the offered set as soon as it's chosen, so this
                    // branch only ever applies once.
                    case PsychicShriek =>
                        self.spellbooks.num == 0 |=> 2600 -> "pick psychic shriek first (guaranteed first pick, creator priority 2026-11-07)"
                        true |=> 1900 -> "pick psychic shriek"
                    case Stalk =>
                        (game.tbMantleInPlay && self.spellbooks.num == 1) |=> 2400 -> "pick stalk (second spellbook pick, right after psychic shriek, creator priority 2026-11-08)"
                        true |=> 1800 -> "pick stalk"
                    case Subterrane =>
                        (self.spellbooks.num >= 2 && game.tbShuddeMellEverAwakened && self.power == 0) |=> 2500 -> "pick subterrane (third spellbook pick, 0 power after remove-gate-place-chthonian banking, creator priority 2026-11-08)"
                        game.tbMantleInPlay |=> 2200 -> "pick subterrane (mantle placed, creator priority)"
                        true |=> 1000 -> "pick subterrane"
                    case Autotomy =>
                        true |=> 1200 -> "pick autotomy"
                    case Ensnare =>
                        game.tbShuddeMellEverAwakened |=> 2000 -> "pick ensnare (shudde m'ell awakened, creator priority)"
                        true |=> 900  -> "pick ensnare"
                    case Grasp =>
                        !self.needs(ShuddeMellInThreeGlyphsReq) |=> 2100 -> "pick grasp (three-glyphs SBR fulfilled, creator priority)"
                        true |=> 300  -> "pick grasp"
                    case _ =>
                        true |=> -1000 -> "unknown spellbook"
                }

                case DoomDoneAction(f) =>
                    f == self                                    |=> 0    -> "done doom"

                // Setup: 8 starting Tentacles. Creator-specified priority (2026-11-01):
                // Glyph Areas in order /^\ (AA) then (*) (OO) then ||| (WW) that aren't
                // any playing faction's Start Area, farthest from any Start Area first;
                // once those run out, cluster adjacent to Tentacles already placed this
                // Setup; any leftover Tentacles go on a playing faction's Start Area,
                // but never on Great Cthulhu's Start Area.
                case TBSetupPlaceTentacleInAction(_, r, _) =>
                    val isGlyphArea = r.glyph == GlyphAA || r.glyph == GlyphOO || r.glyph == GlyphWW
                    val startRegions = game.factions./~(fac => game.starting.get(fac))
                    val isAnyStart = startRegions.has(r)
                    val isGCStart = game.starting.get(GC).contains(r)
                    val farFromStart = startRegions./(sr => game.board.distance(r, sr)).minOr(0)
                    val adjacentToPlaced = self.all(Tentacle)./(_.region).exists(_.connected.has(r))
                    r.glyph == GlyphAA && !isAnyStart               |=> (1300 + farFromStart) -> "setup: /^\\ glyph area, not a start area"
                    r.glyph == GlyphOO && !isAnyStart               |=> (1200 + farFromStart) -> "setup: (*) glyph area, not a start area"
                    r.glyph == GlyphWW && !isAnyStart               |=> (1100 + farFromStart) -> "setup: ||| glyph area, not a start area"
                    !isGlyphArea && adjacentToPlaced && !isAnyStart |=> (500 + farFromStart)  -> "setup: cluster near placed tentacles"
                    isAnyStart && !isGCStart                        |=> 100 -> "setup: leftover tentacle on faction start area"
                    isGCStart                                       |=> -500 -> "setup: avoid Great Cthulhu's start area"

                case MoveAction(f, u, from, to, _) =>
                    u.cultist && to.freeGate                     |=> 1600 -> "cultist to free gate"
                    u.cultist && to.ownGate                      |=> 200  -> "cultist to own gate"
                    u.monster && to.foes.any && self.power > 3   |=> 500  -> "chthonian toward enemies"
                    // 2026-11-08 creator priority: once Shudde M'ell is awakened, if TB
                    // still controls a Gate on the main map (not the Mantle), move the
                    // Head there to protect it. Outranks every other Move option so it
                    // is not overridden by the "shudde mell toward enemies" line below.
                    // 2026-11-08 (2nd fix same day): dropped the "exactly 1 Power" gate
                    // -- restricting this to one exact Power value meant the rule almost
                    // never fired, and the Head instead followed the generic "toward
                    // enemies" 700 score into whatever Area an enemy happened to be,
                    // leaving TB's own Gate to fend for itself (creator report, game
                    // "undefined against colour", rowId 969).
                    u.is(ShuddeMellHead) && game.tbShuddeMellEverAwakened &&
                        self.gates.has(to) && to != TB.mantle    |=> 3000 -> "move shudde m'ell to protect own gate (creator priority 2026-11-08)"
                    u.goo && to.foes.any                         |=> 700  -> "shudde mell toward enemies"
                    (u.goo || u.is(ShuddeMellSegment)) && numSB < 5 &&
                        (to.glyph == GlyphAA || to.glyph == GlyphOO || to.glyph == GlyphWW) |=> -1000 -> "move part to glyph area (three-glyphs SBR deprioritized below 5 spellbooks, creator priority)"
                    (u.goo || u.is(ShuddeMellSegment)) && numSB >= 5 && sbReqsDoneExclGlyphs >= 4 && self.all(ShuddeMellSegment).num >= 2 &&
                        (to.glyph == GlyphAA || to.glyph == GlyphOO || to.glyph == GlyphWW) |=> 1900 -> "move part to glyph area (three-glyphs SBR, creator priority)"
                    to.freeGate                                  |=> 400  -> "move to free gate"

                // Creator fix (2026-11-02): recruiting a Tentacle had no Area-spread
                // preference at all, unlike Thousand Writhing Maws' double-recruit (which
                // already favors an empty Area). Standard Recruit legality naturally offers
                // Areas TB already occupies first, so with no counter-pressure here the bot
                // kept topping up Tentacles into the same already-held Areas instead of
                // spreading -- matching the creator's "never more than 1 tentacle in an
                // area if it can be avoided" report, and the resulting narrow Tentacle
                // footprint starved the bot's Power income (which scales with the number of
                // distinct Tentacle-occupied Areas) for the rest of the game.
                // Creator priority (2026-11-02): all plain-Recruit scoring below is
                // gated on Thousand Writhing Maws NOT being available this turn -- when
                // TWM is legal, TB should always use it instead of a single Recruit.
                case RecruitAction(f, uc, r) =>
                    uc == Tentacle && self.needs(TenTentaclesReq) && self.at(r, Tentacle).none && !twmAvailable |=> 680 -> "recruit tentacle toward 10-tentacle SBR (empty area, creator priority: spread tentacles)"
                    uc == Tentacle && self.needs(TenTentaclesReq) && !twmAvailable |=> 650  -> "recruit tentacle toward 10-tentacle SBR"
                    uc == Cadavolyte && r.freeGate               && !twmAvailable |=> 700  -> "cadavolyte to free gate"
                    uc == Cadavolyte                             && !twmAvailable |=> 300  -> "recruit cadavolyte"
                    uc == Tentacle && self.at(r, Tentacle).none  && !twmAvailable |=> 180  -> "recruit tentacle (empty area, creator priority: spread tentacles)"
                    uc == Tentacle                               && !twmAvailable |=> 150  -> "recruit tentacle"

                // Creator priority (2026-11-02): same TWM-always-preferred rule as Recruit.
                case SummonAction(f, uc, r) =>
                    uc == Chthonian                              && !twmAvailable |=> 400  -> "summon chthonian"

                case BuildGateAction(f, r) =>
                    self.gates.num < 3                           |=> 900  -> "need more gates"
                    self.doom < 10                               |=> 500  -> "build gate for doom"

                case AttackAction(f, r, e, _) =>
                    self.strength(self.at(r), e) > e.strength(e.at(r), self) |=> 1200 -> "favorable battle"
                    e.at(r).goos.any                             |=> 700  -> "attack enemy goo"
                    e.at(r).num >= 2                             |=> 400  -> "attack cluster"

                // Thousand Writhing Maws: double recruit/summon. Creator-specified
                // priority (2026-11-01): recruit 2 Tentacles while short of the
                // 10-Tentacle SBR (SBR-2), placing them on a Tentacle area adjacent
                // to an empty area; once SBR-2 is satisfied, switch to recruiting 2
                // Cadavolytes instead.
                // Creator "different opening" trial (2026-11-02): before the Mantle
                // is overlaid, try Cadavolytes first instead -- two Cadavolytes placed
                // in two Areas adjacent to each other, aimed at the two adjacent Gates
                // Overlay Mantle (SBR-1) needs. Ranked above the Tentacle-SBR line below
                // only until Overlay Mantle is satisfied; the Tentacle rush resumes after.
                case TBWrithingMawsMainAction(f) =>
                    true                                         |=> 600  -> "writhing maws double summon"
                case TBWrithingMawsTypeAction(f, uc) =>
                    // 2026-11-07 creator priority: the 2 Power just banked via Remove-
                    // Gate-Place-Chthonian (at exactly 2 Power, post-awaken) should go
                    // toward a Tentacle double-recruit, matching the Chthonian-swap ->
                    // Subterrane -> Tentacle-recruit sequence requested.
                    // 2026-11-08: creator widened this -- post-awaken, Tentacles should
                    // outrank Cadavolytes for ANY remaining Power (not just exactly the
                    // 2 Power just banked), per "with gained 2 power, or remaining power,
                    // thousand writhing maws recruit tentacles > cadavolytes".
                    uc == Tentacle && game.tbShuddeMellEverAwakened && self.power >= 2  |=> 1000 -> "maws tentacles (post-awaken, any remaining power, creator priority 2026-11-08)"
                    uc == Cadavolyte && self.needs(OverlayMantleReq) |=> 950 -> "maws cadavolytes (creator: new opening, pair toward Overlay Mantle SBR)"
                    uc == Tentacle && self.needs(TenTentaclesReq)    |=> 900  -> "maws tentacles (toward 10-tentacle SBR)"
                    uc == Tentacle && self.all(Tentacle).num <= 8     |=> 850  -> "maws tentacles (creator: 8 or fewer tentacles)"
                    uc == Cadavolyte && !self.needs(TenTentaclesReq) |=> 800  -> "maws cadavolytes (post 10-tentacle SBR)"
                    uc == Cadavolyte && self.all(Cadavolyte).num <= 2 |=> 750 -> "maws cadavolytes (creator: 2 or fewer cadavolytes)"
                    uc == Chthonian                                  |=> 500  -> "maws chthonians"
                    uc == Cadavolyte                                 |=> 400  -> "maws cadavolytes"
                    uc == Tentacle                                   |=> 200  -> "maws tentacles"
                case TBWrithingMawsPlaceFirstAction(f, _) =>
                    true                                         |=> 100  -> "maws place first"
                case TBWrithingMawsPlaceSecondAction(f, uc, r1) =>
                    uc == Tentacle && self.at(r1, Tentacle).any && r1.connected.exists(c => self.at(c, Tentacle).none) |=> 400 -> "maws: tentacle area next to an empty area"
                    uc == Cadavolyte && self.needs(OverlayMantleReq) && r1.connected.any |=> (350 + distFromEnemies(r1)) -> "maws: cadavolyte area with a neighbor available, far from enemies (creator: new opening, pair for adjacent gates away from enemies)"
                    true                                         |=> 100  -> "maws place second"
                case TBWrithingMawsAction(f, uc, r1, r2) =>
                    uc == Tentacle && self.at(r2, Tentacle).any && r2.connected.exists(c => self.at(c, Tentacle).none) |=> 400 -> "maws: tentacle area next to an empty area"
                    uc == Cadavolyte && self.needs(OverlayMantleReq) && r1.connected.has(r2) |=> (500 + distFromEnemies(r2)) -> "maws: two cadavolytes in adjacent areas, far from enemies (creator: new opening, pair toward Overlay Mantle SBR away from enemies)"
                    true                                         |=> 300  -> "maws commit"

                // Abyssal Cline: Move Part to Mantle
                case TBMovePartToMantleMainAction(f) =>
                    true                                         |=> 400  -> "behemoth move part to mantle"
                case TBMovePartToMantlePickAction(f, _) =>
                    true                                         |=> 100  -> "pick part for mantle"
                case TBMovePartToMantleAction(f, _) =>
                    true                                         |=> 200  -> "move part commit"

                // Stalk: post-move relocation. Creator-specified priority (2026-11-01):
                // send a Tentacle from an area holding 2+ Tentacles to any area with
                // no Tentacles, whenever that choice is available.
                case TBStalkMainAction(f, _, _) =>
                    true                                         |=> 300  -> "stalk relocation"
                case TBStalkUseAction(f, _, _) =>
                    true                                         |=> 400  -> "use stalk"
                case TBStalkSkipAction(f, _) =>
                    true                                         |=> 0    -> "skip stalk"
                case TBStalkPickCultistAction(f, _, _) =>
                    true                                         |=> 100  -> "pick stalk cultist"
                case TBStalkDestAction(f, cultist, _, _) =>
                    cultist.uclass == Tentacle && self.at(game.unit(cultist).region, Tentacle).num >= 2 |=> 400 -> "stalk: pick tentacle from a crowded area"
                    true                                         |=> 100  -> "pick stalk dest"
                case TBStalkAction(f, cultist, dest, _) =>
                    cultist.uclass == Tentacle && self.at(dest, Tentacle).none |=> 400 -> "stalk: tentacle to an empty area"
                    // 2026-11-08 creator priority: Cadavolytes must not abandon a Gate
                    // they're holding -- the scoring here (TBStalkAction, not the Soft
                    // TBStalkPickCultistAction/TBStalkDestAction above) is the real leaf
                    // that competes against TBStalkSkipAction's 0, per the same
                    // Soft-is-dead-code lesson as the Psychic Shriek fix -- so a
                    // gate-holding cultist's relocation must score below Skip's 0,
                    // not just below the other Stalk options (creator report: "why did
                    // the cadavolyte abandon the Mantle gate?", game "undefined against
                    // colour", rowId 969).
                    game.unit(cultist).onGate                    |=> -500 -> "stalk: avoid relocating a gate-holding cultist off its own gate (creator priority 2026-11-08)"
                    true                                         |=> 200  -> "stalk commit"

                // Autotomy: post-battle Kill transfer
                case TBAutotomyUseAction(f) =>
                    true                                         |=> 800  -> "use autotomy (ES farming)"
                case TBAutotomySkipAction(f) =>
                    true                                         |=> -200 -> "skip autotomy"
                case TBAutotomyPickSegmentAction(f, _) =>
                    true                                         |=> 100  -> "pick segment for autotomy"
                case TBAutotomyRetreatAction(f, _, _) =>
                    true                                         |=> 100  -> "pick autotomy retreat"
                case TBAutotomyRetreatExecuteAction(f, _, _) =>
                    true                                         |=> 100  -> "autotomy retreat commit"
                case TBAutotomyAction(f, _, _, _) =>
                    true                                         |=> 500  -> "autotomy commit"

                // Subterrane consent under enemy Madness: deny by default so the
                // enemy cannot gain the Mantle as an extra place to strand TB units.
                case TBSubterranePainUseAction(f) =>
                    true                                         |=> -200 -> "allow subterrane for enemy retreat"
                case TBSubterranePainSkipAction(f) =>
                    true                                         |=> 200  -> "deny subterrane for enemy retreat"

                // Ensnare
                case TBEnsnareMainAction(f) =>
                    true                                         |=> 600  -> "ensnare enemy"
                case TBEnsnarePickEnemyAction(f, _) =>
                    true                                         |=> 100  -> "pick ensnare target"
                case TBEnsnareTargetAction(f, _, _) =>
                    true                                         |=> 200  -> "ensnare target commit"

                // Psychic Shriek
                // 2026-11-08 (2nd fix same day): root cause found. TBPsychicShriekMainAction
                // is `Soft`, and BotX.askE scores only Explode.explode's output, which drops
                // every Soft action before eval() ever runs -- so the 5500/1700 priority
                // weights on the MainAction case below were dead code, never once compared
                // against AwakenAction's 5000. The actual leaf action that survives
                // explosion and competes against Awaken is TBPsychicShriekTargetAction, so
                // the creator's priority (mantle placed, pre-awaken, enemy over 3 power,
                // outranks even awakening Shudde M'ell) has to live there instead. Confirmed
                // via browser console trace capture on game "Continuum and Colour" (rowId
                // 963): turn=2, f.power=10, enemy Great Cthulhu at power=10 (>3), yet the
                // bot still awakened -- exactly consistent with the dead-code diagnosis.
                case TBPsychicShriekMainAction(f) =>
                    true                                         |=> 500  -> "psychic shriek"
                case TBPsychicShriekPickEnemyAction(f, _) =>
                    true                                         |=> 100  -> "pick shriek target"
                case TBPsychicShriekTargetAction(f, enemy) =>
                    game.tbMantleInPlay && !game.tbShuddeMellEverAwakened && enemy.power > 3 && (enemy == YS || enemy == CC) |=> 5550 -> "shriek target: mantle placed, pre-awaken, enemy over 3 power, preferred faction YS/CC (creator priority 2026-11-08)"
                    game.tbMantleInPlay && !game.tbShuddeMellEverAwakened && enemy.power > 3 |=> 5500 -> "shriek target: mantle placed, pre-awaken, enemy over 3 power (creator priority 2026-11-08)"
                    allSB && enemy.power > 6                     |=> 1700 -> "shriek target: all spellbooks earned, enemy power > 6 (creator priority)"
                    enemy.power > 3 && (enemy == YS || enemy == CC) |=> 450 -> "shriek target: enemy over 3 power, preferred faction YS/CC (creator priority 2026-11-07)"
                    enemy.power > 3                              |=> 400  -> "shriek target: enemy over 3 power (creator priority 2026-11-07)"
                    true                                         |=> 200  -> "shriek target commit"

                // SBR-1: Overlay the Mantle
                case TBOverlayMantleMainAction(f) =>
                    true                                         |=> 1000 -> "overlay mantle (critical SBR)"
                case TBOverlayMantlePickGatesAction(f, _) =>
                    true                                         |=> 100  -> "pick mantle gates"
                case TBOverlayMantleGatePairAction(f, _, _) =>
                    true                                         |=> 200  -> "mantle gate pair"
                case TBOverlayMantleAreasAction(f, _, _, _, _) =>
                    true                                         |=> 100  -> "mantle areas"
                case TBOverlayMantleAreaToggleAction(f, _, _, _, _, _) =>
                    true                                         |=> 150  -> "add mantle area"
                case TBOverlayMantleDoneAction(f, _, _, _) =>
                    true                                         |=> 300  -> "confirm mantle overlay"
                case TBOverlayMantlePickTransferAction(f, _, _, _) =>
                    true                                         |=> 100  -> "pick gate to transfer"
                case TBOverlayMantleTransferAction(f, _, _) =>
                    true                                         |=> 200  -> "transfer gate to mantle"

                // Awaken Shudde M'ell (Head): cost 8, Mantle-only. No prior case matched
                // this, so it fell through to the catch-all "unknown" -1000 penalty below,
                // meaning the bot almost never awakened it -- blocking Subterrane, Ensnare,
                // Grasp, Autotomy and every later Spellbook that depends on them.
                // 2026-11-06: creator priority -- once the Mantle is placed, awakening
                // Shudde M'ell must outrank every other action in the file (including
                // Remove-Gate-Place-Chthonian below), so the bot never again removes one
                // of the two adjacent Gates that Overlay-the-Mantle still needs before it
                // has a chance to awaken. 5000 safely exceeds every other weight here.
                case AwakenAction(f, ShuddeMellHead, _, _) =>
                    game.tbMantleInPlay                           |=> 5000 -> "awaken Shudde M'ell (mantle placed, creator top priority)"
                    !game.tbMantleInPlay && self.needs(AwakenShuddeMellReq) |=> 2500 -> "awaken Shudde M'ell for SBR"
                    !game.tbMantleInPlay                          |=> 1800 -> "awaken Shudde M'ell (unlocks engine)"

                // SBR-3: Remove Gate, Place Chthonian (end-of-Action-Phase prompt).
                // Creator-specified priority (2026-11-01): trigger this strongly when
                // an enemy threatens a Cadavolyte guarding one of TB's own Gates.
                // 2026-11-06: creator priority -- deprioritized to -1000 until Shudde
                // M'ell is awakened, so as not to break the 2-adjacent-Gates
                // precondition Overlay-the-Mantle still needed.
                // 2026-11-07: a -1000 eval score is not an absolute block under
                // BotX's lexicographic compare -- EndTurnAction (scored ~0) could
                // still lose to it if no OTHER candidate scored closer to 0, which is
                // exactly what happened live (Death for Rupture, game 946). The real
                // fix is withholding this action from the menu entirely before Shudde
                // M'ell is awakened (see the game.tbShuddeMellEverAwakened guards
                // added at every offer site in FactionTB.scala / Game.scala). The
                // -1000 case below is now unreachable in that window but is kept as
                // defense in depth in case any offer site is ever missed.
                case TBRemoveGatePlaceChthonianPromptAction(f, _) =>
                    !game.tbShuddeMellEverAwakened                                                                      |=> -1000 -> "remove gate place chthonian deprioritized until Shudde M'ell awakened, creator priority"
                    // 2026-11-07 creator priority: once Shudde M'ell is awakened, as
                    // soon as TB hits exactly 0 Power, banking 2 Power via this ability
                    // becomes the single highest priority in the file (it only ever
                    // grants Power, never an extra turn, per the 2026-11-07 fix above).
                    game.tbShuddeMellEverAwakened && self.power == 0                                                    |=> 6000 -> "remove gate place chthonian (0 power, banking toward subterrane/tentacles, creator priority 2026-11-07)"
                    game.tbShuddeMellEverAwakened && self.gates.num > 2                                                 |=> 400  -> "remove gate place chthonian"
                    game.tbShuddeMellEverAwakened && self.gates.exists(r => self.at(r, Cadavolyte).any && r.foes.any)   |=> 900  -> "remove gate place chthonian: cadavolyte-held gate under threat"
                case TBRemoveGatePlaceChthonianPickGateAction(f, _) =>
                    true                                         |=> 100  -> "pick gate to remove"
                case TBRemoveGatePlaceChthonianGateAction(f, _, _) =>
                    true                                         |=> 100  -> "gate removal commit"
                // 2026-11-07 fix: destination choice had no preference at all, so the
                // bot placed the freed Chthonian anywhere TB had a unit (e.g. an empty
                // overseas Area) instead of on TB's own remaining Gate, leaving that
                // Gate undefended -- matching the creator's "Gate for Time" report.
                case TBRemoveGatePlaceChthonianPickAreaAction(f, _, _) =>
                    true                                         |=> 100  -> "pick chthonian placement"
                case TBRemoveGatePlaceChthonianAreaAction(f, removedGate, dest, _) =>
                    self.gates.has(dest) && dest != removedGate && dest.foes.any |=> 700  -> "chthonian placement: own gate under threat, creator priority 2026-11-07"
                    self.gates.has(dest) && dest != removedGate                   |=> 500  -> "chthonian placement: own gate, creator priority 2026-11-07"
                    true                                                          |=> 200  -> "chthonian placement commit"

                // SBR-4: Gates at every GOO Area
                case TBGatesAtGOOsMainAction(f) =>
                    val aggressive = numSB >= 5 && self.power >= 8
                    aggressive                                   |=> 2300 -> "gates at goo areas SBR (5 spellbooks + 8 power, creator priority)"
                    !aggressive && numSB < 4                     |=> 150  -> "gates at goo areas SBR (deprioritized before 4 spellbooks earned, creator opening takes precedence)"
                    !aggressive && numSB >= 4                    |=> 700  -> "gates at goo areas SBR"
                case TBGatesAtGOOsAction(f) =>
                    val aggressive = numSB >= 5 && self.power >= 8
                    aggressive                                   |=> 1800 -> "gates at goo commit (creator priority)"
                    !aggressive && numSB < 4                     |=> 150  -> "gates at goo commit (deprioritized before 4 spellbooks earned)"
                    !aggressive && numSB >= 4                    |=> 500  -> "gates at goo commit"

                // SBR-6: Three Glyphs alt payment
                case TBThreeGlyphsPayAction(f) =>
                    numSB < 5                                    |=> -1000 -> "three glyphs pay 6 power deprioritized below 5 spellbooks, creator priority"
                    numSB >= 5 && sbReqsDoneExclGlyphs >= 4      |=> 2000 -> "three glyphs pay 6 power (4 SBRs fulfilled, creator priority)"
                    numSB >= 5                                   |=> 600  -> "three glyphs pay 6"
                case TBThreeGlyphsPayConfirmAction(f) =>
                    numSB < 5                                    |=> -1000 -> "three glyphs confirm deprioritized below 5 spellbooks, creator priority"
                    numSB >= 5                                   |=> 400  -> "three glyphs confirm"

                case EndTurnAction(f) =>
                    f == self                                    |=> 0    -> "end turn"

                case ControlGateAction(_, r, u, _) =>
                    val currentlyOnGate = self.at(r).%(_.onGate)
                    val isSwitch = currentlyOnGate.any && !currentlyOnGate.exists(_.ref == u)
                    !isSwitch                                    |=> 1000 -> "control empty gate"
                    isSwitch                                     |=> -1000000 -> "no-op swap"

                case AbandonGateAction(_, r, _) =>
                    true |=> -1000000 -> "never abandon gate"

                case _ =>
                    true |=> -1000 -> "unknown"
            }
        }
        else {
            implicit val battle = game.battle.get
            val allies  = self.forces
            val enemies = self.opponent.forces

            def elim(u : UnitFigure) {
                u.is(Acolyte)            |=> 800  -> "elim acolyte"
                u.is(Tentacle)           |=> 600  -> "elim tentacle (no gate control)"
                u.is(Cadavolyte)         |=> 200  -> "elim cadavolyte"
                u.is(Chthonian)          |=> -200 -> "dont elim chthonian"
                u.is(ShuddeMellSegment)  |=> 900  -> "elim segment (triggers autotomy)"
                u.goo                    |=> -500 -> "dont elim goo head"
            }

            def retreat(u : UnitFigure) {
                u.gateKeeper             |=> -1000 -> "dont retreat gate keeper"
                u.is(Acolyte)            |=> 800   -> "retreat acolyte"
                u.is(Tentacle)           |=> 700   -> "retreat tentacle"
                u.is(Cadavolyte)         |=> 600   -> "retreat cadavolyte"
                u.is(Chthonian)          |=> -200  -> "dont retreat chthonian"
                u.goo                    |=> -5000 -> "dont retreat goo"
            }

            a match {
                case AssignKillAction(_, _, _, u) => elim(u)
                case AssignPainAction(_, _, _, u) => retreat(u)

                case RetreatUnitAction(_, u, r) =>
                    r.ownGate           |=> 1000 -> "retreat to own gate"
                    r.freeGate          |=> 800  -> "retreat to free gate"
                    u.cultist && r.freeGate |=> 1500 -> "cultist retreats to free gate"

                case _ =>
                    true |=> -1000 -> "unknown"
            }
        }

        result.none |=> 0 -> "none"
        true |=> -((1 + math.random() * 4).round.toInt) -> "random"

        result.sortBy(v => -v.weight.abs)
    }
}
