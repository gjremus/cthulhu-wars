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

            // 2026-11-10 fix (creator report, game "Break through Undefined", rowId
            // 994): same window as the glyph-move bonus below (self already past
            // Remove-Gate-Place-Chthonian, or 4 of the other 5 SBRs done) -- while
            // Three-Glyphs is still unsatisfied and that window is open, the creator's
            // literal instruction is "don't move any other units than Shudde M'ell
            // units when targeting fulfilling the glyph requirement." The bot had
            // instead moved a Tentacle (action 629, toward "spread" scoring) and a
            // Chthonian (action 631, toward "toward enemies" scoring) in the very
            // same Action Phase as the glyph push, wasting Power that should have
            // gone entirely to Shudde M'ell parts.
            val glyphPushActive = self.needs(ShuddeMellInThreeGlyphsReq) &&
                (!self.needs(RemoveGatePlaceChthonianReq) || sbReqsDoneExclGlyphs >= 4)

            // 2026-11-13 fix (creator report, game "Failure against Colour", rowId
            // 1007, item 4 backup): "if TB has 2 parts on the same glyph, abyssal
            // cline to the mantle and move head or segments to a glyph that a head
            // or segment was not already in." The direct "move to still-needed
            // glyph" Move bonus above can never fire for a Part that is ALREADY
            // stuck sharing its glyph type with another Part (moving within the
            // same glyph type is explicitly excluded there as wasted Power), so
            // without this backup such a Part just sits put for the rest of the
            // glyph push. Detects any Head/Segment on a glyph Area whose glyph
            // type is also held by a DIFFERENT Head/Segment.
            val glyphDuplicateParts = (self.all(ShuddeMellHead) ++ self.all(ShuddeMellSegment))
                .%(p => p.region.glyph != NoGlyph &&
                    (self.all(ShuddeMellHead) ++ self.all(ShuddeMellSegment)).%(_.ref != p.ref).exists(_.region.glyph == p.region.glyph))
                ./(_.ref)

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

            // 2026-11-13 (task: 6-spellbook behavior spec, items 5/7): a "weakly
            // protected" enemy Great Old One or Terror is one sharing its Area with
            // at most 2 other enemy units (the creator's literal "2 or less enemy
            // protecting units") -- the target for the Abyssal Cline -> Ensnare/Move
            // -> Unlimited Battle -> Autotomy attack/defense chain scored below.
            // Item 5 (defense) additionally requires the Area already hold a TB
            // Tentacle (the defensive trigger); item 7 (offense) does not, since TB
            // has to move a Tentacle there first as part of the chain itself.
            def tbProtectingCount(u : UnitFigure) : Int =
                u.faction.at(u.region).num - u.faction.at(u.region).%(x => x.goo || x.terror).num
            def tbWeaklyProtected(u : UnitFigure) : Boolean =
                (u.goo || u.terror) && tbProtectingCount(u) <= 2
            val tbDefenseTargets = others./~(e => e.allInPlay.%(u => tbWeaklyProtected(u) && self.at(u.region, Tentacle).any))
            val tbOffenseTargets = others./~(e => e.allInPlay.%(tbWeaklyProtected))

            // 2026-11-13 (task: 6-spellbook behavior spec, item 12): "if on 6
            // spellbooks and 27 doom and more than 4 power left during action phase,
            // maximum priority on ensnare GOO battle autotomy loop and move tentacle
            // and shudde to GOO battle autotomy loop." The existing items 5/7 chain
            // scoring (1000-1950) only outranks the OTHER early/mid-game tiers in this
            // file -- it does not outrank Psychic Shriek's late-game 7000 (item 4) or
            // Awaken's 5000. This flag marks the literal late-game endgame-push window
            // the creator described, used below to boost the same chain actions to
            // the top of the file once it's active.
            val ensnareAutotomyLoopMaxPriority = numSB >= 6 && self.realDoom >= 27 && power > 4

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
                    // 2026-11-10 fix (creator report, game "Break through Undefined",
                    // rowId 994): "Dont move any other units than shudde mell units
                    // when targeting gaining [the glyph-area] spellbook requirement."
                    // While the glyph push is active (see glyphPushActive above), moving
                    // a Tentacle or Chthonian -- neither of which can ever satisfy
                    // Three-Glyphs -- just burns Power that should go entirely to Shudde
                    // M'ell's Head/Segments. Hard-block, same style/magnitude as the
                    // other large negative blocks in this file, so it outranks every
                    // positive score these units would otherwise get below (action 629:
                    // Tentacle moved to Arabia for no reason; action 631: Chthonian moved
                    // toward enemies for no reason, both during an active glyph push).
                    (u.is(Tentacle) || u.monster) && glyphPushActive |=> -2000 -> "avoid moving tentacle/chthonian while the three-glyphs push is active (creator priority 2026-11-10)"
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
                    // 2026-11-11 fix (creator report, game "Break through Lunacy", rowId
                    // 995): this used to fire for ANY destination Gate, even one the Head
                    // was already effectively guarding from -- so a Head already sitting
                    // on TB's own South America Gate got "rewarded" 3000 to relocate to
                    // TB's OTHER own Gate in North America (action 447), a pure
                    // wasted-Power lateral move since both Gates were already safe (no
                    // enemy present at either). Added a from-is-already-an-own-Gate
                    // exclusion so this only fires when the Head is actually leaving a
                    // non-Gate Area to go protect one.
                    u.is(ShuddeMellHead) && game.tbShuddeMellEverAwakened &&
                        self.gates.has(to) && to != TB.mantle &&
                        !(self.gates.has(from) && from != TB.mantle) |=> 3000 -> "move shudde m'ell to protect own gate (creator priority 2026-11-08, gate-to-gate waste fix 2026-11-11)"
                    // 2026-11-08 (3rd fix same day): creator's full post-awaken Power
                    // priority list for the main-action menu is 1) protect own Gate with
                    // the Head (above, 3000) 2) Thousand Writhing Maws recruit Tentacles
                    // (TBWrithingMawsTypeAction, 1000) 3) move a Tentacle to an Area with
                    // no TB Unit in it 4) Thousand Writhing Maws recruit Cadavolytes
                    // (950 and below). This Move option has to slot between tiers 2 and 4
                    // since all of Move/Recruit/TWM compete directly in the same
                    // Action-Phase main menu.
                    u.is(Tentacle) && game.tbShuddeMellEverAwakened && to.allies.none |=> 975 -> "move tentacle to area without TB units (post-awaken priority 3, creator priority 2026-11-08)"
                    // 2026-11-12 fix (creator report, game "Tentacles versus Shift",
                    // rowId 1002, action 329): the bot moved a Tentacle into an Area
                    // that already held another TB Tentacle (NorthAmerica, placed
                    // there at setup), stacking two Tentacles in one Area for no
                    // reason -- Tentacles can never control Gates, so stacking them
                    // together gains nothing and wastes the Area-spread Power-income
                    // benefit the creator's 2026-11-02 spread priority already exists
                    // to protect. Creator: "bot can never move a tentacle into another
                    // tentacle area, and if they must move, should always move away
                    // from areas with other tentacles." Hard-block stacking, and
                    // reward moving out of an already-stacked Area.
                    u.is(Tentacle) && self.at(to, Tentacle).any |=> -2500 -> "avoid moving a tentacle into an area that already holds a TB tentacle (creator priority 2026-11-12)"
                    u.is(Tentacle) && self.at(from, Tentacle).num >= 2 && self.at(to, Tentacle).none |=> 500 -> "move tentacle away from an area stacked with another tentacle (creator priority 2026-11-12)"
                    // 2026-11-10 creator priority (game "Catastrophe through
                    // Blindness", rowId 987): when a Shudde M'ell Part's destination
                    // is the Mantle itself, Abyssal Cline (TBMovePartToMantleAction)
                    // already moves it there for free -- a paid Move to the same
                    // destination just wastes Power. Hard-block so this never
                    // outranks the free alternative.
                    (u.goo || u.is(ShuddeMellSegment)) && game.tbMantleInPlay &&
                        game.tbShuddeMellEverAwakened && to == TB.mantle |=> -3000 -> "avoid paying Power to move a part to the Mantle when Abyssal Cline does it for free (creator priority 2026-11-10)"
                    // 2026-11-08 (3rd fix same day): "until on 6 spellbooks, Shudde M'ell
                    // should not move to adjacent gates nor [move to capture] acolytes...
                    // should not move into ocean/sea areas." Hard-blocks (large negative,
                    // same style as the existing -6000/-1000000 hard blocks elsewhere in
                    // this file) so they outrank the generic "toward enemies"/"free gate"
                    // lines below regardless of magnitude-sorted tie-breaking. Restricted
                    // to freeGate/enemyGate so it does not fight the own-Gate-protection
                    // rule immediately above (which is unaffected, and un-gated by
                    // spellbook count).
                    u.goo && numSB < 6 && (to.freeGate || to.enemyGate) |=> -2000 -> "avoid moving shudde m'ell to an adjacent (non-own) gate until 6 spellbooks (creator priority 2026-11-08)"
                    // "...nor acolytes to capture": Shudde M'ell's presence in an Area
                    // with an enemy Cultist makes that Cultist capturable (the GOO-capture
                    // rule at Game.scala canCapture: f.at(r).goos.any => true), so moving
                    // toward an Area with a capturable enemy Cultist is itself the
                    // "going for a capture" move this restriction targets.
                    u.goo && numSB < 6 && to.foes.cultists.%(c => c.uclass.canBeCaptured(c)).any |=> -2000 -> "avoid moving shudde m'ell to capture enemy cultists until 6 spellbooks (creator priority 2026-11-08)"
                    (u.goo || u.is(ShuddeMellSegment)) && numSB < 6 && to.ocean |=> -2000 -> "avoid moving shudde m'ell into ocean/sea areas until 6 spellbooks (creator priority 2026-11-08)"
                    u.goo && to.foes.any                         |=> 700  -> "shudde mell toward enemies"
                    // 2026-11-09 fix: this used to require numSB >= 5 (5 Spellbooks
                    // already picked) before Shudde M'ell would move onto the three
                    // Glyph Areas -- but Grasp (the 5th Spellbook pick) only outranks
                    // Autotomy's flat 1200 score (SpellbookAction case above) when the
                    // Three-Glyphs SBR is ALREADY satisfied at the moment of that 5th
                    // pick (numSB == 4). Gating the move on numSB >= 5 meant Three-Glyphs
                    // could never be satisfied before the 5th pick, so Autotomy always
                    // won that pick instead of Grasp -- the exact "TB must select Grasp
                    // as the 5th spellbook" requirement this open item describes. Moved
                    // the threshold to numSB >= 4 (right after Ensnare, the 4th pick, and
                    // before the 5th pick is offered) so the glyph-move -- and the Req it
                    // satisfies -- lands in time for Grasp to win that pick.
                    // 2026-11-10 fix (task: "more foolproof sequence"): this used to
                    // block ALL glyph moves until 4 Spellbooks, full stop -- but the
                    // creator's new requested opening explicitly starts the glyph-move
                    // push the moment Remove-Gate-Place-Chthonian (SBR-3) banks its 2
                    // Power, well before 4 Spellbooks are picked. Narrowed the block to
                    // only apply while that SBR is still outstanding, so it no longer
                    // fights the earlier trigger the creator asked for.
                    (u.goo || u.is(ShuddeMellSegment)) && numSB < 4 && self.needs(RemoveGatePlaceChthonianReq) &&
                        (to.glyph == GlyphAA || to.glyph == GlyphOO || to.glyph == GlyphWW) |=> -1000 -> "move part to glyph area (three-glyphs SBR deprioritized until remove-gate-place-chthonian satisfied, creator priority 2026-11-09, narrowed 2026-11-10)"
                    // 2026-11-10 fix (creator report, game "Lunacy for Space", rowId 985):
                    // this used to reward a move onto ANY of the three glyph types with
                    // the same flat score, with no check of which glyph type(s) the
                    // OTHER Shudde M'ell parts already sit on. That let the bot move the
                    // Head from one (*) Area to another (*) Area (a pure wasted-Power
                    // no-op, since (*) was already covered) and send both Segments onto
                    // /^\ Areas instead of splitting across /^\ and ||| -- leaving the
                    // three-glyphs requirement permanently unsatisfied. Now only the
                    // glyph type(s) not already held by one of this part's OTHER parts
                    // (head/segments, excluding the one being moved) count toward the
                    // bonus, so each part is steered at a distinct, still-needed glyph.
                    // 2026-11-10 fix (task: "more foolproof sequence"): widened the
                    // trigger from "4 of the other 5 SBRs done" (sbReqsDoneExclGlyphs >= 4,
                    // which in practice meant late-game, after Gates-at-GOOs) to "Remove-
                    // Gate-Place-Chthonian satisfied" -- the creator's literal request is
                    // to push the Head+first-Segment glyph move immediately off that SBR's
                    // 2 banked Power, with the second Segment following as the first action
                    // of the next Action Phase (naturally highest-scored once the Head/
                    // first-Segment glyph is already covered, since that's the only glyph
                    // move left to make). sbReqsDoneExclGlyphs retained as an alternate
                    // (OR'd) path so the pre-existing late-game fallback keeps working for
                    // games where Remove-Gate-Place-Chthonian is reached later or skipped.
                    // 2026-11-10 fix (creator report, game "Break through Undefined",
                    // rowId 994): this used to reward a move onto any glyph not covered
                    // by the part's OTHER parts, but never checked whether the MOVING
                    // part itself already occupied that same glyph type before the move
                    // -- so Shudde M'ell's Head, already sitting on a (*) Area, could be
                    // "rewarded" 1900 to move to a DIFFERENT (*) Area, a pure wasted-Power
                    // no-op (action 627: Head moved North America (*) to South America
                    // (*), still only covering (*), gaining nothing). Added
                    // from.glyph != to.glyph so a part already providing a glyph's
                    // coverage is never paid to relocate within that same glyph type.
                    // 2026-11-12 fix (creator report, game "Tentacles versus Shift",
                    // rowId 1002, mistakes 4/6/7): this used to also require 2+
                    // Segments already in play before it would fire at all, so the
                    // very first Head move (0 Segments) and the Head-or-first-Segment
                    // move (1 Segment) right after Remove-Gate-Place-Chthonian never
                    // scored anything, leaving the Thousand Writhing Maws fallback
                    // (flat +300, see TBWrithingMawsAction below) to win those turns by
                    // default instead. The glyph-coverage check below already handles
                    // 0/1/2-Segment states correctly on its own (a part with no other
                    // parts placed yet simply has nothing marked "covered"), so the
                    // Segment-count gate was removed entirely.
                    (u.goo || u.is(ShuddeMellSegment)) && (!self.needs(RemoveGatePlaceChthonianReq) || sbReqsDoneExclGlyphs >= 4) && {
                        val otherGlyphsCovered = (self.all(ShuddeMellHead) ++ self.all(ShuddeMellSegment))
                            .%(_.ref != u)./(_.region.glyph).distinct
                        (to.glyph == GlyphAA || to.glyph == GlyphOO || to.glyph == GlyphWW) && to.glyph != from.glyph && !otherGlyphsCovered.has(to.glyph)
                    } |=> 1900 -> "move part to still-needed glyph area (three-glyphs SBR, creator priority 2026-11-09, glyph-coverage fix 2026-11-10, early-trigger widening 2026-11-10, same-glyph-waste fix 2026-11-11, 0/1-segment gap fix 2026-11-12)"
                    // 2026-11-10 creator priority (game "Catastrophe through
                    // Blindness", rowId 987): a gate-holding Cadavolyte was moved
                    // off its own main-map Gate via a plain Move, abandoning TB's
                    // only remaining main-map Gate -- the same "don't abandon your
                    // own Gate" guard already existed for the Stalk path (above)
                    // but had no counterpart here for ordinary Move. Must outrank
                    // every other Move option (hard-block style) so it isn't
                    // overridden by any of the scores above.
                    game.unit(u).onGate                          |=> -2500 -> "avoid relocating a gate-holding cultist off its own gate (creator priority 2026-11-10)"
                    to.freeGate                                  |=> 400  -> "move to free gate"
                    // 2026-11-13 (task: 6-spellbook behavior spec, item 2): once on 6
                    // Spellbooks, move a Segment onto a Gate Area that holds a TB
                    // Cadavolyte currently exposed to capture, so the Segment (a GOO)
                    // shields it (region.capturers requires allies.goos.none to apply,
                    // per the capturable/capturers helpers in BotX.scala).
                    u.is(ShuddeMellSegment) && numSB >= 6 && self.gates.has(to) &&
                        self.at(to, Cadavolyte).any && to.capturers.active.any |=> 1500 -> "move segment to protect a gate-sitting cadavolyte at risk of capture (creator priority 2026-11-13, item 2)"
                    // 2026-11-13 (task: 6-spellbook behavior spec, item 7): "to attack an
                    // enemy GOO or terror... move tentacle to enemy GOO or terror area,
                    // then move Head and segment to GOO area." The Tentacle leg comes
                    // first (it has no Power cost gate on arrival and is what makes the
                    // Area a legal Ensnare/Unlimited-Battle target); the Head/Segment leg
                    // only follows once a TB Tentacle has already reached that Area, so
                    // this does not fire both legs in the same turn out of order.
                    u.is(Tentacle) && numSB >= 6 && tbOffenseTargets.exists(_.region == to) |=> 1000 -> "move tentacle to a weakly-protected enemy goo/terror area (creator priority 2026-11-13, item 7)"
                    (u.goo || u.is(ShuddeMellSegment)) && numSB >= 6 &&
                        tbOffenseTargets.exists(_.region == to) && self.at(to, Tentacle).any |=> 1950 -> "move shudde m'ell to a weakly-protected enemy goo/terror area already held by our tentacle (creator priority 2026-11-13, item 7)"
                    // 2026-11-13 (task: 6-spellbook behavior spec, item 12): once the
                    // 6SB/27-Doom/>4-Power endgame window is open, the creator wants
                    // "maximum priority on... move tentacle and shudde to GOO battle
                    // autotomy loop" -- outranks even Psychic Shriek's 7000 (item 4),
                    // since the whole point of this window is to close out the game.
                    u.is(Tentacle) && ensnareAutotomyLoopMaxPriority && tbOffenseTargets.exists(_.region == to) |=> 7500 -> "endgame push: move tentacle to a weakly-protected enemy goo/terror area, maximum priority (creator priority 2026-11-13, item 12)"
                    (u.goo || u.is(ShuddeMellSegment)) && ensnareAutotomyLoopMaxPriority &&
                        tbOffenseTargets.exists(_.region == to) && self.at(to, Tentacle).any |=> 7600 -> "endgame push: move shudde m'ell to a weakly-protected enemy goo/terror area already held by our tentacle, maximum priority (creator priority 2026-11-13, item 12)"

                // 2026-11-13 fix (creator report, game "Failure against Colour", rowId
                // 1007, item 4): every other faction's bot scores MoveDoneAction with a
                // flat positive weight (BotGC/BotCC/BotTT/BotSL/etc. all use 1000) so the
                // bot stops moving once no remaining move is actually worth making. TB
                // had no case for it at all, so it fell through to the generic
                // "unknown" catch-all (-1000) -- which always loses to ANY other legal
                // Move option, even one with zero applicable scoring rules. That let a
                // Shudde M'ell Head with no legal glyph-advancing destination left (all
                // reachable glyphs already covered by its own Segments that turn) get
                // pushed into a pure wasted-Power lateral move (NorthAmerica (*) to
                // South America (*), action 474) simply because "move somewhere" beat
                // "stop" by default. Added the same flat score every sibling bot uses so
                // TB can choose to stop once nothing left actually helps.
                case MoveDoneAction(_) =>
                    true |=> 1000 -> "move done"

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
                    // 2026-11-13 (task: 6-spellbook behavior spec, items 5/7): once the
                    // chain's setup (Tentacle in the Area, Shudde M'ell parts staged)
                    // is in place, Unlimited-Battling a weakly-protected enemy GOO/Terror
                    // (<=2 other protecting units) is the whole point of the chain --
                    // this is the surviving AttackAction leaf for both the plain-Attack
                    // and the Soft TBUnlimitedBattleMainAction->AttackMainAction route.
                    numSB >= 6 && (tbDefenseTargets.exists(_.region == r) || tbOffenseTargets.exists(_.region == r)) |=> 1250 -> "attack/unlimited-battle a weakly-protected enemy goo/terror (creator priority 2026-11-13, items 5/7)"
                    // 2026-11-13 (task: 6-spellbook behavior spec, item 12): endgame
                    // push window -- "maximum priority on ensnare GOO battle autotomy
                    // loop." Outranks Psychic Shriek's 7000 (item 4).
                    ensnareAutotomyLoopMaxPriority && (tbDefenseTargets.exists(_.region == r) || tbOffenseTargets.exists(_.region == r)) |=> 7500 -> "endgame push: attack/unlimited-battle a weakly-protected enemy goo/terror, maximum priority (creator priority 2026-11-13, item 12)"

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
                // 2026-11-09 fix (game "colour against lunacy"): this flat 600 was
                // always below the post-awaken "move tentacle to empty area" Move option
                // (975, added 2026-11-08), so once Shudde M'ell was awakened the bot
                // never even entered the Thousand Writhing Maws flow to spend its 2
                // banked Power on a Tentacle double-recruit -- it just moved an existing
                // Tentacle instead, exactly as the creator reported. The creator's
                // documented post-awaken Power priority order is 1) protect own Gate
                // (3000) 2) TWM recruit Tentacles 3) move Tentacle to empty Area (975)
                // 4) TWM recruit Cadavolytes, so this main menu entry now scores above
                // 975 specifically when post-awaken and still short of the 10-Tentacle
                // SBR with a Tentacle double-recruit available.
                case TBWrithingMawsMainAction(f) =>
                    game.tbShuddeMellEverAwakened && f.needs(TenTentaclesReq) &&
                        TBExpansion.tbMawsEligibleTypes(f).has(Tentacle) |=> 1000 -> "writhing maws double summon (post-awaken, prefer over moving a tentacle, creator priority 2026-11-08)"
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
                    // 2026-11-09 fix (game "omen versus oblivion"): TBWrithingMawsMainAction
                    // and TBWrithingMawsTypeAction above are both Soft, so Explode.explode
                    // (see Explode.scala) auto-performs and discards them before scoring ever
                    // runs -- their 1000-point post-awaken priorities were dead code, never
                    // once compared against the "move tentacle to empty area" Move option's
                    // 975 (BotTB.scala, MoveAction case). Same dead-Soft-action bug class
                    // already fixed for Psychic Shriek in v2.548. TBWrithingMawsAction is the
                    // only non-Soft leaf in this chain, so the real priority has to live here.
                    // 2026-11-11 fix (creator report, game "Break through Lunacy", rowId
                    // 995): the Move-based glyphPushActive block (above, in MoveAction)
                    // only stopped a Tentacle/Chthonian MOVE during the glyph push -- it
                    // had no counterpart here, so this Tentacle double-recruit still fired
                    // (and won, at 1050) the moment Remove-Gate-Place-Chthonian banked its
                    // 2 Power, spending that Power on Tentacles instead of the glyph-area
                    // moves the creator wants prioritized during that exact window.
                    // 2026-11-12 fix (creator report, game "Tentacles versus Shift",
                    // rowId 1002, mistake 3 vs. mistake 5 reconciled): confirmed against
                    // the real game log that action 334-336's 2-Power Tentacle recruit
                    // happened with ZERO Shudde M'ell Segments in play yet (mistake 3's
                    // exact condition, not mistake 5's "1 Segment" one) -- at 0 Segments
                    // there is no Head-only glyph move available to compete with this
                    // (moving the Head alone can never satisfy Three-Glyphs), so this
                    // Tentacle double-recruit staying top priority here is correct by
                    // the creator's own mistake (3). The 1/2-Segment cases from mistakes
                    // (5)/(6)/(7) are handled by the Move-based glyph bonus above, which
                    // now fires at any Segment count (see the 0/1-segment gap fix there)
                    // and by glyphPushActive already blocking this Tentacle recruit once
                    // a glyph move is actually available to make instead.
                    uc == Tentacle && game.tbShuddeMellEverAwakened && !glyphPushActive && self.needs(TenTentaclesReq) |=> 1050 -> "maws commit: tentacle double-recruit (post-awaken, toward 10-tentacle SBR, outranks moving a tentacle, creator priority 2026-11-09, deferred to glyph push 2026-11-11, 0-segment case confirmed correct 2026-11-12)"
                    // 2026-11-10 fix (task: "more foolproof sequence"): new creator step
                    // after Gates-at-GOOs (5th SBR) -- with Power still available, use TWM
                    // to place Cadavolytes defending Areas that already hold a Shudde
                    // M'ell Part and are one of TB's own Gates (dual Gate+GOO defense).
                    uc == Cadavolyte && !self.needs(GatesAtGOOsReq) &&
                        self.gates.has(r1) && (self.at(r1, ShuddeMellHead).any || self.at(r1, ShuddeMellSegment).any) &&
                        self.gates.has(r2) && (self.at(r2, ShuddeMellHead).any || self.at(r2, ShuddeMellSegment).any) |=> 1150 -> "maws commit: two cadavolytes defending shudde m'ell gate areas (creator priority 2026-11-10)"
                    uc == Cadavolyte && !self.needs(GatesAtGOOsReq) &&
                        ((self.gates.has(r1) && (self.at(r1, ShuddeMellHead).any || self.at(r1, ShuddeMellSegment).any)) ||
                         (self.gates.has(r2) && (self.at(r2, ShuddeMellHead).any || self.at(r2, ShuddeMellSegment).any))) |=> 1100 -> "maws commit: cadavolyte defending a shudde m'ell gate area (creator priority 2026-11-10)"
                    // 2026-11-13 fix (creator report, game "Failure against Colour",
                    // rowId 1007, item 5): "conditional on shudde mell head or segment
                    // parts being in play, thousand writhing maws recruit cadavolyes in
                    // areas with shudde mell head or segment parts." The two bonuses
                    // just above already cover this, but only once Gates-at-GOOs (5th
                    // SBR) is satisfied AND the Area is one of TB's own Gates -- at
                    // action 610-612 of the reported game, TB had neither (only 4
                    // Spellbooks picked, no Gates-at-GOOs yet), so both Cadavolytes
                    // went to WestAfrica/EastAfrica -- nowhere near the Head, which had
                    // not left North America yet. The creator's literal ask has no Gate
                    // or SBR condition at all, so this unconditional pair fills that
                    // early-game gap; kept below the more specific Gate-defense bonuses
                    // so those still win once they also apply.
                    uc == Cadavolyte &&
                        (self.at(r1, ShuddeMellHead).any || self.at(r1, ShuddeMellSegment).any) &&
                        (self.at(r2, ShuddeMellHead).any || self.at(r2, ShuddeMellSegment).any) |=> 950 -> "maws commit: two cadavolytes in areas with shudde m'ell parts (creator priority 2026-11-13, item 5)"
                    uc == Cadavolyte &&
                        ((self.at(r1, ShuddeMellHead).any || self.at(r1, ShuddeMellSegment).any) ||
                         (self.at(r2, ShuddeMellHead).any || self.at(r2, ShuddeMellSegment).any)) |=> 900 -> "maws commit: cadavolyte in an area with a shudde m'ell part (creator priority 2026-11-13, item 5)"
                    // 2026-11-10 fix (task: "more foolproof sequence"): new creator step
                    // heading into the 6th (final) Spellbook pick -- place Tentacles into
                    // Areas a Shudde M'ell Part already occupies, to protect it.
                    uc == Tentacle && numSB >= 5 &&
                        (self.at(r1, ShuddeMellHead).any || self.at(r1, ShuddeMellSegment).any) &&
                        (self.at(r2, ShuddeMellHead).any || self.at(r2, ShuddeMellSegment).any) |=> 1075 -> "maws commit: two tentacles protecting shudde m'ell areas (creator priority 2026-11-10)"
                    uc == Tentacle && numSB >= 5 &&
                        (self.at(r1, ShuddeMellHead).any || self.at(r1, ShuddeMellSegment).any ||
                         self.at(r2, ShuddeMellHead).any || self.at(r2, ShuddeMellSegment).any) |=> 1060 -> "maws commit: tentacle protecting a shudde m'ell area (creator priority 2026-11-10)"
                    uc == Tentacle && self.at(r2, Tentacle).any && r2.connected.exists(c => self.at(c, Tentacle).none) |=> 400 -> "maws: tentacle area next to an empty area"
                    uc == Cadavolyte && self.needs(OverlayMantleReq) && r1.connected.has(r2) |=> (500 + distFromEnemies(r2)) -> "maws: two cadavolytes in adjacent areas, far from enemies (creator: new opening, pair toward Overlay Mantle SBR away from enemies)"
                    // 2026-11-11 fix (creator report, game "Doom in Omen", rowId
                    // 1001, action 282): once Overlay-the-Mantle is already done, the
                    // "far from enemies" Cadavolyte placement bonus above stops
                    // applying and the dedicated "defend a Shudde M'ell gate area"
                    // bonuses further above only fire once Gates-at-GOOs is also
                    // done -- leaving a gap window where TWM Cadavolyte placement had
                    // no enemy-awareness at all. Here a Cadavolyte was placed into
                    // IndianOcean right next to an enemy Acolyte that had just moved
                    // in, an easy future capture target. Same hard-block style already
                    // used for Stalk's Cadavolyte-safety check (above): refuse a
                    // placement Area where an enemy could already capture a Cadavolyte
                    // there, whenever none of the other, more specific Cadavolyte
                    // placement bonuses already apply. Both r1 and r2 receive a
                    // Cadavolyte here (see TBWrithingMawsAction in FactionTB.scala),
                    // so check both.
                    uc == Cadavolyte && (r1.capturers.active.any || r2.capturers.active.any) |=> -500 -> "maws: avoid placing a cadavolyte in an area where it could be captured (creator priority 2026-11-11)"
                    // 2026-11-13 (task: 6-spellbook behavior spec, items 1 and 9): once
                    // on 6 Spellbooks, recruiting Cadavolytes onto an open (uncontrolled)
                    // Gate where TB already has a Tentacle is the single highest TWM
                    // Cadavolyte-placement priority -- outranking even the post-Gates-at-
                    // GOOs defensive pairing above -- as long as the placement is still
                    // capture-safe (the capture-avoidance block above already rejects any
                    // r1/r2 where the Cadavolyte could be captured, so it fires first and
                    // this bonus is never reached for an unsafe Area).
                    uc == Cadavolyte && numSB >= 6 && r1.freeGate && self.at(r1, Tentacle).any &&
                        r2.freeGate && self.at(r2, Tentacle).any |=> 1400 -> "maws commit: two cadavolytes onto open gates held by our tentacles (creator priority 2026-11-13, items 1/9)"
                    uc == Cadavolyte && numSB >= 6 &&
                        ((r1.freeGate && self.at(r1, Tentacle).any) || (r2.freeGate && self.at(r2, Tentacle).any)) |=> 1350 -> "maws commit: cadavolyte onto an open gate held by our tentacle (creator priority 2026-11-13, items 1/9)"
                    // 2026-11-13 fix (creator report, game "Failure against Colour",
                    // rowId 1007, item 8): "TB should have thousand writhing maws
                    // recruited a cadavolyte in on the two open gates australia and
                    // south america in the same action." The two bonuses just above
                    // already cover placing both Cadavolytes onto open Tentacle-held
                    // Gates in one TWM action, but only once numSB >= 6 -- at action
                    // 883 of the reported game, TB was still on its 5th Spellbook
                    // (Autotomy, picked at action 840), one short of that gate, so
                    // neither bonus applied and only one Cadavolyte reached an open
                    // gate while the other went to the Mantle instead. The creator's
                    // literal request has no Spellbook-count condition, so these
                    // unconditional (lower-weight) duplicates fill that gap; kept
                    // below the numSB >= 6 versions so those still win once reached.
                    uc == Cadavolyte && r1.freeGate && self.at(r1, Tentacle).any &&
                        r2.freeGate && self.at(r2, Tentacle).any |=> 1000 -> "maws commit: two cadavolytes onto open gates held by our tentacles (creator priority 2026-11-13, item 8)"
                    uc == Cadavolyte &&
                        ((r1.freeGate && self.at(r1, Tentacle).any) || (r2.freeGate && self.at(r2, Tentacle).any)) |=> 970 -> "maws commit: cadavolyte onto an open gate held by our tentacle (creator priority 2026-11-13, item 8)"
                    // 2026-11-13 (task: 6-spellbook behavior spec, item 3): once on 6
                    // Spellbooks and holding a good number of Gates (3+), recruit
                    // Chthonians onto Gate Areas that already hold a Cadavolyte or a
                    // Shudde M'ell Part, to protect them.
                    uc == Chthonian && numSB >= 6 && self.gates.num >= 3 &&
                        self.gates.has(r1) && (self.at(r1, Cadavolyte).any || self.at(r1, ShuddeMellHead).any || self.at(r1, ShuddeMellSegment).any) &&
                        self.gates.has(r2) && (self.at(r2, Cadavolyte).any || self.at(r2, ShuddeMellHead).any || self.at(r2, ShuddeMellSegment).any) |=> 1300 -> "maws commit: two chthonians protecting gates (creator priority 2026-11-13, item 3)"
                    uc == Chthonian && numSB >= 6 && self.gates.num >= 3 &&
                        ((self.gates.has(r1) && (self.at(r1, Cadavolyte).any || self.at(r1, ShuddeMellHead).any || self.at(r1, ShuddeMellSegment).any)) ||
                         (self.gates.has(r2) && (self.at(r2, Cadavolyte).any || self.at(r2, ShuddeMellHead).any || self.at(r2, ShuddeMellSegment).any))) |=> 1250 -> "maws commit: chthonian protecting a gate (creator priority 2026-11-13, item 3)"
                    true                                         |=> 300  -> "maws commit"

                // Abyssal Cline: Move Part to Mantle
                case TBMovePartToMantleMainAction(f) =>
                    true                                         |=> 400  -> "behemoth move part to mantle"
                // 2026-11-13 (task: 6-spellbook behavior spec, items 5/7): the attack/
                // defense chain opens by staging the Head and a Segment on the Mantle --
                // either to turn the Mantle into the Ensnare-lure kill-zone (item 5) or
                // to bank them for a direct assault move-out afterward (item 7). This is
                // the real (non-Soft) leaf that decides whether Abyssal Cline is used
                // this turn at all, so the boost belongs here, not on the Soft MainAction
                // above (dead-Soft-action pattern, same as Psychic Shriek/Writhing Maws).
                case TBMovePartToMantlePickAction(f, _) =>
                    numSB >= 6 && (tbDefenseTargets.any || tbOffenseTargets.any) |=> 1450 -> "abyssal cline: stage shudde m'ell parts on the mantle to open the attack/defense chain (creator priority 2026-11-13, items 5/7)"
                    // 2026-11-13 (task: 6-spellbook behavior spec, item 12): endgame
                    // push window -- "maximum priority on... move tentacle and shudde
                    // to GOO battle autotomy loop." Outranks Psychic Shriek's 7000.
                    ensnareAutotomyLoopMaxPriority && (tbDefenseTargets.any || tbOffenseTargets.any) |=> 7500 -> "endgame push: abyssal cline: stage shudde m'ell parts on the mantle, maximum priority (creator priority 2026-11-13, item 12)"
                    // 2026-11-13 (item 4 backup): offer to pull a glyph-duplicated
                    // Part back to the Mantle for redeployment -- see glyphDuplicateParts
                    // above. Must outrank the generic fallback Move options (TWM commit
                    // 1050, "move tentacle to empty area" 975) since those parts have no
                    // other useful Move available to them while stuck on a duplicate glyph.
                    glyphPushActive && glyphDuplicateParts.any |=> 1100 -> "abyssal cline: pull a glyph-duplicated shudde m'ell part back to the mantle for redeployment (creator priority 2026-11-13, item 4 backup)"
                    true                                         |=> 100  -> "pick part for mantle"
                case TBMovePartToMantleAction(f, part) =>
                    numSB >= 6 && (tbDefenseTargets.any || tbOffenseTargets.any) |=> 300 -> "abyssal cline: move part to mantle for the attack/defense chain (creator priority 2026-11-13, items 5/7)"
                    ensnareAutotomyLoopMaxPriority && (tbDefenseTargets.any || tbOffenseTargets.any) |=> 7500 -> "endgame push: abyssal cline: move part to mantle, maximum priority (creator priority 2026-11-13, item 12)"
                    // 2026-11-13 fix (creator report, game "Failure against Colour",
                    // rowId 1007, item 4 backup): "if TB has 2 parts on the same glyph,
                    // abyssal cline to the mantle and move head or segments to a glyph
                    // that a head or segment was not already in." Selects the specific
                    // duplicated Part (not just any Part) to send back, so the Part that
                    // IS already providing a unique glyph's coverage stays put.
                    glyphPushActive && glyphDuplicateParts.has(part) |=> 1050 -> "abyssal cline: move glyph-duplicated part back to the mantle for redeployment (creator priority 2026-11-13, item 4 backup)"
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
                    // 2026-11-09 creator priority (game "Curse and decay"): Stalk must
                    // never relocate a Tentacle into an Area that already holds a
                    // Tentacle -- the bot had done exactly that, crowding Tentacles
                    // together instead of spreading them. Same hard-block style as the
                    // gate-holding-cultist rule below (must outrank TBStalkSkipAction's 0).
                    cultist.uclass == Tentacle && self.at(dest, Tentacle).any  |=> -500 -> "stalk: avoid moving a tentacle into an area with another tentacle (creator priority 2026-11-09)"
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
                    // 2026-11-09 creator priority (game "gate in blindness"): Stalk
                    // must never relocate a Cadavolyte into an Area where it could be
                    // captured -- same hard-block style as the other two Stalk
                    // avoidance rules above (must outrank TBStalkSkipAction's 0).
                    cultist.uclass == Cadavolyte && dest.capturers.active.any |=> -500 -> "stalk: avoid moving a cadavolyte into an area where it could be captured (creator priority 2026-11-09)"
                    true                                         |=> 200  -> "stalk commit"

                // Autotomy: post-battle Kill transfer
                // 2026-11-13 (task: 6-spellbook behavior spec, item 6): "always agree
                // yes to Autotomy, never decline." TBAutotomyUseAction is Soft (see
                // FactionTB.scala), so Explode.explode auto-performs it into the
                // non-Soft TBAutotomyPickSegmentAction below (100) before this 800
                // score ever runs -- same dead-Soft-action pattern as Psychic Shriek/
                // Writhing Maws. The real decision is PickSegmentAction vs Skip, so
                // Skip is hard-blocked here (large negative, same style as the other
                // absolute blocks in this file) to make "never decline" unconditional
                // rather than relying on the existing 100-vs-(-200) margin.
                case TBAutotomyUseAction(f) =>
                    true                                         |=> 800  -> "use autotomy (ES farming)"
                case TBAutotomySkipAction(f) =>
                    true                                         |=> -5000 -> "never decline autotomy (creator priority 2026-11-13, item 6)"
                case TBAutotomyPickSegmentAction(f, _) =>
                    true                                         |=> 100  -> "pick segment for autotomy"
                // 2026-11-13 (task: 6-spellbook behavior spec, items 5/7): "...if you
                // have 2 other surviving Units in area autotomy retreat to another GOO
                // area" -- once the battle area still holds 2+ other TB units besides
                // the Segment just sacrificed, retreat toward another Area holding an
                // enemy GOO/Terror instead of a safe Gate, so the chain can re-engage
                // and Autotomy again rather than just going defensive.
                case TBAutotomyRetreatAction(f, segment, arena) =>
                    val survivingOthers = self.at(arena).%(_.ref != segment).num
                    numSB >= 6 && survivingOthers >= 2 |=> 150 -> "autotomy retreat: enough surviving units to re-engage (creator priority 2026-11-13, items 5/7)"
                    true                                         |=> 100  -> "pick autotomy retreat"
                case TBAutotomyRetreatExecuteAction(f, retreatDest, arena) =>
                    numSB >= 6 && retreatDest.foes.exists(u => u.goo || u.terror) |=> 150 -> "autotomy retreat: toward another area with an enemy goo/terror to re-engage (creator priority 2026-11-13, items 5/7)"
                    true                                         |=> 100  -> "autotomy retreat commit"
                // 2026-11-13 (task: 6-spellbook behavior spec, items 5/7): "autotomy
                // kill a segment not on mantle" -- Behemoth only stockpiles Segments on
                // the Mantle when Power drops to 0, and the chain relies on that banked
                // stockpile for its NEXT Abyssal-Cline staging/Autotomy cycle, so
                // sacrifice a field Segment (not on the Mantle) over a Mantle-stockpiled
                // one whenever both are offered as candidates.
                case TBAutotomyAction(f, segment, _, _) =>
                    numSB >= 6 && game.unit(segment).region != TB.mantle |=> 150 -> "autotomy: kill a segment not on the mantle, preserve the mantle stockpile (creator priority 2026-11-13, items 5/7)"
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
                case TBEnsnareTargetAction(f, enemy, area) =>
                    // 2026-11-13 (task: open item 7, game "Failure against Colour",
                    // rowId 1007): creator report -- action index 633 targeted Yellow
                    // Sign with Ensnare while YS sat at 0 Power, so count =
                    // math.min(roll, 0) = 0: no units relocated, just a wasted 1 Power
                    // cast, same root cause as item 1's Psychic Shriek fix above.
                    // "should not have targeted any faction with less than 2 power with
                    // ensnare." Mirrors the Shriek fix: -1200 outweighs CancelAction's
                    // default -1000 when no qualifying target exists.
                    enemy.power < 2                               |=> -1200 -> "ensnare target: avoid casting against a faction under 2 power, cast would do nothing (creator priority 2026-11-13, item 7)"
                    // 2026-11-13 (task: 6-spellbook behavior spec, item 5): "...then
                    // Ensnare the enemy GOO or terror" -- once a weakly-protected enemy
                    // GOO/Terror sits in the Tentacle Area the Abyssal-Cline-staged Head
                    // now covers, Ensnaring that enemy drags it (and the D6-rolled count
                    // of its other units) toward the Head -- normally the Mantle, per
                    // TBEnsnareRollAction's headArea fallback -- setting up the
                    // Unlimited Battle + Autotomy finish.
                    numSB >= 6 && enemy.at(area).exists(tbWeaklyProtected) |=> 1300 -> "ensnare target: drag a weakly-protected enemy goo/terror toward shudde m'ell (creator priority 2026-11-13, item 5)"
                    // 2026-11-13 (task: 6-spellbook behavior spec, item 12): endgame
                    // push window -- "maximum priority on ensnare GOO battle autotomy
                    // loop." Outranks Psychic Shriek's 7000 (item 4).
                    ensnareAutotomyLoopMaxPriority && enemy.at(area).exists(tbWeaklyProtected) |=> 7500 -> "endgame push: ensnare target: drag a weakly-protected enemy goo/terror toward shudde m'ell, maximum priority (creator priority 2026-11-13, item 12)"
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
                    // 2026-11-13 (task: open item 1, game "Failure against Colour",
                    // rowId 1007): creator report confirmed via admin log -- action
                    // index 384 targeted Great Cthulhu with Psychic Shriek while GC sat
                    // at 0 Power (having just paid 10 Power to Awaken Cthulhu with no
                    // Gather Power in between), so the retreat count forced to
                    // math.min(roll, 2*0) = 0: the whole cast did nothing but still cost
                    // 1 Power. "target a faction conditional on having 5 or more power."
                    // A flat -1200 outweighs CancelAction's default -1000 (BotX's
                    // compareEL ranks by sorted absolute weight, so -1200 beats -1000)
                    // whenever no qualifying target exists, so the bot skips casting
                    // Shriek entirely rather than targeting any under-5-power enemy.
                    enemy.power < 5                               |=> -1200 -> "shriek target: avoid casting against a faction under 5 power, cast would do nothing (creator priority 2026-11-13, item 1)"
                    // 2026-11-13 (task: 6-spellbook behavior spec, item 4): "at the
                    // beginning of each action phase use psychic shriek as much as you
                    // can to make chaos highest priority" -- once all 6 Spellbooks are
                    // picked, Psychic Shriek targeting outranks every other tier below
                    // (including the pre-awaken 5500/5550 window, which by this point in
                    // the game no longer applies since Shudde M'ell is long awakened).
                    numSB >= 6                                    |=> 7000 -> "shriek target: 6 spellbooks earned, use psychic shriek as much as possible (creator priority 2026-11-13, item 4)"
                    game.tbMantleInPlay && !game.tbShuddeMellEverAwakened && enemy.power > 3 && (enemy == YS || enemy == CC) |=> 5550 -> "shriek target: mantle placed, pre-awaken, enemy over 3 power, preferred faction YS/CC (creator priority 2026-11-08)"
                    game.tbMantleInPlay && !game.tbShuddeMellEverAwakened && enemy.power > 3 |=> 5500 -> "shriek target: mantle placed, pre-awaken, enemy over 3 power (creator priority 2026-11-08)"
                    allSB && enemy.power > 6                     |=> 1700 -> "shriek target: all spellbooks earned, enemy power > 6 (creator priority)"
                    enemy.power > 3 && (enemy == YS || enemy == CC) |=> 450 -> "shriek target: enemy over 3 power, preferred faction YS/CC (creator priority 2026-11-07)"
                    enemy.power > 3                              |=> 400  -> "shriek target: enemy over 3 power (creator priority 2026-11-07)"
                    true                                         |=> 200  -> "shriek target commit"
                    // 2026-11-08 (3rd fix same day): creator priority -- among any
                    // targets tied on every consideration above, prefer whichever
                    // enemy currently holds the most Power. Kept deliberately
                    // low-magnitude (enemy.power tops out well under 200, the
                    // lowest existing tier above) so it only ever breaks a tie
                    // within a tier and never outranks or reorders the tiers
                    // themselves, per "target faction with most power first...
                    // in addition to other considerations already set out".
                    true                                         |=> enemy.power -> "shriek target: prefer higher-power enemy as tie-break (creator priority 2026-11-08, 'most power first')"

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
                // 2026-11-09 creator priority (game "Failure through blindness"): when
                // choosing which of the two adjacent Gates to transfer to the Mantle,
                // prefer the one closest to enemy-occupied Areas -- the Mantle is safer
                // ground, so the more exposed Gate should be the one moved there, not
                // the one already furthest from any threat. Was a flat score with no
                // enemy-distance preference at all, so the choice was effectively
                // arbitrary between the two gated Areas.
                case TBOverlayMantleTransferAction(f, source, _) =>
                    true                                         |=> (200 + (20 - distFromEnemies(source).min(20))) -> "transfer gate to mantle: prefer the gate closest to enemies (creator priority 2026-11-09)"

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
                // 2026-11-09 creator priority (game "Curse and decay"): prefer
                // removing a main-map Gate over the Mantle Gate -- the bot had
                // removed the Mantle Gate instead, which the creator flagged
                // as a mistake. The Mantle is TB's own staging area and only
                // reachable via its nominated adjacency, so losing its Gate
                // is costlier than losing an ordinary main-map Gate.
                case TBRemoveGatePlaceChthonianGateAction(f, r, _) if r == TB.mantle =>
                    true                                         |=> 50   -> "gate removal commit: avoid removing the Mantle gate if a main-map gate is available (creator priority 2026-11-09)"
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
                    // 2026-11-09 creator priority (game "gate in blindness"): if TB has
                    // no other controlled Gate left to defend (the one being removed
                    // here was the only one), prioritise placing the Chthonian on the
                    // Mantle instead -- TB's own safe home Area -- over any ordinary
                    // occupied Area.
                    self.gates.but(removedGate).none && dest == TB.mantle           |=> 600  -> "chthonian placement: no TB-controlled gate remains, prioritise the Mantle (creator priority 2026-11-09)"
                    true                                                          |=> 200  -> "chthonian placement commit"

                // SBR-4: Gates at every GOO Area
                // 2026-11-12 fix (creator report, game "Tentacles versus Shift", rowId
                // 1002, mistake 8): "conditional on three-glyphs spellbook requirement
                // fulfilled and if on more than 8 power, immediately do the 8 power
                // spellbook." The old numSB >= 5 trigger only turns true once Grasp (the
                // Spellbook earned BY satisfying Three-Glyphs) is actually picked, which
                // happens on a later turn than the Req itself being satisfied -- leaving
                // a gap where the bot could sit on 8+ Power for a full extra Action Phase
                // before paying for this SBR. Keyed directly on the Req instead, so it
                // fires the instant Three-Glyphs is satisfied, matching the creator's
                // literal wording.
                case TBGatesAtGOOsMainAction(f) =>
                    val aggressive = !self.needs(ShuddeMellInThreeGlyphsReq) && self.power >= 8
                    aggressive                                   |=> 2300 -> "gates at goo areas SBR (three-glyphs fulfilled + 8 power, creator priority 2026-11-12)"
                    !aggressive && numSB < 4                     |=> 150  -> "gates at goo areas SBR (deprioritized before 4 spellbooks earned, creator opening takes precedence)"
                    !aggressive && numSB >= 4                    |=> 700  -> "gates at goo areas SBR"
                case TBGatesAtGOOsAction(f) =>
                    val aggressive = !self.needs(ShuddeMellInThreeGlyphsReq) && self.power >= 8
                    aggressive                                   |=> 1800 -> "gates at goo commit (three-glyphs fulfilled + 8 power, creator priority 2026-11-12)"
                    !aggressive && numSB < 4                     |=> 150  -> "gates at goo commit (deprioritized before 4 spellbooks earned)"
                    !aggressive && numSB >= 4                    |=> 500  -> "gates at goo commit"

                // SBR-6: Three Glyphs alt payment
                case TBThreeGlyphsPayAction(f) =>
                    numSB < 5                                    |=> -1000 -> "three glyphs pay 6 power deprioritized below 5 spellbooks, creator priority"
                    // 2026-11-11 fix: was 2000, which beat the free "move part to
                    // still-needed glyph area" bonus (1900, above) by only 100 points --
                    // whenever both were simultaneously legal, the bot could pay 6 Power
                    // instead of taking a free positioning move toward the same
                    // requirement. Lowered below 1900 so the free move always wins when
                    // one is actually available; this Pay action only wins when no
                    // qualifying free move exists (the free-move bonus condition above is
                    // state-gated, so it simply won't fire in that case).
                    numSB >= 5 && sbReqsDoneExclGlyphs >= 4      |=> 1700 -> "three glyphs pay 6 power (4 SBRs fulfilled, creator priority)"
                    numSB >= 5                                   |=> 600  -> "three glyphs pay 6"
                case TBThreeGlyphsPayConfirmAction(f) =>
                    numSB < 5                                    |=> -1000 -> "three glyphs confirm deprioritized below 5 spellbooks, creator priority"
                    numSB >= 5                                   |=> 400  -> "three glyphs confirm"

                // 2026-11-13: creator asked to port YS's Ritual of Annihilation
                // threshold logic and tweak later, since TB had none at all (fell
                // through to the generic "unknown" -1000 case below). Ported
                // directly from BotYS.scala's RitualAction case, dropping only the
                // Hastur-specific "5 SB and less than half available power" line
                // (TB has no Hastur unit) in favor of the equivalent allSB check.
                case RitualAction(_, cost, _) =>
                    instantDeathNow |=> 10000 -> "instant death now"
                    instantDeathNext && self.allSB && others.all(!_.allSB) |=> 10000 -> "ritual if ID next and all SB"

                    self.allSB && self.realDoom + maxDoomGain >= 30 |=> 1100 -> "can break 30, and all SB"
                    instantDeathNext && !self.allSB && others.%(_.allSB).any |=> -1000 -> "don't ritual if ID next and not all SB"
                    instantDeathNext && !self.allSB && others.all(!_.allSB) && self.realDoom < others./(_.aprxDoom).max |=> 900 -> "ritual so ID next and nobody wins"
                    !self.allSB && self.doom + self.gates.num >= 30 |=> -5000 -> "will break 30, but not all SB"
                    !self.allSB && self.doom + self.gates.num < 30 && self.realDoom < 29 && self.realDoom + maxDoomGain >= 29 |=> 700 -> "won't break 30, but come near"
                    self.numSB >= 5 && cost * 2 <= power |=> 800 -> "5 SB and less than half available power"
                    self.numSB >= 2 && aprxDoomGain / cost > 1 |=> 600 -> "very sweet deal"
                    self.numSB >= 3 && aprxDoomGain / cost > 0.75 |=> 400 -> "sweet deal"
                    self.numSB >= 4 && aprxDoomGain / cost > 0.5 |=> 200 -> "ok deal"
                    cost == 5 |=> 100 -> "ritual first"
                    self.pool.goos.any |=> -200 -> "not all goos in play"
                    true |=> -250 -> "don't ritual unless have reasons"

                    !self.allSB |=> -1000 -> "spellbooks first"

                // 2026-11-13 (task: 6-spellbook behavior spec, item 13): "if ever more
                // than 30 doom from doom plus elder signs reveal to win immediately."
                // TB had no RevealESAction case at all -- every other faction's bot
                // (BotWW/BotYS/BotTI/etc.) already scores this; ported the same
                // standard pattern directly so TB reveals to break 30 Doom instead of
                // falling through to the generic "unknown" -1000 default.
                case RevealESAction(_, es, false, _) if self.es != es =>
                    true |=> -10000 -> "better reveal all"

                case RevealESAction(_, _, _, next) if next == DoomAction(self) =>
                    self.allSB && self.realDoom >= 30 |=> 1000 -> "reveal and try to win"
                    true |=> -100 -> "don't reveal"
                    canRitual |=> -2000 -> "ritual first"

                case RevealESAction(_, _, _, _) =>
                    self.allSB && self.realDoom >= 30 |=> 1000 -> "reveal and try to win"
                    self.allSB && self.realDoom < 30 && self.realDoom < self.aprxDoom && self.realDoom < others./(_.aprxDoom).max |=> 900 -> "reveal bad ESs to take off heat"
                    !self.allSB && self.realDoom >= 30 && others.all(!_.allSB) && others./(_.aprxDoom).max >= 27 |=> 800 -> "reveal so 30 broken and nobody wins"
                    true |=> -100 -> "don't reveal"

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
