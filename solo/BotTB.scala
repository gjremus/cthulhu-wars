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
// ============================================================================
object BotTB extends BotX(implicit g => new GameEvaluationTB)

class GameEvaluationTB(implicit game : Game) extends GameEvaluation(TB)(game) {
    def eval(a : Action) : $[Evaluation] = {
        var result : $[Evaluation] = $

        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        if (game.battle.none) {
            a match {

                case FirstPlayerAction(_, f) =>
                    f == self && allSB                          |=> 100  -> "play first all SB"
                    f == self && others.%(ofinale).any           |=> 200  -> "play first someone near win"
                    f == self                                    |=> -50  -> "stall"

                case PlayDirectionAction(_, order) =>
                    (order == game.factions)                     |=> 20   -> "natural order"

                case DoomDoneAction(f) =>
                    f == self                                    |=> 0    -> "done doom"

                case MoveAction(f, u, from, to, _) =>
                    u.cultist && to.freeGate                     |=> 1600 -> "cultist to free gate"
                    u.cultist && to.ownGate                      |=> 200  -> "cultist to own gate"
                    u.monster && to.foes.any && self.power > 3   |=> 500  -> "chthonian toward enemies"
                    u.goo && to.foes.any                         |=> 700  -> "shudde mell toward enemies"
                    to.freeGate                                  |=> 400  -> "move to free gate"

                case RecruitAction(f, uc, r) =>
                    uc == Cadavolyte && r.freeGate               |=> 700  -> "cadavolyte to free gate"
                    uc == Cadavolyte                             |=> 300  -> "recruit cadavolyte"
                    uc == Tentacle                               |=> 150  -> "recruit tentacle"

                case SummonAction(f, uc, r) =>
                    uc == Chthonian                              |=> 400  -> "summon chthonian"

                case BuildGateAction(f, r) =>
                    self.gates.num < 3                           |=> 900  -> "need more gates"
                    self.doom < 10                               |=> 500  -> "build gate for doom"

                case AttackAction(f, r, e, _) =>
                    self.strength(self.at(r), e) > e.strength(e.at(r), self) |=> 1200 -> "favorable battle"
                    e.at(r).goos.any                             |=> 700  -> "attack enemy goo"
                    e.at(r).num >= 2                             |=> 400  -> "attack cluster"

                // Thousand Writhing Maws: double recruit/summon
                case TBWrithingMawsMainAction(f) =>
                    true                                         |=> 600  -> "writhing maws double summon"
                case TBWrithingMawsTypeAction(f, uc) =>
                    uc == Chthonian                              |=> 500  -> "maws chthonians"
                    uc == Cadavolyte                             |=> 400  -> "maws cadavolytes"
                    uc == Tentacle                               |=> 200  -> "maws tentacles"
                case TBWrithingMawsPlaceFirstAction(f, _) =>
                    true                                         |=> 100  -> "maws place first"
                case TBWrithingMawsPlaceSecondAction(f, _, _) =>
                    true                                         |=> 100  -> "maws place second"
                case TBWrithingMawsAction(f, _, _, _) =>
                    true                                         |=> 300  -> "maws commit"

                // Behemoth: Move Part to Mantle
                case TBMovePartToMantleMainAction(f) =>
                    true                                         |=> 400  -> "behemoth move part to mantle"
                case TBMovePartToMantlePickAction(f, _) =>
                    true                                         |=> 100  -> "pick part for mantle"
                case TBMovePartToMantleAction(f, _) =>
                    true                                         |=> 200  -> "move part commit"

                // Stalk: post-move relocation
                case TBStalkMainAction(f, _, _) =>
                    true                                         |=> 300  -> "stalk relocation"
                case TBStalkUseAction(f, _, _) =>
                    true                                         |=> 400  -> "use stalk"
                case TBStalkSkipAction(f, _) =>
                    true                                         |=> 0    -> "skip stalk"
                case TBStalkPickCultistAction(f, _, _) =>
                    true                                         |=> 100  -> "pick stalk cultist"
                case TBStalkDestAction(f, _, _, _) =>
                    true                                         |=> 100  -> "pick stalk dest"
                case TBStalkAction(f, _, _, _) =>
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
                case TBAutotomyAction(f, _, _, _) =>
                    true                                         |=> 500  -> "autotomy commit"

                // Ensnare
                case TBEnsnareMainAction(f) =>
                    true                                         |=> 600  -> "ensnare enemy"
                case TBEnsnarePickEnemyAction(f, _) =>
                    true                                         |=> 100  -> "pick ensnare target"
                case TBEnsnareTargetAction(f, _, _) =>
                    true                                         |=> 200  -> "ensnare target commit"

                // Psychic Shriek
                case TBPsychicShriekMainAction(f) =>
                    true                                         |=> 500  -> "psychic shriek"
                case TBPsychicShriekPickEnemyAction(f, _) =>
                    true                                         |=> 100  -> "pick shriek target"
                case TBPsychicShriekTargetAction(f, _) =>
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

                // SBR-3: Remove Gate, Place Chthonian (end-of-Action-Phase prompt)
                case TBRemoveGatePlaceChthonianPromptAction(f, _) =>
                    self.gates.num > 2                           |=> 400  -> "remove gate place chthonian"
                case TBRemoveGatePlaceChthonianPickGateAction(f, _) =>
                    true                                         |=> 100  -> "pick gate to remove"
                case TBRemoveGatePlaceChthonianGateAction(f, _, _) =>
                    true                                         |=> 100  -> "gate removal commit"
                case TBRemoveGatePlaceChthonianPickAreaAction(f, _, _) =>
                    true                                         |=> 100  -> "pick chthonian placement"
                case TBRemoveGatePlaceChthonianAreaAction(f, _, _, _) =>
                    true                                         |=> 200  -> "chthonian placement commit"

                // SBR-4: Gates at every GOO Area
                case TBGatesAtGOOsMainAction(f) =>
                    true                                         |=> 700  -> "gates at goo areas SBR"
                case TBGatesAtGOOsAction(f) =>
                    true                                         |=> 500  -> "gates at goo commit"

                // SBR-6: Three Glyphs alt payment
                case TBThreeGlyphsPayAction(f) =>
                    true                                         |=> 600  -> "three glyphs pay 6"
                case TBThreeGlyphsPayConfirmAction(f) =>
                    true                                         |=> 400  -> "three glyphs confirm"

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
