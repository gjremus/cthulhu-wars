package cws

import hrf.colmat._

// ============================================================================
// PERMEATION (PRM) BOT
// Simple bot logic for PRM faction. Scoring priorities:
//  (1) Awaken Ycgn'àgnnisssz when possible
//  (2) Use spellbooks strategically (Protogenesis for board presence, Osmosis for disruption)
//  (3) Build gates in land areas (for Control 2 Land Gates SBR)
//  (4) Recruit and summon for board control
//  (5) Battle favorably
// ============================================================================
object BotPRM extends BotX(implicit g => new GameEvaluationPRM)

class GameEvaluationPRM(implicit game : Game) extends GameEvaluation(PRM)(game) {
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
                    u.monster && to.ownGate                      |=> 300  -> "monster to own gate"
                    u.goo && to.foes.any                         |=> 700  -> "ycgna toward enemies"
                    to.foes.any && self.power > 3                |=> 600  -> "move toward enemies"
                    to.freeGate                                  |=> 400  -> "move to free gate"

                case RecruitAction(f, uc, r) =>
                    uc == Acolyte && r.freeGate                  |=> 600  -> "acolyte to free gate"
                    uc == Acolyte                                |=> 200  -> "recruit acolyte"

                case SummonAction(f, uc, r) =>
                    uc == AstralEgg && r.ownGate                 |=> 300  -> "summon egg at own gate"
                    uc == AstralEgg && r.foes.any                |=> 250  -> "summon egg near foes"
                    uc == AstralEgg                              |=> 150  -> "summon egg"
                    uc == CelestialFledgling && r.ownGate        |=> 280  -> "summon fledgling at gate"
                    uc == CelestialFledgling                     |=> 130  -> "summon fledgling"

                case BuildGateAction(f, r) =>
                    r.glyph != Ocean && self.gates.num < 2       |=> 900  -> "need land gates for SBR"
                    r.glyph != Ocean                             |=> 600  -> "build gate in land"
                    self.gates.num < 3                           |=> 800  -> "need more gates"
                    self.doom < 10                               |=> 500  -> "build gate for doom"

                case AttackAction(f, r, e, _) =>
                    self.strength(self.at(r), e) > e.strength(e.at(r), self) |=> 1200 -> "favorable battle"
                    e.at(r).goos.any                             |=> 700  -> "attack enemy goo"

                // Ycgn'àgnnisssz custom Awaken
                case YcgnaAwakenMainAction(f) =>
                    true                                         |=> 1400 -> "awaken ycgna"
                case YcgnaAwakenPickEggAction(f, eggs) =>
                    eggs.any                                     |=> 300  -> "pick egg to replace"
                case YcgnaAwakenAction(f, egg) =>
                    true                                         |=> 400  -> "awaken ycgna action"

                // Atomic Unity - eliminate monster for power
                case AtomicUnityMainAction(f) =>
                    self.power < 3 && self.allInPlay.%(_.uclass.utype == Monster).any |=> 400 -> "need power"
                case AtomicUnityPickMonsterAction(f, monsters) =>
                    monsters.any                                 |=> 200  -> "pick monster"
                case AtomicUnityAction(f, monster) =>
                    true                                         |=> 300  -> "atomic unity"

                // Eldritch Love - gain ES if 2+ face-down SBs
                case EldritchLoveAction(f) =>
                    game.prmFlippedDownSpellbooks.num >= 2       |=> 800  -> "gain ES from flipped SBs"

                // Osmosis - redistribute units
                case OsmosisMainAction(f) =>
                    true                                         |=> 200  -> "osmosis"
                case _ : OsmosisPickFirstAreaAction =>
                    true                                         |=> 100  -> "pick first area"
                case _ : OsmosisPickSecondAreaAction =>
                    true                                         |=> 100  -> "pick second area"
                case _ : OsmosisPlaceUnitAction =>
                    true                                         |=> 100  -> "place unit"

                // Protogenesis - place lowest-cost monster
                case ProtogenesisMainAction(f) =>
                    self.pool.%(_.uclass.utype == Monster).any   |=> 500  -> "protogenesis place monster"
                case ProtogenesisPickAreaAction(f, monster) =>
                    true                                         |=> 200  -> "pick area for monster"
                case ProtogenesisAction(f, monster, dest) =>
                    dest.ownGate                                 |=> 300  -> "place at own gate"
                    dest.foes.any                                |=> 250  -> "place near foes"
                    true                                         |=> 200  -> "place monster"

                // Speak Reality - force enemy action
                case SpeakRealityMainAction(f) =>
                    self.power >= 3                              |=> 300  -> "speak reality"
                case _ : SpeakRealityChoosePowerAction =>
                    true                                         |=> 100  -> "choose power"
                case _ : SpeakRealityChooseFactionAction =>
                    true                                         |=> 100  -> "choose faction"
                case _ : SpeakRealityChooseActionAction =>
                    true                                         |=> 100  -> "choose action"

                // Zealous Spark - move any monster
                case ZealousSparkMainAction(f) =>
                    self.doom >= 2                               |=> 400  -> "zealous spark"
                case ZealousSparkPickMonsterAction(f, monsters) =>
                    monsters.any                                 |=> 200  -> "pick monster to move"
                case ZealousSparkPickDestAction(f, monster, areas) =>
                    areas.any                                    |=> 200  -> "pick destination"
                case ZealousSparkAction(f, monster, dest) =>
                    dest.foes.any                                |=> 300  -> "move toward foes"
                    dest.freeGate                                |=> 250  -> "move to free gate"
                    true                                         |=> 200  -> "move monster"

                // Spellbook requirement: Pay 2 Doom and Flip
                case Pay2DoomFlipSpellbookMainAction(f) =>
                    self.doom >= 2 && self.needs(Pay2DoomFlipSpellbookReq) |=> 700 -> "fulfill SBR"
                case Pay2DoomFlipSpellbookPickAction(f, sbs) =>
                    sbs.any                                      |=> 300  -> "pick SB to flip"
                case Pay2DoomFlipSpellbookAction(f, sb) =>
                    true                                         |=> 400  -> "flip SB"

                // Make as One - enemy activating our spellbooks
                case MakeAsOneActivateAction(f, enemy, sb) =>
                    true                                         |=> 100  -> "enemy activates our SB"
                case MakeAsOneSkipAction(f, enemy) =>
                    true                                         |=> -50  -> "skip make as one"

                // Mother's Aid - opponent controlling Ycgn'àgnnisssz
                case MothersAidActivateAction(f, enemy) =>
                    true                                         |=> -200 -> "don't give ycgna to enemy"

                case _ =>
            }
        }
        else {
            // In battle
            a.unwrap match {
                case AssignKillAction(_, _, _, u) =>
                    u.goo                                        |=> 1000 -> "kill goo"
                    u.monsterly                                  |=> 500  -> "kill monster"
                    u.cultist                                    |=> 100  -> "kill cultist"

                case AssignPainAction(_, _, _, u) =>
                    u.monsterly                                  |=> 300  -> "pain monster"
                    u.cultist                                    |=> 100  -> "pain cultist"

                case DevourAction(_, u) =>
                    u.goo                                        |=> 900  -> "devour goo"
                    u.monsterly                                  |=> 400  -> "devour monster"

                case CaptureAction(_, _, f, _) =>
                    f.cultists.any                               |=> 200  -> "capture cultist"

                case _ =>
            }
        }

        result
    }
}
