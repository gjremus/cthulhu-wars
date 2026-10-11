package cws

import hrf.colmat._

// ============================================================================
// TRIBULATION (DGT) BOT
// Simple baseline bot scoring for DGT faction.
// ============================================================================
object BotDGT extends BotX(implicit g => new GameEvaluationDGT)

class GameEvaluationDGT(implicit game : Game) extends GameEvaluation(DGT)(game) {
    def eval(a : Action) : $[Evaluation] = {
        var result : $[Evaluation] = $

        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        result ++= fbPromptedEvals(a)

        a.unwrap match {
            case MoveAction(_, u, from, to, _) =>
                fbMoveAvoidance(to).foreach(e => true |=> e)
            case BuildGateAction(_, r) =>
                hasFBCrater(r) |=> -8000 -> "cannot build gate on FB crater"
            case _ =>
        }

        if (game.battle.none) {
            a match {

                case FirstPlayerAction(_, f) =>
                    f == self && allSB                          |=> 100  -> "play first all SB"
                    f == self && others.%(ofinale).any          |=> 200  -> "play first someone near win"
                    f == self                                    |=> -50  -> "stall"

                case PlayDirectionAction(_, order) =>
                    (order == game.factions)                     |=> 20   -> "natural order"

                case DoomDoneAction(f) =>
                    f == self                                    |=> 0    -> "done doom"

                case MoveAction(f, u, from, to, _) =>
                    u.cultist && to.freeGate                     |=> 1600 -> "cultist to free gate"
                    u.cultist && to.ownGate                      |=> 200  -> "cultist to own gate"
                    u.monster && to.ownGate                      |=> 300  -> "monster to own gate"
                    to.foes.any && self.power > 3                |=> 600  -> "move toward enemies"
                    to.freeGate                                  |=> 400  -> "move to free gate"

                case RecruitAction(f, uc, r) =>
                    uc == Acolyte && r.freeGate                  |=> 600  -> "acolyte to free gate"
                    uc == Acolyte                                |=> 200  -> "recruit acolyte"

                case SummonAction(f, uc, r) =>
                    uc == Conquest && r.ownGate                  |=> 300  -> "summon conquest at gate"
                    uc == Conquest && r.foes.any                 |=> 250  -> "summon conquest near foes"
                    uc == Conquest                               |=> 150  -> "summon conquest"
                    uc == Pestilence                             |=> 800  -> "summon pestilence"
                    uc == War                                    |=> 800  -> "summon war"
                    uc == Famine                                 |=> 800  -> "summon famine"
                    uc == Death                                  |=> 800  -> "summon death"

                case BuildGateAction(f, r) =>
                    self.gates.num < 3                           |=> 800  -> "need more gates"
                    self.doom < 10                               |=> 500  -> "build gate for doom"

                case AttackAction(f, r, e, _) =>
                    self.strength(self.at(r), e) > e.strength(e.at(r), self) |=> 1200 -> "favorable battle"
                    e.at(r).goos.any                             |=> 700  -> "attack enemy goo"

                case DistributePowerMainAction(f) =>
                    self.needs(MartyrsCryReq)                    |=> 900  -> "distribute power for SBR"
                    true                                         |=> -200 -> "skip power distribution"

                case DistributePowerCancelAction(f) =>
                    true                                         |=> 100  -> "cancel power distribution"

                case DistributeDoomMainAction(f) =>
                    self.needs(CosmicChaosReq)                   |=> 900  -> "distribute doom for SBR"
                    true                                         |=> -200 -> "skip doom distribution"

                case DistributeDoomCancelAction(f) =>
                    true                                         |=> 100  -> "cancel doom distribution"

                case GodsPlagueMainAction(f) =>
                    self.power >= 2                              |=> 400  -> "use god's plague"
                    true                                         |=> -100 -> "skip god's plague"

                case EndTurnAction(f) =>
                    f == self                                    |=> 0    -> "end turn"

                case _ =>
            }
        }

        result
    }
}
