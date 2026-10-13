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

                case ControlGateAction(_, r, u, _) =>
                    val currentlyOnGate = self.at(r).%(_.onGate)
                    val isSwitch = currentlyOnGate.any && !currentlyOnGate.exists(_.ref == u)
                    !isSwitch                                    |=> 1000 -> "control empty gate"
                    isSwitch                                    |=> -1000000 -> "no-op swap"

                case AbandonGateAction(_, r, _) =>
                    true                                         |=> -1000000 -> "never abandon gate"

                case _ =>
                    true                                         |=> -1000 -> "unknown"
            }
        }
        else {
            implicit val battle = game.battle.get
            val allies  = self.forces
            val enemies = self.opponent.forces

            def elim(u : UnitFigure) {
                u.is(Acolyte)          |=> 800  -> "elim acolyte"
                u.is(Conquest)         |=> 600  -> "elim conquest"
                u.is(Pestilence)       |=> 400  -> "elim pestilence"
                u.is(War)              |=> 400  -> "elim war"
                u.is(Famine)           |=> 400  -> "elim famine"
                u.is(Death)            |=> 400  -> "elim death"
            }

            def retreat(u : UnitFigure) {
                u.gateKeeper           |=> -1000 -> "dont retreat gate keeper"
                u.is(Acolyte)          |=> 800   -> "retreat acolyte"
                u.is(Conquest)         |=> 500   -> "retreat conquest"
                u.is(Pestilence)       |=> -500  -> "dont retreat pestilence"
                u.is(War)              |=> -500  -> "dont retreat war"
                u.is(Famine)           |=> -500  -> "dont retreat famine"
                u.is(Death)            |=> -500  -> "dont retreat death"
            }

            a match {
                case RetreatUnitAction(f, u, r) => retreat(u)

                case GodsPlagueEnemyPickAction(f, ef, r, priorPicked) =>
                    true                         |=> 0    -> "god's plague pick"

                case GodsPlagueChooseDestAction(f, u, painedUnits) =>
                    true                         |=> 0    -> "god's plague dest"

                case RampageSacrificeAction(f, cultist, last) =>
                    true                         |=> 100  -> "rampage sacrifice"
                case RampageSkipAction(f, last) =>
                    true                         |=> 0    -> "skip rampage"

                case DistributePowerFactionAction(f, remaining, distributed) =>
                    true                         |=> 0    -> "distribute power pick faction"

                case DistributePowerAmountAction(f, target, remaining, distributed) =>
                    remaining > 0                |=> 100  -> "distribute power amount"
                    true                         |=> 0    -> "zero power remaining"

                case DistributeDoomFactionAction(f, remaining, distributed) =>
                    true                         |=> 0    -> "distribute doom pick faction"

                case DistributeDoomAmountAction(f, target, remaining, distributed) =>
                    remaining > 0                |=> 100  -> "distribute doom amount"
                    true                         |=> 0    -> "zero doom remaining"

                case GodsScaleMainAction(f, then) =>
                    true                         |=> 300  -> "use god's scale"
                case GodsScaleSkipAction(f, then) =>
                    true                         |=> 0    -> "skip god's scale"

                case _ =>
            }
        }

        result
    }
}
