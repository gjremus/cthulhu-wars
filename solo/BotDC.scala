package cws

import hrf.colmat._

object BotDC extends BotX(implicit g => new GameEvaluationDC)

class GameEvaluationDC(implicit game : Game) extends GameEvaluation(DC)(game) {
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
                    f == self && allSB                  |=> 100 -> "play first all SB"
                    f == self                           |=> -50 -> "stall"

                case PlayDirectionAction(_, order) =>
                    (order == game.factions)            |=> 20  -> "natural order"

                case DoomDoneAction(f) =>
                    f == self                           |=> 0   -> "done doom"

                case MoveAction(f, u, from, to, _) =>
                    u.cultist && to.freeGate            |=> 1600 -> "cultist to free gate"
                    u.cultist && to.ownGate             |=> 200  -> "cultist to own gate"
                    u.monster && to.ownGate             |=> 300  -> "monster to own gate"
                    u.goo && to.ownGate                 |=> 500  -> "goo to own gate"
                    to.foes.any && self.power > 3       |=> 800  -> "move toward enemies"
                    to.freeGate                         |=> 400  -> "move to free gate"

                case RecruitAction(f, uc, r) =>
                    uc == Acolyte && r.freeGate         |=> 600 -> "acolyte to free gate"

                case SummonAction(f, uc, r) =>
                    r.ownGate                           |=> 200 -> "summon at own gate"
                    r.foes.any                          |=> 100 -> "summon near foes"
                    uc == MindlessHusk                  |=> 50  -> "summon mindless husk"
                    uc == FallenProphet                 |=> 100 -> "summon fallen prophet"

                case BuildGateAction(f, r) =>
                    self.gates.num < 3                  |=> 800 -> "need more gates"
                    self.doom < 10                      |=> 600 -> "build gate for doom"

                case AttackAction(f, r, e, _) =>
                    self.strength(self.at(r), e) > e.strength(e.at(r), self) |=> 1200 -> "favorable battle"
                    e.at(r).goos.any                    |=> 800 -> "attack enemy goo"

                case AwakenAction(f, YgolonacDC, r, _) =>
                    self.needs(DarkBargainReq)          |=> 2000 -> "awaken Y'Golonac for SBR"
                    self.spellbooks.num <= 2            |=> 1500 -> "cheap awaken"
                    r.ownGate                           |=> 300  -> "awaken at own gate"

                case DCSatiateMainAction(f) =>
                    self.power >= 2                     |=> 1100 -> "use satiate"

                case DCLureMainAction(f) =>
                    self.power >= 1                     |=> 900  -> "use lure"

                case DCPilgrimageMainAction(f) =>
                    self.power >= 1                     |=> 700  -> "use pilgrimage"

                case DCDarkBargainMainAction(f) =>
                    true                                |=> 600  -> "use dark bargain"

                case DCDarkBargainChooseSinAction(f, face) =>
                    (face > 0)                          |=> face * 200 -> "dark bargain sin value"

                case DCProselytizeReqOptInAction(f) =>
                    self.needs(ProselytizeReq)          |=> 1500 -> "take Proselytize for SBR"

                case DCSatiateReqOptInAction(f) =>
                    self.needs(SatiateReq)              |=> 1500 -> "take Satiate for SBR"

                case DCPlaceReservedAcolyteAction(f, sb, r) =>
                    r.freeGate                          |=> 800 -> "place reserved acolyte at free gate"
                    r.ownGate                           |=> 200 -> "place reserved acolyte at own gate"

                case EndTurnAction(f) =>
                    f == self                           |=> 0 -> "end turn"

                case ControlGateAction(_, r, u, _) =>
                    val currentlyOnGate = self.at(r).%(_.onGate)
                    val isSwitch = currentlyOnGate.any && !currentlyOnGate.exists(_.ref == u)
                    !isSwitch                           |=> 1000 -> "control empty gate"
                    isSwitch                            |=> -1000000 -> "no-op swap (lockout in BotX.askE)"

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
                u.is(Acolyte)       |=> 800  -> "elim acolyte"
                u.is(MindlessHusk)  |=> 200  -> "elim husk"
                u.is(FallenProphet) |=> 400  -> "elim prophet"
                u.goo               |=> -500 -> "dont elim goo"
            }

            def retreat(u : UnitFigure) {
                u.gateKeeper        |=> -1000 -> "dont retreat gate keeper"
                u.is(Acolyte)       |=> 800   -> "retreat acolyte"
                u.is(MindlessHusk)  |=> 400   -> "retreat husk"
                u.is(FallenProphet) |=> 500   -> "retreat prophet"
                u.goo               |=> -5000 -> "dont retreat goo"
            }

            a match {
                case AssignKillAction(_, _, _, u) => elim(u)
                case AssignPainAction(_, _, _, u) => retreat(u)
                case RetreatUnitAction(_, u, r) =>
                    r.ownGate    |=> 1000 -> "retreat to own gate"
                    r.freeGate   |=> 800  -> "retreat to free gate"
                case _ =>
                    true |=> -1000 -> "unknown"
            }
        }

        result.none |=> 0 -> "none"
        true |=> -((1 + math.random() * 4).round.toInt) -> "random"

        result.sortBy(v => -v.weight.abs)
    }
}
