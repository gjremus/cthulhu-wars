package cws

import hrf.colmat._

// ============================================================================
// FLESHMASON (FM) BOT
// Basic bot logic for the Fleshmason faction (designer dcylxgn1wkqs).
// Priorities: build Screaming Walls, use Manned Battlements for mobility,
// accumulate Walls for spellbook requirements.
// ============================================================================
object BotFM extends BotX(implicit g => new GameEvaluationFM)

class GameEvaluationFM(implicit game : Game) extends GameEvaluation(FM)(game) {
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
                    f == self                                   |=> -50  -> "stall"

                case PlayDirectionAction(_, order) =>
                    (order == game.factions)                    |=> 20   -> "natural order"

                case DoomDoneAction(f) =>
                    f == self                                   |=> 0    -> "done doom"

                case MoveAction(f, u, from, to, _) =>
                    u.cultist && to.freeGate                    |=> 1600 -> "cultist to free gate"
                    u.cultist && to.ownGate                     |=> 200  -> "cultist to own gate"
                    u.monster && to.ownGate                     |=> 300  -> "monster to own gate"
                    to.foes.any && self.power > 3               |=> 600  -> "move toward enemies"
                    to.freeGate                                 |=> 400  -> "move to free gate"

                case RecruitAction(f, uc, r) =>
                    uc == Acolyte && r.freeGate                 |=> 600  -> "acolyte to free gate"
                    uc == Acolyte                               |=> 200  -> "recruit acolyte"

                case SummonAction(f, uc, r) =>
                    uc == BoneSculptor && r.ownGate             |=> 300  -> "summon bone sculptor"
                    uc == MuscleStitcher && r.ownGate           |=> 350  -> "summon muscle stitcher"
                    uc == BodyArchitect && r.ownGate            |=> 400  -> "summon body architect"

                case BuildGateAction(f, r) =>
                    self.gates.num < 3                          |=> 800  -> "need more gates"
                    self.doom < 10                              |=> 500  -> "build gate for doom"

                case AttackAction(f, r, e, _) =>
                    self.strength(self.at(r), e) > e.strength(e.at(r), self) |=> 1200 -> "favorable battle"

                case CreateScreamingWallMainAction(f) =>
                    true                                        |=> 700  -> "create screaming wall"
                case CreateScreamingWallAction(f, r) =>
                    r.foes.any                                  |=> 300  -> "wall near foes"
                    true                                        |=> 200  -> "create wall here"

                case MannedBattlementsMainAction(f) =>
                    self.has(MannedBattlements)                 |=> 400  -> "use manned battlements"
                case MannedBattlementsSkipAction(f) =>
                    true                                        |=> -100 -> "skip battlements"

                case HymnOfSufferingMainAction(f) =>
                    self.has(HymnOfSuffering)                   |=> 300  -> "use hymn of suffering"
                case HymnOfSufferingSkipAction(f) =>
                    true                                        |=> 0    -> "skip hymn"

                case CrumblingYawpMainAction(f) =>
                    self.has(CrumblingYawp)                     |=> 200  -> "use crumbling yawp"
                case CrumblingYawpSkipAction(f) =>
                    true                                        |=> 0    -> "skip crumbling yawp"

                case TendonFlayingReqMainAction(f, cost) =>
                    self.power >= cost && self.needs(TendonFlayingReq) |=> 800 -> "pay for tendon flaying req"

                case EndTurnAction(f) =>
                    f == self                                   |=> 0    -> "end turn"

                case ControlGateAction(_, r, u, _) =>
                    val currentlyOnGate = self.at(r).%(_.onGate)
                    val isSwitch = currentlyOnGate.any && !currentlyOnGate.exists(_.ref == u)
                    !isSwitch                                   |=> 1000 -> "control empty gate"
                    isSwitch                                    |=> -1000000 -> "no-op swap"

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
                u.is(Acolyte)          |=> 800  -> "elim acolyte"
                u.is(BoneSculptor)     |=> 400  -> "elim bone sculptor"
                u.is(MuscleStitcher)   |=> 300  -> "elim muscle stitcher"
                u.is(BodyArchitect)    |=> 200  -> "elim body architect"
            }

            def retreat(u : UnitFigure) {
                u.gateKeeper           |=> -1000 -> "dont retreat gate keeper"
                u.is(Acolyte)          |=> 800   -> "retreat acolyte"
                u.is(BoneSculptor)     |=> 500   -> "retreat bone sculptor"
                u.is(MuscleStitcher)   |=> 400   -> "retreat muscle stitcher"
                u.is(BodyArchitect)    |=> -500  -> "dont retreat body architect"
            }

            a match {
                case RetreatUnitAction(f, u, r) => retreat(u)

                case SpitefulChiselReturnAction(f, _) =>
                    true                         |=> 500  -> "return captured acolyte"
                case SpitefulChiselSkipAction(f) =>
                    true                         |=> 0    -> "skip spiteful chisel"

                case TendonFlayingOption1Action(f, _, _, _) =>
                    true                         |=> 100  -> "tendon flaying power option 1"
                case TendonFlayingOption2Action(f, _, _, _) =>
                    true                         |=> 100  -> "tendon flaying power option 2"
                case TendonFlayingOption3Action(f, _, _, _) =>
                    self.doom < 25               |=> 200  -> "tendon flaying doom"
                    true                         |=> 50   -> "tendon flaying doom"

                case _ =>
            }
        }

        result
    }
}
