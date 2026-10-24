package cws

import hrf.colmat._

// ============================================================================
// SPHINX WORSHIPPERS (SW) BOT
// Basic bot logic for the Sphinx Worshippers faction (designer lhhz3sjhivcz).
// Priorities: build up Hybrid Mummies, awaken Black Sphinx when affordable,
// use Kidnap post-battle, keep the Hunger sacrifice obligation manageable.
// ============================================================================
object BotSW extends BotX(implicit g => new GameEvaluationSW)

class GameEvaluationSW(implicit game : Game) extends GameEvaluation(SW)(game) {
    def eval(a : Action) : $[Evaluation] = {
        var result : $[Evaluation] = $

        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        result ++= fbPromptedEvals(a)

        if (game.battle.none) {
            a match {

                case FirstPlayerAction(_, f) =>
                    f == self && allSB                          |=> 100  -> "play first all SB"
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
                    uc == HybridMummy && r.ownGate               |=> 350  -> "summon hybrid mummy"
                    uc == PharoahKephren && r.ownGate            |=> 400  -> "summon pharoah kephren"
                    uc == QueenNitokris && r.ownGate             |=> 450  -> "summon queen nitokris"

                case AttackAction(f, r, e, _) =>
                    self.strength(self.at(r), e) > e.strength(e.at(r), self) |=> 1200 -> "favorable battle"

                case SWAwakenSphinxMainAction(f) =>
                    true                                        |=> 700  -> "awaken black sphinx"
                case SWAwakenSphinxRegionAction(f, r) =>
                    true                                        |=> 100  -> "pick awaken region"
                case SWAwakenSphinxSacrificeAction(f, r, v) =>
                    true                                        |=> 50   -> "sacrifice for black sphinx"

                case SWBottomlessPitMainAction(f, _) =>
                    f == self                                   |=> 300  -> "use bottomless pit"
                case SWAncientEmeticMainAction(f, _) =>
                    f == self                                   |=> 500  -> "use ancient emetic"

                case SWStarvationUseAction(f) =>
                    true                                        |=> 300  -> "use starvation discount"
                case SWStarvationSkipAction(f) =>
                    true                                        |=> 0    -> "skip starvation"

                case SWHungerSacrificeDoomAction(_) =>
                    self.doom < 20                               |=> 400  -> "pay hunger with doom"
                    true                                         |=> 100  -> "pay hunger with doom"
                case SWHungerSacrificeCultistAction(_, _) =>
                    true                                         |=> 200  -> "pay hunger with cultist"

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
            a match {
                case RetreatUnitAction(f, u, r) =>
                    u.is(Acolyte)          |=> 800   -> "retreat acolyte"
                    u.is(HybridMummy)      |=> 400   -> "retreat hybrid mummy"
                    u.is(PharoahKephren)   |=> 300   -> "retreat pharoah kephren"

                case SWKidnapOfferAction(f, _) =>
                    true                   |=> 700   -> "accept kidnap offer"
                case SWKidnapDeclineAction(f, _) =>
                    true                   |=> 0     -> "decline kidnap offer"

                case SWKidnapAction(f, _, _) =>
                    true                   |=> 600   -> "kidnap enemy monster"
                case SWKidnapSkipAction(f, _) =>
                    true                   |=> 0     -> "skip kidnap"

                case SWMummiesCurseAction(f, _) =>
                    true                   |=> 300   -> "use mummies curse"
                case SWMummiesCurseSkipAction(f) =>
                    true                   |=> 0     -> "skip mummies curse"

                case SWLifeFromDeathAction(f, _) =>
                    self.power < 3          |=> 300   -> "use life from death when low on units"
                    true                    |=> 0     -> "skip life from death by default"
                case SWLifeFromDeathSkipAction(f) =>
                    true                    |=> 50    -> "skip life from death"

                case _ =>
            }
        }

        result
    }
}
