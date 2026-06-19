package cws

import hrf.colmat._

// ============================================================================
// XYRIOUS STORM (XSS) BOT
// Mirrors the BotFBE structure (object extends BotX, class extends GameEvaluation).
// Scoring priorities (working defaults — tune via simulation):
//  (1) Gate construction/control in Sea AND Land areas (Sea Gates / Land Gates SBRs).
//  (2) Petrichor Awaken timing (cost 8 — requires Cost-3+ Unit present).
//  (3) Tsunami mobility for positioning Eyes of the Storm.
//  (4) Static Accumulator pre-battle reinforcement.
//  (5) Cloud Of Ashes Monster recovery at Doom.
// ============================================================================
object BotXSS extends BotX(implicit g => new GameEvaluationXSS)

class GameEvaluationXSS(implicit game : Game) extends GameEvaluation(XSS)(game) {
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
                    u.monster && to.foes.any && self.power > 3   |=> 500  -> "monster toward enemies"
                    u.goo && to.foes.any                         |=> 700  -> "petrichor toward enemies"
                    to.freeGate                                  |=> 400  -> "move to free gate"

                case RecruitAction(f, uc, r) =>
                    uc == Acolyte && r.freeGate                  |=> 600  -> "acolyte to free gate"
                    uc == Acolyte                                |=> 200  -> "recruit acolyte"

                case SummonAction(f, uc, r) =>
                    uc == Twister && r.glyph != Ocean            |=> 500  -> "summon twister in land (combat 3)"
                    uc == Twister                                |=> 300  -> "summon twister"
                    uc == EyeOfTheStorm && r.glyph == Ocean      |=> 600  -> "summon eye in sea (combat 4)"
                    uc == EyeOfTheStorm                          |=> 350  -> "summon eye"
                    uc == AmphibianCrawler                       |=> 150  -> "summon amphibian crawler"

                case BuildGateAction(f, r) =>
                    self.gates.num < 3                           |=> 800  -> "need more gates"
                    self.doom < 10                               |=> 500  -> "build gate for doom"

                case AttackAction(f, r, e, _) =>
                    self.strength(self.at(r), e) > e.strength(e.at(r), self) |=> 1200 -> "favorable battle"
                    e.at(r).goos.any                             |=> 700  -> "attack enemy goo"
                    e.at(r).num >= 2                             |=> 400  -> "attack cluster"

                // Tsunami — move Eye of the Storm from Sea to Land
                case TsunamiMainAction(f) =>
                    true                                         |=> 600  -> "tsunami positioning"
                case TsunamiEyePickAction(f, _) =>
                    true                                         |=> 100  -> "pick eye for tsunami"
                case TsunamiDestPickAction(f, _, _) =>
                    true                                         |=> 100  -> "pick tsunami destination"
                case TsunamiExtrasPickAction(f, _, _, _, picked, remaining) =>
                    remaining.any                                |=> 200  -> "bring extras along"
                    picked.any                                   |=> 100  -> "tsunami extras picked"
                case TsunamiAction(f, _, _, _, extras) =>
                    extras.any                                   |=> 300  -> "tsunami with extras"
                    true                                         |=> 200  -> "tsunami commit"

                // Static Accumulator — pre-battle reinforcement (multi-source)
                case StaticAccumulatorPreBattleMainAction(f) =>
                    true                                         |=> 500  -> "static accumulator reinforce"
                case StaticAccumulatorSkipAction(f) =>
                    true                                         |=> 0    -> "skip static accumulator"
                case StaticAccumulatorUnitPickAction(f, _, picked, remaining, _) =>
                    remaining.any && picked.num < 2              |=> 200  -> "pick more units"
                    picked.any                                   |=> 100  -> "units picked"
                case StaticAccumulatorDoneAction(f, _, picked) =>
                    picked.any                                   |=> 300  -> "confirm static accumulator"

                // Cloud Of Ashes — Doom phase return
                case CloudOfAshesDoomReturnMainAction(f) =>
                    true                                         |=> 800  -> "return monster from card"
                case CloudOfAshesDoomReturnPickAction(f, _) =>
                    true                                         |=> 100  -> "pick monster to return"
                case CloudOfAshesDoomReturnAreaAction(f, _) =>
                    true                                         |=> 100  -> "pick return area"
                case CloudOfAshesDoomReturnAction(f, _, dest) =>
                    true                                         |=> 200  -> "return monster commit"

                // Cloud Of Ashes — hold/decline on kill
                case CloudOfAshesHoldAction(f, _) =>
                    true                                         |=> 400  -> "hold monster on card"
                case CloudOfAshesDeclineAction(f, _) =>
                    true                                         |=> -100 -> "decline hold (return to pool)"

                // Distant Thunderclap — optional excess pain self-assignment
                case DistantThunderclapOfferAction(f, _, _, _, _) =>
                    true                                         |=> 500  -> "use thunderclap"
                case DistantThunderclapSkipAction(f) =>
                    true                                         |=> -100 -> "skip thunderclap"
                case DistantThunderclapPainAction(f, _, _, _, _, _) =>
                    true                                         |=> 0    -> "thunderclap pain"
                case DistantThunderclapPainTargetAction(f, target, _, _, _, _, _) =>
                    val u = game.unit(target)
                    (u.uclass == AmphibianCrawler)               |=> 500  -> "pain cheapest unit"
                    (u.uclass == Twister)                        |=> 200  -> "pain twister"
                    (u.uclass == EyeOfTheStorm)                  |=> -100 -> "avoid paining eye"
                    (u.uclass == Petrichor)                      |=> -5000 -> "never pain petrichor"

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
                u.is(AmphibianCrawler)   |=> 400  -> "elim crawler"
                u.is(Twister)            |=> -200 -> "dont elim twister"
                u.is(EyeOfTheStorm)      |=> -400 -> "dont elim eye"
                u.goo                    |=> -500 -> "dont elim goo"
            }

            def retreat(u : UnitFigure) {
                u.gateKeeper             |=> -1000 -> "dont retreat gate keeper"
                u.is(Acolyte)            |=> 800   -> "retreat acolyte"
                u.is(AmphibianCrawler)   |=> 600   -> "retreat crawler"
                u.is(Twister)            |=> -200  -> "dont retreat twister"
                u.is(EyeOfTheStorm)      |=> -400  -> "dont retreat eye"
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
