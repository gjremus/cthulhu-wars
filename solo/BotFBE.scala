package cws

import hrf.colmat._

// ============================================================================
// FACELESS BLIGHT (FBE) BOT (§3.16)
// Mirrors the BotBB structure (object extends BotX, class extends GameEvaluation).
// Scoring priorities (working defaults — tune via simulation, §2.10):
//  (1) Cluster-kill Actions (Self Consuming +Power/+Doom on 2+/3+ deaths).
//  (2) Byagoona Awaken timing (cheap Awakens when sac cost is high vs Power).
//  (3) Card-dice accumulation (Changeling Adherents, Animated Rush 2:1 carry).
//  (4) Necromantic Spores when 2+ enemy Kills are expected.
//  (5) Overlord of Death prefers Fungal-Thrall payment when Power is scarce.
// ============================================================================
object BotFBE extends BotX(implicit g => new GameEvaluationFBE)

class GameEvaluationFBE(implicit game : Game) extends GameEvaluation(FBE)(game) {
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
                    u.goo && to.foes.any                         |=> 700  -> "byagoona toward enemies"
                    to.foes.any && self.power > 3                |=> 600  -> "move toward enemies"
                    to.freeGate                                  |=> 400  -> "move to free gate"

                case RecruitAction(f, uc, r) =>
                    uc == Acolyte && r.freeGate                  |=> 600  -> "acolyte to free gate"
                    uc == Acolyte                                |=> 200  -> "recruit acolyte"

                case SummonAction(f, uc, r) =>
                    uc == FungalThrall && r.ownGate              |=> 300  -> "summon thrall at own gate"
                    uc == FungalThrall && r.foes.any             |=> 250  -> "summon thrall near foes"
                    uc == FungalThrall                           |=> 150  -> "summon thrall"

                case BuildGateAction(f, r) =>
                    self.gates.num < 3                           |=> 800  -> "need more gates"
                    self.doom < 10                               |=> 500  -> "build gate for doom"

                case AttackAction(f, r, e, _) =>
                    self.strength(self.at(r), e) > e.strength(e.at(r), self) |=> 1200 -> "favorable battle"
                    e.at(r).goos.any                             |=> 700  -> "attack enemy goo"
                    // Self Consuming reward: clustering enemy units to kill together.
                    e.at(r).num >= 2                             |=> 400  -> "cluster kill (self consuming)"

                // Byagoona custom Awaken — prefer when cheap (low Power balance owed).
                case ByagoonaAwakenMainAction(f) =>
                    true                                         |=> 1400 -> "awaken byagoona"
                case ByagoonaAwakenAreaAction(f, r) =>
                    r.foes.any                                   |=> 300  -> "awaken byagoona near foes"
                    true                                         |=> 200  -> "awaken byagoona here"
                case ByagoonaAwakenPickAction(f, r, picked, remaining) =>
                    // Sacrifice toward the 10-cost threshold; 2+ sacs trigger Self Consuming.
                    (picked.num >= 2)                            |=> 600  -> "sac 2+ for self consuming"
                    remaining.any && picked.num < 1              |=> 400  -> "need at least one sac"
                    picked.any                                   |=> 100  -> "enough to awaken"
                case ByagoonaAwakenDoneAction(f, r, picked) =>
                    picked.any                                   |=> 300  -> "awaken byagoona done"

                // Changeling Adherents dice accumulation — always good.
                case ChangelingAdherentsRollAction(f, _, _) =>
                    true                                         |=> 100  -> "changeling roll"

                // Necromantic Spores requirement (Eliminate Two Fungal Thralls).
                case EliminateTwoFungalThrallsMainAction(f) =>
                    self.needs(NecromanticSporesReq)             |=> 900  -> "eliminate two thralls for SBR"
                case EliminateTwoFungalThrallsPickAction(f, picked, remaining) =>
                    picked.num < 2                               |=> 300  -> "pick thrall"
                    picked.num == 2                              |=> 200  -> "two thralls picked"
                case EliminateTwoFungalThrallsDoneAction(f, picked) =>
                    true                                         |=> 200  -> "done two thralls"

                // Overlord of Death — only when Power is scarce and Thralls abundant.
                case OverlordOfDeathMainAction(f) =>
                    (self.power <= 1 && self.onMap(FungalThrall).num >= 3) |=> 300 -> "convert thrall to power"
                    true                                         |=> -200 -> "hold monsters"
                case OverlordOfDeathEliminateAction(f, _) =>
                    true                                         |=> 100  -> "overlord eliminate"

                // Animated Rush — carry units 2:1 along Byagoona's move.
                case AnimatedRushMainAction(f, _, _, n) =>
                    true                                         |=> (100 * n) -> "animated rush carry"
                case AnimatedRushSkipAction(f) =>
                    true                                         |=> 50   -> "skip animated rush"
                case AnimatedRushDestPickAction(f, _, _, _) =>
                    true                                         |=> 100  -> "pick rush destination"
                case AnimatedRushMoveAction(f, _, _, _, _) =>
                    true                                         |=> 100  -> "rush move unit"
                case AnimatedRushDoneEarlyAction(f) =>
                    true                                         |=> 50   -> "animated rush done early"

                // Succor — opt in if FBE has spare low-value units.
                case SuccorMainAction(f) =>
                    self.units.%(_.region.onMap).num >= 3        |=> 200  -> "succor for elder sign"
                    true                                         |=> -100 -> "skip succor (few units)"
                case SuccorSkipAction(f) =>
                    true                                         |=> 0    -> "skip succor"
                case SuccorPickAction(f, picked, remaining) =>
                    picked.num >= 2 && remaining.any             |=> 150  -> "succor pick more"
                    picked.any                                   |=> 50   -> "succor units picked"
                case SuccorRollStartAction(f, _) =>
                    true                                         |=> 50   -> "succor roll"

                case EndTurnAction(f) =>
                    f == self                                    |=> 0    -> "end turn"

                case ControlGateAction(_, r, u, _) =>
                    val currentlyOnGate = self.at(r).%(_.onGate)
                    val isSwitch = currentlyOnGate.any && !currentlyOnGate.exists(_.ref == u)
                    !isSwitch                                    |=> 1000 -> "control empty gate"
                    isSwitch                                     |=> -1000000 -> "no-op swap (lockout in BotX.askE)"

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
                u.is(FungalThrall)  |=> 300  -> "elim thrall"
                u.goo               |=> -500 -> "dont elim goo"
            }

            def retreat(u : UnitFigure) {
                u.gateKeeper        |=> -1000 -> "dont retreat gate keeper"
                u.is(Acolyte)       |=> 800   -> "retreat acolyte"
                u.is(FungalThrall)  |=> 500   -> "retreat thrall"
                u.goo               |=> -5000 -> "dont retreat goo"
            }

            a match {
                // Shapestealing — steal a costly enemy Monster if a card die is available.
                case ShapestealingPreBattleAction(f) =>
                    game.fbeCardDice.nonEmpty                    |=> 800  -> "use shapestealing"
                case ShapestealingSkipAction(f) =>
                    true                                         |=> 0    -> "skip shapestealing"
                case ShapestealingTargetAction(f, m) =>
                    val mon = game.unit(m)
                    (mon.uclass.cost <= 3)                       |=> 600  -> "shapesteal low-cost monster"
                    true                                         |=> 300  -> "shapesteal monster"

                // Distributed Death — prevent Kills on FBE units (prefer saving more).
                case DistributedDeathMainAction(f, n) =>
                    true                                         |=> (200 * n) -> "distributed death prevent"
                case DistributedDeathSkipAction(f) =>
                    true                                         |=> 0    -> "skip distributed death"

                // Necromantic Spores — spawn thralls per enemy killed.
                case NecromanticSporesMainAction(f, n) =>
                    (n >= 2)                                     |=> (300 * n) -> "necromantic spores (2+ kills)"
                    true                                         |=> 200  -> "necromantic spores"
                case NecromanticSporesSkipAction(f) =>
                    true                                         |=> -100 -> "skip necromantic spores"
                case NecromanticSporesEliminateAction(f, _, _, _) =>
                    true                                         |=> 200  -> "necromantic spores eliminate"

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
