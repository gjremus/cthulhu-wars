package cws

import hrf.colmat._

// ============================================================================
// Tcho-Tcho (TT) BOT: AI evaluation logic for TT-specific actions.
// Simple strategy: build gates, grow Ubbo-Sathla, use tribe abilities.
// ============================================================================
object BotTT extends BotX(implicit g => new GameEvaluationTT)

class GameEvaluationTT(implicit game : Game) extends GameEvaluation(TT)(game) {

    def eval(a : Action) : $[Evaluation] = {
        var result : $[Evaluation] = $

        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        val unwrapped = a.unwrap
        result ++= evalMain(unwrapped)
        // Non-TT factions can be asked about FB Cyclopean Gaze / Eye Opens prompts
        result ++= fbPromptedEvals(a)

        result.none |=> 0 -> "#TT none"
        true |=> (math.random() * 4).round.toInt -> "#TT random"

        result.sortBy(v => -v.weight.abs)
    }

    def evalMain(a : Action) : $[Evaluation] = {
        var result : $[Evaluation] = $

        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        val ubboUp    = self.onMap(UbboSathla).any
        val growth    = game.ubboGrowth
        val numGates  = self.gates.num
        val numSB     = self.spellbooks.num
        val needsUbbo = self.needs(TTAwakenUbboSathla)

        a @@ {
            // TRIBE SELECTION: prefer Leng for flexibility, Tsang for stability, Sarkomand for control
            case TTChooseTribeAction(_, TribeLeng)      => result +:= Evaluation(30, "#TT tribe-leng")
            case TTChooseTribeAction(_, TribeSarkomand) => result +:= Evaluation(28, "#TT tribe-sarkomand")
            case TTChooseTribeAction(_, TribeTsang)     => result +:= Evaluation(25, "#TT tribe-tsang")

            // HELL'S BANQUET: always fire (forced)
            case TTHellsBanquetRollAction(_) =>
                result +:= Evaluation(50, "#TT hellsbanquet")

            // UNSPEAKABLE OATH: sacrifice High Priest for 2 power when short on power
            case TTUnspeakableOathMainAction(_) =>
                (self.power < 3) |=> 20 -> "#TT oath-low-power"
                (self.power >= 3) |=> 5  -> "#TT oath-ok-power"

            case TTUnspeakableOathAction(_, _) =>
                result +:= Evaluation(15, "#TT oath-sacrifice")

            // AWAKEN UBBO-SATHLA: high priority when we can afford it and have a gate
            case TTAwakenUbboSathlaAction(_, r) =>
                (needsUbbo && numGates >= 2)    |=> 60  -> "#TT awaken-ubbo"
                (needsUbbo && numGates >= 1)    |=> 40  -> "#TT awaken-ubbo-1gate"
                (!needsUbbo)                    |=> 30  -> "#TT awaken-ubbo-again"

            // DARK RITUALS (Leng): take early, gives power and SBR
            case TTDarkRitualsMainAction(_) =>
                (self.power < 5 && !TTExpansion.darkRitualsFlipped) |=> 30 -> "#TT dark-rituals"

            // IDOLATRY (Tsang): free acolyte is almost always good
            case TTIdolatryMainAction(_) =>
                (self.pool(Acolyte).any && self.gates.any) |=> 25 -> "#TT idolatry"

            case TTIdolatryAction(_, r) =>
                // Prefer gates with enemies nearby (defence)
                val hasEnemy = game.factions.but(self).exists(_.at(r).any)
                (hasEnemy) |=> 10 -> "#TT idolatry-defend"
                result +:= Evaluation(5, "#TT idolatry-place")

            // MARTYRDOM (Tsang): only sacrifice if growth is low and Ubbo is up
            case TTMartyrdomMainAction(_) =>
                (ubboUp && growth < 4) |=> 20 -> "#TT martyrdom-grow"

            case TTMartyrdomAction(_, _) =>
                result +:= Evaluation(10, "#TT martyrdom-sacrifice")

            // DOOMSDAY (Sarkomand): fire when leading
            case TTDoomsdayMainAction(_) =>
                val leadCount = game.factions.but(self).count(e => self.doom > e.doom)
                (leadCount >= 2) |=> 40 -> "#TT doomsday-strong"
                (leadCount == 1) |=> 20 -> "#TT doomsday-weak"

            // REMOVE GATE (SBR trigger)
            case TTRemoveGateMainAction(_) =>
                (self.needs(TTRemoveControlledGate)) |=> 35 -> "#TT remove-gate-sbr"

            case TTRemoveGateAction(_, _) =>
                result +:= Evaluation(25, "#TT remove-gate")

            case _ =>
        }

        result
    }
}
