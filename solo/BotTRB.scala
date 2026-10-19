package cws

import hrf.colmat._

// ============================================================================
// TRIBULATION v16 (TRB) BOT
// Simple baseline bot scoring for the TRB faction (designer Daganisoraan,
// fid lklqd2fbdudf). Deliberately minimal — the manual only requires a bot
// that never crashes; TRB's generic action evaluation (move/recruit/battle/
// summon/etc.) already comes for free from GameEvaluation. The only nudges
// added here are safe, low-risk heuristics distinct from DGT's own bot (no
// shared code/identifiers with BotDGT.scala).
// ============================================================================
object BotTRB extends BotX(implicit g => new GameEvaluationTRB)

class GameEvaluationTRB(implicit game : Game) extends GameEvaluation(TRB)(game) {
    def eval(a : Action) : $[Evaluation] = {
        var result : $[Evaluation] = $

        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        a.unwrap match {
            // Mild nudge toward summoning the Horsemen (Terrors) once affordable —
            // they are TRB's main combat strength (3 dice each vs Conquest's 1).
            case SummonAction(_, uc, _) if uc == TRBPestilence || uc == TRBWar || uc == TRBFamine || uc == TRBDeath =>
                true |=> 30 -> "summon a Horseman"

            // Prefer completing an earnable SBR when a Distribute action is on offer.
            case TRBDistributePowerMainAction(_) =>
                self.needs(TRBMartyrsCryReq) |=> 15 -> "work toward Martyr's Cry"
            case TRBDistributeDoomMainAction(_) =>
                self.needs(TRBCosmicChaosReq) |=> 15 -> "work toward Cosmic Chaos"

            // When offered a free sacrifice to flip The Black Rider, take it unless it
            // would hand the enemy their own cultist back empty-handed and TRB is low power
            // (sacrifice is strictly a flip + Elder Sign + possible Martyr's Cry Power gain).
            case TRBBlackRiderSacrificeAction(_, _, _) =>
                true |=> 10 -> "flip Black Rider"
            case TRBBlackRiderSkipAction(_, _) =>
                true |=> -5 -> "skip Black Rider flip"

            case _ =>
        }

        result
    }
}
