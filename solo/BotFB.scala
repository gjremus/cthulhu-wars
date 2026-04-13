package cws

import hrf.colmat._


// ============================================================================
// Firstborn (FB) BOT: AI evaluation logic for all FB-specific actions.
// Each action is scored with positive values for good plays and negative for bad.
// The BotX framework picks the highest-scored action.
// ============================================================================
object BotFB extends BotX(implicit g => new GameEvaluationFB)

class GameEvaluationFB(implicit game : Game) extends GameEvaluation(FB)(game) {
    def eval(a : Action) : $[Evaluation] = {
        var result : $[Evaluation] = $

        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        val unwrapped = a.unwrap

        val desiccatedOnMap = self.onMap(Desiccated).num
        val revenantOnMap = self.onMap(RevenantOfKnaa).num
        val gooOnMap = self.onMap(Ghatanothoa).num
        val craterCount = game.fbCraters.num
        val auguryKills = game.fbAuguryKills
        val infernalDiscount = game.fbInfernalPactDiscount

        val earlyGame = numSB < 3 && !have(Ghatanothoa)
        val lateGame = !earlyGame
        val firstAP = game.turn == 1
        val secondAP = game.turn == 2

        unwrapped match {

            // ── WRITHE ── Score Writhe activation based on power level and need for Desiccated
            case FBWritheMainAction(_) =>
                (power >= 4) |=> 3000 -> "writhe with enough power for good dice"
                (power >= 2 && power < 4) |=> 1000 -> "writhe with limited dice"
                (desiccatedOnMap < 3 && self.pool(Desiccated).any) |=> 2000 -> "writhe to generate desiccated"
                earlyGame |=> 1500 -> "early writhe for movement and desiccated"

            // ── WRITHE DICE DECISIONS ── Score reroll vs keep, kill/pain unit targeting, and relocation
            case FBWritheRerollAction(_, _) =>
                true |=> 500 -> "reroll is usually good"

            case FBWritheKeepAction(_, rolls) =>
                val kills = rolls.count(_ == Kill)
                val pains = rolls.count(_ == Pain)
                (kills + pains >= rolls.num / 2) |=> 1000 -> "keep good rolls"
                (kills + pains < rolls.num / 3) |=> -500 -> "bad rolls, prefer reroll"

            case FBWritheKillUnitAction(_, uRef, _, _) =>
                val u = game.unit(uRef)
                (u.uclass == Acolyte && self.pool(Desiccated).any) |=> 3000 -> "kill acolyte to get desiccated"
                (u.uclass == Desiccated) |=> -1000 -> "avoid killing desiccated"
                (u.uclass == RevenantOfKnaa) |=> -2000 -> "avoid killing revenant"
                (u.uclass == Ghatanothoa) |=> -5000 -> "avoid killing goo"

            case FBWritheChoosePainUnitAction(_, uRef, _, _) =>
                val u = game.unit(uRef)
                (u.uclass == Desiccated) |=> 500 -> "writhe desiccated"
                (u.uclass == Acolyte) |=> 1000 -> "writhe acolyte for mobility"
                true |=> 0 -> "pain unit selection"

            case FBWritheMoveAllToRegionAction(_, r, _) =>
                r.enemyGate |=> 2000 -> "writhe all to enemy gate"
                r.foes.cultists.any && r.foes.monsterly.none |=> 1500 -> "writhe all to lone cultists"
                r.ownGate |=> -500 -> "don't writhe all to own gate"

            case FBWritheMoveSeparatelyAction(_, _) =>
                true |=> 1000 -> "move separately for flexibility"

            case FBWritheMoveOneToRegionAction(_, _, r, _) =>
                r.enemyGate |=> 1500 -> "writhe unit to enemy gate"
                r.foes.cultists.any && r.foes.monsterly.none |=> 2000 -> "writhe to area with lone cultists"

            // Bug fix Round 6: score the "join" variant identically to normal region move
            case FBWritheMoveOneJoinAction(_, _, r, _, _) =>
                r.enemyGate |=> 1500 -> "writhe unit to enemy gate"
                r.foes.cultists.any && r.foes.monsterly.none |=> 2000 -> "writhe to area with lone cultists"
                true |=> 500 -> "join previous unit in same region"

            // ── INFERNAL PACT ── Score when to use GOO discount and which spellbooks to flip
            case FBInfernalPactMainAction(_) =>
                (power <= 2 && gooOnMap > 0) |=> 2000 -> "discount when low on power with GOO"
                (power > 4) |=> -1000 -> "don't discount when power is sufficient"

            case FBInfernalPactChooseAction(_, sb) =>
                (sb == Augury && auguryKills > 0) |=> -3000 -> "don't flip augury with kills stored"
                (sb == CyclopeanGaze) |=> -2000 -> "prefer keeping cyclopean gaze"
                (sb == CallOfTheFaithful) |=> 1000 -> "ok to flip call of faithful"
                true |=> 500 -> "flip a spellbook for discount"

            case FBInfernalPactDoneAction(_) =>
                (infernalDiscount >= 1) |=> 2000 -> "done with discount"

            case FBInfernalPactCancelAction(_) =>
                true |=> -500 -> "cancel is usually suboptimal"

            case FBInfernalPactCancelMainAction(_) =>
                true |=> -1000 -> "cancel from main is usually bad"

            // ── INFERNAL PACT (DOOM PHASE VARIANTS) ── Same scoring as the main-action
            // versions. Round 8 Bug 40 added doom-phase variants so the discount can be
            // used for doom-phase rituals. Bot scoring mirrors the main-action equivalents.
            case FBInfernalPactDoomMainAction(_) =>
                (power <= 2 && gooOnMap > 0) |=> 2000 -> "doom infernal pact discount when low on power with GOO"
                (power > 4) |=> -1000 -> "don't doom-pact when power is sufficient"

            case FBInfernalPactDoomChooseAction(_, sb) =>
                (sb == Augury && auguryKills > 0) |=> -3000 -> "don't flip augury with kills stored (doom)"
                (sb == CyclopeanGaze) |=> -2000 -> "prefer keeping cyclopean gaze (doom)"
                (sb == CallOfTheFaithful) |=> 1000 -> "ok to flip call of faithful (doom)"
                true |=> 500 -> "flip a spellbook for doom-phase discount"

            case FBInfernalPactDoomDoneAction(_) =>
                (infernalDiscount >= 1) |=> 2000 -> "done with doom discount"

            case FBInfernalPactDoomCancelAction(_) =>
                true |=> -500 -> "cancel doom infernal pact is usually suboptimal"

            case FBInfernalPactCancelDoomAction(_) =>
                true |=> -1000 -> "cancel doom infernal pact from main is usually bad"

            // ── AWAKEN GHATANOTHOA ── Score awakening timing and cost efficiency
            case FBAwakenGhatanothoaAction(_, cost) =>
                secondAP |=> 5000 -> "awaken in AP2"
                firstAP |=> -2000 -> "don't awaken in AP1"
                (cost <= 3) |=> 3000 -> "cheap awakening"
                (cost >= 6) |=> -1000 -> "expensive awakening"
                (game.fbGhatnothoaAwakenings == 0) |=> 4000 -> "first awakening priority"
                (game.fbGhatnothoaAwakenings == 1) |=> 3000 -> "second awakening for spellbook"
                (game.fbGhatnothoaAwakenings == 2) |=> 5000 -> "third awakening for final spellbook"

            // ── THE EYE OPENS ── Score cultist elimination targeting: prefer sole gate controllers
            case FBTheEyeOpensMainAction(_) =>
                (power <= 2) |=> 2000 -> "stall with eye opens when low power"
                true |=> 1000 -> "eye opens for cultist pressure"

            case FBTheEyeOpensTargetAction(_, r, f) =>
                r.enemyGate |=> 2000 -> "eliminate cultist at enemy gate"
                (f.at(r).%(_.canControlGate).num == 1) |=> 3000 -> "eliminate sole gate controller"
                true |=> 500 -> "eliminate cultist"

            // The painted faction picks WHICH specific cultist to lose. Prefer keeping
            // gate-keepers and high-priests; pick acolytes/non-gate cultists when forced.
            // (Bug 50: per-unit selection for on-gate vs off-gate distinction.)
            case FBTheEyeOpensChooseCultistAction(_, _, _, uRef) =>
                val u = game.unit(uRef)
                (u.uclass == HighPriest) |=> -2000 -> "don't sacrifice high priest to eye opens"
                (u.onGate) |=> -1500 -> "keep gate keeper"
                (u.uclass == Acolyte && !u.onGate) |=> 1000 -> "lose off-gate acolyte"
                true |=> 0 -> "default eye-opens cultist choice"

            // ── CALL OF THE FAITHFUL ── Score free acolyte placement near GOO or enemy gates
            case FBCallOfTheFaithfulMainAction(_) =>
                true |=> 2500 -> "free acolyte is always good"
                (power <= 1) |=> 3000 -> "stall with free acolyte"

            case FBCallOfTheFaithfulAction(_, r) =>
                r.ownGate |=> 1000 -> "place near own gate"
                (self.at(r, Ghatanothoa).any) |=> 1500 -> "place near GOO for protection"
                r.enemyGate |=> 2000 -> "place at enemy gate for pressure"

            // ── DEVIL'S MARK ── Score crater placement in glyph regions for power and ES gain
            case FBDevilsMarkDoomAction(_) =>
                (self.gates.%(r => r.glyph != Ocean).num >= 2) |=> 3000 -> "devils mark with spare gates"
                true |=> 1500 -> "devils mark for ES"

            case FBDevilsMarkPlaceCraterAction(_, r) =>
                val hasGlyph = game.factions.exists(f => game.starting.get(f).has(r))
                hasGlyph |=> 4000 -> "crater in glyph region for power"
                (!hasGlyph) |=> 1000 -> "crater in non-glyph region"
                (game.starting.get(FB).has(r)) |=> 3000 -> "crater in own start area"

            // ── CARNAGE ── Score post-battle ES gain: pay power vs flip spellbook vs cancel
            case FBCarnagePayPowerAction(_) =>
                (power >= 3) |=> 2000 -> "pay power for ES"
                (power < 2) |=> -1000 -> "too low on power for carnage"

            case FBCarnageFlipSpellbookAction(_) =>
                true |=> 1500 -> "flip spellbook for ES"

            case FBCarnageChooseSpellbookAction(_, sb) =>
                (sb == CallOfTheFaithful) |=> 2000 -> "flip call of faithful for ES"
                (sb == Augury && auguryKills == 0) |=> 1500 -> "flip empty augury"
                (sb == Augury && auguryKills > 0) |=> -2000 -> "don't flip augury with kills"

            case FBCarnageCancelAction(_) =>
                true |=> -500 -> "cancel carnage"

            // ── AUGURY ── Score replacing Miss dice with stored Kills from augury pool (Writhe and Battle)
            case FBWritheAuguryReplaceAction(_, _, n) =>
                true |=> 2000 + n * 500 -> "replace misses with augury kills"

            case FBWritheAuguryCancelAction(_, _) =>
                true |=> -200 -> "skip augury"

            case FBAuguryBattleReplaceAction(_, n) =>
                true |=> 2500 + n * 500 -> "replace battle misses with augury"

            case FBAuguryBattleCancelAction(_) =>
                true |=> -200 -> "skip battle augury"

            // ── CYCLOPEAN GAZE ── Score pain targeting: prefer GOOs and gate controllers, send to empty/ocean areas
            // Bug fix Round 4: action shapes updated to carry sourceUnit + sourcesPending + fromBattle —
            // patterns ignore those new fields since the bot only scores by target unit / destination region.
            case FBCyclopeanGazePainUnitAction(_, _, uRef, _, _, _, _) =>
                val u = game.unit(uRef)
                (u.goo) |=> 5000 -> "pain enemy GOO"
                (u.gateKeeper) |=> 4000 -> "pain gate controller"
                (u.cultist) |=> 1000 -> "pain cultist"
                (u.monster) |=> 2000 -> "pain monster"

            case FBCyclopeanGazeDestinationAction(_, uRef, r, _, _, _, _) =>
                val u = game.unit(uRef)
                r.foes.none |=> 2000 -> "pain to empty area"
                r.ownGate |=> -2000 -> "don't pain to own gate area"
                r.ocean |=> 1500 -> "pain to ocean"

            // Bug 51: when CG pain has no legal destinations, the painted faction picks
            // which of their units to eliminate (a "soak" choice). Prefer sacrificing
            // cheap units (Acolytes) and protect valuable ones (HighPriest, Monsters, GOOs).
            // Mirror image of FBCyclopeanGazePainUnitAction's "avoid killing valuable" scoring.
            case FBCyclopeanGazeKillChoiceAction(_, _, killRef, _, _, _, _, _) =>
                val k = game.unit(killRef)
                (k.goo) |=> -5000 -> "don't sacrifice GOO to CG kill"
                (k.uclass == HighPriest) |=> -3000 -> "don't sacrifice HP to CG kill"
                (k.gateKeeper) |=> -2000 -> "don't sacrifice gate keeper"
                (k.monster) |=> -1500 -> "avoid sacrificing monster"
                (k.uclass == Acolyte && !k.onGate) |=> 1000 -> "sacrifice off-gate acolyte"
                (k.cultist && !k.onGate) |=> 500 -> "sacrifice off-gate cultist"
                true |=> 0 -> "default CG kill-choice"

            // ── STANDARD GAME ACTIONS ── Round 8: BotFB used to fall through
            // to a flat zero score for every standard action, which made FB
            // never build gates, never recruit Revenants, never attack, never
            // engage in battles where Carnage / CG could fire. Sims showed
            // 0/20 games with Carnage, 0/20 with CG, 0% gates owned. Adding
            // explicit scoring for the basic action types FB needs to play.

            // ── STARTING REGION ── FB starts in any region (per Bug 43).
            // Prefer LAND areas so we can move Acolytes out and build gates
            // without having to swim across oceans first. Also prefer regions
            // not adjacent to many other starting factions so we don't get
            // immediately attacked.
            case StartingRegionAction(_, r) =>
                r.glyph != Ocean |=> 5000 -> "land start (can build gates immediately)"
                r.glyph == Ocean |=> -3000 -> "ocean start traps acolytes"
                (r.near.%(nr => nr.glyph != Ocean).num >= 2) |=> 1000 -> "two+ adjacent land regions"

            // ── BUILD GATE ── FB needs gates for rituals, Devil's Mark crater
            // placement, and the FBMostDoomOrMoreGates requirement. Without
            // gates FB cannot do anything in the doom phase. Score very high
            // when FB has 0 gates so the bot will spend its main action on
            // building rather than competing options.
            case BuildGateAction(_, r) =>
                self.gates.num == 0 |=> 8000 -> "first gate is critical"
                self.gates.num == 1 |=> 4000 -> "second gate"
                self.gates.num == 2 |=> 2500 -> "third gate"
                self.gates.num >= 3 |=> 1500 -> "additional gate"
                r.glyph != Ocean |=> 500 -> "land gate (for Devil's Mark)"

            case BuildGateMainAction(_, _) =>
                self.gates.num == 0 |=> 8000 -> "open the build menu — first gate is critical"
                self.gates.num == 1 |=> 4000 -> "open the build menu — second gate"
                self.gates.num >= 2 |=> 2500 -> "open the build menu — more gates"

            // ── RECRUIT ── Revenants are FB's main combat unit; their combat
            // scales with Desiccated count. Without Revenants FB has no
            // monster pressure on the board, no Carnage trigger potential.
            case RecruitAction(_, uc, _) =>
                (uc == RevenantOfKnaa) |=> 3000 -> "recruit Revenant — FB's main combat"
                (uc == HighPriest) |=> 1500 -> "recruit High Priest"
                (uc == Acolyte) |=> 1000 -> "recruit Acolyte (Writhe fodder)"

            // ── SUMMON ── Same as recruit. Some FB units summon (depends on
            // pool/gate state) instead of recruiting.
            case SummonAction(_, uc, _) =>
                (uc == RevenantOfKnaa) |=> 3000 -> "summon Revenant"
                (uc == Desiccated) |=> 2000 -> "summon Desiccated"
                (uc == Acolyte) |=> 1000 -> "summon Acolyte"

            // ── MOVE ── Push units toward enemy regions to provoke battles
            // (which trigger Carnage and CG via gaze regions). Avoid moving
            // away from own regions where FB's GOO/Revenants are anchored.
            // ALSO move Acolytes/HighPriest out to empty land regions so FB
            // can build a gate there next turn (this is the only way FB
            // breaks out of its single starting region).
            case MoveAction(_, u, from, to, _) =>
                to.enemyGate |=> 1500 -> "move to enemy gate (provoke battle)"
                to.foes.cultists.any && to.foes.monsterly.none |=> 1200 -> "move to lone enemy cultists"
                (u.uclass == RevenantOfKnaa && to.foes.any) |=> 1000 -> "Revenant moves to engage"
                (u.uclass == Desiccated && from.ownGate) |=> -500 -> "don't strip Desiccated from own gate"
                to.ownGate |=> 200 -> "reinforce own gate"
                // Round 8: drive cultist expansion when FB has no gates yet.
                // Without this, Acolytes never leave Arctic Ocean and FB can
                // never build a gate, leaving FB stuck with 0 gates the
                // entire game (observed in 28/30 sims). Score has to beat
                // Writhe (3000) so the bot prioritizes building over the
                // pre-awakening filler actions when FB has no gate.
                (u.cultist && self.gates.num == 0 && to.foes.none && !to.gate && to.glyph != Ocean) |=>
                    3500 -> "move acolyte to empty land to build a gate"
                (u.cultist && self.gates.num < 2 && to.foes.none && !to.gate && to.glyph != Ocean) |=>
                    2200 -> "move acolyte toward second gate"

            // ── ATTACK ── Engage enemies. The base BotX scoring covers
            // strength comparison; FB just needs a small positive baseline so
            // attacks aren't ranked below "do nothing".
            case AttackAction(_, r, f, _) =>
                self.at(r, RevenantOfKnaa).any |=> 1000 -> "Revenant attacks"
                self.at(r, Ghatanothoa).any |=> 1500 -> "Ghatanothoa attacks"
                f.cultists.num > self.strength(self.at(r), f) / 2 |=> 800 -> "good attack odds"

            case _ =>
                true |=> 0 -> "default"
        }

        result.none |=> 0 -> "none"
        true |=> (math.random() * 4).round.toInt -> "random"

        result
    }
}
