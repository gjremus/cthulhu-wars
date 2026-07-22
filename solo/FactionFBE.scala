package cws

import hrf.colmat._
import html._

// ============================================================================
// FACELESS BLIGHT (FBE) — Homebrew faction
// Spec: "FacelessBlight Faction Implementation Guide" (all sections Creator Audited)
// Faction color #3d5f1c (deep mossy green), style "fbe".
// Led by the Great Old One Byagoona. A kill-and-replace fungal-horror faction.
//
// IMPLEMENTATION NOTES
//  • Faction-Card dice pool (FBE's pseudo-currency, §1.6/§2.4) lives on Game.scala
//    as game.fbeCardDice : $[Int] (pip values 1-6, undo/replay-safe — mirrors the
//    DC dcSin pattern that keeps faction state on Game.scala for replay safety).
//    The combat FACE of each die is derived from its pip: 6=Kill, 4/5=Pain, else Miss.
//    Storing the pip (not just the face) lets Shapestealing compare the literal pip
//    value 1-6 to a Monster's Cost (§1.10 SB3) while Byagoona's Combat counts the
//    Kill/Pain faces (§1.8).
//  • Self Consuming death tally lives on game.fbeSelfConsumingDeaths : $[Boolean]
//    (one entry per Unit of ANY faction that died this Action; the Boolean = "was
//    FBE-controlled"). The 2+ Power trigger counts every Unit; the 3+ Doom bonus
//    counts only FBE-controlled ones (§1.5.1). Pushed in FBEExpansion.eliminate
//    (universal death hook), evaluated and cleared in FBEExpansion.afterAction.
//  • Ghasts are barred from any FBE game at SETUP (§1.6, creator-approved global
//    Ghast ban — see Game.scala loyaltyCards init), so every "Monster" rule reads
//    uniformly across Fungal Thralls and controlled Neutral Monsters with no
//    per-ability Ghast guard.
// ============================================================================


// ── UNITS (§1.7 / §2.1) ─────────────────────────────────────────────────────
// FBE uses the standard Acolyte UnitClass for its 6 Acolytes (no FBE-specific
// Cultist subclass — §2.1). FungalThrall: Monster cost 2, combat 2, pool 10.
// Byagoona: GreatOldOne (GOO) cost 10, combat = card-dice Kills+Pains (§1.8).
case object FungalThrall extends FactionUnitClass(FBE, "Fungal Thrall", Monster, 2)
case object Byagoona     extends FactionUnitClass(FBE, "Byagoona",      GOO,     10)


// ── SPELLBOOKS (§1.10) ──────────────────────────────────────────────────────
case object ChangelingAdherents extends FactionSpellbook(FBE, "Changeling Adherents")
case object NecromanticSpores   extends FactionSpellbook(FBE, "Necromantic Spores")
case object Shapestealing        extends FactionSpellbook(FBE, "Shapestealing") with BattleSpellbook
case object AnimatedRush         extends FactionSpellbook(FBE, "Animated Rush")
case object Succor               extends FactionSpellbook(FBE, "Succor")
case object OverlordOfDeath      extends FactionSpellbook(FBE, "Overlord of Death")

// FBE Unique Ability — Self Consuming (Ongoing). Declared as a FactionSpellbook so
// every reference uses the FCG-blessed SelfConsuming.styled(FBE) form and so it can
// be shown in the faction status panel / borrowed via Ancient Sorcery.
case object SelfConsuming extends FactionSpellbook(FBE, "Self Consuming")


// ── SPELLBOOK REQUIREMENTS (§1.9) ───────────────────────────────────────────
case object ChangelingAdherentsReq extends Requirement("A total of 3 Kills are Rolled in a Battle you Participate in")
case object NecromanticSporesReq   extends Requirement("As an Action, Eliminate Two Fungal Thralls")
case object ShapestealingReq        extends Requirement("Have 3 Units in an Enemy Start Area")
case object AnimatedRushReq         extends Requirement("Have 3 Dice on your Faction Card")
case object SuccorReq               extends Requirement("Byagoona Dies in Battle. Do not fulfill if the Kill/Elimination is prevented")
case object OverlordOfDeathReq      extends Requirement("Awaken Byagoona")


// ============================================================================
// FACELESS BLIGHT (FBE) FACTION OBJECT
// ============================================================================
case object FBE extends Faction { f =>
    def name  = "Faceless Blight"
    def short = "FBE"
    def style = "fbe"

    // Self Consuming is the always-on unique ability (§1.5.1).
    override def abilities = $(SelfConsuming)
    override def library   = $(ChangelingAdherents, NecromanticSpores, Shapestealing, AnimatedRush, Succor, OverlordOfDeath)
    override def requirements(options : $[GameOption]) =
        $(ChangelingAdherentsReq, NecromanticSporesReq, ShapestealingReq, AnimatedRushReq, SuccorReq, OverlordOfDeathReq)

    // 6 Acolytes + 10 Fungal Thralls (pool) + 1 Byagoona. (§1.6 / §2.1)
    val allUnits =
        6.times(Acolyte)      ++
        10.times(FungalThrall) ++
        1.times(Byagoona)

    // Byagoona awakens via a CUSTOM Monster-sacrifice procedure (§1.8), NOT the
    // standard Power-cost awaken. Returning None here keeps game.awakens() from
    // offering Byagoona through the standard AwakenMainAction path; the custom
    // ByagoonaAwakenMainAction is offered from FBE's MainAction instead.
    override def awakenCost(u : UnitClass, r : Region)(implicit game : Game) : |[Int] = None

    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int = {
        // Byagoona's Combat = count of Kill/Pain faces on the Faction-Card dice
        // (§1.8). Pip >= 4 is a Pain or Kill face (4/5 Pain, 6 Kill). Fungal
        // Thralls use their cost (2) as combat. Acolytes (Cultists) have 0 combat.
        // Neutral Monsters are scored ONLY by neutralStrength below — they must be
        // excluded from this cost term or a controlled Neutral Monster (e.g. a Gug)
        // gets double-counted (once by cost here, once by neutralStrength).
        // Exclude by unit CLASS, not by .faction: a controlled/Shapestolen neutral
        // monster carries the real controlling faction, so a faction-based filter
        // would never exclude it. Fungal Thralls and Shapestolen enemy-faction
        // monsters are not NeutralMonster classes, so they stay counted here.
        val byagoonaCount = units.%(_.uclass == Byagoona).num
        val byagoonaStr   = (byagoonaCount > 0).??(game.fbeCardDice.count(_ >= 4))
        units.%(u => u.uclass != Byagoona && u.uclass.utype == Monster && !u.uclass.is[NeutralMonster]).not(Zeroed)./(_.uclass.cost).sum +
        byagoonaStr +
        neutralStrength(units, opponent)
    }
}


// ============================================================================
// FACELESS BLIGHT (FBE) ACTION CASE CLASSES
// All Soft (navigation-only) sub-menus include Cancel; Hard (state-mutating)
// actions do NOT (per §3 / FCG Cancel discipline).
// ============================================================================

// ── BYAGOONA AWAKEN (§1.8 / §3.4.1 / §4.6) ──────────────────────────────────
// Soft area pick → Soft monster multi-select → Hard resolve (roll + place + pay).
case class ByagoonaAwakenMainAction(self : Faction)
    extends OptionFactionAction(("Awaken " + Byagoona.name).styled(FBE)) with MainQuestion with Soft
case class ByagoonaAwakenAreaAction(self : Faction, r : Region)
    extends BaseFactionAction(("Awaken " + Byagoona.name).styled(FBE) + " in", r) with Soft
case class ByagoonaAwakenPickAction(self : Faction, r : Region, picked : $[UnitRef], remaining : $[UnitRef])
    extends ForcedAction with PowerNeutral with Soft {
    // §4.6 live cost preview: "Cost so far: C; Power needed: max(0, 10 - C)".
    override def question(implicit game : Game) = {
        val c = picked./(ur => game.unit(ur).uclass.cost).sum
        ("Awaken " + Byagoona.name).styled(FBE) + ": choose Monsters to Eliminate in " + game.desc(r) +
            " (Cost so far: " + c + "; Power needed: " + math.max(0, 10 - c).power + ")"
    }
}
case class ByagoonaAwakenDoneAction(self : Faction, r : Region, picked : $[UnitRef])
    extends BaseFactionAction(("Awaken " + Byagoona.name).styled(FBE), "Done".styled("power"))
// Replay-safe roll capture: the N rolled dice (pip values) are baked into this action.
case class ByagoonaAwakenRollAction(self : Faction, r : Region, picked : $[UnitRef], rolls : $[Int])
    extends ForcedAction

// ── DISTRIBUTED DEATH (Post-Battle Kill mitigation, §1.8 / §3.4.4 / §4.6) ────
case class DistributedDeathMainAction(self : Faction, n : Int)
    extends OptionFactionAction(implicit g => ("Distributed Death".styled(FBE) + ": discard " + n + " dice to prevent " + n + " " + ("Kill".s(n)).styled("kill"))) with PostBattleQuestion
case class DistributedDeathPickAction(self : Faction, toSave : $[UnitRef], remaining : $[UnitRef], diceToDiscard : Int)
    extends BaseFactionAction("Distributed Death".styled(FBE) + ": save", implicit g => g.unitOpt(toSave.last)./(_.uclass.styled(FBE)).|("unit"))
case class DistributedDeathSkipAction(self : Faction)
    extends OptionFactionAction("Distributed Death".styled(FBE) + ": skip") with PostBattleQuestion

// ── NECROMANTIC SPORES (Post-Battle, §1.10 SB2 / §3.10.2 / §4.4.2) ───────────
case class NecromanticSporesMainAction(self : Faction, n : Int)
    extends OptionFactionAction(implicit g => ("Necromantic Spores".styled(FBE) + ": Eliminate a Monster you control to spawn " + n + " " + FungalThrall.name.styled(FBE))) with PostBattleQuestion
case class NecromanticSporesSkipAction(self : Faction)
    extends OptionFactionAction("Necromantic Spores".styled(FBE) + ": skip") with PostBattleQuestion
case class NecromanticSporesEliminateAction(self : Faction, monster : UnitRef, r : Region, n : Int)
    extends BaseFactionAction("Necromantic Spores".styled(FBE) + ": Eliminate", implicit g => { val mu = g.unit(monster); mu.uclass.styled(FBE) + " in " + mu.region + " (Cost " + mu.uclass.cost + ")" })

// ── SHAPESTEALING (Pre-Battle, §1.10 SB3 / §3.10.3 / §4.4.3) ─────────────────
case class ShapestealingPreBattleAction(self : Faction)
    extends OptionFactionAction(Shapestealing.styled(FBE)) with PreBattleQuestion with Soft
case class ShapestealingSkipAction(self : Faction)
    extends OptionFactionAction(("Skip " + Shapestealing.name).styled(FBE)) with PreBattleQuestion with Soft
case class ShapestealingTargetAction(self : Faction, enemyMonster : UnitRef)
    extends BaseFactionAction(Shapestealing.styled(FBE) + ": roll on", implicit g => g.unit(enemyMonster).uclass.styled(g.unit(enemyMonster).faction) + " (Cost " + g.unit(enemyMonster).uclass.cost + ")") with Soft
// Replay-safe roll capture: rolled pip baked into the terminal action.
case class ShapestealingResolveAction(self : Faction, enemyMonster : UnitRef, roll : Int)
    extends ForcedAction

// ── ANIMATED RUSH (Move, §1.10 SB4 / §3.10.4 / §4.4.4) ───────────────────────
case class AnimatedRushMainAction(self : Faction, source : Region, dest : Region, n : Int)
    extends OptionFactionAction(implicit g => ("Animated Rush".styled(FBE) + ": discard " + n + " " + (n == 1).?("die").|("dice") + ", move " + (2 * n) + " Units")) { override def question(implicit game : Game) = "Animated Rush".styled(FBE) }
case class AnimatedRushSkipAction(self : Faction)
    extends OptionFactionAction("Animated Rush".styled(FBE) + ": skip") { override def question(implicit game : Game) = "Animated Rush".styled(FBE) }
case class AnimatedRushUnitPickAction(self : Faction, movesLeft : Int, moved : $[UnitRef], discardedDice : $[Int], originalPositions : $[(UnitRef, Region)], rushSource : Region, rushDest : Region) extends ForcedAction with PowerNeutral
case class AnimatedRushDestPickAction(self : Faction, unitRef : UnitRef, movesLeft : Int, moved : $[UnitRef], discardedDice : $[Int], originalPositions : $[(UnitRef, Region)], rushSource : Region, rushDest : Region)
    extends ForcedAction with PowerNeutral with Soft { override def question(implicit game : Game) = "Animated Rush".styled(FBE) + ": move " + game.unit(unitRef).uclass.styled(FBE) + " to" }
case class AnimatedRushMoveAction(self : Faction, unitRef : UnitRef, dest : Region, movesLeft : Int, moved : $[UnitRef], discardedDice : $[Int], originalPositions : $[(UnitRef, Region)], rushSource : Region, rushDest : Region) extends ForcedAction with PowerNeutral
case class AnimatedRushCancelAction(self : Faction, discardedDice : $[Int], originalPositions : $[(UnitRef, Region)], rushSource : Region, rushDest : Region)
    extends OptionFactionAction("Cancel Rush") with PowerNeutral { override def question(implicit game : Game) = "Animated Rush".styled(FBE) }
// [LEGACY REPLAY] Pre-632df8c games recorded AnimatedRushDoneEarlyAction(self) instead of cancel.
case class AnimatedRushDoneEarlyAction(self : Faction) extends OptionFactionAction("Done") with PowerNeutral { override def question(implicit game : Game) = "Animated Rush".styled(FBE) }
// [LEGACY REPLAY] Pre-6d1ee19 games recorded AnimatedRushDoneAction — old "pick units, move all at once" flow.
case class AnimatedRushDoneAction(self : Faction, source : Region, dest : Region, k : Int, picked : $[UnitRef]) extends ForcedAction with PowerNeutral

// ── SUCCOR (Doom Phase, §1.10 SB5 / §3.10.5 / §4.4.5) ────────────────────────
case class SuccorMainAction(self : Faction)
    extends OptionFactionAction(Succor.styled(FBE)) with DoomQuestion with Soft
case class SuccorSkipAction(self : Faction)
    extends OptionFactionAction(("Skip " + Succor.name).styled(FBE)) with DoomQuestion with Soft
case class SuccorPickAction(self : Faction, picked : $[UnitRef], remaining : $[UnitRef])
    extends ForcedAction with PowerNeutral with Soft {
    // §4.4.5 prompt: "Succor: choose Units to Eliminate" (vs current Ritual Marker R).
    override def question(implicit game : Game) =
        Succor.styled(FBE) + ": choose Units to Eliminate (roll vs Ritual Marker " + game.ritualCost + ")"
}
case class SuccorRollAction(self : Faction, picked : $[UnitRef], rolls : $[Int])
    extends ForcedAction

// ── OVERLORD OF DEATH (Infernal-Pact-style cost discount, §1.10 SB6 / §3.10.6) ─
// HB Fix 127: redesigned from a standalone free action into a cost discount.
// Multi-select monsters to eliminate; each reduces the Power cost of the NEXT action by 1.
case class OverlordOfDeathMainAction(self : Faction)
    extends OptionFactionAction("Discount with " + OverlordOfDeath.styled(FBE)) with MainQuestion with Soft with PowerNeutral
case class OverlordOfDeathPickAction(self : Faction, picked : $[UnitRef], remaining : $[UnitRef])
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) =
        OverlordOfDeath.styled(FBE) + ": choose Monsters to Eliminate (discount so far: " + picked.num.power + ")"
}
// HB OoD-Hard fix: was `with Soft`, but the handler MUTATES state (eliminates the
// picked Monsters + sets the discount). Soft actions are excluded from undo replay,
// so undoing across a discounted action re-ran the later action with a 0 discount —
// Power was spent instead of the Monster Eliminations. Now Hard so undo correctly
// reverses the elimination + discount and replay re-applies them. Mirrors FB's The
// Eye Opens Soft→Hard fix (Round 8 Bug 42).
case class OverlordOfDeathDoneAction(self : Faction, picked : $[UnitRef])
    extends OptionFactionAction("Done".styled("power")) with PowerNeutral { def question(implicit game : Game) = OverlordOfDeath.styled(FBE) }
case class OverlordOfDeathCancelAction(self : Faction)
    extends OptionFactionAction("Cancel " + OverlordOfDeath.styled(FBE)) with PowerNeutral { def question(implicit game : Game) = OverlordOfDeath.styled(FBE) }
case class OverlordOfDeathCancelMainAction(self : Faction)
    extends OptionFactionAction("Cancel " + OverlordOfDeath.styled(FBE)) with MainQuestion with PowerNeutral

// ── NECROMANTIC SPORES REQUIREMENT — Eliminate Two Fungal Thralls (§3.12.2) ──
// 0 Power Common Action; the two Eliminations DO trigger Self Consuming (+1 Power).
case class EliminateTwoFungalThrallsMainAction(self : Faction)
    extends OptionFactionAction(("Eliminate two " + FungalThrall.name + "s").styled(FBE)) with MainQuestion with Soft
case class EliminateTwoFungalThrallsPickAction(self : Faction, picked : $[UnitRef], remaining : $[UnitRef])
    extends ForcedAction with PowerNeutral with Soft
case class EliminateTwoFungalThrallsDoneAction(self : Faction, picked : $[UnitRef])
    extends BaseFactionAction(("Eliminate two " + FungalThrall.name + "s").styled(FBE), "Done".styled("power"))

// ── MANUAL DIE SELECTION FRAMEWORK (§USER-2026-07-22) ────────────────────────
// Optional flow for manual die selection instead of auto-selecting lowest.
// Sealed trait to indicate which ability is requesting die selection.
sealed trait DieSelectionContext
case object AnimatedRushContext extends DieSelectionContext
case object ShapestealingContext extends DieSelectionContext
case object DistributedDeathContext extends DieSelectionContext

// Manual die picker: player chooses which dice to discard from the faction card.
case class ManualDiePickAction(self : Faction, context : DieSelectionContext, needed : Int, selected : $[Int], continuation : $[Int] => GameAction)
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) = {
        val abilityName = context match {
            case AnimatedRushContext => "Animated Rush"
            case ShapestealingContext => "Shapestealing"
            case DistributedDeathContext => "Distributed Death"
        }
        abilityName.styled(FBE) + ": choose " + (needed - selected.num) + " more " + ((needed - selected.num) == 1).?("die").|("dice") + " to discard"
    }
}

// Individual die choice during manual selection.
case class ManualDieChooseAction(self : Faction, context : DieSelectionContext, needed : Int, selected : $[Int], dieValue : Int, continuation : $[Int] => GameAction)
    extends OptionFactionAction(implicit g => "Discard die: " + dieValue.toString) with PowerNeutral with Soft {
    override def question(implicit game : Game) = {
        val abilityName = context match {
            case AnimatedRushContext => "Animated Rush"
            case ShapestealingContext => "Shapestealing"
            case DistributedDeathContext => "Distributed Death"
        }
        abilityName.styled(FBE) + ": choose dice to discard"
    }
}

// Undo a die selection during manual picking.
case class ManualDieUndoLastAction(self : Faction, context : DieSelectionContext, needed : Int, selected : $[Int], continuation : $[Int] => GameAction)
    extends OptionFactionAction("Undo last selection") with PowerNeutral with Soft {
    override def question(implicit game : Game) = {
        val abilityName = context match {
            case AnimatedRushContext => "Animated Rush"
            case ShapestealingContext => "Shapestealing"
            case DistributedDeathContext => "Distributed Death"
        }
        abilityName.styled(FBE) + ": choose dice to discard"
    }
}


// ============================================================================
// FACELESS BLIGHT (FBE) EXPANSION — game-loop integration
// ============================================================================
object FBEExpansion extends Expansion {

    // Derive a combat FACE from a stored card-die pip value (§1.8 / §2.4).
    def face(pip : Int) : BattleRoll = if (pip >= 6) Kill else if (pip >= 4) Pain else Miss

    // ── SELF CONSUMING death tally (§1.5.1 / §3.6) ───────────────────────────
    // Universal death hook: every Unit eliminated during the active Action (battle
    // or otherwise), regardless of faction, is recorded. The Boolean records whether
    // the dying Unit was FBE-controlled so the +1 Doom "controlled 3+" clause can be
    // checked. Power triggers on 2+ Units of ANY faction; Doom needs 3+ FBE-controlled.
    // Exception: High Priest sacrifices (Unspeakable Oath) are not part of Actions
    // and never count toward Self Consuming.
    // Fix: only record deaths when an action is actively resolving (fbeActionInProgress).
    override def eliminate(u : UnitFigure)(implicit game : Game) {
        if (!game.setup.has(FBE)) return
        if (game.fbeHPSacrificeInProgress) return
        if (!game.fbeActionInProgress) return
        game.fbeSelfConsumingDeaths :+= (u.faction == FBE)
    }

    // ── SELF CONSUMING resolution (fires at end of every Action) (§3.6) ───────
    override def afterAction()(implicit game : Game) {
        if (!game.setup.has(FBE)) return
        val deaths = game.fbeSelfConsumingDeaths
        // §1.5.1: 2+ Units (any faction) Killed/Eliminated in one Action → +1 Power;
        // if FBE controlled at least 3 of those Units → also +1 Doom.
        if (deaths.num >= 2) {
            FBE.power += 1
            FBE.log(SelfConsuming.styled(FBE) + ": 2+ Units died — gained", 1.power)
            if (deaths.count(_ == true) >= 3) {
                FBE.doom += 1
                FBE.log(SelfConsuming.styled(FBE) + ": FBE controlled 3+ — also gained", 1.doom)
            }
        }
        game.fbeSelfConsumingDeaths = $
        // HB Fix 127: Overlord of Death discount applies to the NEXT action only.
        // Reset after the discounted action resolves (afterAction fires via EndAction).
        if (game.fbeOverlordDiscount > 0) {
            FBE.log(OverlordOfDeath.styled(FBE) + ": unused discount expired")
            game.fbeOverlordDiscount = 0
        }
    }

    // ── CONTINUOUS / CONDITION-BASED SBR evaluation (§3.12) ──────────────────
    override def triggers()(implicit game : Game) {
        if (!game.setup.has(FBE)) return
        val f = FBE
        // SBR 3 — Shapestealing: 3 FBE Units in any enemy Start Area (§3.12.3).
        f.satisfyIf(ShapestealingReq, ShapestealingReq.text,
            f.enemies.exists(e => game.starting.get(e).exists(r => f.at(r).num >= 3)))
        // SBR 4 — Animated Rush: 3 dice on the Faction Card (§3.12.4).
        f.satisfyIf(AnimatedRushReq, AnimatedRushReq.text, game.fbeCardDice.num >= 3)
    }

    // Eligible Monsters FBE controls = Fungal Thralls + any controlled Neutral
    // Monster on the map. Ghasts cannot appear (barred at setup, §1.6).
    def controlledMonsters(r : Region)(implicit game : Game) : $[UnitFigure] =
        FBE.at(r).%(u => u.uclass.utype == Monster)
    def controlledMonstersAnywhere(implicit game : Game) : $[UnitFigure] =
        FBE.units.%(u => u.region.onMap && u.uclass.utype == Monster)

    def perform(action : Action, soft : VoidGuard)(implicit game : Game) = action @@ {

        // ── SETUP (§1.6 / §3.8.1) ────────────────────────────────────────────
        // FBE uses the STANDARD StartingRegionAction flow (board.starting(FBE)
        // returns the empty + non-adjacent areas — see Map.scala), which places
        // the 6 Acolytes + 1 Controlled Gate. Starting Power 8 arises naturally
        // from the first Gather Power (1 Gate = 2 + 6 Cultists = 8). The only
        // FBE-specific setup state is the empty Faction-Card dice pool.
        // NOTE: FBE setup area filtering (§1.6) is now handled entirely in
        // Game.scala SetupFactionsAction. board.starting(FBE) returns regions
        // (never empty) so this faction-side fallback is no longer needed.

        // ── CHANGELING ADHERENTS — Gather Power (§1.10 SB1 / §3.8.3) ──────────
        // After Power is gathered (and raised to half), roll one Combat die per FBE
        // Acolyte controlling a Gate and place each on the Faction Card. Rolls are
        // captured in actions (RollD6 chain) so replay is deterministic. A sentinel
        // (game.fbeChangelingDoneThisGather) prevents re-firing on re-entry; once
        // done, FBEExpansion returns UnknownContinue and Game's AfterPowerGather runs.
        case AfterPowerGatherAction
            if game.setup.has(FBE) && FBE.can(ChangelingAdherents) && !game.fbeChangelingDoneThisGather =>
            game.fbeChangelingDoneThisGather = true
            val gateAcolytes = FBE.units.%(u => u.uclass == Acolyte && u.onGate && FBE.gates.has(u.region)).num
            if (gateAcolytes == 0)
                Force(AfterPowerGatherAction)
            else
                Force(ChangelingAdherentsRollAction(FBE, gateAcolytes, $))

        case ChangelingAdherentsRollAction(self, remaining, rolled) =>
            if (remaining <= 0) {
                game.fbeCardDice = game.fbeCardDice ++ rolled
                self.log(ChangelingAdherents.styled(FBE) + ": rolled",
                    rolled./(p => FBEExpansion.face(p).toString).mkString(", ") + ", placed on Faction Card")
                // HB FBE: re-evaluate spellbook requirements right after Changeling
                // Adherents places dice during Gather Power. The phase-level triggers()
                // (Game.scala raise-to-half) runs BEFORE these dice land, so without this
                // the "Have 3 Dice on your Faction Card" (Animated Rush) requirement would
                // only be satisfied later in the Action Phase. Re-running here satisfies it
                // in Gather Power, as intended.
                game.triggers()
                Force(AfterPowerGatherAction)
            }
            else
                RollD6(_ => ChangelingAdherents.styled(FBE) + ": roll for Acolyte controlling a Gate",
                    pip => ChangelingAdherentsRollAction(self, remaining - 1, rolled :+ pip))

        // ── DOOM PHASE (§3.10.5 Succor) ──────────────────────────────────────
        case DoomAction(f : FBE.type) =>
            implicit val asking = Asking(f)

            game.rituals(f)

            game.reveals(f)

            game.highPriests(f)

            game.hires(f)

            // Succor — offer once per Doom phase if acquired and FBE has any unit.
            if (f.can(Succor) && f.units.%(_.region.onMap).any && !f.oncePerAction.has(Succor))
                + SuccorMainAction(f)

            game.doomDone(f)

            asking

        // ── MAIN ACTION (§2.7 / §3.10) ───────────────────────────────────────
        case MainAction(f : FBE.type) if f.active.not =>
            UnknownContinue

        case MainAction(f : FBE.type) if f.acted =>
            UnknownContinue

        case MainAction(f : FBE.type) =>
            implicit val asking = Asking(f)

            // HB Fix 127: Cancel Overlord of Death if discount is active
            if (game.fbeOverlordDiscount > 0)
                + OverlordOfDeathCancelMainAction(f)

            // HB Fix 127: Overlord of Death discount — offer when Byagoona is on map,
            // FBE has the spellbook, FBE has monsters to eliminate, and no discount
            // is currently active (must pick action before discounting again).
            if (f.can(OverlordOfDeath) && f.all(Byagoona).any && controlledMonstersAnywhere.any && game.fbeOverlordDiscount == 0)
                + OverlordOfDeathMainAction(f)

            // HB Fix 127: Transient power boost for offer checks (same pattern as
            // FB Infernal Pact). OoD discount is a separate pool; game.moves/captures/
            // recruits/etc. check f.power >= cost directly and don't know about the
            // discount. Temporarily add the discount to f.power so offer checks see
            // effective purchasing power, then restore at the end of the block.
            val oodBoost = game.fbeOverlordDiscount
            if (oodBoost > 0)
                f.power += oodBoost

            game.moves(f)
            game.captures(f)
            game.recruits(f)
            game.battles(f)
            game.controls(f)
            game.builds(f)
            game.summons(f)
            game.awakens(f)
            game.independents(f)

            // Byagoona custom Awaken (§1.8): offer if Byagoona is in the pool and
            // there exists an area where (sum of monster costs) + current power >= 10.
            if (f.pool.%(_.uclass == Byagoona).any && controlledMonstersAnywhere./(_.region).distinct.exists(r =>
                controlledMonsters(r)./(_.uclass.cost).sum + f.power >= 10))
                + ByagoonaAwakenMainAction(f)

            // Necromantic Spores requirement — Eliminate Two Fungal Thralls (§3.12.2).
            if (f.needs(NecromanticSporesReq) && f.onMap(FungalThrall).num >= 2)
                + EliminateTwoFungalThrallsMainAction(f)

            game.neutralSpellbooks(f)
            game.libraryActions(f)
            game.highPriests(f)
            game.reveals(f)

            game.endTurn(f)(f.battled.any || game.nexed.any)

            // HB Fix 127: restore the transient OoD boost after menu building
            if (oodBoost > 0)
                f.power -= oodBoost

            asking

        // ── BYAGOONA AWAKEN (§1.8 / §3.4.1) ──────────────────────────────────
        case ByagoonaAwakenMainAction(self) =>
            // Only show areas where (sum of monster costs) + current power >= 10 (§1.8).
            val eligibleAreas = controlledMonstersAnywhere./(_.region).distinct.%(r =>
                controlledMonsters(r)./(_.uclass.cost).sum + self.power >= 10)
            Ask(self).each(eligibleAreas)(r => ByagoonaAwakenAreaAction(self, r)).cancel

        case ByagoonaAwakenAreaAction(self, r) =>
            // Begin a multi-select of Monsters in this area to Eliminate (1+).
            Force(ByagoonaAwakenPickAction(self, r, $, controlledMonsters(r)./(_.ref)))

        case ByagoonaAwakenPickAction(self, r, picked, remaining) =>
            implicit val asking = Asking(self)
            remaining.foreach { ur =>
                val u = game.unit(ur)
                + ByagoonaAwakenPickAction(self, r, picked :+ ur, remaining.but(ur))
                    .as("Add " + u.full + " in " + u.region + " (Cost " + u.uclass.cost + ")")
            }
            if (picked.any)
                + ByagoonaAwakenDoneAction(self, r, picked)
            + CancelAction
            asking

        case ByagoonaAwakenDoneAction(self, r, picked) =>
            // Roll one die per Eliminated Monster (replay-safe via RollD6 chain).
            // We roll them all up front, capturing the pips in the action.
            val n = picked.num
            def rollAll(acc : $[Int]) : Continue =
                if (acc.num >= n)
                    Force(ByagoonaAwakenRollAction(self, r, picked, acc))
                else
                    RollD6(_ => "Roll " + Byagoona.name.styled(FBE) + " Awaken die " + (acc.num + 1) + "/" + n,
                        pip => ByagoonaAwakenRollAction(self, r, picked, acc :+ pip))
            rollAll($)

        case ByagoonaAwakenRollAction(self, r, picked, rolls) =>
            val n = picked.num
            if (rolls.num < n) {
                // Continue rolling.
                RollD6(_ => "Roll " + Byagoona.name.styled(FBE) + " Awaken die " + (rolls.num + 1) + "/" + n,
                    pip => ByagoonaAwakenRollAction(self, r, picked, rolls :+ pip))
            }
            else {
                val cost = picked./(ur => game.unit(ur).uclass.cost).sum
                val owed = math.max(0, 10 - cost)
                // Eliminate the sacrificed Monsters (drives Self Consuming).
                picked.foreach(ur => game.eliminate(game.unit(ur)))
                // Place every rolled die (pip) on the Faction Card — re-Awaken ADDS
                // to the existing pool, never replaces it (§4.6 creator-resolved).
                game.fbeCardDice = game.fbeCardDice ++ rolls
                // Pay the Power balance (floored at 0, §1.8).
                self.power -= owed
                if (self.power < 0) self.power = 0
                // Place Byagoona.
                self.place(Byagoona, r)
                self.log("Awaken " + Byagoona.name.styled(FBE) + ": Eliminated", n,
                    ("Monster".s(n)).styled(FBE) + " (cost " + cost + "), rolled",
                    rolls./(p => face(p).toString).mkString(", ") + ", paid", owed.power + ", appears in", r)
                // SBR 6 — Awaken Byagoona (§3.12.6).
                self.satisfy(OverlordOfDeathReq, OverlordOfDeathReq.text)
                EndAction(self)
            }

        // ── DISTRIBUTED DEATH (§1.8 / §3.4.4) ────────────────────────────────
        // The actual Kill-prevention (cancelling assigned Kills + discarding dice)
        // is performed in Battle.scala, which owns live Battle state. These arms
        // are pure pass-throughs so the battle flow resumes via proceed().
        case DistributedDeathMainAction(self, n) =>
            UnknownContinue

        case DistributedDeathPickAction(self, toSave, remaining, diceToDiscard) =>
            UnknownContinue

        case DistributedDeathSkipAction(self) =>
            UnknownContinue

        // ── NECROMANTIC SPORES (§1.10 SB2 / §3.10.2) ─────────────────────────
        // FBE-side logic (Eliminate a controlled Monster, spawn Fungal Thralls);
        // Battle.scala's matching cases resume the battle via proceed().
        case NecromanticSporesMainAction(self, n) =>
            game.battle match {
                case Some(b) =>
                    val monsters = controlledMonsters(b.arena)
                    if (monsters.num == 1)
                        Force(NecromanticSporesEliminateAction(self, monsters.head.ref, b.arena, n))
                    else
                        Ask(self).each(monsters)(u =>
                            NecromanticSporesEliminateAction(self, u.ref, b.arena, n))
                case None => UnknownContinue
            }

        case NecromanticSporesSkipAction(self) =>
            self.log("Necromantic Spores".styled(FBE) + ": declined")
            UnknownContinue

        case NecromanticSporesEliminateAction(self, monster, r, n) =>
            game.eliminate(game.unit(monster))
            // Spawn min(pool, enemyKilled) Fungal Thralls in the Battle area (§1.10 SB2 pool cap).
            val spawnable = math.min(self.pool.%(_.uclass == FungalThrall).num, n)
            1.to(spawnable).foreach(_ => self.place(FungalThrall, r))
            self.log("Necromantic Spores".styled(FBE) + ": Eliminated a Monster, spawned",
                spawnable, ("Fungal Thrall".s(spawnable)).styled(FBE), "in", r)
            UnknownContinue

        // ── SHAPESTEALING (§1.10 SB3 / §3.10.3) ──────────────────────────────
        // FBE picks an enemy Monster, discards the LOWEST card die (weakest first,
        // per Q2 resolution), then ROLLS a fresh d6. The fresh roll's pip value
        // (1-6) is compared to the Monster's Cost. Card text: "roll a die from
        // your Faction Card." Battle.scala's matching cases resume the battle.
        case ShapestealingPreBattleAction(self) =>
            game.battle match {
                case Some(b) =>
                    val enemySide = if (b.attacker == FBE) b.defenders else b.attackers
                    val enemyMonsters = enemySide.forces.%(u => u.faction != FBE && u.uclass.utype == Monster)
                    Ask(self).each(enemyMonsters)(u => ShapestealingTargetAction(self, u.ref)).cancel
                case None => UnknownContinue
            }

        case ShapestealingTargetAction(self, enemyMonster) =>
            // Take the LOWEST card die (weakest first, protects strong dice per
            // Q2 resolution) and use its pip value (1-6) for the Shapestealing check.
            // The die was already rolled when placed on the Faction Card; we use that
            // value and discard the die (§1.10 SB3: "the value compared to the enemy
            // Monster's Cost is the literal pip value (1-6) printed on the rolled card die").
            val sorted = game.fbeCardDice.sortBy(x => x)
            val dieValue = sorted.head
            game.fbeCardDice = sorted.drop(1)
            self.log(Shapestealing.styled(FBE) + ": using card die (value", dieValue.toString + ")")
            Force(ShapestealingResolveAction(self, enemyMonster, dieValue))

        case ShapestealingResolveAction(self, enemyMonster, roll) =>
            val mon = game.unit(enemyMonster)
            val cost = mon.uclass.cost
            if (roll > cost) {
                game.fbeShapestolen :+= enemyMonster
                self.log(Shapestealing.styled(FBE) + ": die value", roll.toString + " vs Cost",
                    cost.toString + " —", mon.uclass.styled(mon.faction), "fights for", FBE.full, "this Combat")
            }
            else
                self.log(Shapestealing.styled(FBE) + ": die value", roll.toString + " vs Cost",
                    cost.toString + " — failed")
            UnknownContinue   // Battle.scala ShapestealingResolveAction case resumes via proceed()

        case ShapestealingSkipAction(self) =>
            self.log("Skip ".styled(FBE) + Shapestealing.styled(FBE))
            UnknownContinue

        // ── ANIMATED RUSH (§1.10 SB4 / §3.10.4) ──────────────────────────────
        // After Byagoona moves: discard dice to move 2 non-Byagoona units per die,
        // each from any area to any adjacent area (independent, not a carry).
        case MovedAction(self : FBE.type, u, from, to)
            if game.unit(u).uclass == Byagoona && self.can(AnimatedRush) && game.fbeCardDice.nonEmpty
               && self.units.%(_.region.onMap).%(_.uclass != Byagoona).any =>
            implicit val asking = Asking(self)
            val maxK = math.min(game.fbeCardDice.num, self.units.%(_.region.onMap).%(_.uclass != Byagoona).num / 2)
            (1 to maxK).foreach(k => + AnimatedRushMainAction(self, from, to, k))
            + AnimatedRushSkipAction(self)
            asking

        case AnimatedRushMainAction(self, source, dest, n) =>
            val sorted = game.fbeCardDice.sortBy(x => x)
            val discarded = sorted.take(n)
            game.fbeCardDice = sorted.drop(n)
            self.log("Animated Rush".styled(FBE) + ": discarded", n, (n == 1).?("die").|("dice"),
                "— moving", 2 * n, "Units")
            Force(AnimatedRushUnitPickAction(self, 2 * n, $, discarded, $, source, dest))

        case AnimatedRushSkipAction(self) =>
            self.log("Animated Rush".styled(FBE) + ": declined")
            Force(MoveContinueAction(self, true))

        case AnimatedRushUnitPickAction(self, movesLeft, moved, discardedDice, originalPositions, rushSource, rushDest) =>
            if (movesLeft <= 0)
                Force(MoveContinueAction(self, true))
            else {
                val eligible = self.units.%(_.region.onMap).%(_.uclass != Byagoona).%(u => !moved.has(u.ref)).%(u => game.board.connected(u.region).%(d => self.affords(0)(d)).any)
                if (eligible.none) {
                    // No units can move — auto-cancel: refund dice and undo moves.
                    game.fbeCardDice = game.fbeCardDice ++ discardedDice
                    originalPositions.foreach { case (ur, r) =>
                        val u = game.unit(ur)
                        u.region = r
                        u.remove(Moved)
                    }
                    self.log("Animated Rush".styled(FBE) + ": cancelled — no eligible units, dice refunded")
                    Force(MoveContinueAction(self, true))
                }
                else {
                    implicit val asking = Asking(self)
                    eligible.foreach { u =>
                        + AnimatedRushDestPickAction(self, u.ref, movesLeft, moved, discardedDice, originalPositions, rushSource, rushDest)
                            .as(u.full + " in " + u.region)
                    }
                    + AnimatedRushCancelAction(self, discardedDice, originalPositions, rushSource, rushDest)
                    asking
                }
            }

        case AnimatedRushDestPickAction(self, unitRef, movesLeft, moved, discardedDice, originalPositions, rushSource, rushDest) =>
            implicit val asking = Asking(self)
            val u = game.unit(unitRef)
            game.board.connected(u.region).%(dest => self.affords(0)(dest)).foreach { dest =>
                + AnimatedRushMoveAction(self, unitRef, dest, movesLeft, moved, discardedDice, originalPositions, rushSource, rushDest)
                    .as(dest + self.iced(dest))
            }
            + CancelAction
            asking

        case AnimatedRushMoveAction(self, unitRef, dest, movesLeft, moved, discardedDice, originalPositions, rushSource, rushDest) =>
            val u = game.unit(unitRef)
            val from = u.region
            // Ice Age tax (§1.5.1): a free Animated Rush move into an enemy Ice Age
            // area still pays +1 Power, unless FBE already paid for that area this
            // turn. payTax returns 0 when the area is already tagged Moved (Byagoona's
            // own paid move, or an earlier Rush unit), so the tax is charged once.
            self.payTax(dest)
            u.region = dest
            u.onGate = false
            u.add(Moved)
            self.log("Animated Rush".styled(FBE) + ":", u.uclass.styled(FBE), from, "→", dest)
            Force(AnimatedRushUnitPickAction(self, movesLeft - 1, moved :+ unitRef, discardedDice, originalPositions :+ (unitRef, from), rushSource, rushDest))

        case AnimatedRushCancelAction(self, discardedDice, originalPositions, rushSource, rushDest) =>
            // Refund the discarded dice.
            game.fbeCardDice = game.fbeCardDice ++ discardedDice
            // Undo any unit moves made during this rush.
            originalPositions.foreach { case (ur, r) =>
                val u = game.unit(ur)
                u.region = r
                u.remove(Moved)
            }
            self.log("Animated Rush".styled(FBE) + ": cancelled — dice refunded")
            // Re-present the Animated Rush prompt (choose dice or skip).
            implicit val asking = Asking(self)
            val maxK = math.min(game.fbeCardDice.num, self.units.%(_.region.onMap).%(_.uclass != Byagoona).num / 2)
            (1 to maxK).foreach(k => + AnimatedRushMainAction(self, rushSource, rushDest, k))
            + AnimatedRushSkipAction(self)
            asking

        // [LEGACY REPLAY] Pre-632df8c "Done early" — keep moves, end rush.
        case AnimatedRushDoneEarlyAction(self) =>
            self.log("Animated Rush".styled(FBE) + ": done early")
            Force(MoveContinueAction(self, true))

        // [LEGACY REPLAY] Pre-6d1ee19 "Done" — discard k dice, move all picked units to dest.
        case AnimatedRushDoneAction(self, source, dest, k, picked) =>
            game.fbeCardDice = game.fbeCardDice.sortBy(x => x).drop(k)
            picked.foreach { ur =>
                val u = game.unit(ur)
                u.region = dest
                // HB Fix 112 (2026-07-09): clear stale onGate when forcibly moved
                u.onGate = false
            }
            self.log("Animated Rush".styled(FBE) + ": discarded", k, (k == 1).?("die").|("dice"),
                "carried", picked.num, ("Unit".s(picked.num)).styled(FBE), "to", dest)
            Force(MoveContinueAction(self, true))

        // ── SUCCOR (§1.10 SB5 / §3.10.5) ─────────────────────────────────────
        case SuccorMainAction(self) =>
            Force(SuccorPickAction(self, $, self.units.%(_.region.onMap)./(_.ref)))

        case SuccorSkipAction(self) =>
            UnknownContinue

        case SuccorPickAction(self, picked, remaining) =>
            implicit val asking = Asking(self)
            remaining.foreach { ur =>
                + SuccorPickAction(self, picked :+ ur, remaining.but(ur))
                    .as("Add " + game.unit(ur).full + " in " + game.unit(ur).region)
            }
            if (picked.any) {
                // Roll Y d6 (replay-safe) then resolve.
                + SuccorRollStartAction(self, picked).as("Done — Eliminate " + picked.num + ", roll " + picked.num + "d6")("Done")
            }
            + CancelAction
            asking

        case SuccorRollStartAction(self, picked) =>
            val y = picked.num
            def rollAll(acc : $[Int]) : Continue =
                if (acc.num >= y) Force(SuccorRollAction(self, picked, acc))
                else RollD6(_ => Succor.styled(FBE) + ": roll d6 " + (acc.num + 1) + "/" + y,
                    pip => SuccorRollAction(self, picked, acc :+ pip))
            rollAll($)

        case SuccorRollAction(self, picked, rolls) =>
            val y = picked.num
            if (rolls.num < y)
                RollD6(_ => Succor.styled(FBE) + ": roll d6 " + (rolls.num + 1) + "/" + y,
                    pip => SuccorRollAction(self, picked, rolls :+ pip))
            else {
                picked.foreach(ur => game.eliminate(game.unit(ur)))
                // Doom Phase eliminations do NOT trigger Self-Consuming (Actions only).
                game.fbeSelfConsumingDeaths = $
                val sum = rolls.sum
                val ritual = game.ritualCost
                // Format rolls as "1+2+2 = 5" for display (user request 2026-07-22)
                val rollDisplay = if (rolls.num > 1) rolls.mkString("+") + " = " + sum.toString else sum.toString
                if (sum > ritual) {
                    self.takeES(1)
                    self.log(Succor.styled(FBE) + ": Eliminated", y, "Units, rolled", rollDisplay,
                        "vs Ritual Marker", ritual.toString + " — gained", 1.es)
                }
                else
                    self.log(Succor.styled(FBE) + ": Eliminated", y, "Units, rolled", rollDisplay,
                        "vs Ritual Marker", ritual.toString + " — no Elder Sign")
                FBE.oncePerAction :+= Succor
                Force(DoomAction(FBE))
            }

        // ── OVERLORD OF DEATH (§1.10 SB6 / §3.10.6) — HB Fix 127 ────────────
        // Redesigned as Infernal-Pact-style cost discount. Multi-select monsters
        // to eliminate; each = 1 Power discount on the NEXT action.
        case OverlordOfDeathMainAction(self) =>
            Force(OverlordOfDeathPickAction(self, $, controlledMonstersAnywhere./(_.ref)))

        case OverlordOfDeathPickAction(self, picked, remaining) =>
            implicit val asking = Asking(self)
            remaining.foreach { ur =>
                val u = game.unit(ur)
                + OverlordOfDeathPickAction(self, picked :+ ur, remaining.but(ur))
                    .as(u.full + " in " + u.region + " (Cost " + u.uclass.cost + ")")
            }
            if (picked.any)
                + OverlordOfDeathDoneAction(self, picked)
            + OverlordOfDeathCancelAction(self)
            asking

        case OverlordOfDeathDoneAction(self, picked) =>
            // Eliminate the selected monsters and set the discount.
            // Deaths accumulate in fbeSelfConsumingDeaths — Self Consuming fires
            // after the discounted action resolves (§1.10 SB6: OoD eliminations
            // count for Self Consuming when 2+ are eliminated).
            picked.foreach(ur => game.eliminate(game.unit(ur)))
            game.fbeOverlordDiscount = picked.num
            self.log(OverlordOfDeath.styled(FBE) + ": Eliminated", picked.num, "Monster" + (picked.num > 1).??("s") +
                ", next action discounted by", picked.num.power)
            Force(MainAction(self))

        case OverlordOfDeathCancelAction(self) =>
            Force(MainAction(self))

        case OverlordOfDeathCancelMainAction(self) =>
            // Cancel an active discount — no monsters are restored (they were eliminated),
            // but the discount is forfeited. This enables undo to work: undoing past
            // this point restores the monsters via the undo system.
            game.fbeOverlordDiscount = 0
            self.log(OverlordOfDeath.styled(FBE) + ": discount cancelled")
            Force(MainAction(self))

        // ── NECROMANTIC SPORES REQUIREMENT (§3.12.2) ─────────────────────────
        case EliminateTwoFungalThrallsMainAction(self) =>
            Force(EliminateTwoFungalThrallsPickAction(self, $, self.onMap(FungalThrall)./(_.ref)))

        case EliminateTwoFungalThrallsPickAction(self, picked, remaining) =>
            implicit val asking = Asking(self)
            if (picked.num < 2)
                remaining.foreach { ur =>
                    val u = game.unit(ur)
                    + EliminateTwoFungalThrallsPickAction(self, picked :+ ur, remaining.but(ur))
                        .as("Add " + u.full + " in " + u.region + " (Cost " + u.uclass.cost + ")")
                }
            if (picked.num == 2)
                + EliminateTwoFungalThrallsDoneAction(self, picked)
            + CancelAction
            asking

        case EliminateTwoFungalThrallsDoneAction(self, picked) =>
            // 0 Power cost; the two Eliminations DO trigger Self Consuming (§3.12.2 resolved).
            picked.foreach(ur => game.eliminate(game.unit(ur)))
            self.log(("Eliminated two " + FungalThrall.name + "s").styled(FBE))
            self.satisfy(NecromanticSporesReq, NecromanticSporesReq.text)
            EndAction(self)

        // ── MANUAL DIE SELECTION FRAMEWORK (§USER-2026-07-22) ────────────────
        // Player manually chooses which dice to discard instead of auto-selecting lowest.
        // NOTE: This framework is PREPARED but NOT ACTIVE. To activate, modify the
        // AnimatedRushMainAction, ShapestealingTargetAction, and DistributedDeathMainAction
        // handlers to call ManualDiePickAction instead of auto-discarding.
        case ManualDiePickAction(self, context, needed, selected, continuation) =>
            if (selected.num >= needed) {
                // Done selecting — execute the continuation with selected dice
                Force(continuation(selected))
            } else {
                implicit val asking = Asking(self)
                val available = game.fbeCardDice.diff(selected)
                available.foreach { dieValue =>
                    + ManualDieChooseAction(self, context, needed, selected, dieValue, continuation)
                        .as("Discard die: " + dieValue.toString)
                }
                if (selected.nonEmpty)
                    + ManualDieUndoLastAction(self, context, needed, selected, continuation)
                asking
            }

        case ManualDieChooseAction(self, context, needed, selected, dieValue, continuation) =>
            // Add this die to the selected list
            Force(ManualDiePickAction(self, context, needed, selected :+ dieValue, continuation))

        case ManualDieUndoLastAction(self, context, needed, selected, continuation) =>
            // Remove the last selected die
            Force(ManualDiePickAction(self, context, needed, selected.dropRight(1), continuation))

        // ── OVERLORD OF DEATH — turn-end cleanup (HB Fix 127) ────────────────
        // If the discount is still active when the turn ends without having
        // gone through EndAction (which fires afterAction → reset), forfeit it.
        case EndTurnAction(f) if f == FBE && (game.fbeOverlordDiscount > 0 || game.fbeSelfConsumingDeaths.any) =>
            if (game.fbeOverlordDiscount > 0)
                FBE.log(OverlordOfDeath.styled(FBE) + ": unused discount expired")
            game.fbeOverlordDiscount = 0
            game.fbeSelfConsumingDeaths = $
            UnknownContinue

        // ── OVERLORD OF DEATH — cost discount intercepts (HB Fix 127) ─────────
        // Same pattern as FB Infernal Pact: pre-consume OoD discount, pre-add
        // the consumed amount to f.power, return UnknownContinue so the base
        // handler's `self.power -= cost` runs against the boosted value.
        // Net result: real power loses (cost - consumed); OoD pool loses consumed.
        case RitualAction(f, cost, k) if f == FBE && game.fbeOverlordDiscount > 0 =>
            val consumed = min(game.fbeOverlordDiscount, cost)
            game.fbeOverlordDiscount -= consumed
            f.power += consumed
            FBE.log(OverlordOfDeath.styled(FBE), "discounted", consumed.power, "on ritual")
            UnknownContinue

        case BuildGateAction(f, r) if f == FBE && game.fbeOverlordDiscount > 0 =>
            val baseCost = 3 - f.has(UmrAtTawil).??(1)
            val consumed = min(game.fbeOverlordDiscount, baseCost)
            game.fbeOverlordDiscount -= consumed
            f.power += consumed
            FBE.log(OverlordOfDeath.styled(FBE), "discounted", consumed.power, "on build gate")
            UnknownContinue

        case RecruitAction(f, uc, r) if f == FBE && game.fbeOverlordDiscount > 0 =>
            val baseCost = f.recruitCost(uc, r)
            val consumed = min(game.fbeOverlordDiscount, baseCost)
            game.fbeOverlordDiscount -= consumed
            f.power += consumed
            FBE.log(OverlordOfDeath.styled(FBE), "discounted", consumed.power, "on recruit")
            UnknownContinue

        case SummonAction(f, uc, r) if f == FBE && game.fbeOverlordDiscount > 0 =>
            val baseCost = f.summonCost(uc, r)
            val consumed = min(game.fbeOverlordDiscount, baseCost)
            game.fbeOverlordDiscount -= consumed
            f.power += consumed
            FBE.log(OverlordOfDeath.styled(FBE), "discounted", consumed.power, "on summon")
            UnknownContinue

        case MoveAction(f, u, from, to, cost) if f == FBE && game.fbeOverlordDiscount > 0 =>
            val consumed = min(game.fbeOverlordDiscount, cost)
            game.fbeOverlordDiscount -= consumed
            f.power += consumed
            if (consumed > 0)
                FBE.log(OverlordOfDeath.styled(FBE), "discounted", consumed.power, "on move")
            UnknownContinue

        case AttackAction(f, r, fe, effect) if f == FBE && game.fbeOverlordDiscount > 0 && effect.has(FromBelow).not =>
            val consumed = min(game.fbeOverlordDiscount, 1)
            game.fbeOverlordDiscount -= consumed
            f.power += consumed
            FBE.log(OverlordOfDeath.styled(FBE), "discounted", consumed.power, "on attack")
            UnknownContinue

        case CaptureAction(f, r, fe, effect) if f == FBE && game.fbeOverlordDiscount > 0 && effect.has(FromBelow).not =>
            val consumed = min(game.fbeOverlordDiscount, 1)
            game.fbeOverlordDiscount -= consumed
            f.power += consumed
            FBE.log(OverlordOfDeath.styled(FBE), "discounted", consumed.power, "on capture")
            UnknownContinue

        case IndependentGOOAction(f, lc, r, _) if f == FBE && game.fbeOverlordDiscount > 0 =>
            val consumed = min(game.fbeOverlordDiscount, lc.power)
            game.fbeOverlordDiscount -= consumed
            f.power += consumed
            FBE.log(OverlordOfDeath.styled(FBE), "discounted", consumed.power, "on awaken")
            UnknownContinue

        case _ => UnknownContinue
    }
}

// Helper action referenced by the Succor d6-roll chain.
case class SuccorRollStartAction(self : Faction, picked : $[UnitRef]) extends ForcedAction

// Changeling Adherents Gather-Power roll chain (§3.8.3) — rolls one die per
// gate-controlling Acolyte, captured in the action for replay safety.
case class ChangelingAdherentsRollAction(self : Faction, remaining : Int, rolled : $[Int]) extends ForcedAction
