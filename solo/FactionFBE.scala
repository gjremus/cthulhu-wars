package cws

import hrf.colmat._
import html._

// ============================================================================
// FACELESS BLIGHT (FBE) — Homebrew faction
// Spec: Designer v16 redesign (request batch r_4HzheHId/QE880Qh0/AYiWN6lB/
// vmGLQAeT/78PKprMU/XuarLJg8/p-0LYtMo/rfBjp3Wr/I9APCaQW, fid tg3w6z5wmg, 2026-11-14).
// Faction color #3d5f1c (deep mossy green), style "fbe".
// Led by the Great Old One Byagoona. A kill-and-replace fungal-horror faction.
//
// v7→v16 REDESIGN NOTE: the 6 Spellbook Scala identifiers below are kept BOUND TO
// THEIR DESIGN ROW ID (not renamed) to avoid a Spellbook-serialization legacy shim —
// Spellbook IS a Record (reflection-serialized by Scala class name, Serialize.scala
// `write`), unlike Requirement. Each object's displayed `name` and mechanic below were
// simply swapped to match whatever row now occupies that id, mirroring how "HB Fix 127"
// previously redesigned OverlordOfDeath's mechanic wholesale with no legacy shim.
//
// v16 STATUS (2026-11-14 tick): ChangelingAdherents↔AnimatedRush identifier/mechanic
// swap DONE; Succor↔OverlordOfDeath identifier/mechanic swap DONE (old HB-Fix-127
// discount retired to [LEGACY REPLAY ONLY], new Succor-mechanic now gated on
// f.can(OverlordOfDeath)); NecromanticSpores identifier's new "Faceless Ones"
// retreat-or-eliminate mechanic DONE; Succor identifier's new "Dimorphic Gestation"
// free-repeat-Summon/Recruit mechanic DONE; Shapestealing identifier's new
// "Grinning Ghouls" choose-type/post-battle-thrall mechanic DONE. "Distributed
// Death" is confirmed UNCHANGED and NOT superseded — the design's own menus
// catalog (menu004) still names it as an active, separate mechanic that coexists
// with Byagoona's new innate ability below. STILL NOT IMPLEMENTED: Byagoona's new
// innate "Overlord of Death" post-battle result-swap unit ability (distinct from
// the OverlordOfDeath Spellbook) — INPUT NEEDED, exact exchange mechanics (what
// happens to swapped-out results, whether card dice are consumed) are not
// specified precisely enough to implement safely, and the design's menus catalog
// has no entry describing a "swap results" UI for it.
//
// IMPLEMENTATION NOTES
//  • Faction-Card dice pool (FBE's pseudo-currency, §1.6/§2.4) lives on Game.scala
//    as game.fbeCardDice : $[Int] (pip values 1-6, undo/replay-safe — mirrors the
//    DC dcSin pattern that keeps faction state on Game.scala for replay safety).
//    The combat FACE of each die is derived from its pip: 6=Kill, 4/5=Pain, else Miss.
//    Storing the pip (not just the face) lets Shapestealing compare the literal pip
//    value 1-6 to a Monster's Cost (§1.10 SB3) while Byagoona's Combat counts the
//    Kill/Pain faces (§1.8). Byagoona Awaken (v16) now rolls dice equal to the total
//    recruitment COST of sacrificed Monsters (not one die per Monster) — the main
//    generator now that Gather-Power rolling is retired; "Succor" (ex-OverlordOfDeath
//    object) also generates dice via its Doom-phase roll.
//  • Feast Upon Death (v16, was Self Consuming) is a flat per-elimination trigger —
//    see FBEExpansion.eliminate below. The old same-Area clustering tally var was
//    renamed game.fbeSelfConsumingDeaths -> game.fbeFeastDeaths and simplified to
//    $[Region] (no controller-faction tracking needed; no Doom clause any more).
//  • Ghasts are barred from any FBE game at SETUP (§1.6, creator-approved global
//    Ghast ban — see Game.scala loyaltyCards init), so every "Monster" rule reads
//    uniformly across Fungal Thralls and controlled Neutral Monsters with no
//    per-ability Ghast guard.
// ============================================================================


// ── UNITS (§1.7 / §2.1) ─────────────────────────────────────────────────────
// FBE uses the standard Acolyte UnitClass for its 6 Acolytes (no FBE-specific
// Cultist subclass — §2.1). FungalThrall: Monster cost 2, combat 2, pool 10.
// Byagoona: GreatOldOne (GOO) cost 10, combat = card-dice Kills+Pains (§1.8).
case object FungalThrall extends FactionUnitClass(FBE, "Fungal Thrall", Monster, FBEValues.FungalThrall_cost)
case object Byagoona     extends FactionUnitClass(FBE, "Byagoona",      GOO,     10)


// ── SPELLBOOKS (§1.10) ──────────────────────────────────────────────────────
// v16: Scala identifiers stay bound to their ORIGINAL library position/row id
// (not renamed) to avoid a Spellbook-serialization legacy shim (see header note).
// Each object's displayed `name` + mechanic below now reflects whatever row
// currently occupies that position in the v16 design.
case object ChangelingAdherents extends FactionSpellbook(FBE, "Animated Rush")        // was "Changeling Adherents"
case object NecromanticSpores   extends FactionSpellbook(FBE, "Faceless Ones")        // was "Necromantic Spores"
case object Shapestealing        extends FactionSpellbook(FBE, "Grinning Ghouls")      // was "Shapestealing"
case object AnimatedRush         extends FactionSpellbook(FBE, "Necromantic Spores")   // was "Animated Rush"
case object Succor               extends FactionSpellbook(FBE, "Dimorphic Gestation")  // was "Succor"
case object OverlordOfDeath      extends FactionSpellbook(FBE, "Succor")               // was "Overlord of Death"

// FBE Unique Ability — Feast Upon Death (Ongoing). Declared as a FactionSpellbook so
// every reference uses the FCG-blessed FeastUponDeath.styled(FBE) form and so it can
// be shown in the faction status panel / borrowed via Ancient Sorcery.
case object FeastUponDeath extends FactionSpellbook(FBE, "Feast Upon Death")  // was "Self Consuming"


// ── SPELLBOOK REQUIREMENTS (§1.9) ───────────────────────────────────────────
// Same identifier-stability rule as the Spellbooks above (Requirement text is
// purely cosmetic for replay — parseRequirement matches by class name only —
// but we keep the identifiers paired with the SAME library position for clarity).
case object ChangelingAdherentsReq extends Requirement("3+ Kills are Rolled in Any Battle")
case object NecromanticSporesReq   extends Requirement("As an Action, Eliminate a Fungal Thrall and Discard a die from your Faction Card")
case object ShapestealingReq        extends Requirement("Have 3 Units in an Enemy Start Area")
case object AnimatedRushReq         extends Requirement("As an Action, Eliminate a Fungal Thrall and Discard a die from your Faction Card")
case object SuccorReq               extends Requirement("Byagoona is Slain")
case object OverlordOfDeathReq      extends Requirement("Awaken Byagoona")


// ============================================================================
// FACELESS BLIGHT (FBE) FACTION OBJECT
// ============================================================================
case object FBE extends Faction { f =>
    def name  = "Faceless Blight"
    def short = "FBE"
    def style = "fbe"

    // Feast Upon Death is the always-on unique ability (§1.5.1).
    override def abilities = $(FeastUponDeath)
    override def library   = $(ChangelingAdherents, NecromanticSpores, Shapestealing, AnimatedRush, Succor, OverlordOfDeath)
    override def requirements(options : $[GameOption]) =
        $(ChangelingAdherentsReq, NecromanticSporesReq, ShapestealingReq, AnimatedRushReq, SuccorReq, OverlordOfDeathReq)

    // 6 Acolytes + 10 Fungal Thralls (pool) + 1 Byagoona. (§1.6 / §2.1)
    val allUnits =
        FBEValues.Acolyte_count.times(Acolyte)           ++
        FBEValues.FungalThrall_count.times(FungalThrall) ++
        FBEValues.Byagoona_count.times(Byagoona)

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
        // Fungal Thrall combat comes from faction-values/fbe.json (FungalThrall.combat); other Monsters (Shapestolen) fight at their cost.
        units.%(u => u.uclass != Byagoona && u.uclass.utype == Monster && !u.uclass.is[NeutralMonster]).not(Zeroed)./(u => (u.uclass == FungalThrall).?(FBEValues.FungalThrall_combat).|(u.uclass.cost)).sum +
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
    // §4.6 live cost preview (creator-corrected 2026-09-23): "Power needed: max(0, 8 - N)",
    // N = Monsters picked so far (was 10 minus their summed recruitment cost).
    override def question(implicit game : Game) = {
        val n = picked.num
        ("Awaken " + Byagoona.name).styled(FBE) + ": choose Monsters to Eliminate in " + game.desc(r) +
            " (Eliminated so far: " + n + "; Power needed: " + math.max(0, 8 - n).power + ")"
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
case class DistributedDeathDiceSelectedAction(self : Faction)
    extends ForcedAction with PowerNeutral with Soft

// ── NECROMANTIC SPORES (Post-Battle, §1.10 SB2 / §3.10.2 / §4.4.2) ───────────
case class NecromanticSporesMainAction(self : Faction, n : Int)
    extends OptionFactionAction(implicit g => ("Necromantic Spores".styled(FBE) + ": Eliminate a Monster you control to spawn " + n + " " + FungalThrall.name.styled(FBE))) with PostBattleQuestion
case class NecromanticSporesSkipAction(self : Faction)
    extends OptionFactionAction("Necromantic Spores".styled(FBE) + ": skip") with PostBattleQuestion
case class NecromanticSporesEliminateAction(self : Faction, monster : UnitRef, r : Region, n : Int)
    extends BaseFactionAction("Necromantic Spores".styled(FBE) + ": Eliminate", implicit g => { val mu = g.unit(monster); mu.uclass.styled(FBE) + " in " + mu.region + " (Cost " + mu.uclass.cost + ")" })

// ── SHAPESTEALING (Pre-Battle, §1.10 SB3 / §3.10.3 / §4.4.3) ─────────────────
// [LEGACY REPLAY ONLY] v16 retired this die-roll-steal mechanic in favor of
// "Grinning Ghouls" below (same Shapestealing identifier, new text). These
// classes and their Battle.scala/FactionFBE.scala handlers are kept ONLY so
// already-recorded games replay identically; no offer site remains for them.
case class ShapestealingPreBattleAction(self : Faction)
    extends OptionFactionAction(Shapestealing.styled(FBE)) with PreBattleQuestion
case class ShapestealingSkipAction(self : Faction)
    extends OptionFactionAction(("Skip " + Shapestealing.name).styled(FBE)) with PreBattleQuestion
case class ShapestealingTargetAction(self : Faction, enemyMonster : UnitRef)
    extends BaseFactionAction(Shapestealing.styled(FBE) + ": roll on", implicit g => g.unit(enemyMonster).uclass.styled(g.unit(enemyMonster).faction) + " (Cost " + g.unit(enemyMonster).uclass.cost + ")")
// Records the specific card die the PLAYER chose to discard (its pip value) so the
// choice PERSISTS through a page refresh — the manual pick itself is Soft/unrecorded,
// but this hard action bakes the chosen die into the game log for replay.
case class ShapestealingRollAction(self : Faction, enemyMonster : UnitRef, discardedDie : Int)
    extends ForcedAction
case class ShapestealingResolveAction(self : Faction, enemyMonster : UnitRef, roll : Int)
    extends ForcedAction

// ── GRINNING GHOULS (Shapestealing identifier, v16 new mechanic, Pre-Battle) ─
// "Choose an enemy Unit Type present. If a Unit of that Type is Killed, place
// a Fungal Thrall in the Area Post-Battle." "Unit Type" here means a specific
// enemy UnitClass present in the battle (mirrors BB's Predator "choose an
// enemy unit type" idiom — FactionBB.scala PredatorTypeChoiceAction — which
// also picks a UnitClass, not the broader Cultist/Monster/Terror/GOO UnitType
// enum). Declared Pre-Battle (no cost, no card-die spend — purely a choice).
case class GrinningGhoulsPreBattleAction(self : Faction)
    extends OptionFactionAction(Shapestealing.styled(FBE)) with PreBattleQuestion
case class GrinningGhoulsSkipAction(self : Faction)
    extends OptionFactionAction(("Skip " + Shapestealing.name).styled(FBE)) with PreBattleQuestion
case class GrinningGhoulsChooseTypeAction(self : Faction, uc : UnitClass)
    extends BaseFactionAction(Shapestealing.styled(FBE) + ": choose an enemy unit type",
        implicit g => uc.styled(g.factions.find(f => f != self && f.allInPlay.%(_.uclass == uc).any).getOrElse(self)))

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

// ── FACELESS ONES (NecromanticSpores identifier, v16 new mechanic, Action, Cost 2)
// "Choose an Area your Acolyte and an Enemy Acolyte Controlling a Gate. Your
// opponent must retreat that Acolyte to an adjacent Area unless they Eliminate
// another Unit Present."
case class FacelessOnesMainAction(self : Faction, l : $[(Region, UnitRef, Faction)])
    extends OptionFactionAction(NecromanticSpores.styled(FBE) + " (Cost " + 2.power + ")") with MainQuestion with Soft
case class FacelessOnesTargetAction(self : Faction, r : Region, target : UnitRef, e : Faction)
    extends ForcedAction {
    override def question(implicit game : Game) = NecromanticSpores.styled(FBE) + ": choose an Area"
}
case class FacelessOnesRetreatAction(self : Faction, r : Region, target : UnitRef, e : Faction)
    extends OptionFactionAction(implicit g => "Retreat " + g.unit(target).full + " from " + r) with PowerNeutral {
    override def question(implicit game : Game) = NecromanticSpores.styled(FBE) + ": retreat or Eliminate"
}
case class FacelessOnesEliminateMainAction(self : Faction, r : Region, target : UnitRef, e : Faction)
    extends OptionFactionAction("Eliminate another Unit instead") with PowerNeutral {
    override def question(implicit game : Game) = NecromanticSpores.styled(FBE) + ": retreat or Eliminate"
}
case class FacelessOnesRetreatPickAction(self : Faction, r : Region, target : UnitRef, e : Faction, dest : Region)
    extends ForcedAction with PowerNeutral
case class FacelessOnesEliminatePickAction(self : Faction, r : Region, target : UnitRef, e : Faction, victim : UnitRef)
    extends ForcedAction with PowerNeutral

// ── SUCCOR (code identifier; v16 displays "Overlord of Death" slot's OLD
// eliminate/roll/Elder-Sign mechanic now lives under the OverlordOfDeath Spellbook
// identifier — see FBEExpansion header note on the v16 identifier swap. These
// classes keep their original names for replay stability (Action extends Record);
// only the display text and gating Spellbook were changed, below and at the call
// sites in FBEExpansion.perform()/DoomAction.) (Doom Phase, §1.10 SB5 / §3.10.5 / §4.4.5) ──
case class SuccorMainAction(self : Faction)
    extends OptionFactionAction(OverlordOfDeath.styled(FBE)) with DoomQuestion with Soft
case class SuccorSkipAction(self : Faction)
    extends OptionFactionAction(("Skip " + OverlordOfDeath.name).styled(FBE)) with DoomQuestion with Soft
case class SuccorPickAction(self : Faction, picked : $[UnitRef], remaining : $[UnitRef])
    extends ForcedAction with PowerNeutral with Soft {
    // §4.4.5 prompt: "choose Units to Eliminate" (vs current Ritual Marker R).
    override def question(implicit game : Game) =
        OverlordOfDeath.styled(FBE) + ": choose Units to Eliminate (roll vs Ritual Marker " + game.ritualCost + ")"
}
case class SuccorRollAction(self : Faction, picked : $[UnitRef], rolls : $[Int])
    extends ForcedAction

// ── DIMORPHIC GESTATION (code identifier Succor; v16 new mechanic, Ongoing,
// §1.10 SB5 new text) — "Whenever you Summon a Monster or Recruit a Cultist,
// Discard Dice from your Faction Card equal to its Cost to repeat the Action
// for free in the same Area." Self-contained: unlike DC's Tenebrosum (which
// broadens Game.scala's shared Recruit/Summon choosers so a repeat may target
// a DIFFERENT unit), FBE's design text says "repeat the Action" — no unit-
// switching requirement — so the repeat directly re-places the SAME unit class
// in the SAME Region via self.place(), without touching Game.scala's shared
// RecruitAction/SummonAction/SummonFromPoolAction handlers at all. Recorded via
// a dedicated observing arm (see FBEExpansion.perform below) mirroring the
// recording half of DC's recordTenebrosum, keyed on game.fbeLastRecruitOrSummon.
case class DimorphicGestationMainAction(self : Faction, uc : UnitClass, r : Region, cost : Int)
    extends OptionFactionAction(
        Succor.styled(FBE) + ": repeat " + uc.styled(self) + " in " + r + " for " + cost.power + " (discard dice)")
    with MainQuestion with Soft with PowerNeutral
case class DimorphicGestationRepeatAction(self : Faction, uc : UnitClass, r : Region, cost : Int)
    extends BaseFactionAction(
        Succor.styled(FBE) + ": repeat " + uc.styled(self) + " in " + r,
        "Confirm".styled("power"))
    with PowerNeutral
// Forced continuation after the manual die picker returns the discarded dice
// (picker itself is Soft/unrecorded — see ManualDiePickAction).
case class DimorphicGestationDiceSelectedAction(self : Faction, uc : UnitClass, r : Region, discarded : $[Int])
    extends ForcedAction with PowerNeutral with Soft

// ── OVERLORD OF DEATH — [LEGACY REPLAY ONLY] Infernal-Pact-style cost discount
// (HB Fix 127, §1.10 SB6 / §3.10.6, RETIRED in v16). This mechanic is no longer
// offered in fresh play (see FBEExpansion.perform() DoomAction/MainAction: the
// offer sites were removed) — the OverlordOfDeath identifier now displays and
// plays as "Succor" (the eliminate/roll/Elder-Sign mechanic above). These classes
// and their handlers are kept ONLY so already-recorded games replay identically.
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

// [LEGACY REPLAY ONLY] USER-2026-07-22: Overlord of Death discount usable in the
// Doom Phase. See retirement note above — no longer offered in fresh play.
case class OverlordOfDeathDoomMainAction(self : Faction)
    extends OptionFactionAction("Discount with " + OverlordOfDeath.styled(FBE)) with DoomQuestion with Soft with PowerNeutral
case class OverlordOfDeathDoomPickAction(self : Faction, picked : $[UnitRef], remaining : $[UnitRef])
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) =
        OverlordOfDeath.styled(FBE) + ": choose Monsters to Eliminate (discount so far: " + picked.num.power + ")"
}
case class OverlordOfDeathDoomDoneAction(self : Faction, picked : $[UnitRef])
    extends OptionFactionAction("Done".styled("power")) with PowerNeutral { def question(implicit game : Game) = OverlordOfDeath.styled(FBE) }
case class OverlordOfDeathDoomCancelAction(self : Faction)
    extends OptionFactionAction("Cancel " + OverlordOfDeath.styled(FBE)) with PowerNeutral { def question(implicit game : Game) = OverlordOfDeath.styled(FBE) }

// ── NECROMANTIC SPORES / ANIMATED RUSH REQUIREMENTS (§3.12.2, §3.12.4) ──────
// v16: both Reqs share identical text "Eliminate a Fungal Thrall and Discard a
// die from your Faction Card" — satisfying ONE normally costs 1 Thrall + 1 Die;
// the sbr multiText lets a single Action satisfy BOTH Reqs at once by spending
// 2 Thralls + 2 Dice. 0 Power Common Action; the Elimination(s) DO trigger Self
// Consuming (+1 Power each).
case class FungalThrallDiceMainAction(self : Faction)
    extends OptionFactionAction(("Eliminate " + FungalThrall.name + "(s) and Discard Dice").styled(FBE)) with MainQuestion with Soft
case class FungalThrallDicePickAction(self : Faction, picked : $[UnitRef], remaining : $[UnitRef])
    extends ForcedAction with PowerNeutral with Soft
case class FungalThrallDiceDoneAction(self : Faction, picked : $[UnitRef])
    extends BaseFactionAction(("Eliminate " + FungalThrall.name + "(s)").styled(FBE), "Done".styled("power"))

// ── MANUAL DIE SELECTION FRAMEWORK (§USER-2026-07-22) ────────────────────────
// Optional flow for manual die selection instead of auto-selecting lowest.
// Sealed trait to indicate which ability is requesting die selection.
sealed trait DieSelectionContext
case object AnimatedRushContext extends DieSelectionContext
case object FungalThrallDiceContext extends DieSelectionContext
case object ShapestealingContext extends DieSelectionContext
case object DistributedDeathContext extends DieSelectionContext
case object DimorphicGestationContext extends DieSelectionContext

// Manual die picker: player chooses which dice to discard from the faction card.
case class ManualDiePickAction(self : Faction, context : DieSelectionContext, needed : Int, selected : $[Int], continuation : $[Int] => ForcedAction)
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) = {
        val abilityName = context match {
            case AnimatedRushContext => "Animated Rush"
            case FungalThrallDiceContext => "Faction Card"
            case ShapestealingContext => "Shapestealing"
            case DistributedDeathContext => "Distributed Death"
            case DimorphicGestationContext => "Dimorphic Gestation"
        }
        abilityName.styled(FBE) + ": choose " + (needed - selected.num) + " more " + ((needed - selected.num) == 1).?("die").|("dice") + " to discard"
    }
}

// Individual die choice during manual selection.
case class ManualDieChooseAction(self : Faction, context : DieSelectionContext, needed : Int, selected : $[Int], dieValue : Int, continuation : $[Int] => ForcedAction)
    extends OptionFactionAction(implicit g => "Discard die: " + dieValue.toString) with PowerNeutral with Soft {
    override def question(implicit game : Game) = {
        val abilityName = context match {
            case AnimatedRushContext => "Animated Rush"
            case FungalThrallDiceContext => "Faction Card"
            case ShapestealingContext => "Shapestealing"
            case DistributedDeathContext => "Distributed Death"
            case DimorphicGestationContext => "Dimorphic Gestation"
        }
        abilityName.styled(FBE) + ": choose dice to discard"
    }
}

// Undo a die selection during manual picking.
case class ManualDieUndoLastAction(self : Faction, context : DieSelectionContext, needed : Int, selected : $[Int], continuation : $[Int] => ForcedAction)
    extends OptionFactionAction("Undo last selection") with PowerNeutral with Soft {
    override def question(implicit game : Game) = {
        val abilityName = context match {
            case AnimatedRushContext => "Animated Rush"
            case FungalThrallDiceContext => "Faction Card"
            case ShapestealingContext => "Shapestealing"
            case DistributedDeathContext => "Distributed Death"
            case DimorphicGestationContext => "Dimorphic Gestation"
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

    // ── FEAST UPON DEATH (v16, was Self Consuming) (§1.5.1 / §3.6) ───────────
    // Universal death hook: every Unit Eliminated during the active Action (battle
    // or otherwise), by a means OTHER than Capture, is recorded with the Area it
    // died in. afterAction then awards 1 Power per Area that had at least one such
    // death AND at least one FBE Unit present (still there, or one of FBE's own that
    // died there — dying there still counts as present). Flat per-elimination
    // trigger — no same-Area 2+/3+ clustering tally is needed any more.
    // Exclusions: High Priest sacrifices (Unspeakable Oath) are not part of an
    // Action; Capture-caused deaths (fbeCaptureInProgress) are explicitly excluded
    // by the v16 ability text ("Eliminated by a means other than Capture").
    override def eliminate(u : UnitFigure)(implicit game : Game) {
        if (!game.setup.has(FBE)) return
        if (game.fbeHPSacrificeInProgress) return
        if (game.fbeCaptureInProgress) return
        game.fbeFeastDeaths :+= u.region
    }

    // ── FEAST UPON DEATH resolution (fires at end of every Action) (§3.6) ────
    override def afterAction()(implicit game : Game) {
        if (!game.setup.has(FBE)) return
        val deathAreas = game.fbeFeastDeaths.distinct
        // 2026-07-27 (carried over from Self Consuming): award to EVERY faction that
        // currently has the ability, not just FBE — SL can copy it with Ancient
        // Sorcery and must collect it too.
        deathAreas.foreach { r =>
            game.factions.%(_.has(FeastUponDeath)).foreach { f =>
                if (f.at(r).any) {
                    f.power += 1
                    f.log(FeastUponDeath.styled(FBE) + ": a Unit was Eliminated (other than by Capture) in", r, "— gained", 1.power)
                }
            }
        }
        game.fbeFeastDeaths = $
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
        // SBR 2/4 — Necromantic Spores / Animated Rush (v16): both share the text
        // "Eliminate a Fungal Thrall and Discard a die from your Faction Card". The
        // player must actively perform that combined Action (see
        // FungalThrallDiceMainAction below) to satisfy either or both Reqs.
    }

    // Eligible Monsters FBE controls = Fungal Thralls + any controlled Neutral
    // Monster on the map. Ghasts cannot appear (barred at setup, §1.6).
    def controlledMonsters(r : Region)(implicit game : Game) : $[UnitFigure] =
        FBE.at(r).%(u => u.uclass.utype == Monster)
    // "In play" for FBE ability purposes includes the Moon (off-map FactionRegion):
    // Overlord of Death, Succor, Byagoona awaken etc. target/eliminate units, which
    // is allowed on the Moon. onMap alone excludes the Moon, so add BB.moon.
    def controlledMonstersAnywhere(implicit game : Game) : $[UnitFigure] =
        FBE.units.%(u => (u.region.onMap || u.region == BB.moon) && u.uclass.utype == Monster)

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

        // [LEGACY REPLAY] v7 "Changeling Adherents" rolled 1 die per gate-Acolyte
        // during Gather Power. v16 relocates identifier ChangelingAdherents's mechanic
        // to the new "Animated Rush" (Byagoona-move double-move, see MovedAction below)
        // — Gather Power dice-rolling is retired, no design row describes it any more.
        // ChangelingAdherentsRollAction's handler is kept below (dead for new play) so
        // any already-recorded game history referencing it still replays identically.
        case ChangelingAdherentsRollAction(self, remaining, rolled) =>
            if (remaining <= 0) {
                game.fbeCardDice = game.fbeCardDice ++ rolled
                self.log("Changeling Adherents".styled(FBE) + ": rolled",
                    rolled./(p => FBEExpansion.face(p).toString).mkString(", ") + ", placed on Faction Card")
                game.triggers()
                Force(AfterPowerGatherAction)
            }
            else
                RollD6(_ => "Changeling Adherents".styled(FBE) + ": roll for Acolyte controlling a Gate",
                    pip => ChangelingAdherentsRollAction(self, remaining - 1, rolled :+ pip))

        // ── DOOM PHASE (§3.10.5 Succor) ──────────────────────────────────────
        case DoomAction(f : FBE.type) =>
            implicit val asking = Asking(f)

            // USER-2026-07-22: transient power boost so a pending Overlord of Death
            // discount lets the Ritual be OFFERED at its effective (discounted) cost.
            // Same pattern as the Main-phase offer below; restored before returning.
            val doomOodBoost = game.fbeOverlordDiscount
            if (doomOodBoost > 0)
                f.power += doomOodBoost

            game.rituals(f)

            game.reveals(f)

            game.highPriests(f)

            game.hires(f)

            game.recruitsWhateley(f)

            game.motherOfMonsters(f)

            // v16: the OverlordOfDeath identifier now displays/plays as "Succor"
            // (eliminate Units, roll that many dice vs Ritual Marker for an Elder
            // Sign). Offer once per Doom phase if acquired and FBE has any unit
            // (Moon units count — eliminating Units is allowed on the Moon).
            if (f.can(OverlordOfDeath) && f.units.%(u => u.region.onMap || u.region == BB.moon).any && !f.oncePerAction.has(OverlordOfDeath))
                + SuccorMainAction(f)

            // [LEGACY REPLAY ONLY] USER-2026-07-22: Overlord of Death discount was
            // usable in the Doom Phase to reduce a Ritual's cost. Retired in v16 —
            // no longer offered in fresh play (the identifier now plays as Succor
            // above), but a pending discount from an old save must still resolve.
            if (game.fbeOverlordDiscount > 0)
                + OverlordOfDeathDoomCancelAction(f)

            if (doomOodBoost > 0)
                f.power -= doomOodBoost

            game.doomDone(f)

            asking

        // ── MAIN ACTION (§2.7 / §3.10) ───────────────────────────────────────
        case MainAction(f : FBE.type) if f.active.not =>
            UnknownContinue

        case MainAction(f : FBE.type) if f.acted =>
            implicit val asking = Asking(f)

            game.controls(f)

            if (f.hasAllSB)
                game.battles(f)

            // Dimorphic Gestation (v16, Succor identifier, Ongoing): offer a free
            // repeat of the just-resolved Recruit/Summon if FBE has it and enough
            // card dice to discard equal to the unit's Cost. self-contained — see
            // DimorphicGestationMainAction/RepeatAction above and the recording
            // arm below (RecruitAction/SummonAction-family observing cases).
            if (f.can(Succor))
                game.fbeLastRecruitOrSummon.foreach { case (uc, r, cost) =>
                    if (game.fbeCardDice.num >= cost)
                        + DimorphicGestationMainAction(f, uc, r, cost)
                }

            game.reveals(f)

            game.endTurn(f)(true)

            + NeedOk

            asking

        case MainAction(f : FBE.type) =>
            implicit val asking = Asking(f)

            // [LEGACY REPLAY ONLY] HB Fix 127: Overlord of Death cost discount was
            // retired in v16 (the identifier now plays as Succor, Doom-phase only —
            // see DoomAction above). No longer offered here, but an old save with a
            // discount already pending must still be able to cancel/spend it down.
            if (game.fbeOverlordDiscount > 0)
                + OverlordOfDeathCancelMainAction(f)

            // [LEGACY REPLAY ONLY] HB Fix 127: Transient power boost for offer checks
            // (same pattern as FB Infernal Pact), kept only to let an old save's
            // already-pending discount still be spendable via the intercept arms below.
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

            // Byagoona custom Awaken (§1.8, creator-corrected 2026-09-23: pay 8 Power
            // minus 1 per Monster sacrificed, was 10 minus their summed recruitment
            // cost): offer if Byagoona is in the pool and there exists an area where
            // (Monster count there) + current power >= 8.
            if (f.pool.%(_.uclass == Byagoona).any && controlledMonstersAnywhere./(_.region).distinct.exists(r =>
                controlledMonsters(r).num + f.power >= FBEValues.Byagoona_awakenCost))
                + ByagoonaAwakenMainAction(f)

            // Necromantic Spores / Animated Rush requirements (§3.12.2, §3.12.4, v16):
            // both share "Eliminate a Fungal Thrall and Discard a die from your Faction
            // Card" — offer the combined Action whenever at least one Req is open, a
            // Thrall is available (Moon counts), and a card die is available.
            if ((f.needs(NecromanticSporesReq) || f.needs(AnimatedRushReq)) &&
                f.units.%(u => u.uclass == FungalThrall && (u.region.onMap || u.region == BB.moon)).any &&
                game.fbeCardDice.any)
                + FungalThrallDiceMainAction(f)

            // Faceless Ones (v16 new mechanic, NecromanticSpores identifier): choose
            // an Area with your Acolyte and an enemy Acolyte controlling a Gate there.
            if (f.can(NecromanticSpores) && f.power >= FBEValues.FacelessOnes_cost) {
                val targets = f.onMap(Acolyte)./(_.region).distinct./~{ r =>
                    f.enemies./~(e => e.at(r, Acolyte).%(_.onGate)./(u => (r, u.ref, e)))
                }
                if (targets.any)
                    + FacelessOnesMainAction(f, targets)
            }

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
            // Only show areas where (Monster count there) + current power >= 8 (§1.8, corrected).
            val eligibleAreas = controlledMonstersAnywhere./(_.region).distinct.%(r =>
                controlledMonsters(r).num + self.power >= FBEValues.Byagoona_awakenCost)
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
            // v16: roll dice equal to the TOTAL COST of the Eliminated Monsters
            // (was one die per Monster). Replay-safe via RollD6 chain, rolled all
            // up front, capturing the pips in the action.
            val diceNeeded = picked./(ur => game.unit(ur).uclass.cost).sum
            def rollAll(acc : $[Int]) : Continue =
                if (acc.num >= diceNeeded)
                    Force(ByagoonaAwakenRollAction(self, r, picked, acc))
                else
                    RollD6(_ => "Roll " + Byagoona.name.styled(FBE) + " Awaken die " + (acc.num + 1) + "/" + diceNeeded,
                        pip => ByagoonaAwakenRollAction(self, r, picked, acc :+ pip))
            rollAll($)

        case ByagoonaAwakenRollAction(self, r, picked, rolls) =>
            val diceNeeded = picked./(ur => game.unit(ur).uclass.cost).sum
            if (rolls.num < diceNeeded) {
                // Continue rolling.
                RollD6(_ => "Roll " + Byagoona.name.styled(FBE) + " Awaken die " + (rolls.num + 1) + "/" + diceNeeded,
                    pip => ByagoonaAwakenRollAction(self, r, picked, rolls :+ pip))
            }
            else {
                val n = picked.num
                // §1.8: pay 8 Power minus 1 per Monster sacrificed.
                val owed = math.max(0, FBEValues.Byagoona_awakenCost - n)
                // Eliminate the sacrificed Monsters (drives Feast Upon Death).
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
                    ("Monster".s(n)).styled(FBE) + ", rolled",
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

        case DistributedDeathDiceSelectedAction(self) =>
            UnknownContinue

        // ── NECROMANTIC SPORES (§1.10 SB2 / §3.10.2) ─────────────────────────
        // FBE-side logic (Eliminate a controlled Monster, spawn Fungal Thralls);
        // Battle.scala's matching cases resume the battle via proceed().
        case NecromanticSporesMainAction(self, n) =>
            game.battle match {
                case Some(b) =>
                    // Exclude shapestolen units from Necromantic Spores (per user requirement).
                    // Shapestolen units are battle-temp controlled, not owned.
                    val monsters = controlledMonsters(b.arena).%(u => !game.fbeShapestolen.contains(u.ref))
                    if (monsters.num == 1)
                        Force(NecromanticSporesEliminateAction(self, monsters.head.ref, b.arena, n))
                    else if (monsters.nonEmpty)
                        Ask(self).each(monsters)(u =>
                            NecromanticSporesEliminateAction(self, u.ref, b.arena, n))
                    else {
                        // No eligible monsters (all are shapestolen)
                        self.log("Necromantic Spores".styled(FBE) + ": no eligible Monsters (stolen units excluded)")
                        UnknownContinue
                    }
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

        // ── GRINNING GHOULS (Shapestealing identifier, v16, Pre-Battle, §1.10 SB3) ──
        // FBE picks an enemy UnitClass present in the battle; if a Unit of that
        // type is Killed this battle, a Fungal Thrall is placed Post-Battle (the
        // payoff check lives in Battle.scala's PostBattlePhase, which owns the
        // authoritative `eliminated` list).
        case GrinningGhoulsPreBattleAction(self) =>
            game.battle match {
                case Some(b) =>
                    val enemySide = if (b.attacker == FBE) b.defenders else b.attackers
                    val enemyTypes = enemySide.forces.%(_.faction != FBE)./(_.uclass).distinct
                    Ask(self).each(enemyTypes)(uc => GrinningGhoulsChooseTypeAction(self, uc)).cancel
                case None => UnknownContinue
            }

        case GrinningGhoulsChooseTypeAction(self, uc) =>
            game.battle.foreach(_.fbeGrinningGhoulsChosenType = Some(uc))
            self.log(Shapestealing.styled(FBE) + ": chose", uc.styled(FBE), "— a Fungal Thrall will be placed if one is Killed this Combat")
            UnknownContinue

        case GrinningGhoulsSkipAction(self) =>
            self.log("Skip ".styled(FBE) + Shapestealing.styled(FBE))
            UnknownContinue

        // [LEGACY REPLAY ONLY] — old Shapestealing die-roll-steal mechanic, retired
        // in v16. No offer site remains; kept only so old recorded games replay.
        case ShapestealingPreBattleAction(self) =>
            game.battle match {
                case Some(b) =>
                    val enemySide = if (b.attacker == FBE) b.defenders else b.attackers
                    val enemyMonsters = enemySide.forces.%(u => u.faction != FBE && u.uclass.utype == Monster)
                    Ask(self).each(enemyMonsters)(u => ShapestealingTargetAction(self, u.ref)).cancel
                case None => UnknownContinue
            }

        case ShapestealingTargetAction(self, enemyMonster) =>
            // OLD-FORMAT REPLAY COMPATIBILITY: games recorded before this change
            // discarded the LOWEST die HERE and had no separate roll action (the next
            // recorded action was ShapestealingResolveAction). Detect that by peeking
            // at the next recorded action; if it is NOT a ShapestealingRollAction,
            // reproduce the legacy discard-lowest + roll so those games replay
            // identically. New games record a ShapestealingRollAction that carries the
            // player's chosen die.
            if (game.nextReplayActionHint.any && !game.nextReplayActionHint.exists(_.contains("ShapestealingRollAction"))) {
                val sorted = game.fbeCardDice.sortBy(x => x)
                val discarded = sorted.headOption.getOrElse(0)
                game.fbeCardDice = sorted.drop(1)
                self.log(Shapestealing.styled(FBE) + ": discarded lowest card die (value", discarded.toString + ")")
                RollD6(_ => Shapestealing.styled(FBE) + ": roll for Shapestealing",
                    roll => ShapestealingResolveAction(self, enemyMonster, roll))
            }
            else
            // Live play (and new-format replay): the player manually chooses which die
            // to discard. On replay the resulting menu is ignored and the recorded
            // ShapestealingRollAction (carrying the chosen die) is applied instead.
            Force(ManualDiePickAction(self, ShapestealingContext, 1, $, selectedDice =>
                ShapestealingRollAction(self, enemyMonster, selectedDice.headOption.getOrElse(0))))

        case ShapestealingRollAction(self, enemyMonster, discardedDie) =>
            // Discard the SPECIFIC die the player chose (persisted across reload). Fall
            // back to the lowest die only if the recorded value is missing/absent (e.g.
            // an interrupted pick) so the card count still decrements correctly.
            val toDiscard =
                if (discardedDie > 0 && game.fbeCardDice.contains(discardedDie)) discardedDie
                else game.fbeCardDice.sortBy(x => x).headOption.getOrElse(0)
            if (game.fbeCardDice.contains(toDiscard))
                game.fbeCardDice = game.fbeCardDice.diff($(toDiscard))
            self.log(Shapestealing.styled(FBE) + ": discarded chosen card die (value", toDiscard.toString + ")")
            RollD6(_ => Shapestealing.styled(FBE) + ": roll for Shapestealing",
                roll => ShapestealingResolveAction(self, enemyMonster, roll))

        case ShapestealingResolveAction(self, enemyMonster, roll) =>
            val mon = game.unit(enemyMonster)
            val cost = mon.uclass.cost
            if (roll > cost) {
                // Battle-scoped control: track the unit for this Combat only.
                // Ownership stays with the original faction; FBE gains temporary control
                // for combat dice and hit assignment during this battle.
                // On battle-end, this tracking is cleared and control reverts automatically.
                game.fbeShapestolen :+= enemyMonster
                self.log(Shapestealing.styled(FBE) + ": rolled", roll.toString + " vs Cost",
                    cost.toString + " —", mon.uclass.styled(mon.faction), "fights for", FBE.full, "this Combat")
            }
            else
                self.log(Shapestealing.styled(FBE) + ": rolled", roll.toString + " vs Cost",
                    cost.toString + " — failed")
            UnknownContinue   // Battle.scala ShapestealingResolveAction case resumes via proceed()

        case ShapestealingSkipAction(self) =>
            self.log("Skip ".styled(FBE) + Shapestealing.styled(FBE))
            UnknownContinue

        // ── ANIMATED RUSH (v16 row 1, now bound to identifier ChangelingAdherents
        // which displays "Animated Rush" — §1.10 SB1) ────────────────────────────
        // Text: "When Byagoona Moves, Discard any number of Dice from your Faction
        // Card to move exactly twice that many other Units for free." This is the
        // same mechanic the pre-v16 code implemented under the OLD "Animated Rush"
        // library slot (identifier AnimatedRush); only the capability-gate check
        // moved to ChangelingAdherents to match the v16 identifier swap (header note).
        // After Byagoona moves: discard dice to move 2 non-Byagoona units per die,
        // each from any area to any adjacent area (independent, not a carry).
        case MovedAction(self : FBE.type, u, from, to)
            if game.unit(u).uclass == Byagoona && self.can(ChangelingAdherents) && game.fbeCardDice.nonEmpty
               && self.units.%(u => u.region.onMap || u.region == BB.moon).%(_.uclass != Byagoona).any =>
            implicit val asking = Asking(self)
            val maxK = math.min(game.fbeCardDice.num, self.units.%(u => u.region.onMap || u.region == BB.moon).%(_.uclass != Byagoona).num / 2)
            (1 to maxK).foreach(k => + AnimatedRushMainAction(self, from, to, k))
            + AnimatedRushSkipAction(self)
            asking

        case AnimatedRushMainAction(self, source, dest, n) =>
            // REPLAY COMPATIBILITY (mirrors Distributed Death, Battle.scala
            // fbeDistributedDeathSaveWithManualDice): the manual die-pick actions
            // (ManualDiePick/Choose/UndoLast) are Soft and are NEVER recorded in the
            // game log, so on replay there is no pick action to resume — the
            // discard-continuation below never runs and the dice would be REFUNDED on
            // every refresh ("dice expenditure recanted", reported in game 'Disaster
            // for Lunacy'). On replay, auto-discard the lowest n dice deterministically
            // (the pre-e4f1866 behaviour) so the spend PERSISTS. Only present the
            // interactive picker during genuine live play.
            if (game.nextReplayActionHint.any) {
                val discard = math.min(n, game.fbeCardDice.num)
                val discarded = game.fbeCardDice.sortBy(x => x).take(discard)
                game.fbeCardDice = game.fbeCardDice.sortBy(x => x).drop(discard)
                self.log("Animated Rush".styled(FBE) + ": discarded", discard, (discard == 1).?("die").|("dice"),
                    "— moving", 2 * n, "Units")
                Force(AnimatedRushUnitPickAction(self, 2 * n, $, discarded, $, source, dest))
            }
            else
            // Manual die selection: player chooses which dice to discard
            Force(ManualDiePickAction(self, AnimatedRushContext, n, $, selectedDice => {
                val discarded = selectedDice
                game.fbeCardDice = game.fbeCardDice.diff(selectedDice)
                self.log("Animated Rush".styled(FBE) + ": discarded", n, (n == 1).?("die").|("dice"),
                    "— moving", 2 * n, "Units")
                AnimatedRushUnitPickAction(self, 2 * n, $, discarded, $, source, dest)
            }))

        case AnimatedRushSkipAction(self) =>
            self.log("Animated Rush".styled(FBE) + ": declined")
            Force(MoveContinueAction(self, true))

        case AnimatedRushUnitPickAction(self, movesLeft, moved, discardedDice, originalPositions, rushSource, rushDest) =>
            if (movesLeft <= 0)
                // Chronophage: Animated Rush moved own units → offer teleport (A16). If nothing
                // actually moved, CronophageAfterMoveAction is a no-op and just continues.
                Force(moved.any.?(CronophageAfterMoveAction(self, MoveContinueAction(self, true)) : ForcedAction).|(MoveContinueAction(self, true)))
            else {
                val eligible = self.units.%(u => u.region.onMap || u.region == BB.moon).%(_.uclass != Byagoona).%(u => !moved.has(u.ref)).%(u => (if (u.region == BB.moon) areas else game.board.connected(u.region)).%(d => self.affords(0)(d)).any)
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
            // A unit on the Moon may move OFF to any Area (the Moon is adjacent to
            // all regions), mirroring normal movement. Nothing moves TO the Moon via
            // Animated Rush (board.connected never yields BB.moon).
            val rushDests = if (u.region == BB.moon) areas else game.board.connected(u.region)
            rushDests.%(dest => self.affords(0)(dest)).foreach { dest =>
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
            val maxK = math.min(game.fbeCardDice.num, self.units.%(u => u.region.onMap || u.region == BB.moon).%(_.uclass != Byagoona).num / 2)
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

        // ── FACELESS ONES (NecromanticSpores identifier, v16 new mechanic) ──────
        case FacelessOnesMainAction(self, l) =>
            implicit val asking = Asking(self)
            l.foreach { case (r, target, e) =>
                + FacelessOnesTargetAction(self, r, target, e)
                    .as("Target " + game.unit(target).full + " in " + r)
            }
            + CancelAction
            asking

        case FacelessOnesTargetAction(self, r, target, e) =>
            self.power -= FBEValues.FacelessOnes_cost
            self.payTax(r)
            self.log(NecromanticSpores.styled(FBE) + ": targeted", game.unit(target).full, "in", r)
            implicit val asking = Asking(e)
            + FacelessOnesRetreatAction(self, r, target, e)
            // Only offer the Eliminate-instead option if another Unit is present to Eliminate.
            if (e.at(r).%(_.ref != target).any)
                + FacelessOnesEliminateMainAction(self, r, target, e)
            asking

        case FacelessOnesRetreatAction(self, r, target, e) =>
            val dests = game.board.connectedForRetreat(r)
            val u = game.unit(target)
            if (dests.num == 1)
                Force(FacelessOnesRetreatPickAction(self, r, target, e, dests.head))
            else {
                implicit val asking = Asking(e)
                dests.foreach(d => + FacelessOnesRetreatPickAction(self, r, target, e, d).as(d)("Retreat", u.full, "from", r, "to"))
                asking
            }

        case FacelessOnesRetreatPickAction(self, r, target, e, dest) =>
            val u = game.unit(target)
            u.region = dest
            u.onGate = false
            e.log("retreated", u.full, "from", r, "to", dest, "(" + NecromanticSpores.styled(FBE) + ")")
            EndAction(self)

        case FacelessOnesEliminateMainAction(self, r, target, e) =>
            implicit val asking = Asking(e)
            e.at(r).%(_.ref != target).foreach(u => + FacelessOnesEliminatePickAction(self, r, target, e, u.ref).as(u.full))
            asking

        case FacelessOnesEliminatePickAction(self, r, target, e, victim) =>
            val u = game.unit(victim)
            e.log("Eliminated", u.full, "in", r, "(" + NecromanticSpores.styled(FBE) + ", avoiding retreat)")
            game.eliminate(u)
            EndAction(self)

        // ── SUCCOR mechanic, now under the OverlordOfDeath identifier (v16) ──
        // (§1.10 SB5 / §3.10.5) — see header note on the v16 identifier swap.
        case SuccorMainAction(self) =>
            // Eliminates your own units (to gain Doom) — Moon units are eligible.
            Force(SuccorPickAction(self, $, self.units.%(u => u.region.onMap || u.region == BB.moon)./(_.ref)))

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
                else RollD6(_ => OverlordOfDeath.styled(FBE) + ": roll d6 " + (acc.num + 1) + "/" + y,
                    pip => SuccorRollAction(self, picked, acc :+ pip))
            rollAll($)

        case SuccorRollAction(self, picked, rolls) =>
            val y = picked.num
            if (rolls.num < y)
                RollD6(_ => OverlordOfDeath.styled(FBE) + ": roll d6 " + (rolls.num + 1) + "/" + y,
                    pip => SuccorRollAction(self, picked, rolls :+ pip))
            else {
                picked.foreach(ur => game.eliminate(game.unit(ur)))
                // Doom Phase eliminations do NOT trigger Feast Upon Death (Actions only).
                game.fbeFeastDeaths = $
                val sum = rolls.sum
                val ritual = game.ritualCost
                // Format rolls as "1+2+2 = 5" for display (user request 2026-07-22)
                val rollDisplay = if (rolls.num > 1) rolls.mkString("+") + " = " + sum.toString else sum.toString
                if (sum > ritual) {
                    self.takeES(1)
                    self.log(OverlordOfDeath.styled(FBE) + ": Eliminated", y, "Units, rolled", rollDisplay,
                        "vs Ritual Marker", ritual.toString + " — gained", 1.es)
                }
                else
                    self.log(OverlordOfDeath.styled(FBE) + ": Eliminated", y, "Units, rolled", rollDisplay,
                        "vs Ritual Marker", ritual.toString + " — no Elder Sign")
                FBE.oncePerAction :+= OverlordOfDeath
                Force(DoomAction(FBE))
            }

        // ── OVERLORD OF DEATH — [LEGACY REPLAY ONLY] (§1.10 SB6 / §3.10.6, HB Fix 127)
        // Infernal-Pact-style cost discount, retired in v16 (unreachable from fresh
        // play — no offer site remains). Kept so old recorded games replay identically.
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
            // Deaths accumulate in fbeFeastDeaths — Feast Upon Death fires
            // after the discounted action resolves (flat per-elimination trigger).
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

        // ── OVERLORD OF DEATH — DOOM PHASE [LEGACY REPLAY ONLY] (USER-2026-07-22) ──
        // Mirror of the Main-phase handlers; retired in v16, unreachable from fresh
        // play (no offer site remains — see DoomAction above), kept for old replays.
        case OverlordOfDeathDoomMainAction(self) =>
            Force(OverlordOfDeathDoomPickAction(self, $, controlledMonstersAnywhere./(_.ref)))

        case OverlordOfDeathDoomPickAction(self, picked, remaining) =>
            implicit val asking = Asking(self)
            remaining.foreach { ur =>
                val u = game.unit(ur)
                + OverlordOfDeathDoomPickAction(self, picked :+ ur, remaining.but(ur))
                    .as(u.full + " in " + u.region + " (Cost " + u.uclass.cost + ")")
            }
            if (picked.any)
                + OverlordOfDeathDoomDoneAction(self, picked)
            + OverlordOfDeathDoomCancelAction(self)
            asking

        case OverlordOfDeathDoomDoneAction(self, picked) =>
            picked.foreach(ur => game.eliminate(game.unit(ur)))
            game.fbeOverlordDiscount = picked.num
            self.log(OverlordOfDeath.styled(FBE) + ": Eliminated", picked.num, "Monster" + (picked.num > 1).??("s") +
                ", next action discounted by", picked.num.power)
            Force(DoomAction(self))

        case OverlordOfDeathDoomCancelAction(self) =>
            // Forfeit a pending discount (monsters already eliminated); undo restores them.
            game.fbeOverlordDiscount = 0
            self.log(OverlordOfDeath.styled(FBE) + ": discount cancelled")
            Force(DoomAction(self))

        // ── NECROMANTIC SPORES / ANIMATED RUSH REQUIREMENTS (§3.12.2, §3.12.4, v16) ──
        // Eliminate 1 Fungal Thrall + Discard 1 die to satisfy whichever single Req
        // is still open; eliminate 2 Thralls + Discard 2 dice to satisfy BOTH at once
        // (sbr multiText bonus), capped by Thralls and card dice actually available.
        case FungalThrallDiceMainAction(self) =>
            Force(FungalThrallDicePickAction(self, $, self.units.%(u => u.uclass == FungalThrall && (u.region.onMap || u.region == BB.moon))./(_.ref)))

        case FungalThrallDicePickAction(self, picked, remaining) =>
            implicit val asking = Asking(self)
            val maxPick = math.min(2, game.fbeCardDice.num)
            if (picked.num < maxPick)
                remaining.foreach { ur =>
                    val u = game.unit(ur)
                    + FungalThrallDicePickAction(self, picked :+ ur, remaining.but(ur))
                        .as("Add " + u.full + " in " + u.region + " (Cost " + u.uclass.cost + ")")
                }
            if (picked.any)
                + FungalThrallDiceDoneAction(self, picked)
            + CancelAction
            asking

        case FungalThrallDiceDoneAction(self, picked) =>
            val n = picked.num
            picked.foreach(ur => game.eliminate(game.unit(ur)))
            self.log(("Eliminated " + n + " " + FungalThrall.name + (n > 1).??("s")).styled(FBE))

            def discardAndSatisfy() = {
                if (n == 2) {
                    self.satisfy(NecromanticSporesReq, NecromanticSporesReq.text)
                    self.satisfy(AnimatedRushReq, AnimatedRushReq.text)
                }
                else if (self.needs(NecromanticSporesReq))
                    self.satisfy(NecromanticSporesReq, NecromanticSporesReq.text)
                else
                    self.satisfy(AnimatedRushReq, AnimatedRushReq.text)
                EndAction(self)
            }

            // Replay-safety: the manual die picker is Soft (unrecorded), so on replay
            // auto-discard the lowest N dice deterministically instead of re-running
            // the interactive picker.
            if (game.nextReplayActionHint.any) {
                game.fbeCardDice = game.fbeCardDice.sortBy(x => x).drop(n)
                self.log(("Discarded " + n + " " + (n == 1).?("die").|("dice") +
                    " from Faction Card").styled(FBE))
                discardAndSatisfy()
            }
            else
                Force(ManualDiePickAction(self, FungalThrallDiceContext, n, $, selectedDice => {
                    game.fbeCardDice = game.fbeCardDice.diff(selectedDice)
                    self.log(("Discarded " + n + " " + (n == 1).?("die").|("dice") +
                        " from Faction Card").styled(FBE))
                    discardAndSatisfy()
                }))

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

        // ── DIMORPHIC GESTATION (v16, Succor identifier, Ongoing) ───────────
        // Record FBE's own Recruit/Summon so the post-acted MainAction branch
        // above can offer a free repeat. Mirrors the recording half of DC's
        // recordTenebrosum, but the repeat itself never touches these shared
        // handlers — see DimorphicGestationRepeatAction below. Only recorded
        // while the Ongoing ability is actually acquired (f.can(Succor)) so an
        // FBE game that hasn't earned it yet carries no dead state.
        case RecruitAction(self : FBE.type, uc, r) if self.can(Succor) =>
            game.fbeLastRecruitOrSummon = Some((uc, r, self.recruitCost(uc, r)))
            UnknownContinue
        case SummonAction(self : FBE.type, uc, r) if self.can(Succor) =>
            game.fbeLastRecruitOrSummon = Some((uc, r, self.summonCost(uc, r)))
            UnknownContinue
        case SummonFromPoolAction(self : FBE.type, uc, r) if self.can(Succor) =>
            game.fbeLastRecruitOrSummon = Some((uc, r, self.summonCost(uc, r)))
            UnknownContinue
        case SummonFromVelvetFanAction(self : FBE.type, uc, r) if self.can(Succor) =>
            game.fbeLastRecruitOrSummon = Some((uc, r, self.summonCost(uc, r)))
            UnknownContinue

        case DimorphicGestationMainAction(self, uc, r, cost) =>
            // Stale-menu guard (same pattern as DC's DCTenebrosumMainAction):
            // if the recorded action is gone or dice dropped below cost since
            // the menu was rendered, bail to a fresh main menu.
            if (game.fbeLastRecruitOrSummon.none || game.fbeCardDice.num < cost)
                Force(MainAction(self))
            else {
                implicit val asking = Asking(self)
                + DimorphicGestationRepeatAction(self, uc, r, cost)
                + CancelAction
                asking
            }

        case DimorphicGestationRepeatAction(self, uc, r, cost) =>
            if (game.fbeCardDice.num < cost)
                Force(MainAction(self))
            else {
                def repeat(discarded : $[Int]) : Continue = {
                    self.place(uc, r)
                    self.log(Succor.styled(FBE) + ": discarded " + cost + " " + (cost == 1).?("die").|("dice") +
                        ", repeated " + uc.styled(self) + " in " + r + " for free")
                    // Clear so the SAME repeat can't be offered/clicked again; a
                    // fresh Recruit/Summon re-records it for the next repeat.
                    game.fbeLastRecruitOrSummon = None
                    Force(MainAction(self))
                }
                // Replay-safety: the manual die picker is Soft (unrecorded), so on
                // replay auto-discard the lowest `cost` dice deterministically
                // instead of re-running the interactive picker (same idiom as
                // FungalThrallDiceDoneAction / AnimatedRushMainAction above).
                if (game.nextReplayActionHint.any) {
                    val discard = game.fbeCardDice.sortBy(x => x).take(cost)
                    game.fbeCardDice = game.fbeCardDice.sortBy(x => x).drop(cost)
                    repeat(discard)
                }
                else
                    Force(ManualDiePickAction(self, DimorphicGestationContext, cost, $, selectedDice => {
                        game.fbeCardDice = game.fbeCardDice.diff(selectedDice)
                        DimorphicGestationDiceSelectedAction(self, uc, r, selectedDice)
                    }))
            }

        case DimorphicGestationDiceSelectedAction(self, uc, r, discarded) =>
            self.place(uc, r)
            self.log(Succor.styled(FBE) + ": discarded " + discarded.num + " " + (discarded.num == 1).?("die").|("dice") +
                ", repeated " + uc.styled(self) + " in " + r + " for free")
            game.fbeLastRecruitOrSummon = None
            Force(MainAction(self))

        // ── OVERLORD OF DEATH — turn-end cleanup (HB Fix 127) ────────────────
        // If the discount is still active when the turn ends without having
        // gone through EndAction (which fires afterAction → reset), forfeit it.
        // Also clears Dimorphic Gestation's recorded repeat (one-shot per turn).
        case EndTurnAction(f) if f == FBE && (game.fbeOverlordDiscount > 0 || game.fbeFeastDeaths.any || game.fbeLastRecruitOrSummon.any) =>
            if (game.fbeOverlordDiscount > 0)
                FBE.log(OverlordOfDeath.styled(FBE) + ": unused discount expired")
            game.fbeOverlordDiscount = 0
            game.fbeFeastDeaths = $
            game.fbeLastRecruitOrSummon = None
            UnknownContinue

        // ── OVERLORD OF DEATH — cost discount intercepts [LEGACY REPLAY ONLY]
        // (HB Fix 127). Retired in v16 — unreachable in fresh play since no offer
        // site can set fbeOverlordDiscount > 0 anymore; kept only so an old
        // in-progress save with a pending discount still spends it correctly.
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
