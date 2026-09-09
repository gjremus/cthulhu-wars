package cws

import hrf.colmat._

import html._


// ============================================================================
// The Invasion (TI) — Great Old One: Baphomet
// ----------------------------------------------------------------------------
// LAYER 2 (this file): the playable core built from the Implementation Guide,
// Sections 1-4. Every persistent piece of TI state lives on Game.scala (undo
// HARD RULE) and every shared-engine hook is guarded by `factions.has(TI)`, so
// games without The Invasion are provably unaffected.
//
// IMPLEMENTED here + in Game.scala:
//   * Lord's Shadow (§1.2) — a gate-like object with no Controlling Unit, TI-
//     controlled by default, giving ordinary Doom-Phase / Gather-Power income
//     (via TI.gates membership + a checkGatesLost exemption + the
//     tiShadowController control-recompute in triggers()). A Controlled Gate
//     sharing the Area takes the Shadow over as an additional gate.
//   * Demon Larvae "Larvae" ongoing (§1.7) — Larvae are a plain Cultist class,
//     so they already count as Acolytes everywhere yet cannot Build/Control
//     Gates (base UnitClass.canControlGate is false; only Acolyte/HighPriest
//     override it to true). No toggle, exactly as the guide specifies.
//   * Portents + Portend (§1.7/§2.2) — per-Area token counts on Game.scala; the
//     Portend action grows them and the 4th converts to a Lord's Shadow + Fiend.
//   * Opponent Portent-placement setup micro-phase (§2.5/§4.1).
//   * Baphomet awaken by permanent unit-removal (§1.8/§3.4.1) and variable combat
//     = 4 + Doom earned from Elder Signs this Action Phase (§1.8, counter on Game).
//   * All six Spellbook Requirements (§1.9) as triggers()/action satisfies, and
//     the Scavenge/Entropy-Siphon acquisition gating (Game.scala CheckSpellbooks).
//   * Entropy Siphon's "Fiends can Control Gates" flag (§1.10).
//   * Blood Offering (§1.10/§3.10.5) — draw+reveal 4 ES, per-enemy (turn-order, no
//     timer — creator ruling 2026-09-08 "Option A") offer-a-Cultist micro-phase,
//     TI accepts at most one offer and Captures for free; face-down until Gather Power.
//
// DEFERRED to LAYER 3 (documented, each a distinct piece, mostly Battle.scala /
// cross-faction / payment-layer surface): Sacrament of Flesh forced Doom-Phase
// removal; Unquenchable Thirst (Doom-for-Power in Baphomet's Area); Eternal
// Servitude virtual-Power; Baphomet's Fury (Torment + Transference); the active
// EFFECTS of Scavenge, Eclipse, Infernolatreia, and Hellgate; Entropy Siphon's
// enemy Power-loss allocation; the Lord's-Shadow-destroyed / Portent-Area-destroyed
// Elder-Sign rewards; and the distinct Lord's Shadow glyph.
// ============================================================================


// The Invasion (TI) UNITS (guide §1.7).
// Demon Larvae is TI's Cultist. As a plain Cultist-class FactionUnitClass it
// inherits UnitClass.canControlGate == false (only Acolyte/HighPriest override to
// true), which IS the "Larvae cannot Build or Control Gates but count as Acolytes
// for all other purposes" ongoing (§1.7) — no extra flag or transform needed.
case object DemonLarvae extends FactionUnitClass(TI, "Demon Larvae", Cultist, 1)
case object Gryllus     extends FactionUnitClass(TI, "Gryllus", Monster, 2)
case object Fiend       extends FactionUnitClass(TI, "Fiend", Monster, 4) {
    // Entropy Siphon (§1.10 / §3.10.6): while TI holds this Spellbook, Fiends can
    // Control Gates — the Monster-side mirror of Demon Larvae's restriction,
    // implemented as a canControlGate override checked at gate-control-eligibility
    // time (the same site the whole engine already consults). Guarded so it only
    // ever applies in a TI game that has actually earned Entropy Siphon.
    override def canControlGate(u : UnitFigure)(implicit game : Game) : Boolean =
        game.factions.has(TI) && TI.has(EntropySiphon)
}
case object Baphomet    extends FactionUnitClass(TI, "Baphomet", GOO, 0)


// The Invasion (TI) ABILITIES (innate faction / GOO / unit powers; use .has()).
// Modeled as FactionSpellbook objects so the overlay can style/reference them by
// name (same pattern as CS). Behaviour (where implemented) lives in TIExpansion
// and the Game.scala hooks; the ones still deferred are noted in the header above.
case object EternalServitude   extends FactionSpellbook(TI, "Eternal Servitude")   // Unique Ability (headline), §1.5
case object Portend            extends FactionSpellbook(TI, "Portend")             // Demon Larvae action, §1.7
case object Larvae             extends FactionSpellbook(TI, "Larvae")              // Demon Larvae ongoing, §1.7
case object UnquenchableThirst extends FactionSpellbook(TI, "Unquenchable Thirst") // Baphomet ongoing, §1.8
case object SacramentOfFlesh   extends FactionSpellbook(TI, "Sacrament of Flesh")  // Baphomet Doom Phase, §1.8
case object BaphometsFury      extends FactionSpellbook(TI, "Baphomet's Fury")     // two-sided card, §1.8


// The Invasion (TI) SPELLBOOKS (library, unlockable, use .can()) — guide §1.10.
// Scavenge is tied to Gryllus, Entropy Siphon to Fiend (the called-out deviation);
// the other four are free-choice. Acquisition gating lives in Game.CheckSpellbooks.
case object Scavenge       extends FactionSpellbook(TI, "Scavenge")
case object Eclipse        extends FactionSpellbook(TI, "Eclipse")
case object Infernolatreia extends FactionSpellbook(TI, "Infernolatreia")
case object Hellgate       extends FactionSpellbook(TI, "Hellgate")
case object BloodOffering  extends FactionSpellbook(TI, "Blood Offering")
case object EntropySiphon  extends FactionSpellbook(TI, "Entropy Siphon")


// The Invasion (TI) SPELLBOOK REQUIREMENTS (guide §1.9). Not paired to a specific
// spellbook (satisfying any lets TI take any it lacks, except Scavenge/Entropy
// Siphon which also need the linked unit in play — enforced in Game.CheckSpellbooks).
case object TIAwakenBaphomet      extends Requirement("Awaken Baphomet")
case object TISecondDoomPhase     extends Requirement("End of the second Doom Phase")
case object TIPayPowerDoom        extends Requirement("Pay 4 Power and 1 Doom as your action")
case object TICapturedTwoFactions extends Requirement("Captured Cultists from 2+ factions")
case object TICreateShadow1       extends Requirement("Create a Lord's Shadow")
case object TICreateShadow2       extends Requirement("Create another Lord's Shadow")


// The Invasion (TI) FACTION OBJECT
case object TI extends Faction { f =>
    def name = "The Invasion"
    def short = "TI"
    def style = "ti"

    override def abilities = $(EternalServitude, Portend, Larvae, UnquenchableThirst, SacramentOfFlesh, BaphometsFury)
    override def library = $(Scavenge, Eclipse, Infernolatreia, Hellgate, BloodOffering, EntropySiphon)
    override def requirements(options : $[GameOption]) = $(TIAwakenBaphomet, TISecondDoomPhase, TIPayPowerDoom, TICapturedTwoFactions, TICreateShadow1, TICreateShadow2)

    val allUnits =
        1.times(Baphomet) ++
        6.times(DemonLarvae) ++
        6.times(Gryllus) ++
        4.times(Fiend)

    // Baphomet awakens via a custom Soft action (permanently remove one of your own
    // units; Baphomet appears in that Area — see TIAwakenBaphomet* below), NOT via the
    // generic pay-Power awaken machinery, so awakenCost stays None (the stock awaken
    // menu never offers him).
    override def awakenCost(u : UnitClass, r : Region)(implicit game : Game) : |[Int] = None

    // Combat (recomputed at read time — Battle.scala reads strength() live, §2.12):
    //   Gryllus = 1, Fiend = 3, Demon Larvae = 0.
    //   Baphomet = 4 + Doom earned from Elder Signs this Action Phase (§1.8). The
    //   running counter lives on Game (reset each Action Phase, incremented on TI's
    //   Elder-Sign→Doom conversions), so it is never cached at awaken/phase start.
    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int = {
        units(Gryllus).num * 1 +
        units(Fiend).num * 3 +
        units(Baphomet).not(Zeroed).num * (4 + game.tiElderSignDoomThisActionPhase) +
        neutralStrength(units, opponent)
    }
}


// ============================================================================
// The Invasion (TI) ACTION CLASSES
// ============================================================================

// Setup: opponent Portent-placement micro-phase (§2.5/§4.1).
case class TIPortentSetupAction(queue : $[Faction], placed : Int) extends ForcedAction
case class TIPortentSetupPlaceAction(who : Faction, r : Region, queue : $[Faction], placed : Int) extends ForcedAction

// Portend (Demon Larvae action, §1.7).
case class TIPortendMainAction(l : $[Region]) extends OptionFactionAction("Portend — grow a Portent (" + Portend.name + ")") with MainQuestion with Soft { override def self = TI }
case class TIPortendAction(r : Region) extends ForcedAction

// Awaken Baphomet (§1.8/§3.4.1): pick an Area, then which owned unit to permanently remove.
case class TIAwakenBaphometMainAction(l : $[Region]) extends OptionFactionAction("Awaken " + Baphomet.styled(TI) + " (permanently remove one of your units)") with MainQuestion with Soft { override def self = TI }
case class TIAwakenBaphometRegionAction(r : Region) extends ForcedAction
case class TIAwakenBaphometUnitAction(r : Region, uc : UnitClass) extends BaseFactionAction(
    implicit g => "Awaken " + Baphomet.styled(TI) + " in " + r + ", permanently removing a " + uc.styled(TI),
    implicit g => uc.styled(TI) + " in " + r) { override def self = TI }

// Spellbook Requirement 3 (§1.9): pay 4 Power and 1 Doom as your Action.
case class TIPayPowerDoomMainAction() extends OptionFactionAction("Pay 4 Power and 1 Doom (Spellbook Requirement)") with MainQuestion with Soft { override def self = TI }

// Sacrament of Flesh (Baphomet Doom Phase, §1.8): a MANDATORY Doom-Phase step —
// permanently remove `toRemove` Faction Units you Control (1, or 2 with all 6
// Spellbooks, each reduced by a spared Cultist), then gain `esGain` Elder Signs.
// TISacramentResolveAction is the driver (recurses removal→removal→ES→continue);
// TISacramentRemoveUnitAction is the per-unit pick (recorded — it mutates state).
case class TISacramentResolveAction(toRemove : Int, esGain : Int) extends ForcedAction
case class TISacramentRemoveUnitAction(r : Region, uc : UnitClass, toRemove : Int, esGain : Int)
    extends OptionFactionAction(implicit g => "Permanently remove a " + uc.styled(TI) + " in " + r) with DoomQuestion { override def self = TI }

// Hellgate (library Spellbook, Action cost 2, §1.10/§2.7): relocate any number of TI
// units from ONE Area with a Lord's Shadow to ANOTHER Area with a Lord's Shadow. No
// unit-type restriction (Baphomet himself is eligible). Modeled on the proven
// Undimensioned multi-unit "move-any-number-then-Done" idiom: the source/destination
// picks and the loop driver are Soft navigation; only TIHellgateMoveAction and the Done
// mutate state (and are recorded). The 2 Power is paid once, on the first actual move.
case class TIHellgateMainAction(sources : $[Region]) extends OptionFactionAction("Hellgate — relocate units between Lord's Shadows (2 Power)") with MainQuestion with Soft { override def self = TI }
case class TIHellgateSourceAction(src : Region) extends BaseFactionAction(g => "Hellgate from " + src, implicit g => "" + src) with Soft { override def self = TI }
case class TIHellgateDestAction(src : Region, dst : Region) extends BaseFactionAction(g => "Hellgate " + src + " to " + dst, implicit g => "" + dst) with Soft { override def self = TI }
case class TIHellgateLoopAction(src : Region, dst : Region) extends ForcedAction with Soft
case class TIHellgateMoveAction(src : Region, dst : Region, uc : UnitClass) extends BaseFactionAction(
    g => "Hellgate move " + uc + " from " + src + " to " + dst,
    implicit g => uc.styled(TI)) { override def self = TI }
case class TIHellgateDoneAction() extends BaseFactionAction(None, "Done") { override def self = TI }

// Eclipse (library Spellbook, Only Once, §1.10/§2.7): play at any time before the Doom
// Phase; a FREE play (no "Action" cost on the card — it does not consume TI's turn), so
// after arming it we re-present the main menu. It arms game.tiEclipseArmed for the next
// Doom Phase and is marked used-forever (TI.oncePerGame) so it can never be replayed.
case class TIEclipseMainAction() extends OptionFactionAction("Eclipse — deny enemies gate Doom next Doom Phase (one-shot)") with MainQuestion { override def self = TI }

// Scavenge (Gryllus-linked library Spellbook, Action cost 2, §1.10/§3.10.1): a two-step
// Action.  STEP 1 — move any number of TI Gryllusses for FREE (each moves one adjacent Area,
// standard movement; the 2 Power is the whole Action cost, not a per-move charge).  STEP 2 —
// for each map Area that now holds a TI Gryllus, TI may resolve ONE free Battle OR ONE 1-Power
// Capture — never both, and never more than one per Area.  "Only your Gryllusses participate on
// your side of any Battles" is enforced by the effect==Scavenge exemption in Battle.scala
// (mirrors Grasping Dead).  Modeled on Hellgate's proven move-any-number-then-Done idiom.
//
// The 2 Power and TI.acted are committed the instant TI selects Scavenge (like Eclipse, a
// MainQuestion WITHOUT Soft records the action), so no paid-flag needs threading.  Chosen
// Battles are enqueued directly into game.queue (FREE — bypassing AttackAction's 1-Power
// charge) and proceeded once at Done; Captures resolve immediately and loop back.  The Soft
// loop drivers re-derive on undo replay; only the mutating leaves are recorded.  The
// one-Battle-or-Capture-per-Area rule is guarded by game.tiScavengeResolvedAreas (a queued
// Battle leaves its target present, so "still has a target" alone can't mark an Area done).
case class TIScavengeMainAction() extends OptionFactionAction("Scavenge — move Gryllusses, then Battle or Capture in their Areas (2 Power)") with MainQuestion { override def self = TI }
case class TIScavengeMoveLoopAction() extends ForcedAction with Soft
case class TIScavengeMoveSelectAction(from : Region) extends BaseFactionAction(
    g => "Scavenge move Gryllus from " + from, implicit g => Gryllus.styled(TI) + " in " + from) with Soft { override def self = TI }
case class TIScavengeMoveAction(from : Region, to : Region) extends BaseFactionAction(
    g => "Scavenge move Gryllus from " + from + " to " + to, implicit g => "" + to) { override def self = TI }
case class TIScavengeMoveDoneAction() extends BaseFactionAction(None, "Done moving — proceed to Battle/Capture") { override def self = TI }
case class TIScavengeAreaLoopAction() extends ForcedAction with Soft
case class TIScavengeBattleAction(r : Region, f : Faction) extends BaseFactionAction(
    g => "Scavenge Battle " + f + " in " + r, implicit g => "Battle " + f.full + " in " + r) { override def self = TI }
case class TIScavengeCaptureAction(r : Region, f : Faction) extends BaseFactionAction(
    g => "Scavenge Capture in " + r, implicit g => "Capture in " + r + " (1 Power)") with Soft { override def self = TI }
case class TIScavengeCaptureTargetAction(r : Region, f : Faction, ur : UnitRef) extends BaseFactionAction(
    g => "Scavenge capture " + ur + " in " + r, implicit g => "" + ur.full) { override def self = TI }
case class TIScavengeDoneAction() extends BaseFactionAction(None, "Done") { override def self = TI }

// Blood Offering (library Spellbook, Action cost 0, §1.10/§3.10.5): draw and reveal 4
// Elder Signs, then give each enemy — in turn order, no timer (creator ruling
// 2026-09-08, "Option A," replacing the card's real-time 2-minute window for this
// synchronous engine) — one chance to offer a single eligible Cultist from play for
// the Doom of one of the 4 revealed Elder Signs. TI then accepts AT MOST ONE offer
// (or none — "may accept," never forced) and Captures that Cultist for free (no Power
// cost; the "payment" is the enemy's Cultist and the enemy's Doom gain). All Elder
// Signs not used that way (the other 3, or all 4 if TI declines every offer) simply
// return to the pool — they were never added to any faction's es/revealed track, so
// no further bookkeeping is needed for them. Flips face-down (TI.oncePerGame) until
// the next Gather Power Phase, mirroring the existing Nuclear Chaos face-down/face-up
// idiom (see the ActionPhaseAction case in Game.scala), except reset at Gather Power
// instead of at the start of the Action Phase — see TIExpansion.triggers() below.
//
// "Eligible Cultist" (the guide leaves this undefined beyond "a Cultist from play"):
// mirrors Scavenge's own capture-target filter (canBeCaptured, excluding
// MindParasiteCultist) — but unlike Scavenge/the stock CaptureAction, does NOT apply
// the GateDiplomacy "clings" restriction or the GOO-blocks-capture Area rule, since
// here the enemy is voluntarily offering one of ITS OWN units (a self-sacrifice), not
// being forcibly captured out of a protected Area — those protections don't apply.
// Modeled on Entropy Siphon's own per-enemy allocation micro-phase immediately below
// (same "queue in game.factions.but(TI) turn order" idiom).
case class TIBloodOfferingMainAction() extends OptionFactionAction("Blood Offering — draw 4 Elder Signs, let enemies offer a Cultist (0 Power)") with MainQuestion { override def self = TI }
case class TIBloodOfferingDrawAction(drawn : $[ElderSign], remaining : Int) extends ForcedAction
case class TIBloodOfferingOfferLoopAction(queue : $[Faction], drawn : $[ElderSign], offers : $[(Faction, UnitRef, Int)]) extends ForcedAction
case class TIBloodOfferingOfferAction(who : Faction, ur : UnitRef, i : Int, queue : $[Faction], drawn : $[ElderSign], offers : $[(Faction, UnitRef, Int)]) extends BaseFactionAction(
    g => "Blood Offering: " + who + " offers " + ur + " for " + drawn(i).short,
    implicit g => "Offer " + ur.full + " for " + drawn(i).short) { override def self = who }
case class TIBloodOfferingDeclineAction(who : Faction, queue : $[Faction], drawn : $[ElderSign], offers : $[(Faction, UnitRef, Int)]) extends BaseFactionAction(None, "Decline") { override def self = who }
case class TIBloodOfferingResolveAction(drawn : $[ElderSign], offers : $[(Faction, UnitRef, Int)]) extends ForcedAction
case class TIBloodOfferingAcceptAction(who : Faction, ur : UnitRef, i : Int, drawn : $[ElderSign]) extends BaseFactionAction(
    g => "Blood Offering: accepted " + who + "'s offer of " + ur,
    implicit g => "Accept " + ur.full + " from " + who.full + " for " + drawn(i).short) { override def self = TI }
case class TIBloodOfferingDeclineAllAction(offers : $[(Faction, UnitRef, Int)]) extends BaseFactionAction(None, "Decline all offers") { override def self = TI }

// Entropy Siphon (Fiend-linked library Spellbook, Ongoing, §1.10/§3.10.6). Part 1
// (Fiends can Control Gates) lives on Fiend.canControlGate above. Part 2 is this
// end-of-Doom-Phase penalty: enemies collectively decide how to lose a combined
// 4 Power per Fiend-Controlled Gate; every full 4 Power short of that total costs
// EVERY player with more Doom than TI 1 Doom. Built as a per-enemy allocation
// micro-phase mirroring the opponent Portent-placement micro-phase (§4.1): each
// enemy, in turn order, is asked (Hard) how much Power to contribute toward the
// running remaining total — 0 is always an allowed choice (declining to pay pushes
// the shortfall onto the Doom penalty, which only bites players ahead of TI, so the
// "collective decision" tension is preserved). Whoever controls that enemy answers,
// exactly as the already-shipped Portent micro-phase relies on the same Ask dispatch
// (there is no human/bot predicate in this engine to branch on, so this guide-literal
// menu is the consistent choice). A "Fiend-Controlled Gate" is a TI-controlled Gate
// whose controller is a Fiend — a TI.gates Area that is NOT a Lord's Shadow Area
// (those are Shadow-controlled, tiLordsShadowRegions) and holds a live TI Fiend.
// All transient state is carried in the action parameters (queue / remaining / then),
// so undo replay reconstructs it deterministically. then = the end-of-Doom-Phase
// continuation into the Action Phase. NOTE (owner review): the guide's exact
// "Fiend-Controlled Gate" scope and menu-vs-deterministic allocation were resolved
// here with the most guide-faithful reading available under autonomous operation.
case class TIEntropySiphonStartAction(then : ForcedAction) extends ForcedAction
case class TIEntropySiphonAllocAction(queue : $[Faction], remaining : Int, then : ForcedAction) extends ForcedAction
case class TIEntropySiphonPayAction(who : Faction, amount : Int, queue : $[Faction], remaining : Int, then : ForcedAction)
    extends BaseFactionAction(
        g => "Entropy Siphon: " + who + " loses " + amount + " Power",
        implicit g => "Lose " + amount.power) { override def self = who }


// ============================================================================
// The Invasion (TI) EXPANSION — action dispatch, triggers, setup, main menu.
// ============================================================================
object TIExpansion extends Expansion {

    // Portend cost (§1.7): number of Portents already in the Area + 1, OR 0 if TI's
    // Larvae in the Area already meet-or-exceed that same value.
    def tiPortendCost(r : Region)(implicit game : Game) : Int = {
        val p = game.tiPortents.getOrElse(r, 0)
        val larvae = TI.at(r).%(_.uclass == DemonLarvae).not(Zeroed).num
        if (larvae >= p + 1) 0 else p + 1
    }

    // Deterministic recompute after every action (guide §2.2 control-recompute hook +
    // §1.9 Spellbook Requirements). Runs for every expansion after each action; guarded
    // to a TI game and side-effect-free except for the reconciliation/satisfy calls,
    // all of which are idempotent — safe under undo/replay.
    override def triggers()(implicit game : Game) : Unit = {
        if (game.setup.has(TI).not) return

        // Sacrament of Flesh (§1.8): snapshot the Cultists sitting on TI's Faction
        // Sheet (its prison) during Gather Power — each spares one Unit from the
        // forced Doom-Phase removal. gatherPowerPhase is true only inside the single
        // Gather-Power triggers() call, so this captures the Gather-Power value and is
        // never overwritten by the mid-action recomputes that also run triggers().
        if (game.gatherPowerPhase) {
            game.tiSacramentSpared = game.factions./~(e => e.at(TI.prison)).%(_.uclass.utype == Cultist).num

            // Blood Offering (§1.10/§3.10.5): flip back face-up at the Gather Power
            // Phase — the card's "flip this face down until the Gather Power Phase"
            // text, mirroring Nuclear Chaos's identical face-down/face-up idiom (which
            // resets at ActionPhaseAction instead — see Game.scala).
            TI.oncePerGame = TI.oncePerGame.but(BloodOffering)
        }

        // Lord's Shadow control reconciliation (§1.2 rule 3). A Shadow is TI-controlled
        // by default (kept in TI.gates with no unit); if a Controlled Gate shares its
        // Area, that gate's controller takes the Shadow over as an ADDITIONAL gate, so
        // TI drops it from TI.gates (its extra income is added via tiShadowAdditionalGates).
        game.tiLordsShadowRegions.foreach { r =>
            game.tiShadowController(r) match {
                case Some(TI) => if (TI.gates.has(r).not) TI.gates :+= r
                case Some(_)  => if (TI.gates.has(r))      TI.gates :-= r
                case None     =>
            }
        }

        // Spellbook Requirements (§1.9 / §3.12). satisfy is permanent + idempotent.
        // SBR1: Baphomet in play (also satisfied inline in the awaken handler).
        TI.satisfyIf(TIAwakenBaphomet, "Awaken Baphomet", TI.onMap(Baphomet).not(Zeroed).any)
        // SBR2: at (the second) Doom Phase — all other players also lose 1 Doom.
        if (TI.needs(TISecondDoomPhase) && game.tiDoomPhaseCount >= 2) {
            TI.satisfy(TISecondDoomPhase, "End of the second Doom Phase")
            TI.enemies.foreach { e =>
                if (e.doom > 0) {
                    e.doom = 0.max(e.doom - 1)
                    e.log("lost", 1.doom, "to", "The Invasion".styled(TI))
                }
            }
        }
        // SBR4: captured Cultists on TI's Faction Sheet (its prison) from 2+ factions.
        TI.satisfyIf(TICapturedTwoFactions, "Captured Cultists from 2+ factions",
            game.factions./~(e => e.at(TI.prison)).%(_.uclass.utype == Cultist)./(_.faction).distinct.num >= 2)
        // SBR5/6: Lord's Shadows CREATED via Portend (the Setup one does not count).
        TI.satisfyIf(TICreateShadow1, "Create a Lord's Shadow",       game.tiLordsShadowCreated >= 1)
        TI.satisfyIf(TICreateShadow2, "Create another Lord's Shadow", game.tiLordsShadowCreated >= 2)
    }

    // Baphomet's Fury — Transference resolves "at the end of the current Action" (§2.12).
    // afterAction() is exactly that seam. If a battle this Action queued a transfer, move
    // ownership now; TI gains 1 Doom whenever ownership actually changes hands. Guarded to
    // a TI game and self-clearing, so it is a no-op after it fires (and under replay it
    // re-derives from the same queued state).
    override def afterAction()(implicit game : Game) : Unit = {
        if (game.setup.has(TI).not) return

        game.tiFuryTransferTo.foreach { newOwner =>
            val old = game.tiFuryOwnerF
            if (newOwner != old) {
                game.tiFuryOwner = (newOwner == TI).?(None : |[Faction]).|(|(newOwner))
                TI.doom += 1
                TI.log(BaphometsFury.styled(newOwner) + " passes to", newOwner.full, "(it killed the holder's unit) —", TI.full, "gains", 1.doom)
            }
        }
        game.tiFuryTransferTo = None
    }

    def perform(action : Action, soft : VoidGuard)(implicit game : Game) : Continue = action @@ {

        // ================================================================
        // SETUP (guide §1.6 / §2.5)
        // ================================================================
        // (1) TI self-placement — like Xyrious Storm, TI is not a printed-map faction,
        // so it intercepts SetupFactionsAction to place itself. TIExpansion is dispatched
        // before Game's own handler, so this always wins while TI is unplaced.
        case SetupFactionsAction if game.setup.has(TI) && !game.starting.contains(TI) =>
            val eligible = game.board.regions.%(r =>
                game.factions.forall(e => e.at(r).none) &&
                game.starting.values.$.has(r).not)
            if (eligible.num == 1)
                Force(StartingRegionAction(TI, eligible.head))
            else
                Ask(TI).each(eligible)(r => StartingRegionAction(TI, r).as(r)(TI, "starts in")).cancel

        case StartingRegionAction(self : TI.type, r) =>
            game.starting = game.starting + (TI -> r)
            // Real Lord's Shadow: registered as a Shadow object AND kept in TI.gates for
            // default TI control with NO Controlling Unit (§1.2). Income + area control
            // then flow through the normal gate machinery; checkGatesLost exempts it from
            // the unit-abandon rule, and triggers() hands it to a gate-controller if one
            // ever shares the Area. This Shadow does NOT count toward Requirement 5/6.
            game.tiLordsShadowRegions :+= r
            self.gates :+= r
            self.place(Fiend, r)
            1.to(6).foreach(_ => self.place(DemonLarvae, r))
            self.power = 8
            self.log("starts in", r, "with a", "Lord's Shadow".styled(TI) + ",", "a", Fiend.styled(TI), "and 6", DemonLarvae.styled(TI), "and 8 Power")
            Force(SetupFactionsAction)

        // (2) Opponent Portent-placement micro-phase — fires once TI is placed and every
        // faction EXCEPT Opener of the Way is seated, and before OW sets up (§2.5). Then
        // hands back to the base loop, which seats OW last.
        case SetupFactionsAction
            if game.setup.has(TI) && game.starting.contains(TI) && !game.tiPortentSetupDone
            && game.setup.but(TI).but(OW).forall(game.starting.contains) =>
            Force(TIPortentSetupAction(game.setup.but(TI), 0))

        case TIPortentSetupAction(queue, placed) =>
            if (queue.none || placed >= 5) {
                game.tiPortentSetupDone = true
                Force(SetupFactionsAction)
            }
            else {
                val who     = queue.head
                val start   = game.starting(TI)
                val adjacent = game.board.connected(start)
                // Any Area with no Gate and no Portent, not TI's Start and not adjacent to it.
                val valid = game.board.regions.%(r =>
                    game.allGates.has(r).not && game.tiPortents.contains(r).not &&
                    r != start && adjacent.has(r).not)
                if (valid.none)
                    Force(TIPortentSetupAction(queue.tail, placed))   // auto-skip: no valid Area
                else
                    Ask(who).each(valid)(r => TIPortentSetupPlaceAction(who, r, queue, placed).as(r)(who, "places a Portent in"))
            }

        case TIPortentSetupPlaceAction(who, r, queue, placed) =>
            game.tiPortents = game.tiPortents + (r -> 1)
            who.log("placed a", "Portent".styled(TI), "in", r)
            Force(TIPortentSetupAction(queue.tail, placed + 1))

        // ================================================================
        // MAIN ACTION MENU (TI's own turn)
        // ================================================================
        case MainAction(f : TI.type) if f.active.not =>
            UnknownContinue

        case MainAction(f : TI.type) if f.acted =>
            UnknownContinue

        // Eternal Servitude (§1.5/§3.6): a broke TI with Baphomet in play, while any other
        // faction is still active, is handed a REAL 2 Power for this one turn so it can keep
        // participating. Reclaimed to 0 the moment the turn ends (tiReclaimEternalServitude),
        // so it never becomes income and never reaches the Doom Phase (where it would wrongly
        // fund a Ritual of Annihilation, which Eternal Servitude forbids). The granted flag
        // makes this fire exactly once per turn; afterwards the normal menu below runs with
        // the 2 Power in hand.
        case MainAction(f : TI.type) if f.acted.not && game.tiEternalServitudeApplies(f) && game.tiEternalServitudeGranted.not =>
            game.tiEternalServitudeGranted = true
            f.power += 2
            f.log(EternalServitude.styled(TI) + " — 0 Power with", Baphomet.styled(TI), "in play; participates with", 2.power, "this turn")
            Force(MainAction(TI))

        case MainAction(f : TI.type) =>
            implicit val asking = Asking(f)

            game.moves(f)
            game.captures(f)
            game.recruits(f)
            game.battles(f)
            game.controls(f)
            game.builds(f)
            game.summons(f)
            game.awakens(f)
            game.independents(f)

            // Portend (§1.7): an Area containing a TI Demon Larva AND at least 1 Portent,
            // that TI can pay the auto-computed cost for.
            val portendAreas = game.board.regions.%(r =>
                game.tiPortents.getOrElse(r, 0) >= 1 &&
                f.at(r).%(_.uclass == DemonLarvae).not(Zeroed).any &&
                f.power >= tiPortendCost(r))
            if (portendAreas.any)
                + TIPortendMainAction(portendAreas)

            // Awaken Baphomet (§1.8/§3.4.1): whenever TI controls at least one non-Baphomet
            // unit on the map and Baphomet is not yet in play.
            if (f.pool(Baphomet).not(Zeroed).any && f.onMap(Baphomet).not(Zeroed).none) {
                val rs = f.allInPlay.%(u => u.uclass != Baphomet && u.region.onMap).not(Zeroed)./(_.region).distinct
                if (rs.any)
                    + TIAwakenBaphometMainAction(rs)
            }

            // Spellbook Requirement 3 (§1.9): pay 4 Power and 1 Doom as your Action.
            if (f.needs(TIPayPowerDoom) && f.power >= 4 && f.doom >= 1)
                + TIPayPowerDoomMainAction()

            // Hellgate (§1.10): needs the earned spellbook, 2 Power, at least two Lord's
            // Shadow Areas, and a source Shadow Area actually holding a TI unit to move.
            if (f.can(Hellgate) && f.power >= 2 && game.tiLordsShadowRegions.num >= 2) {
                val sources = game.tiLordsShadowRegions.%(r => f.at(r).not(Zeroed).any)
                if (sources.any)
                    + TIHellgateMainAction(sources)
            }

            // Eclipse (§1.10): one-shot, playable any time before the Doom Phase (offered
            // during TI's action phase; it is free and does not consume the turn).
            if (f.can(Eclipse) && game.doomPhase.not && game.tiEclipseArmed.not)
                + TIEclipseMainAction()

            // Scavenge (§1.10/§3.10.1): needs the earned spellbook, 2 Power, and at least one
            // Gryllus on the map to move/act with.
            if (f.can(Scavenge) && f.power >= 2 && f.onMap(Gryllus).not(Zeroed).any)
                + TIScavengeMainAction()

            // Blood Offering (§1.10/§3.10.5): free (0 Power) Action — draw & reveal 4
            // Elder Signs, then let each enemy (turn order) optionally offer one
            // eligible Cultist for one of them; TI may accept at most one offer (or
            // none) and Captures that Cultist for free. Flips face-down (TI.oncePerGame)
            // until the next Gather Power Phase (see triggers() above).
            if (f.can(BloodOffering))
                + TIBloodOfferingMainAction()

            game.neutralSpellbooks(f)
            game.libraryActions(f)
            game.highPriests(f)
            game.reveals(f)

            game.endTurn(f)(f.battled.any || game.nexed.any)

            asking

        // ================================================================
        // PORTEND (§1.7 / §2.2)
        // ================================================================
        case TIPortendMainAction(l) =>
            Ask(TI).each(l)(r => TIPortendAction(r).as(r)(TI, "Portend in")).cancel

        case TIPortendAction(r) =>
            val cost = tiPortendCost(r)
            if (TI.power < cost)
                EndAction(TI)
            else {
                TI.power -= cost
                if (cost > 0) TI.log("paid", cost.power, "for", Portend.styled(TI), "in", r)
                val now = game.tiPortents.getOrElse(r, 0) + 1
                if (now >= 4) {
                    // 4th Portent: remove them all, create a Lord's Shadow + a Fiend (§1.7).
                    game.tiPortents = game.tiPortents - r
                    game.tiLordsShadowRegions :+= r
                    TI.gates :+= r
                    game.tiLordsShadowCreated += 1
                    if (TI.pool(Fiend).not(Zeroed).any)
                        TI.place(Fiend, r)
                    TI.log(Portend.styled(TI) + ":", "4 Portents in", r, "became a", "Lord's Shadow".styled(TI), "and a", Fiend.styled(TI))
                    // Requirements 5/6 are satisfied by triggers() off tiLordsShadowCreated.
                }
                else {
                    game.tiPortents = game.tiPortents + (r -> now)
                    TI.log(Portend.styled(TI) + ": added a Portent in", r, "(now " + now + ")")
                }
                EndAction(TI)
            }

        // ================================================================
        // AWAKEN BAPHOMET (§1.8 / §3.4.1)
        // ================================================================
        case TIAwakenBaphometMainAction(l) =>
            Ask(TI).each(l)(r => TIAwakenBaphometRegionAction(r).as(r)(TI, "Awaken Baphomet in")).cancel

        case TIAwakenBaphometRegionAction(r) =>
            val ucs = TI.at(r).%(u => u.uclass != Baphomet).not(Zeroed)./(_.uclass).distinct
            if (ucs.none)
                EndAction(TI)
            else
                Ask(TI).each(ucs)(uc => TIAwakenBaphometUnitAction(r, uc)).cancel

        case TIAwakenBaphometUnitAction(r, uc) =>
            val victim = TI.at(r).%(_.uclass == uc).not(Zeroed).headOption
            if (TI.pool(Baphomet).not(Zeroed).none || victim.isEmpty)
                EndAction(TI)
            else {
                val u = victim.get
                // Permanent removal from the game (NOT returned to pool) — the same
                // primitive Quachil's Dust to Dust uses (Battle.scala): drop the figure.
                TI.units = TI.units.%(_.ref != u.ref)
                TI.log("permanently removed a", uc.styled(TI), "in", r, "to awaken", Baphomet.styled(TI))
                TI.place(Baphomet, r)
                TI.log("awakened", Baphomet.styled(TI), "in", r)
                TI.satisfy(TIAwakenBaphomet, "Awaken Baphomet")
                AwakenedAction(TI, Baphomet, r, 0)
            }

        // Terminal for the custom awaken (a handler is mandatory or the dispatcher
        // crashes with "unknown continue"). Baphomet has no further on-awaken effect.
        case AwakenedAction(self : TI.type, Baphomet, _, _) =>
            EndAction(self)

        // ================================================================
        // SPELLBOOK REQUIREMENT 3 (§1.9)
        // ================================================================
        case TIPayPowerDoomMainAction() =>
            if (TI.power >= 4 && TI.doom >= 1 && TI.needs(TIPayPowerDoom)) {
                TI.power -= 4
                TI.doom -= 1
                TI.log("paid", 4.power, "and", 1.doom, "as an action")
                TI.satisfy(TIPayPowerDoom, "Pay 4 Power and 1 Doom as your Action")
            }
            EndAction(TI)

        // ================================================================
        // SACRAMENT OF FLESH (Baphomet Doom Phase, §1.8 / §2.4)
        // ================================================================
        // Mandatory at TI's Doom Phase while Baphomet is in play: permanently remove
        // 1 Unit (2 with all 6 Spellbooks), reduced by 1 per Cultist that sat on TI's
        // Faction Sheet during Gather Power (tiSacramentSpared), then gain 1 Elder Sign
        // (2 with all 6 Spellbooks). We intercept DoomAction(TI) ONCE (guarded by the
        // per-phase tiSacramentDoneThisDoom flag), resolve Sacrament, then Force
        // DoomAction(TI) again — the guard is now false, so TIExpansion returns
        // UnknownContinue and Game.scala's normal DoomAction(f) flow (rituals / reveals /
        // high priests / hires / doomDone) runs. Elder Signs route through takeES so the
        // 36-pool overflow-to-Doom (and future Infernolatreia) applies.
        case DoomAction(f : TI.type) if !game.tiSacramentDoneThisDoom && f.onMap(Baphomet).not(Zeroed).any =>
            game.tiSacramentDoneThisDoom = true
            val base     = if (f.hasAllSB) 2 else 1
            val toRemove = 0.max(base - game.tiSacramentSpared)
            val esGain   = if (f.hasAllSB) 2 else 1
            f.log(SacramentOfFlesh.styled(TI) + ": must remove", toRemove.toString.styled("kill"),
                (toRemove == 1).?("Unit").|("Units"),
                (game.tiSacramentSpared > 0).??("(spared " + game.tiSacramentSpared + " for captured Cultists) "),
                "and gain", esGain.es)
            Force(TISacramentResolveAction(toRemove, esGain))

        case TISacramentResolveAction(toRemove, esGain) =>
            val units = TI.allInPlay.%(_.region.onMapOrMoon).not(Zeroed)
            if (toRemove <= 0 || units.none) {
                if (esGain > 0) {
                    TI.takeES(esGain)
                    TI.log(SacramentOfFlesh.styled(TI) + ": gained", esGain.es)
                }
                Force(DoomAction(TI))
            }
            else {
                implicit val asking = Asking(TI)
                units./(u => (u.region, u.uclass)).distinct.foreach { case (r, uc) =>
                    + TISacramentRemoveUnitAction(r, uc, toRemove, esGain)
                }
                asking   // MANDATORY — no cancel
            }

        case TISacramentRemoveUnitAction(r, uc, toRemove, esGain) =>
            val victim = TI.at(r).%(_.uclass == uc).not(Zeroed).headOption
            victim.foreach { u =>
                // Permanent removal from the game (NOT returned to pool) — same primitive
                // as the Awaken-Baphomet sacrifice and Quachil's Dust to Dust.
                TI.units = TI.units.%(_.ref != u.ref)
                TI.log(SacramentOfFlesh.styled(TI) + ": permanently removed a", uc.styled(TI), "in", r)
            }
            Force(TISacramentResolveAction(toRemove - 1, esGain))

        // ================================================================
        // HELLGATE (§1.10 / §2.7)
        // ================================================================
        case TIHellgateMainAction(sources) =>
            Ask(TI).each(sources)(r => TIHellgateSourceAction(r)).cancel

        case TIHellgateSourceAction(src) =>
            // Destination is any OTHER Lord's Shadow Area.
            val dests = game.tiLordsShadowRegions.but(src)
            if (dests.none)
                EndAction(TI)
            else
                Ask(TI).each(dests)(d => TIHellgateDestAction(src, d)).cancel

        case TIHellgateDestAction(src, dst) =>
            Force(TIHellgateLoopAction(src, dst))

        // Loop driver: offer each still-unmoved TI unit type in the source Area (plus a
        // Done once at least one move has happened). Mirrors Undimensioned's Moved-tag
        // bookkeeping, so the first move pays the 2 Power and Done clears the tags.
        case TIHellgateLoopAction(src, dst) =>
            val movable = TI.at(src).not(Zeroed).not(Moved)./(_.uclass).distinct
            val didMove = TI.units.tag(Moved).any
            if (movable.none)
                Then(TIHellgateDoneAction())
            else if (didMove)
                Ask(TI).add(TIHellgateDoneAction()).each(movable)(uc => TIHellgateMoveAction(src, dst, uc))
            else
                Ask(TI).each(movable)(uc => TIHellgateMoveAction(src, dst, uc)).cancel

        case TIHellgateMoveAction(src, dst, uc) =>
            if (TI.units.tag(Moved).none) {
                TI.power -= 2
                TI.log("units", Hellgate.styled(TI))
            }
            val u = TI.at(src, uc).not(Moved).first
            u.region = dst
            u.add(Moved)
            TI.log(uc.styled(TI), "relocated from", src, "to", dst, "via", Hellgate.styled(TI))
            Force(TIHellgateLoopAction(src, dst))

        case TIHellgateDoneAction() =>
            TI.units.foreach(_.remove(Moved))
            EndAction(TI)

        // ================================================================
        // ECLIPSE (§1.10 / §2.7)
        // ================================================================
        case TIEclipseMainAction() =>
            game.tiEclipseArmed = true
            TI.oncePerGame :+= Eclipse   // used-forever: can(Eclipse) is now false
            TI.log("plays", Eclipse.styled(TI), "— enemies gain no Doom from Unit-Controlled Gates next Doom Phase")
            // Free play (no Action cost): return to the menu so TI can still act this turn.
            Force(MainAction(TI))

        // ================================================================
        // SCAVENGE (§1.10 / §3.10.1)
        // ================================================================
        // Commit the Action immediately (non-Soft MainQuestion, like Eclipse): pay the flat
        // 2 Power, mark TI as having acted (battles only auto-set acted when TI lacks all
        // Spellbooks, so set it explicitly here for the capture-only / no-battle case), and
        // clear the per-Action resolved-Areas guard. Then enter the free Gryllus move loop.
        case TIScavengeMainAction() =>
            TI.power -= 2
            TI.acted = true
            game.tiScavengeResolvedAreas = $
            TI.log("uses", Scavenge.styled(TI) + " — paid", 2.power)
            Force(TIScavengeMoveLoopAction())

        // STEP 1 — free Gryllus move loop (Hellgate-style Soft driver). Offer, per Area still
        // holding an unmoved Gryllus, a "move a Gryllus from here" pick, plus a Done that
        // always ends the move step (zero moves is allowed — the Action is already committed).
        case TIScavengeMoveLoopAction() =>
            val moveAreas = TI.onMap(Gryllus).not(Zeroed).not(Moved)./(_.region).distinct
            if (moveAreas.none)
                Then(TIScavengeMoveDoneAction())
            else
                Ask(TI)
                    .add(TIScavengeMoveDoneAction())
                    .each(moveAreas)(r => TIScavengeMoveSelectAction(r))

        case TIScavengeMoveSelectAction(from) =>
            val dests = game.board.connected(from)
            if (dests.none)
                Force(TIScavengeMoveLoopAction())
            else
                Ask(TI).each(dests)(to => TIScavengeMoveAction(from, to)).cancel

        case TIScavengeMoveAction(from, to) =>
            val u = TI.at(from, Gryllus).not(Zeroed).not(Moved).first
            u.region = to
            u.add(Moved)
            u.onGate = false
            TI.log(Scavenge.styled(TI) + ": moved a", Gryllus.styled(TI), "from", from, "to", to)
            Force(TIScavengeMoveLoopAction())

        // Transition to STEP 2: clear the Moved tags used to drive the move loop (they play no
        // part in the Battle/Capture step and must not leak past this Action).
        case TIScavengeMoveDoneAction() =>
            TI.units.%(_.uclass == Gryllus).foreach(_.remove(Moved))
            Force(TIScavengeAreaLoopAction())

        // STEP 2 — per-Area Battle/Capture loop (Soft driver). For each Gryllus Area not yet
        // resolved, offer a free Battle against each attackable enemy and (if TI has >= 1 Power)
        // a 1-Power Capture against each capturable enemy; plus a Done. When no Area offers any
        // legal Battle or Capture, finish automatically.
        case TIScavengeAreaLoopAction() =>
            val remaining = TI.onMap(Gryllus).not(Zeroed)./(_.region).distinct.diff(game.tiScavengeResolvedAreas)
            val ee = game.factionlike.but(TI)
            val battleVariants = remaining./~(r => ee.%(_.present(r)).%(TI.canAttack(r))./(e => (r, e)))
            val captureVariants = (TI.power >= 1).?(remaining./~(r => ee.%(_.present(r)).%(TI.canCapture(r))./(e => (r, e)))).|($)
            if (battleVariants.none && captureVariants.none)
                Then(TIScavengeDoneAction())
            else
                Ask(TI)
                    .each(battleVariants)((r, e) => TIScavengeBattleAction(r, e))
                    .each(captureVariants)((r, e) => TIScavengeCaptureAction(r, e))
                    .add(TIScavengeDoneAction())

        // Free Battle: enqueue it directly (no AttackAction 1-Power charge). The
        // effect==Scavenge exemption in Battle.scala restricts TI's side to Gryllusses only.
        // Battles accumulate in the queue and all proceed at Done (mirrors the Nyogtha
        // From Below multi-battle chain).
        case TIScavengeBattleAction(r, f) =>
            game.tiScavengeResolvedAreas :+= r
            game.queue = game.queue :+ new Battle(r, TI, f, |(Scavenge))
            TI.log("battled", f, "in", r, "with", Scavenge.styled(TI))
            Force(TIScavengeAreaLoopAction())

        // 1-Power Capture: Soft navigation to the target pick (the mutating leaf is
        // TIScavengeCaptureTargetAction). Mirrors the stock CaptureAction target filtering
        // (canBeCaptured, clings), but excludes the exotic Mind-Parasite-Cultist case to avoid
        // the unparasitize sub-flow — a Scavenge Capture of a parasitized acolyte is not offered.
        case TIScavengeCaptureAction(r, f) =>
            val l = f.at(r).cultists.%(u => u.uclass.canBeCaptured(u)).%(_.uclass != MindParasiteCultist).sortBy(u => u.uclass.cost * 10 + u.onGate.??(5))
            if (l.none)
                Force(TIScavengeAreaLoopAction())
            else {
                val ll = f.clings.?(l.take(1)).|(l)
                Ask(TI).each(ll)(u => TIScavengeCaptureTargetAction(r, f, u.ref)).cancel
            }

        case TIScavengeCaptureTargetAction(r, f, ur) =>
            TI.power -= 1
            game.tiScavengeResolvedAreas :+= r
            val victim = game.unit(ur)
            game.eliminate(victim)
            victim.region = TI.prison
            TI.log("captured", victim, "in", r, "with", Scavenge.styled(TI))
            TI.satisfy(CaptureCultist, "Capture Cultist")
            if (game.factions.has(FB))
                game.fbCyclopeanGazeActionRegions :+= r
            // Requirement 4 (Captured Cultists from 2+ factions) is state-derived in triggers().
            Force(TIScavengeAreaLoopAction())

        // Done: run any enqueued free Battles (they drain sequentially), else just end the
        // Action. TI.acted was already set at commit, so the turn ends after the battles.
        case TIScavengeDoneAction() =>
            game.tiScavengeResolvedAreas = $
            if (game.queue.any)
                ProceedBattlesAction
            else
                EndAction(TI)

        // ================================================================
        // BLOOD OFFERING (§1.10 / §3.10.5)
        // ================================================================
        // Commit the Action immediately (flat 0 Power, like Eclipse/Scavenge — a
        // MainQuestion WITHOUT Soft records the action) and flip the card face-down
        // (TI.oncePerGame) before entering the 4-Elder-Sign draw loop.
        case TIBloodOfferingMainAction() =>
            TI.acted = true
            TI.oncePerGame :+= BloodOffering
            TI.log("plays", BloodOffering.styled(TI) + " — draws 4", "Elder Signs".styled("es"))
            Force(TIBloodOfferingDrawAction($, 4))

        // Draw loop: one real Elder-Sign draw at a time via the same DrawES physical-
        // companion seam every other ES gain in the engine uses (Game.CheckSpellbooksAction),
        // weighted by what is not already accounted for — every faction's held/revealed
        // ES, PLUS this card's own already-drawn-but-not-yet-returned tokens (so the 4
        // draws within a single Blood Offering never double-count each other). x == 0
        // means the shared 36-token pool is exhausted — stop early with whatever was
        // drawn so far rather than loop forever or crash on drawn(i).
        case TIBloodOfferingDrawAction(drawn, remaining) if remaining <= 0 =>
            TI.log(BloodOffering.styled(TI), "revealed", drawn./(_.short).mkString(" "))
            Force(TIBloodOfferingOfferLoopAction(game.factions.but(TI), drawn, $))

        case TIBloodOfferingDrawAction(drawn, remaining) =>
            val known = game.factions./~(f => f.es ++ f.revealed) ++ drawn
            DrawES("Blood Offering draws an Elder Sign",
                18 - known.count(_.value == 1), 12 - known.count(_.value == 2), 6 - known.count(_.value == 3),
                (x, _) => if (x == 0) TIBloodOfferingDrawAction(drawn, 0) else TIBloodOfferingDrawAction(drawn :+ ElderSign(x), remaining - 1))

        // Per-enemy offer loop (mirrors Entropy Siphon's own per-enemy allocation
        // micro-phase immediately below): each enemy in turn order (game.factions.but(TI))
        // may offer ONE of its eligible Cultists for ONE of the 4 revealed Elder Signs, or
        // Decline; enemies with no eligible Cultist (or if none were drawn) are auto-
        // skipped. Creator ruling (2026-09-08, "Option A"): synchronous, no timer, turn
        // order — replacing the card's real-time 2-minute window for this engine.
        case TIBloodOfferingOfferLoopAction(queue, drawn, offers) =>
            if (queue.none)
                Force(TIBloodOfferingResolveAction(drawn, offers))
            else {
                val who = queue.head
                val eligible = who.cultists.%(u => u.uclass.canBeCaptured(u)).%(_.uclass != MindParasiteCultist)
                if (eligible.none || drawn.none)
                    Force(TIBloodOfferingOfferLoopAction(queue.tail, drawn, offers))
                else {
                    val variants = eligible.sortBy(u => u.uclass.cost * 10 + u.onGate.??(5))./~(u => drawn.indices./(i => (u.ref, i)))
                    Ask(who)
                        .each(variants)((ur, i) => TIBloodOfferingOfferAction(who, ur, i, queue.tail, drawn, offers))
                        .add(TIBloodOfferingDeclineAction(who, queue.tail, drawn, offers))
                }
            }

        case TIBloodOfferingOfferAction(who, ur, i, queue, drawn, offers) =>
            who.log("offers", game.unit(ur), "for", drawn(i).short, "—", BloodOffering.styled(TI))
            Force(TIBloodOfferingOfferLoopAction(queue, drawn, offers :+ (who, ur, i)))

        case TIBloodOfferingDeclineAction(who, queue, drawn, offers) =>
            Force(TIBloodOfferingOfferLoopAction(queue, drawn, offers))

        // TI's final choice: accept AT MOST ONE offer (never forced — "may accept," per
        // the card), Capturing that Cultist for free and awarding the matching Elder
        // Sign's Doom to the offering enemy. Every Elder Sign not used this way (the
        // other 3, or all 4 if TI declines) simply returns to the pool — they were never
        // added to any faction's es/revealed track, so no further bookkeeping is needed.
        case TIBloodOfferingResolveAction(drawn, offers) =>
            if (offers.none) {
                TI.log(BloodOffering.styled(TI) + ": no enemy offered a Cultist — all Elder Signs return to the pool")
                EndAction(TI)
            }
            else
                Ask(TI).each(offers)(o => TIBloodOfferingAcceptAction(o._1, o._2, o._3, drawn)).add(TIBloodOfferingDeclineAllAction(offers))

        case TIBloodOfferingAcceptAction(who, ur, i, drawn) =>
            val es = drawn(i)
            who.doom += es.value
            who.log("gains", es.value.doom, "—", BloodOffering.styled(TI), "(offer accepted)")
            val victim = game.unit(ur)
            val r = victim.region
            game.eliminate(victim)
            victim.region = TI.prison
            TI.log("captures", victim, "in", r, "via", BloodOffering.styled(TI) + " — remaining Elder Signs return to the pool")
            TI.satisfy(CaptureCultist, "Capture Cultist")
            if (game.factions.has(FB))
                game.fbCyclopeanGazeActionRegions :+= r
            EndAction(TI)

        case TIBloodOfferingDeclineAllAction(offers) =>
            TI.log(BloodOffering.styled(TI) + ": declined all offers — Elder Signs return to the pool")
            EndAction(TI)

        // Entropy Siphon Part 2 (§1.10/§3.10.6): the end-of-Doom-Phase enemy Power-loss
        // penalty. See the action-class declarations above for the full design rationale.
        case TIEntropySiphonStartAction(then) =>
            // Fiend-Controlled Gates: TI.gates Areas that are NOT Lord's Shadow Areas
            // (those are Shadow-controlled) and hold a live TI Fiend. Guarded so this
            // only ever fires in a TI game that actually holds Entropy Siphon.
            val fiendGates : $[Region] =
                if (game.factions.has(TI).not || TI.has(EntropySiphon).not) $
                else TI.gates.diff(game.tiLordsShadowRegions).%(r => TI.at(r, Fiend).not(Zeroed).any)
            val total = fiendGates.num * 4
            if (total <= 0)
                Force(then)
            else {
                TI.log(EntropySiphon.styled(TI) + ": enemies must collectively lose", total.power,
                    "for", fiendGates.num.toString.styled(TI), "Fiend-Controlled " + (fiendGates.num == 1).?("Gate").|("Gates"))
                Force(TIEntropySiphonAllocAction(game.factions.but(TI), total, then))
            }

        case TIEntropySiphonAllocAction(queue, remaining, then) =>
            if (queue.none || remaining <= 0) {
                // Whatever the enemies failed to lose converts to Doom loss: 1 Doom per
                // full 4 Power short, charged to every player with more Doom than TI
                // (clamped so it never drops a player below 0 Doom).
                val doomLoss = remaining / 4
                if (doomLoss > 0)
                    game.factions.but(TI).%(_.doom > TI.doom).foreach { e =>
                        val d = doomLoss.min(e.doom)
                        e.doom -= d
                        e.log("lost", d.doom, "—", EntropySiphon.styled(TI), "(enemies left", remaining.power, "unpaid)")
                    }
                Force(then)
            }
            else {
                val who = queue.head
                val cap = who.power.min(remaining)
                if (cap <= 0)
                    Force(TIEntropySiphonAllocAction(queue.tail, remaining, then))   // auto-skip: no Power to give
                else
                    Ask(who).each((0 to cap).toList)(n => TIEntropySiphonPayAction(who, n, queue, remaining, then))
            }

        case TIEntropySiphonPayAction(who, amount, queue, remaining, then) =>
            if (amount > 0) {
                who.power -= amount
                who.log("lost", amount.power, "toward", EntropySiphon.styled(TI))
            }
            Force(TIEntropySiphonAllocAction(queue.tail, remaining - amount, then))

        case _ => UnknownContinue
    }
}
