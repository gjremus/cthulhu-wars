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
//
// DEFERRED to LAYER 3 (documented, each a distinct piece, mostly Battle.scala /
// cross-faction / payment-layer surface): Sacrament of Flesh forced Doom-Phase
// removal; Unquenchable Thirst (Doom-for-Power in Baphomet's Area); Eternal
// Servitude virtual-Power; Baphomet's Fury (Torment + Transference); the active
// EFFECTS of Scavenge, Eclipse, Infernolatreia, Hellgate, Blood Offering, and
// Entropy Siphon's enemy Power-loss allocation; the Lord's-Shadow-destroyed /
// Portent-Area-destroyed Elder-Sign rewards; and the distinct Lord's Shadow glyph.
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

        case _ => UnknownContinue
    }
}
