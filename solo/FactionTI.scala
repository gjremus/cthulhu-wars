package cws

import hrf.colmat._

import html._


// ============================================================================
// The Invasion (TI) — Great Old One: Baphomet
// ----------------------------------------------------------------------------
// LAYER 1 (this file, current state): faction identity, unit roster, spellbooks,
// abilities, requirements, variable combat, custom setup, and an expansion shell
// so the faction COMPILES and APPEARS IN THE PICKER with a working action menu.
// This mirrors exactly how Colour Out of Space (FactionCS.scala) was born as a
// "Layer 1" shell before its novel mechanics were layered on.
//
// DEFERRED to later layers (each a separate deploy, per the implementation guide
// Sections 2-4). Nothing below is implemented yet; these are the novel mechanics
// that make TI distinct and each is a substantial piece:
//   * Lord's Shadow — a NEW gate-like board object with custom control/destruction
//     rules and no controlling unit (guide §1.2 / §2.2). The guide calls this "the
//     single highest-risk registration gap." For LAYER 1 it is stood in for by an
//     ordinary Controlled Gate placed at Setup (see TIExpansion setup below).
//   * Portent tokens + the Demon Larvae "Portend" action that grows them into a
//     Lord's Shadow + Fiend (guide §1.7 / §2.2), and the opponent Portent-placement
//     setup micro-phase (guide §2.5).
//   * Demon Larvae "Larvae" ongoing (count as Acolytes but cannot build/control
//     Gates). For LAYER 1 Demon Larvae control Gates normally so the Setup gate
//     yields income; the restriction lands with the real Lord's Shadow.
//   * Baphomet awaken (permanently remove one of your own units; Baphomet appears
//     there), variable combat (4 + Doom-from-Elder-Signs this Action Phase),
//     Unquenchable Thirst (pay Doom not Power to attack/capture in his Area),
//     Sacrament of Flesh (Doom-Phase forced removal), and the two-sided,
//     ownership-transferable Baphomet's Fury card (Torment + Transference).
//     For LAYER 1 Baphomet is a defined GOO but NOT yet awakenable (awakenCost None).
//   * Eternal Servitude (act as if you had 2 Power at 0 Power) — cost-substitution.
//   * The six spellbook effects (Scavenge, Eclipse, Infernolatreia, Hellgate,
//     Blood Offering, Entropy Siphon) and the six spellbook-requirement triggers.
// ============================================================================


// The Invasion (TI) UNITS (guide §1.7).
// Demon Larvae is TI's Cultist — a custom named Cultist class (precedent: TB's
// Cadavolyte). The generic recruit machinery (Game.recruits) offers every
// Cultist-class in the pool, so Demon Larvae recruit like Acolytes automatically.
case object DemonLarvae extends FactionUnitClass(TI, "Demon Larvae", Cultist, 1)
case object Gryllus     extends FactionUnitClass(TI, "Gryllus", Monster, 2)
case object Fiend       extends FactionUnitClass(TI, "Fiend", Monster, 4)
case object Baphomet    extends FactionUnitClass(TI, "Baphomet", GOO, 0)


// The Invasion (TI) ABILITIES (innate faction / GOO / unit powers; use .has()).
// Modeled as FactionSpellbook objects purely so the overlay can style/reference
// them by name (same pattern as CS's ChromaticPerversion / CorruptedRending).
// Their BEHAVIOUR is deferred to a later layer — these are names + card text only.
case object EternalServitude  extends FactionSpellbook(TI, "Eternal Servitude")   // Unique Ability (headline), §1.5
case object Portend           extends FactionSpellbook(TI, "Portend")             // Demon Larvae action, §1.7
case object Larvae            extends FactionSpellbook(TI, "Larvae")              // Demon Larvae ongoing, §1.7
case object UnquenchableThirst extends FactionSpellbook(TI, "Unquenchable Thirst") // Baphomet ongoing, §1.8
case object SacramentOfFlesh  extends FactionSpellbook(TI, "Sacrament of Flesh")  // Baphomet Doom Phase, §1.8
case object BaphometsFury     extends FactionSpellbook(TI, "Baphomet's Fury")     // two-sided card, §1.8


// The Invasion (TI) SPELLBOOKS (library, unlockable, use .can()) — guide §1.10.
// Scavenge is tied to Gryllus, Entropy Siphon to Fiend (guide's called-out
// deviation); the other four are free-choice. Effects deferred to a later layer.
case object Scavenge      extends FactionSpellbook(TI, "Scavenge")
case object Eclipse       extends FactionSpellbook(TI, "Eclipse")
case object Infernolatreia extends FactionSpellbook(TI, "Infernolatreia")
case object Hellgate      extends FactionSpellbook(TI, "Hellgate")
case object BloodOffering extends FactionSpellbook(TI, "Blood Offering")
case object EntropySiphon extends FactionSpellbook(TI, "Entropy Siphon")


// The Invasion (TI) SPELLBOOK REQUIREMENTS (guide §1.9). Not paired to a specific
// spellbook (satisfying any lets TI take any it lacks, except Scavenge/Entropy
// Siphon which also need the linked unit in play). Trigger logic deferred.
case object TIAwakenBaphomet     extends Requirement("Awaken Baphomet")
case object TISecondDoomPhase    extends Requirement("End of the second Doom Phase")
case object TIPayPowerDoom       extends Requirement("Pay 4 Power and 1 Doom as your action")
case object TICapturedTwoFactions extends Requirement("Captured Cultists from 2+ factions")
case object TICreateShadow1      extends Requirement("Create a Lord's Shadow")
case object TICreateShadow2      extends Requirement("Create another Lord's Shadow")


// The Invasion (TI) FACTION OBJECT
case object TI extends Faction { f =>
    def name = "The Invasion"
    def short = "TI"
    def style = "ti"

    // Per the guide HARD RULE, all persistent TI state (Portent counts, Lord's
    // Shadow locations, Baphomet's Fury flip/owner, the Elder-Sign-Doom-this-phase
    // counter, etc.) will live on Game.scala when those mechanics are layered in —
    // NOT on this faction object — so undo/replay works. Layer 1 adds no such state.

    override def abilities = $(EternalServitude, Portend, Larvae, UnquenchableThirst, SacramentOfFlesh, BaphometsFury)
    override def library = $(Scavenge, Eclipse, Infernolatreia, Hellgate, BloodOffering, EntropySiphon)
    override def requirements(options : $[GameOption]) = $(TIAwakenBaphomet, TISecondDoomPhase, TIPayPowerDoom, TICapturedTwoFactions, TICreateShadow1, TICreateShadow2)

    val allUnits =
        1.times(Baphomet) ++
        6.times(DemonLarvae) ++
        6.times(Gryllus) ++
        4.times(Fiend)

    // LAYER 1: Baphomet is defined but NOT yet awakenable — its real awaken
    // (permanently remove one of your own units; Baphomet appears in that Area,
    // no Power cost) lands with the Baphomet layer. Returning None means the
    // awaken menu never offers him, so no AwakenedAction terminal is needed yet.
    override def awakenCost(u : UnitClass, r : Region)(implicit game : Game) : |[Int] = None

    // Combat (recomputed at read time):
    //   Gryllus = 1, Fiend = 3, Demon Larvae = 0.
    //   Baphomet = 4 + Doom earned from Elder Signs this Action Phase (guide §1.8).
    //   LAYER 1: the per-Action-Phase Elder-Sign-Doom counter is not tracked yet,
    //   so Baphomet reads a flat 4. The running counter lands with the Baphomet layer.
    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int = {
        units(Gryllus).num * 1 +
        units(Fiend).num * 3 +
        units(Baphomet).not(Zeroed).num * 4 +
        neutralStrength(units, opponent)
    }
}


// The Invasion (TI) EXPANSION — action dispatch. LAYER 1: custom setup + the
// standard active-player action menu only. TI-specific actions (Portend, the six
// spellbooks, Baphomet awaken, etc.) are appended here in later layers.
object TIExpansion extends Expansion {

    def perform(action : Action, soft : VoidGuard)(implicit game : Game) : Continue = action @@ {

        // -- SETUP (guide §1.6 / §2.5) ----------------------------------------
        // TI is not a printed-map faction, so (like Xyrious Storm) it must
        // intercept SetupFactionsAction to place itself rather than relying on the
        // generic board.starting(f) seating loop. Place a Lord's Shadow (LAYER 1
        // stand-in: an ordinary Controlled Gate), one Fiend and six Demon Larvae in
        // one unoccupied Area, and start with 8 Power (guide §1.6).
        //   DEFERRED: the true "after Windwalker, before Opener of the Way" seating
        //   slot, the Opener-of-the-Way map restriction, and the opponent
        //   Portent-placement micro-phase (guide §2.5) all land with the Lord's
        //   Shadow / Portent layer.
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
            // LAYER 1 Lord's Shadow stand-in: an ordinary Controlled Gate so area
            // control and Doom-Phase income work through the existing gate machinery.
            game.gates :+= r
            self.gates :+= r
            self.place(Fiend, r)
            1.to(6).foreach(_ => self.place(DemonLarvae, r))
            // Put one Demon Larva on the gate so it is Controlled and yields income
            // (LAYER 1 only — the real Lord's Shadow is TI-controlled without a unit).
            self.at(r).one(DemonLarvae).onGate = true
            self.power = 8
            self.log("starts in", r, "with a", "Lord's Shadow".styled(TI), "(a Controlled Gate for now), a", Fiend.styled(TI), "and 6", DemonLarvae.styled(TI))
            Force(SetupFactionsAction)

        // -- MAIN ACTION -------------------------------------------------------
        // The engine has NO generic active-player action menu; each faction's
        // expansion assembles its own (see FactionCS / FactionXSS). Defer the
        // passive/acted cases to Game's own handlers (UnknownContinue) and build the
        // standard action menu on TI's own turn. Without this, MainAction(TI)
        // matches nothing and the game crashes on TI's first turn.
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

            // LAYER 1: no TI-specific spellbook/ability actions yet — they are
            // appended here (Portend, Scavenge, Hellgate, Blood Offering, Awaken
            // Baphomet, ...) as each mechanic is layered in.

            game.neutralSpellbooks(f)
            game.libraryActions(f)
            game.highPriests(f)
            game.reveals(f)

            game.endTurn(f)(f.battled.any || game.nexed.any)

            asking

        case _ => UnknownContinue
    }
}
