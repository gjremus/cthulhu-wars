package cws

import hrf.colmat._

import html._


// ============================================================================
// Colour Out of Space (CS) — Great Old One: Tulzscha
// ----------------------------------------------------------------------------
// LAYER 1 (this file, current state): faction identity, unit roster, spellbooks,
// requirements, variable combat, and a stub awaken/expansion so the faction
// compiles and appears in the picker. Novel mechanics (Chromatic Perversion
// Prismatic Wells, Tulzscha's sacrifice-and-roll awaken, Corrupted Rending,
// and the six spellbook effects) are layered in on top of this shell.
//
// NOTE: the engine already has an INDEPENDENT-GOO `Tulzscha` (IGOOs.scala:47).
// The CS faction GOO must be a DISTINCT object so it never shares state with
// the iGOO — hence `CSTulzscha` here, with display name still "Tulzscha".
// ============================================================================


// Colour Out of Space (CS) UNITS
// Meteorite (Monster, cost 1, combat 0), Effervescent Excrescence (Monster,
// cost 2, combat 2 -> 6 with Vermiculite Hypertrophy), Luminous Globule
// (Terror, cost 4, does not fight), Tulzscha (GOO, awaken 5 Power + sacrifice).
case object Meteorite extends FactionUnitClass(CS, "Meteorite", Monster, 1)
case object EffervescentExcrescence extends FactionUnitClass(CS, "Effervescent Excrescence", Monster, 2) {
    // SB2 Vermiculite Hypertrophy (Task 3.10.2): while CS controls that spellbook, Excrescences
    // become immune to Move — they can neither be chosen for a Move action nor retreat out of a
    // battle (so a battle Pain result against them can only be absorbed as a kill). Checked at
    // move-validation time against the live spellbook flag, never a base-stat edit.
    override def canMove(u : UnitFigure)(implicit game : Game) : Boolean = CS.can(VermiculiteHypertrophy).not
    override def canBeMoved(u : UnitFigure)(implicit game : Game) : Boolean = CS.can(VermiculiteHypertrophy).not
}
case object LuminousGlobule extends FactionUnitClass(CS, "Luminous Globule", Terror, 4) {
    // SB6 Core Exposure (Task 3.10.6): once CS controls that spellbook, Globules "cannot be moved by
    // any other means" than for free alongside a Cultist. This override enforces the RESTRICTION —
    // it removes Globules from the normal (paid) Move menu once Core Exposure is active. The
    // complementary free-carry-with-a-Cultist path is a separate additive move hook (deferred: it
    // must not preempt the shared MovedAction handlers CG/Hound rely on). Read-time check, never a
    // base-stat edit. Before Core Exposure is active, Globules move normally.
    override def canMove(u : UnitFigure)(implicit game : Game) : Boolean = CS.can(CoreExposure).not
    override def canBeMoved(u : UnitFigure)(implicit game : Game) : Boolean = CS.can(CoreExposure).not
}
case object CSTulzscha extends FactionUnitClass(CS, "Tulzscha", GOO, 5)
// A Gate corrupted into a Prismatic Well is rendered as a Token in place of the normal Gate
// (display-only; the well is not a placeable pool unit). Lives at top level (moved out of
// CthulhuWarsSolo's render-token block) so the faction overlay card can list it too.
case object PrismaticWell extends FactionUnitClass(CS, "Prismatic Well", Token, 0)


// Colour Out of Space (CS) ABILITIES (always-on faction powers, use .has())
// Chromatic Perversion — Meteorites or Globules convert gates in their region to Prismatic Wells.
// Corrupted Rending — Tulzscha's GOO ability: force a battle between two other factions.
case object ChromaticPerversion extends FactionSpellbook(CS, "Chromatic Perversion")
case object CorruptedRending extends FactionSpellbook(CS, "Corrupted Rending")


// Colour Out of Space (CS) SPELLBOOKS (library, unlockable, use .can())
case object Insanity extends FactionSpellbook(CS, "Insanity")
case object VermiculiteHypertrophy extends FactionSpellbook(CS, "Vermiculite Hypertrophy")
case object CosmicLandfall extends FactionSpellbook(CS, "Cosmic Landfall")
case object SpectralCollapse extends FactionSpellbook(CS, "Spectral Collapse")
case object EffulgentSacrifice extends FactionSpellbook(CS, "Effulgent Sacrifice")
case object CoreExposure extends FactionSpellbook(CS, "Core Exposure")


// Colour Out of Space (CS) SPELLBOOK REQUIREMENTS (Section 1.9 — NOT paired to a
// specific spellbook; satisfying any one lets CS pick any spellbook it lacks).
case object CSMeteoriteInEnemyStart extends Requirement("Meteorite in an enemy start area")
case object CSPrismaticWellExists extends Requirement("A Prismatic Well exists")
case object CSGlobuleEliminated extends Requirement("A Globule is eliminated")
case object CSSacrificeGlobule extends Requirement("Sacrifice a Globule as an action")
case object CSEnemyControlsExcrescence extends Requirement("Another player controls an Excrescence")
case object CSAwakenTulzscha extends Requirement("Awaken Tulzscha")


// Colour Out of Space (CS) FACTION OBJECT
case object CS extends Faction { f =>
    def name = "Colour Out of Space"
    def short = "CS"
    def style = "cs"

    // CHROMATIC PERVERSION state (regions whose Gate has been corrupted into a
    // Prismatic Well) lives on Game.scala as `game.csPrismaticWellRegions`, NOT on
    // this faction object — see the guide's HARD RULE "Faction State MUST Live On
    // Game.scala For Undo To Work". A Game-instance var auto-resets on `new Game()`
    // (undo replay), same pattern as `game.fbCraters`.

    override def abilities = $(ChromaticPerversion, CorruptedRending)
    override def library = $(Insanity, VermiculiteHypertrophy, CosmicLandfall, SpectralCollapse, EffulgentSacrifice, CoreExposure)
    override def requirements(options : $[GameOption]) = $(CSMeteoriteInEnemyStart, CSPrismaticWellExists, CSGlobuleEliminated, CSSacrificeGlobule, CSEnemyControlsExcrescence, CSAwakenTulzscha)

    val allUnits =
        1.times(CSTulzscha) ++
        6.times(Meteorite) ++
        8.times(EffervescentExcrescence) ++
        6.times(LuminousGlobule) ++
        6.times(Acolyte)

    // Tulzscha awakens for a fixed 5 Power (always, not variable) in a region that holds one
    // of CS's Acolytes AND a Luminous Globule — the sacrifice + kill-roll are injected by
    // CSExpansion's custom AwakenAction handler. awakenCost only gates WHERE it's offered; it
    // must therefore mirror that same Acolyte-and-Globule condition so the menu only shows
    // valid regions.
    override def awakenCost(u : UnitClass, r : Region)(implicit game : Game) : |[Int] = u match {
        case CSTulzscha =>
            (f.at(r).%(_.uclass == Acolyte).not(Zeroed).any &&
             f.at(r).%(_.uclass == LuminousGlobule).not(Zeroed).any &&
             f.pool(CSTulzscha).any).?(5)
        case _ => None
    }

    // Combat (recomputed at read time, like DC's Fallen Prophet):
    //   Tulzscha            = 2 x Luminous Globules anywhere on the board
    //   Eff. Excrescence    = 2 combat (6 while Vermiculite Hypertrophy is in play)
    //   Meteorite / Globule / Acolyte = 0
    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int = {
        val globulesOnBoard  = f.onMap(LuminousGlobule).not(Zeroed).num
        val excrescenceCombat = f.can(VermiculiteHypertrophy).?(6).|(2)
        units(EffervescentExcrescence).not(Zeroed).num * excrescenceCombat +
        units(CSTulzscha).not(Zeroed).num * globulesOnBoard * 2 +
        neutralStrength(units, opponent)
    }
}


// Colour Out of Space (CS) — Chromatic Perversion cross-faction Well summon (Task 3.8.3).
// A Prismatic Well may ONLY be used to summon Effervescent Excrescences, drawn from CS's
// pool, by whichever faction controls that Well's Gate (any faction, including CS itself),
// paying CS's Excrescence cost (2 Power). Physically the figure stays a CS UnitFigure — the
// engine has no true ownership-transfer, so control by the Well holder is a read-time derived
// concept (exactly as Mind Control leaves a controlled unit's own faction unchanged). SBR5 and
// the Layer-5 battle-side selection read that derived ownership; nothing rewrites the figure's
// faction. Two-step like the stock Summon menu: a Soft MainAction picks the well region, the
// Hard leaf does the placement.
case class CSWellSummonMainAction(self : Faction, l : $[Region]) extends OptionFactionAction("Summon " + EffervescentExcrescence.styled(CS) + " from a " + "Prismatic Well".styled(CS)) with MainQuestion with Soft
case class CSWellSummonAction(self : Faction, r : Region) extends BaseFactionAction(implicit g => "Summon " + EffervescentExcrescence.styled(CS) + g.forNPowerWithTax(r, self, EffervescentExcrescence.cost) + " in", implicit g => r + self.iced(r))

// Colour Out of Space (CS) — Tulzscha awaken kill-roll resolution (Task 3.4.1). One die is rolled
// via a RollD6 continuation (like the FBE Byagoona awaken die): in auto-dice mode the app rolls a
// genuine random 1-6; in manual-dice mode the player enters the actual die FACE (1-6), NOT a
// pre-labelled Kill/Pain/Miss outcome — so there is no "pick Kill" shortcut that lets the awaken
// always eliminate the globule. The pip is mapped to a BattleRoll (6 = Kill, 5|4 = Pain, else Miss,
// matching BattleRoll.roll) and stored here as a single-element list so the recorded action shape is
// unchanged (older games recorded `CSTulzschaAwakenRollAction(r, [Kill])` — replay stays valid). On a
// Kill it eliminates the region's Globule and takes an Elder Sign, then places Tulzscha regardless of
// the result. MUST be registered in isRollAction (every build) so undo cannot rewind past the roll.
case class CSTulzschaAwakenRollAction(r : Region, rolls : $[BattleRoll]) extends ForcedAction


// Colour Out of Space (CS) LAYER-4 SPELLBOOK ACTIONS. Each is a standard two-step menu entry
// (Soft MainAction picks the region, Hard leaf resolves it), offered from CS's own MainAction
// menu only while its spellbook is active / requirement unmet and a qualifying region exists.

// SB2 Vermiculite Hypertrophy (Task 3.10.2): a Cultist sharing a region with a Globule may summon
// an Excrescence there WITHOUT a Gate, sacrificing that Cultist and paying the 2-Power Excrescence cost.
case class CSVermiculiteMainAction(l : $[Region]) extends OptionFactionAction("Summon " + EffervescentExcrescence.styled(CS) + " at a " + Acolyte.styled(CS) + " (" + VermiculiteHypertrophy.styled(CS) + ")") with MainQuestion with Soft { override def self = CS }
case class CSVermiculiteAction(r : Region) extends BaseFactionAction(implicit g => "Summon " + EffervescentExcrescence.styled(CS) + g.forNPowerWithTax(r, CS, EffervescentExcrescence.cost) + ", sacrificing a " + Acolyte.styled(CS) + " in", implicit g => r + CS.iced(r)) { override def self = CS }

// SB5 Effulgent Sacrifice (Task 3.10.5): sacrifice a controlled Globule for an Elder Sign (Cost 1);
// any Cultists or Excrescences in that region are eliminated, each owner refunded half cost (round
// down). Additionally, every faction (including CS itself) with units remaining in the region must
// retreat them all out (a plain forced retreat, not a battle pain — see CSEffulgentRetreat* below).
case class CSEffulgentMainAction(l : $[Region]) extends OptionFactionAction("Sacrifice " + LuminousGlobule.styled(CS) + " for an " + "Elder Sign".styled("es") + " (" + EffulgentSacrifice.styled(CS) + ")") with MainQuestion with Soft { override def self = CS }
case class CSEffulgentAction(r : Region) extends BaseFactionAction(implicit g => "Sacrifice " + LuminousGlobule.styled(CS) + g.forNPowerWithTax(r, CS, 1) + " in", implicit g => r + CS.iced(r)) { override def self = CS }

// SB5 Effulgent Sacrifice retreat sweep (addendum): after the sacrifice and cultist/excrescence
// cleanup resolve, every faction (including CS itself, game.setup) with units still in the region
// must retreat all of them out. If 2+ factions are present, CS chooses which faction goes next
// (skipped when only one remains); the chosen faction then picks its own units to retreat one at
// a time, in its own chosen order, looping until it has none left in the region before moving on
// to the next faction. Modelled on Howl's per-unit retreat (Battle.scala) and Corrupted Rending's
// faction-ordering (below) — this is a plain retreat (no kill/pain assignment).
case class CSEffulgentRetreatFactionsAction(r : Region, remaining : $[Faction]) extends ForcedAction
case class CSEffulgentRetreatPickFactionAction(r : Region, a : Faction, remaining : $[Faction]) extends ForcedAction
case class CSEffulgentRetreatUnitAction(f : Faction, r : Region, remaining : $[Faction]) extends ForcedAction
case class CSEffulgentRetreatUnitPickAction(f : Faction, u : UnitRef, r : Region, remaining : $[Faction]) extends ForcedAction
case class CSEffulgentRetreatMoveAction(f : Faction, u : UnitRef, to : Region, r : Region, remaining : $[Faction]) extends ForcedAction

// SB5 Effulgent Sacrifice — owner respec (0-cost cross-faction action). Once CS has earned the
// Effulgent Sacrifice spellbook, ANY faction may take this on its own turn (offered from
// Game.neutralSpellbooks), in a region holding a Prismatic Well controlled by a faction OTHER than
// CS where the acting faction has at least one Cultist. Taking it eliminates ALL the acting
// faction's Cultists in that region and the Well's Globule; the acting faction gains 1 Doom, and
// CS gains an Elder Sign (so if CS itself acts, CS gains both). Turn-consuming, costs no power.
case class CSEffulgentSacrificeAction(self : Faction, r : Region) extends OptionFactionAction(EffulgentSacrifice.styled(CS)) with MainQuestion with PowerNeutral

// SB6 Core Exposure (Task 3.10.6): summon a Globule (no Gate) in a region with a Cultist and a
// Meteorite; the Meteorite is eliminated. Cost 1 Power.
case class CSCoreExposureMainAction(l : $[Region]) extends OptionFactionAction("Summon " + LuminousGlobule.styled(CS) + " at a " + Meteorite.styled(CS) + " (" + CoreExposure.styled(CS) + ")") with MainQuestion with Soft { override def self = CS }
case class CSCoreExposureAction(r : Region) extends BaseFactionAction(implicit g => "Summon " + LuminousGlobule.styled(CS) + g.forNPowerWithTax(r, CS, 1) + ", consuming a " + Meteorite.styled(CS) + " in", implicit g => r + CS.iced(r)) { override def self = CS }

// SB6 Core Exposure free-carry (Task 3.10.6): once Core Exposure is active a Globule may ONLY move
// for free alongside a moving Cultist (paid moves are blocked by LuminousGlobule.canMove/canBeMoved).
// When CS moves one of its Cultists, offer to carry any Globule(s) sharing the origin region along to
// the destination. Modelled on Shantak's carry-cultist (NeutralMonsters.scala).
case class CSCarryGlobuleOfferAction(o : Region, to : Region) extends ForcedAction
case class CSCarryGlobuleAction(self : Faction, o : Region, ur : UnitRef, to : Region) extends ForcedAction

// SBR4 standalone sacrifice action (Task 3.12.2): sacrifice a Globule as an action; if an enemy
// controls a Well in its region, they gain 2 Power. Offered while the requirement is unmet.
case class CSSacrificeGlobuleMainAction(l : $[Region]) extends OptionFactionAction("Sacrifice " + LuminousGlobule.styled(CS) + " as an action") with MainQuestion with Soft { override def self = CS }
case class CSSacrificeGlobuleAction(r : Region) extends BaseFactionAction(implicit g => "Sacrifice " + LuminousGlobule.styled(CS) + " in", implicit g => r + CS.iced(r)) { override def self = CS }

// SB4 Spectral Collapse (Task 3.10.4) — post-battle sequential offer chain. `self` is the faction
// currently being asked; `queue` is the factions still to offer after a decline (CS→attacker→defender).
// Use/Skip are PostBattleQuestion so they render in the post-battle context; the terminal actions
// resume the paused battle via Battle.scala's proceed(). The roll goes through RollBattle so the
// dice UI + undo guard handle it (CSSpectralCollapseRollAction must be whitelisted in isRollAction).
case class CSSpectralCollapseUseAction(self : Faction, r : Region, queue : $[Faction]) extends OptionFactionAction(("Use " + SpectralCollapse.name).styled(CS)) with PostBattleQuestion
case class CSSpectralCollapseSkipAction(self : Faction, r : Region, queue : $[Faction]) extends OptionFactionAction(("Skip " + SpectralCollapse.name).styled(CS)) with PostBattleQuestion
case class CSSpectralCollapseRollAction(self : Faction, r : Region, rolls : $[BattleRoll]) extends ForcedAction


// SB3 Cosmic Landfall (Task 3.10.3) — when a GOO is awakened for 4+ Power, CS
// may place one free Meteorite in any region of its choice. The interrupt is threaded into EndAction's normal
// end-of-action continuation (Game.scala) via `then`, exactly like BrownJenkinFamiliarCheckAction:
// the Check offers the region menu (skippable) and every branch resumes the game by Force(then).
// Detection lives in CSExpansion.afterAction (a cathedral-count snapshot), so no action-consuming
// hook is needed and it replays deterministically alongside csPrismaticWellRegions.
case class CSCosmicLandfallCheckAction(self : Faction, then : ForcedAction) extends ForcedAction
case class CSCosmicLandfallPlaceAction(self : Faction, r : Region, then : ForcedAction) extends BaseFactionAction(implicit g => "Place a free " + Meteorite.styled(CS) + " (" + CosmicLandfall.styled(CS) + ") in", implicit g => r + CS.iced(r))


// Corrupted Rending (Tulzscha's GOO ability, §1.8, 1 Power Action) — CS forces a battle between two
// factions (CS itself may be one of them) in a region that holds a Globule and units from 2+
// factions. Flow, all as part of CS's own action: pick region → pick the two factions to battle →
// gated on at least one of the two having positive combat here → if only one does, that faction is
// forced to be the attacker; if both do, CS chooses which is the attacker → a normal battle runs
// between them; afterward control returns to CS (csCorruptedRendingActor).
case class CSCorruptedRendingMainAction(l : $[Region]) extends OptionFactionAction(CorruptedRending.styled(CS) + " — force a battle between at least 2 factions") with MainQuestion with Soft { override def self = CS }
case class CSCorruptedRendingRegionAction(r : Region) extends BaseFactionAction(implicit g => CorruptedRending.styled(CS) + " — force a battle (1 Power) in", implicit g => r + CS.iced(r)) { override def self = CS }
case class CSRendingPickFirstAction(r : Region, a : Faction) extends BaseFactionAction(CorruptedRending.styled(CS) + " — force a battle in " + r + ". Choose factions to participate in battle.", a.full) { override def self = CS }
case class CSRendingPickSecondAction(r : Region, a : Faction, b : Faction) extends BaseFactionAction(CorruptedRending.styled(CS) + " — force a battle in " + r + ". Choose the second faction (to battle " + a.full + ").", b.full) { override def self = CS }
case class CSRendingLaunchAction(r : Region, attacker : Faction, defender : Faction) extends ForcedAction


// Colour Out of Space (CS) EXPANSION — action dispatch + triggers.
object CSExpansion extends Expansion {

    // Is there a live (non-eliminating) Luminous Globule in region r?
    // Globules are a CS-only unit, so only CS can hold one. .not(Zeroed) excludes
    // units killed mid-battle but not yet removed (guide combat-count rule).
    private def globuleIn(r : Region)(implicit game : Game) : Boolean =
        CS.at(r).%(_.uclass == LuminousGlobule).not(Zeroed).any

    // Corrupted Rending (Tulzscha's GOO ability) triggers on a Meteorite or a Globule, per
    // creator correction; kept separate from globuleIn since Chromatic Perversion's Well
    // conversion and Vermiculite Hypertrophy still require a Globule specifically.
    private def meteorOrGlobuleIn(r : Region)(implicit game : Game) : Boolean =
        CS.at(r).%(u => u.uclass == LuminousGlobule || u.uclass == Meteorite).not(Zeroed).any

    // Derived controller of an Excrescence per Chromatic Perversion (Section 1.5): if its region is
    // a Prismatic Well, the faction controlling that Well's Gate owns it; otherwise it is CS's own
    // (summoned via Vermiculite Hypertrophy without a Gate). Figures always stay physical CS units.
    def excrescenceOwner(u : UnitFigure)(implicit game : Game) : Faction =
        if (game.csPrismaticWellRegions.has(u.region))
            game.setup.find(e => e.gates.has(u.region)).getOrElse(CS)
        else CS

    // CHROMATIC PERVERSION (Ongoing) + condition-based Spellbook Requirements.
    // triggers() is invoked from many points (after every action, gather power, rituals),
    // so a state-derived reconciliation here keeps Well status continuously correct —
    // the same shape as DSExpansion.triggers()'s Chaos Gate reversion.
    override def triggers()(implicit game : Game) : Unit = {
        // Use game.setup (not game.factions, which is empty during setup) — guide HARD RULE.
        if (game.setup.has(CS)) {
            // Conversion: any real Gate (game.gates excludes Yog-Sothoth unit-gates and the
            // Moon) sharing a region with a Globule becomes a Prismatic Well, keeping its
            // current controller. Chaos Gates (DS) never convert.
            game.gates.foreach { r =>
                if (game.csPrismaticWellRegions.has(r).not && DS.chaosGateRegions.has(r).not && globuleIn(r)) {
                    game.csPrismaticWellRegions :+= r
                    CS.log("Gate in", r, "became a", "Prismatic Well".styled(CS))
                }
            }
            // Reversion: a Well with no Globule (eliminated or moved away) reverts to a normal
            // Gate. Per the creator, reversion is available in the Action and Doom phases but
            // NOT during Gather Power (so Well income is not disturbed mid-calculation).
            if (game.gatherPowerPhase.not)
                game.csPrismaticWellRegions.foreach { r =>
                    if (globuleIn(r).not) {
                        game.csPrismaticWellRegions = game.csPrismaticWellRegions.%(c => c != r)
                        CS.log("Prismatic Well".styled(CS), "in", r, "became a normal gate")
                    }
                }

            // SBR2: a Prismatic Well exists.
            CS.satisfyIf(CSPrismaticWellExists, "A Prismatic Well exists", game.csPrismaticWellRegions.any)
            // SBR5: another player controls an Excrescence. Per Chromatic Perversion, an
            // Excrescence sharing a region with a Well controlled by a non-CS faction is owned by
            // that faction — so satisfied when any Excrescence sits in such a Well region.
            CS.satisfyIf(CSEnemyControlsExcrescence, "Another player controls an Excrescence",
                game.csPrismaticWellRegions.exists(r =>
                    CS.at(r).%(_.uclass == EffervescentExcrescence).not(Zeroed).any &&
                    game.factions.exists(e => e != CS && e.gates.has(r))))
            // SBR1: a Meteorite exists in another player's starting area. Event-like: once true it
            // stays satisfied (satisfy is permanent), so a later relocation cannot un-satisfy it.
            CS.satisfyIf(CSMeteoriteInEnemyStart, "A Meteorite in an enemy start area",
                CS.onMap(Meteorite).not(Zeroed).exists(u =>
                    game.setup.but(CS).exists(e => game.starting.get(e).contains(u.region))))
        }
    }

    // SB3 Cosmic Landfall (Task 3.10.3) detection. afterAction() runs after every action for every
    // expansion (Game.scala:4481), so CS observes the whole board without consuming anyone's action.
    // The faction card fires Cosmic Landfall "any time a GOO is awakened for 4 power or more" — so we
    // watch the TOTAL number of Great Old Ones in play across ALL factions (own Tulzscha and enemy
    // GOOs alike). A rise since last action means a GOO just entered play, i.e. was awakened; GOO
    // awaken costs in this game are always >= 4 Power, so the count going up is exactly the trigger.
    // We can't read the power paid from this observer hook (afterAction gets no action), and the
    // per-awaken AwakenedAction is single-handler (adding a broad case would steal enemy GOO
    // handling), so the board-count snapshot is the safe, order-independent detector. On a rise we
    // arm the pending flag; Game.scala's EndAction tail (csCosmicLandfallPending) then threads CS's
    // free-Meteorite offer. The snapshot is a Game var, so `new Game()` resets it and replay rebuilds
    // it in lock-step (same undo-safety as csPrismaticWellRegions).
    //   (This replaces the v2.24 4th-cathedral trigger, which required the Ancients faction to be in
    //   the game and so never fired at all in Ancients-less games — the "isn't firing" bug.)
    override def afterAction()(implicit game : Game) : Unit = {
        if (game.setup.has(CS).not) return
        val goos = game.factions.flatMap(_.allInPlay).%(_.uclass.isGOO).not(Zeroed).num
        if (CS.can(CosmicLandfall) && goos > game.csKnownGOOCount)
            game.csCosmicLandfallPending = true
        game.csKnownGOOCount = goos
    }

    // SBR3: a Globule is eliminated (by any cause). eliminate() fires exactly when a unit is
    // removed, so this is the precise event hook the requirement describes.
    override def eliminate(u : UnitFigure)(implicit game : Game) : Unit = {
        if (game.setup.has(CS) && u.uclass == LuminousGlobule && CS.needs(CSGlobuleEliminated))
            CS.satisfy(CSGlobuleEliminated, "A Globule was eliminated")
    }

    def perform(action : Action, soft : VoidGuard)(implicit game : Game) : Continue = action @@ {
        // MAIN ACTION — the engine has NO generic active-player action menu; each faction's
        // expansion assembles its own (see FactionBB/FactionAN). Defer the passive/acted cases
        // to Game's own handlers (UnknownContinue), and build the standard action menu for CS's
        // own turn. CS-specific spellbook/ability actions are appended alongside the stock
        // builders. Without this, MainAction(CS) matches nothing and the game crashes on CS's
        // first turn.
        case MainAction(f : CS.type) if f.active.not =>
            UnknownContinue

        case MainAction(f : CS.type) if f.acted =>
            UnknownContinue

        case MainAction(f : CS.type) =>
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

            // SB2 Vermiculite Hypertrophy: summon an Excrescence at a Cultist+Globule region, no Gate.
            if (f.can(VermiculiteHypertrophy) && f.pool(EffervescentExcrescence).any) {
                val rs = f.onMap(LuminousGlobule).not(Zeroed)./(_.region).distinct
                    .%(r => f.at(r).%(_.uclass == Acolyte).not(Zeroed).any)
                    .%(r => f.affords(EffervescentExcrescence.cost)(r))
                if (rs.any)
                    + CSVermiculiteMainAction(rs)
            }

            // SB5 Effulgent Sacrifice — RETIRED old version (kept, faction WIP). Superseded by the
            // owner respec: a 0-cost action offered to ANY faction (from Game.neutralSpellbooks, see
            // CSEffulgentSacrificeAction), so it is no longer offered from CS's own menu here.
            // if (f.can(EffulgentSacrifice)) {
            //     val rs = f.onMap(LuminousGlobule).not(Zeroed)./(_.region).distinct
            //         .%(r => f.affords(1)(r))
            //     if (rs.any)
            //         + CSEffulgentMainAction(rs)
            // }

            // SB6 Core Exposure: summon a Globule at a Cultist+Meteorite region, consuming the Meteorite.
            if (f.can(CoreExposure) && f.pool(LuminousGlobule).any) {
                val rs = f.onMap(Meteorite).not(Zeroed)./(_.region).distinct
                    .%(r => f.at(r).%(_.uclass == Acolyte).not(Zeroed).any)
                    .%(r => f.affords(1)(r))
                if (rs.any)
                    + CSCoreExposureMainAction(rs)
            }

            // SBR4: standalone Globule sacrifice, offered only until the requirement is met.
            if (f.needs(CSSacrificeGlobule)) {
                val rs = f.onMap(LuminousGlobule).not(Zeroed)./(_.region).distinct
                if (rs.any)
                    + CSSacrificeGlobuleMainAction(rs)
            }

            // Corrupted Rending (Tulzscha's GOO ability): only while Tulzscha is awake (on map) and
            // CS can pay 1 Power; a valid region holds a Globule or Meteorite and units from 2+
            // factions (CS's own units there count, so CS may end up as one of the two factions
            // battling).
            if (f.onMap(CSTulzscha).not(Zeroed).any && f.power >= 1) {
                val rs = areas.%(r => meteorOrGlobuleIn(r) && game.setup.%(e => e.at(r).not(Zeroed).any).num >= 2)
                if (rs.any)
                    + CSCorruptedRendingMainAction(rs)
            }

            game.neutralSpellbooks(f)
            game.libraryActions(f)
            game.highPriests(f)
            game.reveals(f)

            game.endTurn(f)(f.battled.any || game.nexed.any)

            asking

        // Chromatic Perversion cross-faction Well summon (Task 3.8.3): expand the region
        // pick into one Hard leaf per controllable Well, then place from CS's pool.
        case CSWellSummonMainAction(self, l) =>
            Ask(self).each(l)(r => CSWellSummonAction(self, r)).cancel

        case CSWellSummonAction(self, r) =>
            // Draw from CS's pool (not self's — self has no Excrescences of its own); the
            // summoner pays 2 Power + the region's tax. place() runs on CS so the figure is a
            // CS UnitFigure and the universal Cyclopean-Gaze trigger fires with CS as placer.
            if (CS.pool(EffervescentExcrescence).none || self.affords(EffervescentExcrescence.cost)(r).not || game.csPrismaticWellRegions.has(r).not || self.gates.has(r).not)
                EndAction(self)
            else {
                self.power -= EffervescentExcrescence.cost
                self.payTax(r)
                CS.place(EffervescentExcrescence, r)
                self.log("summoned", EffervescentExcrescence.styled(CS), "from a", "Prismatic Well".styled(CS), "in", r)
                SummonedAction(self, EffervescentExcrescence, r, $)
            }

        // Tulzscha custom awaken (Task 3.4.1) — intercept the generic AwakenAction leaf BEFORE
        // Game's own handler (CSExpansion is dispatched ahead of `this`). Pay a fixed 5 Power
        // (always), sacrifice an Acolyte in the region, then roll one die. Tulzscha is placed only
        // after the roll resolves, so hand off to a RollD6 continuation (auto: random 1-6; manual:
        // player enters the actual face). The pip maps to a BattleRoll exactly like BattleRoll.roll.
        case AwakenAction(CS, CSTulzscha, r, cost) =>
            val acolytes = CS.at(r).%(_.uclass == Acolyte).not(Zeroed)
            val globules = CS.at(r).%(_.uclass == LuminousGlobule).not(Zeroed)
            if (CS.pool(CSTulzscha).none || CS.affords(cost)(r).not || acolytes.none || globules.none)
                EndAction(CS)
            else {
                CS.power -= cost
                CS.payTax(r)
                log(CthulhuWarsSolo.DottedLine)
                val a = acolytes.head
                game.eliminate(a)
                CS.log("sacrificed", a, "in", r, "to awaken", CSTulzscha.styled(CS))
                RollD6(_ => "Roll a die for " + CSTulzscha.styled(CS) + "'s awakening", pip => {
                    val result = pip @@ { case 6 => Kill; case 5 | 4 => Pain; case _ => Miss }
                    CSTulzschaAwakenRollAction(r, $(result))
                })
            }

        case CSTulzschaAwakenRollAction(r, rolls) =>
            CS.log("rolled", rolls.mkString(", "), "for", CSTulzscha.styled(CS) + "'s awakening")
            if (rolls.has(Kill)) {
                CS.at(r).%(_.uclass == LuminousGlobule).not(Zeroed).some.foreach { gs =>
                    game.eliminate(gs.head)
                    CS.log("Kill".styled("kill"), "— eliminated a", LuminousGlobule.styled(CS), "and took an", "Elder Sign".styled("es"))
                    CS.takeES(1)
                }
            }
            else
                CS.log("no", "Kill".styled("kill"), "— the", LuminousGlobule.styled(CS), "survives")
            CS.place(CSTulzscha, r)
            // Set CS's start area to Tulzscha's region if it was never recorded (per spec); at
            // setup CS already chose a start, so this is normally a no-op but kept for safety.
            if (game.starting.get(CS).isEmpty)
                game.starting += (CS -> r)
            CS.log("awakened", CSTulzscha.styled(CS), "in", r)
            CS.satisfy(CSAwakenTulzscha, "Awaken Tulzscha")
            // AwakenedAction so downstream GOO-awaken hooks fire (incl. Cosmic Landfall SB3).
            AwakenedAction(CS, CSTulzscha, r, 5)

        // Terminal consumer for the AwakenedAction above — every custom-effect GOO awaken
        // needs one of these or the dispatcher crashes with "unknown continue" (this one was
        // missing, which is what crashed the game). Tulzscha has no further bonus effect here;
        // CSAwakenTulzscha was already satisfied above, so just end the turn.
        case AwakenedAction(self : CS.type, CSTulzscha, _, _) =>
            EndAction(self)

        // SB2 Vermiculite Hypertrophy summon-without-gate (Task 3.10.2).
        case CSVermiculiteMainAction(l) =>
            Ask(CS).each(l)(r => CSVermiculiteAction(r)).cancel

        case CSVermiculiteAction(r) =>
            val cultists = CS.at(r).%(_.uclass == Acolyte).not(Zeroed)
            if (CS.pool(EffervescentExcrescence).none || CS.affords(EffervescentExcrescence.cost)(r).not || cultists.none || globuleIn(r).not)
                EndAction(CS)
            else {
                CS.power -= EffervescentExcrescence.cost
                CS.payTax(r)
                val a = cultists.head
                game.eliminate(a)
                CS.place(EffervescentExcrescence, r)
                CS.log("sacrificed", a, "to summon", EffervescentExcrescence.styled(CS), "in", r, "(" + VermiculiteHypertrophy.styled(CS) + ")")
                SummonedAction(CS, EffervescentExcrescence, r, $)
            }

        /* RETIRED old Effulgent Sacrifice handler (kept, faction WIP). Superseded by the owner
           respec: see CSEffulgentSacrificeAction below and the offer in Game.neutralSpellbooks.
        // SB5 Effulgent Sacrifice (Task 3.10.5).
        case CSEffulgentMainAction(l) =>
            Ask(CS).each(l)(r => CSEffulgentAction(r)).cancel

        case CSEffulgentAction(r) =>
            val globs = CS.at(r).%(_.uclass == LuminousGlobule).not(Zeroed)
            if (CS.affords(1)(r).not || globs.none)
                EndAction(CS)
            else {
                CS.power -= 1
                CS.payTax(r)
                log(CthulhuWarsSolo.DottedLine)
                game.eliminate(globs.head)
                CS.log("sacrificed", LuminousGlobule.styled(CS), "in", r, "for an", "Elder Sign".styled("es"), "(" + EffulgentSacrifice.styled(CS) + ")")
                CS.takeES(1)
                // Any Cultists or Excrescences in the region are eliminated; each owner refunded
                // half the unit's cost rounded down. An Excrescence's owner is its derived Well
                // controller (Section 1.5), else CS; a Cultist's owner is its own faction.
                game.setup.foreach { e =>
                    e.at(r).%(u => u.uclass.utype == Cultist || u.uclass == EffervescentExcrescence).not(Zeroed).foreach { u =>
                        val owner = (u.uclass == EffervescentExcrescence).?(excrescenceOwner(u)).|(u.faction)
                        val refund = u.uclass.cost / 2
                        game.eliminate(u)
                        if (refund > 0) {
                            owner.power += refund
                            owner.log("lost", u.uclass.styled(owner), "in", r, "and recovered", refund.power)
                        }
                    }
                }
                // Every faction (including CS itself) with units still standing in r must now retreat them all out.
                val others = game.setup.%(e => e.at(r).not(Zeroed).any)
                if (others.any)
                    Force(CSEffulgentRetreatFactionsAction(r, others))
                else
                    EndAction(CS)
            }

        // SB5 Effulgent Sacrifice retreat sweep (addendum) — see case classes above.
        case CSEffulgentRetreatFactionsAction(r, remaining) =>
            if (remaining.none)
                EndAction(CS)
            else if (remaining.num == 1)
                Force(CSEffulgentRetreatUnitAction(remaining.head, r, $))
            else
                Ask(CS).each(remaining)(a => CSEffulgentRetreatPickFactionAction(r, a, remaining.%(_ != a)).as(a)("Choose the next faction to retreat out of", r))

        case CSEffulgentRetreatPickFactionAction(r, a, remaining) =>
            CS.log("chose", a.full, "to retreat next out of", r, "(" + EffulgentSacrifice.styled(CS) + ")")
            Force(CSEffulgentRetreatUnitAction(a, r, remaining))

        case CSEffulgentRetreatUnitAction(f, r, remaining) =>
            val units = f.at(r).not(Zeroed)
            if (units.none)
                Force(CSEffulgentRetreatFactionsAction(r, remaining))
            else
                Ask(f).each(units)(u => CSEffulgentRetreatUnitPickAction(f, u.ref, r, remaining).as(u.ref)("Retreat a unit from", r))

        case CSEffulgentRetreatUnitPickAction(f, u, r, remaining) =>
            Ask(f).each(r.connectedForRetreat)(to => CSEffulgentRetreatMoveAction(f, u, to, r, remaining).as(to)("Retreat", u.full, "to"))

        case CSEffulgentRetreatMoveAction(f, u, to, r, remaining) =>
            u.region = to
            u.onGate = false
            f.log("retreated", u.full, "from", r, "to", to, "(" + EffulgentSacrifice.styled(CS) + ")")
            Force(CSEffulgentRetreatUnitAction(f, r, remaining))
        */

        // SB5 Effulgent Sacrifice — owner respec (0-cost cross-faction action, see class above).
        case CSEffulgentSacrificeAction(f, r) =>
            log(CthulhuWarsSolo.DottedLine)
            // Eliminate ALL the acting faction's Cultists in the region.
            f.at(r).%(_.uclass.utype == Cultist).not(Zeroed).foreach(game.eliminate)
            // Eliminate the Well's Globule (a Prismatic Well always has a Globule).
            CS.at(r).%(_.uclass == LuminousGlobule).not(Zeroed).foreach(game.eliminate)
            f.log("sacrificed all its cultists and eliminated the", LuminousGlobule.styled(CS), "in", r, "(" + EffulgentSacrifice.styled(CS) + ")")
            // Acting faction gains a Doom.
            f.doom += 1
            f.log("gained", 1.doom, "(" + EffulgentSacrifice.styled(CS) + ")")
            // CS gains an Elder Sign no matter who acts (so if CS itself acts, it gains both).
            CS.takeES(1)
            CS.log("gained an", "Elder Sign".styled("es"), "(" + EffulgentSacrifice.styled(CS) + ")")
            EndAction(f)

        // SB6 Core Exposure (Task 3.10.6).
        case CSCoreExposureMainAction(l) =>
            Ask(CS).each(l)(r => CSCoreExposureAction(r)).cancel

        case CSCoreExposureAction(r) =>
            val mets = CS.at(r).%(_.uclass == Meteorite).not(Zeroed)
            val cultists = CS.at(r).%(_.uclass == Acolyte).not(Zeroed)
            if (CS.pool(LuminousGlobule).none || mets.none || cultists.none || CS.affords(1)(r).not)
                EndAction(CS)
            else {
                CS.power -= 1
                CS.payTax(r)
                game.eliminate(mets.head)
                CS.place(LuminousGlobule, r)
                CS.log("consumed a", Meteorite.styled(CS), "to summon", LuminousGlobule.styled(CS), "in", r, "(" + CoreExposure.styled(CS) + ")")
                SummonedAction(CS, LuminousGlobule, r, $)
            }

        // SBR4 standalone Globule sacrifice (Task 3.12.2).
        case CSSacrificeGlobuleMainAction(l) =>
            Ask(CS).each(l)(r => CSSacrificeGlobuleAction(r)).cancel

        case CSSacrificeGlobuleAction(r) =>
            val globs = CS.at(r).%(_.uclass == LuminousGlobule).not(Zeroed)
            if (globs.none)
                EndAction(CS)
            else {
                log(CthulhuWarsSolo.DottedLine)
                game.eliminate(globs.head)
                CS.log("sacrificed", LuminousGlobule.styled(CS), "in", r, "as an action")
                CS.satisfy(CSSacrificeGlobule, "Sacrifice a Globule as an action")
                // If an enemy controls the Well's Gate in this region, they gain 2 Power.
                if (game.csPrismaticWellRegions.has(r))
                    game.setup.find(e => e != CS && e.gates.has(r)).foreach { e =>
                        e.power += 2
                        e.log("gained", 2.power, "from a", "Prismatic Well".styled(CS), "as", LuminousGlobule.styled(CS), "was sacrificed in", r)
                    }
                EndAction(CS)
            }

        // SB4 Spectral Collapse (Task 3.10.4) — logic side (Battle.scala arms the offer and
        // resumes the battle via proceed()). Accepting rolls one combat die: Kill → 1 Elder Sign,
        // Pain → 1 Doom; then the Globule collapses regardless of the result.
        case CSSpectralCollapseUseAction(self, r, queue) =>
            RollBattle(self, SpectralCollapse.name, 1, rolls => CSSpectralCollapseRollAction(self, r, rolls))

        case CSSpectralCollapseRollAction(self, r, rolls) =>
            self.log("rolled", rolls.mkString(", "), "for", SpectralCollapse.styled(CS))
            if (rolls.has(Kill)) {
                self.takeES(1)
                self.log("Kill".styled("kill"), "— gained an", "Elder Sign".styled("es"))
            }
            else if (rolls.has(Pain)) {
                self.doom += 1
                self.log("Pain".styled("pain"), "— gained", 1.doom)
            }
            CS.at(r).%(_.uclass == LuminousGlobule).not(Zeroed).some.foreach { gs =>
                game.eliminate(gs.head)
                log(LuminousGlobule.styled(CS), "collapsed in", r, "(" + SpectralCollapse.styled(CS) + ")")
            }
            // Hand back to Battle.scala, which resumes the paused battle via proceed().
            UnknownContinue

        case CSSpectralCollapseSkipAction(self, r, queue) =>
            self.log("declined", SpectralCollapse.styled(CS))
            if (queue.any)
                Ask(queue.head)
                    .add(CSSpectralCollapseUseAction(queue.head, r, queue.tail))
                    .add(CSSpectralCollapseSkipAction(queue.head, r, queue.tail))
            else
                // No one accepted — hand back to Battle.scala's proceed().
                UnknownContinue

        // SB3 Cosmic Landfall (Task 3.10.3) — the interrupt, threaded from Game.scala's EndAction
        // tail. Consume the pending flag here (idempotent + replay-stable); if CS still controls the
        // spellbook and has a Meteorite in pool, offer the (skippable) region menu restricted to
        // EMPTY regions — a region with no UNITS (Cultist/Monster/Terror/GOO) of any faction.
        // Buildings, tokens, map-markers, glyphs, and gates (even abandoned ones) do NOT count as
        // occupation, so a region holding only those is still empty. Every path resumes via Force(then).
        case CSCosmicLandfallCheckAction(self, then) =>
            game.csCosmicLandfallPending = false
            def emptyOfUnits(r : Region) =
                game.setup.%(e => e.at(r).not(Zeroed).%(u => u.uclass.utype != Token && u.uclass.utype != Building && u.uclass.utype != MapUnit).any).none
            if (CS.can(CosmicLandfall) && CS.pool(Meteorite).any)
                Ask(CS).each(areas.%(emptyOfUnits))(r => CSCosmicLandfallPlaceAction(CS, r, then)).skip(then)
            else
                Force(then)

        case CSCosmicLandfallPlaceAction(self, r, then) =>
            CS.place(Meteorite, r)
            CS.log("placed a free", Meteorite.styled(CS), "in", r, "(" + CosmicLandfall.styled(CS) + ")")
            Force(then)

        // Corrupted Rending (§1.8) — CS forces a battle between at least 2 factions (CS included).
        case CSCorruptedRendingMainAction(l) =>
            Ask(CS).each(l)(r => CSCorruptedRendingRegionAction(r)).cancel

        case CSCorruptedRendingRegionAction(r) =>
            // Re-validate (menu may be stale): Globule or Meteorite present + 2+ factions with units here.
            val candidates = game.setup.%(e => e.at(r).not(Zeroed).any)
            if (meteorOrGlobuleIn(r).not || candidates.num < 2 || CS.power < 1)
                EndAction(CS)
            else if (candidates.num == 2)
                // Only two factions here — no choice to make; go straight to the battle.
                Force(CSRendingPickSecondAction(r, candidates.head, candidates.last))
            else
                Ask(CS).each(candidates)(a => CSRendingPickFirstAction(r, a)).cancel

        case CSRendingPickFirstAction(r, a) =>
            val candidates = game.setup.%(e => e.at(r).not(Zeroed).any).%(_ != a)
            if (candidates.none)
                EndAction(CS)
            else
                Ask(CS).each(candidates)(b => CSRendingPickSecondAction(r, a, b)).cancel

        case CSRendingPickSecondAction(r, a, b) =>
            if (CS.power < 1 || a.at(r).not(Zeroed).none || b.at(r).not(Zeroed).none)
                EndAction(CS)
            else {
                val combatA = a.strength(a.at(r).not(Zeroed), b)
                val combatB = b.strength(b.at(r).not(Zeroed), a)
                // Gate: at least one of the two chosen factions must have positive combat here,
                // or there is no legal attacker for this pair.
                if (combatA <= 0 && combatB <= 0)
                    EndAction(CS)
                else {
                    CS.power -= 1
                    CS.acted = true
                    log(CthulhuWarsSolo.DottedLine)
                    CS.log("used", CorruptedRending.styled(CS), "to force a battle between", a.full, "and", b.full, "in", r)
                    if (combatA <= 0)
                        // a has no combat here — b is the only faction that can attack.
                        Force(CSRendingLaunchAction(r, b, a))
                    else if (combatB <= 0)
                        // b has no combat here — a is the only faction that can attack.
                        Force(CSRendingLaunchAction(r, a, b))
                    else
                        Ask(CS)
                            .add(CSRendingLaunchAction(r, a, b).as(a.full)("Corrupted Rending: Choose Attacking Faction"))
                            .add(CSRendingLaunchAction(r, b, a).as(b.full)("Corrupted Rending: Choose Attacking Faction"))
                }
            }

        case CSRendingLaunchAction(r, attacker, defender) =>
            // Re-validate the two factions still have units in the arena (choosing the attacker
            // doesn't move units, so normally fine). Then run a standard battle between them, with
            // control returning to CS at battle-end via csCorruptedRendingActor (Game/Battle terminus).
            if (attacker.at(r).not(Zeroed).none || defender.at(r).not(Zeroed).none)
                EndAction(CS)
            else {
                game.csCorruptedRendingActor = |(CS)
                // Ghatanothoa's combat reads FB.power at battle start; snapshot it if FB is present.
                if (game.factions.has(FB))
                    game.fbPowerAtBattleStart = FB.power
                attacker.log("was forced to battle", defender.full, "in", r, "by", CorruptedRending.styled(CS))
                game.queue = game.queue ++ $(new Battle(r, attacker, defender, None))
                ProceedBattlesAction
            }

        // SB6 Core Exposure free-carry — CS moved a Cultist out of `o`; offer to bring any Globule(s)
        // at `o` along to the destination for free. This case is GUARDED so that when there is nothing
        // to carry it falls through to `case _ => UnknownContinue`, letting the Hound Chronophage hook
        // (NeutralMonsters.scala) or the Game.scala fallback handle the MovedAction normally. CSExpansion
        // is iterated ahead of NeutralMonstersExpansion, so this fires FIRST; every resolution path then
        // threads into CronophageAfterMoveAction so a CS-owned Hound still gets its teleport afterward.
        case MovedAction(self, u, o, r) if self == CS && CS.can(CoreExposure) && u.uclass.utype == Cultist && CS.at(o).%(_.uclass == LuminousGlobule).not(Moved).not(Zeroed).any =>
            Force(CSCarryGlobuleOfferAction(o, r))

        case CSCarryGlobuleOfferAction(o, to) =>
            val gs = CS.at(o).%(_.uclass == LuminousGlobule).not(Moved).not(Zeroed)
            if (gs.any)
                Ask(CS)
                    .each(gs.sortA)(g => CSCarryGlobuleAction(CS, o, g, to).as(g.ref.full, "to", to)(CoreExposure.styled(CS) + " — carry " + LuminousGlobule.styled(CS) + " (free)"))
                    .skip(CronophageAfterMoveAction(CS, MoveContinueAction(CS, true)))
            else
                Force(CronophageAfterMoveAction(CS, MoveContinueAction(CS, true)))

        case CSCarryGlobuleAction(self, o, u, to) =>
            u.region = to
            u.onGate = false
            u.add(Moved)
            u.add(MovedForFree)
            CS.log(CoreExposure.styled(CS) + ": carried", u, "from", o, "to", to, "(free)")
            // Re-offer any remaining Globules at the origin, then continue.
            Force(CSCarryGlobuleOfferAction(o, to))

        case _ => UnknownContinue
    }
}
