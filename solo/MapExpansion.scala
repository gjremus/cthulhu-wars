package cws

import hrf.colmat._
import html._

// ══════════════════════════════════════════════════════════════════════════════
// LIBRARY AT CELAENO — Map Expansion
// Implements Library-specific mechanics: Silence Tokens, Custodian, Librarian,
// Library Tomes (4 map spellbooks), and Agony Die.
// ══════════════════════════════════════════════════════════════════════════════

// Agony tally — $[Offer] of (faction, amount). Carried as an action field on
// every Custodian/Librarian agony action so the running total survives the
// write→parse round trip. Stored as a list (not Map) to reuse the existing
// offer/list grammar; a Map literal {F->n} would collide with the offer rule.
object AgonyOps {
    implicit class AgonyExt(val xs : $[Offer]) extends AnyVal {
        def agonyFor(f : Faction) : Int = xs.find(_.f == f)./(_.n).|(0)
        def agonySet(f : Faction, n : Int) : $[Offer] = xs.%(_.f != f) :+ Offer(f, n)
        def agonyAdd(f : Faction, n : Int) : $[Offer] = {
            val current = xs.find(_.f == f)./(_.n).|(0)
            xs.%(_.f != f) :+ Offer(f, current + n)
        }
        def agonyClear(f : Faction) : $[Offer] = xs.%(_.f != f)
        def agonyTotal : Int = xs./(_.n).sum
    }
}
import AgonyOps._

// ── MAP UNITS ──
case object TheCustodian extends MapUnitClass("Custodian")
case object TheLibrarian extends MapUnitClass("Librarian")

// ── NEUTRAL MAP FACTION ──
case object LibraryFaction extends NeutralFaction {
    def name = "Library"
    def short = "LB"
    def style = "lb"

    override def abilities = $
    override def library = $
    override def requirements(options : $[GameOption]) = $

    val allUnits = $(TheCustodian, TheLibrarian)

    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int = 0
}

// ── LIBRARY TOMES (Map Spellbooks) ──
// Four tomes, one per Special Collection room:
//   1. Barrier of Naach-Tith (always active, never flips) — battle gate
//   2. Guardian under the Lake (flippable) — move units between Archways
//   3. Larvae of the Outer Gods (flippable) — gain ES if opponent has more Power
//   4. Yr and the Nhhngr (flippable) — place Monster or gain Power if opponent has more Doom
sealed trait LibraryTome extends Record {
    def name : String
    def region(implicit game : Game) : Region
    def elem = name.styled("lb")
    override def toString = name
}
case object TomeBarrier extends LibraryTome {
    def name = "Barrier of Naach-Tith"
    def region(implicit game : Game) = LibraryCelaeno55.BarrierOfNaachTith
}
case object TomeGuardian extends LibraryTome {
    def name = "Guardian under the Lake"
    def region(implicit game : Game) = LibraryCelaeno55.GuardianUnderLake
}
case object TomeLarvae extends LibraryTome {
    def name = "Larvae of the Outer Gods"
    def region(implicit game : Game) = LibraryCelaeno55.LarvaeOfOuterGods
}
case object TomeYr extends LibraryTome {
    def name = "Yr and the Nhhngr"
    def region(implicit game : Game) = LibraryCelaeno55.YrAndTheNhhngr
}

// ── AGONY DIE ──
// Custom d6: faces 1, 2, 2, 3, 3, 4
object AgonyDie {
    val faces = $(1, 2, 2, 3, 3, 4)
    def roll() : Int = faces.shuffle.first
}

// ── CUSTODIAN / LIBRARIAN starting positions (black circles on L floor bitmaps) ──
// In joined vertical image coordinates. Librarian = left circle, Custodian = right circle.
object LibraryUnitPlacement {
    // (cx, cy) in joined vertical image space, diameter ~175px
    val librarianPos5L = (653, 1910)
    val custodianPos5L = (1164, 1910)
    val librarianPos3L = (642, 1959)
    val custodianPos3L = (1092, 1959)
    val unitSize = 150 // larger than monsters (~80) but smaller than GOOs (~160+)
}

// ── CONNECTION GLYPH HELPER ──
// Archway glyph: only when archway is the ONLY means of connection (not in base adjacency).
// Stairwell glyph: any base adjacency between regions on different floors (upper vs lower).
object ConnectionGlyph {
    def apply(from : Region, to : Region)(implicit game : Game) : String = {
        if (!game.board.isLibraryMap) return ""
        val base = game.board.connectedForRetreat(from) // = baseConnected on Library maps
        val inBase = base.contains(to)

        // Determine if regions are on different floors
        val upperRegions = LibraryCelaeno55.upperRegions
        val fromUpper = upperRegions.contains(from)
        val toUpper = upperRegions.contains(to)
        val crossFloor = fromUpper != toUpper

        // Stairwell: base adjacency across floors
        if (inBase && crossFloor) {
            val letter = StairwellLetters.letterFor(from, to).getOrElse("A")
            return s"""<img src="${Overlays.imageSource("stairwell-" + letter)}" style="height:1.4em;vertical-align:middle;margin-right:2px"/>"""
        }

        // Archway: only if NOT in base adjacency (archway is the sole connection)
        if (!inBase && game.board.archways.contains(from) && game.board.archways.contains(to))
            return s"""<img src="${Overlays.imageSource("archway-glyph")}" style="height:1.2em;vertical-align:middle;margin-right:2px"/>"""

        ""
    }
}

// ── STAIRWELL LETTER MAPPINGS ──
// Stairwell letters are assigned to physical positions. Different regions connect
// through them depending on which map halves (3U/5U, 3L/5L) are used.
// The letter is determined by the PAIR of cross-floor regions, not individual regions.
object StairwellLetters {
    import LibraryCelaeno55._
    // Cross-floor pairs → letter. Order doesn't matter within the pair.
    val pairs : $[((Region, Region), String)] = $(
        // A: top-left stairwell
        ((Byakhiary, ChamberOfSngac), "A"),
        ((YrAndTheNhhngr, ChamberOfSngac), "A"),  // 3U variant (no Byakhiary)
        // B: center stairwell
        ((Fountain, Oubliette), "B"),
        // C: top-right stairwell
        ((Horrorium, BlueHall), "C"),
        ((GuardianUnderLake, BlueHall), "C"),  // 3U variant (no Horrorium)
        // D: left-mid stairwell
        ((BarrierOfNaachTith, ChamberOfApkallu), "D"),  // 3L variant
        ((BarrierOfNaachTith, ScorchedChamber), "D"),    // 5L variant
        // E: center-bottom stairwell
        ((LakeOfHaliOverlook, Hyperquarium), "E"),
        // F: right-bottom stairwell
        ((LarvaeOfOuterGods, TheCrawlingOnes), "F")
    )

    def letterFor(from : Region, to : Region) : |[String] = {
        pairs.find { case ((a, b), _) => (a == from && b == to) || (a == to && b == from) }.map(_._2)
    }
}

// ── TOME PLACEMENT on map (pixel coordinates in vertical placement bitmap) ──
// Each tome occupies a black square on the 5U/3U tome bitmaps.
// Center and size are in the 1791x3584 vertical image coordinate space.
object TomePlacement {
    // 5U tome positions (in vertical placement bitmap coords)
    val positions5U : $[(LibraryTome, Int, Int, Int, Int)] = $(
        (TomeYr,       292,  749, 234, 234),
        (TomeGuardian, 1402, 774, 234, 234),
        (TomeLarvae,   1349, 1312, 231, 232),
        (TomeBarrier,  303,  1417, 238, 236)
    )
    // 3U tome positions (different layout)
    val positions3U : $[(LibraryTome, Int, Int, Int, Int)] = $(
        (TomeYr,       302,  552, 204, 196),
        (TomeGuardian, 1409, 537, 206, 200),
        (TomeLarvae,   1425, 1235, 206, 200),
        (TomeBarrier,  300,  1353, 208, 198)
    )
    // Pick positions based on whether the board has 5U or 3U upper floor
    def positions(implicit game : Game) : $[(LibraryTome, Int, Int, Int, Int)] = {
        val has5U = game.board.regions.exists(_.name == "Byakhiary")
        if (has5U) positions5U else positions3U
    }
}

// ── ACTIONS ──

// Custodian activation
case class SpendOnCustodianAction(self : Faction) extends OptionFactionAction(implicit g => "Activate " + "Custodian".styled("lb")) with MainQuestion
case class CustodianMoveAction(self : Faction, r : Region) extends BaseFactionAction(implicit g => "Move " + "Custodian".styled("lb") + " to", r)
case class CustodianStayAction(self : Faction, r : Region) extends BaseFactionAction(implicit g => "Custodian".styled("lb") + " stays in", implicit g => r.toString + " " + "(+1 to Agony roll)".styled("power"))
case class CustodianAgonyRolledAction(self : Faction, r : Region, rolled : Int, staying : Boolean) extends ForcedAction
case class CustodianAssignAgonyAction(self : Faction, r : Region, remaining : Int, assigned : $[Offer]) extends ForcedAction
case class CustodianAssignToFactionAction(self : Faction, r : Region, remaining : Int, assigned : $[Offer], target : Faction) extends BaseFactionAction(implicit g => "Assign 1 " + "Agony".styled("lb") + " to", implicit g => target.full + " (" + remaining + " remaining)")
case class CustodianResolveAgonyAction(self : Faction, r : Region, assigned : $[Offer]) extends ForcedAction
case class CustodianMoveToOublietteAction(self : Faction, r : Region, target : Faction, uRef : UnitRef, remaining : $[Offer]) extends BaseFactionAction(implicit g => target.full + " moves", implicit g => g.unit(uRef).full + " to " + "Oubliette".styled("lb"))

// Librarian activation
case class SpendOnLibrarianAction(self : Faction) extends OptionFactionAction(implicit g => "Activate " + "Librarian".styled("lb")) with MainQuestion
case class LibrarianMoveAction(self : Faction, r : Region) extends BaseFactionAction(implicit g => "Move " + "Librarian".styled("lb") + " to", r)
case class LibrarianStayAction(self : Faction, r : Region) extends BaseFactionAction(implicit g => "Librarian".styled("lb") + " stays in", implicit g => r.toString + " " + "(+1 to Agony roll)".styled("power"))
// Librarian agony distribution: activator picks a faction, then picks how many of
// the remaining agony to assign to them, repeating until all agony is allocated.
// `total` is the original agony rolled (for "Reset agony assignments").
// `remaining` is the unassigned amount; `assigned` maps faction → count assigned;
// `order` preserves the activator's selection sequence so resolution walks in
// the same order the activator chose.
case class LibrarianAgonyRolledAction(self : Faction, r : Region, rolled : Int, staying : Boolean) extends ForcedAction
case class LibrarianAssignAgonyAction(self : Faction, r : Region, total : Int, remaining : Int, assigned : $[Offer], order : $[Faction]) extends ForcedAction
case class LibrarianAssignToFactionAction(self : Faction, r : Region, total : Int, remaining : Int, assigned : $[Offer], order : $[Faction], target : Faction) extends BaseFactionAction(
    implicit g => self.full + " — choose a faction to assign up to " + remaining + " " + "Agony".styled("lb") + " to",
    implicit g => {
        val sofar = assigned.agonyFor(target)
        if (sofar > 0) target.full + " (" + sofar + " assigned)" else target.full
    }
)
case class LibrarianAssignAmountAction(self : Faction, r : Region, total : Int, remaining : Int, assigned : $[Offer], order : $[Faction], target : Faction, amount : Int) extends BaseFactionAction(
    implicit g => self.full + " — assign " + "Agony".styled("lb") + " to " + target.full + " (up to " + remaining + ")",
    implicit g => amount.toString + " " + "Agony".styled("lb")
)
case class LibrarianAssignCancelAction(self : Faction, r : Region, total : Int, remaining : Int, assigned : $[Offer], order : $[Faction]) extends BaseFactionAction(
    implicit g => self.full + " — assign " + "Agony".styled("lb"),
    implicit g => "Cancel"
)
case class LibrarianResetAgonyAction(self : Faction, r : Region, total : Int) extends BaseFactionAction(
    implicit g => self.full + " — choose a faction to assign up to " + total + " " + "Agony".styled("lb") + " to",
    implicit g => "Reset agony assignments"
)
case class LibrarianResolveAgonyAction(self : Faction, assigned : $[Offer], order : $[Faction]) extends ForcedAction
case class LibrarianSatisfyAgonyAction(target : Faction, agony : Int, remaining : $[Offer], activator : Faction, order : $[Faction]) extends ForcedAction
case class LibrarianEliminateUnitMainAction(self : Faction, agony : Int, remaining : $[Offer], activator : Faction, order : $[Faction]) extends OptionFactionAction("Eliminate a unit (1 " + "Agony".styled("lb") + ")") {
    def question(implicit game : Game) = self.full + " " + "Satisfy Agony".styled("lb") + " — " + agony + " remaining"
}
case class LibrarianEliminateRegionAction(self : Faction, r : Region, agony : Int, remaining : $[Offer], activator : Faction, order : $[Faction], eliminated : $[UnitRef]) extends ForcedAction
case class LibrarianEliminateUnitAction(self : Faction, uRef : UnitRef, r : Region, agony : Int, remaining : $[Offer], activator : Faction, order : $[Faction], eliminated : $[UnitRef]) extends BaseFactionAction(implicit g => "Eliminate", implicit g => g.unit(uRef).full)
case class LibrarianEliminateDoneAction(self : Faction, agony : Int, remaining : $[Offer], activator : Faction, order : $[Faction], eliminated : $[UnitRef]) extends ForcedAction
case class LibrarianReturnTomeMainAction(self : Faction, agony : Int, remaining : $[Offer], activator : Faction, order : $[Faction]) extends OptionFactionAction("Return " + "Overdue Tome".styled("lb") + " (1 " + "Agony".styled("lb") + ")") {
    def question(implicit game : Game) = self.full + " " + "Satisfy Agony".styled("lb") + " — " + agony + " remaining"
}
case class LibrarianReturnTomeAction(self : Faction, tome : LibraryTome, agony : Int, remaining : $[Offer], activator : Faction, order : $[Faction]) extends BaseFactionAction(implicit g => "Return", implicit g => tome.elem)
case class LibrarianLoseDoomAction(self : Faction, agony : Int, remaining : $[Offer], activator : Faction, order : $[Faction]) extends OptionFactionAction("Lose 1 " + "Doom".styled("doom") + " (1 " + "Agony".styled("lb") + ")") {
    def question(implicit game : Game) = self.full + " " + "Satisfy Agony".styled("lb") + " — " + agony + " remaining"
}

// Barrier of Naach-Tith battle gate
// AttackBarrierCheckAction is a SOFT wrapper that sits between AttackMainAction's
// target picker and AttackAction. Because it is Soft, the engine does NOT update
// the `continue` cancel-snapshot when it returns the barrier menu Ask — so the
// Cancel button on the barrier menu returns to the battle target picker, not to
// itself. See "Cancel-Button Pattern" in the Faction/Map implementation guides
// for the full explanation of why Soft is required for ANY sub-menu launcher.
// BarrierMenuOpenAction: appears in AttackMainAction's variant list IN PLACE OF
// AttackAction when the target faction holds Barrier of Naach-Tith. The button reads
// "n/a — <faction>" and clicking it opens the payment sub-menu. Soft so the payment
// Ask doesn't overwrite the continue snapshot — Cancel returns to the battle picker.
// Barrier-protected target variant. Question groups under the normal "Battle in r"
// header so it sorts with the other regions. Option (the button label) shows the
// region name so the player can identify which target it is — clicking opens the
// payment sub-menu.
case class BarrierMenuOpenAction(self : Faction, r : Region, f : Faction, effect : |[Spellbook]) extends BaseFactionAction(implicit g => "Battle in " + r + effect./(" with " + _).|(""), implicit g => "" + f + " (Barrier)") with Soft

// Legacy Soft cancel-bridge — re-issues the parent battle target picker.
case class BarrierCancelToAttackMainAction(self : Faction, r : Region, effect : |[Spellbook]) extends ForcedAction with Soft

// Legacy AttackBarrierCheckAction (replaced by BarrierMenuOpenAction; class kept so
// existing bot patterns don't break). Soft no-op forwards to AttackAction.
case class AttackBarrierCheckAction(self : Faction, r : Region, f : Faction, effect : |[Spellbook]) extends ForcedAction with Soft
case class BarrierCheckAction(attacker : Faction, defender : Faction, then : Action) extends ForcedAction with Soft
case class BarrierReleaseCultistFactionAction(self : Faction, captiveFaction : Faction, then : Action) extends ForcedAction
case class BarrierDiscardESAction(self : Faction, then : Action) extends ForcedAction
case class BarrierDiscardTokenAction(self : Faction, then : Action) extends ForcedAction
case class BarrierBlockedAction(self : Faction) extends ForcedAction

// Tome flip via Silence Token
case class SpendToFlipTomeAction(self : Faction, tome : LibraryTome) extends BaseFactionAction(implicit g => "Flip " + tome.elem + " face-up", "(" + "Silence Token".styled("lb") + ")") {
    override def question(implicit game : Game) = "Spend " + "Silence Token".styled("lb")
}

// Tome usage actions
case class UseTomeGuardianMainAction(self : Faction) extends OptionFactionAction(implicit g => "Use " + TomeGuardian.elem + " (" + "1 Power".styled("power") + ")") with MainQuestion with Soft
case class UseTomeGuardianRegionAction(self : Faction, source : Region) extends BaseFactionAction(implicit g => "Relocate enemy units in", source) with Soft {
    override def question(implicit game : Game) = "Choose region with " + TomeGuardian.elem
}
case class UseTomeGuardianRelocateAction(self : Faction, source : Region, target : Faction) extends BaseFactionAction(implicit g => "Choose faction to relocate from " + source, implicit g => target.full) with Soft
case class UseTomeGuardianDestAction(self : Faction, source : Region, target : Faction, dest : Region) extends BaseFactionAction(implicit g => "Move to", dest) {
    override def question(implicit game : Game) = "Choose destination for " + target.full + " units from " + source
}

case class UseTomeLarvaeAction(self : Faction) extends OptionFactionAction(implicit g => "Use " + TomeLarvae.elem + " (" + "1 Power".styled("power") + ")") with MainQuestion

case class UseTomeYrMainAction(self : Faction) extends OptionFactionAction(implicit g => "Use " + TomeYr.elem + " (" + "1 Power".styled("power") + ")") with MainQuestion with Soft
case class UseTomeYrMonsterAction(self : Faction) extends OptionFactionAction(implicit g => "Place a Monster at a Gate") with Soft {
    def question(implicit game : Game) = "Use " + TomeYr.elem
}
case class UseTomeYrMonsterChooseAction(self : Faction, uc : UnitClass, r : Region) extends BaseFactionAction(implicit g => "Place " + uc.styled(self) + " in", r)
case class UseTomeYrPowerAction(self : Faction) extends OptionFactionAction(implicit g => "Gain " + "2 Power".styled("power") + " (net " + "+1".styled("power") + ")") {
    def question(implicit game : Game) = "Use " + TomeYr.elem
}

// Flippable tome flip-up actions (unlimited, no power cost) — shared by Guardian, Larvae, Yr
case class FlipTomeReleaseCultistMainAction(self : Faction, tome : LibraryTome) extends OptionFactionAction(implicit g => "Release a Captured Cultist to flip " + tome.elem + " face-up") with MainQuestion with Soft {
    override def question(implicit game : Game) = "Flip " + tome.elem + " face-up"
}
case class FlipTomeReleaseCultistAction(self : Faction, tome : LibraryTome, uRef : UnitRef) extends ForcedAction
case class FlipTomeDiscardESAction(self : Faction, tome : LibraryTome) extends OptionFactionAction(implicit g => "Discard an " + "Elder Sign".styled("es") + " to flip " + tome.elem + " face-up") with MainQuestion
case class FlipTomeDiscardTokenAction(self : Faction, tome : LibraryTome) extends OptionFactionAction(implicit g => "Discard a " + "Silence Token".styled("lb") + " to flip " + tome.elem + " face-up") with MainQuestion


// ── EXPANSION ──
object LibraryExpansion extends Expansion {

    def perform(action : Action, soft : VoidGuard)(implicit game : Game) : Continue = action @@ {


        // ── CUSTODIAN ACTIVATION ──
        case SpendOnCustodianAction(self) =>
            game.silenceTokens = game.silenceTokens + (self -> (game.silenceTokens.getOrElse(self, 0) - 1))
            self.log("spent a", "Silence Token".styled("lb"), "to activate the", "Custodian".styled("lb"))

            val currentRegion = game.custodianRegion

            val stayActions = currentRegion./(r => CustodianStayAction(self, r)).$
            val moveActions = game.board.regions.%(_.glyph.onMap).%(r => !currentRegion.has(r))./( r =>
                CustodianMoveAction(self, r))
            Ask(self).each(stayActions ++ moveActions)(identity)

        case CustodianMoveAction(self, r) =>
            game.custodianRegion = |(r)
            self.log("moved", "Custodian".styled("lb"), "to", r)
            RollAgony(_ => "Roll Agony die for Custodian", x => CustodianAgonyRolledAction(self, r, x, false))

        case CustodianStayAction(self, r) =>
            self.log("Custodian".styled("lb"), "stays in", r, "(+1 to Agony roll)")
            RollAgony(_ => "Roll Agony die for Custodian (+1 for staying)", x => CustodianAgonyRolledAction(self, r, x, true))

        // ── CUSTODIAN AGONY ROLL (recorded so replay/online shows the roll) ──
        case CustodianAgonyRolledAction(self, r, rolled, staying) =>
            if (staying) {
                log("Agony Die".styled("lb"), "rolled", rolled.toString.styled("power"), "+ 1 for staying =", (rolled + 1).toString.styled("power"), "total")
                Force(CustodianAssignAgonyAction(self, r, rolled + 1, $))
            }
            else {
                log("Agony Die".styled("lb"), "rolled", rolled.toString.styled("power"))
                Force(CustodianAssignAgonyAction(self, r, rolled, $))
            }

        // ── CUSTODIAN AGONY DISTRIBUTION ──
        case CustodianAssignAgonyAction(self, r, remaining, assigned) =>
            // Custodian forcibly MOVES units to the Oubliette. An AN Cathedral can never be
            // moved by any means (Building or Holy-Ground GOO alike), so it is excluded here
            // regardless of phase/spellbook.
            val present = factions.%(_.at(r).%(u => u.uclass.utype != MapUnit && u.uclass != Cathedral).any)
            val totalUnitsPresent = present./~(_.at(r).%(u => u.uclass.utype != MapUnit && u.uclass != Cathedral)).num

            if (remaining <= 0 || totalUnitsPresent == 0)
                Force(CustodianResolveAgonyAction(self, r, assigned))
            else
                Ask(self).each(present)(f =>
                    CustodianAssignToFactionAction(self, r, remaining, assigned, f))

        case CustodianAssignToFactionAction(self, r, remaining, assigned, target) =>
            val newAssigned = assigned.agonyAdd(target, 1)
            Force(CustodianAssignAgonyAction(self, r, remaining - 1, newAssigned))

        // ── CUSTODIAN AGONY RESOLUTION — units to Oubliette ──
        case CustodianResolveAgonyAction(self, r, assigned) =>
            val next = assigned.find(_.n > 0)
            next match {
                case None => EndAction(self)
                case Some(Offer(target, agony)) =>
                    val unitsInRegion = target.at(r).%(u => u.uclass.utype != MapUnit && u.uclass != Cathedral)
                    if (unitsInRegion.none)
                        Force(CustodianResolveAgonyAction(self, r, assigned.agonyClear(target)))
                    else
                        Ask(target).each(unitsInRegion)(u =>
                            CustodianMoveToOublietteAction(self, r, target, u.ref, assigned.agonySet(target, agony - 1)))
            }

        case CustodianMoveToOublietteAction(self, r, target, uRef, remaining) =>
            val u = game.unit(uRef)
            val oubliette = game.board.regions.%(_.name == "Oubliette").head
            // Parallel-guide Fix 38: Custodian forced move must NOT trigger FB Cyclopean Gaze.
            game.fbSuppressCGForPlacement = true
            u.region = oubliette
            game.fbSuppressCGForPlacement = false
            u.onGate = false
            target.log(u.uclass.styled(target), "moved to", oubliette, "(" + "Custodian".styled("lb") + ")")
            // Chronophage: the moved unit belongs to `target` (not the activator `self`), so
            // offer `target` its free Hound teleport. No-op unless `target` owns the Hound card.
            Force(CronophageAfterMoveAction(target, CustodianResolveAgonyAction(self, r, remaining)))

        // ── LIBRARIAN ACTIVATION ──
        case SpendOnLibrarianAction(self) =>
            game.silenceTokens = game.silenceTokens + (self -> (game.silenceTokens.getOrElse(self, 0) - 1))
            self.log("spent a", "Silence Token".styled("lb"), "to activate the", "Librarian".styled("lb"))

            val currentRegion = game.librarianRegion

            val overdueHolders = factions.but(self).%(f =>
                game.tomeOverdue.exists { case (tome, overdue) => overdue && game.tomeHolders.get(tome).flatten.has(f) })
            // Librarian moves toward a tome-holder with UNITS on the map. A still-Building AN
            // Cathedral is not a Unit (AN.figureIsUnit is Holy-Ground/Action-Phase-aware), so
            // it does not attract the Librarian on its own.
            val validRegions = game.board.regions.%(_.glyph.onMap).%(r =>
                overdueHolders.exists(_.at(r).%(u => u.uclass.utype != MapUnit && AN.figureIsUnit(u)).any))

            val stayActions = currentRegion./(r => LibrarianStayAction(self, r)).$
            val moveActions = validRegions.%(r => !currentRegion.has(r))./( r =>
                LibrarianMoveAction(self, r))
            val allActions = stayActions ++ moveActions

            // Offer guard at Game.scala:1622 only checks that *some* faction has an
            // overdue tome; it does not check that the holder has non-MapUnit units
            // on the map for the Librarian to actually visit. If the bot picks
            // SpendOnLibrarianAction and no valid move/stay action exists, `Ask` would
            // produce an empty action list and crash BotX.askE with `head of empty list`.
            // End the action cleanly in that case.
            if (allActions.isEmpty)
                EndAction(self)
            else
                Ask(self).each(allActions)(identity)

        case LibrarianMoveAction(self, r) =>
            game.librarianRegion = |(r)
            self.log("moved", "Librarian".styled("lb"), "to", r)
            RollAgony(_ => "Roll Agony die for Librarian", x => LibrarianAgonyRolledAction(self, r, x, false))

        case LibrarianStayAction(self, r) =>
            self.log("Librarian".styled("lb"), "stays in", r, "(+1 to Agony roll)")
            RollAgony(_ => "Roll Agony die for Librarian (+1 for staying)", x => LibrarianAgonyRolledAction(self, r, x, true))

        // ── LIBRARIAN AGONY DISTRIBUTION ──
        // Step 1: pick a faction (or reset). Eligible = ANY other faction with
        // units in the librarian's region — overdue-tome status is NOT a filter
        // (the activator may target any faction with units; that target then
        // satisfies the agony however they can — eliminate units, lose doom, OR
        // return an overdue tome if they hold one). If only one faction is
        // eligible AND nothing is assigned yet, skip the chooser.
        // ── LIBRARIAN AGONY ROLL (recorded so replay/online shows the roll) ──
        case LibrarianAgonyRolledAction(self, r, rolled, staying) =>
            if (staying) {
                val total = rolled + 1
                log("Agony Die".styled("lb"), "rolled", rolled.toString.styled("power"), "+ 1 for staying =", total.toString.styled("power"), "total")
                Force(LibrarianAssignAgonyAction(self, r, total, total, $, $))
            }
            else {
                log("Agony Die".styled("lb"), "rolled", rolled.toString.styled("power"))
                Force(LibrarianAssignAgonyAction(self, r, rolled, rolled, $, $))
            }

        case LibrarianAssignAgonyAction(self, r, total, remaining, assigned, order) =>
            val eligibleTargets = factions.but(self).%(f =>
                f.at(r).%(u => u.uclass.utype != MapUnit).any)

            if (remaining <= 0 || eligibleTargets.none)
                Force(LibrarianResolveAgonyAction(self, assigned, order))
            else if (eligibleTargets.num == 1 && assigned.isEmpty)
                Force(LibrarianAssignAmountAction(self, r, total, remaining, assigned, order, eligibleTargets.head, remaining))
            else {
                val base = Ask(self).each(eligibleTargets)(f =>
                    LibrarianAssignToFactionAction(self, r, total, remaining, assigned, order, f))
                if (assigned.agonyTotal > 0)
                    base.add(LibrarianResetAgonyAction(self, r, total))
                else
                    base
            }

        // Step 2: pick how many agony to assign to the chosen target. Cancel
        // returns to step 1 with no change.
        case LibrarianAssignToFactionAction(self, r, total, remaining, assigned, order, target) =>
            Ask(self).each((1 to remaining).toList)(n =>
                LibrarianAssignAmountAction(self, r, total, remaining, assigned, order, target, n)
            ).add(LibrarianAssignCancelAction(self, r, total, remaining, assigned, order))

        case LibrarianAssignAmountAction(self, r, total, remaining, assigned, order, target, amount) =>
            val newAssigned = assigned.agonyAdd(target, amount)
            val newOrder = if (order.has(target)) order else order :+ target
            self.log("assigned", amount.toString.styled("power"), "Agony".styled("lb"), "to", target.full)
            Force(LibrarianAssignAgonyAction(self, r, total, remaining - amount, newAssigned, newOrder))

        case LibrarianAssignCancelAction(self, r, total, remaining, assigned, order) =>
            Force(LibrarianAssignAgonyAction(self, r, total, remaining, assigned, order))

        case LibrarianResetAgonyAction(self, r, total) =>
            self.log("reset", "Agony".styled("lb"), "assignments")
            Force(LibrarianAssignAgonyAction(self, r, total, total, $, $))

        // ── LIBRARIAN AGONY RESOLUTION — victim chooses ──
        // Walk factions in the activator's selection order.
        case LibrarianResolveAgonyAction(self, assigned, order) =>
            order.find(f => assigned.agonyFor(f) > 0) match {
                case None => EndAction(self)
                case Some(target) =>
                    val agony = assigned.agonyFor(target)
                    Force(LibrarianSatisfyAgonyAction(target, agony, assigned.agonySet(target, 0), self, order))
            }

        case LibrarianSatisfyAgonyAction(target, agony, remaining, activator, order) =>
            if (agony <= 0)
                Force(LibrarianResolveAgonyAction(activator, remaining, order))
            else {
                val hasUnits = game.librarianRegion./(r => target.at(r).%(u => u.uclass.utype != MapUnit).any).|(false)
                val hasDoom = target.doom > 0
                val hasOverdueTomes = game.tomeOverdue.exists { case (tome, overdue) =>
                    overdue && game.tomeHolders.get(tome).flatten.has(target) }

                if (!hasUnits && !hasDoom && !hasOverdueTomes)
                    Force(LibrarianSatisfyAgonyAction(target, 0, remaining, activator, order))
                else
                    Ask(target)
                        .when(hasUnits)(LibrarianEliminateUnitMainAction(target, agony, remaining, activator, order))
                        .when(hasOverdueTomes)(LibrarianReturnTomeMainAction(target, agony, remaining, activator, order))
                        .when(hasDoom)(LibrarianLoseDoomAction(target, agony, remaining, activator, order))
            }

        case LibrarianEliminateUnitMainAction(self, agony, remaining, activator, order) =>
            val libRegion = game.librarianRegion.get
            val units = self.at(libRegion).%(u => u.uclass.utype != MapUnit)
            if (units.none)
                Force(LibrarianEliminateDoneAction(self, agony, remaining, activator, order, $))
            else
                Force(LibrarianEliminateRegionAction(self, libRegion, agony, remaining, activator, order, $))

        case LibrarianEliminateRegionAction(self, r, agony, remaining, activator, order, eliminated) =>
            // Step 2: choose units in the selected region (with cancel/done)
            val units = self.at(r).%(u => u.uclass.utype != MapUnit && !eliminated.has(u.ref))
            if (units.none || agony <= 0)
                Force(LibrarianEliminateDoneAction(self, agony, remaining, activator, order, eliminated))
            else {
                val q = self.full + " " + "Satisfy Agony".styled("lb") + " - eliminate units in " + r
                Ask(self).each(units)(u =>
                    LibrarianEliminateUnitAction(self, u.ref, r, agony, remaining, activator, order, eliminated)
                ).add(LibrarianEliminateDoneAction(self, agony, remaining, activator, order, eliminated).as("Done")(q))
            }

        case LibrarianEliminateUnitAction(self, uRef, r, agony, remaining, activator, order, eliminated) =>
            // Don't eliminate yet - just mark as selected, reduce agony counter
            Force(LibrarianEliminateRegionAction(self, r, agony - 1, remaining, activator, order, eliminated :+ uRef))

        case LibrarianEliminateDoneAction(self, agony, remaining, activator, order, eliminated) =>
            // Now actually eliminate all selected units
            eliminated.foreach { ref =>
                val u = game.unit(ref)
                val r = u.region
                game.eliminate(u)
                self.log("eliminated", u.uclass.styled(self), "in", r, "to satisfy", "Agony".styled("lb"))
            }
            // 2026-05-11 loop-break: if the bot reaches Done with no eliminations,
            // it picked into Eliminate path but then refused every unit (Bot3 scoring
            // can rate every available unit more negatively than the empty-Done -100).
            // Without this guard, SatisfyAgony re-offers Eliminate, same outcome,
            // and the game enters a 7000-action `ControlGate`/`Eliminate` loop that
            // the SimRunner watchdog has to kill. Treat empty-Done as "waive the
            // current agony" so the satisfy loop exits.
            val newAgony = if (eliminated.isEmpty) 0 else agony
            Force(LibrarianSatisfyAgonyAction(self, newAgony, remaining, activator, order))


        case LibrarianReturnTomeMainAction(self, agony, remaining, activator, order) =>
            val overdueTomes = $(TomeBarrier, TomeGuardian, TomeLarvae, TomeYr).%(tome =>
                game.tomeOverdue.getOrElse(tome, false) && game.tomeHolders.get(tome).flatten.has(self))
            if (overdueTomes.num == 1)
                Force(LibrarianReturnTomeAction(self, overdueTomes.head, agony, remaining, activator, order))
            else
                Ask(self).each(overdueTomes)(t =>
                    LibrarianReturnTomeAction(self, t, agony, remaining, activator, order))

        case LibrarianReturnTomeAction(self, tome, agony, remaining, activator, order) =>
            game.tomeHolders = game.tomeHolders + (tome -> None)
            game.tomeOverdue = game.tomeOverdue + (tome -> false)
            self.log("returned", tome.elem, "to satisfy", "Agony".styled("lb"))
            Force(LibrarianSatisfyAgonyAction(self, agony - 1, remaining, activator, order))

        case LibrarianLoseDoomAction(self, agony, remaining, activator, order) =>
            self.doom -= 1
            self.log("lost", 1.doom, "to satisfy", "Agony".styled("lb"))
            Force(LibrarianSatisfyAgonyAction(self, agony - 1, remaining, activator, order))

        // ── TOME FLIP VIA SILENCE TOKEN ──
        case SpendToFlipTomeAction(self, tome) =>
            game.silenceTokens = game.silenceTokens + (self -> (game.silenceTokens.getOrElse(self, 0) - 1))
            game.tomeFaceUp = game.tomeFaceUp + (tome -> true)
            self.log("spent a", "Silence Token".styled("lb"), "to flip", tome.elem, "face-up")
            EndAction(self)

        // ── BARRIER OF NAACH-TITH — battle gate cost ──
        // BarrierMenuOpenAction is the variant clicked from AttackMainAction when the target
        // faction holds the barrier. Soft → showing this menu doesn't overwrite the cancel
        // snapshot, so the Cancel button correctly returns to the parent picker.
        case BarrierMenuOpenAction(self, r, f, effect) =>
            // Captive cultists held BY the attacker, indexed by their original owner.
            val captiveFactions = factions.but(self)
                .%(ff => ff.at(self.prison).%(_.uclass.utype == Cultist).any)
            val hasES = self.es.any
            val hasToken = game.silenceTokens.getOrElse(self, 0) > 0

            val q = "Barrier of Naach-Tith battle payment"
            val then : Action = AttackAction(self, r, f, effect)
            val cancelBack = BarrierCancelToAttackMainAction(self, r, effect).as("Cancel")

            if (captiveFactions.none && !hasES && !hasToken) {
                Ask(self)
                    .group(q + " — No available payment")
                    .add(cancelBack)
            }
            else {
                Ask(self)
                    .each(captiveFactions)(ff =>
                        BarrierReleaseCultistFactionAction(self, ff, then)
                            .as("Release captured ", ff.full, " cultist")(q))
                    .when(hasES)(BarrierDiscardESAction(self, then)
                        .as("Discard 1 ", "Elder Sign".styled("es"))(q))
                    .when(hasToken)(BarrierDiscardTokenAction(self, then)
                        .as("Discard 1 ", "Silence Token".styled("lb"))(q))
                    .add(cancelBack)
            }

        // Soft bridge for the Barrier menu's Cancel button — re-issues the parent
        // battle target picker.
        case BarrierCancelToAttackMainAction(self, r, effect) =>
            Force(AttackMainAction(self, $(r), effect))

        // Legacy AttackBarrierCheckAction (no-op forward).
        case AttackBarrierCheckAction(self, r, f, effect) =>
            Force(AttackAction(self, r, f, effect))

        // Legacy BarrierCheckAction — preserved for any older bot-scoring paths but
        // no longer triggered from AttackAction. Same logic as the wrapper above, with
        // a back-to-main fallback for the cancel path.
        case BarrierCheckAction(attacker, defender, then) =>
            val captiveFactions = factions.but(attacker)
                .%(ff => ff.at(attacker.prison).%(_.uclass.utype == Cultist).any)
            val hasES = attacker.es.any
            val hasToken = game.board.isLibraryMap && game.silenceTokens.getOrElse(attacker, 0) > 0

            val q = "Pay the " + TomeBarrier.elem + " Battle Cost."

            if (captiveFactions.none && !hasES && !hasToken) {
                Ask(attacker)
                    .group(q + " — No available payment")
                    .cancel
            }
            else {
                Ask(attacker)
                    .each(captiveFactions)(f =>
                        BarrierReleaseCultistFactionAction(attacker, f, then)
                            .as("Release captured " + f.full + " cultist")(q))
                    .when(hasES)(BarrierDiscardESAction(attacker, then)
                        .as("Discard 1 " + "Elder Sign".styled("es"))(q))
                    .when(hasToken)(BarrierDiscardTokenAction(attacker, then)
                        .as("Discard 1 " + "Silence Token".styled("lb"))(q))
                    .cancel
            }

        case BarrierReleaseCultistFactionAction(self, captiveFaction, then) =>
            // captiveFaction's cultist currently held in self's prison.
            val u = captiveFaction.at(self.prison).%(_.uclass.utype == Cultist).head
            u.region = captiveFaction.reserve
            u.onGate = false
            log(captiveFaction.full, "cultist was returned")
            // Mark barrier paid so AttackAction's safety-net check doesn't re-trigger.
            game.barrierPaid = true
            Force(then)

        case BarrierDiscardESAction(self, then) =>
            val es = self.es.head
            self.es = self.es.but(es)
            log("Elder Sign".styled("es"), "was discarded")
            game.barrierPaid = true
            Force(then)

        case BarrierDiscardTokenAction(self, then) =>
            game.silenceTokens = game.silenceTokens + (self -> (game.silenceTokens.getOrElse(self, 0) - 1))
            log("Silence Token".styled("lb"), "was discarded")
            game.barrierPaid = true
            Force(then)

        case BarrierBlockedAction(self) =>
            self.log("attempted to battle.", "Battle was blocked by", TomeBarrier.elem)
            EndAction(self)

        // ── GUARDIAN UNDER THE LAKE — move enemy units between Archway regions ──
        case UseTomeGuardianMainAction(self) =>
            val arches = game.board.regions.%(game.board.archways.contains)
            // Guardian forcibly MOVES enemy units between Archways. An AN Cathedral can never
            // be moved by any means (Building or Holy-Ground GOO alike), so it is excluded from
            // both the eligible-region scan and the actual relocation below.
            val regionsWithEnemies = arches.%(r =>
                factions.but(self).%(_.at(r).%(u => u.uclass.utype != MapUnit && u.uclass != Cathedral).any).any)
            Ask(self).each(regionsWithEnemies)(r =>
                UseTomeGuardianRegionAction(self, r)).cancel

        case UseTomeGuardianRegionAction(self, source) =>
            val enemies = factions.but(self).%(_.at(source).%(u => u.uclass.utype != MapUnit && u.uclass != Cathedral).any)
            if (enemies.num == 1)
                Force(UseTomeGuardianRelocateAction(self, source, enemies.head))
            else
                Ask(self).each(enemies)(f =>
                    UseTomeGuardianRelocateAction(self, source, f)).cancel

        case UseTomeGuardianRelocateAction(self, source, target) =>
            val arches = game.board.regions.%(game.board.archways.contains).but(source)
            Ask(self).each(arches)(dest =>
                UseTomeGuardianDestAction(self, source, target, dest)).cancel

        case UseTomeGuardianDestAction(self, source, target, dest) =>
            self.payPower(1, "on Guardian under the Lake")
            game.tomeFaceUp = game.tomeFaceUp + (TomeGuardian -> false)
            // Parallel-guide Fix 40: Library Guardian tome forcibly relocates enemy units.
            // This must NOT trigger FB Cyclopean Gaze.
            game.fbSuppressCGForPlacement = true
            target.at(source).%(u => u.uclass.utype != MapUnit && u.uclass != Cathedral).foreach { u =>
                u.region = dest
                u.onGate = false
            }
            game.fbSuppressCGForPlacement = false
            self.log("used", TomeGuardian.elem, "to move", target.full, "units from", source, "to", dest)
            EndAction(self)

        // ── LARVAE OF THE OUTER GODS — gain ES ──
        case UseTomeLarvaeAction(self) =>
            self.payPower(1, "on Larvae of the Outer Gods")
            game.tomeFaceUp = game.tomeFaceUp + (TomeLarvae -> false)
            if (factions.but(self).exists(_.power > self.power)) {
                self.takeES(1)
                self.log("used", TomeLarvae.elem, "and gained", 1.es)
            } else {
                self.log("used", TomeLarvae.elem, "(no opponent has more Power — no ES gained)")
            }
            EndAction(self)

        // ── YR AND THE NHHNGR ──
        case UseTomeYrMainAction(self) =>
            Ask(self)
                .when(self.pool.%(_.uclass.utype == Monster).any && self.gates.onMapOrMoon.any)(
                    UseTomeYrMonsterAction(self))
                .add(UseTomeYrPowerAction(self))
                .cancel

        case UseTomeYrMonsterAction(self) =>
            val monsters = self.pool.%(_.uclass.utype == Monster)./(_.uclass).distinct
            val gates = self.gates.onMapOrMoon
            Ask(self).each(monsters./~(uc => gates./(r => UseTomeYrMonsterChooseAction(self, uc, r))))(identity).cancel

        case UseTomeYrMonsterChooseAction(self, uc, r) =>
            self.payPower(1, "on Yr and the Nhhngr")
            game.tomeFaceUp = game.tomeFaceUp + (TomeYr -> false)
            val u = self.pool.%(_.uclass == uc).head
            u.region = r
            self.log("used", TomeYr.elem, "to place", uc.styled(self), "in", r)
            EndAction(self)

        case UseTomeYrPowerAction(self) =>
            self.payPower(1, "on Yr and the Nhhngr")
            game.tomeFaceUp = game.tomeFaceUp + (TomeYr -> false)
            self.power += 2 // net +1
            self.log("used", TomeYr.elem, "and gained", 2.power)
            EndAction(self)

        // ── FLIPPABLE TOME FLIP-UP (unlimited actions, shared by Guardian/Larvae/Yr) ──
        case FlipTomeReleaseCultistMainAction(self, tome) =>
            val captured = factions./~(f => f.at(f.prison).%(_.uclass.utype == Cultist))
            Ask(self).each(captured)(u =>
                FlipTomeReleaseCultistAction(self, tome, u.ref)
                    .as(u.full, "of", u.faction)("Release a Captured Cultist")).cancel

        case FlipTomeReleaseCultistAction(self, tome, uRef) =>
            val u = game.unit(uRef)
            val owner = u.faction
            u.region = owner.reserve
            u.onGate = false
            game.tomeFaceUp = game.tomeFaceUp + (tome -> true)
            self.log("released", u.uclass.styled(owner), "to flip", tome.elem, "face-up")
            Force(MainAction(self))

        case FlipTomeDiscardESAction(self, tome) =>
            val es = self.es.head
            self.es = self.es.but(es)
            game.tomeFaceUp = game.tomeFaceUp + (tome -> true)
            self.log("discarded", es.short, "to flip", tome.elem, "face-up")
            Force(MainAction(self))

        case FlipTomeDiscardTokenAction(self, tome) =>
            game.silenceTokens = game.silenceTokens + (self -> (game.silenceTokens.getOrElse(self, 0) - 1))
            game.tomeFaceUp = game.tomeFaceUp + (tome -> true)
            self.log("discarded a", "Silence Token".styled("lb"), "to flip", tome.elem, "face-up")
            Force(MainAction(self))

        case _ => UnknownContinue
    }

    def checkTomeAcquisition()(implicit game : Game) {
        $(TomeBarrier, TomeGuardian, TomeLarvae, TomeYr).foreach { tome =>
            if (game.board.regions.has(tome.region)) {
                val gateOwner = factions.find(_.gates.has(tome.region))
                val currentHolder = game.tomeHolders.getOrElse(tome, None)
                gateOwner match {
                    // Initial acquisition: tome is unheld and a faction now controls
                    // its gate. They take the tome.
                    case Some(f) if currentHolder.isEmpty =>
                        game.tomeHolders = game.tomeHolders + (tome -> |(f))
                        game.tomeOverdue = game.tomeOverdue + (tome -> false)
                        log(f, "acquired", tome.elem)
                    // Holder still controls the tome's gate — clear overdue if set.
                    case Some(f) if currentHolder.has(f) && game.tomeOverdue.getOrElse(tome, false) =>
                        game.tomeOverdue = game.tomeOverdue + (tome -> false)
                        log(f, tome.elem, "is no longer", "Overdue".styled("lb"))
                    // Holder no longer controls the gate (uncontrolled OR another
                    // faction controls it). Tome stays with the original holder —
                    // it is only released via the Librarian "return cursed tome"
                    // action — but it is now Overdue so the Librarian can target it.
                    case _ if currentHolder.isDefined && !gateOwner.has(currentHolder.get) =>
                        if (!game.tomeOverdue.getOrElse(tome, false)) {
                            game.tomeOverdue = game.tomeOverdue + (tome -> true)
                            log(currentHolder.get, tome.elem, "is now", "Overdue".styled("lb"))
                        }
                    case _ =>
                }
            }
        }
    }

    override def afterAction()(implicit game : Game) {
        if (!game.board.isLibraryMap) return
        checkTomeAcquisition()
    }

    override def eliminate(u : UnitFigure)(implicit game : Game) {
        if (u.uclass == TheCustodian) game.custodianRegion = None
        if (u.uclass == TheLibrarian) game.librarianRegion = None
    }
}

// ══════════════════════════════════════════════════════════════════════════════
// YUGGOTH MAP — Phase 2: THE WATCHER
// A neutral Great Old One tied to the shared Doom Track, not owned by any Faction.
// Rules sourced from the Yuggoth Map Creation Guide §1.7-1.9 as CORRECTED by its
// own §1.12 issues list (I1-I6), which the guide's author flags as authoritative
// over the guide's own earlier (wrong) §1.9/§2.0/§2.4 text and over the older
// "Phases 2-5 Architecture Spec.md" draft:
//   I1/I4 — immune to ALL Pain (a Pain landing on it is simply wasted — it is NOT
//           converted to a Kill, unlike the old guide/spec text). Only an actual
//           Kill result moves its Token down 1 on the (shared) Doom Track.
//   I2    — the Watcher Token sits on the real Doom Track at position 12. Combat
//           = current Token position. It Awakens on EITHER of two triggers,
//           checked at Doom Phase start (deferred if the threshold trigger fires
//           mid-Action-Phase, per the verbatim §1.7 text): (a) a 1d6 roll matches
//           the printed number of a Green-Pyramid-family Area (incl. The
//           Watcher's Postern) that currently has a Controlled Gate or
//           Yog-Sothoth present, or (b) any Faction's Doom total reaches/passes
//           12. Either way it is placed at The Watcher's Postern.
//   I3    — while Awake it Attacks once per player each Action Phase (First
//           Player first, then turn order) before anyone's Main Phase. Each
//           Attack: the controller MUST move it (never stays put) to an
//           adjacent Area with any player's Units, or teleports to any Area
//           with Units if none are adjacent, then it Battles each Faction
//           present there SEPARATELY (controller's choice of order) — all of
//           those sub-battles count as one Attack. It never attacks Slime
//           Molds (out of scope this phase — no Slime Molds exist yet). Any
//           Faction may ALSO voluntarily start a normal Battle against it later
//           in the Action Phase (free: it is simply a legal `factionlike`
//           Battle target wherever it stands — no extra menu code needed).
//   I4    — costs 0 Power/Doom for Tsathoggua/Ithaqua/Nyarlathotep math. Counts
//           as a Great Old One for all purposes, so Nyarlathotep's existing
//           Harbinger ("2 Elder Signs for a Kill on an enemy GOO") mechanic
//           fires on it automatically (TheWatcherUnit.utype = GOO) — no bespoke
//           Nyarlathotep code is needed; Harbinger's own non-stacking `Harbinged`
//           tag already matches "do not stack even with multiple Kills."
//   I5    — when the Token reaches 0 it is removed and resets Asleep at 12. It
//           never re-rolls the Awaken check on the same Doom Phase it died, nor
//           decays further the Phase it Awakens (both naturally true here since
//           there is no separate per-Doom-Phase decay roll at all — I1 replaced
//           that old-guide/spec mechanic with pure Kill-driven reduction).
//   I6    — the board's Pyramid "+1" glyph is a +1 POWER symbol (Gather Power
//           bonus), NOT a die modifier — and that passive bonus is explicitly
//           its own later phase (Phase 5 "Pyramid Power" per the architecture
//           spec's phase tracker), so it is NOT built here to avoid scope creep.
//
// RULING ON A GENUINE AMBIGUITY (flagged per task instructions, not blocking):
// the "controller" of each forced Attack only directs its Move target and the
// order of its sub-battles — the Watcher itself is still the actual Battle
// participant (attacker = TheWatcherFaction) so results against it follow the
// corrected Pain/Kill rule regardless of who is "controlling" it that turn.
// ══════════════════════════════════════════════════════════════════════════════

// ── MAP UNIT — a true Great Old One (utype GOO), not MapUnitClass (which is
// non-combat by construction — Custodian/Librarian never fight). Combat is NOT
// a fixed field; every combat-value read goes through TheWatcherFaction.strength
// below, which looks up the live game.watcherPosition at Battle time. ──
case object TheWatcherUnit extends UnitClass("The Watcher", GOO, 0) {
    // Its own forced Move (WatcherMoveAction, below) mutates .region directly,
    // bypassing this gate entirely — same idiom LibraryExpansion uses for the
    // Custodian/Librarian. canMove/canBeMoved being false here only blocks OTHER
    // factions' generic Move menus and forced-relocation effects from touching it
    // (I3: "Windwalker's Howl cannot send the Watcher out of the Battle Area").
    override def canMove(u : UnitFigure)(implicit game : Game) : Boolean = false
    override def canBeMoved(u : UnitFigure)(implicit game : Game) : Boolean = false
    override def canBattle(u : UnitFigure)(implicit game : Game) : Boolean = true
    override def canCapture(u : UnitFigure)(implicit game : Game) : Boolean = false
    override def canBeCaptured(u : UnitFigure)(implicit game : Game) : Boolean = false
    override def canControlGate(u : UnitFigure)(implicit game : Game) : Boolean = false
    override def canBeRecruited(f : Faction)(implicit game : Game) : Boolean = false
    override def canBeSummoned(f : Faction)(implicit game : Game) : Boolean = false
}

// ── NEUTRAL MAP FACTION — pure bookkeeping owner for the single Watcher figure,
// same idiom as LibraryFaction/NeutralAbhoth. Registered permanently into
// game.neutrals at Game construction (see Game.scala) whenever the board is a
// Yuggoth map, so `factionlike` (and therefore every generic Battle/Capture
// target scan already in the codebase) sees it automatically — this is exactly
// how I3's "any player may also voluntarily Battle it" falls out for free. ──
case object TheWatcherFaction extends NeutralFaction {
    def name = "The Watcher"
    def short = "WT"
    def style = "nt"

    override def abilities = $
    override def library = $
    override def requirements(options : $[GameOption]) = $

    val allUnits = $(TheWatcherUnit)

    // neutralStrength covers every OTHER neutral/monster unit class (all zero
    // here, since TheWatcherFaction only ever holds TheWatcherUnit) — included
    // only for consistency with every other Faction.strength implementation.
    // The Watcher's actual Combat is its live Doom-Track position.
    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int =
        neutralStrength(units, opponent) + units(TheWatcherUnit).not(Zeroed).num * game.watcherPosition
}

// ── ACTIONS ──

// Doom Phase hook (Hard chain; see Game.scala's DoomPhaseStartAction).
case class WatcherDoomPhaseCheckAction(then : Action) extends ForcedAction
case class WatcherAwakenRollAction(roll : Int, then : Action) extends ForcedAction
case class WatcherAwakenAction(then : Action) extends ForcedAction

// Oct 2026: Decay actions (roll die, reduce position, eliminate at 0)
case class WatcherDecayCheckAction(then : Action) extends ForcedAction
case class WatcherDecayRollAction(roll : Int, then : Action) extends ForcedAction

// Action Phase forced-attack-sequence hook (see Game.scala's ActionPhaseAction).
case class WatcherAttackPhaseStartAction(then : Action) extends ForcedAction
case class WatcherAttackTurnAction(then : Action) extends ForcedAction
case class WatcherMoveAction(controller : Faction, dest : Region, then : Action) extends BaseFactionAction(implicit g => "Move " + "The Watcher".styled("nt") + " to", dest) {
    def self = controller
}
case class WatcherBattleOrderAction(controller : Faction, dest : Region, then : Action) extends ForcedAction
case class WatcherBattleChooseAction(controller : Faction, dest : Region, target : Faction, then : Action) extends BaseFactionAction(implicit g => "The Watcher".styled("nt") + " attacks in " + dest, target) {
    def self = controller
}
case class WatcherBattleStartAction(controller : Faction, dest : Region, target : Faction, then : Action) extends ForcedAction

// ══════════════════════════════════════════════════════════════════════════════
// YUGGOTH MAP — Phase 3: SLIME MOLDS
// Controlled neutral Monsters. Allegiance determined by who Controls the Slime
// Sea Overlook gate. When gate control changes, ALL Slime Molds switch to the
// new controller. When the gate is Abandoned, they become neutral (no controller).
// ══════════════════════════════════════════════════════════════════════════════

// Slime Molds are actual Monster units (Combat 2), not MapUnit tokens. They
// can Move, Battle, be Captured, etc. Unlike normal faction units, they have
// a dynamic controller (whoever holds the Slime Sea Overlook gate). State is
// tracked via SlimeMoldRef tuples on game.slimeMolds (see Game.scala).
case object SlimeMoldMonster extends UnitClass("Slime Mold", Monster, 1) {
    // Slime Molds can Move, but NOT to Slime Sea Overlook (restriction).
    override def canMove(u : UnitFigure)(implicit game : Game) : Boolean = true
    override def canBeMoved(u : UnitFigure)(implicit game : Game) : Boolean = true
    override def canBattle(u : UnitFigure)(implicit game : Game) : Boolean = true
    override def canCapture(u : UnitFigure)(implicit game : Game) : Boolean = true
    override def canBeCaptured(u : UnitFigure)(implicit game : Game) : Boolean = true
    override def canControlGate(u : UnitFigure)(implicit game : Game) : Boolean = false
    override def canBeRecruited(f : Faction)(implicit game : Game) : Boolean = false
    override def canBeSummoned(f : Faction)(implicit game : Game) : Boolean = false
}

// Neutral faction for Slime Molds. Similar to TheWatcherFaction, but Slime Molds
// are Monsters (Combat 2) and can be controlled by player factions.
case object SlimeMoldFaction extends NeutralFaction {
    def name = "Slime Molds"
    def short = "SM"
    def style = "nt"

    override def abilities = $
    override def library = $
    override def requirements(options : $[GameOption]) = $

    val allUnits = $(SlimeMoldMonster)

    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int =
        neutralStrength(units, opponent) + units(SlimeMoldMonster).not(Zeroed).num * 2
}

// ── PHASE 3 ACTIONS ──
case class CallSlimeMoldMainAction(self : Faction) extends OptionFactionAction(implicit g => {
    val cost = if (g.slimeMolds.isEmpty) 0 else 1
    "Call Slime Mold (" + cost + " Power)"
}) with MainQuestion with Soft

case class CallSlimeMoldChooseAction(self : Faction, r : Region) extends BaseFactionAction(implicit g => "Place Slime Mold in", r)

// ══════════════════════════════════════════════════════════════════════════════
// YUGGOTH MAP — Phase 4: SURGERY & BRAIN CYLINDERS
// Brain Cylinders are a Cultist subtype (Combat 0, cannot Move independently,
// can Control/Abandon Gates, generate Power). Created via Surgery action at the
// Laboratory. Can be carried by any moving unit, captured, and converted.
// ══════════════════════════════════════════════════════════════════════════════

// Brain Cylinders are Cultists (NOT Acolytes). They can Control Gates and
// generate Power, but cannot Move independently (must be carried). Immune to
// Pain in Battle (Kills only). Do NOT block enemy units being Pained into their area.
case object BrainCylinderCultist extends UnitClass("Brain Cylinder", Cultist, 0) {
    override def canMove(u : UnitFigure)(implicit game : Game) : Boolean = false
    override def canBeMoved(u : UnitFigure)(implicit game : Game) : Boolean = true
    override def canBattle(u : UnitFigure)(implicit game : Game) : Boolean = true
    override def canCapture(u : UnitFigure)(implicit game : Game) : Boolean = true
    override def canBeCaptured(u : UnitFigure)(implicit game : Game) : Boolean = true
    override def canControlGate(u : UnitFigure)(implicit game : Game) : Boolean = true
    override def canBeRecruited(f : Faction)(implicit game : Game) : Boolean = false
    override def canBeSummoned(f : Faction)(implicit game : Game) : Boolean = false
}

// ── PHASE 4 ACTIONS ──
case class SurgeryMainAction(self : Faction) extends OptionFactionAction("Surgery (1 Power)") with MainQuestion with Soft
case class SurgeryChooseCultistsAction(self : Faction, selected : $[UnitRef]) extends ForcedAction with Soft
case class SurgeryToggleUnitAction(self : Faction, uRef : UnitRef, selected : $[UnitRef]) extends BaseFactionAction(implicit g => "Surgery — select Cultists to convert", implicit g => {
    val u = g.unit(uRef)
    val mark = if (selected.has(uRef)) " [X]" else " [ ]"
    u.full + mark
}) with Soft
case class SurgeryDoneAction(self : Faction, selected : $[UnitRef]) extends BaseFactionAction(implicit g => "Surgery — select Cultists to convert", "Done (convert " + selected.num + " Cultist" + (if (selected.num == 1) "" else "s") + ")")
case class SurgeryCancelAction(self : Faction) extends BaseFactionAction(implicit g => "Surgery — select Cultists to convert", "Cancel") with Soft
case class SurgeryPerformAction(self : Faction, cultists : $[UnitRef]) extends ForcedAction

object YuggothExpansion extends Expansion {

    // Green Pyramid Areas (incl. The Watcher's Postern) on the current board that
    // right now have a Controlled Gate OR Yog-Sothoth present, mapped to the set
    // of die faces that would Awaken the Watcher — union of all qualifying Areas'
    // printed numbers (board.watcherAwakenNumbers; see Map.scala per variant).
    def eligibleAwakenNumbers(implicit game : Game) : Set[Int] =
        game.board.greenPyramidRegions
            .%(r => factions.exists(_.gates.contains(r)) || factions.exists(_.at(r).exists(_.uclass == YogSothoth)))
            ./~(r => game.board.watcherAwakenNumbers.getOrElse(r, Set()).toList)
            .toSet

    def watcherUnit(implicit game : Game) : UnitFigure = TheWatcherFaction.units.head

    // "Any player's Units" for the forced-attack Move/target checks must mean real
    // combatants (unit.uclass.canBattle), not e.g. a still-Building Cathedral (The
    // Ancients, pre-Holy-Ground) sitting in the Area — otherwise a Cathedral-only Area
    // becomes an infinite loop: the Watcher picks that Faction as present, starts a
    // Battle, Battle.scala's own BattleStart phase exempts the Cathedral as a
    // non-combatant (same rule normal Attacks already respect), the Battle ends with
    // nothing resolved, and the Cathedral is STILL "present" on the next check — round
    // and round forever. canBattle is the same generic combatant gate the rest of the
    // engine already uses (also correctly excludes MapUnit tokens, mummified/kidnapped
    // units, etc.), so this one check covers Cathedral and any other such exemption.
    def combatPresent(f : Faction, r : Region)(implicit game : Game) : Boolean =
        f.at(r).exists(u => u.uclass.canBattle(u))

    def perform(action : Action, soft : VoidGuard)(implicit game : Game) : Continue = action @@ {

        // ── DOOM PHASE: AWAKEN CHECK (only while Asleep; no more per-Doom-Phase
        // decay roll exists — I1 replaced that old mechanic with pure Kill-driven
        // Token reduction during Battle) ──
        case WatcherDoomPhaseCheckAction(then) =>
            if (!game.board.isYuggothMap || !game.watcherAsleep)
                Force(then)
            // Oct 2026: detect 12-doom CROSSING (from <12 to >=12), not just >=12
            else if (game.watcherDoomCrossingPending || factions.exists(f => game.watcherPrevDoomValues.getOrElse(f, 0) < 12 && f.doom >= 12)) {
                log("The Watcher".styled("nt") + ": a Faction's Doom has crossed 12 —")
                game.watcherDoomCrossingPending = false
                Force(WatcherAwakenAction(then))
            }
            else {
                val eligible = eligibleAwakenNumbers
                if (eligible.none)
                    Force(then)
                else
                    RollD6(_ => "The Watcher — Awaken check (needs " + eligible.toList.sorted.mkString(" or ") + ")", roll => WatcherAwakenRollAction(roll, then))
            }

        case WatcherAwakenRollAction(roll, then) =>
            log("The Watcher".styled("nt") + ": Awaken roll —", roll.toString.styled("power"))
            if (eligibleAwakenNumbers.contains(roll))
                Force(WatcherAwakenAction(then))
            else {
                log("The Watcher".styled("nt") + ": remains Asleep")
                Force(then)
            }

        case WatcherAwakenAction(then) =>
            game.watcherAsleep = false
            game.watcherPosition = 12
            val postern = game.board.watcherPostern.|!("Yuggoth board missing watcherPostern")
            // Suppress Firstborn's Cyclopean Gaze "unit arrived in a gaze region" trigger —
            // the Watcher belongs to no Faction, so this placement must not be treated as
            // some Faction's own Action landing in the region (same idiom LibraryExpansion
            // uses for its own forced Custodian/Librarian moves).
            game.fbSuppressCGForPlacement = true
            watcherUnit.region = postern
            game.fbSuppressCGForPlacement = false
            watcherUnit.onGate = false
            log("The Watcher".styled("nt") + " Awakens! Placed at", postern, "— Combat", 12)
            game.watcherAwakenedThisDoom = true  // Skip decay this Doom Phase
            Force(then)

        // ── DECAY: END OF DOOM PHASE (after rituals, before Action Phase) ──
        case WatcherDecayCheckAction(then) =>
            if (!game.board.isYuggothMap || game.watcherAsleep || game.watcherAwakenedThisDoom) {
                game.watcherAwakenedThisDoom = false  // Reset for next Doom Phase
                Force(then)
            }
            else
                RollD6(_ => "The Watcher — Decay roll", roll => WatcherDecayRollAction(roll, then))

        case WatcherDecayRollAction(roll, then) =>
            val prev = game.watcherPosition
            val next = prev - roll
            log("The Watcher".styled("nt") + ": Decay roll —", roll.toString.styled("power"), "(Combat", prev, "→", math.max(0, next) + ")")
            game.watcherPosition = math.max(0, next)
            game.watcherAwakenedThisDoom = false  // Reset for next Doom Phase
            if (next <= 0) {
                log("The Watcher".styled("nt") + ": eliminated (Combat reached 0)")
                watcherUnit.region = null
                game.watcherAsleep = true
                game.watcherPosition = 12
                game.watcherAttackQueue = $
            }
            Force(then)

        // ── ACTION PHASE: FORCED ATTACK SEQUENCE (before anyone's Main Phase) ──
        case WatcherAttackPhaseStartAction(then) =>
            if (!game.board.isYuggothMap || game.watcherAsleep)
                Force(then)
            else {
                // `factions` is already reordered to turn order starting at the
                // First Player by the time ActionPhaseAction reaches this hook.
                game.watcherAttackQueue = game.factions
                Force(WatcherAttackTurnAction(then))
            }

        case WatcherAttackTurnAction(then) =>
            if (game.watcherAsleep || game.watcherAttackQueue.none) {
                game.watcherAttackQueue = $
                Force(then)
            }
            else {
                val controller = game.watcherAttackQueue.head
                game.watcherAttackQueue = game.watcherAttackQueue.tail

                val current = watcherUnit.region
                val adjacent = game.board.connected(current).%(r => game.factions.exists(f => combatPresent(f, r)))
                val options = if (adjacent.any) adjacent else game.board.regions.but(current).%(r => game.factions.exists(f => combatPresent(f, r)))

                if (options.none) {
                    controller.log("has no Area to Move", "The Watcher".styled("nt"), "to — its forced Attack is skipped")
                    Force(WatcherAttackTurnAction(then))
                }
                else
                    Ask(controller).each(options)(r => WatcherMoveAction(controller, r, then))
            }

        case WatcherMoveAction(controller, dest, then) =>
            // Suppress Cyclopean Gaze's arrival trigger here too — see WatcherAwakenAction.
            // Also critical for correctness, not just flavor: without this, Firstborn's Gaze
            // can grab the Watcher's own figure (as "actor" = the Watcher itself, since the
            // move was not a real Faction's Action) and relocate it out from under this
            // forced-Attack chain, desyncing arena vs. the figure's actual region — an
            // infinite-Battle loop risk (see git history for the reproduction).
            game.fbSuppressCGForPlacement = true
            watcherUnit.region = dest
            game.fbSuppressCGForPlacement = false
            watcherUnit.onGate = false
            controller.log("moved", "The Watcher".styled("nt"), "to", dest)
            Force(WatcherBattleOrderAction(controller, dest, then))

        case WatcherBattleOrderAction(controller, dest, then) =>
            if (game.watcherAsleep) {
                game.watcherAttackQueue = $
                Force(then)
            }
            else {
                val targets = game.factions.%(f => combatPresent(f, dest))
                if (targets.none)
                    Force(WatcherAttackTurnAction(then))
                else if (targets.num == 1)
                    Force(WatcherBattleStartAction(controller, dest, targets.head, then))
                else
                    Ask(controller).each(targets)(f => WatcherBattleChooseAction(controller, dest, f, then))
            }

        case WatcherBattleChooseAction(controller, dest, target, then) =>
            Force(WatcherBattleStartAction(controller, dest, target, then))

        case WatcherBattleStartAction(controller, dest, target, then) =>
            if (!combatPresent(target, dest))
                Force(WatcherBattleOrderAction(controller, dest, then))
            else {
                log("The Watcher".styled("nt") + " (Combat " + game.watcherPosition + ") attacks", target, "in", dest)
                game.watcherForcedBattleThen = |(WatcherBattleOrderAction(controller, dest, then))
                game.queue = $(new Battle(dest, TheWatcherFaction, target, None)) ++ game.queue
                Force(ProceedBattlesAction)
            }

        // ══════════════════════════════════════════════════════════════════════════════
        // PHASE 3: SLIME MOLDS
        // ══════════════════════════════════════════════════════════════════════════════

        case CallSlimeMoldMainAction(self) =>
            val slimeSeaAreas = game.board.regions.%(r => r.name.endsWith("Slime Sea"))
            Ask(self).each(slimeSeaAreas)(r => CallSlimeMoldChooseAction(self, r)).cancel

        case CallSlimeMoldChooseAction(self, r) =>
            val cost = if (game.slimeMolds.isEmpty) 0 else 1
            self.payPower(cost, "to Call Slime Mold")

            // Find controller: whoever holds Slime Sea Overlook gate, or Yog if present
            val overlook = game.board.regions.find(_.name == "Slime Sea Overlook")
            val controller = overlook.flatMap(ov => {
                factions.find(_.gates.contains(ov)).orElse(
                    if (factions.exists(f => f.at(ov).exists(_.uclass == YogSothoth))) |(self) else None
                )
            })

            // Create new Slime Mold
            val nextId = if (game.slimeMolds.isEmpty) 1 else game.slimeMolds./(_.id).max + 1
            game.slimeMolds = game.slimeMolds :+ SlimeMoldRef(nextId, r, controller)

            self.log("called", "Slime Mold".styled("nt"), "in", r)
            EndAction(self)

        // ══════════════════════════════════════════════════════════════════════════════
        // PHASE 4: SURGERY & BRAIN CYLINDERS
        // ══════════════════════════════════════════════════════════════════════════════

        case SurgeryMainAction(self) =>
            // Find all Cultists owned by self (on map, Submerged, or on Cursed Slumbering Gate)
            val cultists = self.units.%(u =>
                u.uclass.utype == Cultist &&
                u.uclass != BrainCylinderCultist &&
                (u.region.onMap || u.region == self.reserve || game.board.regions.has(u.region))
            )./(_.ref)

            if (cultists.isEmpty)
                EndAction(self)
            else
                Force(SurgeryChooseCultistsAction(self, $))

        case SurgeryChooseCultistsAction(self, selected) =>
            val cultists = self.units.%(u =>
                u.uclass.utype == Cultist &&
                u.uclass != BrainCylinderCultist &&
                (u.region.onMap || u.region == self.reserve || game.board.regions.has(u.region))
            )./(_.ref)

            // Count existing Brain Cylinders
            val existingBCs = self.units.%(_.uclass == BrainCylinderCultist).num
            val maxNew = 4 - existingBCs

            if (cultists.isEmpty)
                Force(SurgeryPerformAction(self, selected))
            else
                Ask(self)
                    .each(cultists.take(maxNew))(uRef => SurgeryToggleUnitAction(self, uRef, selected))
                    .add(SurgeryDoneAction(self, selected))
                    .add(SurgeryCancelAction(self))

        case SurgeryToggleUnitAction(self, uRef, selected) =>
            val newSelected = if (selected.has(uRef)) selected.but(uRef) else selected :+ uRef
            Force(SurgeryChooseCultistsAction(self, newSelected))

        case SurgeryDoneAction(self, selected) =>
            Force(SurgeryPerformAction(self, selected))

        case SurgeryCancelAction(self) =>
            EndAction(self)

        case SurgeryPerformAction(self, cultists) =>
            if (cultists.isEmpty) {
                EndAction(self)
            } else {
                self.payPower(1, "for Surgery")

                cultists.foreach { uRef =>
                    val u = game.unit(uRef)
                    val region = u.region
                    val onGate = u.onGate

                    // Remove Cultist
                    game.eliminate(u)

                    // Create Brain Cylinder at same location (belongs to the faction, not neutrals)
                    val bcIndex = self.units.%(_.uclass == BrainCylinderCultist).num
                    val bc = new UnitFigure(self, BrainCylinderCultist, bcIndex, region, onGate)
                    self.units ::= bc

                    self.log("converted", uRef.short, "to", "Brain Cylinder".styled(self), "in", region)
                }

                EndAction(self)
            }

        case _ => UnknownContinue
    }

    // ── AFTER ACTION HOOK: Slime Mold ownership switches when Slime Sea Overlook gate
    // control changes. Check after every action. ──
    override def afterAction()(implicit game : Game) {
        if (!game.board.isYuggothMap) return

        val overlook = game.board.regions.find(_.name == "Slime Sea Overlook")
        overlook.foreach { ov =>
            // Find current gate controller (if any)
            val gateController = factions.find(_.gates.contains(ov))

            // Switch ALL Slime Molds to new controller (or None if abandoned)
            game.slimeMolds = game.slimeMolds.map(sm => sm.copy(controller = gateController))
        }
    }

    // When the Watcher's Token is reduced to 0 (Battle.scala sets its UnitFigure's
    // health = Killed), the generic EliminatePhase sweep calls game.eliminate(u),
    // which runs this hook BEFORE returning the figure to TheWatcherFaction.reserve
    // (the generic base eliminate() body already does that region reset — this
    // hook only resets the Watcher-specific Doom-Track bookkeeping, per I5).
    override def eliminate(u : UnitFigure)(implicit game : Game) {
        if (u.uclass == TheWatcherUnit) {
            game.watcherAsleep = true
            game.watcherPosition = 12
            log("The Watcher".styled("nt") + " is destroyed — returns to slumber at 12")
        }
    }
}
