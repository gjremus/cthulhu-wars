package cws

import hrf.colmat._

import html._


// ============================================================================
// DEFILERS COURT (DC) — Homebrew faction
//
// Per "DC Faction Implementation Guide.docx" (read 2026-06-06).
// Theme: corruption/conversion faction led by Y'Golonac (standard GOO).
// Resource: Sin (uncapped Int, lives on Game.scala as game.dcSin per G29).
// Units: Acolyte (6 — start ON the 6 SB-requirement slots, NOT in pool),
//        MindlessHusk (pool 5, base combat 1), FallenProphet (pool 4, variable
//        combat), YgolonacDC (1 GOO, awaken cost = SB count, combat = ceil(Sin/2)).
// Unique abilities: Tenebrosum (Ongoing — repeat Common/SB Action paying Sin
//                   as Power; 1 repeat max; non-recursive) and
//                   Depravity (Gather Power — +1 Sin per DC Acolyte on map).
// Faction color: gold (#F0EDA8 per glyph).
// No starting region — Y'Golonac's first awaken area becomes the Start Area.
// ============================================================================

// ── UNIT CLASSES ───────────────────────────────────────────────────────────
case object MindlessHusk  extends FactionUnitClass(DC, "Mindless Husk",  Monster, 1)
case object FallenProphet extends FactionUnitClass(DC, "Fallen Prophet", Monster, 3)
// Y'Golonac is a STANDARD GOO (not ElderGod). Awaken cost sentinel 0 → dynamic
// via DC.awakenCost (= number of DC spellbooks on sheet, 0–6).
// Bacchanal (§1.8): Y'Golonac can Build & Control Gates — predicate override.
case object YgolonacDC      extends FactionUnitClass(DC, "Y'Golonac",      GOO,     0) {
    override def canControlGate(u : UnitFigure)(implicit game : Game) : Boolean = true
}


// ── SPELLBOOKS ─────────────────────────────────────────────────────────────
// Faction unique abilities (Ongoing — always-on, not in library).
case object Tenebrosum extends FactionSpellbook(DC, "Tenebrosum")
case object Depravity  extends FactionSpellbook(DC, "Depravity")

// Six DC spellbooks (per guide §1.10). Names verbatim from card art.
case object Proselytize extends FactionSpellbook(DC, "Proselytize")
case object Satiate     extends FactionSpellbook(DC, "Satiate")
case object Lure        extends FactionSpellbook(DC, "Lure")
case object Eschar      extends FactionSpellbook(DC, "Eschar")
case object Pilgrimage  extends FactionSpellbook(DC, "Pilgrimage")
case object DarkBargain extends FactionSpellbook(DC, "Dark Bargain")


// ── SPELLBOOK REQUIREMENTS ─────────────────────────────────────────────────
// 1-to-1 slot mapping per authoritative §1.9 user-supplied list.
case object ProselytizeReq extends Requirement("Doom Phase: gain 2 Sin per enemy GOO")
case object SatiateReq     extends Requirement("Doom Phase: +1 Power per other SB, +1 Sin per remaining pool SB")
case object LureReq        extends Requirement("No Mindless Husks in your Pool")
case object EscharReq      extends Requirement("No Fallen Prophets in your Pool")
case object PilgrimageReq  extends Requirement("Any player performs a Ritual of Annihilation")
case object DarkBargainReq extends Requirement("Awaken Y'Golonac, Lord of Sin")


// ── FACTION OBJECT ─────────────────────────────────────────────────────────
case object DC extends Faction { f =>
    def name  = "Defilers Court"
    def short = "DC"
    def style = "dc"

    // Tenebrosum + Depravity are ALWAYS-ON unique abilities (Ongoing).
    override def abilities = $(Tenebrosum, Depravity)
    override def library   = $(Proselytize, Satiate, Lure, Eschar, Pilgrimage, DarkBargain)
    override def requirements(options : $[GameOption]) =
        $(ProselytizeReq, SatiateReq, LureReq, EscharReq, PilgrimageReq, DarkBargainReq)

    val allUnits =
        1.times(YgolonacDC)      ++
        5.times(MindlessHusk)  ++
        4.times(FallenProphet) ++
        6.times(Acolyte)

    // Y'Golonac awaken: cost = number of SBs on DC's sheet, target must be a
    // LAND AREA without a Controlled Gate.
    // Awaken cost is dynamic per guide §1.8: "Pay Power equal to the number of
    // Spellbooks on your Faction Sheet."
    override def awakenCost(u : UnitClass, r : Region)(implicit game : Game) : |[Int] = u match {
        case YgolonacDC =>
            val landNoCtrlGate = r.glyph != Ocean && !game.factions.exists(_.gates.has(r))
            if (landNoCtrlGate) Some(f.spellbooks.num) else None
        case _ => None
    }

    // Strength: Mindless Husk = 1, Fallen Prophet handled in Battle.scala hook
    // (variable: enemy cultists in area during DC turn, own cultists otherwise),
    // Y'Golonac combat dice = ceil(Sin / 2) — also handled via Battle hook.
    // Here we provide a base sum; Battle.scala overrides Prophet + Y'Golonac.
    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int = {
        val husks  = units(MindlessHusk).not(Zeroed).num
        // Fallen Prophet (§1.10 Reading + §2.12): during DC's turn = enemy Cultists
        // in the Prophet's Area; any other time = DC Cultists in the Prophet's Area.
        // Use game.battle attacker as proxy for "DC's turn" (during a battle DC
        // initiated). Outside battle, DC.acted/active also implies DC's turn.
        val isDCTurn = game.battle./(_.attacker).has(f) || (game.battle.none && f.active && !f.acted)
        val prophets = units(FallenProphet).not(Zeroed)./{ p =>
            if (isDCTurn) opponent.at(p.region).%(_.uclass.utype == Cultist).num
            else          f.at(p.region).%(_.uclass.utype == Cultist).num
        }.sum
        // Y'Golonac: ceil(Sin / 2), no base/cap/minimum.
        val ygolonac = units(YgolonacDC).not(Zeroed).num * ((game.dcSin + 1) / 2)
        husks + prophets + ygolonac + neutralStrength(units, opponent)
    }
}


// ============================================================================
// DC ACTION CASE CLASSES
// ============================================================================

// ── Tenebrosum (post-action repeat — §1.5.1 + §2.6a) ────────────────────────
// Soft prompt (menu navigation) + Hard repeat action (state mutation).
// Per guide G3 + G6 + FCG: Soft for prompt, Hard for debit/re-enqueue.
// Offered as a main-menu option when game.dcLastActionForTenebrosum is set.
// 2026-06-06 Fix 75: strict same-action replay. Button label uses the
// action name (Battle/Move/Build/etc.) of the last action. PER-TURN flag,
// not per-action: once Tenebrosum has been used this turn it's hidden until
// PreMainAction resets it. self is the casting faction (DC, or SL via the
// Ancient Sorcery permanent bundle).
case class DCTenebrosumMainAction(self : Faction, cost : Int, actionName : String)
    extends OptionFactionAction(Tenebrosum.styled(DC) + ": Repeat " + actionName.styled(self) + " for " + cost.toString.styled("dc") + " Sin")
    with MainQuestion with Soft with PowerNeutral
case class DCTenebrosumRepeatAction(self : Faction, cost : Int, actionName : String)
    extends ForcedAction with PowerNeutral

// ── Reserved-Acolyte placement (fires on SB acquisition — §1.6 + §2.5) ─────
case class DCPlaceReservedAcolyteMainAction(self : Faction, sb : Spellbook)
    extends ForcedAction with PowerNeutral
case class DCPlaceReservedAcolyteAction(self : Faction, sb : Spellbook, r : Region)
    extends BaseFactionAction(
        "Place reserved " + Acolyte.styled(DC) + " from " + sb.styled(DC) + " in",
        implicit g => r.toString)

// ── Proselytize per-enemy drag (G11: self=enemyFaction for enemy-colored border) ─
case class DCProselytizeDragAction(self : Faction, acolyte : UnitRef, to : Region)
    extends BaseFactionAction(
        Proselytize.styled(DC) + ": choose " + Acolyte.styled(self) + " dragged to " + to.toString,
        implicit g => g.unit(acolyte).uclass.styled(self) + " from " + g.unit(acolyte).region.toString)
    with Soft with PowerNeutral

// ── Satiate (Action: Cost 2 — §1.10) ───────────────────────────────────────
case class DCSatiateMainAction(self : Faction)
    extends OptionFactionAction(Satiate.styled(DC) + " (Cost " + 2.power + ")")
    with MainQuestion with Soft
case class DCSatiateConfirmAction(self : Faction)
    extends BaseFactionAction(Satiate.styled(DC), "Confirm".styled("power")) with Soft
// Per-faction Cultist pick (self=affectedFaction). Hard — once a faction has
// committed to Satiate the captured unit is no longer cancelable.
case class DCSatiateFactionPickAction(self : Faction, area : Region, remaining : $[Faction], capturedSoFar : Int)
    extends ForcedAction with PowerNeutral
case class DCSatiatePickCultistAction(self : Faction, area : Region, cultist : UnitRef, remaining : $[Faction], capturedSoFar : Int)
    extends BaseFactionAction(Satiate.styled(DC) + ": " + self.short.styled(self) + " loses", implicit g => cultist.uclass.styled(self))
    with PowerNeutral
case class DCSatiateFinishAction(self : Faction, area : Region, capturedSoFar : Int)
    extends ForcedAction with PowerNeutral

// ── Lure (Action: Cost 1 — §1.10) ──────────────────────────────────────────
case class DCLureMainAction(self : Faction)
    extends OptionFactionAction(Lure.styled(DC) + " (Cost " + 1.power + ")")
    with MainQuestion with Soft
case class DCLureConfirmAction(self : Faction)
    extends BaseFactionAction(Lure.styled(DC), "Confirm".styled("power")) with Soft
case class DCLureFactionPickAction(self : Faction, area : Region, remaining : $[Faction])
    extends ForcedAction with PowerNeutral
case class DCLurePickCultistAction(self : Faction, area : Region, cultist : UnitRef, remaining : $[Faction])
    extends BaseFactionAction(Lure.styled(DC) + ": " + self.short.styled(self) + " moves", implicit g => cultist.uclass.styled(self) + " to " + area.toString)
    with PowerNeutral

// ── Pilgrimage (Action: Cost 1 — §1.10) ────────────────────────────────────
case class DCPilgrimageMainAction(self : Faction)
    extends OptionFactionAction(Pilgrimage.styled(DC) + " (Cost " + 1.power + ")")
    with MainQuestion with Soft
case class DCPilgrimageProphetAction(self : Faction, prophet : UnitRef)
    extends BaseFactionAction(
        Pilgrimage.styled(DC) + ": choose destination",
        implicit g => FallenProphet.styled(DC) + " in " + g.unit(prophet).region.toString)
    with Soft with PowerNeutral
case class DCPilgrimageDestAction(self : Faction, prophet : UnitRef, dest : Region)
    extends BaseFactionAction(Pilgrimage.styled(DC) + ": move other units to", implicit g => dest.toString) with Soft

// ── Dark Bargain (Action: Cost 0 — §1.10) ──────────────────────────────────
// Round-of-prompts pattern modeled on DS Azathoth + CC Thousand Forms.
case class DCDarkBargainMainAction(self : Faction)
    extends OptionFactionAction(DarkBargain.styled(DC) + " (Cost " + 0.power + ")")
    with MainQuestion with Soft
case class DCDarkBargainConfirmAction(self : Faction)
    extends BaseFactionAction(DarkBargain.styled(DC), "Confirm".styled("power")) with Soft
// Round-of-prompts: each enemy F picks a D6 face (self=enemy per G11 so menu
// border renders in enemy color). Continue action steps through remaining enemies.
case class DCDarkBargainEnemyContinueAction(self : Faction, remaining : $[Faction])
    extends ForcedAction with PowerNeutral
case class DCDarkBargainEnemyPickAction(self : Faction, face : Int, remaining : $[Faction])
    extends BaseFactionAction(DarkBargain.styled(DC) + ": " + self.short.styled(self) + " picks D6 face", implicit g => face.toString) with PowerNeutral
case class DCDarkBargainChooseSinAction(self : Faction, face : Int)
    extends BaseFactionAction(DarkBargain.styled(DC) + ": gain Sin", implicit g => face.toString.styled("dc")) with PowerNeutral


// ============================================================================
// DC EXPANSION — game-loop integration
// ============================================================================
object DCExpansion extends Expansion {

    // Per-SB reserved Acolyte tracking. Each SB starts with a reserved Acolyte;
    // when DC acquires the SB the Acolyte enters play in a chosen Area.
    // Tracked as a $[Spellbook] (grammar primitive — NOT Set per G35).
    // State lives on Game.scala (var dcReservedSpellbookAcolytes) for undo safety.

    override def eliminate(u : UnitFigure)(implicit game : Game) {
        if (!game.setup.has(DC)) return
        // No DC-specific eliminate hook required by spec (SBRs trigger on
        // pool/sheet inspection during doom/main phases).
    }

    def perform(action : Action, soft : VoidGuard)(implicit game : Game) : Continue = action @@ {

        // ── SETUP: starting Power=4, Sin=0, 6 Acolytes on SB-requirement slots ─
        case SetupFactionsAction if game.setup.has(DC) && !game.starting.contains(DC) =>
            val f = DC
            // No map units placed. All 6 Acolytes are "reserved" on the 6 SB-req slots.
            // Each SB → Acolyte mapping is implicit (1-to-1 by index).
            game.dcReservedSpellbookAcolytes = $(Proselytize, Satiate, Lure, Eschar, Pilgrimage, DarkBargain)
            // Mark DC as "placed" using a sentinel — Y'Golonac's first awaken
            // becomes the real Start Area (set in AwakenedAction handler below).
            // Use a placeholder Pool region so SetupFactionsAction loop unblocks.
            game.starting = game.starting + (DC -> f.reserve)
            // Starting power 4 (override default 0)
            f.power = 4
            f.log("starts with", 4.power + " and 0 Sin".styled("dc"))
            f.log("places 6 reserved", Acolyte.styled(DC), "on Spellbook requirement slots")
            Force(SetupFactionsAction)

        // ── Y'Golonac Awakening: set Start Area on first awaken ──────────────
        case AwakenedAction(self : DC.type, YgolonacDC, r, _) =>
            // First-awaken sets the DC Start Area (per §1.6 / §3.4.3).
            if (game.starting.get(DC).contains(self.reserve)) {
                game.starting = game.starting + (DC -> r)
                game.gates :+= r
                self.gates :+= r
                self.log("Start Area set to", r, "(via Y'Golonac awakening)")
            }
            // Satisfy DarkBargain SBR (§3.12.6). Wrap in CheckSpellbooksAction (G28/Item 9).
            self.satisfyIf(DarkBargainReq, "Awoke Y'Golonac, Lord of Sin", true)
            Force(CheckSpellbooksAction(EndAction(self)))

        // ── PilgrimageReq: trigger on any RitualAction ───────────────────────
        case a : RitualAction if game.setup.has(DC) =>
            DC.satisfyIf(PilgrimageReq, "A player performed a Ritual of Annihilation", true)
            UnknownContinue

        // ── SB acquisition: Reserved-Acolyte placement trigger (Item 5) ──────
        // When DC acquires a SB (via SpellbookAction in Game.scala line 3087),
        // queue a reserved-Acolyte placement. We detect this in afterAction by
        // comparing dcReservedSpellbookAcolytes vs current sheet.
        // Hook here: after SpellbookAction resolves for DC, check for new SBs
        // and append to pending placements queue.
        case SpellbookAction(f, sb, then) if f == DC && game.dcReservedSpellbookAcolytes.has(sb) =>
            // Let standard handler run first (it sets f.spellbooks). Then queue placement.
            game.dcPendingAcolytePlacements :+= sb
            UnknownContinue

        // ── MAIN ACTION ─────────────────────────────────────────────────────
        case MainAction(f : DC.type) if f.active.not =>
            UnknownContinue
        case MainAction(f : DC.type) if f.acted && !game.dcTenebrosumExtraTurn =>
            // Post-acted: still allow Tenebrosum repeat + controls + endTurn (G1)
            implicit val asking = Asking(f)
            game.controls(f)
            if (f.hasAllSB)
                game.battles(f)
            // Tenebrosum: offer post-action repeat if last action recorded + Sin sufficient
            // 2026-06-06 Fix 75: hide once Tenebrosum has been used this turn.
            game.dcLastActionForTenebrosum.foreach { case (_, cost, an) =>
                if (cost > 0 && game.dcSin >= cost && !game.dcTenebrosumGuard && !game.dcTenebrosumUsedThisTurn)
                    + DCTenebrosumMainAction(f, cost, an)
            }
            game.reveals(f)
            game.endTurn(f)(true)
            asking

        case MainAction(f : DC.type) =>
            implicit val asking = Asking(f)

            // Tenebrosum extra-turn: consume the flag (one-shot).
            if (game.dcTenebrosumExtraTurn) game.dcTenebrosumExtraTurn = false

            // Pending reserved-Acolyte placement (Item 5): if any SBs pending, prompt first.
            // Force-direct to placement (no other options) — Soft chain.
            if (game.dcPendingAcolytePlacements.any) {
                val sb = game.dcPendingAcolytePlacements.first
                return Force(DCPlaceReservedAcolyteMainAction(f, sb))
            }

            game.moves(f)
            game.captures(f)
            game.recruits(f)
            game.battles(f)
            game.controls(f)
            game.builds(f)
            game.summons(f)
            game.awakens(f)
            game.independents(f)
            game.neutralSpellbooks(f)

            // DC spellbook actions
            if (f.can(Satiate) && f.power >= 2 && f.allInPlay.%(_.uclass == YgolonacDC).any)
                + DCSatiateMainAction(f)
            if (f.can(Lure) && f.power >= 1 && f.allInPlay.%(_.uclass == YgolonacDC).any)
                + DCLureMainAction(f)
            if (f.can(Pilgrimage) && f.power >= 1 && f.allInPlay.%(_.uclass == FallenProphet).any)
                + DCPilgrimageMainAction(f)
            if (f.can(DarkBargain) && f.allInPlay.%(_.uclass == YgolonacDC).any && !game.dcDarkBargainFacedown)
                + DCDarkBargainMainAction(f)

            // Tenebrosum (Item 1): offer repeat of last DC Common/SB action if Sin >= cost
            // 2026-06-06 Fix 75: hide once Tenebrosum has been used this turn.
            game.dcLastActionForTenebrosum.foreach { case (_, cost, an) =>
                if (cost > 0 && game.dcSin >= cost && !game.dcTenebrosumGuard && !game.dcTenebrosumUsedThisTurn)
                    + DCTenebrosumMainAction(f, cost, an)
            }

            game.libraryActions(f)
            game.highPriests(f)
            game.reveals(f)
            game.endTurn(f)(f.battled.any || game.nexed.any)

            asking

        // ── Tenebrosum: Soft prompt — opens "are you sure" confirm ───────────
        case DCTenebrosumMainAction(self, cost, an) =>
            implicit val asking = Asking(self)
            + DCTenebrosumRepeatAction(self, cost, an)
            + CancelAction
            asking

        // ── Tenebrosum: Hard — debit Sin and grant one extra main action ──
        // 2026-06-06 Fix 75: per-TURN used flag (NOT per-action). DC pays from
        // dcSin; SL (via the Ancient Sorcery permanent bundle) pays from slSin.
        case DCTenebrosumRepeatAction(self, cost, an) =>
            game.dcLastActionForTenebrosum match {
                case Some(_) =>
                    if (self == SL) {
                        game.slSin -= cost
                        game.slTenebrosumUsedThisTurn = true
                    } else {
                        game.dcSin -= cost
                        game.dcTenebrosumUsedThisTurn = true
                    }
                    game.dcTenebrosumGuard = true
                    game.dcTenebrosumExtraTurn = true
                    self.log(Tenebrosum.styled(DC) + ": spent", cost.toString.styled("dc"), "Sin to Repeat", an.styled(self))
                    // Clear last-action so the SAME action isn't repeated as the "last" again
                    game.dcLastActionForTenebrosum = None
                    Force(MainAction(self))
                case None =>
                    UnknownContinue
            }

        // ── DOOM ────────────────────────────────────────────────────────────
        case DoomAction(f : DC.type) =>
            implicit val asking = Asking(f)

            // ProselytizeReq: Doom-Phase opt-in (+2 Sin per enemy GOO on satisfy)
            if (f.needs(ProselytizeReq))
                + DCProselytizeReqOptInAction(f)
            // SatiateReq: Doom-Phase opt-in (+1 Power per other SB, +1 Sin per pool SB)
            if (f.needs(SatiateReq))
                + DCSatiateReqOptInAction(f)

            game.rituals(f)
            game.reveals(f)
            game.highPriests(f)
            + DoomDoneAction(f)
            asking

        // ── Doom-Phase SB-Requirement opt-ins (wrap in CheckSpellbooksAction per Item 9) ─
        case DCProselytizeReqOptInAction(self) =>
            val enemyGOOs = game.factions.but(self)./~(_.allInPlay).%(_.uclass.utype == GOO).num
            self.satisfy(ProselytizeReq, "Took Proselytize in Doom Phase")
            game.dcSin += 2 * enemyGOOs
            self.log("Proselytize SBR: gained", (2 * enemyGOOs).toString.styled("dc"), "Sin (2 per", enemyGOOs, "enemy GOO)")
            Force(CheckSpellbooksAction(DoomAction(self)))

        case DCSatiateReqOptInAction(self) =>
            val otherSBs = self.spellbooks.num
            val poolSBs  = self.unfulfilled.num
            self.satisfy(SatiateReq, "Took Satiate in Doom Phase")
            self.power += otherSBs
            game.dcSin += poolSBs
            self.log("Satiate SBR: gained", otherSBs.power, "and", poolSBs.toString.styled("dc"), "Sin")
            Force(CheckSpellbooksAction(DoomAction(self)))

        // ── Reserved-Acolyte placement on SB acquisition ─────────────────────
        case DCPlaceReservedAcolyteMainAction(self, sb) =>
            implicit val asking = Asking(self)
            // Offer every region on the map (any Area legal per §1.6).
            areas.foreach { r =>
                + DCPlaceReservedAcolyteAction(self, sb, r)
            }
            asking

        case DCPlaceReservedAcolyteAction(self, sb, r) =>
            // Find an Acolyte in reserve and place it in r.
            val placed = self.units.%(u => u.uclass == Acolyte && u.region == self.reserve).headOption
            placed match {
                case Some(u) =>
                    u.region = r
                    game.dcReservedSpellbookAcolytes = game.dcReservedSpellbookAcolytes.but(sb)
                    self.log("placed reserved", Acolyte.styled(DC), "from", sb.styled(DC), "in", r)
                case None =>
                    self.log("no reserved Acolyte to place for", sb.styled(DC))
            }
            // Remove this SB from pending placements queue
            game.dcPendingAcolytePlacements = game.dcPendingAcolytePlacements.but(sb)
            UnknownContinue

        // ── Satiate (cost 2, capture from each faction in Y'Golonac's area) ──
        case DCSatiateMainAction(self) =>
            implicit val asking = Asking(self)
            + DCSatiateConfirmAction(self)
            + CancelAction
            asking

        case DCSatiateConfirmAction(self) =>
            self.power -= 2
            // Record last action for Tenebrosum (Item 1)
            recordTenebrosum(action, 2, "Satiate")
            val ygolonacOpt = self.allInPlay.%(_.uclass == YgolonacDC).headOption
            ygolonacOpt match {
                case Some(yg) =>
                    val area = yg.region
                    val factionsWithCultists = game.factions.%(ff => ff.at(area).%(_.uclass.utype == Cultist).any)
                    // Per-faction pick chain (Item 8): each faction with 2+ cultists chooses which to lose.
                    Force(DCSatiateFactionPickAction(self, area, factionsWithCultists, 0))
                case None =>
                    EndAction(self)
            }

        case DCSatiateFactionPickAction(self, area, remaining, capturedSoFar) =>
            if (remaining.none) {
                Force(DCSatiateFinishAction(self, area, capturedSoFar))
            } else {
                val ff = remaining.first
                val cultists = ff.at(area).%(_.uclass.utype == Cultist)
                if (cultists.num == 1) {
                    // Auto-capture the only cultist
                    val c = cultists.first
                    c.region = self.prison
                    ff.log(Satiate.styled(DC) + ":", ff.short.styled(ff), "loses", c.uclass.styled(ff), "(only one in", area, ")")
                    Force(DCSatiateFactionPickAction(self, area, remaining.dropStarting, capturedSoFar + 1))
                } else if (cultists.num > 1) {
                    // Ask the affected faction which to lose (self=ff for enemy-colored border)
                    implicit val asking = Asking(ff)
                    cultists.foreach { c =>
                        + DCSatiatePickCultistAction(ff, area, c.ref, remaining.dropStarting, capturedSoFar)
                    }
                    asking
                } else {
                    Force(DCSatiateFactionPickAction(self, area, remaining.dropStarting, capturedSoFar))
                }
            }

        case DCSatiatePickCultistAction(self, area, cultistRef, remaining, capturedSoFar) =>
            val c = game.unit(cultistRef)
            c.region = DC.prison
            self.log(Satiate.styled(DC) + ":", self.short.styled(self), "loses", c.uclass.styled(self), "to", DC.full)
            Force(DCSatiateFactionPickAction(DC, area, remaining, capturedSoFar + 1))

        case DCSatiateFinishAction(self, area, capturedSoFar) =>
            val bonus = math.max(0, capturedSoFar - 1)
            if (bonus > 0) self.takeES(bonus)
            self.log(Satiate.styled(DC) + ": captured", capturedSoFar, "cultist".s(capturedSoFar), "in", area,
                if (bonus > 0) ", gained " + bonus.es else "")
            EndAction(self)

        // ── Lure (cost 1, force-move adjacent enemy cultists into Y'Golonac's area) ──
        case DCLureMainAction(self) =>
            implicit val asking = Asking(self)
            + DCLureConfirmAction(self)
            + CancelAction
            asking

        case DCLureConfirmAction(self) =>
            self.power -= 1
            recordTenebrosum(action, 1, "Lure")
            val ygolonacOpt = self.allInPlay.%(_.uclass == YgolonacDC).headOption
            ygolonacOpt match {
                case Some(yg) =>
                    val area = yg.region
                    val enemies = game.factions.but(self)
                    Force(DCLureFactionPickAction(self, area, enemies))
                case None =>
                    EndAction(self)
            }

        case DCLureFactionPickAction(self, area, remaining) =>
            if (remaining.none) {
                EndAction(self)
            } else {
                val e = remaining.first
                val adj = game.board.connected(area)
                val eligible = adj./~{ r =>
                    val hasEnemyGOO = game.factions.but(e).exists(o => o.at(r).%(_.uclass.utype == GOO).any)
                    val hasTerror   = game.factions./~(_.at(r)).%(_.uclass.utype == Terror).any
                    val hasFactionBuilding = game.factions./~(_.at(r)).%(_.uclass.utype == Building).any
                    val isMoon = r.toString.contains("Moon")
                    if (hasEnemyGOO || hasTerror || hasFactionBuilding || isMoon) $()
                    else e.at(r).%(_.uclass.utype == Cultist)
                }
                if (eligible.num == 0) {
                    // Skip this enemy
                    Force(DCLureFactionPickAction(self, area, remaining.dropStarting))
                } else if (eligible.num == 1) {
                    val c = eligible.first
                    val from = c.region
                    c.region = area
                    e.log(Lure.styled(DC) + ":", e.short.styled(e), "moves", c.uclass.styled(e), "from", from, "to", area)
                    Force(DCLureFactionPickAction(self, area, remaining.dropStarting))
                } else {
                    // Ask enemy which Cultist to move (self=e for enemy-colored menu border per G11)
                    implicit val asking = Asking(e)
                    eligible.foreach { c =>
                        + DCLurePickCultistAction(e, area, c.ref, remaining.dropStarting)
                    }
                    asking
                }
            }

        case DCLurePickCultistAction(self, area, cultistRef, remaining) =>
            val c = game.unit(cultistRef)
            val from = c.region
            c.region = area
            self.log(Lure.styled(DC) + ":", self.short.styled(self), "moves", c.uclass.styled(self), "from", from, "to", area)
            Force(DCLureFactionPickAction(DC, area, remaining))

        // ── Pilgrimage (cost 1, free move for other DC units in Prophet's area) ──
        case DCPilgrimageMainAction(self) =>
            implicit val asking = Asking(self)
            val prophets = self.allInPlay.%(_.uclass == FallenProphet)
            prophets.foreach { p =>
                + DCPilgrimageProphetAction(self, p.ref)
            }
            + CancelAction
            asking

        case DCPilgrimageProphetAction(self, prophet) =>
            implicit val asking = Asking(self)
            val p = game.unit(prophet)
            val dests = game.board.connected(p.region)
            dests.foreach { d =>
                + DCPilgrimageDestAction(self, prophet, d)
            }
            + CancelAction
            asking

        case DCPilgrimageDestAction(self, prophet, dest) =>
            self.power -= 1
            recordTenebrosum(action, 1, "Pilgrimage")
            val p = game.unit(prophet)
            val src = p.region
            // Move all other DC units (NOT the prophet itself) to dest
            val movers = self.at(src).%(u => u.ref != prophet)
            movers.foreach(_.region = dest)
            self.log(Pilgrimage.styled(DC) + ": moved", movers.num, "unit".s(movers.num), "from", src, "to", dest)
            EndAction(self)

        // ── Dark Bargain (cost 0, D6 redistribution — full round-of-prompts) ─
        case DCDarkBargainMainAction(self) =>
            implicit val asking = Asking(self)
            + DCDarkBargainConfirmAction(self)
            + CancelAction
            asking

        case DCDarkBargainConfirmAction(self) =>
            recordTenebrosum(action, 0, "Dark Bargain")
            // Start round-of-prompts: each enemy picks a D6 face 1..6.
            game.dcDarkBargainPicks = $
            val enemies = game.factions.but(self)
            Force(DCDarkBargainEnemyContinueAction(self, enemies))

        case DCDarkBargainEnemyContinueAction(self, remaining) =>
            if (remaining.none) {
                // All enemies have picked — now ask DC which face to choose
                implicit val asking = Asking(self)
                $(1, 2, 3, 4, 5, 6).foreach { face =>
                    + DCDarkBargainChooseSinAction(self, face)
                }
                asking
            } else {
                val e = remaining.first
                // Per-enemy D6 face pick (self=e for enemy-colored menu border per G11)
                implicit val asking = Asking(e)
                $(1, 2, 3, 4, 5, 6).foreach { face =>
                    + DCDarkBargainEnemyPickAction(e, face, remaining.dropStarting)
                }
                asking
            }

        case DCDarkBargainEnemyPickAction(self, face, remaining) =>
            game.dcDarkBargainPicks :+= ((self, face))
            self.log(DarkBargain.styled(DC) + ":", self.short.styled(self), "picked D6 face", face.toString)
            Force(DCDarkBargainEnemyContinueAction(DC, remaining))

        case DCDarkBargainChooseSinAction(self, face) =>
            game.dcSin += face
            // Gather enemies who picked the chosen face — they forfeit Power for redistribution
            val matchingEnemies : $[Faction] = game.dcDarkBargainPicks.%((_, pf) => pf == face)./((f, _) => f).distinct
            val totalForfeited = matchingEnemies./(e => math.min(e.power, face)).sum
            matchingEnemies.foreach { e =>
                val amt = math.min(e.power, face)
                if (amt > 0) {
                    e.power -= amt
                    self.log(DarkBargain.styled(DC) + ": took", amt.power, "from", e.full, "(picked", face.toString + ")")
                }
            }
            // Redistribute pooled power evenly across ALL enemies (per §1.10 reading)
            val allEnemies = game.factions.but(self)
            if (allEnemies.any && totalForfeited > 0) {
                val per   = totalForfeited / allEnemies.num
                val extra = totalForfeited % allEnemies.num
                allEnemies.zipWithIndex.foreach { case (e, i) =>
                    val amt = per + (if (i < extra) 1 else 0)
                    if (amt > 0) {
                        e.power += amt
                        self.log(DarkBargain.styled(DC) + ": gave", amt.power, "to", e.full, "(redistribution)")
                    }
                }
            }
            game.dcDarkBargainFacedown = true
            game.dcDarkBargainPicks = $
            self.log(DarkBargain.styled(DC) + ": flipped facedown until Gather Power; gained", face.toString.styled("dc"), "Sin")
            EndAction(self)

        // ── Gather Power: flip Dark Bargain face-up again ─────────────────────
        case PowerGatherAction(_) if game.setup.has(DC) && game.dcDarkBargainFacedown =>
            game.dcDarkBargainFacedown = false
            UnknownContinue

        // ── Proselytize (Item 2): per-Acolyte drag chain on Move ─────────────
        // After a DC Acolyte completes its move (MovedAction), prompt each
        // enemy F with Acolyte(s) in source to choose one of theirs to drag.
        // The chain is auto-resolved (forced for each enemy) — if enemy F has
        // exactly 1 Acolyte at source, drag it; if 2+, ask F (self=F for G11).
        case MovedAction(self : DC.type, u, o, r) if u.uclass == Acolyte && self.can(Proselytize) && o != r =>
            val enemies = game.factions.but(self).%(e => e.at(o).%(_.uclass == Acolyte).any)
            if (enemies.any) {
                // Process enemies one at a time; each enemy with 1 Acolyte → auto-drag,
                // 2+ → menu. Return MoveContinueAction at the end (standard MovedAction fall-through).
                enemies.foreach { e =>
                    val acolytes = e.at(o).%(_.uclass == Acolyte)
                    if (acolytes.num == 1) {
                        val a = acolytes.first
                        a.region = r
                        e.log(Proselytize.styled(DC) + ":", e.short.styled(e), Acolyte.styled(e), "dragged from", o, "to", r)
                    } else if (acolytes.num >= 2) {
                        // Force a per-enemy choose-Acolyte menu (G11: self=e for enemy-colored border).
                        // The pick action moves the chosen one to r.
                        implicit val asking = Asking(e)
                        acolytes.foreach { a =>
                            + DCProselytizeDragAction(e, a.ref, r)
                        }
                        return asking
                    }
                }
                MoveContinueAction(self, true)
            } else {
                UnknownContinue
            }

        case DCProselytizeDragAction(self, acolyteRef, to) =>
            val a = game.unit(acolyteRef)
            val from = a.region
            a.region = to
            self.log(Proselytize.styled(DC) + ":", self.short.styled(self), Acolyte.styled(self), "dragged from", from, "to", to)
            // Continue the move chain (standard post-move)
            MoveContinueAction(DC, true)

        // ── LureReq / EscharReq pool-check satisfaction (Item 7) ─────────────
        // These are evaluated on triggers/state changes. Easiest hook: after
        // SummonAction or AfterAction for DC, check pool state and satisfy.
        case AfterAction(self : DC.type) =>
            // LureReq: Have no Mindless Husks in your Pool.
            self.satisfyIf(LureReq, "No Mindless Husks in Pool", self.pool(MindlessHusk).none)
            // EscharReq: Have no Fallen Prophets in your Pool.
            self.satisfyIf(EscharReq, "No Fallen Prophets in Pool", self.pool(FallenProphet).none)
            UnknownContinue

        // ── Record Tenebrosum-eligible Common Actions (Item 1) ──────────────
        // For Common Actions handled in Game.scala (Move/Build/Control/Recruit/
        // Summon/Capture/Awaken/Attack/Ritual), record the cost BEFORE the
        // engine handler executes. Return UnknownContinue so engine still runs.
        // 2026-06-06 Fix 75: also record SL versions (when SL has the permanent
        // DC bundle) so SL gets "Repeat <action>" with its own Sin tracker.
        case MoveAction(self, u, o, r, cost) if cost > 0 && tenebrosumEligible(self) =>
            recordTenebrosum(action, cost, "Move")
            UnknownContinue
        case BuildGateAction(self, _) if tenebrosumEligible(self) =>
            // Build Gate cost: 3 - (UmrAtTawil discount). We don't have an UmrAtTawil/DC interaction.
            recordTenebrosum(action, 3, "Build Gate")
            UnknownContinue
        case RecruitAction(self, uc, r) if tenebrosumEligible(self) =>
            recordTenebrosum(action, self.recruitCost(uc, r), "Recruit")
            UnknownContinue
        case SummonAction(self, uc, r) if tenebrosumEligible(self) =>
            recordTenebrosum(action, self.summonCost(uc, r), "Summon")
            UnknownContinue
        case CaptureAction(self, _, _, _) if tenebrosumEligible(self) =>
            recordTenebrosum(action, 1, "Capture")
            UnknownContinue
        case AttackAction(self, _, _, _) if tenebrosumEligible(self) =>
            recordTenebrosum(action, 1, "Battle")
            UnknownContinue
        case AwakenAction(self, uc, r, _) if tenebrosumEligible(self) =>
            self.awakenCost(uc, r).foreach(c => recordTenebrosum(action, c, "Awaken"))
            UnknownContinue

        case EndAction(self) if tenebrosumEligible(self) =>
            // Clear the guard flag if we were in a Tenebrosum repeat (one-shot).
            if (game.dcTenebrosumGuard) {
                game.dcTenebrosumGuard = false
                game.dcLastActionForTenebrosum = None
            }
            UnknownContinue

        // 2026-06-06 Fix 75: per-TURN reset on PreMainAction. The used flag is
        // cleared at the start of each new turn so Tenebrosum becomes available
        // again. Also clears any stale last-action recorded from a prior turn.
        case PreMainAction(self) if tenebrosumEligible(self) =>
            if (self == SL) game.slTenebrosumUsedThisTurn = false
            else            game.dcTenebrosumUsedThisTurn = false
            game.dcLastActionForTenebrosum = None
            UnknownContinue

        case _ => UnknownContinue
    }

    // Helper: record the just-resolved action + its Power cost + display name.
    // Non-recursive: skipped if we're already inside a Tenebrosum-driven repeat.
    private def recordTenebrosum(a : Action, cost : Int, an : String)(implicit game : Game) : Unit = {
        if (!game.dcTenebrosumGuard && cost > 0) {
            game.dcLastActionForTenebrosum = Some((a, cost, an))
        }
    }

    // 2026-06-06 Fix 75: a faction is Tenebrosum-eligible if it's DC, or if it's
    // SL holding the permanent DC bundle via Ancient Sorcery (Fix 2).
    private def tenebrosumEligible(self : Faction)(implicit game : Game) : Boolean =
        self == DC || (self == SL && game.slPermanentBorrowed.has(Tenebrosum))
}


// Doom-phase SB-requirement opt-in actions
case class DCProselytizeReqOptInAction(self : Faction)
    extends OptionFactionAction(("Take " + Proselytize.name).styled(DC) + ": +2 Sin per enemy GOO")
    with DoomQuestion
case class DCSatiateReqOptInAction(self : Faction)
    extends OptionFactionAction(("Take " + Satiate.name).styled(DC) + ": +1 Power per other SB, +1 Sin per pool SB")
    with DoomQuestion
