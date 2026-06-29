package cws

import hrf.colmat._

import html._


// ============================================================================
// DEFILERS COURT (DC) — Homebrew faction
//
// Per "DC Faction Implementation Guide.docx" (read 2026-06-06).
// Theme: corruption/conversion faction led by Y'Golonac (standard GOO).
// Resource: Sin (Int, lives on Game.scala as game.dcSin per G29). HB Fix 96
//           (2026-06-07): Sin is capped at 2 * ritualMarker per user directive
//           and faction-card tooltip ("max equal to twice the Ritual Marker").
//           All grant sites must go through game.grantDCSin which clamps.
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

    override def gooValue(u : UnitClass)(implicit game : Game) : Int = u match {
        case YgolonacDC => f.spellbooks.num
        case _ => u.cost
    }

    // Strength: Mindless Husk = 1, Fallen Prophet handled in Battle.scala hook
    // (variable: enemy cultists in area during DC turn, own cultists otherwise),
    // Y'Golonac combat dice = ceil(Sin / 2) — also handled via Battle hook.
    // Here we provide a base sum; Battle.scala overrides Prophet + Y'Golonac.
    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int = {
        val husks  = units(MindlessHusk).not(Zeroed).num
        // Fallen Prophet (§1.10 Reading + §2.12): during DC's turn = enemy Cultists
        // in the Prophet's Area; any other time = DC Cultists in the Prophet's Area.
        // HB Fix 100.A (2026-06-08, closes audit BUG #3): the only reliable
        // "DC's turn" signal during combat is who INITIATED the battle. DC can
        // only initiate battles on its own turn, so battle.attacker == self
        // proves it's DC's turn for THIS battle — even when DC has already
        // acted earlier in the turn (e.g. multi-battle Tenebrosum chains, or
        // unlimited-combat scenarios with several attacks in one turn). The
        // prior proxy `f.active && !f.acted` flipped false after DC's first
        // action of the turn and broke the basis on every subsequent battle.
        // In-battle: use battle.attacker == self (the initiator field).
        // Out-of-battle (bot projections only): fall back to f.active (the
        // active-player flag), without the broken !f.acted clamp.
        val isDCTurn = game.battle./(_.attacker).has(f) || (game.battle.none && f.active)
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
    extends OptionFactionAction(
        // HB Fix 84 (2026-06-07): cost==0 (e.g. Dark Bargain) renders as a
        // "free repeat" per user spec; cost>0 renders the Sin price.
        // HB Fix 117 (2026-06-15): a Summon repeat may now pick ANY summonable
        // unit (Fix 113 broadened chooser), so the Sin cost is the PICKED unit's
        // power cost — not the recorded unit's. The menu can no longer show a
        // single fixed price, so it renders "(Sin = unit's cost)" for Summon.
        // All other action types keep the fixed recorded-cost label.
        Tenebrosum.styled(DC) + ": Repeat " + actionName.styled(self) + {
            if (cost == 0) " (free)"
            else if (actionName == "Summon") " (Sin = unit's cost)"
            else if (actionName == "Awaken") " (Sin = awakening cost)"
            else " for " + cost.toString.styled("dc") + " Sin"
        })
    with MainQuestion with Soft with PowerNeutral
// HB Fix 95 (2026-06-07): was a bare ForcedAction, which made the post-
// Tenebrosum confirm step render with title "N/A" and option "N/A" (the
// abstract ForcedAction defaults) — the user-visible bug "subsequent menu
// has 'N/A' and 'Cancel' with title 'N/A.'" Now a BaseFactionAction with
// the same shape as DCSatiateConfirmAction / DCLureConfirmAction /
// DCDarkBargainConfirmAction so the confirm prompt shows
// "Tenebrosum: Repeat <action> for N Sin"  +  "Confirm" option.
// HB Fix 109.E (2026-06-11): removed Soft so this action is RECORDED for
// undo replay. Without recording, undo replays the downstream action
// (BuildGateAction etc.) without the Tenebrosum guard — causing Power to be
// deducted instead of Sin and the log to lose its "used Tenebrosum to" prefix.
case class DCTenebrosumRepeatAction(self : Faction, cost : Int, actionName : String)
    extends BaseFactionAction(
        Tenebrosum.styled(DC) + ": Repeat " + actionName.styled(self) + {
            if (cost == 0) " (free)"
            else if (actionName == "Summon") " for Sin"
            else if (actionName == "Awaken") " for Sin"
            else " for " + cost.toString.styled("dc") + " Sin"
        },
        "Confirm".styled("power"))
    with PowerNeutral

// ── Tenebrosum broadened Summon / Awaken choosers (HB Fix 113, 2026-06-13) ──
// Per owner: a Sin-paid Tenebrosum SUMMON repeat may bring out a DIFFERENT unit
// than the one just summoned, and a Sin-paid AWAKEN repeat may awaken a DIFFERENT
// GOO / iGOO. These sub-menus reuse the canonical summon/awaken/independent menu
// builders (which now bypass the Power-affordability filter under the Tenebrosum
// guard) so every structurally-legal unit / GOO is offered, not just the recorded
// one. This reverses the same-kind-only restriction added in Fix 106.
case class DCTenebrosumSummonChooserAction(self : Faction)
    extends OptionFactionAction(("Summon").styled(self)) with MainQuestion with Soft
case class DCTenebrosumAwakenChooserAction(self : Faction)
    extends OptionFactionAction(("Awaken").styled(self)) with MainQuestion with Soft

// ── Reserved-Acolyte placement (conditional unlimited action — HB Fix 83) ──
// HB Fix 83 (2026-06-07): Same-turn delivery via conditional UNLIMITED action.
// When DC has at least one Acolyte still in DCFactionCardHold AND at least one
// earned (in-hand) library Spellbook whose Acolyte has not yet been delivered,
// a "Place Faction-Card Acolyte" option appears in DC's main menu (visible in
// both pre-acted and post-acted branches — unlimited). Picking opens a region
// picker; resolving moves one Acolyte from the faction-card pool to the map
// and decrements game.dcReservedSpellbookAcolytes by the matching SB.
case class DCPlaceReservedAcolyteMainAction(self : Faction)
    extends OptionFactionAction("Place " + Acolyte.styled(DC) + " from Faction Card")
    with MainQuestion with Soft with PowerNeutral
case class DCPlaceReservedAcolyteAction(self : Faction, r : Region)
    extends BaseFactionAction(
        "Place " + Acolyte.styled(DC) + " from Faction Card in",
        implicit g => r.toString)
    with PowerNeutral

// ── HB Fix 104: forced IMMEDIATE Acolyte delivery on SB acquisition ─────────
// HB Fix 104 (2026-06-10): per user — "every time [a SBR] is achieved, the
// acolyte needs to prompt the DC player to place it immediately, not as an
// unlimited action." Modeled on AN's immediate-grant SBR flow
// (GiveWorstMonster…AskAction → Force(then) at FactionAN.scala), but pulling
// the Acolyte from the DC FACTION-CARD reserve pool (DCFactionCardHold), not
// the normal cultist pool. Game.scala's SpellbookAction core handler forces
// this action (wrapping the downstream CheckSpellbooksAction continuation) the
// moment DC earns a library SB whose reserved Acolyte is still on the card.
// Forced (no Cancel): the placement is mandatory per spec. `sb` is the
// just-earned Spellbook; `then` is the continuation to resume the SB/ES loop.
case class DCDeliverReservedAcolyteForceAction(self : Faction, sb : Spellbook, then : ForcedAction)
    extends ForcedAction with PowerNeutral
case class DCDeliverReservedAcolytePlaceAction(self : Faction, sb : Spellbook, r : Region, then : ForcedAction)
    extends BaseFactionAction(
        "Place " + Acolyte.styled(DC) + " from Faction Card in",
        implicit g => r.toString)
    with PowerNeutral

// ── Proselytize per-enemy drag (G11: self=enemyFaction for enemy-colored border) ─
// Fix HB-79 (2026-06-06): Mirror DC Satiate/Lure per-faction chain. EVERY enemy
// with an Acolyte in the source area is forced to give up one Acolyte to drag
// along with the DC Acolyte. The only "choice" is WHICH Acolyte when the enemy
// has both an off-gate and an on-gate Acolyte available. The DC player's
// DCProselytizePlan command controls whether to prompt or auto-prefer one side.
// Pattern matches GC Devolve / HP Unspeakable Oath / Gate Diplomacy ("command thing").
case class DCProselytizeFactionPickAction(self : Faction, area : Region, to : Region, remaining : $[Faction])
    extends ForcedAction with PowerNeutral
case class DCProselytizePickCultistAction(self : Faction, area : Region, to : Region, cultist : UnitRef, remaining : $[Faction])
    extends BaseFactionAction(
        Proselytize.styled(DC) + ": " + self.short.styled(self) + " drags",
        implicit g => g.unit(cultist).uclass.styled(self) + (if (g.unit(cultist).onGate) " (on gate)" else " (off gate)") + " to " + to.toString)
    with PowerNeutral

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
// Per-unit opt-in chain (Fix HB-77): after the destination is chosen, prompt
// the DC player for each OTHER DC unit in the Prophet's source area. Each unit
// gets a Move/Stay choice; Done finishes the entire Pilgrimage early.
case class DCPilgrimageUnitContinueAction(self : Faction, prophet : UnitRef, dest : Region, remaining : $[UnitRef])
    extends ForcedAction with PowerNeutral
case class DCPilgrimageUnitMoveAction(self : Faction, prophet : UnitRef, dest : Region, unit : UnitRef, remaining : $[UnitRef])
    extends BaseFactionAction(Pilgrimage.styled(DC) + ": move", implicit g => g.unit(unit).uclass.styled(DC) + " to " + dest.toString)
    with Soft with PowerNeutral
case class DCPilgrimageUnitStayAction(self : Faction, prophet : UnitRef, dest : Region, unit : UnitRef, remaining : $[UnitRef])
    extends BaseFactionAction(Pilgrimage.styled(DC) + ": leave", implicit g => g.unit(unit).uclass.styled(DC) + " in " + g.unit(unit).region.toString)
    with Soft with PowerNeutral
case class DCPilgrimageDoneAction(self : Faction, prophet : UnitRef, dest : Region)
    extends BaseFactionAction(Pilgrimage.styled(DC), "Done".styled("power")) with Soft with PowerNeutral

// ── Dark Bargain (Action: Cost 0 — §1.10) ──────────────────────────────────
// HB Fix 93 (2026-06-07): rewrote Dark Bargain per user spec.
//   1. Each enemy faction secretly picks a D6 face. Their pick is NOT logged
//      until ALL enemies have chosen (then a single combined reveal line).
//   2. DC then chooses ONE face from the DISTINCT values enemies picked
//      (NOT 1..6). E.g. if enemies pick {1,3,5}, DC chooses from {1,3,5}.
//   3. DC gains N Sin (where N = chosen face). N Power is then DISTRIBUTED
//      to enemy factions as evenly as possible, whole numbers, NO subtraction.
//   4. Distribution prompts always ask for the rarer slot. Wording follows
//      user spec verbatim: single-slot "choose the faction that gets/only gets
//      X power"; multi-slot "choose the {ordinal} of {K} factions to receive
//      {X} power" with sequential picks removing chosen factions from the
//      list. On ties, prompt the LOW (lesser-power) slot.
case class DCDarkBargainMainAction(self : Faction)
    extends OptionFactionAction(DarkBargain.styled(DC) + " (Cost " + 0.power + ")")
    with MainQuestion with Soft
case class DCDarkBargainConfirmAction(self : Faction)
    extends BaseFactionAction(DarkBargain.styled(DC), "Confirm".styled("power")) with Soft
// Round-of-prompts: each enemy F secretly picks a D6 face. NO log line is
// emitted per-enemy — the picks are stored on game.dcDarkBargainPicks and
// revealed as a single combined line once all enemies are done.
// (self=enemy per G11 so menu border renders in enemy color.)
case class DCDarkBargainEnemyContinueAction(self : Faction, remaining : $[Faction])
    extends ForcedAction with PowerNeutral
case class DCDarkBargainEnemyPickAction(self : Faction, face : Int, remaining : $[Faction])
    extends BaseFactionAction(DarkBargain.styled(DC) + ": " + self.short.styled(self) + " secretly picks D6 face", implicit g => face.toString) with PowerNeutral
// DC's pick: only the DISTINCT faces enemies chose are offered.
case class DCDarkBargainChooseSinAction(self : Faction, face : Int)
    extends BaseFactionAction(DarkBargain.styled(DC) + ": choose face", implicit g => face.toString.styled("dc")) with PowerNeutral
// Distribution prompt: DC picks WHICH enemy gets the rarer power slot. The
// `slot` is the ordinal of this pick within the rarer-slot batch (0-indexed,
// total slots in `total`), `amt` is the power amount being assigned, and
// `otherAmt` is the amount the unprompted factions will auto-receive.
case class DCDarkBargainDistributeContinueAction(self : Faction, face : Int, remainingEnemies : $[Faction],
        baseline : Int, plusOneRemaining : Int, slotIndex : Int, totalSlots : Int, promptAmt : Int, autoAmt : Int)
    extends ForcedAction with PowerNeutral
case class DCDarkBargainDistributePickAction(self : Faction, face : Int, picked : Faction,
        baseline : Int, plusOneRemaining : Int, remainingEnemies : $[Faction],
        slotIndex : Int, totalSlots : Int, promptAmt : Int, autoAmt : Int)
    extends BaseFactionAction(DarkBargain.styled(DC) + ": assign", implicit g => picked.short.styled(picked) + " → " + promptAmt.power) with PowerNeutral {
    // Spec-exact prompt header per user (HB Fix 93):
    //  - totalSlots == 1: "choose the faction that {only} gets X power"
    //    (use "only" when promptAmt < autoAmt, no "only" otherwise)
    //  - totalSlots  > 1: "choose the {ordinal} of {totalSlots} factions to
    //    receive {promptAmt} power" — sequentially worded; user example was
    //    "first/second of two factions to receive 0 power".
    override def question(implicit game : Game) : String = {
        val header = DarkBargain.styled(DC) + " — "
        if (totalSlots == 1) {
            val only = if (promptAmt < autoAmt) "only " else ""
            header + "choose the faction that " + only + "gets " + promptAmt.power
        } else {
            val ord = DCDarkBargainOrdinals.ordinal(slotIndex)
            val cnt = DCDarkBargainOrdinals.numberWord(totalSlots)
            header + "choose the " + ord + " of " + cnt + " factions to receive " + promptAmt.power
        }
    }
}

// Ordinal/number-word helper for Dark Bargain distribution prompts (HB Fix 93).
// Used to render "first/second/third of two/three/four factions to receive X
// power" in the spec-exact wording.
object DCDarkBargainOrdinals {
    private val ordinals : Array[String] = Array(
        "first", "second", "third", "fourth", "fifth", "sixth"
    )
    private val numberWords : Array[String] = Array(
        "zero", "one", "two", "three", "four", "five", "six"
    )
    def ordinal(i : Int) : String =
        if (i >= 0 && i < ordinals.length) ordinals(i) else (i + 1).toString
    def numberWord(n : Int) : String =
        if (n >= 0 && n < numberWords.length) numberWords(n) else n.toString
}


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

        // ── SETUP: starting Power=4, Sin=0, 6 Acolytes on Faction-Card pool ──
        // HB Fix 78 (2026-06-06): DC's standard cultist pool starts EMPTY. All
        // 6 Acolytes are moved out of f.reserve into the separate faction-card
        // pool (DCFactionCardHold) so they cannot be summoned/recruited until
        // released by a Spellbook requirement. Death later sends them to the
        // standard pool (via game.eliminate → u.region = u.faction.reserve).
        case SetupFactionsAction if game.setup.has(DC) && !game.starting.contains(DC) =>
            val f = DC
            // Move all 6 Acolyte UnitFigures from f.reserve to the faction-card pool.
            // Husk/Prophet/Y'Golonac stay in f.reserve (standard pool) as normal.
            f.units.%(_.uclass == Acolyte).foreach { a =>
                a.region = DCFactionCardHold(f)
            }
            // Track which Spellbook each Acolyte is gated by (1-to-1 by library index).
            game.dcReservedSpellbookAcolytes = $(Proselytize, Satiate, Lure, Eschar, Pilgrimage, DarkBargain)
            // Mark DC as "placed" using a sentinel — Y'Golonac's first awaken
            // becomes the real Start Area (set in AwakenedAction handler below).
            // Use a placeholder Pool region so SetupFactionsAction loop unblocks.
            game.starting = game.starting + (DC -> f.reserve)
            // Starting power 4 (override default 0)
            f.power = 4
            f.log("starts with", 4.power + " and 0 Sin".styled("dc"))
            // HB Fix 99 (2026-06-08): log the starting Sin cap so the user sees
            // it is 10 at game start (2 × Ritual Marker position 5), NEVER 0.
            f.log("Sin cap:", game.dcSinCap.toString.styled("dc"), "(2 × Ritual Marker " + game.dcRitualMarkerPosition + " = " + game.dcSinCap + ")")
            f.log("places 6", Acolyte.styled(DC), "on Faction Card (one per Spellbook requirement)")
            // HB Fix 79: register DC Proselytize plan menu (Always prompt /
            // Prefer Acolytes on Gate / Prefer Acolytes off Gate). Default is
            // Always prompt — the human-style answer per spec.
            f.plans ++= $(DCProselytizePrompt, DCProselytizePreferOnGate, DCProselytizePreferOffGate)
            f.commands :+= DCProselytizePrompt
            Force(SetupFactionsAction)

        // ── Y'Golonac Awakening: set Start Area on first awaken ──────────────
        // HB Fix 97.A (2026-06-07): per user "Yes - no free gate". Rule §1.6/§3.4.3
        // is just "this becomes the Start Area" — no silent gate grant. Removed
        // game.gates :+= r and self.gates :+= r. Start Area marking preserved.
        case AwakenedAction(self : DC.type, YgolonacDC, r, _) =>
            // First-awaken sets the DC Start Area (per §1.6 / §3.4.3).
            if (game.starting.get(DC).contains(self.reserve)) {
                game.starting = game.starting + (DC -> r)
                self.log("Start Area set to", r, "(via Y'Golonac awakening)")
            }
            // Satisfy DarkBargain SBR (§3.12.6). Wrap in CheckSpellbooksAction (G28/Item 9).
            self.satisfyIf(DarkBargainReq, "Awoke Y'Golonac, Lord of Sin", true)
            Force(CheckSpellbooksAction(EndAction(self)))

        // ── PilgrimageReq: trigger on any RitualAction ───────────────────────
        // Also records Tenebrosum-eligible Ritual for the caster (§1.5.1 scope
        // includes Ritual). Audit 2026-06-06: prior code skipped Tenebrosum
        // recording on Ritual because the RitualAction case matched first.
        case a @ RitualAction(self, cost, _) if game.setup.has(DC) =>
            DC.satisfyIf(PilgrimageReq, "A player performed a Ritual of Annihilation", true)
            // HB Fix 84 (2026-06-07): record all costs (0 too) — Tenebrosum free-repeat for 0-cost.
            if (tenebrosumEligible(self)) recordTenebrosum(a, cost, "Ritual")
            UnknownContinue

        // ── SB acquisition: Reserved-Acolyte same-turn delivery (HB Fix 83) ──
        // HB Fix 83 (2026-06-07): No queue / no force-redirect. The standard
        // SpellbookAction handler in Game.scala records the SB in f.spellbooks.
        // The conditional unlimited action DCPlaceReservedAcolyteMainAction
        // becomes visible in DC's main menu the next time it renders — both in
        // the pre-acted main branch and the post-acted unlimited branch — so
        // DC can deliver the Faction-Card Acolyte THIS SAME TURN.
        // (No SpellbookAction case needed — base handler already does the work.)

        // ── MAIN ACTION ─────────────────────────────────────────────────────
        case MainAction(f : DC.type) if f.active.not =>
            UnknownContinue
        case MainAction(f : DC.type) if f.acted =>
            // Post-acted: still allow Tenebrosum repeat + controls + endTurn (G1)
            // HB Fix 90 (2026-06-07): dcTenebrosumExtraTurn flag retired — Tenebrosum
            // now replays the SAME action class via the chooser (not a fresh main),
            // and post-acted unlimited menu fires naturally after the replay ends.
            // HB Fix 109.E (2026-06-11): if the Tenebrosum guard is still set when
            // we arrive at the post-acted menu, it means the user Cancel'd from the
            // downstream chooser (the successful path clears the guard in EndAction).
            // Clear the stale guard + refund Sin to prevent the next action from
            // accidentally skipping its Power deduction.
            if (game.dcTenebrosumGuard) {
                game.dcTenebrosumGuard = false
                game.dcTenebrosumPrefixPending = false
                game.dcTenebrosumPrefixCost = 0
                game.dcTenebrosumMovePerUnit = false
            }
            implicit val asking = Asking(f)
            game.controls(f)
            if (f.hasAllSB)
                game.battles(f)
            // HB Fix 83 (2026-06-07): Reserved-Acolyte unlimited delivery —
            // visible whenever DC has an earned library SB whose Faction-Card
            // Acolyte hasn't been placed yet AND at least one Acolyte still in
            // the Faction-Card pool. Same-turn delivery supported.
            if (dcCanDeliverFactionCardAcolyte(f))
                + DCPlaceReservedAcolyteMainAction(f)
            // Tenebrosum: offer post-action repeat if last action recorded + Sin sufficient
            // 2026-06-06 Fix 75: hide once Tenebrosum has been used this turn.
            // HB Fix 84 (2026-06-07): 0-cost actions ARE eligible (free repeat),
            // per user spec. Sin check is game.dcSin >= cost (trivially true at 0).
            // HB Fix 90 (2026-06-07): also check the action class is still LEGAL
            // to repeat (pool/target/region availability) — see tenebrosumLegalToRepeat.
            // HB Fix 128 (2026-06-26): for broadened actions (Summon/Awaken), gate
            // on the cheapest available option, not the last unit's cost.
            game.dcLastActionForTenebrosum.foreach { case (a, cost, an) =>
                val minCost = tenebrosumMinSinCost(f, a, cost, an)
                if (game.dcSin >= minCost && !game.dcTenebrosumGuard && !game.dcTenebrosumUsedThisTurn
                    && tenebrosumLegalToRepeat(f, a, cost, an))
                    + DCTenebrosumMainAction(f, cost, an)
            }
            game.reveals(f)
            game.endTurn(f)(true)
            asking

        case MainAction(f : DC.type) =>
            // HB Fix 109.E (2026-06-11): stale guard cleanup (same as post-acted).
            if (game.dcTenebrosumGuard) {
                game.dcTenebrosumGuard = false
                game.dcTenebrosumPrefixPending = false
                game.dcTenebrosumPrefixCost = 0
                game.dcTenebrosumMovePerUnit = false
            }
            implicit val asking = Asking(f)

            // HB Fix 83 (2026-06-07): Reserved-Acolyte conditional unlimited
            // delivery — offered alongside normal main-menu options (NOT a
            // force-redirect). Visible whenever DC has an earned library SB
            // whose Faction-Card Acolyte hasn't been placed AND at least one
            // Acolyte remains in the Faction-Card pool. Once placed it just
            // functions as a normal Acolyte (death → standard pool).
            if (dcCanDeliverFactionCardAcolyte(f))
                + DCPlaceReservedAcolyteMainAction(f)

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
            // HB Fix 84 (2026-06-07): 0-cost actions ARE eligible (free repeat),
            // per user spec. Sin check is game.dcSin >= cost (trivially true at 0).
            // HB Fix 90 (2026-06-07): also check the action class is still LEGAL
            // to repeat (pool/target/region availability) — see tenebrosumLegalToRepeat.
            // HB Fix 128 (2026-06-26): for broadened actions (Summon/Awaken), gate
            // on the cheapest available option, not the last unit's cost.
            game.dcLastActionForTenebrosum.foreach { case (a, cost, an) =>
                val minCost = tenebrosumMinSinCost(f, a, cost, an)
                if (game.dcSin >= minCost && !game.dcTenebrosumGuard && !game.dcTenebrosumUsedThisTurn
                    && tenebrosumLegalToRepeat(f, a, cost, an))
                    + DCTenebrosumMainAction(f, cost, an)
            }

            game.libraryActions(f)
            game.highPriests(f)
            game.reveals(f)
            game.endTurn(f)(f.battled.any || game.nexed.any)

            asking

        // ── Tenebrosum: Soft prompt — opens "are you sure" confirm ───────────
        // HB Fix 102 (2026-06-08): defensive re-entry guard. If the player
        // clicks a STALE Tenebrosum offer (e.g. the chooser was Cancel'd back
        // to a snapshot Ask that still has the option), the flags
        // dcTenebrosumGuard/dcTenebrosumUsedThisTurn/dcLastActionForTenebrosum
        // may already be in their post-confirm state. Re-entering would
        // either crash (None last-action) or double-debit Sin. Detect the
        // stale state and bail to a freshly rendered MainAction (which will
        // NOT offer Tenebrosum because the flag checks now correctly fail).
        case DCTenebrosumMainAction(self, cost, an) =>
            if (game.dcTenebrosumGuard || game.dcTenebrosumUsedThisTurn || game.dcLastActionForTenebrosum.none) {
                // Stale menu — caller should never have been able to click. Clear
                // any leftover one-shot prefix flag (paranoia) and re-render the
                // main menu so the player sees the current legal options.
                game.dcTenebrosumPrefixPending = false
                game.dcTenebrosumPrefixCost = 0
                Force(MainAction(self))
            } else {
                implicit val asking = Asking(self)
                + DCTenebrosumRepeatAction(self, cost, an)
                + CancelAction
                asking
            }

        // ── Tenebrosum: Hard — debit Sin and replay the same action class ──
        // 2026-06-06 Fix 75: per-TURN used flag (NOT per-action). DC pays from
        // dcSin; SL (via the Ancient Sorcery permanent bundle) pays from slSin.
        // HB Fix 90 (2026-06-07): Tenebrosum no longer opens a fresh main menu
        // (which previously let DC take a 2nd power-paid main action). Instead
        // it refunds the recorded action's Power cost and re-opens the SAME
        // action-class chooser (Move / Build / Recruit / Summon / Capture /
        // Attack / Awaken / Ritual or DC SB chooser). After that one action
        // resolves, EndAction → AfterAction → PreMainAction → MainAction(self)
        // and since f.acted is still true the post-acted UNLIMITED branch fires
        // (NOT a second main menu).
        // HB Fix 94 (2026-06-07): repeat action follows the SAME interactive
        // menu flow as the original action (sin paid in lieu of power per
        // §1.5.1). Sin debit + power refund happen here, then control hands
        // off to tenebrosumRepeatChooser which routes through the existing
        // MainAction variants (Summon → region picker, Battle → region+enemy
        // picker, DC SB → confirm + downstream picks, etc.). No code in this
        // repeat path re-implements menu logic — it reuses the engine's own
        // OptionFactionAction / MainQuestion handlers.
        case DCTenebrosumRepeatAction(self, cost, an) =>
            // HB Fix 102 (2026-06-08): defensive re-entry guard. Fix 101 set
            // dcTenebrosumGuard=true here and cleared dcLastActionForTenebrosum
            // immediately, but never cleared the guard on the cancel path of
            // the downstream chooser (`tenebrosumRepeatChooser` → empty/Cancel).
            // The player could then click a STALE Tenebrosum offer in the
            // snapshot post-acted Ask and re-enter this handler with the
            // last-action already None, hitting UnknownContinue (= engine
            // crash). Detect any of the stale-state markers and bail to a
            // fresh MainAction so the next render reflects the true state
            // (which correctly suppresses the offer because the flags are
            // set). The full clear of the guard / used / prefix flags happens
            // in EndAction (when a real action completes) and in PreMainAction
            // at round start (belt-and-suspenders for paths where no action
            // ever resolved).
            if (game.dcTenebrosumGuard || game.dcLastActionForTenebrosum.none) {
                game.dcTenebrosumPrefixPending = false
                game.dcTenebrosumPrefixCost = 0
                return Force(MainAction(self))
            }
            game.dcLastActionForTenebrosum match {
                case Some((recordedAction, _, _)) =>
                    // HB Fix 108 (2026-06-10): per user — "Tenebrosum is also
                    // failing for movement - it would be one sin per unit moved,
                    // right now it is one sin to move All units." A normal Move
                    // Action moves any number of units (1 Power each), so the
                    // total Power spent — and thus the Sin cost per §1.5.1 /
                    // Fix 97.C — is 1 per unit moved, NOT a flat 1 for the whole
                    // repeat. For a Move repeat we therefore SKIP the flat
                    // upfront Sin debit here and instead charge 1 Sin per unit
                    // in the MoveAction handler (Game.scala), stopping the move
                    // loop when Sin is exhausted (MoveContinueAction). All other
                    // action classes keep the single upfront flat debit.
                    val isMoveRepeat = recordedAction.isInstanceOf[MoveAction]
                    // HB Fix 117 (2026-06-15): per owner — a Sin-paid Summon
                    // repeat must cost Sin equal to the POWER cost of the unit
                    // actually summoned (e.g. Fallen Prophet = 3 Sin), not the
                    // flat recorded `cost` of the unit summoned before. Because
                    // the broadened chooser (Fix 113) lets the player pick a
                    // DIFFERENT unit, the upfront flat debit here is wrong — it
                    // would charge the old unit's cost. So, exactly like a Move
                    // repeat, SKIP the upfront Sin debit for a Summon repeat and
                    // let the SummonAction handler (Game.scala) debit Sin at the
                    // picked unit's actual summonCost.
                    val isSummonRepeat = recordedAction.isInstanceOf[SummonAction]
                    // HB Fix 128: same logic for Awaken iGOO — the broadened
                    // chooser lets the player pick a DIFFERENT iGOO whose cost
                    // differs from the first. Skip upfront debit; the
                    // IndependentGOOAction handler debits Sin at pick time.
                    val isAwakenRepeat = recordedAction.isInstanceOf[AwakenAction] ||
                        recordedAction.isInstanceOf[IndependentGOOAction] ||
                        recordedAction.isInstanceOf[CthughaAwakenAction]
                    val skipUpfront = isMoveRepeat || isSummonRepeat || isAwakenRepeat
                    if (self == SL) {
                        if (!skipUpfront) game.slSin -= cost
                        game.slTenebrosumUsedThisTurn = true
                    } else {
                        if (!skipUpfront) game.dcSin -= cost
                        game.dcTenebrosumUsedThisTurn = true
                    }
                    // Signal the move loop to charge Sin per unit moved.
                    game.dcTenebrosumMovePerUnit = isMoveRepeat
                    game.dcTenebrosumGuard = true
                    // HB Fix 97.E (2026-06-07): per user "game log should show this
                    // as a tenebrosum action, and it should Not have the turn
                    // divider between it and the preceding normal action". Clear
                    // any pending DottedLine (buffered via pendingLine) so the
                    // Tenebrosum repeat does NOT inherit a turn-passed divider
                    // from a prior path.
                    game.pendingLine = None
                    // HB Fix 101 (2026-06-08): per user verbatim "HB DC- the game
                    // log for tenbrosum needs to always start with 'Defilers Court
                    // used Tenebrosum to'. Right now, because the game log just
                    // shows the action, the game engine (serializer? Something
                    // like that) is seeing it as a normal action, deducting power,
                    // and even running DC into negative power situations." Set a
                    // one-shot prefix flag that appendLog consumes on the very
                    // next log line — that line is the action handler's own log
                    // (e.g. "summoned Mindless Husk in Roundtree"), so the final
                    // log entry becomes "Defilers Court used Tenebrosum to
                    // summoned Mindless Husk in Roundtree (2 Sin)". No standalone
                    // pre-action log line is emitted (would have been double-
                    // prefixed and confused the serializer). Power refund is
                    // REMOVED: prior code refunded the recorded cost and let the
                    // handler re-debit, but if the current cost differed (tax /
                    // discount drift) the net could be negative. The handlers
                    // now skip self.power -= cost entirely when dcTenebrosumGuard
                    // is true (guarded inline in Game.scala) — so power is never
                    // touched and negative-power states cannot arise.
                    game.dcTenebrosumPrefixPending = true
                    game.dcTenebrosumPrefixFaction = self
                    // For a per-unit Move repeat, the first moved-unit log line
                    // is tagged "(1 Sin)" (each unit costs 1 Sin); subsequent
                    // units are charged + logged individually in the handler.
                    // HB Fix 117 (2026-06-15): for a Summon repeat the real Sin
                    // cost depends on the unit the player picks, so seed the
                    // prefix cost to 0 here; the SummonAction handler overwrites
                    // it with the picked unit's actual summonCost before it logs,
                    // so the log line shows the correct Sin amount.
                    game.dcTenebrosumPrefixCost =
                        if (isMoveRepeat) 1
                        else if (isSummonRepeat) 0
                        else if (isAwakenRepeat) 0
                        else cost
                    // Clear last-action so the SAME action isn't repeated as the "last" again
                    game.dcLastActionForTenebrosum = None
                    // Force the SAME chooser the recorded action came from. After
                    // it resolves, EndAction → unlimited menu (post-acted MainAction
                    // branch fires since f.acted is true and extra-turn flag isn't
                    // set — this is the Bug 2 fix per spec).
                    tenebrosumRepeatChooser(self, recordedAction)
                case None =>
                    UnknownContinue
            }

        // ── Tenebrosum broadened Summon chooser (HB Fix 113, 2026-06-13) ─────
        // Offer EVERY structurally-summonable unit (not just the one just
        // summoned). game.summons bypasses its Power-affordability filter under
        // dcTenebrosumGuard (set by DCTenebrosumRepeatAction), and SummonAction
        // skips the Power debit under the guard — so the Sin already paid covers
        // the repeat, and the player may pick a DIFFERENT unit. Cancel routes
        // back to MainAction, whose stale-guard cleanup (Fix 109.E) clears the
        // guard if the player backs out.
        case DCTenebrosumSummonChooserAction(self) =>
            implicit val asking = Asking(self)
            game.summons(self)
            + CancelAction
            asking

        // ── Tenebrosum broadened Awaken chooser (HB Fix 113, 2026-06-13) ─────
        // Offer EVERY awakenable faction GOO + iGOO (not just the one just
        // awakened). game.awakens bypasses affords under the guard, and
        // AwakenAction / IndependentGOOAction skip the Power debit under guard.
        // HB Fix 128: Only offer awakening options — NOT iGOO SBR actions
        // (Yig gate removal, Ghatanothoa pay-4, Tulzscha, etc.). Tenebrosum
        // copies only the SAME action type (Awaken), not side-effect powers.
        case DCTenebrosumAwakenChooserAction(self) =>
            implicit val asking = Asking(self)
            game.awakens(self)
            game.tenebrosumIndependentAwakensOnly(self)
            + CancelAction
            asking

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
            game.hires(f)
            game.doomDone(f)
            asking

        // ── Doom-Phase SB-Requirement opt-ins (wrap in CheckSpellbooksAction per Item 9) ─
        case DCProselytizeReqOptInAction(self) =>
            val enemyGOOs = game.factions.but(self)./~(_.allInPlay).%(_.uclass.utype == GOO).num
            self.satisfy(ProselytizeReq, "Doom Phase SBR: gain 2 Sin per enemy GOO")
            // HB Fix 96: clamp Sin grant to dcSinCap = 2 * ritualMarker
            val proselytizeWant   = 2 * enemyGOOs
            val proselytizeGained = game.grantDCSin(proselytizeWant)
            self.log("SBR: gained", proselytizeGained.toString.styled("dc"), "Sin (2 per", enemyGOOs, "enemy GOO)")
            if (proselytizeGained < proselytizeWant)
                self.log("SBR: Sin capped at", game.dcSinCap.toString.styled("dc"), "(2 × Ritual Marker " + game.dcRitualMarkerPosition + ")")
            Force(CheckSpellbooksAction(DoomAction(self)))

        case DCSatiateReqOptInAction(self) =>
            val otherSBs = self.spellbooks.num
            val poolSBs  = self.unfulfilled.num
            self.satisfy(SatiateReq, "Doom Phase SBR: +1 Power per other SB, +1 Sin per remaining pool SB")
            self.power += otherSBs
            // HB Fix 96: clamp Sin grant to dcSinCap = 2 * ritualMarker
            val satiateGained = game.grantDCSin(poolSBs)
            self.log("SBR: gained", otherSBs.power, "and", satiateGained.toString.styled("dc"), "Sin")
            if (satiateGained < poolSBs)
                self.log("SBR: Sin capped at", game.dcSinCap.toString.styled("dc"), "(2 × Ritual Marker " + game.dcRitualMarkerPosition + ")")
            Force(CheckSpellbooksAction(DoomAction(self)))

        // ── Reserved-Acolyte conditional unlimited delivery (HB Fix 83) ──────
        case DCPlaceReservedAcolyteMainAction(self) =>
            implicit val asking = Asking(self)
            // Offer every region on the map (any Area legal per §1.6).
            areas.foreach { r =>
                + DCPlaceReservedAcolyteAction(self, r)
            }
            + CancelAction
            asking

        case DCPlaceReservedAcolyteAction(self, r) =>
            // HB Fix 78 + 83: pull from the FACTION-CARD pool (DCFactionCardHold)
            // — NOT the standard pool. Standard-pool Acolytes (post-death) are
            // ungated and recruit/summon via normal mechanics. The SB consumed
            // is any earned library SB whose acolyte is still pending; we pick
            // the first eligible to decrement the visual marker.
            val placed = self.units.%(u => u.uclass == Acolyte && u.region == DCFactionCardHold(self)).headOption
            placed match {
                case Some(u) =>
                    u.region = r
                    // Decrement the SB tracker by the first earned-library SB
                    // whose Acolyte is still pending (visual marker bookkeeping).
                    // HB Fix 97.F (2026-06-07): per user "acolytes are on spell
                    // book requirements, not on spell books. Game log should
                    // credit the specific requirement for having been fulfilled."
                    // Map the earned SB to its matching SBR and credit the SBR
                    // name in the log instead of the SB name.
                    val sbToClear = game.dcReservedSpellbookAcolytes.%(sb => self.spellbooks.has(sb)).headOption
                    sbToClear.foreach { sb =>
                        game.dcReservedSpellbookAcolytes = game.dcReservedSpellbookAcolytes.but(sb)
                        val sbrText = game.dcLastFulfilledSBR./(_.text).|(sb.name)
                        self.log("placed", Acolyte.styled(DC), "for fulfilled SBR", sbrText.styled(DC), "in", r)
                    }
                    if (sbToClear.isEmpty)
                        self.log("placed", Acolyte.styled(DC), "from Faction Card in", r)
                case None =>
                    self.log("no Faction Card Acolyte available to place")
            }
            // Unlimited action — return to MainAction so DC can take more turns.
            Force(MainAction(self))

        // ── HB Fix 104: forced IMMEDIATE Acolyte delivery on SB acquisition ──
        // Forced region picker (no Cancel). Pulls one Acolyte from the
        // FACTION-CARD reserve pool (DCFactionCardHold) — NOT the standard pool.
        // After placement, Force(then) resumes the SB/ES loop that issued us.
        case DCDeliverReservedAcolyteForceAction(self, sb, then) =>
            // Defensive: if (somehow) no card Acolyte remains, just continue.
            val hasCardAcolyte = self.units.%(u => u.uclass == Acolyte && u.region == DCFactionCardHold(self)).any
            if (!hasCardAcolyte)
                Force(then)
            else {
                implicit val asking = Asking(self)
                // Every Area on the map is a legal placement target (§1.6).
                areas.foreach { r =>
                    + DCDeliverReservedAcolytePlaceAction(self, sb, r, then)
                }
                asking
            }

        case DCDeliverReservedAcolytePlaceAction(self, sb, r, then) =>
            val placed = self.units.%(u => u.uclass == Acolyte && u.region == DCFactionCardHold(self)).headOption
            placed match {
                case Some(u) =>
                    u.region = r
                    if (game.dcReservedSpellbookAcolytes.has(sb)) {
                        game.dcReservedSpellbookAcolytes = game.dcReservedSpellbookAcolytes.but(sb)
                        val sbrText = game.dcLastFulfilledSBR./(_.text).|(sb.name)
                        self.log("placed", Acolyte.styled(DC), "for fulfilled SBR", sbrText.styled(DC), "in", r)
                    } else {
                        self.log("placed", Acolyte.styled(DC), "from Faction Card in", r)
                    }
                case None =>
                    self.log("no Faction Card Acolyte available to place")
            }
            Force(then)

        // ── Satiate (cost 2, capture from each faction in Y'Golonac's area) ──
        case DCSatiateMainAction(self) =>
            implicit val asking = Asking(self)
            + DCSatiateConfirmAction(self)
            + CancelAction
            asking

        case DCSatiateConfirmAction(self) =>
            // HB Fix 101 (2026-06-08): on a Tenebrosum repeat (sin-paid), skip
            // the power debit — Sin was already debited in DCTenebrosumRepeatAction.
            if (!game.dcTenebrosumGuard)
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
            // HB Fix 97.G (2026-06-07): per user "Yes, add it." — when Satiate
            // captures only a single Cultist (e.g. just a DC self-Cultist with
            // no enemies in the area), 0 ES is correctly awarded but the log
            // was silent. Add an explicit explanatory log line.
            if (capturedSoFar > 0 && bonus == 0)
                self.log(Satiate.styled(DC) + ": no Elder Sign because no captures beyond the first")
            EndAction(self)

        // ── Lure (cost 1, force-move adjacent enemy cultists into Y'Golonac's area) ──
        case DCLureMainAction(self) =>
            implicit val asking = Asking(self)
            + DCLureConfirmAction(self)
            + CancelAction
            asking

        case DCLureConfirmAction(self) =>
            // HB Fix 101 (2026-06-08): on a Tenebrosum repeat (sin-paid), skip
            // the power debit — Sin was already debited in DCTenebrosumRepeatAction.
            if (!game.dcTenebrosumGuard)
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
                    // HB Fix 113 (2026-06-13): per owner — Lure is blocked out of
                    // any source Area where ANY enemy of DC (i.e. any faction other
                    // than DC itself, self) has a Great Old One, an independent
                    // Great Old One (iGOO), a Faction Building, or a Terror —
                    // INCLUDING the Cultist's own faction. This matches the
                    // verbatim card text ("Enemy Cultists in Areas containing an
                    // enemy Great Old One, Terror or Faction Building are exempt"),
                    // where "enemy" is from DC's perspective and so includes the
                    // Cultist's owner. Examples: CC's Nyarlathotep blocks luring
                    // CC's own Cultists out of that Area; AN's Cathedral (Building)
                    // or Yothan (Terror) blocks AN's Cultists; a controlled neutral
                    // Terror on map (Brown Jenkin, Dhole, ...) blocks the
                    // controlling faction's Cultists. `isGOO` covers GOO, iGOO and
                    // ElderGod (Bastet); controlled neutral Terrors are utype
                    // Terror on the controlling faction's units.
                    // (Prior code only checked factions OTHER than the Cultist owner
                    //  for GOOs, and any faction — including DC — for Terror/Building,
                    //  which both mis-scoped the exemption.)
                    val enemiesOfDC = game.factions.but(self)
                    val hasEnemyGOO        = enemiesOfDC.exists(o => o.at(r).%(_.uclass.isGOO).any)
                    val hasEnemyTerror     = enemiesOfDC.exists(o => o.at(r).%(_.uclass.utype == Terror).any)
                    val hasEnemyBuilding   = enemiesOfDC.exists(o => o.at(r).%(_.uclass.utype == Building).any)
                    // HB Fix 97.B (2026-06-07): per user "fix, this only applies to
                    // the Bubastis moon area". Prior code matched any region whose
                    // display name contained "Moon" (string match). Replaced with
                    // canonical BB.moon region equality — only the Bubastis Moon
                    // tile is exempted.
                    val isMoon = r == BB.moon
                    if (hasEnemyGOO || hasEnemyTerror || hasEnemyBuilding || isMoon) $()
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
            // HB Fix 101 (2026-06-08): on a Tenebrosum repeat (sin-paid), skip
            // the power debit — Sin was already debited in DCTenebrosumRepeatAction.
            if (!game.dcTenebrosumGuard)
                self.power -= 1
            recordTenebrosum(action, 1, "Pilgrimage")
            val p = game.unit(prophet)
            val src = p.region
            // Per-unit opt-in (Fix HB-77): rules say each OTHER DC unit in the
            // Prophet's source area gets its own choice (move with the Prophet
            // or stay behind). Iterate units via the Continue chain.
            val others = self.at(src).%(u => u.ref != prophet)./(_.ref)
            self.log(Pilgrimage.styled(DC) + ": Fallen Prophet moves to", dest)
            Force(DCPilgrimageUnitContinueAction(self, prophet, dest, others))

        case DCPilgrimageUnitContinueAction(self, prophet, dest, remaining) =>
            if (remaining.none) {
                EndAction(self)
            } else {
                implicit val asking = Asking(self)
                val u = remaining.first
                + DCPilgrimageUnitMoveAction(self, prophet, dest, u, remaining.dropStarting)
                + DCPilgrimageUnitStayAction(self, prophet, dest, u, remaining.dropStarting)
                + DCPilgrimageDoneAction(self, prophet, dest)
                asking
            }

        case DCPilgrimageUnitMoveAction(self, prophet, dest, unitRef, remaining) =>
            val u = game.unit(unitRef)
            val src = u.region
            u.region = dest
            self.log(Pilgrimage.styled(DC) + ": moved", u.uclass.styled(DC), "from", src, "to", dest)
            Force(DCPilgrimageUnitContinueAction(self, prophet, dest, remaining))

        case DCPilgrimageUnitStayAction(self, prophet, dest, unitRef, remaining) =>
            val u = game.unit(unitRef)
            self.log(Pilgrimage.styled(DC) + ": left", u.uclass.styled(DC), "in", u.region)
            Force(DCPilgrimageUnitContinueAction(self, prophet, dest, remaining))

        case DCPilgrimageDoneAction(self, prophet, dest) =>
            EndAction(self)

        // ── Dark Bargain (cost 0, secret D6 + even power distribution) ────────
        // HB Fix 93 (2026-06-07): rewritten per user spec. Enemy picks are
        // SECRET (no per-enemy log line); reveal is a single combined line.
        // DC chooses face from DISTINCT enemy picks (NOT 1..6). DC gains face
        // Sin and distributes face Power across enemies as evenly as possible
        // (whole numbers, NO subtraction). Rarer-slot prompts (single faction:
        // "choose the faction that {only} gets X power"; multi: "choose the
        // {ordinal} of K factions to receive X power"). Ties prompt LOW side.
        case DCDarkBargainMainAction(self) =>
            implicit val asking = Asking(self)
            + DCDarkBargainConfirmAction(self)
            + CancelAction
            asking

        case DCDarkBargainConfirmAction(self) =>
            recordTenebrosum(action, 0, "Dark Bargain")
            // Start secret round-of-prompts: each enemy picks a D6 face 1..6.
            game.dcDarkBargainPicks = $
            val enemies = game.factions.but(self)
            Force(DCDarkBargainEnemyContinueAction(self, enemies))

        case DCDarkBargainEnemyContinueAction(self, remaining) =>
            if (remaining.none) {
                // All enemies have picked. Reveal as a single combined line
                // (per user spec: "Do not expose their dice sides in the game
                // log until all have chosen.").
                val combined = game.dcDarkBargainPicks./((f, face) =>
                    f.short.styled(f) + "=" + face.toString).mkString(", ")
                DC.log(DarkBargain.styled(DC) + ": enemies revealed picks —", combined)
                // DC chooses ONE face from the DISTINCT values enemies picked.
                val distinctFaces : $[Int] = game.dcDarkBargainPicks./((_, pf) => pf).distinct.sortBy(x => x)
                implicit val asking = Asking(DC)
                distinctFaces.foreach { face =>
                    + DCDarkBargainChooseSinAction(DC, face)
                }
                asking
            } else {
                val e = remaining.first
                // Per-enemy D6 face pick (self=e for enemy-colored menu border per G11).
                // SECRET: stored on game state, not logged until reveal.
                implicit val asking = Asking(e)
                $(1, 2, 3, 4, 5, 6).foreach { face =>
                    + DCDarkBargainEnemyPickAction(e, face, remaining.dropStarting)
                }
                asking
            }

        case DCDarkBargainEnemyPickAction(self, face, remaining) =>
            // Store SILENTLY — no per-enemy log line. Reveal happens once all
            // enemies have picked (handled in EnemyContinueAction with empty
            // remaining branch).
            game.dcDarkBargainPicks :+= ((self, face))
            Force(DCDarkBargainEnemyContinueAction(DC, remaining))

        case DCDarkBargainChooseSinAction(self, face) =>
            // DC gains `face` Sin. NO subtraction from enemies. Distribute
            // `face` Power across enemies as evenly as possible (whole numbers).
            // HB Fix 96: clamp Sin grant to dcSinCap = 2 * ritualMarker
            val darkBargainGained = game.grantDCSin(face)
            self.log(DarkBargain.styled(DC) + ": DC chose face", face.toString.styled("dc") + "; gained", darkBargainGained.toString.styled("dc"), "Sin")
            if (darkBargainGained < face)
                self.log(DarkBargain.styled(DC) + ": Sin capped at", game.dcSinCap.toString.styled("dc"), "(2 × Ritual Marker " + game.dcRitualMarkerPosition + ")")
            val allEnemies = game.factions.but(self)
            val e          = allEnemies.num
            if (e == 0 || face == 0) {
                // Degenerate — no enemies (shouldn't happen) or face 0.
                game.dcDarkBargainFacedown = true
                game.dcDarkBargainPicks = $
                self.log(DarkBargain.styled(DC) + ": flipped facedown until Gather Power")
                EndAction(self)
            } else {
                val baseline   = face / e
                val remainder  = face % e
                val highCount  = remainder         // factions getting baseline+1
                val lowCount   = e - remainder     // factions getting baseline
                if (highCount == 0) {
                    // Clean division — every enemy gets baseline. No prompt.
                    allEnemies.foreach { ee =>
                        ee.power += baseline
                        self.log(DarkBargain.styled(DC) + ": gave", baseline.power, "to", ee.full)
                    }
                    game.dcDarkBargainFacedown = true
                    game.dcDarkBargainPicks = $
                    self.log(DarkBargain.styled(DC) + ": flipped facedown until Gather Power")
                    EndAction(self)
                } else {
                    // Pick rarer slot. On tie (highCount == lowCount), prompt
                    // the LOW (baseline) side per user spec example
                    // (2 power → 4 enemies → prompt 2 to receive 0).
                    val (promptCount, promptAmt, autoAmt) =
                        if (highCount < lowCount) (highCount, baseline + 1, baseline)
                        else                       (lowCount,  baseline,     baseline + 1)
                    Force(DCDarkBargainDistributeContinueAction(self, face, allEnemies,
                        baseline, promptCount, 0, promptCount, promptAmt, autoAmt))
                }
            }

        case DCDarkBargainDistributeContinueAction(self, face, remainingEnemies, baseline, plusOneRemaining, slotIndex, totalSlots, promptAmt, autoAmt) =>
            if (slotIndex >= totalSlots || remainingEnemies.none) {
                // All rarer-slot picks done. Award `autoAmt` to every
                // remaining enemy (the un-prompted ones).
                remainingEnemies.foreach { ee =>
                    ee.power += autoAmt
                    if (autoAmt > 0)
                        self.log(DarkBargain.styled(DC) + ": gave", autoAmt.power, "to", ee.full)
                }
                game.dcDarkBargainFacedown = true
                game.dcDarkBargainPicks = $
                self.log(DarkBargain.styled(DC) + ": flipped facedown until Gather Power")
                EndAction(self)
            } else {
                implicit val asking = Asking(self)
                remainingEnemies.foreach { ee =>
                    + DCDarkBargainDistributePickAction(self, face, ee, baseline, plusOneRemaining,
                        remainingEnemies, slotIndex, totalSlots, promptAmt, autoAmt)
                }
                asking
            }

        case DCDarkBargainDistributePickAction(self, face, picked, baseline, plusOneRemaining, remainingEnemies, slotIndex, totalSlots, promptAmt, autoAmt) =>
            // Award promptAmt to the chosen faction, drop it from remaining,
            // advance to next slot.
            picked.power += promptAmt
            if (promptAmt > 0)
                self.log(DarkBargain.styled(DC) + ": gave", promptAmt.power, "to", picked.full)
            else
                self.log(DarkBargain.styled(DC) + ":", picked.full, "receives", 0.power)
            val next = remainingEnemies.but(picked)
            Force(DCDarkBargainDistributeContinueAction(self, face, next, baseline, plusOneRemaining,
                slotIndex + 1, totalSlots, promptAmt, autoAmt))

        // ── Gather Power: flip Dark Bargain face-up again ─────────────────────
        case PowerGatherAction(_) if game.setup.has(DC) && game.dcDarkBargainFacedown =>
            game.dcDarkBargainFacedown = false
            UnknownContinue

        // ── Proselytize (Item 2): forced per-enemy drag chain on Move ──────
        // Fix HB-79 (2026-06-06): EVERY enemy with an Acolyte in the source
        // area MUST give up one Acolyte dragged to the destination along with
        // the moving DC Acolyte. NOT a yes/no opt-in. The only "choice" each
        // enemy gets is WHICH Acolyte — between an off-gate Acolyte and an
        // on-gate Acolyte when both exist. The DC player's DCProselytizePlan
        // command controls whether to prompt or auto-prefer one side. Pattern
        // matches GC Devolve / HP Unspeakable Oath / Gate Diplomacy "command
        // thing" + DC Satiate/Lure per-faction chain.
        case MovedAction(self : DC.type, u, o, r) if u.uclass == Acolyte && self.can(Proselytize) && o != r =>
            val enemies = game.factions.but(self).%(e => e.at(o).%(_.uclass == Acolyte).any)
            if (enemies.any) {
                Force(DCProselytizeFactionPickAction(self, o, r, enemies))
            } else {
                UnknownContinue
            }

        case DCProselytizeFactionPickAction(self, area, to, remaining) =>
            if (remaining.none) {
                // All enemies processed — fall back to standard post-move chain.
                MoveContinueAction(DC, true)
            } else {
                val e = remaining.first
                val acolytes = e.at(area).%(_.uclass == Acolyte)
                if (acolytes.none) {
                    // Defensive: enemy may have lost Acolytes from this area mid-chain.
                    Force(DCProselytizeFactionPickAction(self, area, to, remaining.dropStarting))
                } else if (acolytes.num == 1) {
                    // Only one Acolyte — no choice, auto-drag.
                    val c = acolytes.first
                    c.region = to
                    e.log(Proselytize.styled(DC) + ":", e.short.styled(e), Acolyte.styled(e),
                        if (c.onGate) "(on gate)" else "(off gate)", "dragged from", area, "to", to)
                    Force(DCProselytizeFactionPickAction(self, area, to, remaining.dropStarting))
                } else {
                    val offGate = acolytes.%(c => !c.onGate)
                    val onGate  = acolytes.%(c => c.onGate)
                    if (offGate.none || onGate.none) {
                        // All Acolytes share gate status — no real choice, auto-pick first.
                        val c = acolytes.first
                        c.region = to
                        e.log(Proselytize.styled(DC) + ":", e.short.styled(e), Acolyte.styled(e),
                            if (c.onGate) "(on gate)" else "(off gate)", "dragged from", area, "to", to,
                            "(no choice — all", acolytes.num.toString, "same status)")
                        Force(DCProselytizeFactionPickAction(self, area, to, remaining.dropStarting))
                    } else if (DC.commands.has(DCProselytizePreferOnGate)) {
                        // DC plan: auto-pick the on-gate Acolyte.
                        val c = onGate.first
                        c.region = to
                        e.log(Proselytize.styled(DC) + ":", e.short.styled(e), Acolyte.styled(e),
                            "(on gate)", "dragged from", area, "to", to, "(DC prefers on gate)")
                        Force(DCProselytizeFactionPickAction(self, area, to, remaining.dropStarting))
                    } else if (DC.commands.has(DCProselytizePreferOffGate)) {
                        // DC plan: auto-pick the off-gate Acolyte.
                        val c = offGate.first
                        c.region = to
                        e.log(Proselytize.styled(DC) + ":", e.short.styled(e), Acolyte.styled(e),
                            "(off gate)", "dragged from", area, "to", to, "(DC prefers off gate)")
                        Force(DCProselytizeFactionPickAction(self, area, to, remaining.dropStarting))
                    } else {
                        // Default DCProselytizePrompt: present off-gate vs on-gate choice.
                        // G11: self=e for enemy-colored menu border.
                        implicit val asking = Asking(e)
                        + DCProselytizePickCultistAction(e, area, to, offGate.first.ref, remaining.dropStarting)
                        + DCProselytizePickCultistAction(e, area, to, onGate.first.ref, remaining.dropStarting)
                        asking
                    }
                }
            }

        case DCProselytizePickCultistAction(self, area, to, cultistRef, remaining) =>
            val c = game.unit(cultistRef)
            val from = c.region
            c.region = to
            self.log(Proselytize.styled(DC) + ":", self.short.styled(self), Acolyte.styled(self),
                if (c.onGate) "(on gate)" else "(off gate)", "dragged from", from, "to", to)
            Force(DCProselytizeFactionPickAction(DC, area, to, remaining))

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
        case MoveAction(self, u, o, r, cost) if tenebrosumEligible(self) =>
            // HB Fix 84 (2026-06-07): record all move costs (including 0 — e.g.
            // free moves) so Tenebrosum free-repeat for 0-cost actions works.
            recordTenebrosum(action, cost, "Move")
            UnknownContinue
        case BuildGateAction(self, _) if tenebrosumEligible(self) =>
            // HB Fix 97.C (2026-06-07): per user "the cost, in sin, of a tenebrosum
            // action is the total power spent on that action". Build Gate's actual
            // power deduction in Game.scala (BuildGateAction handler) is
            // `3 - self.has(UmrAtTawil).??(1)` — record the same expression here
            // instead of the hardcoded 3 so the Sin cost matches the actual power
            // spent (covers future UmrAtTawil/DC interactions).
            recordTenebrosum(action, 3 - self.has(UmrAtTawil).??(1), "Build Gate")
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
        // HB Fix 115 (2026-06-14): awakening an INDEPENDENT GOO does not produce an
        // AwakenAction (it produces IndependentGOOAction / CthughaAwakenAction), so
        // Tenebrosum was never recorded and the "Repeat Awaken" button never appeared
        // after an iGOO awaken. Record these the same way (they carry their own cost).
        case IndependentGOOAction(self, _, _, cost) if tenebrosumEligible(self) =>
            recordTenebrosum(action, cost, "Awaken")
            UnknownContinue
        case CthughaAwakenAction(self, _, _, cost) if tenebrosumEligible(self) =>
            recordTenebrosum(action, cost, "Awaken")
            UnknownContinue

        case EndAction(self) if tenebrosumEligible(self) =>
            // Clear the guard flag if we were in a Tenebrosum repeat (one-shot).
            if (game.dcTenebrosumGuard) {
                game.dcTenebrosumGuard = false
                game.dcLastActionForTenebrosum = None
                // HB Fix 101: defensively clear the one-shot prefix flag in case
                // some action path resolved without emitting any log line.
                game.dcTenebrosumPrefixPending = false
                game.dcTenebrosumPrefixCost = 0
                // HB Fix 108: clear the per-unit Move-repeat marker.
                game.dcTenebrosumMovePerUnit = false
            }
            UnknownContinue

        // 2026-06-06 Fix 75: per-TURN reset on PreMainAction. The used flag is
        // cleared at the start of each new turn so Tenebrosum becomes available
        // again. Also clears any stale last-action recorded from a prior turn.
        // HB Fix 84 (2026-06-07): GUARD with !self.acted. PreMainAction fires
        // AFTER EVERY action of self (engine routes EndAction → AfterAction →
        // PreMainAction(self) at Game.scala line 3696). Previously this handler
        // nuked dcLastActionForTenebrosum the moment an action ended, so the
        // post-acted MainAction(f : DC.type) branch never saw a recorded
        // action and the Tenebrosum repeat offer never rendered. Round-start
        // is the only PreMainAction where self.acted is still false (cleared
        // by NextPlayerAction at Game.scala 3712). Restrict reset to that.
        case PreMainAction(self) if tenebrosumEligible(self) && !self.acted =>
            if (self == SL) game.slTenebrosumUsedThisTurn = false
            else            game.dcTenebrosumUsedThisTurn = false
            game.dcLastActionForTenebrosum = None
            // HB Fix 102 (2026-06-08): also clear the guard + one-shot prefix
            // flags at round-start. Fix 101 set dcTenebrosumGuard=true on the
            // confirm path but only cleared it on EndAction. If the chooser
            // was canceled (no inner action ever ran → no EndAction), the
            // guard persisted into the next turn and silently suppressed
            // power deductions for non-Tenebrosum actions. PreMainAction
            // round-start (self.acted=false) is the canonical reset point.
            game.dcTenebrosumGuard = false
            game.dcTenebrosumPrefixPending = false
            game.dcTenebrosumPrefixCost = 0
            // HB Fix 108: belt-and-suspenders clear of the Move-repeat marker.
            game.dcTenebrosumMovePerUnit = false
            UnknownContinue

        case _ => UnknownContinue
    }

    // Helper: record the just-resolved action + its Power cost + display name.
    // Non-recursive: skipped if we're already inside a Tenebrosum-driven repeat.
    // Fix HB-77 (2026-06-06): Tenebrosum explicitly does NOT track or offer a
    // repeat for Control Gate or Abandon Gate. Both are free Common Actions
    // that should not be repeatable via Sin (per user directive).
    // HB Fix 84 (2026-06-07): per user spec, 0-cost actions (e.g. Dark Bargain)
    // ARE Tenebrosum-eligible (free repeat — no Sin debit). Removed cost > 0
    // guard. Sin-sufficiency is enforced separately at the offer site.
    private def recordTenebrosum(a : Action, cost : Int, an : String)(implicit game : Game) : Unit = {
        a match {
            case _ : ControlGateAction => return
            case _ : AbandonGateAction => return
            case _ =>
        }
        if (!game.dcTenebrosumGuard) {
            game.dcLastActionForTenebrosum = Some((a, cost, an))
        }
    }

    // HB Fix 128 (2026-06-26): the minimum Sin cost to offer Tenebrosum for
    // broadened action types (Summon/Awaken) is the cheapest available option,
    // not the cost of the unit last used. For all other action types, the
    // recorded cost is still the correct gate.
    private def tenebrosumMinSinCost(self : Faction, a : Action, recordedCost : Int, an : String)(implicit game : Game) : Int = a match {
        case _ : SummonAction =>
            val summonAreas = areas ++ ((self == BB).??($(BB.moon)))
            val accessibleAreas = summonAreas.nex.%(self.canAccessGate)
            if (accessibleAreas.none) Int.MaxValue
            else {
                val candidates = self.pool.monsterly./(_.uclass).distinct.%(_.canBeSummoned(self))
                if (candidates.none) Int.MaxValue
                else candidates./(uc => accessibleAreas./(r => self.summonCost(uc, r)).min).min
            }
        case _ : AwakenAction | _ : IndependentGOOAction | _ : CthughaAwakenAction =>
            val hasPoolGOO = self.pool.%(_.uclass.isGOO).any
            val heldIGOOs = self.loyaltyCards.of[IGOOLoyaltyCard]
            val costs = {
                (if (hasPoolGOO) self.pool.%(_.uclass.isGOO)./(_.uclass).distinct./(uc =>
                    areas./~(r => self.awakenCost(uc, r)).some./(_.min).|(self.gooValue(uc))
                ) else $()) ++
                heldIGOOs./(_.power)
            }
            if (costs.any) costs.min else Int.MaxValue
        case _ => recordedCost
    }

    // 2026-06-06 Fix 75: a faction is Tenebrosum-eligible if it's DC, or if it's
    // SL holding the permanent DC bundle via Ancient Sorcery (Fix 2).
    private def tenebrosumEligible(self : Faction)(implicit game : Game) : Boolean =
        self == DC || (self == SL && game.slPermanentBorrowed.has(Tenebrosum))

    // HB Fix 90 (2026-06-07): Public wrapper so FactionSL can call the same
    // legality check for its borrowed-Tenebrosum offer.
    // HB Fix 92 (2026-06-07): now takes the recorded cost so the legality
    // check can account for the power refund Tenebrosum applies before the
    // replay chooser opens. SL call-site updated accordingly.
    def tenebrosumLegalToRepeatPublic(self : Faction, a : Action, cost : Int, an : String)(implicit game : Game) : Boolean =
        tenebrosumLegalToRepeat(self, a, cost, an)

    // HB Fix 128 (2026-06-26): Public wrapper for the min-sin-cost computation
    // so SL's borrowed-Tenebrosum offer can use the same broadened gate.
    def tenebrosumMinSinCostPublic(self : Faction, a : Action, cost : Int, an : String)(implicit game : Game) : Int =
        tenebrosumMinSinCost(self, a, cost, an)

    // HB Fix 90 (2026-06-07): Bug 1 — Tenebrosum can only be OFFERED if the
    // recorded action class is still legally repeatable. Pool empty, no valid
    // region, no remaining target → no offer. Re-uses the same predicates the
    // engine uses when deciding whether to render the original main-action.
    //
    // HB Fix 92 (2026-06-07): bug — the affordability checks below used
    // self.affords(n)(r) which requires self.power >= taxIn(r) + n at CURRENT
    // power. But Tenebrosum REFUNDS the recorded action's power cost before
    // the chooser fires (Sin is paid in lieu of Power). So the legality test
    // must use post-refund power (self.power + cost), otherwise repeatable
    // actions like "Summon Mindless Husk for 1 power" get hidden when the
    // player has 0 power remaining despite having Sin + pool + gate. User
    // bug 2026-06-07: "I just summon a mindless husk for 1 power. I have
    // 3 sin. 4 mindless husks in pool. It should have prompted me to
    // summon Another mindless husk for 1 sin." The recorded cost is threaded
    // through so each affords check considers the refund.
    private def tenebrosumLegalToRepeat(self : Faction, a : Action, cost : Int, an : String)(implicit game : Game) : Boolean = {
        // HB Fix 105 (2026-06-10): per user — "Tenebrosum doesn't seem to be
        // prompted when the player has run out of power." Tenebrosum pays SIN in
        // lieu of Power (§1.5.1), so the legality test must NOT require Power at
        // all. Sin-sufficiency is already enforced at the offer site
        // (game.dcSin >= cost). Previously these branches gated on post-refund
        // power affordability (self.power + cost >= taxIn + n), which suppressed
        // the offer whenever DC was at/near 0 power even though Sin could pay —
        // the user-reported bug. The checks below now verify only STRUCTURAL
        // legality (pool has the unit, a valid target/region exists), exactly
        // mirroring how BG ghoul-summon-with-Thousand-Young and FB Call of the
        // Faithful remain offered at 0 power. `cost`/`an` retained for signature
        // compatibility with the public wrapper / SL borrow path.
        a match {
            // MOVE — any of self's units still un-Moved. No power gate (Sin pays).
            case _ : MoveAction =>
                self.units.nex.onMap.not(Moved).%(_.canMove).any
            // BUILD GATE — any gate-less area where self has a gate-controller.
            // HB Fix 107 (2026-06-10): also covers the user-reported "Tenebrosum
            // failing to allow the player to duplicate the create gate action"
            // — Build/Create Gate IS repeatable; only Control/Abandon Gate are
            // excluded (in recordTenebrosum). No power gate (Sin pays).
            case _ : BuildGateAction =>
                areas.nex.%!(game.gates.has).%(r => self.at(r).%(_.canControlGate).any).any
            // RECRUIT — pool has a cultist + at least one Area present/legal.
            case RecruitAction(_, uc, _) =>
                self.pool(uc).any && areas.%(self.present).some.|(areas).nex.any
            // SUMMON — HB Fix 113 (2026-06-13): broadened repeat may bring out a
            // DIFFERENT unit, so the offer appears whenever ANY summonable unit
            // (faction monster or neutral loyalty-card monster in pool) has an
            // accessible gate area — not only the exact unit just summoned.
            case _ : SummonAction =>
                val summonAreas = areas ++ ((self == BB).??($(BB.moon)))
                summonAreas.nex.%(self.canAccessGate).any &&
                    self.pool.monsterly./(_.uclass).distinct.%(_.canBeSummoned(self)).any
            // CAPTURE — any area with an eligible enemy cultist target.
            case _ : CaptureAction =>
                areas.nex.%(r => game.factionlike.but(self).%(self.canCapture(r)).any).any
            // BATTLE — any region with a valid attack target that self HAS NOT
            // already battled in this turn (CW rules: 1 battle per region per turn).
            case _ : AttackAction =>
                areas.nex.diff(self.battled).%(r => game.factionlike.but(self).exists(self.canAttack(r))).any
            // AWAKEN — HB Fix 113 (2026-06-13): broadened repeat may awaken a
            // DIFFERENT GOO or iGOO, so the offer appears whenever ANY faction GOO
            // remains in pool OR any iGOO loyalty card is held and awakenable —
            // not only the exact GOO just awakened (e.g. after awakening Y'Golonac
            // the player may use Tenebrosum to awaken a held iGOO).
            case _ : AwakenAction | _ : IndependentGOOAction | _ : CthughaAwakenAction =>
                areas.nex.any && {
                    val hasPoolGOO = self.pool.%(_.uclass.isGOO).any
                    val hasIGOO    = game.loyaltyCards.of[IGOOLoyaltyCard].any ||
                                     self.loyaltyCards.of[IGOOLoyaltyCard].any
                    hasPoolGOO || hasIGOO
                }
            // RITUAL — at least one gate to ritual at.
            case _ : RitualAction =>
                self.allGates.any
            // DC SB Satiate — needs Y'Golonac on map + an enemy cultist in its
            // area. No power gate (Sin pays).
            case _ : DCSatiateConfirmAction =>
                self.can(Satiate) && self.allInPlay.%(_.uclass == YgolonacDC).any &&
                    self.allInPlay.%(_.uclass == YgolonacDC).exists(yg => game.factions.but(self).exists(e => e.at(yg.region).%(_.uclass.utype == Cultist).any))
            // DC SB Lure — needs Y'Golonac on map + an adjacent eligible enemy
            // cultist. No power gate (Sin pays).
            case _ : DCLureConfirmAction =>
                self.can(Lure) && self.allInPlay.%(_.uclass == YgolonacDC).any &&
                    self.allInPlay.%(_.uclass == YgolonacDC).exists { yg =>
                        val adj = game.board.connected(yg.region)
                        game.factions.but(self).exists(e => adj./~(e.at).%(_.uclass.utype == Cultist).any)
                    }
            // DC SB Pilgrimage — needs a Fallen Prophet on map. No power gate (Sin pays).
            case _ : DCPilgrimageDestAction =>
                self.can(Pilgrimage) && self.allInPlay.%(_.uclass == FallenProphet).any
            // DC SB Dark Bargain — Y'Golonac on map, not facedown this round.
            case _ : DCDarkBargainConfirmAction =>
                self.can(DarkBargain) && self.allInPlay.%(_.uclass == YgolonacDC).any && !game.dcDarkBargainFacedown
            // Default: if we don't recognize the action, assume legal (conservative).
            case _ => true
        }
    }

    // HB Fix 90 (2026-06-07): Bug 2 — open the SAME action-class chooser as
    // the recorded action (so DC re-picks the unit / target / region) rather
    // than re-issuing the exact same action or opening the full main menu.
    // Power was already refunded by the caller; the chooser will re-debit it
    // (net zero). After the action resolves, the engine routes to the
    // post-acted unlimited menu naturally.
    // HB Fix 94 (2026-06-07): each branch below maps the recorded action to
    // its existing engine-side MainAction handler, which in turn opens the
    // SAME interactive prompt the player would see in the original action
    // flow. Per user spec: summons prompt for region, battles prompt with
    // region+enemy variants in a single grouped Ask, captures prompt for
    // region+enemy, DC SB actions re-open their confirm + downstream picker
    // chains. No menu logic is duplicated here — Force(XxxMainAction) hands
    // off to the canonical engine path. Sin is paid in lieu of power (debit
    // + refund handled in the DCTenebrosumRepeatAction case above).
    private def tenebrosumRepeatChooser(self : Faction, a : Action)(implicit game : Game) : Continue = a match {
        // HB Fix 105/107 (2026-06-10): the region/target lists below are NO
        // LONGER power-gated by self.affords(...). Tenebrosum pays Sin in lieu
        // of Power, and the handlers skip the power debit under dcTenebrosumGuard
        // (no refund is applied), so a self.affords(cost) filter at current power
        // would wrongly empty the picker whenever DC is at/near 0 power — the
        // user-reported "not prompted when out of power" (Fix 105) and "create
        // gate fails to duplicate" (Fix 107) bugs. Structural legality only.
        case _ : MoveAction               => Force(MoveMainAction(self))
        case _ : BuildGateAction          =>
            val l = areas.nex.%!(game.gates.has).%(r => self.at(r).%(_.canControlGate).any).some.|($)
            Force(BuildGateMainAction(self, l))
        case RecruitAction(_, uc, _)      =>
            val l = areas.%(self.present).some.|(areas).nex.some.|($)
            Force(RecruitMainAction(self, uc, l))
        case _ : SummonAction             =>
            // HB Fix 113 (2026-06-13): per owner — a Sin-paid Summon repeat may
            // summon a DIFFERENT unit than the one just summoned (reverses the
            // Fix 106 same-kind-only restriction). Open a chooser that offers
            // every structurally-summonable unit (faction monsters + any neutral
            // loyalty-card monster in pool), not just the recorded `uc`.
            Force(DCTenebrosumSummonChooserAction(self))
        case _ : CaptureAction            =>
            val l = areas.nex.%(r => game.factionlike.but(self).%(self.canCapture(r)).any).some.|($)
            Force(CaptureMainAction(self, l, None))
        case _ : AttackAction             =>
            val l = areas.nex.diff(self.battled).%(r => game.factionlike.but(self).exists(self.canAttack(r))).some.|($)
            Force(AttackMainAction(self, l, None))
        case _ : AwakenAction | _ : IndependentGOOAction | _ : CthughaAwakenAction =>
            // HB Fix 113 (2026-06-13): per owner — a Sin-paid Awaken repeat may
            // awaken a DIFFERENT GOO or iGOO than the one just awakened (reverses
            // the same-kind restriction). Open a chooser that offers every
            // awakenable faction GOO + every awakenable iGOO, not just the
            // recorded `uc`.
            Force(DCTenebrosumAwakenChooserAction(self))
        // RitualAction has no chooser — re-issue the recorded action directly.
        // Cost was refunded by caller; the RitualAction handler re-debits it.
        case _ : RitualAction             => Force(a)
        case _ : DCSatiateConfirmAction   => Force(DCSatiateMainAction(self))
        case _ : DCLureConfirmAction      => Force(DCLureMainAction(self))
        case _ : DCPilgrimageDestAction   => Force(DCPilgrimageMainAction(self))
        case _ : DCDarkBargainConfirmAction => Force(DCDarkBargainMainAction(self))
        // Fallback: re-issue the recorded action directly.
        case _                            => Force(a)
    }

    // HB Fix 83 (2026-06-07): Conditional unlimited Faction-Card Acolyte
    // delivery — visible whenever (a) at least one Acolyte still in the
    // Faction-Card pool (DCFactionCardHold) AND (b) at least one earned
    // library SB whose matching Faction-Card Acolyte has not yet been
    // delivered. Per spec: "use acolyte faction card pool minus spell books
    // earned" — i.e. only earned SBs unlock delivery; remaining SBs stay
    // gated. Death/capture-return routes Acolytes to standard pool, not here.
    private def dcCanDeliverFactionCardAcolyte(f : Faction)(implicit game : Game) : Boolean = {
        val acolytesInFactionCard = f.units.%(u => u.uclass == Acolyte && u.region == DCFactionCardHold(f)).num
        val earnedSBsStillPending = game.dcReservedSpellbookAcolytes.%(sb => f.spellbooks.has(sb)).num
        acolytesInFactionCard > 0 && earnedSBsStillPending > 0
    }

    // HB Fix 104 (2026-06-10): true when DC has just earned library spellbook
    // `sb` and that SB's reserved Acolyte is still on the Faction Card — i.e.
    // an immediate forced placement is owed. Called by Game.scala's core
    // SpellbookAction handler to force DCDeliverReservedAcolyteForceAction
    // before the SB/ES loop continues. Only DC library SBs gate Acolytes;
    // Tenebrosum/Depravity (abilities) and neutral SBs never do.
    def dcShouldDeliverOnAcquire(sb : Spellbook)(implicit game : Game) : Boolean =
        game.dcReservedSpellbookAcolytes.has(sb) &&
        DC.units.%(u => u.uclass == Acolyte && u.region == DCFactionCardHold(DC)).any

    // HB Fix 97.F (2026-06-07): Map a DC spellbook to its matching SBR so the
    // reserved-Acolyte placement log can credit the SPECIFIC requirement that
    // got fulfilled (per user: "acolytes are on spell book requirements, not
    // on spell books. Game log should credit the specific requirement for
    // having been fulfilled.").
    private def dcSBRForSB(sb : Spellbook) : Requirement = sb match {
        case Proselytize => ProselytizeReq
        case Satiate     => SatiateReq
        case Lure        => LureReq
        case Eschar      => EscharReq
        case Pilgrimage  => PilgrimageReq
        case DarkBargain => DarkBargainReq
        case _           => ProselytizeReq // unreachable for DC SBs; fallback
    }
}


// Doom-phase SB-requirement opt-in actions
// HB Fix 109.D (2026-06-11): dynamic computed values per user spec.
// Proselytize: "SBR: +X Sin (2/ enemy GOO)"
// Satiate: "SBR: +X Power, +Y Sin (1P/ claimed SB, 1S/ unclaimed SB)" where X+Y=6
case class DCProselytizeReqOptInAction(self : Faction)
    extends OptionFactionAction(implicit g => {
        val enemyGOOs = g.factions.but(self)./~(_.allInPlay).%(_.uclass.utype == GOO).num
        val total = 2 * enemyGOOs
        "SBR".styled(DC) + ": " + ("+" + total + " Sin").styled("dc") + " (2/ enemy GOO)"
    })
    with DoomQuestion
case class DCSatiateReqOptInAction(self : Faction)
    extends OptionFactionAction(implicit g => {
        val claimedSBs = self.spellbooks.num
        val unclaimedSBs = 6 - claimedSBs
        "SBR".styled(DC) + ": " + ("+" + claimedSBs + " Power").styled("power") + ", " + ("+" + unclaimedSBs + " Sin").styled("dc") + " (1P/ claimed SB, 1S/ unclaimed SB)"
    })
    with DoomQuestion
