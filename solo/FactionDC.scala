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
    extends OptionFactionAction(
        // HB Fix 84 (2026-06-07): cost==0 (e.g. Dark Bargain) renders as a
        // "free repeat" per user spec; cost>0 renders the Sin price.
        Tenebrosum.styled(DC) + ": Repeat " + actionName.styled(self) +
            (if (cost > 0) " for " + cost.toString.styled("dc") + " Sin" else " (free)"))
    with MainQuestion with Soft with PowerNeutral
case class DCTenebrosumRepeatAction(self : Faction, cost : Int, actionName : String)
    extends ForcedAction with PowerNeutral

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
            f.log("places 6", Acolyte.styled(DC), "on Faction Card (one per Spellbook requirement)")
            // HB Fix 79: register DC Proselytize plan menu (Always prompt /
            // Prefer Acolytes on Gate / Prefer Acolytes off Gate). Default is
            // Always prompt — the human-style answer per spec.
            f.plans ++= $(DCProselytizePrompt, DCProselytizePreferOnGate, DCProselytizePreferOffGate)
            f.commands :+= DCProselytizePrompt
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
            game.dcLastActionForTenebrosum.foreach { case (a, cost, an) =>
                if (game.dcSin >= cost && !game.dcTenebrosumGuard && !game.dcTenebrosumUsedThisTurn
                    && tenebrosumLegalToRepeat(f, a, cost, an))
                    + DCTenebrosumMainAction(f, cost, an)
            }
            game.reveals(f)
            game.endTurn(f)(true)
            asking

        case MainAction(f : DC.type) =>
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
            game.dcLastActionForTenebrosum.foreach { case (a, cost, an) =>
                if (game.dcSin >= cost && !game.dcTenebrosumGuard && !game.dcTenebrosumUsedThisTurn
                    && tenebrosumLegalToRepeat(f, a, cost, an))
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
        case DCTenebrosumRepeatAction(self, cost, an) =>
            game.dcLastActionForTenebrosum match {
                case Some((recordedAction, _, _)) =>
                    if (self == SL) {
                        game.slSin -= cost
                        game.slTenebrosumUsedThisTurn = true
                    } else {
                        game.dcSin -= cost
                        game.dcTenebrosumUsedThisTurn = true
                    }
                    game.dcTenebrosumGuard = true
                    // HB Fix 84 (2026-06-07): 0-cost repeat ("free repeat") logs
                    // without the Sin-spent phrase to match user spec wording.
                    if (cost > 0)
                        self.log(Tenebrosum.styled(DC) + ": spent", cost.toString.styled("dc"), "Sin to Repeat", an.styled(self))
                    else
                        self.log(Tenebrosum.styled(DC) + ": free repeat of", an.styled(self))
                    // Clear last-action so the SAME action isn't repeated as the "last" again
                    game.dcLastActionForTenebrosum = None
                    // Refund the recorded cost so the replay chooser nets zero
                    // power (Sin paid in lieu of Power per Tenebrosum semantics).
                    if (cost > 0) self.power += cost
                    // Force the SAME chooser the recorded action came from. After
                    // it resolves, EndAction → unlimited menu (post-acted MainAction
                    // branch fires since f.acted is true and extra-turn flag isn't
                    // set — this is the Bug 2 fix per spec).
                    tenebrosumRepeatChooser(self, recordedAction)
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
                    val sbToClear = game.dcReservedSpellbookAcolytes.%(sb => self.spellbooks.has(sb)).headOption
                    sbToClear.foreach { sb =>
                        game.dcReservedSpellbookAcolytes = game.dcReservedSpellbookAcolytes.but(sb)
                        self.log("placed", Acolyte.styled(DC), "from", sb.styled(DC), "Faction Card slot in", r)
                    }
                    if (sbToClear.isEmpty)
                        self.log("placed", Acolyte.styled(DC), "from Faction Card in", r)
                case None =>
                    self.log("no Faction Card Acolyte available to place")
            }
            // Unlimited action — return to MainAction so DC can take more turns.
            Force(MainAction(self))

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
            game.dcSin += face
            self.log(DarkBargain.styled(DC) + ": DC chose face", face.toString.styled("dc") + "; gained", face.toString.styled("dc"), "Sin")
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
        // Post-refund affordability: same shape as self.affords(n)(r) =
        // power >= taxIn(r) + n, but using (power + cost) since the recorded
        // cost will be refunded before the chooser opens.
        def affordsRefund(n : Int)(r : Region) : Boolean = self.power + cost >= self.taxIn(r) + n
        a match {
            // MOVE — any of self's units still un-Moved. Power gate: post-refund
            // (self.power + cost) > 0; since recorded Move costs are typically 1
            // and refund restores them, this trivially holds for the same-region
            // repeat case but a different region with higher tax could fail.
            case _ : MoveAction =>
                self.units.nex.onMap.not(Moved).%(_.canMove).any && (self.power + cost) > 0
            // BUILD GATE — any area without a gate where self has a gate-controller.
            case _ : BuildGateAction =>
                areas.nex.%(affordsRefund(3 - self.has(UmrAtTawil).??(1))).%!(game.gates.has).%(r => self.at(r).%(_.canControlGate).any).any
            // RECRUIT — pool has a cultist + at least one valid area (post-refund affords).
            case RecruitAction(_, uc, _) =>
                self.pool(uc).any && areas.%(self.present).some.|(areas).nex.%(r => affordsRefund(self.recruitCost(uc, r))(r)).any
            // SUMMON — pool has the unit + at least one valid gate area (post-refund affords).
            case SummonAction(_, uc, _) =>
                val summonAreas = areas ++ ((self == BB).??($(BB.moon)))
                self.pool(uc).any && summonAreas.nex.%(r => affordsRefund(self.summonCost(uc, r))(r)).%(self.canAccessGate).any
            // CAPTURE — any area with an eligible enemy cultist target.
            case _ : CaptureAction =>
                areas.nex.%(affordsRefund(1)).%(r => game.factionlike.but(self).%(self.canCapture(r)).any).any
            // BATTLE — any region with a valid attack target that self HAS NOT
            // already battled in this turn (CW rules: 1 battle per region per turn).
            case _ : AttackAction =>
                areas.nex.%(affordsRefund(1)).diff(self.battled).%(r => game.factionlike.but(self).exists(self.canAttack(r))).any
            // AWAKEN — pool has the GOO (e.g. Y'Golonac: only 1 in pool — if awoken, no offer).
            case AwakenAction(_, uc, _, _) =>
                self.pool(uc).any && areas.nex.%(r => affordsRefund(self.awakenCost(uc, r).|(999))(r)).any
            // RITUAL — at least one gate to ritual at.
            case _ : RitualAction =>
                self.allGates.any
            // DC SB Satiate — needs Y'Golonac on map, post-refund 2 power, and
            // at least one enemy cultist in Y'Golonac's area.
            case _ : DCSatiateConfirmAction =>
                self.can(Satiate) && (self.power + cost) >= 2 && self.allInPlay.%(_.uclass == YgolonacDC).any &&
                    self.allInPlay.%(_.uclass == YgolonacDC).exists(yg => game.factions.but(self).exists(e => e.at(yg.region).%(_.uclass.utype == Cultist).any))
            // DC SB Lure — needs Y'Golonac on map, post-refund 1 power, and at
            // least one adjacent enemy cultist eligible.
            case _ : DCLureConfirmAction =>
                self.can(Lure) && (self.power + cost) >= 1 && self.allInPlay.%(_.uclass == YgolonacDC).any &&
                    self.allInPlay.%(_.uclass == YgolonacDC).exists { yg =>
                        val adj = game.board.connected(yg.region)
                        game.factions.but(self).exists(e => adj./~(e.at).%(_.uclass.utype == Cultist).any)
                    }
            // DC SB Pilgrimage — needs a Fallen Prophet on map, post-refund 1 power.
            case _ : DCPilgrimageDestAction =>
                self.can(Pilgrimage) && (self.power + cost) >= 1 && self.allInPlay.%(_.uclass == FallenProphet).any
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
    private def tenebrosumRepeatChooser(self : Faction, a : Action)(implicit game : Game) : Continue = a match {
        case _ : MoveAction               => Force(MoveMainAction(self))
        case _ : BuildGateAction          =>
            val l = areas.nex.%(self.affords(3 - self.has(UmrAtTawil).??(1))).%!(game.gates.has).%(r => self.at(r).%(_.canControlGate).any).some.|($)
            Force(BuildGateMainAction(self, l))
        case RecruitAction(_, uc, _)      =>
            val l = areas.%(self.present).some.|(areas).nex.%(r => self.affords(self.recruitCost(uc, r))(r)).some.|($)
            Force(RecruitMainAction(self, uc, l))
        case SummonAction(_, uc, _)       =>
            val summonAreas = areas ++ ((self == BB).??($(BB.moon)))
            val l = summonAreas.nex.%(r => self.affords(self.summonCost(uc, r))(r)).%(self.canAccessGate).some.|($)
            Force(SummonMainAction(self, uc, l))
        case _ : CaptureAction            =>
            val l = areas.nex.%(self.affords(1)).%(r => game.factionlike.but(self).%(self.canCapture(r)).any).some.|($)
            Force(CaptureMainAction(self, l, None))
        case _ : AttackAction             =>
            val l = areas.nex.%(self.affords(1)).diff(self.battled).%(r => game.factionlike.but(self).exists(self.canAttack(r))).some.|($)
            Force(AttackMainAction(self, l, None))
        case AwakenAction(_, uc, _, _)    =>
            val l = areas.nex.%(r => self.affords(self.awakenCost(uc, r).|(999))(r)).some.|($)
            Force(AwakenMainAction(self, uc, l))
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
}


// Doom-phase SB-requirement opt-in actions
case class DCProselytizeReqOptInAction(self : Faction)
    extends OptionFactionAction(("Take " + Proselytize.name).styled(DC) + ": +2 Sin per enemy GOO")
    with DoomQuestion
case class DCSatiateReqOptInAction(self : Faction)
    extends OptionFactionAction(("Take " + Satiate.name).styled(DC) + ": +1 Power per other SB, +1 Sin per pool SB")
    with DoomQuestion
