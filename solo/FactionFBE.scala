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
//    (one entry per Unit that died this Action; the Boolean = "was FBE-controlled").
//    Pushed in FBEExpansion.eliminate (universal death hook), evaluated and cleared
//    in FBEExpansion.afterAction (§1.5.1 / §3.6).
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
case object SuccorReq               extends Requirement("Byagoona Dies in Battle")
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
        // (§1.8). Pip >= 4 is a Pain or Kill face (4/5 Pain, 6 Kill). All other
        // FBE units use their printed cost. Neutral Monsters use neutralStrength.
        val byagoonaCount = units.%(_.uclass == Byagoona).num
        val byagoonaStr   = (byagoonaCount > 0).??(game.fbeCardDice.count(_ >= 4))
        units.%(u => u.uclass != Byagoona).not(Zeroed)./(_.uclass.cost).sum +
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
    extends ForcedAction with PowerNeutral with Soft
case class ByagoonaAwakenDoneAction(self : Faction, r : Region, picked : $[UnitRef])
    extends BaseFactionAction(("Awaken " + Byagoona.name).styled(FBE), "Done".styled("power"))
// Replay-safe roll capture: the N rolled dice (pip values) are baked into this action.
case class ByagoonaAwakenRollAction(self : Faction, r : Region, picked : $[UnitRef], rolls : $[Int])
    extends ForcedAction

// ── DISTRIBUTED DEATH (Post-Battle Kill mitigation, §1.8 / §3.4.4 / §4.6) ────
case class DistributedDeathMainAction(self : Faction, n : Int)
    extends OptionFactionAction(implicit g => ("Distributed Death".styled(FBE) + ": discard " + n + " dice to prevent " + n + " " + ("Kill".s(n)).styled("kill"))) with PostBattleQuestion
case class DistributedDeathSkipAction(self : Faction)
    extends OptionFactionAction("Distributed Death".styled(FBE) + ": skip") with PostBattleQuestion

// ── NECROMANTIC SPORES (Post-Battle, §1.10 SB2 / §3.10.2 / §4.4.2) ───────────
case class NecromanticSporesMainAction(self : Faction, n : Int)
    extends OptionFactionAction(implicit g => ("Necromantic Spores".styled(FBE) + ": Eliminate a Monster you control to spawn " + n + " " + FungalThrall.name.styled(FBE))) with PostBattleQuestion
case class NecromanticSporesSkipAction(self : Faction)
    extends OptionFactionAction("Necromantic Spores".styled(FBE) + ": skip") with PostBattleQuestion
case class NecromanticSporesEliminateAction(self : Faction, monster : UnitRef, r : Region, n : Int)
    extends BaseFactionAction("Necromantic Spores".styled(FBE) + ": Eliminate", implicit g => g.unit(monster).uclass.styled(FBE))

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
    extends OptionFactionAction(implicit g => ("Animated Rush".styled(FBE) + ": discard " + n + " dice, carry " + (2 * n) + " " + ("Unit".s(2 * n)).styled(FBE))) { override def question(implicit game : Game) = "Animated Rush".styled(FBE) }
case class AnimatedRushSkipAction(self : Faction)
    extends OptionFactionAction("Animated Rush".styled(FBE) + ": skip") { override def question(implicit game : Game) = "Animated Rush".styled(FBE) }
case class AnimatedRushPickAction(self : Faction, source : Region, dest : Region, k : Int, picked : $[UnitRef], remaining : $[UnitRef])
    extends ForcedAction with PowerNeutral with Soft
case class AnimatedRushDoneAction(self : Faction, source : Region, dest : Region, k : Int, picked : $[UnitRef])
    extends BaseFactionAction("Animated Rush".styled(FBE), "Done".styled("power"))

// ── SUCCOR (Doom Phase, §1.10 SB5 / §3.10.5 / §4.4.5) ────────────────────────
case class SuccorMainAction(self : Faction)
    extends OptionFactionAction(Succor.styled(FBE)) with DoomQuestion with Soft
case class SuccorSkipAction(self : Faction)
    extends OptionFactionAction(("Skip " + Succor.name).styled(FBE)) with DoomQuestion with Soft
case class SuccorPickAction(self : Faction, picked : $[UnitRef], remaining : $[UnitRef])
    extends ForcedAction with PowerNeutral with Soft
case class SuccorRollAction(self : Faction, picked : $[UnitRef], rolls : $[Int])
    extends ForcedAction

// ── OVERLORD OF DEATH (Ongoing pay-cost substitution, §1.10 SB6 / §3.10.6) ───
// Handled inline by the Necromantic-Spores requirement / Awaken flows that need a
// cost; a dedicated MainAction lets FBE proactively convert Monsters → Power.
case class OverlordOfDeathMainAction(self : Faction)
    extends OptionFactionAction(OverlordOfDeath.styled(FBE) + ": Eliminate a Monster for " + 1.power) with MainQuestion
case class OverlordOfDeathEliminateAction(self : Faction, monster : UnitRef)
    extends BaseFactionAction(OverlordOfDeath.styled(FBE) + ": Eliminate", implicit g => g.unit(monster).uclass.styled(FBE))

// ── NECROMANTIC SPORES REQUIREMENT — Eliminate Two Fungal Thralls (§3.12.2) ──
// 0 Power Common Action; the two Eliminations DO trigger Self Consuming (+1 Power).
case class EliminateTwoFungalThrallsMainAction(self : Faction)
    extends OptionFactionAction(("Eliminate two " + FungalThrall.name + "s").styled(FBE)) with MainQuestion with Soft
case class EliminateTwoFungalThrallsPickAction(self : Faction, picked : $[UnitRef], remaining : $[UnitRef])
    extends ForcedAction with PowerNeutral with Soft
case class EliminateTwoFungalThrallsDoneAction(self : Faction, picked : $[UnitRef])
    extends BaseFactionAction(("Eliminate two " + FungalThrall.name + "s").styled(FBE), "Done".styled("power"))


// ============================================================================
// FACELESS BLIGHT (FBE) EXPANSION — game-loop integration
// ============================================================================
object FBEExpansion extends Expansion {

    // Derive a combat FACE from a stored card-die pip value (§1.8 / §2.4).
    def face(pip : Int) : BattleRoll = if (pip >= 6) Kill else if (pip >= 4) Pain else Miss

    // ── SELF CONSUMING death tally (§1.5.1 / §3.6) ───────────────────────────
    // Universal death hook: every Unit eliminated during the active Action (battle
    // or otherwise) is recorded. The Boolean records whether the dying Unit was
    // FBE-controlled (so the +1 Doom 3-controlled clause can be checked).
    override def eliminate(u : UnitFigure)(implicit game : Game) {
        if (!game.setup.has(FBE)) return
        game.fbeSelfConsumingDeaths :+= (u.faction == FBE)
    }

    // ── SELF CONSUMING resolution (fires at end of every Action) (§3.6) ───────
    override def afterAction()(implicit game : Game) {
        if (!game.setup.has(FBE)) return
        val deaths = game.fbeSelfConsumingDeaths
        if (deaths.num >= 2) {
            FBE.power += 1
            FBE.log(SelfConsuming.styled(FBE) + ": 2+ Units died — gained", 1.power)
            if (deaths.count(_ == true) >= 3) {
                FBE.doom += 1
                FBE.log(SelfConsuming.styled(FBE) + ": FBE controlled 3+ — also gained", 1.doom)
            }
        }
        game.fbeSelfConsumingDeaths = $
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
        case SetupFactionsAction if game.setup.has(FBE) && !game.starting.contains(FBE) && game.board.starting(FBE).none =>
            // Fallback only — if no legal empty/non-adjacent area exists, seat FBE
            // with a placeholder so the setup loop unblocks (per §2.0a).
            val taken = game.starting.values.$
            val placeholder = areas.%(r => !taken.has(r)).headOption.|(areas.last)
            game.starting = game.starting + (FBE -> placeholder)
            FBE.log("INPUT NEEDED: no empty non-adjacent start area; placed at", placeholder)
            Force(SetupFactionsAction)

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

            // Succor — offer once per Doom phase if acquired and FBE has any unit.
            if (f.can(Succor) && f.units.%(_.region.onMap).any)
                + SuccorMainAction(f)

            + DoomDoneAction(f)

            asking

        // ── MAIN ACTION (§2.7 / §3.10) ───────────────────────────────────────
        case MainAction(f : FBE.type) if f.active.not =>
            UnknownContinue

        case MainAction(f : FBE.type) if f.acted =>
            UnknownContinue

        case MainAction(f : FBE.type) =>
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

            // Byagoona custom Awaken (§1.8): offer if Byagoona is in the pool and
            // FBE controls at least one Monster to sacrifice.
            if (f.pool.%(_.uclass == Byagoona).any && controlledMonstersAnywhere.any)
                + ByagoonaAwakenMainAction(f)

            // Necromantic Spores requirement — Eliminate Two Fungal Thralls (§3.12.2).
            if (f.needs(NecromanticSporesReq) && f.onMap(FungalThrall).num >= 2)
                + EliminateTwoFungalThrallsMainAction(f)

            // Overlord of Death (§1.10 SB6): proactively convert a Monster → 1 Power.
            if (f.can(OverlordOfDeath) && f.all(Byagoona).any && controlledMonstersAnywhere.any)
                + OverlordOfDeathMainAction(f)

            game.neutralSpellbooks(f)
            game.libraryActions(f)
            game.highPriests(f)
            game.reveals(f)

            game.endTurn(f)(f.battled.any || game.nexed.any)

            asking

        // ── BYAGOONA AWAKEN (§1.8 / §3.4.1) ──────────────────────────────────
        case ByagoonaAwakenMainAction(self) =>
            val areasWithMonsters = controlledMonstersAnywhere./(_.region).distinct
            Ask(self).each(areasWithMonsters)(r => ByagoonaAwakenAreaAction(self, r)).cancel

        case ByagoonaAwakenAreaAction(self, r) =>
            // Begin a multi-select of Monsters in this area to Eliminate (1+).
            Force(ByagoonaAwakenPickAction(self, r, $, controlledMonsters(r)./(_.ref)))

        case ByagoonaAwakenPickAction(self, r, picked, remaining) =>
            implicit val asking = Asking(self)
            remaining.foreach { ur =>
                + ByagoonaAwakenPickAction(self, r, picked :+ ur, remaining.but(ur))
                    .as("Add " + game.unit(ur).uclass.styled(FBE))
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
        // FBE picks an enemy Monster (Soft), then rolls one die FROM the card
        // (RollD6, replay-safe) → the terminal resolve action carries the pip.
        // The actual battle resume is driven by Battle.scala's matching cases.
        case ShapestealingPreBattleAction(self) =>
            game.battle match {
                case Some(b) =>
                    val enemySide = if (b.attacker == FBE) b.defenders else b.attackers
                    val enemyMonsters = enemySide.forces.%(u => u.faction != FBE && u.uclass.utype == Monster)
                    Ask(self).each(enemyMonsters)(u => ShapestealingTargetAction(self, u.ref)).cancel
                case None => UnknownContinue
            }

        case ShapestealingTargetAction(self, enemyMonster) =>
            RollD6(_ => Shapestealing.styled(FBE) + ": roll a die from your Faction Card",
                pip => ShapestealingResolveAction(self, enemyMonster, pip))

        case ShapestealingResolveAction(self, enemyMonster, roll) =>
            // Discard one card die (the lowest-value die, deterministic for replay)
            // — a card die IS consumed (§1.10 SB3 Resolved).
            if (game.fbeCardDice.any)
                game.fbeCardDice = game.fbeCardDice.sortBy(x => x).drop(1)
            val mon = game.unit(enemyMonster)
            val cost = mon.uclass.cost
            if (roll > cost) {
                game.fbeShapestolen :+= enemyMonster
                self.log(Shapestealing.styled(FBE) + ": discarded card die (value", roll.toString + "); vs Cost",
                    cost.toString + " —", mon.uclass.styled(mon.faction), "fights for", FBE.full, "this Combat")
            }
            else
                self.log(Shapestealing.styled(FBE) + ": discarded card die (value", roll.toString + "); vs Cost",
                    cost.toString + " — failed")
            UnknownContinue   // Battle.scala ShapestealingResolveAction case resumes via proceed()

        case ShapestealingSkipAction(self) =>
            self.log("Skip ".styled(FBE) + Shapestealing.styled(FBE))
            UnknownContinue

        // ── ANIMATED RUSH (§1.10 SB4 / §3.10.4) ──────────────────────────────
        // Animated Rush triggers after Byagoona performs a (Common-Action) Move.
        case MovedAction(self : FBE.type, u, from, to)
            if game.unit(u).uclass == Byagoona && self.can(AnimatedRush) && game.fbeCardDice.nonEmpty
               && self.at(from).%(_.uclass != Byagoona).any =>
            implicit val asking = Asking(self)
            val maxK = math.min(game.fbeCardDice.num, self.at(from).%(_.uclass != Byagoona).num / 2)
            (1 to maxK).foreach(k => + AnimatedRushMainAction(self, from, to, k))
            + AnimatedRushSkipAction(self)
            asking

        case AnimatedRushMainAction(self, source, dest, n) =>
            Force(AnimatedRushPickAction(self, source, dest, n, $, self.at(source).%(_.uclass != Byagoona)./(_.ref)))

        case AnimatedRushSkipAction(self) =>
            self.log("Animated Rush".styled(FBE) + ": declined")
            Force(MoveContinueAction(self, true))

        case AnimatedRushPickAction(self, source, dest, k, picked, remaining) =>
            implicit val asking = Asking(self)
            val need = 2 * k
            if (picked.num < need)
                remaining.foreach { ur =>
                    + AnimatedRushPickAction(self, source, dest, k, picked :+ ur, remaining.but(ur))
                        .as("Add " + game.unit(ur).uclass.styled(FBE))
                }
            if (picked.num == need)
                + AnimatedRushDoneAction(self, source, dest, k, picked)
            + CancelAction
            asking

        case AnimatedRushDoneAction(self, source, dest, k, picked) =>
            // Discard k dice (lowest values, deterministic) from the Faction Card.
            game.fbeCardDice = game.fbeCardDice.sortBy(x => x).drop(k)
            picked.foreach(ur => game.unit(ur).region = dest)
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
                    .as("Add " + game.unit(ur).uclass.styled(FBE))
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
                val sum = rolls.sum
                val ritual = game.ritualCost
                if (sum > ritual) {
                    self.takeES(1)
                    self.log(Succor.styled(FBE) + ": Eliminated", y, "Units, rolled", sum.toString,
                        "vs Ritual Marker", ritual.toString + " — gained", 1.es)
                }
                else
                    self.log(Succor.styled(FBE) + ": Eliminated", y, "Units, rolled", sum.toString,
                        "vs Ritual Marker", ritual.toString + " — no Elder Sign")
                EndAction(self)
            }

        // ── OVERLORD OF DEATH (§1.10 SB6 / §3.10.6) ──────────────────────────
        case OverlordOfDeathMainAction(self) =>
            Ask(self).each(controlledMonstersAnywhere)(u =>
                OverlordOfDeathEliminateAction(self, u.ref)).cancel

        case OverlordOfDeathEliminateAction(self, monster) =>
            game.eliminate(game.unit(monster))
            self.power += 1
            self.log(OverlordOfDeath.styled(FBE) + ": Eliminated a Monster for", 1.power)
            EndAction(self)

        // ── NECROMANTIC SPORES REQUIREMENT (§3.12.2) ─────────────────────────
        case EliminateTwoFungalThrallsMainAction(self) =>
            Force(EliminateTwoFungalThrallsPickAction(self, $, self.onMap(FungalThrall)./(_.ref)))

        case EliminateTwoFungalThrallsPickAction(self, picked, remaining) =>
            implicit val asking = Asking(self)
            if (picked.num < 2)
                remaining.foreach { ur =>
                    + EliminateTwoFungalThrallsPickAction(self, picked :+ ur, remaining.but(ur))
                        .as("Add " + game.unit(ur).uclass.styled(FBE))
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

        case _ => UnknownContinue
    }
}

// Helper action referenced by the Succor d6-roll chain.
case class SuccorRollStartAction(self : Faction, picked : $[UnitRef]) extends ForcedAction

// Changeling Adherents Gather-Power roll chain (§3.8.3) — rolls one die per
// gate-controlling Acolyte, captured in the action for replay safety.
case class ChangelingAdherentsRollAction(self : Faction, remaining : Int, rolled : $[Int]) extends ForcedAction
