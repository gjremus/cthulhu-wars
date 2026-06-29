package cws

import hrf.colmat._

import html._


// ============================================================================
// BUBASTIS (BB) UNITS
// EarthCat: Monster cost 1 — starts on Moon, counts as Cultist while on Moon (Lunacy)
// CatFromMars: Monster cost 2
// CatFromSaturn: Monster cost 3
// CatFromUranus: Monster cost 4
// Bastet: ElderGod cost 6 — does not roll dice; no Elder Sign contribution on Ritual
// ============================================================================
case object EarthCat      extends FactionUnitClass(BB, "Earth Cat",       Monster,  1)
case object CatFromMars   extends FactionUnitClass(BB, "Cat from Mars",   Monster,  2)
case object CatFromSaturn extends FactionUnitClass(BB, "Cat from Saturn", Monster,  3)
case object CatFromUranus extends FactionUnitClass(BB, "Cat from Uranus", Monster,  4)
case object Bastet        extends FactionUnitClass(BB, "Bastet",          ElderGod, 6)


// ============================================================================
// BUBASTIS (BB) SPELLBOOKS
// Standard six: Catabolism, Zagazig, Savagery, Predator, Catnapping, Ailurophobia
// Alt-variant (BBAlternateSpellbooks option): Catabolism→Syzygy, Ailurophobia→Carnivore
// ============================================================================
case object Catabolism   extends FactionSpellbook(BB, "Catabolism")
case object Zagazig      extends FactionSpellbook(BB, "Zagazig")    with BattleSpellbook
case object Savagery     extends FactionSpellbook(BB, "Savagery")   with BattleSpellbook
case object Predator     extends FactionSpellbook(BB, "Predator")   with BattleSpellbook
case object Catnapping   extends FactionSpellbook(BB, "Catnapping")
case object Ailurophobia extends FactionSpellbook(BB, "Ailurophobia")

// Alt spellbooks (active when BBAlternateSpellbooks game option is on)
case object Syzygy    extends FactionSpellbook(BB, "Syzygy")
case object Carnivore extends FactionSpellbook(BB, "Carnivore") with BattleSpellbook

// FACTION POWER — Lunacy is BB's signature unique ability (always-on).
// Earth Cats count as Cultists for enemy-targeting effects (Zingaya, Ghroth, Dreams,
// He Who Is Not To Be Named). Earth Cats cannot be captured. Implementation lives
// in Game.scala / implicits.scala; this case object exposes Lunacy as a borrowable
// faction signature (for Ancient Sorcery and the per-faction status panel).
case object Lunacy extends FactionSpellbook(BB, "Lunacy")

// Bastet's per-GOO doom-phase ritual ability. Declared as a FactionSpellbook so
// every reference uses the FCG-blessed `RequiresAttention.styled(BB)` form
// (FCG line 717) rather than literal-string-styled.
case object RequiresAttention extends FactionSpellbook(BB, "Requires Attention")


// ============================================================================
// BUBASTIS (BB) SPELLBOOK REQUIREMENTS
// ============================================================================
case object Pay2ForBB           extends Requirement("As your Action, pay 2 Power")
case object NoEarthCatsOnMoon   extends Requirement("No Earth Cats on the Moon")
case object CatInEveryEnemyStart extends Requirement("A Cat is in every enemy Faction's Start Area")
case object MarsOrSaturnLost    extends Requirement("A Cat from Mars or Saturn is Killed or Eliminated")
case object UranusLost          extends Requirement("A Cat from Uranus is Killed or Eliminated")
case object AwakenBastet        extends Requirement("Awaken Bastet")


// BBAlternateSpellbooks GameOption — declared in Game.scala (sealed trait restriction)


// ============================================================================
// BUBASTIS (BB) FACTION OBJECT
// ============================================================================
case object BB extends Faction { f =>
    def name  = "Bubastis"
    def short = "BB"
    def style = "bb"

    override def abilities = $(Lunacy)
    override def library = $(Catabolism, Zagazig, Savagery, Predator, Catnapping, Ailurophobia)
    override def requirements(options : $[GameOption]) =
        $(Pay2ForBB, NoEarthCatsOnMoon, CatInEveryEnemyStart, MarsOrSaturnLost, UranusLost, AwakenBastet)

    // BB cannot recruit High Priests at all (no Acolytes, no HP in pool, no Hierophants HP)
    override val canRecruitHP = false

    // Moon hold: declared here so BB.moon is accessible everywhere.
    // MoonHold region added in Task 3.2.1; this is the forward reference.
    // (MoonHold case class lives in Game.scala — Task 3.2.1)
    lazy val moon : MoonHold = MoonHold(BB)

    // CRIT-8: BB has NO Acolytes per spec §1.6 / §2.5. Earth Cats fill the
    // Cultist-targeting role via Lunacy, but they remain Monsters, not Acolytes.
    val allUnits =
        1.times(Bastet)        ++
        6.times(EarthCat)      ++
        2.times(CatFromMars)   ++
        2.times(CatFromSaturn) ++
        2.times(CatFromUranus)

    override def awakenCost(u : UnitClass, r : Region)(implicit game : Game) : |[Int] = u match {
        // Bastet: cost 6; requires one of each Cat variety in play (Moon counts
        // as in-play — a cat on Earth or on the Moon satisfies its variety),
        // and no enemy units in the target region. Per user correction
        // (v2.4.4): "The Moon is in play. If at least one of each cat variety
        // is either on earth, or on the moon, then the Bastet requirement is
        // met." Use `f.all(uclass)` (region.inPlay) instead of `f.onMap(uclass)`
        // so Moon-resident cats count toward the variety check.
        case Bastet =>
            (f.all(EarthCat).any &&
             f.all(CatFromMars).any &&
             f.all(CatFromSaturn).any &&
             f.all(CatFromUranus).any &&
             f.enemies.forall(e => e.at(r).none)).?(6)
        case _ => None
    }

    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int = {
        // Bastet does not roll dice — her combat contribution is handled via Battle.scala
        // hook (Task 3.4.2). Here we count every non-Bastet unit normally.
        // V8 audit cleanup: removed unused bastetCount/bastetStr locals — Bastet contribution lives in the Battle hook.
        // Combat values: EarthCat 0, CatFromMars 1, CatFromSaturn 2, CatFromUranus 3
        // (each is 1 less than their summon cost).
        units.%(_.uclass != Bastet).not(Zeroed)./{ u => u.uclass match {
            case EarthCat      => 0
            case CatFromMars   => 1
            case CatFromSaturn => 2
            case CatFromUranus => 3
            case _             => 0
        }}.sum +
        neutralStrength(units, opponent)
    }
}


// ============================================================================
// BUBASTIS (BB) ACTION CASE CLASSES
// Stubs for actions defined in Tasks 3.4.x, 3.10.x, 3.12.x.
// Filled in as those tasks are implemented.
// ============================================================================

// ── GATHER POWER ─────────────────────────────────────────────────────────────
// (no separate action class needed — BB's gather power additions are inline triggers)

// ── CATNAPPING (Task 3.10.5) ─────────────────────────────────────────────────
// Audit V2/V3: Soft chain so Cancel mid-flow is safe; CatnappingDoneAction is Hard
// and is where the Power cost and unit moves are committed atomically.
case class CatnappingMainAction(self : Faction)
    extends OptionFactionAction(("Use " + Catnapping.name).styled(BB)) with MainQuestion with Soft
case class CatnappingFactionPickAction(self : Faction, picked : $[Faction], remaining : $[Faction])
    extends ForcedAction with PowerNeutral with Soft
case class CatnappingDoneAction(self : Faction, picked : $[Faction])
    extends BaseFactionAction(Catnapping, "Done".styled("power"))

// ── ZAGAZIG (Task 3.10.2 / 3.14.2) ──────────────────────────────────────────
case class ZagazigUseAction(self : Faction)
    extends OptionFactionAction(("Use " + Zagazig.name).styled(BB)) with PreBattleQuestion with Soft
case class ZagazigSkipAction(self : Faction)
    extends OptionFactionAction(("Skip " + Zagazig.name).styled(BB)) with PreBattleQuestion with Soft

// ── SAVAGERY (Task 3.10.3 / 3.14.3) ─────────────────────────────────────────
case class SavageryUseAction(self : Faction)
    extends OptionFactionAction(("Use " + Savagery.name).styled(BB)) with PreBattleQuestion with Soft
case class SavagerySkipAction(self : Faction)
    extends OptionFactionAction(("Skip " + Savagery.name).styled(BB)) with PreBattleQuestion with Soft

// ── PREDATOR (Task 3.10.4 / 3.14.4) ─────────────────────────────────────────
// PostBattleQuestion trait defined in Battle.scala (Task 3.14.4)
// Audit V4/V5/V6: Soft chain on the BB-side selection actions so Cancel mid-flow
// rewinds cleanly. Final eliminate is HARD and is dispatched on the affected
// enemy faction (FCG #26) which then picks the specific unit by UnitRef
// (FCG #27, #28).
// CRIT-3: Predator is OPTIONAL ("may select") — BB gets a Use/Skip prompt
// post-battle before the type-pick chain runs.
case class PredatorUseAction(self : Faction)
    extends OptionFactionAction(("Use " + Predator.name).styled(BB)) with PostBattleQuestion with Soft
case class PredatorSkipAction(self : Faction)
    extends OptionFactionAction(("Skip " + Predator.name).styled(BB)) with PostBattleQuestion with Soft
case class PredatorPickEnemyTypeAction(self : Faction, lostTypes : $[UnitClass])
    extends ForcedAction with PowerNeutral with Soft
case class PredatorTypeChoiceAction(self : Faction, uc : UnitClass)
    extends BaseFactionAction(Predator.styled(BB) + ": choose enemy unit class to eliminate", implicit g => uc.styled(BB)) with PowerNeutral with Soft
// FCG #26: affected-faction is `self`, so the enemy gets to pick which unit dies.
// FCG #27: identify the specific unit by UnitRef, not (UnitClass, Region).
case class PredatorEnemyEliminateAction(self : Faction, picker : Faction, u : UnitRef)
    extends BaseFactionAction(
        implicit g => Predator.styled(BB) + ": " + self.name.styled(self) + " choose " + g.unit(u).uclass.styled(self) + " to eliminate",
        implicit g => g.unit(u).full)

// ── REQUIRES ATTENTION RITUAL (Task 3.4.3) ───────────────────────────────────
case class RequiresAttentionMainAction(self : Faction)
    extends OptionFactionAction(RequiresAttention.styled(BB)) with MainQuestion with Soft
case class RequiresAttentionTargetAction(self : Faction, r : Region)
    extends BaseFactionAction(RequiresAttention.styled(BB) + ": resolve in", r)
case class RequiresAttentionSkipAction(self : Faction)
    extends OptionFactionAction(("Skip " + RequiresAttention.name).styled(BB)) with MainQuestion with Soft

// ── SPELLBOOK REQUIREMENTS (Task 3.12.1) ─────────────────────────────────────
// Audit V11: handler mutates Power and ends terminally — must be HARD, not Soft.
case class Pay2ForBBAction(self : Faction)
    extends OptionFactionAction("As your Action, pay ".styled(BB) + 2.power) with MainQuestion


// ============================================================================
// BUBASTIS (BB) EXPANSION — game-loop integration
// ============================================================================
object BBExpansion extends Expansion {
    override def eliminate(u : UnitFigure)(implicit game : Game) {
        if (!game.setup.has(BB)) return
        if (u.faction == BB) {
            if (u.uclass == CatFromMars || u.uclass == CatFromSaturn)
                BB.satisfyIf(MarsOrSaturnLost, "A Cat from Mars or Saturn was killed or eliminated", true)
            if (u.uclass == CatFromUranus)
                BB.satisfyIf(UranusLost, "A Cat from Uranus was killed or eliminated", true)
        }
    }

    def perform(action : Action, soft : VoidGuard)(implicit game : Game) = action @@ {
        // SETUP: move Earth Cats from reserve to Moon; no map starting area
        case SetupFactionsAction if game.setup.has(BB) && !game.starting.contains(BB) =>
            val f = BB
            f.units.%(_.uclass == EarthCat).foreach(_.region = BB.moon)
            // Moon counts as a Bubastis-Controlled Gate "for all purposes" per
            // source/bubastis.txt line 108 + FAQ #1 + FAQ #19. The Moon is
            // "inherent and always present" (FAQ #19) so it is registered in
            // BB.gates at setup and never removed. Control cannot be seized
            // (rule exception); abandon/seize/glacier paths must skip BB.moon
            // (see Game.scala:checkGatesLost guard).
            f.gates :+= BB.moon
            // Use a placeholder entry so the SetupFactionsAction loop doesn't block
            game.starting = game.starting + (BB -> BB.moon)
            f.log("starts with", 6, EarthCat.styled(f), "on", BB.moon)
            Force(SetupFactionsAction)

        // DOOM
        case DoomAction(f : BB.type) =>
            implicit val asking = Asking(f)

            // Syzygy (alt spellbook): if BB has no units on the Moon, gain 1 Elder Sign.
            // CRIT-7 sentinel: only fire once per Doom phase even if DoomAction is re-entered.
            if (f.can(Syzygy) && f.at(BB.moon).none && !game.bbSyzygyDone) {
                f.takeES(1)
                f.log(Syzygy.styled(BB) + ": no units on", BB.moon, "— gained", 1.es)
                game.bbSyzygyDone = true
            }

            // Ailurophobia: gain 1 Doom per distinct Cat variety that shares at least
            // one non-Moon Area with one or more enemy Units. Per-variety counting
            // (de-duplicated across areas), max 3 in normal play (Mars, Saturn, Uranus
            // — Earth Cats stay on Moon, but if any are on the map they count too).
            // CRIT-7 sentinel: only fire once per Doom phase even if DoomAction is re-entered.
            if (f.can(Ailurophobia) && !game.bbAilurophobiaDone) {
                val catClasses = $(EarthCat, CatFromMars, CatFromSaturn, CatFromUranus)
                val varietyCount = catClasses.count(uc =>
                    f.onMap(uc).%(_.region != BB.moon).exists(u =>
                        game.factions.but(f).exists(e => e.at(u.region).any)))
                if (varietyCount > 0) {
                    f.doom += varietyCount
                    f.log(Ailurophobia.styled(BB) + ": gained", varietyCount.doom,
                        "for", varietyCount, "Cat varietie".s(varietyCount), "co-present with enemies")
                }
                game.bbAilurophobiaDone = true
            }

            game.rituals(f)

            game.reveals(f)

            game.highPriests(f)

            game.hires(f)

            game.doomDone(f)

            asking

        // MAIN ACTION
        case MainAction(f : BB.type) if f.active.not =>
            UnknownContinue

        case MainAction(f : BB.type) if f.acted =>
            UnknownContinue

        case MainAction(f : BB.type) =>
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

            game.neutralSpellbooks(f)

            if (f.needs(Pay2ForBB) && f.power >= 2)
                + Pay2ForBBAction(f)

            if (f.can(Catnapping) && f.allInPlay.%(_.uclass == Bastet).any && f.power >= 1 &&
                game.factions.but(f).exists(e => f.allInPlay.%(_.uclass == Bastet).headOption.exists(b => e.at(b.region).any)))
                + CatnappingMainAction(f)

            game.libraryActions(f)

            game.highPriests(f)

            game.reveals(f)

            game.endTurn(f)(f.battled.any || game.nexed.any)

            asking

        // ── CATNAPPING (Task 3.10.5) ─────────────────────────────────────────────
        // Audit V3: Soft handler must not mutate state — Power cost moved into the Hard
        // CatnappingDoneAction so Cancel mid-flow refunds the Power automatically.
        case CatnappingMainAction(self) =>
            val bastetRegion = self.allInPlay.%(_.uclass == Bastet).headOption.map(_.region)
            bastetRegion match {
                case Some(r) =>
                    val eligibleFactions = game.factions.but(self).%(f => f.at(r).any)
                    // Start multi-select with all factions as remaining
                    Force(CatnappingFactionPickAction(self, $(), eligibleFactions))
                case None =>
                    UnknownContinue
            }

        case CatnappingFactionPickAction(self, picked, remaining) =>
            implicit val asking = Asking(self)
            if (picked.any)
                + GroupAction(Catnapping.styled(BB) + ": picked " + picked./(_.full).mkString(", "))
            remaining.foreach { f =>
                + CatnappingFactionPickAction(self, picked :+ f, remaining.but(f)).as("Add " + f.full)
            }
            if (picked.any)
                + CatnappingDoneAction(self, picked)
            + CancelAction
            asking

        // Audit V2/V3: Hard action — pay Power and move units atomically here so
        // Cancel before Done leaves Power untouched.
        case CatnappingDoneAction(self, picked) =>
            val bastetRegion = self.allInPlay.%(_.uclass == Bastet).headOption.map(_.region)
            bastetRegion match {
                case Some(r) =>
                    self.power -= 1
                    picked.foreach { f =>
                        val units = f.at(r)
                        units.foreach { u =>
                            u.region = BB.moon
                        }
                        if (units.any)
                            self.log(Catnapping.styled(BB) + ": moved", units.num, "unit".s(units.num), "of", f.full, "from", r, "to", BB.moon)
                    }
                    EndAction(self)
                case None =>
                    EndAction(self)
            }

        // ── PREDATOR (Task 3.10.4 / 3.14.4) ─────────────────────────────────────
        // CRIT-3: Use/Skip pair so BB can decline ("may select" per spec §1.10).
        // PredatorUseAction recomputes lost types from the live battle and dispatches
        // the type-pick chain; PredatorSkipAction simply ends the action.
        case PredatorUseAction(self) =>
            game.battle match {
                case Some(b) =>
                    val enemy = if (b.attacker == BB) b.defender else b.attacker
                    val lostTypes = b.eliminated.%(_.faction == enemy)./(_.uclass).distinct
                    if (lostTypes.any) Force(PredatorPickEnemyTypeAction(self, lostTypes))
                    else UnknownContinue
                case None => UnknownContinue
            }

        case PredatorSkipAction(self) =>
            self.log(Predator.styled(BB) + ": declined")
            UnknownContinue

        // Post-battle: if CatFromUranus fought and enemy lost ≥1 unit, BB picks a
        // lost unit class; the affected enemy faction then picks which specific
        // unit of that class dies (FCG #26, #27, #28).
        case PredatorPickEnemyTypeAction(self, lostTypes) =>
            implicit val asking = Asking(self)
            lostTypes.foreach { uc =>
                + PredatorTypeChoiceAction(self, uc)
            }
            + CancelAction
            asking

        // FCG #26: when BB has picked the unit class, hand the prompt to the
        // affected enemy faction so they pick which specific unit dies (FCG #28).
        case PredatorTypeChoiceAction(self, uc) =>
            val enemyWithUnit = game.factions.find(f => f != self && f.allInPlay.%(_.uclass == uc).any)
            enemyWithUnit match {
                case Some(e) =>
                    val candidates = e.allInPlay.%(_.uclass == uc)
                    if (candidates.none) {
                        self.log(Predator.styled(BB) + ": no", uc.styled(e), "on map to eliminate")
                        UnknownContinue
                    } else if (candidates.num == 1) {
                        // Only one possible target — skip the per-unit prompt.
                        Force(PredatorEnemyEliminateAction(e, self, candidates.head.ref))
                    } else {
                        // FCG #28: enemy faction picks which unit dies.
                        Ask(e).each(candidates)(u => PredatorEnemyEliminateAction(e, self, u.ref))
                    }
                case None =>
                    self.log(Predator.styled(BB) + ": no", uc.styled(BB), "on map to eliminate")
                    UnknownContinue
            }

        // FCG #26: this is HARD (no Soft) — the actual eliminate is committed
        // here, and `self` is the affected enemy faction so logs/Asking attribute
        // correctly. FCG #27: unit identified by UnitRef.
        case PredatorEnemyEliminateAction(self, picker, uref) =>
            val u = game.unit(uref)
            val r = u.region
            game.eliminate(u)
            picker.log(Predator.styled(BB) + ":", u.uclass.styled(self), "in", r, "eliminated by", picker.full)
            UnknownContinue

        // ── DOOM PHASE END: reset BB doom-phase sentinels (CRIT-7) ───────────────
        // Both Ailurophobia and Syzygy must re-arm for the next Doom Phase.
        case DoomDoneAction(f) if f == BB =>
            game.bbAilurophobiaDone = false
            game.bbSyzygyDone = false
            UnknownContinue

        // ── AWAKEN BASTET REQUIREMENT (Task 3.12.6) ──────────────────────────────
        case AwakenedAction(self : BB.type, Bastet, _, _) =>
            self.satisfy(AwakenBastet, "Awaken Bastet")
            EndAction(self)

        // ── PAY-2 REQUIREMENT (Task 3.12.1) ──────────────────────────────────────
        case Pay2ForBBAction(self) =>
            self.power -= 2
            self.log(Pay2ForBB.text + ": paid", 2.power)
            self.satisfy(Pay2ForBB, "Pay 2 Power as an Action")
            EndAction(self)

        // ...
        case _ => UnknownContinue
    }
}
