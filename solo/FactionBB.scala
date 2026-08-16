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
// BB MOON OVERLAY SIZING — anti-overlap auto-shrink
// ----------------------------------------------------------------------------
// The Moon overlay scatters each unit's sprite across the visible moon disc.
// When many units sit on the Moon their sprites overlap. To keep the display
// legible we cap the TOTAL sprite area so no unit is likely to be >50% covered
// by others, shrinking every sprite uniformly when needed.
//
// The numbers below were derived empirically (offline simulation over 4000
// random unit mixes placed by the live scatter algorithm, measuring per-unit
// box coverage — see the analysis in the task that added this):
//   * areaFraction(uc): the sprite's on-screen box area as a fraction of the
//     visible moon-disc area, at the DEFAULT (un-shrunk) size. Box area =
//     (spriteHeight) * (spriteHeight * aspect). spriteHeight fraction of the
//     moon HEIGHT is the same %-of-moon value the overlay uses (14/70 * onMap),
//     and `aspect` (w/h) was measured off the rendered sprites per unit class.
//   * SAFE_AREA_RATIO: the SUM of all on-Moon units' box area (as a fraction of
//     the disc) is held at or below this. It was tuned by simulating the ACTUAL
//     scatter placement over hundreds of crowded games, applying the shrink, and
//     measuring how often any unit ends up >50% covered. Real placement clusters
//     worse than a uniform sprinkle, so the cutoff has to be well under 1.0:
//       0.65 -> 94% of crowded games still had an over-50 unit
//       0.45 -> 58%,  0.35 -> 20%,  0.30 -> 8%  (avg 0.09 over-50 units/game)
//     The analysis above favored a tighter 0.30, but the value actually in live
//     use — and confirmed identical + working in BB + HB — is SAFE_AREA_RATIO =
//     0.60 (see below). 0.60 is deliberately permissive: it keeps sprites large
//     and readable while still capping the worst crowding. (These comments were
//     corrected to 0.60 on 2026-08-11; they previously mis-stated 0.30.)
// The overlay sums areaFraction across the units actually on the Moon; if the
// sum exceeds SAFE_AREA_RATIO it shrinks every sprite by
//   scale = sqrt(SAFE_AREA_RATIO / sumRatio)
// (area scales with the square of the linear shrink) so the post-shrink sum
// lands right at the safe ratio in ONE step — no recursion needed.
// ============================================================================
object BBMoonSizing {
    val SAFE_AREA_RATIO : Double = 0.60

    // Visible moon disc: centre (513,440) radius 379 on the 1024x878 asset.
    // Disc area in the same "fraction of moon HEIGHT" units used for sprites:
    // we work in fractions of moon height throughout, so express the disc area
    // as (pi * r^2) / (moonH^2) to match sprite box areas expressed as
    // (h_frac) * (h_frac * aspect).
    private val moonH : Double = 878.0
    private val discR : Double = 379.0
    val discAreaFrac : Double = math.Pi * discR * discR / (moonH * moonH)

    // Measured width/height aspect (w/h) of each unit's rendered sprite box.
    def aspect(uc : UnitClass) : Double = uc match {
        case EarthCat      => 0.921
        case CatFromMars   => 0.779
        case CatFromSaturn => 0.751
        case CatFromUranus => 0.937
        case Bastet        => 0.937   // same silhouette proportions as the large cat
        case _             => 0.85    // any visiting non-BB unit: conservative default
    }

    /** Sprite box area (as a fraction of the moon-disc area) for a unit whose
      * sprite height is `hFracOfMoon` (e.g. 0.14 for an Earth Cat). */
    def areaFraction(uc : UnitClass, hFracOfMoon : Double) : Double = {
        val boxArea = hFracOfMoon * (hFracOfMoon * aspect(uc))  // in (frac-of-moonH)^2
        boxArea / discAreaFrac
    }

    /** Given the summed area-ratio of all on-Moon sprites at default size,
      * return the linear shrink factor to apply to EVERY sprite so the total
      * lands at (or below) the safe ratio. 1.0 = no shrink needed. */
    def shrinkFactor(sumAreaRatio : Double) : Double =
        if (sumAreaRatio <= SAFE_AREA_RATIO || sumAreaRatio <= 0.0) 1.0
        else math.sqrt(SAFE_AREA_RATIO / sumAreaRatio)
}


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
case object CatInEveryEnemyStart extends Requirement("Cats in Start Regions; 1p/")
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
    extends OptionFactionAction(("Use " + Zagazig.name).styled(BB)) with PreBattleQuestion
case class ZagazigSkipAction(self : Faction)
    extends OptionFactionAction(("Skip " + Zagazig.name).styled(BB)) with PreBattleQuestion

// ── SAVAGERY (Task 3.10.3 / 3.14.3) ─────────────────────────────────────────
case class SavageryUseAction(self : Faction)
    extends OptionFactionAction(("Use " + Savagery.name).styled(BB)) with PreBattleQuestion
case class SavagerySkipAction(self : Faction)
    extends OptionFactionAction(("Skip " + Savagery.name).styled(BB)) with PreBattleQuestion

// ── PREDATOR (Task 3.10.4 / 3.14.4) ─────────────────────────────────────────
// PostBattleQuestion trait defined in Battle.scala (Task 3.14.4)
// Audit V4/V5/V6: Soft chain on the BB-side selection actions so Cancel mid-flow
// rewinds cleanly. Final eliminate is HARD and is dispatched on the affected
// enemy faction (FCG #26) which then picks the specific unit by UnitRef
// (FCG #27, #28).
// CRIT-3: Predator is OPTIONAL ("may select") — BB gets a Use/Skip prompt
// post-battle before the type-pick chain runs.
case class PredatorUseAction(self : Faction)
    extends OptionFactionAction(("Use " + Predator.name).styled(BB)) with PostBattleQuestion
case class PredatorSkipAction(self : Faction)
    extends OptionFactionAction(("Skip " + Predator.name).styled(BB)) with PostBattleQuestion
case class PredatorPickEnemyTypeAction(self : Faction, lostTypes : $[UnitClass])
    extends ForcedAction with PowerNeutral with Soft
// NOTE: must be RECORDED (no Soft). BB clicks this to pick the unit class; the
// handler then returns an Ask directed at the affected enemy faction. `continue`
// (the live menu) is only updated for recorded actions (Game.scala isRecorded /
// performContinue), so a Soft here left the type-pick menu on screen and the pick
// appeared to loop — the enemy's instance prompt never showed.
case class PredatorTypeChoiceAction(self : Faction, uc : UnitClass)
    extends BaseFactionAction(Predator.styled(BB) + ": choose enemy unit class to eliminate", implicit g => uc.styled(BB)) with PowerNeutral
// FCG #26: affected-faction is `self`, so the enemy gets to pick which unit dies.
// FCG #27: identify the specific unit by UnitRef, not (UnitClass, Region).
case class PredatorEnemyEliminateAction(self : Faction, picker : Faction, u : UnitRef)
    extends BaseFactionAction(
        implicit g => Predator.styled(BB) + ": " + self.name.styled(self) + " choose " + g.unit(u).uclass.styled(self) + " to eliminate",
        implicit g => g.unit(u).full)

// ── REQUIRES ATTENTION RITUAL (Task 3.4.3) ───────────────────────────────────
case class RequiresAttentionMainAction(self : Faction)
    extends OptionFactionAction(implicit g => "Ritual with " + RequiresAttention.styled(BB) + " for " + self.can(Herald).?(5).|(g.ritualCost).power) with MainQuestion with Soft
case class RequiresAttentionTargetAction(self : Faction, r : Region)
    extends BaseFactionAction(implicit g => "Ritual with " + RequiresAttention.styled(BB) + " for " + self.can(Herald).?(5).|(g.ritualCost).power, implicit g => r.toString)
case class RequiresAttentionSkipAction(self : Faction)
    extends OptionFactionAction(("Skip " + RequiresAttention.name).styled(BB)) with MainQuestion

// ── SPELLBOOK REQUIREMENTS (Task 3.12.1) ─────────────────────────────────────
// Audit V11: handler mutates Power and ends terminally — must be HARD, not Soft.
case class Pay2ForBBAction(self : Faction)
    extends OptionFactionAction("As your Action, pay ".styled(BB) + 2.power) with MainQuestion


// ============================================================================
// BUBASTIS (BB) EXPANSION — game-loop integration
// ============================================================================
object BBExpansion extends Expansion {
    override def triggers()(implicit game : Game) {
        if (!game.setup.has(BB)) return
        BB.satisfyIf(NoEarthCatsOnMoon, "No Earth Cats on the Moon", BB.at(BB.moon).%(_.uclass == EarthCat).none)

        // Cat in every enemy start — checked here (BEFORE Cyclopean Gaze) so
        // that momentarily having cats in all starts earns the SBR even if CG
        // subsequently pains a cat away.
        if (BB.needs(CatInEveryEnemyStart)) {
            val catInEveryStart = game.factions.but(BB).forall(e => game.starting.get(e).exists(r => BB.at(r).%(u => u.uclass == EarthCat || u.uclass == CatFromMars || u.uclass == CatFromSaturn || u.uclass == CatFromUranus).any))
            if (catInEveryStart) {
                val bonus = game.factions.but(BB).num
                BB.satisfy(CatInEveryEnemyStart, "A Cat is in every enemy Faction's Start Area; gain 1 Power per enemy Faction")
                if (bonus > 0) {
                    BB.power += bonus
                    BB.log("gained", bonus.power, "(Cat in every Start Area bonus)")
                }
            }
        }
    }

    def postCGTriggers()(implicit game : Game) {
        // Cat-in-every-start check moved back to triggers() so it fires
        // BEFORE Cyclopean Gaze.  This method is retained as a no-op so
        // callers in Game.scala do not break.
    }

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

            // Alt Ancients Holy Ground cathedrals cannot be moved, so they cannot be
            // Catnapped — require at least one movable enemy unit in Bastet's Area.
            if (f.can(Catnapping) && f.onMap(Bastet).any && f.power >= 1 &&
                f.onMap(Bastet).headOption.exists(b => game.factions.but(f).exists(e => e.at(b.region).%(_.canBeMoved).any)))
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
            val bastetRegion = self.onMap(Bastet).headOption.map(_.region)
            bastetRegion match {
                case Some(r) =>
                    // Only factions with a movable unit here are eligible (Holy Ground
                    // cathedrals cannot be moved, so they can't be Catnapped).
                    val eligibleFactions = game.factions.but(self).%(f => f.at(r).%(_.canBeMoved).any)
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
            self.oncePerRound :+= Catnapping
            val bastetRegion = self.onMap(Bastet).headOption.map(_.region)
            bastetRegion match {
                case Some(r) =>
                    self.power -= 1
                    picked.foreach { f =>
                        // Skip units that cannot be moved (Holy Ground cathedrals).
                        val units = f.at(r).%(_.canBeMoved)
                        units.foreach { u =>
                            u.region = BB.moon
                            u.onGate = false
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
            val wasMonster = u.uclass.utype == Monster
            game.eliminate(u)
            picker.log(Predator.styled(BB) + ":", u.uclass.styled(self), "in", r, "eliminated by", picker.full)
            // Carnivore (alt SB) reads "1 Doom per enemy Monster Killed OR Eliminated by You
            // in Battle." A Monster removed by Predator is an elimination caused by BB as part
            // of the Battle, so it earns Carnivore Doom on top of the arena kills tallied at
            // Battle.scala PostBattlePhase. The Predator target is a SEPARATE map unit that is
            // never added to the battle `eliminated` list, so this cannot double-count that
            // batch award. Validated against game 669 "Pain for Oblivion": GC/DeepOne/4
            // eliminated by Predator adds +1 on top of the +2 for the two Deep Ones killed in
            // the arena, for a total Carnivore yield of 3.
            if (wasMonster && BB.has(Carnivore)) {
                BB.log(Carnivore.styled(BB) + ": gained", 1.doom, "for enemy Monster eliminated by", Predator.name)
                BB.doom += 1
            }
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
