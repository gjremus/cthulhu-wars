package cws

import hrf.colmat._

import html._

// ============================================================================
// REGRESSION BUILD of Colour Out of Space (CS), written blind from the
// implementation guide's Section 1 (rules) only — NOT copied from the committed
// FactionCS.scala. Kept in a separate file (FactionCS_regression.scala) so it
// can be diffed against the real one without colliding. This is a CALIBRATION
// artifact, not a merge candidate.
// ============================================================================

// CS UNITS (verbatim stats from guide §1.7):
//  Acolyte (Cultist, 6) reused standard; Meteorite (Monster 6, cost1 combat0);
//  Effervescent Excrescence (Monster 8, cost2 combat2); Luminous Globule
//  (Terror 6, cost4, no combat); Prismatic Well (Gate-derived token 6, never
//  paid/placed); Tulzscha (GOO 1, fixed 5 to awaken, combat = 2x globules).
case object Meteorite extends FactionUnitClass(CS, "Meteorite", Monster, 1)
case object EffervescentExcrescence extends FactionUnitClass(CS, "Effervescent Excrescence", Monster, 2)
case object LuminousGlobule extends FactionUnitClass(CS, "Luminous Globule", Terror, 4)
// Prismatic Well: a Gate-derived token. Not summoned/paid; created by Chromatic
// Perversion converting an existing Gate in a Globule's region. Modeled as a
// Token-type unit class. RISK: committed code may instead layer "well" state onto
// the existing Gate object rather than declaring a new unit class (guide §2.3
// calls it "a Gate-derived token, not a region tile" — ambiguous on representation).
case object PrismaticWell extends FactionUnitClass(CS, "Prismatic Well", Token, 0)
// NOTE: Tulzscha already exists as a unit in the codebase (appears in
// Faction.neutralStrength). CS reuses that object as its GOO rather than
// redeclaring it. RISK: if the existing Tulzscha is wired as neutral/IGOO-only,
// reusing it as a faction GOO may need extra plumbing.

// CS SPELLBOOKS (§1.5 unique ability + §1.10 library of six)
case object ChromaticPerversion extends FactionSpellbook(CS, "Chromatic Perversion")   // unique ability (ongoing)
case object Insanity extends FactionSpellbook(CS, "Insanity") with BattleSpellbook       // post-battle
case object VermiculiteHypertrophy extends FactionSpellbook(CS, "Vermiculite Hypertrophy")
case object CosmicLandfall extends FactionSpellbook(CS, "Cosmic Landfall")               // ongoing
case object SpectralCollapse extends FactionSpellbook(CS, "Spectral Collapse") with BattleSpellbook // post-battle
case object EffulgentSacrifice extends FactionSpellbook(CS, "Effulgent Sacrifice")       // action, cost 1
case object CoreExposure extends FactionSpellbook(CS, "Core Exposure")                   // action, cost 0

// CS SPELLBOOK REQUIREMENTS (§1.9) — any one satisfied lets the player pick any book
case object CSMeteoriteInEnemyStart extends Requirement("A meteorite exists in another player's starting area")
case object CSPrismaticWellExists extends Requirement("A prismatic well exists")
case object CSGlobuleEliminated extends Requirement("A globule is eliminated")
case object CSSacrificeGlobule extends Requirement("Sacrifice a globule as an action")
case object CSEnemyControlsExcrescence extends Requirement("Another player controls an excrescence")
case object CSAwakenTulzscha extends Requirement("Awaken Tulzscha")


case object CS extends Faction { f =>
    def name = "Colour Out of Space"
    def short = "CS"
    def style = "cs"

    override def abilities = $(ChromaticPerversion)
    override def library = $(Insanity, VermiculiteHypertrophy, CosmicLandfall, SpectralCollapse, EffulgentSacrifice, CoreExposure)
    override def requirements(options : $[GameOption]) =
        $(CSMeteoriteInEnemyStart, CSPrismaticWellExists, CSGlobuleEliminated, CSSacrificeGlobule, CSEnemyControlsExcrescence, CSAwakenTulzscha)

    // §1.7 roster. Prismatic Wells are created by conversion, not held in pool for
    // summon — included for the 6-count cap but never recruited/summoned directly.
    val allUnits =
        1.times(Tulzscha) ++
        6.times(LuminousGlobule) ++
        8.times(EffervescentExcrescence) ++
        6.times(Meteorite) ++
        6.times(PrismaticWell) ++
        6.times(Acolyte)

    // §1.3: Tulzscha always costs a flat 5 Power. (The cultist-sacrifice + die-roll
    // + Elder Sign is a separate scripted step handled in CSExpansion, not priced
    // through awakenCost.) Eligible only in a region with one of CS's cultists and
    // a Globule.
    override def awakenCost(u : UnitClass, r : Region)(implicit game : Game) : |[Int] = u match {
        case Tulzscha => (f.at(r, Acolyte).any && f.at(r, LuminousGlobule).any).?(5)
        case _ => None
    }

    // Combat:
    //  Meteorite 0, Globule 0 (no combat), Excrescence 2 (6 with Vermiculite),
    //  Tulzscha = 2 x (Luminous Globules anywhere on the map), recalculated per battle.
    // RISK: neutralStrength() already adds Tulzscha * 1, so adding Tulzscha here as
    // well would double-count. Committed code likely computes Tulzscha explicitly
    // and does NOT route it through neutralStrength — to be verified in the diff.
    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int = {
        val globulesOnMap = f.onMap(LuminousGlobule).not(Zeroed).num
        val excrescenceCombat = f.can(VermiculiteHypertrophy).?(6).|(2)
        units(EffervescentExcrescence).not(Zeroed).num * excrescenceCombat +
        units(Tulzscha).not(Zeroed).num * (2 * globulesOnMap) +
        neutralStrength(units, opponent)
    }
}
