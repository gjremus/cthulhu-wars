package cws

import hrf.colmat._

import html._


@scala.scalajs.reflect.annotation.EnableReflectiveInstantiation
trait Record extends Product with GoodMatch


sealed abstract class Glyph(val inPlay : Boolean, val onMap : Boolean)

trait AreaGlyph extends Glyph
case object Ocean extends Glyph(true, true) with AreaGlyph
case object GlyphAA extends Glyph(true, true) with AreaGlyph
case object GlyphOO extends Glyph(true, true) with AreaGlyph
case object GlyphWW extends Glyph(true, true) with AreaGlyph
case object NoGlyph extends Glyph(true, true) with AreaGlyph
case object Pool extends Glyph(false, false)
case object Prison extends Glyph(false, false)
case object Deep extends Glyph(true, false)
case object Slumber extends Glyph(true, false)
case object Sorcery extends Glyph(true, false)
case object Extinct extends Glyph(false, false)
case object MoonGlyph extends Glyph(true, false)

trait Region extends GoodMatch {
    val id : String
    val name : String
    val glyph : Glyph
    def elem : String

    def +(ia : IceAges) = elem + ia.toString

    override def toString = elem
}

case class Area(name : String, glyph : AreaGlyph) extends Region {
    val id = name.split(" ").mkString("")
    def elem = glyph @@ {
        case Ocean => name.styled("sea")
        case _ => name.styled("region")
    }
}

trait FactionRegion extends Region {
    val faction : Faction
    def elem = name.styled(faction)
}

case class Pool(faction : Faction) extends FactionRegion {
    val glyph = Pool
    val id = "???"
    val name = faction.name + " Pool"
}

case class Prison(faction : Faction) extends FactionRegion {
    val glyph = Prison
    val id = "???"
    val name = faction.name + " Prison"
}

case class Deep(faction : Faction) extends FactionRegion {
    val glyph = Deep
    val id = "Deep"
    val name = "Ocean Deep"
}

case class Slumber(faction : Faction) extends FactionRegion {
    val glyph = Slumber
    val id = "Slumber"
    val name = "Cursed Slumber"
}

case class Sorcery(faction : Faction) extends FactionRegion {
    val glyph = Sorcery
    val id = "???"
    val name = "Ancient Sorcery"
}

case class ShamblerHold(faction : Faction) extends FactionRegion {
    val glyph = Pool
    val id = "ShamblerHold"
    val name = faction.name + " Shambler Hold"
}

case class VelvetFanHold(faction : Faction) extends FactionRegion {
    val glyph = Pool
    val id = "VelvetFanHold"
    val name = faction.name + " Velvet Fan"
}

case class Extinct(faction : Faction) extends FactionRegion {
    val glyph = Extinct
    val id = "???"
    val name = "Extinct"
}

// BUBASTIS: Moon — off-map region that holds BB's Earth Cats at setup and
// catnapped enemy units. onMap=false is used to keep Moon out of map-iteration
// queries (Bastet awakening prerequisites, Ailurophobia non-Moon scoring,
// etc.) but the Moon DOES count as a Bubastis-Controlled Gate "for all
// purposes" per the Moon Tile rule — Earth Cats on the Moon still generate
// Gather Power per their unit-card Special, are still in-play, and the Moon
// is still a valid controlled-Gate Summon target.
case class MoonHold(faction : Faction) extends FactionRegion {
    val glyph = MoonGlyph
    val id    = "MoonHold"
    val name  = "the Moon"
}


trait Board {
    def id : String
    def name : String
    def regions : $[Region]
    def connected(region : Region) : $[Region]
    def connectedForRetreat(region : Region) : $[Region] = connected(region)
    def starting(faction : Faction) : $[Region]
    def distance(a : Region, b : Region) : Int
    def gateXYO(r : Region) : (Int, Int)
    val width : Int = 1791
    val nonFactionRegions : $[Region]
    val west : $[Region]
    val east : $[Region]
    val isLibraryMap : Boolean = false
    def gateXYOHorizontal(r : Region) : (Int, Int) = gateXYO(r)
    def silenceTokenMax(f : Faction) : Int = 0
    val unitScale : Double = 1.0
    val archways : Set[Region] = Set()
    def allows6PowerAltForBB : Boolean = false
}

case class ElderSign(value : Int) {
    def short = "[" + (value > 0).?(value.toString).|("?").styled("es") + "]"
}


trait LoyaltyCard {
    val icon : UnitClass
    val unit : UnitClass
    def name = unit.name
    val doom : Int = 0
    val power : Int = 0
    val cost : Int
    val quantity : Int
    val combat : Int

    def short = name.styled("nt")
}

abstract class IGOOLoyaltyCard(val icon : UnitClass, val unit : UnitClass, override val power : Int, val quantity : Int = 1, val combat : Int = 0) extends LoyaltyCard {
    override val doom = 0
    override val cost = power
}

abstract class NeutralMonsterLoyaltyCard(val icon : UnitClass, val unit : UnitClass, val cost : Int, val quantity : Int, val combat : Int) extends LoyaltyCard {
    override val doom = 2
    override val power = 0
}

abstract class NeutralTerrorLoyaltyCard(val icon : UnitClass, val unit : UnitClass, val cost : Int, val powerCost : Int, val quantity : Int, val combat : Int) extends LoyaltyCard {
    override val doom = cost
    override val power = powerCost
}


sealed trait UnitType {
    def name = toString
    def plural = name + "s"
    val priority : Int
}
case object Cultist extends UnitType { val priority = 10 }
case object Monster extends UnitType { val priority = 20 }
case object Terror extends UnitType { val priority = 30 }
case object GOO extends UnitType { val priority = 40 }
// BUBASTIS: ElderGod UnitType — same priority as GOO; Bastet uses this type.
// isGOO helper below includes ElderGod so GOO-targeted effects fire for Bastet.
case object ElderGod extends UnitType { val priority = 40 }
case object Token extends UnitType { val priority = 2 }
case object Building extends UnitType { val priority = 5 }
case object MapUnit extends UnitType { val priority = 1 }

trait IGOO

abstract class UnitClass(val name : String, val utype : UnitType, val cost : Int) extends Record {
    def plural = name + "s"
    def styled(f : Faction) = name.styled(f)
    val priority = utype.priority * 1_00_00_00 + cost * 1_00 + this.is[NeutralMonster].??(1_00_00)

    // BUBASTIS: isGOO includes ElderGod so every GOO-targeted check fires for Bastet
    def isGOO     : Boolean = utype == GOO || utype == ElderGod
    def isElderGod: Boolean = utype == ElderGod

    def canMove(u : UnitFigure)(implicit game : Game) : Boolean = !game.mummifiedCultists.has(u.ref) && !MindParasite.isParasitized(u)
    def canBeMoved(u : UnitFigure)(implicit game : Game) : Boolean = !game.mummifiedCultists.has(u.ref) && !MindParasite.isParasitized(u)
    def canBattle(u : UnitFigure)(implicit game : Game) : Boolean = !game.mummifiedCultists.has(u.ref)
    def canCapture(u : UnitFigure)(implicit game : Game) : Boolean = true
    def canControlGate(u : UnitFigure)(implicit game : Game) : Boolean = false
    def canBeRecruited(f : Faction)(implicit game : Game) : Boolean = utype == Cultist
    def canBeSummoned(f : Faction)(implicit game : Game) : Boolean = utype == Monster || utype == Terror
}


case object Acolyte extends UnitClass("Acolyte", Cultist, 1) {
    override def canCapture(u : UnitFigure)(implicit game : Game) = false
    override def canControlGate(r : UnitFigure)(implicit game : Game) : Boolean = true
}

case object HighPriest extends UnitClass("High Priest", Cultist, 3) {
    override def canCapture(u : UnitFigure)(implicit game : Game) = false
    override def canControlGate(u : UnitFigure)(implicit game : Game) = true
}

case object HighPriestIcon extends UnitClass(HighPriest.name + " Icon", Token, 0)
case object HighPriestCard extends LoyaltyCard {
    val icon = HighPriestIcon
    val unit = HighPriest
    val cost = 3
    val quantity = 1
    val combat = 0
}


abstract class FactionUnitClass(val faction : Faction, name : String, utype : UnitType, cost : Int) extends UnitClass(name, utype, cost) {
    def elem = name.styled(faction)
    override def toString = elem
}

abstract class MapUnitClass(name : String) extends UnitClass(name, MapUnit, 0) {
    override def canMove(u : UnitFigure)(implicit game : Game) : Boolean = false
    override def canBeMoved(u : UnitFigure)(implicit game : Game) : Boolean = false
    override def canBattle(u : UnitFigure)(implicit game : Game) : Boolean = false
    override def canCapture(u : UnitFigure)(implicit game : Game) : Boolean = false
    override def canControlGate(u : UnitFigure)(implicit game : Game) : Boolean = false
    override def canBeRecruited(f : Faction)(implicit game : Game) : Boolean = false
    override def canBeSummoned(f : Faction)(implicit game : Game) : Boolean = false

    def elem = name.styled("lb")
    override def toString = elem
}

sealed class UnitState(val text : String) extends Ordered[UnitState]  {
    def elem = text
    override def toString = elem

    def compare(that : UnitState) = text.compare(that.text)
}

case object Moved extends UnitState("moved")
case object Retreated extends UnitState("retreated")
case object Absorbed extends UnitState("absorbed")
case object Harbinged extends UnitState("harbinged")
case object Invised extends UnitState("invised")
case object Hidden extends UnitState("hidden")
case object Zeroed extends UnitState("zeroed")
case object MovedForFree extends UnitState("moved-for-free")
case object MovedForExtra extends UnitState("moved-for-extra")
case object Eliminated extends UnitState("eliminated")
case object Mummified extends UnitState("mummified")

class UnitFigure(val faction : Faction, val uclass : UnitClass, val index : Int, initialRegion : Region, var onGate : Boolean = false, var state : $[UnitState] = $, var health : UnitHealth = Alive) {
    // Round 9 bug fix (user-reported): every region change for an enemy unit
    // landing in an FB Cyclopean Gaze region must fire CG, regardless of
    // which action path caused it (MoveAction, SummonAction, self.place,
    // Avatar sn.region, Screaming Dead, Arctic Wind, Undulate, etc.). Wrap
    // the `region` assignment in a setter that runs the CG edge-case hook
    // whenever an enemy non-Building unit enters a gaze region. The previous
    // per-action hooks missed many direct-assignment paths.
    private var _region : Region = initialRegion
    def region : Region = _region
    def region_=(r : Region)(implicit game : Game) : Unit = {
        val prev = _region
        _region = r
        // CG edge-case hook: only check when FB is in game with active CG
        // Optimized: short-circuit early, single-pass unit check
        if (prev != r && faction != FB && uclass.utype != Building &&
            !game.fbSuppressCGForPlacement &&
            game.fbHasCGActive &&
            FB.units.exists(u => u.region == r && (u.uclass == RevenantOfKnaa || u.uclass == Ghatanothoa)))
            game.fbCyclopeanGazeActionRegions :+= r
    }

    override def toString = short

    def dbg = faction.short + "/" + uclass.name + "/" + index

    def short = uclass.name.styled(faction)

    def full = uclass.name.styled(faction) + onGate.??(" (on the gate)") + state.some./(_.mkString(" (", "/", ")")).|("") + (health != Alive && health != DoubleHP(Alive, Alive)).??(" (" + health + ")")

    def styledName(implicit game : Game) : String = if (uclass == MindParasiteCultist) {
        MindParasite.styledUnit(this)
    } else uclass.name.styled(faction)

    def tag(s : UnitState) = state.has(s)
    def add(s : UnitState) { state :+= s }
    def add(l : $[UnitState]) { state ++= l }
    def remove(s : UnitState) { state = state.but(s) }
    def count(s : UnitState) = state.count(_ == s)
    def ref = UnitRef(faction, uclass, index)
}

case class UnitRef(faction : Faction, uclass : UnitClass, index : Int) {
    def short = uclass.styled(faction)
    def full = UnitRefFull(this)
}

case class UnitRefShort(r : UnitRef)
case class UnitRefFull(r : UnitRef)

sealed abstract class Spellbook(val name : String) extends Record {
    def elem : String
    override def toString = elem
    def styled(f : Faction) = name.styled(f)
}

trait BattleSpellbook extends Spellbook

abstract class NeutralSpellbook(name : String) extends Spellbook(name) {
    override def elem = name.styled("nt")
}

abstract class FactionSpellbook(val faction : Faction, name : String) extends Spellbook(name) {
    override def elem = name.styled(faction)
}

abstract class Requirement(val text : String, val es : Int = 0) {
    // Optional dynamic label override — subclasses can vary by game options
    // (e.g. OW UnitsAtEnemyGates threshold changes with CheapMutants).
    // The static `text` remains the stable pattern-match key for overlay
    // dispatch; displayText is what gets rendered to the user.
    def displayText(implicit game : Game) : String = text
}

trait Faction { f =>
    def name : String
    def short : String
    def style : String
    def abbr : String = style.toUpperCase.styled(style)
    val reserve = Pool(f)
    val prison = Prison(f)

    def full = name.styled(style, "inline-block")
    def ss = short.styled(style)

    override def toString = full

    def allUnits : $[UnitClass]
    def abilities : $[Spellbook]
    def library : $[Spellbook]
    def requirements(options : $[GameOption]) : $[Requirement]
    def canRecruitHP : Boolean = true
    def recruitCost(u : UnitClass, r : Region)(implicit game : Game) = u.cost
    def summonCost(u : UnitClass, r : Region)(implicit game : Game) = u.cost
    def awakenCost(u : UnitClass, r : Region)(implicit game : Game) : |[Int] = None
    def awakenDesc(u : UnitClass) : |[String] = None
    def canAwakenIGOO(r : Region)(implicit game : Game) : Boolean = f.gates.has(r) && f.at(r, GOO, ElderGod).any
    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int
    def neutralStrength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) =
        units(Ghast).num * 0 +
        units(Gug).num * 3 +
        units(Shantak).num * 2 +
        units(StarVampire).num * 1 +
 	units(Voonith).num * 1 +
    units(DimensionalShamblerUnit).num * 2 +
        units(Gnorri).num * 2 +
        units(Ygolonac).not(Zeroed).num * 1 +
        units(Tulzscha).not(Zeroed).num * 1 +
        units(Byatis).not(Zeroed).num * 4 +
        units(Abhoth).not(Zeroed).num * f.all(Filth).num +
        units(Daoloth).not(Zeroed).num * 0 +
        units(Nyogtha).not(Zeroed).num * game.battle./(_.attacker).has(f).?(4).|(1) +
        // New Terrors
        units(Dhole).not(Zeroed).num * 5 +
        units(GreatRaceOfYith).not(Zeroed).num * 3 +
        units(QuachilUttaus).not(Zeroed).num * 1 +
        units(HoundOfTindalos).not(Zeroed).num * 4 +
        units(ElderShoggoth).not(Zeroed).num * 2 +
        // New Monsters
        units(AlbinoPenguins).num * -2 +
        units(MoonbeastUnit).num * 0 +
        units(ElderThing).num * 2 +
        units(LengSpiderUnit).num * 1 +
        units(Satyr).num * 1 +
        units(ServitorUnit).num * -1 +
        // New IGOOs
        units(Yig).not(Zeroed).num * 2 +
        units(BloatedWoman).not(Zeroed).num * 1 +
        // Mother Hydra: max(1, 6 - enemy units in area)
        units(MotherHydra).not(Zeroed)./(u => max(1, 6 - opponent.at(u.region).%(_.uclass.utype != MapUnit).num)).sum +
        // Father Dagon: 2 land / 6 ocean
        units(FatherDagon).not(Zeroed)./(u => if (u.region.glyph == Ocean) 6 else 2).sum +
        // Ghatanothoa IGOO: = enemy cultists on map
        units(GhatanotoaIGOO).not(Zeroed).num * opponent.allInPlay.%(_.uclass.utype == Cultist).num +
        // Cthugha: pre-battle combat = 1 if enemy GOO present in region (enables battle declaration)
        // Actual combat is calculated dynamically during battle (picks highest enemy GOO)
        // HIGH-2 revised: ElderGod (Bastet) counts as GOO per spec §1.3.
        units(Cthugha).not(Zeroed).%(u => opponent.at(u.region).%(_.uclass.isGOO).any).num +
        0 +
        // Atlach-Nacha: combat 4
        units(AtlachNacha).not(Zeroed).num * 4 +
        // Bokrug: combat 0
        units(Bokrug).not(Zeroed).num * 0 +
        // Gla'aki IGOO: combat 0
        units(GlaakiIGOO).not(Zeroed).num * 0 +
        // Azathoth IGOO: = glyph position
        units(AzathothIGOO).not(Zeroed).num * game.azathothGlyphPosition
}

// Elder Thing Mind Control: suppress GOO special abilities when Elder Thing shares area
// HIGH-2 revised: ElderGod (Bastet) counts as GOO for ETMC suppression per spec §3.18.44.
object ElderThingMindControl {
    def suppresses(u : UnitFigure)(implicit game : Game) : Boolean =
        u.uclass.isGOO && game.factions.exists(f2 => f2 != u.faction && f2.at(u.region, ElderThing).any)
}

// Insects from Shaggai Mind Parasite
// When an off-gate Acolyte shares area with enemy Insect, it's converted to MindParasiteCultist
// under the insect owner. When it leaves the insect area, it converts back.
object MindParasite {
    // Check if a unit is a parasitized cultist
    def isParasitized(u : UnitFigure)(implicit game : Game) : Boolean = u.uclass == MindParasiteCultist

    // Alternating-color styled text for parasitized cultist menu items
    def styledAlt(text : String, origFaction : Faction, insectFaction : Faction) : String = {
        text.zipWithIndex.map { case (c, i) =>
            val style = if (i % 2 == 0) origFaction.style else insectFaction.style
            s"<span class='$style'>$c</span>"
        }.mkString("") + " <span class='" + insectFaction.style + "'>controlled by Mind Parasite</span>"
    }

    // Get styled name for a unit — uses alternating color if parasitized
    def styledUnit(u : UnitFigure)(implicit game : Game) : String = {
        if (isParasitized(u)) {
            val origFac = originalFaction(u).|(u.faction)
            styledAlt("Acolyte", origFac, u.faction)
        } else {
            u.uclass.styled(u.faction)
        }
    }

    // Get the original faction of a parasitized unit
    def originalFaction(u : UnitFigure)(implicit game : Game) : |[Faction] =
        game.mindParasiteOriginalFaction.get(u.ref)

    // Check if an Acolyte SHOULD be parasitized (off-gate, shares area with enemy Insect)
    // BB Fix 80, v2.4.30 — Mind Parasite affects Earth Cats. Earth Cats are BB's
    // Cultist-equivalent: every effect targeting Cultists also targets Earth Cats
    // (Lunacy rule). The Acolyte filter is extended to also match EarthCat so the
    // parasite conversion fires when an Insect from Shaggai shares an off-gate
    // area with a BB Earth Cat. Earth Cats on the Moon are unaffected because
    // u.region.onMap is false for the Moon.
    def shouldParasitize(u : UnitFigure)(implicit game : Game) : |[Faction] = {
        if ((u.uclass != Acolyte && u.uclass != EarthCat) || u.onGate || !u.region.onMap) return None
        game.factions.find(f => f != u.faction && f.loyaltyCards.has(InsectsFromShaggaiCard) && f.at(u.region, InsectsFromShaggai).any)
    }

    // Convert an Acolyte (or BB Earth Cat) to MindParasiteCultist under the insect owner
    // BB Fix 80, v2.4.30 — Earth Cats are parasitizable; track the original
    // UnitClass so unparasitize restores the correct class.
    def parasitize(u : UnitFigure, insectOwner : Faction)(implicit game : Game) : Unit = {
        val originalFac = u.faction
        val originalUClass = u.uclass
        val region = u.region
        val origIndex = u.index
        // Remove original unit
        originalFac.units :-= u
        // Create parasitized version under insect owner with unique index
        val mpIndex = game.mindParasiteNextIndex
        game.mindParasiteNextIndex += 1
        val parasitized = new UnitFigure(insectOwner, MindParasiteCultist, mpIndex, region)
        insectOwner.units :+= parasitized
        // Track original faction, index, and UnitClass for restoration
        game.mindParasiteOriginalFaction += (parasitized.ref -> originalFac)
        game.mindParasiteOriginalIndex += (parasitized.ref -> origIndex)
        game.mindParasiteOriginalUClass += (parasitized.ref -> originalUClass)
        game.appendLog($(styledAlt(originalUClass.name, originalFac, insectOwner), "in", region, "afflicted with", "Mind Parasite".styled("nt")))
    }

    // Convert a MindParasiteCultist back to its original UnitClass under the original faction
    // BB Fix 80, v2.4.30 — restore using the tracked original UnitClass (Acolyte
    // or EarthCat). Default fallback is Acolyte for backward compatibility with
    // saves from before this fix.
    def unparasitize(u : UnitFigure)(implicit game : Game) : Unit = {
        val origFac = game.mindParasiteOriginalFaction.getOrElse(u.ref, u.faction)
        val region = u.region
        val origIndex = game.mindParasiteOriginalIndex.getOrElse(u.ref, u.index)
        val origUClass = game.mindParasiteOriginalUClass.getOrElse(u.ref, Acolyte)
        // Remove parasitized unit from insect owner
        u.faction.units :-= u
        // Create restored unit under original faction with original index and class
        val restored = new UnitFigure(origFac, origUClass, origIndex, region)
        origFac.units :+= restored
        // Clean up tracking
        game.mindParasiteOriginalFaction -= u.ref
        game.mindParasiteOriginalIndex -= u.ref
        game.mindParasiteOriginalUClass -= u.ref
        game.appendLog($(styledAlt(origUClass.name, origFac, u.faction), "in", region, "freed from", "Mind Parasite".styled("nt")))
    }

    // Run conversion check — called from triggers()
    // BB Fix 80, v2.4.30 — Mind Parasite affects Earth Cats. The Acolyte
    // candidate-set filter is extended to also include EarthCat so Earth
    // Cats co-located with enemy Insects from Shaggai get parasitized.
    def checkConversions()(implicit game : Game) : Unit = {
        // Convert Acolytes / Earth Cats that should be parasitized
        game.factions.foreach { f =>
            f.units.%(u => (u.uclass == Acolyte || u.uclass == EarthCat) && u.region.onMap && !u.onGate).foreach { u =>
                val sp = shouldParasitize(u)
                sp.foreach { insectOwner =>
                    parasitize(u, insectOwner)
                }
            }
        }
        // Un-parasitize MindParasiteCultists that are no longer in an insect area
        game.factions.foreach { f =>
            f.units.%(_.uclass == MindParasiteCultist).foreach { u =>
                val stillInInsectArea = f.at(u.region, InsectsFromShaggai).any
                if (!stillInInsectArea) {
                    unparasitize(u)
                }
            }
        }
    }

    // Old compatibility — check by Acolyte position (for canMove etc.)
    // BB Fix 80, v2.4.30 — Mind Parasite affects Earth Cats. Extend the
    // Acolyte filter to also match EarthCat so canMove / canBeMoved / parasite
    // controller checks treat Earth Cats co-located with enemy Insects as
    // parasite-controlled (Lunacy: Earth Cats are BB's Cultist-equivalent).
    def controller(u : UnitFigure)(implicit game : Game) : |[Faction] = {
        if (u.uclass == MindParasiteCultist) |(u.faction)
        else if ((u.uclass != Acolyte && u.uclass != EarthCat) || u.onGate) None
        else game.factions.find(f => f != u.faction && f.loyaltyCards.has(InsectsFromShaggaiCard) && f.at(u.region, InsectsFromShaggai).any)
    }
}

object NoFaction extends Faction {
    def name = "No Faction"
    def short = "NF"
    def style = "nt"

    def abilities = $
    def library = $
    def requirements(options : $[GameOption]) = $

    val allUnits = $

    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) = 0
}

trait Action extends Record {
    def question(implicit game : Game) : String
    def safeQ(implicit game : Game) = question(game).splt("<br/>").first
    def option(implicit game : Game) : String
    def unwrap : Action = this

    def isMore : Boolean = this match {
        case a : Wrapped => unwrap.isMore
        case a : More => true
        case _ => false
    }

    def isCancel : Boolean = this match {
        case a : Wrapped => unwrap.isCancel
        case a : Cancel => true
        case _ => false
    }

    def isSoft : Boolean = this match {
        case a : Wrapped => unwrap.isSoft
        case a : Soft => true
        case _ => false
    }

    def isInfo : Boolean = this match {
        case a : Wrapped => unwrap.isInfo
        case a : Info => true
        case _ => false
    }

    def isVoid : Boolean = this match {
        case a : Wrapped => unwrap.isVoid
        case a : Void => true
        case _ => false
    }

    def isOutOfTurn : Boolean = this match {
        case a : Wrapped => unwrap.isOutOfTurn
        case a : OutOfTurn => true
        case _ => false
    }

    def isNoClear : Boolean = this match {
        case a : Wrapped => unwrap.isNoClear
        case a : NoClear => true
        case _ => false
    }

    def isRecorded = (isMore || isCancel || isSoft || isVoid).not

    def unary_+(implicit wrapper : AskWrapper) = wrapper.ask = wrapper.ask.add(this)
}

trait FactionAction extends Action {
    def self : Faction
}

trait Soft extends Action
trait Cancel extends Action
trait Info extends Action
trait More extends Soft
trait PowerNeutral extends Action
trait Wrapped extends Action
trait NoClear extends Action

trait Continue

object Info {
    def apply(l : Any*)(g : Any*) = InfoAction(g.$, l.$)
}

case class MultiAsk(asks : $[Ask]) extends Continue

case class Ask(faction : Faction, actions : $[Action] = $) extends Continue {
    def add(a : Action) = Ask(faction, actions :+ a)
    def when(c : Boolean)(a : Action) = c.?(add(a)).|(this)
    def list(l : $[Action]) = Ask(faction, actions ++ l)
    def prepend(a : Action) = Ask(faction, a +: actions)
    def each[T](l : IterableOnce[T])(a : T => Action) = list(l.iterator.map(a).$)
    def each[T, U](l : IterableOnce[(T, U)])(a : (T, U) => Action) = list(l.iterator.map { case (t, u) => a(t, u) }.$)
    def some[T](l : IterableOnce[T])(a : T => IterableOnce[Action]) = list(l.iterator.flatMap(a).$)
    def group(t : Any*) = add(GroupAction(t.$.but("").mkString(" ")))
    def done(a : ForcedAction) = add(a.as("Done")(" "))
    def doneIf(c : Boolean)(a : ForcedAction) = c.?(add(a.as("Done"))).|(this)
    def skip(a : ForcedAction) = add(a.as("Skip"))
    def skipIf(c : Boolean)(a : ForcedAction) = c.?(add(a.as("Skip"))).|(this)
    def cancel = add(CancelAction)
    def cancelIf(c : Boolean) = c.?(add(CancelAction)).|(this)
    def bail(c : Continue) = actions.none.?(c).|(this)
}

case object StartContinue extends Continue
case class Force(action : Action) extends Continue
case class Then(action : Action) extends Continue
case class DelayedContinue(delay : Int, continue : Continue) extends Continue
case class RollD6(question : Game => String, roll : Int => ForcedAction) extends Continue
case class RollAgony(question : Game => String, roll : Int => ForcedAction) extends Continue
case class RollBattle(question : Game => String, n : Int, roll : $[BattleRoll] => ForcedAction) extends Continue
case class DrawES(question : Game => String, es1 : Int, es2 : Int, es3 : Int, draw : (Int, Boolean) => ForcedAction) extends Continue
case class GameOver(winners : $[Faction]) extends Continue
case object UnknownContinue extends Continue
case object TryAgain extends Continue

class AskWrapper(var ask : Ask)

object Asking {
    def apply(f : Faction) = new AskWrapper(Ask(f))
}

object RollBattle {
    def apply(faction : Faction, side : String, n : Int, roll : $[BattleRoll] => ForcedAction) : RollBattle = RollBattle("" + faction + " rolls " + (n == 0).?(" no dice").|((n == 1).?(" one die").|("" + n + " dice")) + " for " + side, n, roll)
}

trait SelfPerform { self : Action =>
    def perform(soft : VoidGuard)(implicit game : Game) : Continue
}

abstract class ForcedAction extends Action {
    def question(implicit game : Game) = "N/A"
    def option(implicit game : Game) = "N/A"
    def as(o : Any*) = new WrappedForcedAction(this, o.$)
}

case object CancelAction extends Action with Cancel {
    def question(implicit game : Game) = ""
    def option(implicit game : Game) = "Cancel"
}

case class GroupAction(t : String) extends Action with Info {
    def question(implicit game : Game) = t
    def option(implicit game : Game) = ""
}

case class InfoAction(g : $[Any], o : $[Any]) extends Action with Info {
    def question(implicit game : Game) = g./(game.desc).but("").mkString(" ")
    def option(implicit game : Game) = o./(game.desc).but("").mkString(" ")
}

trait HiddenAction extends Action with Info {
    def question(implicit game : Game) = ""
    def option(implicit game : Game) = ""
}

case object NeedOk extends HiddenAction


case class WrappedForcedAction(action : ForcedAction, o : $[Any]) extends Action with Wrapped {
    override def unwrap = action.unwrap
    def apply(q : Any*) = new WrappedQForcedAction(action, q.$, o)
    def question(implicit game : Game) = ""
    def option(implicit game : Game) = o./(game.desc).but("").mkString(" ")
}

case class WrappedQForcedAction(action : ForcedAction, q : $[Any], o : $[Any]) extends Action with Wrapped {
    override def unwrap = action.unwrap
    def question(implicit game : Game) = q./(game.desc).but("").mkString(" ")
    def option(implicit game : Game) = o./(game.desc).but("").mkString(" ")
}

trait Void { self : ForcedAction => }
trait OutOfTurn { self : Action => }

case object OutOfTurnReturn extends ForcedAction with Soft
case object OutOfTurnDone extends ForcedAction
case class OutOfTurnRepeat(faction : Faction, action : Action with Soft with OutOfTurn) extends ForcedAction with Soft



case object ReloadAction extends ForcedAction with Void
case object UpdateAction extends ForcedAction with Void
case class CommentAction(comment : String) extends ForcedAction with Void


trait Plan extends Record {
    val label : String
    val unselected : String = label
    val selected : String = label.hl
    val info : String = label.hh
    val group : String
    val requires : $[$[Plan]] = $($)
    def followers : $[Plan] = $
    def unfollowers : $[Plan] = $
}
trait DefaultPlan extends Plan
trait OnlyOnPlan extends Plan
trait OneOfPlan extends Plan with OnlyOnPlan


sealed abstract class GateDiplomacyPlan(val label : String) extends Plan {
    val group = "Gate Diplomacy".hh
}
case object GateDiplomacyPrompt extends GateDiplomacyPlan("Display all options") with DefaultPlan with OneOfPlan
case object GateDiplomacySkipAbandon extends GateDiplomacyPlan("Don't prompt abandoning gates") with OneOfPlan
case object GateDiplomacyCling extends GateDiplomacyPlan("Cling to the gates") with OneOfPlan


sealed abstract class HighPriestGatesPlan(val label : String) extends Plan {
    val group = "High Priests".hh
}
case object HighPriestGatesPrompt extends HighPriestGatesPlan("Prompt controlling a gate") with DefaultPlan with OneOfPlan
case object HighPriestGatesSkip extends HighPriestGatesPlan("Always prefer Acolytes on the gates") with OneOfPlan


sealed abstract class DevolvePlan(val label : String) extends Plan {
    val group = "Devolve".styled(GC)
}
case object DevolvePrompt extends DevolvePlan("Always prompt") with DefaultPlan with OneOfPlan
case object DevolveSkip extends DevolvePlan("Skip, unless Acolyte is under...") with OneOfPlan { override val followers = $(DevolveThreatOfCapture) }
trait DevolveThreat extends DevolvePlan { override val requires = $($(DevolveSkip)) }
case object DevolveThreatOfCapture extends DevolvePlan("...threat of capture") with DevolveThreat
case object DevolveThreatOfZingaya extends DevolvePlan("...threat of Zingaya") with DevolveThreat
case object DevolveThreatOfBeyondOne extends DevolvePlan("...threat of Beyond One") with DevolveThreat
case object DevolveThreatOfAttackOnGate extends DevolvePlan("...credible threat to the controlled gate") with DevolveThreat
case object DevolveThreatOfAttackOnGOO extends DevolvePlan("...credible threat of battle against GOO") with DevolveThreat


sealed abstract class DragonAscendingPlan(val label : String) extends Plan {
    val group = "Dragon Ascending".styled(OW)
}
case object DragonAscendingPrompt extends DragonAscendingPlan("Always prompt") with DefaultPlan with OneOfPlan
case object DragonAscendingSkip extends DragonAscendingPlan("Skip this action phase, unless...") with OneOfPlan { override val followers = $(DragonAscendingPowerPlus2) }
trait DragonAscendingPower extends DragonAscendingPlan with OnlyOnPlan {
    override val requires = $($(DragonAscendingSkip))
    override def unfollowers = $(DragonAscendingPowerPlus2, DragonAscendingPowerPlus3, DragonAscendingPowerPlus5, DragonAscendingPowerPlus7, DragonAscendingPowerPlus9).diff($(this))
    val power : Int
}
case object DragonAscendingPowerPlus2 extends DragonAscendingPlan("...to gain 2+ power") with DragonAscendingPower { val power = 2 }
case object DragonAscendingPowerPlus3 extends DragonAscendingPlan("...to gain 3+ power") with DragonAscendingPower { val power = 3 }
case object DragonAscendingPowerPlus5 extends DragonAscendingPlan("...to gain 5+ power") with DragonAscendingPower { val power = 5 }
case object DragonAscendingPowerPlus7 extends DragonAscendingPlan("...to gain 7+ power") with DragonAscendingPower { val power = 7 }
case object DragonAscendingPowerPlus9 extends DragonAscendingPlan("...to gain 9+ power") with DragonAscendingPower { val power = 9 }


sealed abstract class UnspeakableOathPlan(val label : String) extends Plan {
    val group = "Unspeakable Oath".hh
}
case object UnspeakableOathImmediately extends UnspeakableOathPlan("Queue Activation") with OneOfPlan {
    override val unselected = "§".styled("pain") + " " + label + " " + "§".styled("pain")
    override val selected = "§".styled("highlight") + " " + label.styled("kill") + " " + "§".styled("highlight")
    override val info = selected
}
case object UnspeakableOathPrompt extends UnspeakableOathPlan("Always prompt") with DefaultPlan with OneOfPlan
case object UnspeakableOathSkip extends UnspeakableOathPlan("Skip, unless...") with OneOfPlan { override val followers = $(UnspeakableOathThreatOfHPCapture, UnspeakableOathThreatOfAcolyteCapture, UnspeakableOathThreatOfAttackOnHighPriest, UnspeakableOathOpportunityEndOfPhase, UnspeakableOathThreatOfCatnapping) }
trait UnspeakableThreat extends UnspeakableOathPlan { override val requires = $($(UnspeakableOathSkip)) }
case object UnspeakableOathThreatOfHPCapture extends UnspeakableOathPlan("...threat of High Priest capture") with UnspeakableThreat
case object UnspeakableOathThreatOfGhroth extends UnspeakableOathPlan("...threat of Ghroth eliminating High Priest") with UnspeakableThreat
case object UnspeakableOathThreatOfThousandForms extends UnspeakableOathPlan("...threat of unopposed Thousand Forms") with UnspeakableThreat
case object UnspeakableOathThreatOfDryEternal extends UnspeakableOathPlan("...threat of battle again Rhan-Tegoth with no power") with UnspeakableThreat
case object UnspeakableOathOpportunityOfDreadCurse extends UnspeakableOathPlan("...opportunity for Dread Curse") with UnspeakableThreat
case object UnspeakableOathOpportunityEndOfPhase extends UnspeakableOathPlan("...end of Action Phase") with UnspeakableThreat
case object UnspeakableOathOpportunityFirstPlayer extends UnspeakableOathPlan("...become eligible First Player") with UnspeakableThreat

case object UnspeakableOathThreatOfAcolyteCapture extends UnspeakableOathPlan("...threat of Acolyte capture") with UnspeakableThreat
case object UnspeakableOathThreatOfCatnapping extends UnspeakableOathPlan("...threat of Catnapping to the Moon") with UnspeakableThreat
case object UnspeakableOathThreatOfAttackOnHighPriest extends UnspeakableOathPlan("...credible threat of High Priest being killed") with UnspeakableThreat
case object UnspeakableOathThreatOfAttackOnGate extends UnspeakableOathPlan("...credible threat to the controlled gate") with UnspeakableThreat
case object UnspeakableOathThreatOfAttackOnGOO extends UnspeakableOathPlan("...credible threat of battle against GOO") with UnspeakableThreat


class Player(private val f : Faction)(implicit game : Game) {
    var gates : $[Region] = $
    var abandoned : $[Region] = $

    var spellbooks : $[Spellbook] = $
    var upgrades : $[Spellbook] = $
    var borrowed : $[Spellbook] = $

    var oncePerBattle : $[Spellbook] = $
    var oncePerAction : $[Spellbook] = $
    var oncePerRound : $[Spellbook] = $
    var oncePerTurn : $[Spellbook] = $
    var oncePerGame : $[Spellbook] = $

    var ignorePerInstant : $[Spellbook] = $
    var ignorePerTurn : $[Spellbook] = $

    var ignorePerGame : $[Spellbook] = $
    var ignorePerGameNew : $[Spellbook] = $

    def clings = options.has(GateDiplomacy).not || commands.has(GateDiplomacyCling)

    var unfulfilled : $[Requirement] = f.requirements(game.options)

    var power : Int = 8
    var doom : Int = 0
    var es : $[ElderSign] = $
    var revealed : $[ElderSign] = $
    var loyaltyCards : $[LoyaltyCard] = $
    var hired : Boolean = false

    var battled : $[Region] = $
    var acted : Boolean = false

    var units : $[UnitFigure] = f.allUnits.indexed./((uc, i) => new UnitFigure(f, uc, f.allUnits.take(i).count(uc) + 1, f.reserve))

    var hibernating = false
    var iceAge : |[Region] = None
    var unitGate : |[UnitFigure] = None

    var active = true

    var plans : $[Plan] = $
    var commands : $[Plan] = $

    def allGates = gates ++ unitGate./(_.region).$
    def needs(rq : Requirement) = unfulfilled.contains(rq)
    def has(sb : Spellbook) = f.abilities.contains(sb) || spellbooks.contains(sb) || upgrades.contains(sb) || (borrowed.contains(sb) && spellbooks.contains(AncientSorcery) && !used(AncientSorcery))
    def used(sb : Spellbook) = oncePerGame.contains(sb) || oncePerTurn.contains(sb) || oncePerRound.contains(sb) || oncePerAction.contains(sb)
    def can(sb : Spellbook) = has(sb) && !used(sb)
    def ignored(sb : Spellbook) = ignorePerGame.contains(sb) || ignorePerTurn.contains(sb) || ignorePerInstant.contains(sb)
    def want(sb : Spellbook) = can(sb) && !ignored(sb)
    def hasAllSB = unfulfilled.none
    def unclaimedSB = f.library.num - spellbooks.num - unfulfilled.num
    def present(region : Region) = units.exists(_.region == region)
    def at(region : Region) = units.%(_.region == region)
    def at(region : Region, uclass : UnitClass) = units.%(_.region == region).%(_.uclass == uclass)
    def at(region : Region, utype : UnitType) = units.%(_.region == region).%(_.uclass.utype == utype)
    def at(region : Region, utype : UnitType, utype2 : UnitType) = units.%(_.region == region).%(u => u.uclass.utype == utype || u.uclass.utype == utype2)
    def pool = units.%(_.region == f.reserve)
    def onMap(uclass : UnitClass) = units.%(u => u.region.onMap && u.uclass == uclass)
    def onMap(utype : UnitType) = units.%(u => u.region.onMap && u.uclass.utype == utype)
    def all(uclass : UnitClass) = units.%(u => u.region.inPlay && u.uclass == uclass)
    def all(utype : UnitType) = units.%(u => u.region.inPlay && u.uclass.utype == utype)
    def allInPlay = units.%(_.region.inPlay)
    // HIGH-2 revised: ElderGod (Bastet) counts as GOO per spec §1.3 for capture-blocking,
    // Nyarlathotep Harbinger 2-ES, multi-GOO checks, etc.
    def goos = units.%(_.region.inPlay).%(_.uclass.isGOO)
    def cultists = units.%(_.region.inPlay).%(_.uclass.utype == Cultist)
    // Lunacy (BB): Earth Cats count as Cultists for enemy-targeting effects.
    def cultistsForEnemyTargeting = units.%(_.region.inPlay).%(_.targetableAsCultistByEnemy)
    def acolytes = units.%(_.region.inPlay).%(_.uclass == Acolyte)

    def goo(uclass : UnitClass) = all(uclass).headOption.getOrElse(new UnitFigure(f, uclass, 1, f.reserve))
    def has(uclass : UnitClass) = all(uclass).any

    def satisfy(rq : Requirement, text : String, es : Int = 0) {
        if (f.needs(rq)) {
            f.unfulfilled = f.unfulfilled.but(rq)
            f.log("achieved", text.styled(f), ((es + rq.es) > 0).??("and gained " + (es + rq.es).es))
            f.takeES(es + rq.es)
        }
    }

    def satisfyIf(rq : Requirement, text : String, c : => Boolean, p : Int = 0) {
        if (f.needs(rq)) {
            if (c) {
                f.satisfy(rq, text, 0)
                if (p != 0) {
                    f.power += p
                    f.log("got", p.power)
                }
            }
        }
    }

    def takeES(n : Int) {
        val total = factions./(f => f.es.num + f.revealed.num).sum
        var count = n

        if (total + count > 36) {
            f.log("got", (total + count - 36).doom, "instead of", (total + count - 36).es)
            f.doom += (total + count - 36)
            count = 36 - total
        }

        f.es ++= count.times(ElderSign(0))
    }

    def place(uc : UnitClass, r : Region) {
        // Bubastis Moon Guard (Fix 45, v2.4.13): non-BB factions may NEVER place
        // any unit, building, or gate-equivalent on the Moon. The ONLY way for a
        // non-BB unit to enter the Moon is BB's Catnapping action (which writes
        // u.region = BB.moon directly in FactionBB.scala — it does not go
        // through this place() chokepoint). This guard is defense-in-depth:
        // every destination-list filter (areas, board.connected, summonRegions,
        // game.gates) already excludes Moon for non-BB callers, but if a future
        // ability accidentally lets Moon slip through, this stops it cold.
        // Also blocks Yog-Sothoth gate-stacking, Cathedrals, Chaos Gates, Craters,
        // Pyramids (when added), Glaciers, Worms of Groth, Insects from Shaggai,
        // Mother Hydra Acolytes (Zygote), Hounds, Moonbeasts, etc. on Moon.
        // Exception: OW with TheyBreakThrough may summon to Moon (faction power).
        // Exception: BG Avatar (BB Fix 71, v2.4.29) — Shub-Niggurath may avatar
        // to the Moon, and may avatar off the Moon with ANY unit (the swap-
        // target moves to the Moon during the swap). Avatar is unrestricted
        // with regards to moon access. Detected via game.bgInAvatar transient
        // flag set in FactionBG.AvatarAction.
        // Exception: AN Dematerialization (BB Fix, v2.4.29) — AN may
        // dematerialize a unit onto the Moon. Detected via
        // game.anInDematerialize transient flag set in
        // FactionAN.DematerializationMoveUnitAction.
        // Exception: Dimensional Shambler deploy (BB Fix, v2.4.29) — ANY
        // faction's Shambler may deploy to the Moon. Detected via
        // game.shamblerInDeploy transient flag set in
        // NeutralMonsters.ShamblerDeployAction.
        // Exception: GC Unsubmerge (BB Bullet 50, v2.4.29) — Great Cthulhu
        // may unsubmerge from the deep onto the Moon (the Moon is adjacent
        // to all regions for arrival purposes; GC's submerged court resides
        // off-map and may surface anywhere). Detected via game.gcInUnsubmerge
        // transient flag set in FactionGC.UnsubmergeAction.
        // Exception: Undimensioned (BB Fix 77, v2.4.30) — any faction may
        // rearrange a unit onto the Moon via Undimensioned, but only if the
        // faction already has at least one unit on the Moon (the destination-
        // list filter at NeutralSpellbooks.scala enforces the eligibility;
        // this flag carves the place() guard exception). Detected via
        // game.undimensionedInPlay transient flag.
        // Exception: Insects from Shaggai initial deploy (BB Fix 80, v2.4.30)
        // — the loyalty-card deploy step explicitly allows placement in any
        // Area for any faction, which includes the Moon. Detected via
        // game.insectsInDeploy transient flag set in NeutralMonsters.scala.
        // Exception: Ghosts of Ib (BB Fix 86, v2.4.31) — Bokrug's doom-phase
        // placement may target the Moon (Moon counts as a valid "Area" for
        // Ghosts of Ib). Detected via game.ghostsInPlace transient flag set
        // in IGOOs.scala GhostsOfIbChooseAction.
        // Exception: Byatis God of Forgetfulness (BB Fix 87, v2.4.31) — when
        // Byatis is on the Moon, cultists/EarthCats forgotten there land on
        // the Moon. Detected via game.byatisInForgetfulness transient flag
        // set in IGOOs.scala GodOfForgetfulnessAction.
        if (f != BB && !(f == OW && f.can(TheyBreakThrough)) && !(f == BG && game.bgInAvatar) && !(f == AN && game.anInDematerialize) && !game.shamblerInDeploy && !(f == GC && game.gcInUnsubmerge) && !game.undimensionedInPlay && !game.insectsInDeploy && !game.ghostsInPlace && !game.byatisInForgetfulness && r == BB.moon) {
            f.log("placement of", uc.styled(f), "on", BB.moon, "blocked: only BB units may enter the Moon (Catnapping is the sole exception, and OW after They Break Through)")
        }
        else {
            val u = f.pool(uc).first
            u.region = r
            // Universal CG trigger — single chokepoint for all placements. Any
            // non-FB placement of a non-Building unit into a region with an FB
            // Revenant or Ghatanothoa must register the region for CG dispatch.
            // Covers: SummonAction, AwakenAction, RecruitAction (via Game.scala),
            // StartAction's initial acolyte placement (skipped — FB check guards),
            // plus every expansion that calls `self.place(...)` directly
            // (BG Avatar's replacement, AN Cathedral/Festival, YS Screaming Dead,
            // TS Undulate/Hecatomb, SL Lethargy, etc.).
            if (f != FB && uc.utype != Building &&
                game.fbHasCGActive &&
                FB.units.exists(u => u.region == r && (u.uclass == RevenantOfKnaa || u.uclass == Ghatanothoa)))
                game.fbCyclopeanGazeActionRegions :+= r
            game.logElderThingMovementBlocks(u, r)
        }
    }

    def canAccessGate(r : Region) = gates.contains(r) || f.unitGate.?(_.region == r) || (has(TheyBreakThrough) && game.gates.contains(r)) || (f == BB && r == BB.moon)

    def canAttack(r : Region)(e : Faction) = e.at(r).any && f.strength(at(r), e) > 0 && at(r).%(_.canBattle).any

    def canCapture(r : Region)(e : Faction) : Boolean =
        if (f == e)
            false
        else
        if (e.at(r, Cultist).none)
            false
        else
        // Great Race of Yith: Possession — captures ignoring all protection
        if (f.at(r, GreatRaceOfYith).any)
            true
        else
        if (e.at(r, GOO, ElderGod).any)
            false
        else
        if (f.at(r, GOO, ElderGod).any)
            true
        else
        if (e.at(r, Terror).any)
            false
        else
        if (e.at(r, Monster).any)
            e.at(r, Monster).none
        else
        // Elder Thing Mind Control: Ferox suppressed if Ithaqua shares area with enemy Elder Thing
        if (e.has(Ferox) && e.has(Ithaqua) && e.allInPlay.%(_.uclass == Ithaqua).any && !ElderThingMindControl.suppresses(e.goo(Ithaqua)))
            false
        else
        if (f.at(r, Terror).any)
            true
        else {
            f.at(r, Monster).%(u => u.uclass.canCapture(u)).any
    }

    def summonRegions : $[Region] = (f.allGates.onMap ++ f.can(TheyBreakThrough).??(f.enemies./~(_.allGates) ++ game.abandonedGates) ++ (f == BB).??($(BB.moon))).nex.distinct

    def iced(r : Region) : IceAges = {
        if (game.anyIceAge.not)
            NoIceAges
        else {
            val movedHere = f.at(r).tag(Moved).any
            // Show Ice Age unless explicitly moved here this turn *and* it's a normal (paid) move.
            if (movedHere && !f.oncePerAction.contains(FromBelow))
                NoIceAges
            else
                IceAges(factions.%(f => f.iceAge./(_ == r).|(false) && f.can(IceAge)).but(f))
        }
    }

    def taxIn(r : Region) : Int = f.iced(r).tax

    def affords(n : Int)(r : Region) = f.power >= f.taxIn(r) + n

    def payTax(r : Region) : Int = {
        val s = iced(r)
        if (s.any) {
            f.power -= s.tax
            f.log("lost", s.tax.power, "due to", s.list./(IceAge.styled).mkString(", "))
            s.tax
        }
        else
            0
    }
}

object RitualTrack {
    val for3 = 5 :: 6 :: 7 :: 8 :: 9 :: 10 :: 999
    val for4 = 5 :: 6 :: 7 :: 7 :: 8 :: 8 :: 9 :: 10 :: 999
    val for5 = 5 :: 6 :: 6 :: 7 :: 7 :: 8 :: 8 :: 9 :: 9 :: 10 :: 999
}

case class IceAges(list : $[Faction]) {
    def any = list.any
    def tax = list.num
    def elem = list./(e => " (" + IceAge.styled(e) + ")").mkString("")
    override def toString = elem
}

object NoIceAges extends IceAges($)


sealed trait GameOption extends Record

case object QuickGame extends GameOption

case object GateDiplomacy extends GameOption
case object AsyncActions extends GameOption

case object HighPriests extends GameOption
case object NeutralSpellbooks extends GameOption
case object NeutralMonsters extends GameOption
case object NeutralTerrors extends GameOption
case object IGOOs extends GameOption

case object IceAgeAffectsLethargy extends GameOption
case object Opener4P10Gates extends GameOption
case object OpenerCheapMutants extends GameOption
case object OpenerYogCurseDie extends GameOption
case object DemandTsathoggua extends GameOption
case object SleeperEasierSBR extends GameOption
case object SleeperEnergyNexusPreBattle extends GameOption
case object DSAlternateSpellbooks extends GameOption
// BUBASTIS: alt-variant spellbook option (Syzygy replaces Catabolism, Carnivore replaces Ailurophobia)
case object BBAlternateSpellbooks extends GameOption


trait MapOption extends GameOption
case object MapEarth33 extends MapOption
case object MapEarth35 extends MapOption
case object MapEarth53 extends MapOption
case object MapEarth55 extends MapOption
case object MapLibrary33 extends MapOption
case object MapLibrary35 extends MapOption
case object MapLibrary53 extends MapOption
case object MapLibrary55 extends MapOption

abstract class LoyaltyCardGameOption(val lc : LoyaltyCard) extends GameOption

sealed trait NeutralMonsterOption extends LoyaltyCardGameOption
case object UseGhast extends LoyaltyCardGameOption(GhastCard) with NeutralMonsterOption
case object UseGug extends LoyaltyCardGameOption(GugCard) with NeutralMonsterOption
case object UseShantak extends LoyaltyCardGameOption(ShantakCard) with NeutralMonsterOption
case object UseStarVampire extends LoyaltyCardGameOption(StarVampireCard) with NeutralMonsterOption
case object UseVoonith extends LoyaltyCardGameOption(VoonithCard) with NeutralMonsterOption
case object UseDimensionalShamblers extends LoyaltyCardGameOption(DimensionalShamblerCard) with NeutralMonsterOption
case object UseGnorri extends LoyaltyCardGameOption(GnorriCard) with NeutralMonsterOption

// New Terrors
sealed trait NeutralTerrorOption extends LoyaltyCardGameOption
case object UseDhole extends LoyaltyCardGameOption(DholeCard) with NeutralTerrorOption
case object UseGreatRaceOfYith extends LoyaltyCardGameOption(GreatRaceOfYithCard) with NeutralTerrorOption
case object UseQuachilUttaus extends LoyaltyCardGameOption(QuachilUttausCard) with NeutralTerrorOption
case object UseShadowPharaoh extends LoyaltyCardGameOption(ShadowPharaohCard) with NeutralTerrorOption
case object UseHoundOfTindalos extends LoyaltyCardGameOption(HoundOfTindalosCard) with NeutralTerrorOption
case object UseBrownJenkin extends LoyaltyCardGameOption(BrownJenkinCard) with NeutralTerrorOption
case object UseElderShoggoth extends LoyaltyCardGameOption(ElderShoggothCard) with NeutralTerrorOption

// New Monsters
case object UseMoonbeast extends LoyaltyCardGameOption(MoonbeastCard) with NeutralMonsterOption
case object UseAlbinoPenguins extends LoyaltyCardGameOption(AlbinoPenguinsCard) with NeutralMonsterOption
case object UseElderThing extends LoyaltyCardGameOption(ElderThingCard) with NeutralMonsterOption
case object UseLengSpider extends LoyaltyCardGameOption(LengSpiderCard) with NeutralMonsterOption
case object UseSatyr extends LoyaltyCardGameOption(SatyrCard) with NeutralMonsterOption
case object UseInsectsFromShaggai extends LoyaltyCardGameOption(InsectsFromShaggaiCard) with NeutralMonsterOption
case object UseServitor extends LoyaltyCardGameOption(ServitorCard) with NeutralMonsterOption

sealed trait IGOOOption extends LoyaltyCardGameOption
case object UseByatis extends LoyaltyCardGameOption(ByatisCard) with IGOOOption
case object UseAbhoth extends LoyaltyCardGameOption(AbhothCard) with IGOOOption
case object UseDaoloth extends LoyaltyCardGameOption(DaolothCard) with IGOOOption
case object UseNyogtha extends LoyaltyCardGameOption(NyogthaCard) with IGOOOption
case object UseTulzscha extends LoyaltyCardGameOption(TulzschaCard) with IGOOOption
case object UseYgolonac extends LoyaltyCardGameOption(YgolonacCard) with IGOOOption
case object UseAzathothIGOO extends LoyaltyCardGameOption(AzathothIGOOCard) with IGOOOption
case object UseCthugha extends LoyaltyCardGameOption(CthughaCard) with IGOOOption
case object UseMotherHydra extends LoyaltyCardGameOption(MotherHydraCard) with IGOOOption
case object UseYig extends LoyaltyCardGameOption(YigCard) with IGOOOption
case object UseFatherDagon extends LoyaltyCardGameOption(FatherDagonCard) with IGOOOption
case object UseGhatanotoaIGOO extends LoyaltyCardGameOption(GhatanotoaIGOOCard) with IGOOOption
case object UseBloatedWoman extends LoyaltyCardGameOption(BloatedWomanCard) with IGOOOption
case object UseAtlachNacha extends LoyaltyCardGameOption(AtlachNachaCard) with IGOOOption
case object UseBokrug extends LoyaltyCardGameOption(BokrugCard) with IGOOOption
case object UseGlaakiIGOO extends LoyaltyCardGameOption(GlaakiIGOOCard) with IGOOOption

case class PlayerCount(n : Int) extends GameOption

object GameOptions {
    val all = $(
        QuickGame,
        GateDiplomacy,
        AsyncActions,
        HighPriests,
        NeutralSpellbooks,
        NeutralMonsters,
        NeutralTerrors,
        IGOOs,
        IceAgeAffectsLethargy,
        Opener4P10Gates,
        OpenerCheapMutants,
        OpenerYogCurseDie,
        DemandTsathoggua,
        SleeperEasierSBR,
        SleeperEnergyNexusPreBattle,
        DSAlternateSpellbooks,
        // BUBASTIS: alt-variant spellbook option
        BBAlternateSpellbooks,
        MapEarth33,
        MapEarth35,
        MapEarth53,
        MapEarth55,
        MapLibrary33,
        MapLibrary35,
        MapLibrary53,
        MapLibrary55,
        UseGhast,
        UseGug,
        UseShantak,
        UseStarVampire,
	UseVoonith,
        UseDimensionalShamblers,
        UseGnorri,
        UseDhole,
        UseGreatRaceOfYith,
        UseQuachilUttaus,
        UseShadowPharaoh,
        UseHoundOfTindalos,
        UseBrownJenkin,
        UseElderShoggoth,
        UseMoonbeast,
        UseAlbinoPenguins,
        UseElderThing,
        UseLengSpider,
        UseSatyr,
        UseInsectsFromShaggai,
        UseServitor,
        UseByatis,
        UseAbhoth,
        UseDaoloth,
        UseNyogtha,
        UseTulzscha,
        UseYgolonac,
        UseAzathothIGOO,
        UseCthugha,
        UseMotherHydra,
        UseYig,
        UseFatherDagon,
        UseGhatanotoaIGOO,
        UseBloatedWoman,
        UseAtlachNacha,
        UseBokrug,
        UseGlaakiIGOO,
        PlayerCount(3),
        PlayerCount(4),
        PlayerCount(5),
    )
}

trait Expansion {
    def perform(a : Action, soft : VoidGuard)(implicit game : Game) : Continue

    def triggers()(implicit game : Game) : Unit = {}

    def eliminate(u : UnitFigure)(implicit game : Game) : Unit = {}

    def afterAction()(implicit game : Game) : Unit = {}

    implicit class ActionMatch(val a : Action) {
        def @@(t : Action => Continue) = t(a)
        def @@(t : Action => Boolean) = t(a)
    }
}


case object StartAction extends ForcedAction
case object SetupFactionsAction extends ForcedAction
case class CheckSpellbooksAction(then : ForcedAction) extends ForcedAction
case object AfterPowerGatherAction extends ForcedAction
case class BeforeFirstPlayerAction(l : $[Faction]) extends ForcedAction
case object FirstPlayerDeterminationAction extends ForcedAction
case object PlayOrderAction extends ForcedAction
case class PowerGatherAction(then : Faction) extends ForcedAction
case object DoomPhaseAction extends ForcedAction
case object ActionPhaseAction extends ForcedAction
case object GameOverPhaseAction extends ForcedAction

abstract class BaseFactionAction(private val q : Game => String, private val o : Game => String) extends FactionAction {
    def question(implicit game : Game) = q(game)
    def option(implicit game : Game) = o(game)
}
abstract class OptionFactionAction(private val o: Game => String) extends FactionAction {
    def option(implicit game : Game) = o(game)
}

case class StartingRegionAction(self : Faction, r : Region) extends ForcedAction
case class FirstPlayerAction(self : Faction, f : Faction) extends BaseFactionAction("First player", f)
case class PlayDirectionAction(self : Faction, order : $[Faction]) extends BaseFactionAction("Order of play", order.mkString(" > "))

trait MainQuestion extends FactionAction {
    def question(implicit game : Game) = game.nexed.some./(n => "" + EnergyNexus + " in " + n.mkString(", ")).|("" + self + (self.acted || self.battled.any).??(" unlimited") + " action") + " (" + (self.power > 0).?(self.power.power).|("0 power") + ")"
    override def safeQ(implicit game : Game) = self @@ {
        case f if game.nexed.any => "" + EnergyNexus + " in " + game.nexed.mkString(", ")
        case f if f.acted => "Unlimited actions"
        case f => "Main action"
    }
}

trait DoomQuestion extends FactionAction {
    def question(implicit game : Game) = "" + self + " doom phase (" + (self.power > 0).?(self.power.power).|("0 power") + ")"
    override def safeQ(implicit game : Game) = "" + self + " doom phase"
}

case class SpellbookAction(self : Faction, sb : Spellbook, then : ForcedAction) extends BaseFactionAction(implicit g => (self.unclaimedSB == 1).?("Receive spellbook").|("Receive " + self.unclaimedSB + " spellbooks"), {
    val p = s""""${self.short}", "${sb.name.replace('\\'.toString, '\\'.toString + '\\'.toString)}"""".replace('"'.toString, "&quot;")
    val qm = Overlays.imageSource("question-mark")
    "<div class=sbdiv>" +
        sb +
        s"""<img class=explain src="${qm}" onclick="event.stopPropagation(); onExternalClick(${p})" onpointerover="onExternalOver(${p})" onpointerout="onExternalOut(${p})" />""" +
    "</div>"
})

case class ElderSignAction(f : Faction, n : Int, value : Int, public : Boolean, then : ForcedAction) extends ForcedAction

case class DoomAction(faction : Faction) extends ForcedAction
case class DoomNextPlayerAction(faction : Faction) extends ForcedAction
case class DoomDoneAction(self : Faction) extends BaseFactionAction(" ", "Done".styled("power")) with PowerNeutral
// Doom phase mandatory choices — shown in DoomAction menu, must resolve before Done
case class InnsmouthLookDoomAction(self : Faction) extends OptionFactionAction("The Innsmouth Look".styled("nt") + " (mandatory)") with DoomQuestion
case class MessengerOfYigDoomAction(self : Faction, yigOwner : Faction) extends OptionFactionAction("Messenger of Yig".styled("nt") + " (mandatory)") with DoomQuestion

case class PreMainAction(faction : Faction) extends ForcedAction
case class PreActionPromptsAction(faction : Faction, l : $[Faction]) extends ForcedAction
case class MainGatesAction(faction : Faction) extends ForcedAction
case class MainAction(faction : Faction) extends ForcedAction

case class EndPhasePromptsAction(last : Faction, l : $[Faction]) extends ForcedAction


case class OutOfTurnRefresh(action : Action) extends HiddenAction

case class EndAction(self : Faction) extends ForcedAction
case class AfterAction(self : Faction) extends ForcedAction

case object ProceedBattlesAction extends ForcedAction

case class EndTurnAction(self : Faction) extends BaseFactionAction(" ", "Done".styled("power")) with PowerNeutral
case class NextPlayerAction(faction : Faction) extends ForcedAction

case class AdjustGateControlAction(self : Faction, changed : Boolean, then : ForcedAction) extends OptionFactionAction("Control gates") with MainQuestion with Soft
case class ControlGateAction(self : Faction, r : Region, u : UnitRef, then : ForcedAction) extends ForcedAction with NoClear
case class AbandonGateAction(self : Faction, r : Region, then : ForcedAction) extends ForcedAction with NoClear

case class RitualAction(self : Faction, cost : Int, k : Int) extends OptionFactionAction(implicit g => {
    val ipDiscount = if (self == FB) min(g.fbEffectiveIPDiscount, cost) else 0
    val effectiveCost = cost - ipDiscount
    val base = "Perform " + "Ritual of Annihilation".styled("doom") + " for " + effectiveCost.power +
        (ipDiscount > 0).??(" (" + "IP discounted".styled(FB.style) + ")")
    // TT Tablets of the Gods: show bonus ES count and HP elimination warning
    val tabletsExtra = (self == TT && self.can(TabletsOfTheGods)).??{
        val bonus = self.gates.count(r => self.at(r, HighPriest).any)
        " — " + TabletsOfTheGods.styled(TT) + ": +" + bonus.es + ", eliminate all " + HighPriest.styled(TT)
    }
    // TT Inerrant: show bonus ES count
    val inerrantExtra = (self == TT && self.can(Inerrant)).??{
        val bonus = g.board.regions.count(r => self.at(r).goos.any && g.factions.but(TT).exists(e => e.gates.has(r)))
        " — " + Inerrant.styled(TT) + ": +" + bonus.es
    }
    base + tabletsExtra + inerrantExtra
}) with DoomQuestion

case class RevealESMainAction(self : Faction, then : ForcedAction) extends BaseFactionAction("", "View " + "Elder Signs".styled("es")) with Soft with PowerNeutral
case class RevealESOutOfTurnAction(self : Faction) extends BaseFactionAction("Elder Signs", "View " + "Elder Signs".styled("es")) with Soft
case class InfoESOutOfTurnAction(self : Faction) extends BaseFactionAction("Elder Signs", "View " + "Elder Signs".styled("es")) with Soft
case class RevealESAction(self : Faction, es : $[ElderSign], power : Boolean, next : Action) extends BaseFactionAction(implicit g => "Elder Signs".styled("es") + " " + self.es./(_.short).mkString(" "), implicit g => (es.%(_.value > 0).num == 1).?("Reveal " + es(0).short).|("Reveal all for " + self.es./(_.value).sum.doom)) with OutOfTurn
case class InfoESAction(self : Faction, es : $[ElderSign], power : Boolean, next : Action) extends BaseFactionAction(implicit g => "Elder Signs".styled("es") + " " + self.es./(_.short).mkString(" "), implicit g => (es.%(_.value > 0).num == 1).?("Reveal " + es(0).short).|("Reveal all for " + self.es./(_.value).sum.doom)) with Info

case class PassAction(self : Faction) extends OptionFactionAction("Pass and lose remaining power") with MainQuestion

case class MoveMainAction(self : Faction) extends OptionFactionAction("Move") with MainQuestion with Soft
case class MoveContinueAction(self : Faction, moved : Boolean) extends ForcedAction with Soft
case class MoveSelectAction(self : Faction, u : UnitRef, r : Region, cost : Int) extends ForcedAction with Soft
case class MoveAction(self : Faction, u : UnitRef, from : Region, to : Region, cost : Int) extends ForcedAction
case class MovedAction(self : Faction, u : UnitRef, from : Region, to : Region) extends ForcedAction
case class MoveDoneAction(self : Faction) extends ForcedAction

case class AttackMainAction(self : Faction, l : $[Region], effect : |[Spellbook]) extends OptionFactionAction("Battle") with MainQuestion with Soft
case class AttackAction(self : Faction, r : Region, f : Faction, effect : |[Spellbook]) extends BaseFactionAction(implicit g => "Battle in " + r + effect./(" with " + _).|("") + self.iced(r), f)

case class BuildGateMainAction(self : Faction, l : $[Region]) extends OptionFactionAction("Build Gate") with MainQuestion with Soft
case class BuildGateAction(self : Faction, r : Region) extends BaseFactionAction(implicit g => "Build gate" + g.forNPowerWithTax(r, self, 3 - self.has(UmrAtTawil).??(1)) + " in", r)

case class CaptureMainAction(self : Faction, l : $[Region], effect : |[Spellbook]) extends OptionFactionAction("Capture") with MainQuestion with Soft
case class CaptureAction(self : Faction, r : Region, f : Faction, effect : |[Spellbook]) extends ForcedAction
case class CaptureTargetAction(self : Faction, r : Region, f : Faction, ur : UnitRef, effect : |[Spellbook]) extends ForcedAction
// Mind Parasite: separate capture flow for parasitized cultists
case class MindParasiteCaptureMainAction(self : Faction) extends OptionFactionAction("Attempt to Capture Acolyte with Mind Parasite") with MainQuestion with Soft
case class MindParasiteCaptureTargetAction(self : Faction, r : Region, ur : UnitRef) extends ForcedAction
// Mind Parasite: original faction can block capture of their parasitized cultist
case class MindParasiteBlockCaptureAction(self : Faction, captor : Faction, r : Region, ur : UnitRef) extends BaseFactionAction("Mind Parasite", "Block capture of parasitized Acolyte")
case class MindParasiteAllowCaptureAction(self : Faction, captor : Faction, r : Region, ur : UnitRef) extends BaseFactionAction("Mind Parasite", "Allow capture")

case class RecruitMainAction(self : Faction, uc : UnitClass, l : $[Region]) extends OptionFactionAction("Recruit " + uc.styled(self)) with MainQuestion with Soft
case class RecruitAction(self : Faction, uc : UnitClass, r : Region) extends BaseFactionAction(implicit g => "Recruit " + uc.styled(self) + g.forNPowerWithTax(r, self, self.recruitCost(uc, r)) + " in", implicit g => r + self.iced(r))

case class SummonMainAction(self : Faction, uc : UnitClass, l : $[Region]) extends OptionFactionAction("Summon " + uc.styled(self)) with MainQuestion with Soft
case class SummonAction(self : Faction, uc : UnitClass, r : Region) extends BaseFactionAction(implicit g => "Summon " + uc.styled(self) + g.forNPowerWithTax(r, self, self.summonCost(uc, r)) + " in", implicit g => r + self.iced(r))
case class SummonedAction(self : Faction, uc : UnitClass, r : Region, l : $[Region]) extends ForcedAction

case class AwakenMainAction(self : Faction, uc : UnitClass, l : $[Region]) extends OptionFactionAction("Awaken " + uc.styled(self)) with MainQuestion with Soft
case class AwakenAction(self : Faction, uc : UnitClass, r : Region, cost : Int) extends BaseFactionAction(g => "Awaken " + uc.styled(self) + g.forNPowerWithTax(r, self, cost) + " in", implicit g => r + self.iced(r))
case class AwakenedAction(self : Faction, uc : UnitClass, r : Region, cost : Int) extends ForcedAction

case class Offer(f : Faction, n : Int)

case class SacrificeHighPriestDoomAction(self : Faction) extends OptionFactionAction("Sacrifice " + "High Priest".styled(self)) with DoomQuestion with Soft with PowerNeutral
case class SacrificeHighPriestMainAction(self : Faction) extends OptionFactionAction("Sacrifice " + "High Priest".styled(self)) with MainQuestion with Soft
case class SacrificeHighPriestPromptAction(self : Faction, then : ForcedAction) extends ForcedAction with Soft
case class SacrificeHighPriestAction(self : Faction, r : Region, then : ForcedAction) extends BaseFactionAction("Sacrifice to gain " + 2.power, HighPriest.styled(self) + " in " + r)

case object SacrificeHighPriestAllowedAction extends HiddenAction
case class SacrificeHighPriestOutOfTurnMainAction(self : Faction) extends BaseFactionAction("Unspeakable Oath".hl, "Sacrifice " + "High Priest".styled(self)) with Soft

case class CommandsMainAction(self : Faction) extends BaseFactionAction("  ", "Commands".styled(self)) with OutOfTurn with Soft
case class CommandsAddAction(self : Faction, plan : Plan) extends BaseFactionAction(plan.group, plan.unselected) with OutOfTurn with NoClear
case class CommandsRemoveAction(self : Faction, plan : Plan) extends BaseFactionAction(plan.group, plan.selected) with OutOfTurn with NoClear
case class CommandsInfoAction(self : Faction, plan : Plan) extends BaseFactionAction(plan.group, plan.unselected) with Info


class Game(val board : Board, val ritualTrack : $[Int], val setup : $[Faction], val logging : Boolean, val providedOptions : $[GameOption]) extends Expansion {
    private implicit val game : Game = this

    val options = providedOptions ++ $(PlayerCount(setup.num))

    var expansions : $[Expansion] =
        setup./~ {
            case GC => $(GCExpansion)
            case CC => $(CCExpansion)
            case BG => $(BGExpansion)
            case YS => $(YSExpansion)
            case SL => $(SLExpansion)
            case WW => $(WWExpansion)
            case OW => $(OWExpansion)
            case AN => $(ANExpansion)
            // Tombstalker (TS): register TS expansion handler for Death March, Hecatomb, tomes, and all TS actions
            case TS => $(TSExpansion)
            // Firstborn (FB): register FB expansion handler for action dispatch, triggers, and state
            case FB => $(FBExpansion)
            // Daemon Sultan (DS): register DS expansion handler
            case DS => $(DSExpansion)
            // Tcho-Tcho (TT): register TT expansion handler
            case TT => $(TTExpansion)
            // Bubastis (BB): register BB expansion handler
            case BB => $(BBExpansion)
        } ++
        options.has(NeutralSpellbooks).$(NeutralSpellbooksExpansion) ++
        (options.of[NeutralMonsterOption].any || options.of[NeutralTerrorOption].any).$(NeutralMonstersExpansion) ++
        options.of[IGOOOption].any.$(IGOOsExpansion) ++
        board.isLibraryMap.$(LibraryExpansion) ++
        $(this)

    val players = setup./(f => f -> new Player(f)).toMap

    // Opener Buff: Cheap Mutants — pool size stays at 4 (removed reduction)
    // Old behavior removed: no longer reduces mutant pool from 4 to 3


    val noPlayer = new Player(NoFaction)

    var neutrals : Map[NeutralFaction, Player] = Map()
    def factionlike = setup ++ neutrals.keys

    var starting = Map[Faction, Region]()
    var turn = 1
    var round = 1
    var doomPhase = false
    var gatherPowerPhase = false
    var endActionPhasePrompts = false
    def inActionPhase : Boolean = !doomPhase && !gatherPowerPhase && !endActionPhasePrompts
    var factions : $[Faction] = $
    var doomOrder : $[Faction] = $
    var first : Faction = setup.first
    var gates : $[Region] = $
    def allGates = gates ++ factions./~(_.unitGate)./(_.region)
    var cathedrals : $[Region] = $
    var desecrated : $[Region] = $

    // DS singleton vars must be reset here so undo replay (which creates a new Game) starts clean
    DS.chaosGateRegions = $
    DS.azathothTrack = 0
    DS.azathothDieRoll = 0
    DS.startingDecided = false
    // TS singleton vars (TSExpansion object) — same reason; undo creates a new Game and these
    // would otherwise persist across instances and corrupt replay state. (Parallel-guide Fix 33)
    TSExpansion.shepherdDoneThisGather = false
    TSExpansion.pgrLastFaction = null
    TSExpansion.pureDHRitualsDone = 0
    TSExpansion.pureDHMarkerIndices = $
    TSExpansion.graspingDeadRemaining = $()
    TSExpansion.graspingDeadFought = $()
    TSExpansion.graspingDeadActive = false
    var ritualMarker = 0
    var ritualHistory : $[Faction] = $
    var ritualHistoryCeremony : $[Boolean] = $
    var battle : |[Battle] = None
    var nexed : $[Region] = $
    var battleResumePhase : |[String] = None  // Energy Nexus PB: resume battle at this phase after SL turn
    var queue : $[Battle] = $
    var anyIceAge : Boolean = false
    var lastDaolothRegion : |[Region] = None
    var nextReplayActionHint : |[String] = None
    var tulzschaFlameTurn : Int = 1
    var neutralSpellbooks : $[Spellbook] = options.contains(NeutralSpellbooks).$(MaoCeremony, Recriminations, Shriveling, StarsAreRight, UmrAtTawil, Undimensioned)
    var loyaltyCards : $[LoyaltyCard] = options.of[LoyaltyCardGameOption]./(_.lc)

    // Solution for keeping track of use cases for dematerialization, for the AN bot.
    var demCaseMap : Map[Region, Int] = areas.map(r => r -> 0).toMap

    // Tcho-Tcho (TT) state: tribe selection and Ubbo-Sathla Growth counter
    var ttTribe : TTTribe = TribeLeng  // default; overwritten by TTChooseTribeAction
    var ubboGrowth : Int = 0           // Ubbo-Sathla's combat value; starts at 0, grows via Hell's Banquet
    var ttHellsBanquetDone : Boolean = false  // sentinel: prevents Hell's Banquet re-firing when DoomAction re-entered after roll
    var ttTribeChosen : Boolean = false        // sentinel: tribe selection asked once on first DoomAction

    // Bubastis (BB) doom-phase passive sentinels — prevent Ailurophobia / Syzygy from
    // re-firing each time DoomAction(BB) is re-entered (CRIT-7). Reset at end of BB's
    // doom phase via DoomDoneAction(BB).
    var bbAilurophobiaDone : Boolean = false
    var bbSyzygyDone       : Boolean = false

    // Tombstalker (TS) state: Death's Head counter, Cursed Tome stack index, and per-faction tome ownership
    var deathsHead : Int = 0
    var tsTomesOnCard : Int = 0
    var cursedTomesOwned : Map[Faction, $[(Int, Boolean)]] = Map()

    // Firstborn (FB) state — all stored on Game so undo (which creates a new Game and replays
    // recorded actions) properly resets it. NEVER use FBExpansion singleton vars for state
    // that needs to survive undo, as the singleton persists across `new Game()` instances.
    var fbAuguryKills : Int = 0
    var fbGhatnothoaAwakenings : Int = 0
    var fbCraters : $[Region] = $
    var fbInfernalPactDiscount : Int = 0
    // Elder Thing Mind Control: effective discount is 0 if Ghatanothoa suppressed
    def fbEffectiveIPDiscount : Int =
        if (factions.has(FB) && FB.has(Ghatanothoa) && FB.all(Ghatanothoa).any && ElderThingMindControl.suppresses(FB.goo(Ghatanothoa))(this)) 0
        else fbInfernalPactDiscount
    // Round 8 Bug 75: removed fbInfernalPactPowerAdded — discount is no longer
    // added to self.power on flip. It's a separate pool that only becomes
    // "power" temporarily during cost-bearing action intercepts (transient boost
    // pattern). fbInfernalPactStartPower is still used by Cancel to clamp power
    // back if needed, though with separate pools the clamp is a no-op.
    var fbInfernalPactStartPower : Int = -1
    // Round 8 Bug 40: widened from $[FactionSpellbook] to $[Spellbook] to support
    // IGOO spellbooks (NeutralSpellbook) being flipped facedown via Infernal Pact
    var fbInfernalPactFlipped : $[Spellbook] = $
    // Round 8 Bug 72: snapshots of FB.spellbooks and FB.unfulfilled taken on the
    // first flip of an IP session. Used by IP Cancel handlers to revert any
    // spellbook awarded mid-session (when flipping the 2nd spellbook facedown
    // satisfied FBTwoFacedownSpellbooks). Without these, cancel reverts the
    // flipped spellbooks and refunded power but LEAVES the newly-earned
    // spellbook in place, which breaks "cancel restores pre-IP state".
    var fbInfernalPactSpellbooksBeforeSession : $[Spellbook] = $
    var fbInfernalPactUnfulfilledBeforeSession : $[Requirement] = $
    // Round 9 bug fix: IP can be used both BEFORE and AFTER the main action
    // (the after-main IP is for unlimited actions like FBTheAllSpellbooks
    // battles). When the main action commits, any pre-main flips must become
    // NON-CANCELABLE — cancelling a post-main IP session must NOT undo the
    // pre-main flips. Mechanism: this var is the "floor" discount that can't
    // be refunded by a subsequent Cancel, set when the main action's state
    // mutates (EndAction) and cleared at end of turn.
    var fbInfernalPactCommittedDiscount : Int = 0
    // Library tomes flipped via Infernal Pact. Cleared on commit (EndAction) and end of turn.
    var fbInfernalPactFlippedTomes : $[LibraryTome] = $
    // §19b: per-turn cancel flag to prevent IP cancel→reenter loop
    var fbInfernalPactCancelledThisTurn : Boolean = false
    // Round 9 bug fix: some action handlers (e.g. AN GiveWorstMonster /
    // GiveBestMonster SBRs) place units into other factions' pools/regions
    // as a game-level effect, not as a region-targeting action by the
    // receiving or summoning faction. CG must NOT fire for those. Set this
    // flag true before a qualifying `place()` or `u.region = r` call, false
    // after. The UnitFigure region setter skips the CG append when true.
    var fbSuppressCGForPlacement : Boolean = false
    // Performance: cached flag for CG active check. Updated when spellbooks change.
    // Avoids repeated factions.has(FB) + FB.has(CG) + oncePerGame checks per unit move.
    def fbHasCGActive : Boolean = factions.has(FB) && FB.can(CyclopeanGaze)
    var fbDevilsMarkUsedThisDoom : Boolean = false
    // BB Fix 71 (v2.4.29): BG Avatar transient flag. Set true while a BG Avatar
    // action is resolving (Shub swap to/from any region including Moon). The
    // Moon-entry place() guard (BB Fix 65) checks this flag to permit BG units
    // moving onto the Moon during Avatar resolution. BG avatar is unrestricted
    // with regards to moon access — Shub may avatar to the Moon, and Shub may
    // avatar off the Moon swapping with ANY unit (the swap-target moves to the
    // Moon, which is the only Avatar path that ever places a non-BB unit on
    // the Moon and must be allowed). Most Avatar moves use direct
    // `u.region = r` assignment (which bypasses place()), so this flag is
    // primarily defense-in-depth for any future place() route through Avatar.
    var bgInAvatar : Boolean = false
    // BB Fix (v2.4.29): AN Dematerialization transient flag. Set true while
    // an AN Dematerialization move is resolving (unit being relocated onto
    // its destination, which may be BB.moon). The Moon-entry place() guard
    // (BB Fix 65) checks this flag to permit AN units moving onto the Moon
    // during Dematerialization resolution. The Dematerialization case in
    // FactionAN.scala uses direct `u.region = d` assignment (which bypasses
    // place()), so this flag is primarily defense-in-depth for any future
    // place() route through Dematerialization.
    var anInDematerialize : Boolean = false
    // BB Fix (v2.4.29): Dimensional Shambler deploy transient flag. Set true
    // while a Shambler deploy is resolving (Shambler relocated from the
    // faction-card hold to a chosen destination, which may be BB.moon).
    // The Shambler deploy explicitly opens the Moon to ANY faction — this
    // is broader than other Moon allowances. The Moon-entry place() guard
    // (BB Fix 65) checks this flag to permit any faction's Shambler moving
    // onto the Moon during deploy. ShamblerDeployAction in NeutralMonsters
    // .scala uses direct `u.region = r` assignment (which bypasses place()),
    // so this flag is primarily defense-in-depth for any future place()
    // route through Shambler deploy.
    var shamblerInDeploy : Boolean = false
    // BB Bullet 50 (v2.4.29): GC Unsubmerge transient flag. Set true while a
    // GC Unsubmerge is resolving (Cthulhu and his court relocated from the
    // submerged hold (GC.deep) to a chosen destination, which may be
    // BB.moon). The Moon-entry place() guard (BB Fix 65) checks this flag
    // to permit GC units moving onto the Moon during Unsubmerge resolution.
    // The UnsubmergeAction in FactionGC.scala uses direct `u.region = r`
    // assignment (which bypasses place()), so this flag is primarily
    // defense-in-depth for any future place() route through Unsubmerge.
    var gcInUnsubmerge : Boolean = false
    // BB Fix 77 (v2.4.30): Undimensioned (neutral spellbook) transient flag.
    // Set true while an Undimensioned rearrange step is resolving (faction
    // unit relocated among areas the faction occupies, which may include
    // BB.moon if the faction already has at least one unit there). The
    // Moon-entry place() guard checks this flag to permit any faction's
    // unit moving onto the Moon during Undimensioned. UndimensionedAction
    // in NeutralSpellbooks.scala uses direct `u.region = r` assignment
    // (which bypasses place()), so this flag is primarily defense-in-depth
    // for any future place() route through Undimensioned.
    var undimensionedInPlay : Boolean = false
    // BB Fix 80 (v2.4.30): Insects from Shaggai initial-deploy transient
    // flag. Set true while the Insects-from-Shaggai loyalty-card initial
    // deployment is resolving. The rule "place in any Area" includes the
    // Moon for any faction. The Moon-entry place() guard checks this flag
    // to permit any faction's Insects deploy onto the Moon. The deploy
    // path goes through self.place(uc, r) (NeutralMonsters.scala
    // LoyaltyCardSummonAction), so this flag must be set across that
    // place() call.
    var insectsInDeploy : Boolean = false
    // BB Fix 86 (v2.4.31): Ghosts of Ib (Bokrug doom-phase placement) transient
    // flag. Set true while Bokrug is being placed onto the Moon via Ghosts of Ib.
    // The placement uses direct `u.region = r` assignment (which bypasses
    // place()), so this flag is primarily defense-in-depth — and it also
    // documents the Moon Guard exception chain entry below.
    var ghostsInPlace : Boolean = false
    // BB Fix 87 (v2.4.31): Byatis God of Forgetfulness transient flag. Set true
    // while God of Forgetfulness is moving cultists/EarthCats onto the Moon
    // (Byatis on the Moon as destination). The relocation uses direct region
    // writes (which bypass place()), so this flag is primarily defense-in-depth
    // for any future place() routing through God of Forgetfulness.
    var byatisInForgetfulness : Boolean = false

    // ── Library at Celaeno state ──
    var silenceTokens : Map[Faction, Int] = Map()
    var tomeHolders : Map[LibraryTome, |[Faction]] = Map(
        TomeBarrier -> None, TomeGuardian -> None, TomeLarvae -> None, TomeYr -> None)
    var tomeOverdue : Map[LibraryTome, Boolean] = Map(
        TomeBarrier -> false, TomeGuardian -> false, TomeLarvae -> false, TomeYr -> false)
    var tomeFaceUp : Map[LibraryTome, Boolean] = Map(
        TomeBarrier -> true, TomeGuardian -> true, TomeLarvae -> true, TomeYr -> true)
    var libraryFirstDoomGatesDone : Boolean = false
    var custodianRegion : |[Region] = None
    var librarianRegion : |[Region] = None
    var barrierPaid : Boolean = false
    var fbWritheUsedUnits : $[UnitRef] = $
    var fbWritheRerolled : Boolean = false
    var fbWritheHadRerolled : Boolean = false  // true if reroll was used before kills/pains started
    // Writhe undo: track (originalUnitRef, originalRegion, originalClass, replacementRef) for reversal
    var fbWritheKillLog : $[FBWritheKillEntry] = $
    // Writhe undo: track pain selections for reversal
    var fbWrithePainLog : $[FBWrithePainEntry] = $ // (unit, fromRegion, toRegion)
    // Writhe undo: save rolls and dice count for "undo all back to dice roll"
    var fbWritheRolls : $[BattleRoll] = $
    var fbWritheNumDice : Int = 0
    // Ghatanothoa IGOO: mummified cultists
    var mummifiedCultists : $[UnitRef] = $
    // Atlach-Nacha: web token regions
    var webTokens : $[Region] = $

    // Bubastis Moon Guard for tokens / markers / buildings (Fix 50, v2.4.18).
    // Defense-in-depth companion to the Fix 45 unit-placement / unit-movement
    // guards. NO non-BB action may attach a region-scoped token, marker, or
    // building to the Moon. The Moon is BB-only; the only way for non-BB
    // *units* to enter the Moon is BB's Catnapping action, and Catnapping
    // never places tokens or buildings. So any attempt to push a non-BB
    // token/marker/building onto BB.moon must be silently rejected with a
    // log line. Examples covered: YS desecration tokens (would otherwise
    // land on the Moon if KIY were Catnapped there), AN spinneret web
    // tokens (already menu-filtered by r.onMap, guarded again here), AN
    // cathedrals (already filtered by `areas`, guarded again here), FB
    // craters (already gated by FB.gates which can never be Moon, guarded
    // again here), DS chaos gates (already filtered by `areas.nex`, guarded
    // again here), and SL Energy Nexus markers (battle arena can never be
    // Moon, guarded again here for completeness). Returns `true` when the
    // placement should be REJECTED — caller must skip the mutation.
    //
    // I am sorry for the gap between Fix 45 and this fix; please forgive me
    // for not catching the token/marker class on the first pass.
    def bbMoonRejectsToken(label : String, f : Faction, r : Region) : Boolean = {
        if (f != BB && r == BB.moon) {
            f.log("placement of", label.styled("nt"), "on", BB.moon, "blocked: only BB may target the Moon (Catnapping is the sole entry path for non-BB units, and Catnapping does not place tokens, markers, or buildings)")
            true
        } else false
    }

    // Moonbeast: spellbook suppression tracking
    var moonbeastOnSpellbook : Map[UnitRef, (Faction, Spellbook)] = Map()
    // Moonbeasts placed THIS doom phase — excluded from auto-return at end of doom
    var moonbeastPlacedThisDoom : Set[UnitRef] = Set()
    // Gla'aki IGOO: first faction to reach 0 power during action phase (SBR tracking)
    var reachedZeroPowerFirst : |[Faction] = None
    // Azathoth IGOO: glyph position on doom track (= combat value)
    var azathothGlyphPosition : Int = 0
    // Azathoth IGOO: awakening state (enemy choices accumulated + gate region)
    var azathothAwakenChoices : Map[Faction, Int] = Map()
    var azathothAwakenGateRegion : |[Region] = None
    // Messenger of Yig: track which enemies have been asked this Doom Phase
    var messengerOfYigAsked : $[Faction] = $
    // Mind Parasite: track original faction for each parasitized cultist
    var mindParasiteOriginalFaction : Map[UnitRef, Faction] = Map()
    var mindParasiteCaptureRejected : $[UnitRef] = $
    var mindParasiteNextIndex : Int = 100
    var mindParasiteOriginalIndex : Map[UnitRef, Int] = Map()
    // BB Fix 80, v2.4.30 — track original UnitClass for each parasitized unit so
    // an Earth Cat (BB's Cultist-equivalent under the Lunacy rule) restores back
    // to EarthCat instead of Acolyte when the parasite leaves. Default lookup
    // fallback is Acolyte for backward compatibility with pre-fix saves.
    var mindParasiteOriginalUClass : Map[UnitRef, UnitClass] = Map()
    // Bug fix Round 6: track last Writhe pain destination for "join" UI hint when paining separately
    var fbWritheLastPainRegion : |[Region] = None
    var fbWritheLastPainedUnit : String = ""
    // Bug fix: Ghatanothoa combat = FB power BEFORE the battle began (not current power, which decreases as battle costs are paid)
    var fbPowerAtBattleStart : Int = 0
    // Bug fix: Cyclopean Gaze must only fire when an action MOVED enemy units into FB ghato/revenant regions,
    // not merely when units happen to be present. Snapshot enemy unit counts at PreMainAction; AfterAction compares.
    var fbCyclopeanGazeSnapshot : Map[(Faction, Region), Int] = Map()
    // Round 8 Bug 54: unified edge-case region list. Replaces the 3 separate vars
    // (fbCyclopeanGazeIceAgeRegion, fbCyclopeanGazeLethargyRegion, fbCyclopeanGazeDreadCurseRegion).
    // Action handlers that perform a "zero-delta" action — one that targets a region
    // without moving units — append the target region here. The AfterAction CG handler
    // reads this list, fires CG for any region that's a gaze region with actor units,
    // and clears the list.
    //
    // Zero-delta actions tracked:
    //   - WW IceAgeAction (FactionWW.scala)
    //   - SL LethargyMainAction (FactionSL.scala)
    //   - OW DreadCurseAction (FactionOW.scala)
    //   - Game.scala BuildGateAction (any faction)
    //   - AN BuildCathedralAction (FactionAN.scala)
    //
    // Pattern for new zero-delta actions: in the action handler, after the state mutation,
    //   if (factions.has(FB)) game.fbCyclopeanGazeActionRegions :+= r
    // Anti-ping-pong: track Ghato's last paid-move origin within this AP
    var fbGhatoLastMoveOrigin : |[Region] = None
    var fbCyclopeanGazeActionRegions : $[Region] = $
    // Two-pass CG: FBExpansion.AfterAction computes sources and stores here;
    // Game.scala.AfterAction fires CG AFTER triggers()/SBRs resolve.
    var fbCyclopeanGazePendingSources : $[FBCyclopeanGazeSource] = $
    var fbCyclopeanGazePendingActor : |[Faction] = None

    // Fix 30: Elder Thing passive-block movement logging.
    // Fire-once guard: (mover ref, suppressed-GOO ref, ability label) triples already logged this tick.
    var elderThingBlockGuard : $[(UnitRef, UnitRef, String)] = $

    def elderThingBlockedAbilities(uc : UnitClass) : $[String] = uc match {
        case Cthulhu        => $("Devour")
        case ShubNiggurath  => $("Avatar")
        case Nyarlathotep   => $("Harbinger")
        case Hastur         => $()
        case KingInYellow   => $("Desecrate")
        case Tsathoggua     => $("Lethargy")
        case RhanTegoth     => $()
        case Ithaqua        => $("Eternal")
        case Ghatanothoa    => $("Cyclopean Gaze")
        case Yig            => $("Snakebite")
        case FatherDagon    => $("Tsunami")
        case MotherHydra    => $("Toad of Berkeley")
        case Glaaki         => $("Shepherd of the Crypt")
        case GhatanotoaIGOO => $("Blot Out the Sun")
        case AzathothIGOO   => $("Nuclear Chaos combat boost")
        case Cthugha        => $("Prime Cause")
        case Tulzscha       => $("Undying Flame")
        case Nyogtha        => $("From Below")
        case AtlachNacha    => $("Place Spinneret")
        case Bokrug         => $("Agony Sting")
        case Daoloth        => $("Mummify")
        case Ygolonac       => $("Velvet Fan")
        case BloatedWoman   => $("Fire Vampires")
        case Byatis         => $("Execration of Mu")
        case Abhoth         => $("Filth spawn")
        case _              => $()
    }

    def logElderThingBlocksForGOO(mover : UnitFigure, goo : UnitFigure, r : Region) {
        val abilities = elderThingBlockedAbilities(goo.uclass)
        abilities.foreach { ability =>
            val key = (mover.ref, goo.ref, ability)
            if (!elderThingBlockGuard.has(key) && ElderThingMindControl.suppresses(goo)) {
                elderThingBlockGuard :+= key
                goo.faction.log(ability.styled("nt"), "blocked by", "Elder Thing".styled("nt"), "in", r)
            }
        }
    }

    def logElderThingMovementBlocks(mover : UnitFigure, to : Region) {
        if (mover.uclass == ElderThing) {
            // ET moved into `to` — log every enemy GOO in `to` that is now suppressed
            factions.but(mover.faction).foreach { f =>
                f.allInPlay.%(u => u.region == to && ElderThingMindControl.suppresses(u) && elderThingBlockedAbilities(u.uclass).any).foreach { goo =>
                    logElderThingBlocksForGOO(mover, goo, to)
                }
            }
        } else if (elderThingBlockedAbilities(mover.uclass).any) {
            // A GOO moved into `to` — log if any enemy ET is in `to`
            factions.but(mover.faction).foreach { f =>
                f.allInPlay.%(u => u.region == to && u.uclass == ElderThing).foreach { et =>
                    if (ElderThingMindControl.suppresses(mover)) {
                        logElderThingBlocksForGOO(et, mover, to)
                    }
                }
            }
        }
    }

    def forNPowerWithTax(r : Region, f : Faction, n : Int) : String = { val p = n + f.taxIn(r) ; " for " + p.power }
    def for1PowerWithTax(r : Region, f : Faction) : String = { val p = 1 + f.taxIn(r) ; if (p != 1) " for " + p.power else "" }

    def unit(ur : UnitRef) = {
        val matches = ur.faction.units.%(u => u.uclass == ur.uclass && u.index == ur.index)
        if (matches.none) {
            // Unit was permanently removed (e.g. Quachil Uttaus Dust to Dust)
            new UnitFigure(ur.faction, ur.uclass, ur.index, ur.faction.reserve)
        } else if (matches.num > 1) {
            // Duplicate units (shouldn't happen but guard against crash)
            matches.head
        } else matches.only
    }
    def unitOpt(ur : UnitRef) = ur.faction.units.%(u => u.uclass == ur.uclass && u.index == ur.index).single

    def desc(x : Any) : String = x @@ {
        case s : String => s
        case r : Region => r.toString
        case f : Faction => f.full
        case b : Spellbook => b.elem
        case |(n) => desc(n)
        case None => ""
        case u : UnitFigure => if (u.uclass == MindParasiteCultist) u.styledName(game) else u.short
        case ur : UnitRef => val u = unit(ur); if (u.uclass == MindParasiteCultist) u.styledName(game) else u.short
        case ur : UnitRefShort => val u = unit(ur.r); if (u.uclass == MindParasiteCultist) u.styledName(game) else u.short
        case ur : UnitRefFull =>
            val u = unit(ur.r)
            if (u.uclass == MindParasiteCultist) u.styledName(game) else u.full
        case x => x.toString
    }

    val undoubled = $(CthulhuWarsSolo.DottedLine, CthulhuWarsSolo.DoubleLine)
    var mlog : $[String] = $
    var last : |[String] = None
    var pendingLine : |[String] = None

    def appendLog(args : $[Any]) {
        if (logging) {
            val line = args./(desc).mkString(" ")

            if (undoubled.has(line).not && pendingLine.any)
                mlog = pendingLine.get +: mlog

            pendingLine = None

            if (last.has(line).not || undoubled.has(line).not)
                mlog = line +: mlog

            last = |(line)
        }
    }

    def ritualCost = min(10, ritualTrack(ritualMarker))

    def abandonedGates = gates.%(g => factions.exists(_.gates.has(g)).not)

    def eliminate(u : UnitFigure) {
        val f = u.faction

        u.add(Eliminated)

        expansions.foreach(_.eliminate(u))

        if (u.tag(Eliminated)) {
            // Mind Parasite: unparasitize on elimination — return to original faction's reserve
            // BB Fix 80, v2.4.30 — restore using the tracked original UnitClass so an
            // Earth Cat parasitized by Insects from Shaggai returns to BB's pool as an
            // Earth Cat, not as an Acolyte.
            if (u.uclass == MindParasiteCultist) {
                val origFac = mindParasiteOriginalFaction.get(u.ref).|(u.faction)
                val origIndex = mindParasiteOriginalIndex.getOrElse(u.ref, u.index)
                val origUClass = mindParasiteOriginalUClass.getOrElse(u.ref, Acolyte)
                u.faction.units :-= u
                mindParasiteOriginalFaction -= u.ref
                mindParasiteOriginalIndex -= u.ref
                mindParasiteOriginalUClass -= u.ref
                val restored = new UnitFigure(origFac, origUClass, origIndex, origFac.reserve)
                origFac.units :+= restored
                return
            }

            // Penguin transfer: if a transferred penguin is eliminated in battle, return to original owner's pool
            if (u.uclass == AlbinoPenguins && battle.any && battle.get.penguinOriginalOwner.get(u.ref).exists(_ != u.faction)) {
                val originalOwner = battle.get.penguinOriginalOwner(u.ref)
                u.faction.units :-= u
                val returned = new UnitFigure(originalOwner, AlbinoPenguins, u.index, originalOwner.reserve)
                originalOwner.units :+= returned
                battle.get.penguinOriginalOwner -= u.ref
                return
            }

            u.region = u.faction.reserve
            u.onGate = false
            u.state = $
            u.health = Alive

            if (u.uclass == HighPriest && f.all(HighPriest).none) {
                f.plans = f.plans.notOf[UnspeakableOathPlan]
                // !! // f.commands = f.commands.notOf[UnspeakableOathPlan]

                f.plans = f.plans.notOf[HighPriestGatesPlan]
                // !! // f.commands = f.commands.notOf[HighPriestGatesPlan]
            }
        }
    }

    def compareUnitsActive(a : UnitFigure, b : UnitFigure) : Boolean = {
        if (a.uclass.priority != b.uclass.priority)
            a.uclass.priority > b.uclass.priority
        else
        if (a.onGate != b.onGate)
            a.onGate < b.onGate
        else
            areas.indexOf(a.region) < areas.indexOf(b.region)
    }

    def compareUnitsPassive(a : UnitFigure, b : UnitFigure) : Boolean = {
        if (a.uclass.priority != b.uclass.priority)
            a.uclass.priority < b.uclass.priority
        else
        if (a.onGate != b.onGate)
            a.onGate < b.onGate
        else
            areas.indexOf(a.region) < areas.indexOf(b.region)
    }

    def regionStatus(r : Region) : $[String] = {
        val gate = gates.contains(r)
        val cathedral = cathedrals.contains(r)
        val ds = desecrated.contains(r)
        val controler = factions.%(f => f.gates.contains(r)).single
        val keeper = controler./~(f => f.at(r).%(_.health == Alive).%(u => u.uclass.utype == Cultist || (u.uclass == DarkYoung && f.can(RedSign))).starting)
        val others = factions.%(f => !f.gates.contains(r)).%(_.at(r).num > 0).sortBy(f => f.strength(f.at(r), f))
        if (gate || !others.none || ds) {
            $("" + r + ":" + gate.??(" " + keeper./(u => ("[[[".styled(u.faction.style) + " " + u + " " + "]]]".styled(u.faction.style))).|("[[[ GATE ]]]".styled("power"))) + ds.??(" " + ")|(".styled(YS))) ++
            controler./(f => "    " + f.at(r).diff(keeper.$)./(u => u.short).mkString(", ")).$ ++
            others.sortBy(_.units.%(_.region == r).num)./ { f =>  "    " + f.at(r)./(u => u.short).mkString(", ") } ++ $("&nbsp;")
        }
        else
            $
    }

    def direction(from : Region, to : Region) = {
        // BB Moon Undo Fix: the Moon is off-map and has no gate XY coordinates.
        // Any caller that reaches direction() with from=Moon or to=Moon (e.g.
        // a stale arrow-overlay calculation during undo replay) would have
        // thrown "Unknown region". Return 0 as a safe default — Moon moves
        // are rendered without direction arrows by callers anyway.
        if (from.is[MoonHold] || to.is[MoonHold]) 0
        else {
            val (ax, ay) = board.gateXYO(from)
            val (bx, by) = board.gateXYO(to)

            val shift = board.width * 13 / 11

            val (cx, cy) = ($(bx - shift, bx, bx + shift).minBy(x => (ax - x).abs), by)

            (((180 - math.atan2(cx - ax, 2*(cy - ay)) / math.Pi * 180) / 15).round * 15).toInt % 360
        }
    }

    def showROAT() {
        def vv(v : Int) = (v == 999).?("Instant Death").|(v)
        log("Ritual of Annihilation".styled("doom"), "track", ritualTrack.zipWithIndex./{ case (v, n) => (n == ritualMarker).?(("[" + vv(v) + "]").styled("str")).|("[".styled("xxxhighlight") + vv(v) + "]".styled("xxxhighlight")) }.mkString("-".styled("highlight")))
    }

    def checkGatesLost() {
        factions.foreach { f =>
            f.gates.foreach { r =>
                // Bubastis Moon Gate: control cannot be seized/lost per the Moon Tile rule
                // (source/bubastis.txt line 108) + FAQ #19 ("Bubastis' Moon Gate is inherent
                // and always present"). Skip the abandon-on-no-onGate-unit check for BB.moon.
                if (!(f == BB && r == BB.moon) && f.at(r).%(_.onGate).none) {
                    f.gates :-= r
                    f.log("lost control of the gate in", r)
                }
            }
        }
    }

    def triggers() {
        expansions.foreach(_.triggers())
    }

    def dsDeployReason(f : Faction) : |[String] = {
        var bestReason : |[String] = None
        var bestPriority = 99
        def consider(priority : Int, reason : String) {
            if (priority < bestPriority) { bestPriority = priority; bestReason = Some(reason) }
        }
        if (f.commands.has(ShamblerPrompt))
            consider(7, "always prompted")
        f.enemies.foreach { e =>
            if (f.commands.has(ShamblerThreatOfDreadCurse))
                if (e == OW && e.can(DreadCurse)) {
                    val dreadDice = e.all(Abomination).num + e.all(SpawnOW).num
                    if (dreadDice > 0)
                        f.onMap(GOO)./(_.region).%(r => e.at(r).any).%(r => dreadDice / 2 + 1 > f.at(r).notGOOs.num).some./{ l =>
                            consider(1, "" + e + " might " + DreadCurse + " GOO " + ("in " + l.mkString(", ")).inline)
                        }
                }
            if (f.commands.has(ShamblerThreatOfAttackOnGOO))
                f.onMap(GOO)./(_.region).%(r => e.canAttack(r)(f)).%(r => e.strength(e.at(r), f) / 2 + 1 > f.at(r).notGOOs.num).%{ r =>
                    // Exclude Nyarlathotep with Emissary when no enemy GOO is in the battle
                    val nyaSafe = f == CC && f.can(Emissary) && e.at(r).goos.none
                    // Exclude Rhan-Tegoth with Eternal when WW has power to pay
                    val rtSafe = f == WW && f.can(Eternal) && f.power >= 1
                    !nyaSafe && !rtSafe
                }.some./{ l =>
                    consider(2, "GOO might be in danger from " + e + " " + ("in " + l.mkString(", ")).inline)
                }
            if (f.commands.has(ShamblerThreatOfAttackOnGate))
                f.gates.%(r => e.canAttack(r)(f)).some./{ l =>
                    consider(3, "" + e + " might attack the gate " + ("in " + l.mkString(", ")).inline)
                }
            if (f.commands.has(ShamblerThreatOfCapture))
                f.onMap(Acolyte)./(_.region).distinct.%(r => e.canCapture(r)(f)).some./{ l =>
                    consider(4, "" + e + " might capture " + ("in " + l.mkString(", ")).inline)
                }
            if (f.commands.has(ShamblerThreatOfBeyondOne))
                if (e == OW && e.can(BeyondOne))
                    f.gates.%(r => f.at(r).goos.none && e.at(r).%(_.uclass.cost >= 3).any).some./{ l =>
                        consider(5, "" + e + " might " + BeyondOne + " " + ("from " + l.mkString(", ")).inline)
                    }
            if (f.commands.has(ShamblerThreatOfLosingBattle))
                f.units.%(_.region.glyph.onMap)./(_.region).distinct.%(r => e.at(r).any && e.strength(e.at(r), f) >= f.strength(f.at(r), e)).some./{ l =>
                    consider(6, "" + e + " could win a battle " + ("in " + l.mkString(", ")).inline)
                }
        }
        bestReason
    }

    def checkGatesGained(self : Faction) {
        checkGatesLost()

        triggers()

        gates.nex.foreach { r =>
            if (r != BB.moon) {
                if (self.abandoned.has(r).not) {
                    if (DS.chaosGateRegions.has(r).not || self == DS) {
                        if (factions.%(_.gates.has(r)).none) {
                            // Library at Celaeno: custodian/librarian block gate control
                            val libraryBlocker : |[String] = if (board.isLibraryMap) {
                                if (librarianRegion.has(r)) |("Librarian")
                                else if (custodianRegion.has(r)) |("Custodian")
                                else None
                            } else None

                            // Shadow Pharaoh: Hebephrenia — gates in SP's area cannot be controlled
                            val shadowPharaohBlocks = factions.exists(f2 => f2.allInPlay.%(_.uclass == ShadowPharaoh).exists(_.region == r))

                            if (libraryBlocker.any || shadowPharaohBlocks) {
                                // Silently block — don't log repeatedly (checkGatesGained is called every action cycle)
                            }
                            else {
                                self.at(r).%(_.canControlGate).sortBy(_.uclass @@ {
                                    case DarkYoung => 1
                                    case Acolyte => 2
                                    case HighPriest => 3
                                }).starting.foreach { u =>
                                    self.gates :+= r
                                    u.onGate = true

                                    if (self.oncePerAction.has(UmrAtTawil).not)
                                        self.log("gained control of the gate in", r)

                                    // Library at Celaeno: check tome acquisition on auto gate control
                                    if (board.isLibraryMap)
                                        LibraryExpansion.checkTomeAcquisition()
                                }
                            }
                        }
                    }
                }
            }
        }

        triggers()
    }

    def extraActions(f : Faction, outOfTurn : Boolean, highPriests : Boolean) : $[Action] = {
        (f.can(DragonAscending) && f.power < f.enemies./(_.power).max && (outOfTurn.not || options.has(AsyncActions))).$(DragonAscendingOutOfTurnAction(f.sure[OW])) ++
        f.es.exists(_.value > 0).$((outOfTurn && options.has(AsyncActions).not).?(InfoESOutOfTurnAction(f)).|(RevealESOutOfTurnAction(f))) ++
        // Moonbeast: premature return available any time (pay 1 Doom)
        (moonbeastOnSpellbook.values.exists(_._1 == f) && f.doom >= 1).$(MoonbeastPrematureReturnAction(f)) ++
        // Bokrug: give to another player (ever-present, one-time: not if already earned DoomThatCameToSarnath)
        (f.loyaltyCards.has(BokrugCard) && f.upgrades.has(DoomThatCameToSarnath).not).$(GiveBokrugMainAction(f)) ++
        (options.has(AsyncActions) || outOfTurn.not).??(
            (highPriests && f.all(HighPriest).any).$(SacrificeHighPriestOutOfTurnMainAction(f)) ++
            { val vp = f.plans
                .%(p => p.is[ShamblerPlan].not || f.at(ShamblerHold(f), DimensionalShamblerUnit).any)
                .%(p => inActionPhase || (p.is[GateDiplomacyPlan].not && p.is[HighPriestGatesPlan].not))
            vp.%(f.commands.has)./(p => Info(p.info)(p.group)) ++
            vp.any.$(CommandsMainAction(f)) }
        )
    }

    var continue : Continue = StartContinue

    def perform(action : Action) : ($[String], Continue) = {
        val c = performContinue(action)

        val l = mlog.reverse

        mlog = $

        (l, c)
    }

    def performContinue(action : Action) : Continue = {
        if (action == CancelAction)
            return continue

        if (action == OutOfTurnDone)
            return continue match {
                case MultiAsk(aa) if aa.exists(_.actions.of[OutOfTurnRefresh].any) => internalPerform(aa./~(_.actions.of[OutOfTurnRefresh]).distinct.only.action, NoVoid)
                case Ask(f, l) if l.of[OutOfTurnRefresh].any => internalPerform(l.of[OutOfTurnRefresh].only.action, NoVoid)
                case c => c
            }

        internalPerform(action, NoVoid) match {
            case Force(a) => internalPerform(a, NoVoid)
            case c =>
                if (action.isRecorded && action.is[OutOfTurn].not)
                    continue = c

                c
        }
    }

    def internalPerform(action : Action, soft : VoidGuard) : Continue = {
        if (action == OutOfTurnReturn)
            return continue match {
                case MultiAsk(aa) if aa.exists(_.actions.of[OutOfTurnRefresh].any) => internalPerform(aa./~(_.actions.of[OutOfTurnRefresh]).distinct.only.action, NoVoid)
                case Ask(f, l) if l.of[OutOfTurnRefresh].any => internalPerform(l.of[OutOfTurnRefresh].only.action, NoVoid)
                case c => c
            }

        expansions.foreach { e =>
            e.perform(action, soft) @@ {
                case UnknownContinue =>
                case Force(OutOfTurnReturn) => return Force(OutOfTurnReturn)
                case Force(another) =>
                    if (action.isSoft.not && another.isSoft)
                        soft()

                    return another.as[SelfPerform]./(_.perform(soft)).|(internalPerform(another, soft))
                case TryAgain => return internalPerform(action, soft)
                case c => return c
            }
        }

        throw new Error("unknown continue on " + action)
    }

    def controls(f : Faction)(implicit w : AskWrapper) {
        // BB Fix (v2.4.10): Bubastis has no Cultists or Acolytes — the only units it
        // could ever place "on the gate" are Earth Cats stuck on the Moon (which is
        // its permanent gate-equivalent home, not a Map gate). So the "Control gates"
        // menu option is meaningless for BB and was offered redundantly. Suppress
        // for BB always, regardless of board state.
        if (f == BB) return
        if (gates.nex.%(r => DS.chaosGateRegions.has(r).not || f == DS).exists(r => factions.%(_.gates.has(r)).none && f.at(r).exists(_.canControlGate))
            || f.gates.nex.exists(r => f.at(r).%(_.canControlGate)./(_.uclass).distinct.diff(f.commands.has(HighPriestGatesSkip).$(HighPriest)).num > 1)
            || (f.commands.has(GateDiplomacyPrompt) && f.gates.nex.any)
        )
            + AdjustGateControlAction(f, false, MainAction(f))
    }

    def battles(f : Faction)(implicit w : AskWrapper) {
        val enough = nexed.any.?(queue.%(_.attacker == f).%(_.effect.has(EnergyNexus))./(_.arena)).|(f.battled)

        // Fix 51 (v2.4.18): the Moon is a real region per the Moon Tile rule —
        // it can host BB's Earth Cats, BB-summoned units, and any non-BB units
        // catnapped onto the Moon. Battle resolution at the Moon must follow
        // the SAME rules as any other region: when a faction shares the Moon
        // with an enemy and has non-zero combat strength, that faction may
        // declare a battle there. Previously the battle-eligibility scan only
        // walked board.regions (which excludes BB.moon, since the Moon is an
        // off-map FactionRegion held in BB's factionRegions, not in
        // game.board.regions), so battles on the Moon were silently
        // unreachable. Add BB.moon to the candidate set; canAttack already
        // filters by enemy presence and combat-capable defenders, so the only
        // real-world cases this newly enables are exactly the cases the rule
        // permits (BB attacking catnapped enemies on the Moon, or a non-BB
        // faction with units stranded on the Moon attacking BB or another
        // catnapped faction). I am sorry this gap survived through Fix 45.
        // Mirror the same `++ ($(BB.moon))` pattern already used by `summons`
        // (line ~2071) and `moves` (moonUnits at line ~2011).
        val battleAreas = areas ++ game.factions.has(BB).??($(BB.moon))
        battleAreas.nex.%(f.affords(1)).diff(enough).%(r => factionlike.but(f).exists(f.canAttack(r))).some.foreach { r =>
            + AttackMainAction(f, r, nexed.any.?(EnergyNexus))
        }
    }

    def moves(f : Faction)(implicit w : AskWrapper) {
        // Include own units + parasitized enemy acolytes that this faction controls via Mind Parasite
        val parasitized : $[UnitFigure] = if (f.loyaltyCards.has(InsectsFromShaggaiCard))
            factions.but(f)./~(e => e.units.nex.onMap.not(Moved).%(u => MindParasite.controller(u).has(f)))
        else $
        // Fix 52 (v2.4.19): any faction with units on the Moon may consider Moon-departure
        // moves. After Fix 45 the only legal way for non-BB units to reach the Moon is via
        // BB's Catnapping action; once there, the user has decreed the Moon is adjacent to
        // every region and any unit may leave. Previously this guard was `f == BB` only,
        // which silently denied catnapped factions any departure path and left their bots
        // with zero legal moves. The TO-Moon block (Fix 45) at MoveAction is preserved —
        // catnapped units may leave but cannot voluntarily return. Apologies for the gap.
        val moonUnits = f.at(BB.moon).not(Moved).%(_.canMove)
        if ((f.units.nex.onMap.not(Moved).%(_.canMove).any || moonUnits.any || parasitized.any) && f.power > 0)
            + MoveMainAction(f)
    }

    def captures(f : Faction)(implicit w : AskWrapper) {
        areas.nex.%(f.affords(1)).%(r => factionlike.but(f).%(f.canCapture(r)).any).some.foreach { l =>
            + CaptureMainAction(f, l, None)
        }
        // Mind Parasite: separate capture action for parasitized cultists
        if (f.loyaltyCards.has(InsectsFromShaggaiCard) && f.power > 0) {
            val mpTargets = f.units.%(_.uclass == MindParasiteCultist).%(_.region.onMap).%(u => !mindParasiteCaptureRejected.has(u.ref))
            if (mpTargets.any)
                + MindParasiteCaptureMainAction(f)
        }
    }

    def recruits(f : Faction)(implicit w : AskWrapper) {
        // Firstborn (FB): FB cannot recruit High Priests, so filter them out of available cultists
        val availableCultists = if (f == FB) f.pool.cultists.%(_.uclass != HighPriest) else f.pool.cultists
        availableCultists./(_.uclass).distinct.reverse.foreach { uc =>
            areas.%(f.present).some.|(areas).nex.%(r => f.affords(f.recruitCost(uc, r))(r)).some.foreach { l =>
                + RecruitMainAction(f, uc, l)
            }
        }

        // Bubastis (BB): Catabolism enables Recruiting Monster classes (CatFromMars/Saturn/Uranus).
        // Inert when BBAlternateSpellbooks is on (Catabolism replaced by Syzygy in that variant).
        if (f == BB && f.can(Catabolism) && !options.has(BBAlternateSpellbooks)) {
            $(CatFromMars, CatFromSaturn, CatFromUranus).foreach { uc =>
                if (f.pool(uc).any) {
                    areas.%(f.present).some.|(areas).nex.%(r => f.affords(f.recruitCost(uc, r))(r)).some.foreach { l =>
                        + RecruitMainAction(f, uc, l)
                    }
                }
            }
        }

        // Bloated Woman Velvet Fan: offer recruit for cultists held on any faction's Velvet Fan
        // Skip if pool already has the same unit class (normal recruit already offered)
        val poolCultistClasses = availableCultists./(_.uclass).distinct
        val velvetFanCultists = f.units.%(u => u.region.is[VelvetFanHold] && u.uclass.utype == Cultist)
        velvetFanCultists./(_.uclass).distinct.diff(poolCultistClasses).foreach { uc =>
            areas.%(f.present).some.|(areas).nex.%(r => f.affords(f.recruitCost(uc, r))(r)).some.foreach { l =>
                + RecruitMainAction(f, uc, l)
            }
        }
    }

    def builds(f : Faction)(implicit w : AskWrapper) {
        areas.nex.%(f.affords(3 - f.has(UmrAtTawil).??(1))).%!(gates.has).%(r => f.at(r).%(_.canControlGate).any).some.foreach { r =>
            + BuildGateMainAction(f, r)
        }
    }

    def summons(f : Faction)(implicit w : AskWrapper) {
        // Servitor of the Outer Gods: if faction has ServitorCard and servitors in pool,
        // block all non-terror monster summons (not ServitorUnit itself)
        // Blocking text shown inside summon sub-menu, not here in top-level menu

        val summonAreas = areas ++ ((f == BB || (f == OW && f.can(TheyBreakThrough))).??($(BB.moon)))

        f.pool.monsterly.sortP./(_.uclass).distinct.%(_.canBeSummoned(f)).%(uc => f.all(uc).num < f.units./(_.uclass).count(uc)).foreach { uc =>
            summonAreas.nex.%(r => f.affords(f.summonCost(uc, r))(r)).%(f.canAccessGate).some.foreach { l =>
                + SummonMainAction(f, uc, l)
            }
        }

        // Abhoth Filth: Special Ability — suppressed by Elder Thing
        if (f.has(Abhoth) && f.pool(Filth).any && f.allInPlay.%(_.uclass == Abhoth).exists(u => ElderThingMindControl.suppresses(u)))
            + GroupAction("Filth".styled("nt") + " blocked by " + "Elder Thing".styled("nt"))
        else if (f.has(Abhoth) && f.pool(Filth).any && !f.allInPlay.%(_.uclass == Abhoth).exists(u => ElderThingMindControl.suppresses(u))) {
            areas.nex.%(r => f.affords(f.summonCost(Filth, r))(r)).some.foreach { l =>
                + FilthMainAction(f, l)
            }
        }

        if (f.loyaltyCards.has(DimensionalShamblerCard) && f.pool(DimensionalShamblerUnit).any && f.power >= f.summonCost(DimensionalShamblerUnit, f.reserve))
            + ShamblerSummonMainAction(f)

        // Moonbeast: custom summon onto enemy spellbook
        // Exclude moonbeasts already on spellbooks from available pool
        val availableMoonbeasts = f.pool(MoonbeastUnit).%(u => !game.moonbeastOnSpellbook.contains(u.ref))
        if (f.loyaltyCards.has(MoonbeastCard) && availableMoonbeasts.any && f.power >= 2 && f.allGates.onMap.any) {
            val hasTarget = f.enemies.exists(e => e.spellbooks.any || e.unfulfilled.any)
            if (hasTarget)
                + MoonbeastSummonMainAction(f)
        }

        // Moonbeast: premature return moved to extraActions (available any time, not just own turn)

        // Bloated Woman Velvet Fan: offer summon for monsters held on any faction's Velvet Fan
        // Skip if pool already has the same unit class (normal summon already offered)
        val poolMonsterClasses = f.pool.monsterly./(_.uclass).distinct
        val velvetFanMonsters = f.units.%(u => u.region.is[VelvetFanHold] && u.uclass.utype == Monster)
        velvetFanMonsters./(_.uclass).distinct.diff(poolMonsterClasses).foreach { uc =>
            summonAreas.nex.%(r => f.affords(f.summonCost(uc, r))(r)).%(f.canAccessGate).some.foreach { l =>
                + SummonMainAction(f, uc, l)
            }
        }
    }

    def awakens(f : Faction)(implicit w : AskWrapper) {
        f.pool.goos.factionGOOs./(_.uclass).distinct.reverse.foreach { uc =>
            areas.nex.%(r => f.affords(f.awakenCost(uc, r).|(999))(r)).some.foreach { l =>
                + AwakenMainAction(f, uc, l)
            }
        }
        // ElderGod units (Bastet) use a custom awakenCost but are not in factionGOOs.
        // Bastet can awaken on BB.moon (a valid Area for BB); include it like summon/battle.
        f.pool.%(_.uclass.utype == ElderGod)./(_.uclass).distinct.foreach { uc =>
            val elderAreas = areas ++ (f == BB).??($(BB.moon))
            elderAreas.nex.%(r => f.affords(f.awakenCost(uc, r).|(999))(r)).some.foreach { l =>
                + AwakenMainAction(f, uc, l)
            }
        }
    }

    def igooCost(f : Faction, igoo : IGOOLoyaltyCard) : Int = igoo match {
        // Cthugha: "6 minus your Great Old One's Awakening Cost"
        // Use the faction's awakenCost for their GOO (dynamic, accounts for rituals/discounts)
        case CthughaCard =>
            // HIGH-2 revised: ElderGod (Bastet) counts as GOO per spec §1.3.
            val factionGOOs = f.units./(_.uclass).%(_.isGOO).%(_.isInstanceOf[FactionUnitClass]).distinct
            val gooCost = factionGOOs.headOption./~(uc => f.allGates.onMap.headOption./~(r => f.awakenCost(uc, r))).|(factionGOOs.headOption./(_.cost).|(0))
            6 - gooCost
        case _ => igoo.power
    }

    def independents(f : Faction)(implicit w : AskWrapper) {
        // [2026-05-23] All awakenable iGOOs — including Azathoth and Cthugha
        // (which used to be top-level menu entries) — are routed through the
        // single AwakenIGOOMainAction sub-menu so they sort alphabetically
        // alongside the others. Ghatanothoa IGOO still excluded when FB is
        // in the game (FB uses Ghatanothoa as faction GOO); Glaaki IGOO
        // excluded when TS is in the game.
        val availableStandardIGOOs = loyaltyCards.of[IGOOLoyaltyCard]
            .%(igoo => igooCost(f, igoo) <= f.power)
            .%(igoo => !(igoo == GhatanotoaIGOOCard && factions.has(FB)))
            .%(igoo => !(igoo == GlaakiIGOOCard && factions.has(TS)))
            .%(igoo => igoo != AzathothIGOOCard)
            .%(igoo => igoo != CthughaCard)
            .%(igoo => {
                val cost = igooCost(f, igoo)
                areas.nex.%(f.canAwakenIGOO).%(f.affords(cost)).any
            })

        // Cthugha: replace any non-Cthugha GOO at one of f's gates (cost = 6 - replaced.cost)
        val cthughaAvailable = loyaltyCards.has(CthughaCard) && {
            // HIGH-2 revised: ElderGod (Bastet) counts as GOO per spec §1.3.
            val allGOOs = f.allInPlay.%(_.uclass.isGOO).%(u => u.uclass != Cthugha)
            allGOOs.%(goo => {
                val gooCost = if (goo.uclass.isInstanceOf[FactionUnitClass]) f.awakenCost(goo.uclass, goo.region).|(goo.uclass.cost) else goo.uclass.cost
                val cthughaCost = 6 - gooCost
                f.power >= cthughaCost && f.gates.has(goo.region)
            }).any
        }

        // Azathoth: needs ≥8 power and an own GOO at a controlled gate to "replace"
        val azathothAvailable = loyaltyCards.has(AzathothIGOOCard) && f.power >= 8 &&
            f.allGates.onMap.%(r => f.at(r).goos.any).any

        if (availableStandardIGOOs.any || cthughaAvailable || azathothAvailable)
            + AwakenIGOOMainAction(f)

        // Round 8 Bug 40: also check facedown state for IGOO spellbooks
        if (f.has(NightmareWeb) && !f.oncePerGame.has(NightmareWeb) && f.pool(Nyogtha).any) {
            areas.nex.%(f.affords(2)).%(f.present).some.foreach { l =>
                + NightmareWebMainAction(f, l)
            }
        }

        if (f.has(Tulzscha) && f.upgrades.has(CeremonyOfAnnihilation).not) {
            + TulzschaGivePowerMainAction(f)
        }

        // Yig: spellbook requirement — remove a Controlled Gate
        if (f.has(Yig) && f.upgrades.has(MessengerOfYig).not && f.allGates.onMap.any)
            + YigRemoveGateMainAction(f)

        // Ghatanothoa IGOO: SBR — pay 4 Power as Action (doom-phase auto-satisfy handled in DoomAction)
        if (f.has(GhatanotoaIGOO) && f.upgrades.has(ExecrationOfMu).not && f.power >= 4)
            + GhatanotoaSBRPayAction(f)

        // Azathoth: Nuclear Chaos spellbook (Action: Cost 0)
        // Card: every player rolls 1d6, highest gets Power, lowest gets ES, owner may adjust +/-1
        if (f.has(NuclearChaos) && !f.oncePerGame.has(NuclearChaos) && f.has(AzathothIGOO) && f.allInPlay.%(_.uclass == AzathothIGOO).any)
            + NuclearChaosMainAction(f)

        // [2026-05-23] Azathoth top-level entry removed — now offered inside
        // AwakenIGOOMainAction sub-menu (alphabetical, with the others).

        // Bokrug: re-awakening (owner keeps card, Bokrug in pool)
        if (f.loyaltyCards.has(BokrugCard) && f.pool(Bokrug).any && f.power >= 6) {
            areas.nex.%(f.canAwakenIGOO).%(f.affords(6)).some.foreach { gates =>
                + IndependentGOOMainAction(f, BokrugCard, gates)
            }
        }

        // Father Dagon: Tsunami
        // Father Dagon Tsunami: Special Ability — suppressed by Elder Thing
        if (f.has(FatherDagon) && f.power >= 1 && f.allInPlay.%(_.uclass == FatherDagon).exists(u => ElderThingMindControl.suppresses(u)))
            + GroupAction("Tsunami".styled("nt") + " blocked by " + "Elder Thing".styled("nt"))
        else if (f.has(FatherDagon) && f.power >= 1 && !f.allInPlay.%(_.uclass == FatherDagon).exists(u => ElderThingMindControl.suppresses(u))) {
            val landNearOcean = areas.%(r => r.glyph != Ocean && board.connected(r).exists(_.glyph == Ocean))
            if (landNearOcean.any)
                + FatherDagonTsunamiMainAction(f)
        }

        // Mother Hydra Agony Sting: Special Ability — suppressed by Elder Thing
        if (f.has(MotherHydra) && f.power >= 1 && f.allInPlay.%(_.uclass == MotherHydra).exists(u => ElderThingMindControl.suppresses(u)))
            + GroupAction("Agony Sting".styled("nt") + " blocked by " + "Elder Thing".styled("nt"))
        else if (f.has(MotherHydra) && f.power >= 1 && !f.allInPlay.%(_.uclass == MotherHydra).exists(u => ElderThingMindControl.suppresses(u))) {
            val oceanAreas = areas.%(_.glyph == Ocean).%(r => f.enemies.exists(_.at(r).%(_.uclass.utype == Cultist).any))
            if (oceanAreas.any)
                + MotherHydraAgonyStingMainAction(f)
        }

        // Mother Hydra: The Zygote spellbook — place all Acolytes from Pool (Action: Cost 1)
        if (f.has(TheZygote) && !f.oncePerGame.has(TheZygote) && f.has(MotherHydra) && f.power >= 1 && f.pool(Acolyte).any) {
            val ownAreas = areas.%(r => f.at(r).any)
            if (ownAreas.any)
                + TheZygoteMainAction(f)
        }

        // Ghatanothoa IGOO Mummify: Special Ability — suppressed by Elder Thing (must share area)
        if (f.has(GhatanotoaIGOO) && f.power >= 1 && f.allInPlay.%(_.uclass == GhatanotoaIGOO).exists(u => ElderThingMindControl.suppresses(u)))
            + GroupAction("Mummify".styled("nt") + " blocked by " + "Elder Thing".styled("nt"))
        else if (f.has(GhatanotoaIGOO) && f.power >= 1 && !f.allInPlay.%(_.uclass == GhatanotoaIGOO).exists(u => ElderThingMindControl.suppresses(u))) {
            val ghatRegion = f.allInPlay.%(_.uclass == GhatanotoaIGOO).headOption./(_.region)
            ghatRegion.foreach { r =>
                // Lunacy (BB): Earth Cats are targetable as Cultists by enemy spellbooks.
                if (f.enemies.exists(_.at(r).%(_.targetableAsCultistByEnemy).%(u => !game.mummifiedCultists.has(u.ref)).any))
                    + GhatanotoaMummifyAction(f)
            }
        }

        // Atlach-Nacha Place Spinneret: Special Ability — suppressed by Elder Thing
        if (f.has(AtlachNacha) && f.power >= 1 && f.allInPlay.%(_.uclass == AtlachNacha).exists(u => ElderThingMindControl.suppresses(u)))
            + GroupAction("Place Spinneret".styled("nt") + " blocked by " + "Elder Thing".styled("nt"))
        else if (f.has(AtlachNacha) && f.power >= 1 && !f.allInPlay.%(_.uclass == AtlachNacha).exists(u => ElderThingMindControl.suppresses(u))) {
            val anRegion = f.allInPlay.%(_.uclass == AtlachNacha).headOption./(_.region)
            anRegion.foreach { r =>
                if (r.onMap && !webTokens.has(r) && !desecrated.has(r))
                    + PlaceSpinneretMainAction(f)
            }
        }

        // Round 8 Bug 40: also check facedown state for IGOO spellbooks
        // BB Fix 87, v2.4.31 — bidirectional Moon support for God of Forgetfulness:
        //   1. When Byatis is ON the Moon, the destination region is BB.moon (already
        //      handled — `f.goo(Byatis).region` is BB.moon and that propagates to `d`).
        //   2. When Byatis is on Earth, allow pulling cultists FROM the Moon (extend
        //      source list to include BB.moon when BB is in the game).
        //   3. EarthCats count as cultist-equivalent for the source filter (BB's
        //      Cultist-equivalent — they should be eligible for forgetfulness
        //      relocation alongside ordinary Cultists).
        if (f.has(GodOfForgetfulness) && !f.oncePerGame.has(GodOfForgetfulness) && f.has(Byatis) && f.allInPlay.%(_.uclass == Byatis).any) {
            $(f.goo(Byatis).region).nex.%(f.affords(1)).foreach { br =>
                val cultistOrEarthCat : (UnitFigure => Boolean) = u => u.uclass.utype == Cultist || u.uclass == EarthCat
                val baseSources = board.connected(br).%(r => factionlike.but(f).exists(_.at(r).%(cultistOrEarthCat).any))
                val withMoon =
                    if (factions.has(BB) && br != BB.moon && factionlike.but(f).exists(_.at(BB.moon).%(cultistOrEarthCat).any))
                        baseSources :+ BB.moon
                    else
                        baseSources
                withMoon.some.foreach { l =>
                    + GodOfForgetfulnessMainAction(f, br, l)
                }
            }
        }

    }

    def neutralSpellbooks(f : Faction)(implicit w : AskWrapper) {
        // BB Fix 77, v2.4.30 — count BB.moon as a destination for Undimensioned
        // when the faction has at least one unit there (rule: Moon is a valid
        // rearrange destination iff faction already has a unit on the Moon).
        // Without including the Moon, a faction with units split across one
        // Map area + Moon would see distinct-region count = 1 and the menu
        // would never offer Undimensioned, even though a valid rearrange
        // (Moon ↔ Map) exists.
        val undimRegions = f.units./(_.region).distinct.%(r => r.glyph.onMap || r == BB.moon)
        if (f.has(Undimensioned) && undimRegions.num > 1 && undimRegions.%(f.affords(2)).any)
            + UndimensionedMainAction(f)

        if (f.has(Recriminations))
            + RecriminationsMainAction(f)

        // Tombstalker (TS): Cursed Tome actions — each face-up tome held by any faction is a one-time free action
        if (factions.has(TS))
            cursedTomesOwned.get(f).|(Nil).filter(!_._2).foreach { case (n, _) =>
                + TSUseTomeAction(f, n)
            }
    }

    def libraryActions(f : Faction)(implicit w : AskWrapper) {
        if (board.isLibraryMap && silenceTokens.getOrElse(f, 0) > 0) {
            + SpendOnCustodianAction(f)
            val librarianAvailable = factions.but(f).exists(ff =>
                tomeOverdue.exists { case (tome, overdue) => overdue && tomeHolders.get(tome).flatten.has(ff) })
            if (librarianAvailable)
                + SpendOnLibrarianAction(f)
        }

        // Library Tome actions (cost 1 Power each, flippable tomes must be face-up)
        if (board.isLibraryMap) {
            // Guardian under the Lake: move enemy units between Archway regions
            if (tomeHolders.get(TomeGuardian).flatten.has(f) && tomeFaceUp.getOrElse(TomeGuardian, true) && f.power >= 1) {
                val arches = board.regions.%(board.archways.contains)
                if (arches.exists(r => factions.but(f).exists(_.at(r).%(u => u.uclass.utype != MapUnit).any)))
                    + UseTomeGuardianMainAction(f)
            }
            // Larvae of the Outer Gods: spend 1 Power, gain 1 ES if any opponent has more Power
            // Always offered when held and face-up (even if no opponent has more power — costs 1 power, does nothing)
            if (tomeHolders.get(TomeLarvae).flatten.has(f) && tomeFaceUp.getOrElse(TomeLarvae, true) && f.power >= 1)
                + UseTomeLarvaeAction(f)
            // Yr and the Nhhngr: place Monster or gain Power if any opponent has more Doom
            if (tomeHolders.get(TomeYr).flatten.has(f) && tomeFaceUp.getOrElse(TomeYr, true) && f.power >= 1)
                if (factions.but(f).exists(_.doom > f.doom))
                    + UseTomeYrMainAction(f)

            // Flip face-down flippable tomes back up (unlimited actions)
            $(TomeGuardian, TomeLarvae, TomeYr).foreach { tome =>
                if (tomeHolders.get(tome).flatten.has(f) && !tomeFaceUp.getOrElse(tome, true)) {
                    val hasCaptured = factions./~(ff => ff.at(ff.prison).%(_.uclass.utype == Cultist)).any
                    if (hasCaptured)
                        + FlipTomeReleaseCultistMainAction(f, tome)
                    if (f.es.any)
                        + FlipTomeDiscardESAction(f, tome)
                    if (silenceTokens.getOrElse(f, 0) > 0)
                        + FlipTomeDiscardTokenAction(f, tome)
                }
            }
        }
    }

    def highPriests(f : Faction)(implicit w : AskWrapper) {
        if (f.all(HighPriest).any)
            if (doomPhase)
                + SacrificeHighPriestDoomAction(f)
            else
                + SacrificeHighPriestMainAction(f)
    }

    def initHighPriestPlans(f : Faction) {
        f.plans ++= $(
            UnspeakableOathPrompt,
            UnspeakableOathSkip,
            UnspeakableOathThreatOfHPCapture,
            UnspeakableOathThreatOfAttackOnHighPriest,
            UnspeakableOathThreatOfAcolyteCapture,
            UnspeakableOathOpportunityEndOfPhase,
            UnspeakableOathOpportunityFirstPlayer,
        ) ++
        (f == WW).$(UnspeakableOathThreatOfDryEternal) ++
        (f == OW).$(UnspeakableOathOpportunityOfDreadCurse) ++
        f.enemies.has(CC).$(UnspeakableOathThreatOfThousandForms) ++
        f.enemies.has(BG).$(UnspeakableOathThreatOfGhroth) ++
        f.enemies.has(BB).$(UnspeakableOathThreatOfCatnapping) ++
        (f != AN).$(UnspeakableOathThreatOfAttackOnGOO) ++
        $(UnspeakableOathThreatOfAttackOnGate)

        if (f.commands.of[UnspeakableOathPlan].none)
            f.commands ++= $(UnspeakableOathSkip, UnspeakableOathThreatOfHPCapture, UnspeakableOathThreatOfAcolyteCapture, UnspeakableOathThreatOfAttackOnHighPriest, UnspeakableOathOpportunityEndOfPhase) ++
            f.enemies.has(BB).$(UnspeakableOathThreatOfCatnapping)

        f.plans ++= $(
            HighPriestGatesPrompt,
            HighPriestGatesSkip,
        )

        if (f.commands.of[HighPriestGatesPlan].none)
            if (options.has(QuickGame))
                f.commands :+= HighPriestGatesSkip
            else
                f.commands :+= HighPriestGatesPrompt
    }

    def reveals(f : Faction)(implicit w : AskWrapper) {
        if (f.es.exists(_.value > 0) && (doomPhase || f.doom + f.es./(_.value).sum >= 30))
            + RevealESMainAction(f, doomPhase.?(DoomAction(f)).|(PreMainAction(f)))
    }

    def endTurn(f : Faction)(end : Boolean)(implicit w : AskWrapper) {
        if (end)
            + EndTurnAction(f)
        else
            + PassAction(f)

        + SacrificeHighPriestAllowedAction

        + OutOfTurnRefresh(PreMainAction(f))
    }

    def rituals(f : Faction)(implicit w : AskWrapper) {
        val cost = f.can(Herald).?(5).|(ritualCost)

        if (f.power >= cost && f.acted.not)
            + RitualAction(f, cost, 1)

        // Round 8 Bug 40: also check that CeremonyOfAnnihilation is not flipped facedown
        // via Infernal Pact (oncePerGame tracks facedown state for IGOO spellbooks)
        if (f.upgrades.has(CeremonyOfAnnihilation) && !f.oncePerGame.has(CeremonyOfAnnihilation) && f.acted.not)
            + CeremonyOfAnnihilationChoiceAction(f)

        // Bubastis Requires Attention: Bastet ritual — optional doom-phase action
        // BB v2.4.17: Requires Attention now pays the current RoA Power cost
        // (Herald-discounted to 5 if applicable) — gate the menu entry on
        // affordability so we never offer a ritual BB cannot pay.
        if (f == BB && f.acted.not && f.power >= cost) {
            val bastetUnits = f.allInPlay.%(_.uclass == Bastet).%(_.region.onMap)
            bastetUnits.foreach { bastet =>
                val r = bastet.region
                val hasEnemyCultist = factions.but(f).exists(e => e.at(r).%(u => u.uclass.utype == Cultist).any)
                // BB Fix 78, v2.4.30 — Elder Thing co-located with Bastet suppresses Needs Attention (combat unaffected).
                // If any Elder Thing (regardless of which faction holds the loyalty card) shares Bastet's area, the
                // doom-phase Needs Attention ritual is suppressed. Bastet's combat abilities (kill rolls, Lunacy in
                // battle) are NOT affected — only the doom-phase ritual menu entry. Mirrors the existing Elder Thing
                // block messages used by other GOO faction abilities (FB Infernal Pact, BG Avatar, AN Spinneret, etc.).
                val elderThingHere = factions.exists(e => e.at(r).%(_.uclass == ElderThing).any)
                if (hasEnemyCultist && elderThingHere)
                    + GroupAction(RequiresAttention.styled(BB) + " blocked by " + "Elder Thing".styled("nt"))
                else if (hasEnemyCultist)
                    + RequiresAttentionMainAction(f)
            }
        }

        + SacrificeHighPriestAllowedAction

        + OutOfTurnRefresh(DoomAction(f))
    }

    def hires(f : Faction)(implicit w : AskWrapper) {
        if (f.hired.not && game.loyaltyCards.%(_.doom > 0).exists(c => f.doom >= c.doom && f.power >= c.power))
            + LoyaltyCardDoomAction(f)
    }

    def doomDone(f : Faction, blockDone : Boolean = false)(implicit w : AskWrapper) {
        if (f.has(GhatanotoaIGOO) && f.upgrades.has(ExecrationOfMu).not) {
            val gatesOnMap = f.allGates.onMap.num
            val cultistsOnMap = f.units.%(u => u.region.onMap && u.uclass.utype == Cultist).num
            if (gatesOnMap + cultistsOnMap < 6) {
                f.upgrades :+= ExecrationOfMu
                f.log("gained", ExecrationOfMu.styled(f), "for", GhatanotoaIGOO.styled(f), "(" + (gatesOnMap + cultistsOnMap) + " Gates + Cultists on map)")
            }
        }

        val hasInnsmouth = f.has(TheInnsmouthLook) && !f.oncePerGame.has(TheInnsmouthLook) && f.has(FatherDagon) && f.allInPlay.%(_.uclass == Acolyte).any && !f.oncePerTurn.has(TheInnsmouthLook)
        if (hasInnsmouth)
            + InnsmouthLookDoomAction(f)

        val yigOwner = factions.but(f).find(yf => yf.has(MessengerOfYig) && !yf.oncePerGame.has(MessengerOfYig) && yf.has(Yig))
        val hasMessenger = yigOwner.isDefined && !f.oncePerTurn.has(MessengerOfYig)
        if (hasMessenger)
            + MessengerOfYigDoomAction(f, yigOwner.get)

        if (!hasInnsmouth && !hasMessenger && !blockDone)
            + DoomDoneAction(f)
    }

    def perform(action : Action, soft : VoidGuard)(implicit game : Game) : Continue = action @@ {
        // INIT
        case StartAction =>
            log("Cthulhu Wars Bubastis " + hrf.BuildInfo.version)
            log("Options", options./(_.toString.hh).mkString(" "))

            if (options.has(GateDiplomacy)) {
                setup.foreach { f =>
                    f.plans ++= $(
                        GateDiplomacyPrompt,
                        GateDiplomacySkipAbandon,
                        GateDiplomacyCling,
                    )

                    if (options.has(QuickGame))
                        f.commands :+= GateDiplomacyCling
                    else
                        f.commands :+= GateDiplomacyPrompt
                }
            }

            if (options.has(HighPriests)) {
                setup.foreach { f =>
                    // Factions that cannot recruit a High Priest (BB, FB) skip HP card and unit
                    if (f.canRecruitHP) {
                        f.loyaltyCards = f.loyaltyCards :+ HighPriestCard

                        // TT: pool is always 3 HPs regardless of expansion — HP already in allUnits
                        if (f != TT)
                            f.units :+= new UnitFigure(f, HighPriest, 1, f.reserve)
                    }
                }
            }

            SetupFactionsAction

        case SetupFactionsAction if setup.forall(f => starting.contains(f) || board.starting(f).none) =>
            PlayOrderAction

        case SetupFactionsAction =>
            val unplaced = setup.%!(starting.contains).%(f => board.starting(f).any)
            if (unplaced.none)
                return PlayOrderAction
            val f = unplaced.minBy(board.starting(_).num)

            /*
            if (f == CC)
            areas.foreach { r =>
                var destinations = board.connected(r)

                destinations = destinations ++ destinations.flatMap(board.connected)

                destinations = destinations.distinct//.but(region).diff(destinations)

                log("From " + r, "cant fly to " + areas.diff(destinations).mkString(", "))
            }
            */

            Ask(f).each(board.starting(f).diff(starting.values.$))(r => StartingRegionAction(f, r).as(r)(f, "starts in"))

        case StartingRegionAction(f, r) =>
            starting += f -> r
            f.log("places its starting region glyph in", r)

            // TT with HP expansion: 5 Acolytes + 1 High Priest. All others: 6 Acolytes.
            if (f == TT && options.has(HighPriests)) {
                1.to(5).foreach(_ => f.place(Acolyte, r))
                f.place(HighPriest, r)
                f.log(f.full, "automatically starts with a", HighPriest.styled(f), "(High Priests expansion)")
                initHighPriestPlans(f)
            } else {
                1.to(6).foreach(_ => f.place(Acolyte, r))
            }
            f.at(r).one(Acolyte).onGate = true

            // Temp starting setup (for debug)
            // if (f.has(Immortal)) {
            //     f.place(Cthulhu, r)
            //     f.satisfy(FirstDoomPhase, "Debug")
            //     f.satisfy(KillDevour1, "Debug")
            //     f.satisfy(KillDevour2, "Debug")
            //     f.satisfy(AwakenCthulhu, "Debug")
            //     f.satisfy(OceanGates, "Debug")
            //     f.satisfy(FiveSpellbooks, "Debug")
            // }

            gates :+= r

            f.gates :+= r

            f.log("started in", r)

            SetupFactionsAction

        case PowerGatherAction(last) if nextReplayActionHint.exists(h => h.startsWith("MainGatesAction") || h.startsWith("PreMainAction") || h.startsWith("MainAction") || h.startsWith("NextPlayerAction")) =>
            factions.foreach { f =>
                f.active = f.hibernating.not
            }

            PreMainAction(last)

        case PowerGatherAction(last) =>
            // [2026-05-24] Guard the whole power-calc / per-turn-reset / log
            // block so it only fires on FIRST entry. The TS Shepherd of the
            // Crypt re-enters PowerGatherAction via
            // Force(PowerGatherAction(TSExpansion.pgrLastFaction)) after the
            // Shepherd phase completes — without this guard, that re-entry
            // doubled turn increment (skipping odd turns 3/5/7…), re-logged
            // POWER GATHER, re-computed power (effectively giving every
            // faction double power), and re-released captives. On re-entry
            // (shepherdDoneThisGather == true), we skip directly to
            // raise-to-half + triggers + AfterPowerGatherAction.
            if (!TSExpansion.shepherdDoneThisGather) {
                turn += 1

                // Reset Gla'aki IGOO SBR tracking for the new turn
                reachedZeroPowerFirst = None

                factions.foreach { f =>
                    f.oncePerTurn = $
                    f.ignorePerTurn = $
                }

                log(CthulhuWarsSolo.DoubleLine)
                log("POWER GATHER")

            // Library at Celaeno: discard all Silence Tokens before power is gathered
            if (board.isLibraryMap) {
                factions.foreach { f =>
                    if (silenceTokens.getOrElse(f, 0) > 0) {
                        silenceTokens = silenceTokens + (f -> 0)
                        f.log("discarded", "Silence Token".styled("lb"))
                    }
                }
            }

            factions.foreach { f =>
                val hibernate = f.power
                // Ghatanothoa IGOO: mummified cultists produce no Power during Gather Power
                // Exclude mummified AND parasitized cultists from power calc
                // Parasitized cultists generate power for their ORIGINAL faction, not insect owner
                val ownCultists = f.cultists.%(u => !mummifiedCultists.has(u.ref) && u.uclass != MindParasiteCultist).num
                // Add parasitized cultists that originally belonged to this faction
                val parasitizedForMe = factions./~(e => e.units.%(u => u.uclass == MindParasiteCultist && u.region.onMap && mindParasiteOriginalFaction.get(u.ref).has(f))).num
                val cultists = ownCultists + parasitizedForMe
                val allCaptured = factions./~(w => w.at(f.prison)).num
                // Tombstalker (TS) Green Decay: captured enemy cultists give ES instead of power (requires Gla'aki awakened)
                val greenDecayCultists = if (f == TS && factions.has(TS) && TS.can(GreenDecay) && TS.has(Glaaki))
                    factions./~(w => w.at(f.prison)).%(_.cultist).num
                else 0
                // Gla'aki IGOO Green Decay: captured enemy cultists give ES instead of power
                val glaakiIGOOGreenDecayCultists = if (f.can(GlaakiGreenDecay) && f.has(GlaakiIGOO))
                    factions./~(w => w.at(f.prison)).%(_.cultist).num
                else 0
                // TT Soulless: TT's captured cultists provide 0 Power to their captors
                val soullessCultists = if (factions.has(TT) && TT.can(Soulless))
                    factions./~(w => w.at(f.prison)).%(u => u.faction == TT && u.cultist).num
                else 0
                val captured = allCaptured - greenDecayCultists - glaakiIGOOGreenDecayCultists - soullessCultists
                // Disaster Looms (Bloated Woman spellbook): Gates earn 1 ES instead of 2 Power
                val disasterLooms = f.can(DisasterLooms)
                val yogGateSuppressed = f.unitGate.any && f.unitGate.exists(u => ElderThingMindControl.suppresses(u))
                val yogGateCount = f.unitGate.any.??(if (yogGateSuppressed) 0 else 1)
                val ownGates = if (disasterLooms) 0 else f.gates.num + yogGateCount
                val disasterLoomsGates = if (disasterLooms) f.gates.num + yogGateCount else 0
                val oceanGates = (f.can(YhaNthlei) && f.has(Cthulhu)).??(f.enemies./(f => f.allGates.%(_.glyph == Ocean).num).sum)
                val darkYoungs = f.can(RedSign).??(f.all(DarkYoung).num)
                val feast = f.has(Feast).??(desecrated.%(r => f.at(r).any).num)
                val abandoned = abandonedGates.num
                var worship = 0

                if (f.can(WorshipServices))
                    f.enemies.foreach { e =>
                        areas.%(cathedrals.contains).%(e.gates.has).some.foreach { l => worship += l.num }
                    }
                else
                if (factions.%(_.can(WorshipServices)).num > 0)
                    areas.%(cathedrals.contains).%(f.gates.has).some.foreach { l => worship += l.num }

                // Firstborn (FB): grant +1 power during Gather Power if the High Priests game option is enabled
                val fbHPBonus = (f == FB && options.has(HighPriests)).??(1)

                f.power = hibernate + ownGates * 2 + abandoned + cultists + captured + oceanGates + darkYoungs + feast + worship + fbHPBonus
                f.hibernating = false

                val fromHibernate = (hibernate > 0).?(hibernate.styled("region") + " hibernate")
                val fromGates = (ownGates > 0).?((("2 x " + ownGates).styled("region") + " gate".s(ownGates)))
                val fromAbandoned = (abandoned > 0).?(abandoned.styled("region") + " abandoned")
                val fromCultist = (cultists > 0).?(cultists.styled("region") + " cultist".s(cultists))
                val fromCaptured = (captured > 0).?(captured.styled("region") + " captured")
                val fromGreenDecay = (greenDecayCultists > 0).?(greenDecayCultists.styled("region") + " captured cultist".s(greenDecayCultists) + " → " + greenDecayCultists.es + " (Green Decay)")
                val fromGlaakiIGOOGreenDecay = (glaakiIGOOGreenDecayCultists > 0).?(glaakiIGOOGreenDecayCultists.styled("region") + " captured cultist".s(glaakiIGOOGreenDecayCultists) + " → " + glaakiIGOOGreenDecayCultists.es + " (Green Decay)")
                val fromYhaNthlei = (oceanGates > 0).?(oceanGates.styled("region") + " enemy controlled ocean gate".s(oceanGates))
                val fromDarkYoungs = (darkYoungs > 0).?(darkYoungs.styled("region") + " Dark Young".s(darkYoungs))
                val fromFeast = (feast > 0).?(feast.styled("region") + " desecrated")
                val fromWorship = (worship > 0).?(worship.styled("region") + " cathedral".s(worship))
                // Firstborn (FB): log line for the High Priest power bonus
                val fromFBHP = (fbHPBonus > 0).?(fbHPBonus.styled("region") + " High Priest bonus")

                f.log("got", f.power.power, "(" + $(fromHibernate, fromGates, fromAbandoned, fromCultist, fromCaptured, fromYhaNthlei, fromDarkYoungs, fromFeast, fromWorship, fromFBHP).flatten.mkString(" + ") + ")")

                if (greenDecayCultists > 0) {
                    f.log("Green Decay".styled("nt") + ":", greenDecayCultists, "captured " + "cultist".s(greenDecayCultists), "→", greenDecayCultists.es, "(not power)")
                    f.takeES(greenDecayCultists)
                }

                // Gla'aki IGOO Green Decay: captured enemy cultists give ES instead of power
                if (glaakiIGOOGreenDecayCultists > 0) {
                    f.log("Green Decay".styled("nt") + ":", glaakiIGOOGreenDecayCultists, "captured " + "cultist".s(glaakiIGOOGreenDecayCultists), "→", glaakiIGOOGreenDecayCultists.es, "(not power)")
                    f.takeES(glaakiIGOOGreenDecayCultists)
                }

                // TT Soulless: log blocked power from TT's captured cultists
                if (soullessCultists > 0)
                    f.log("power from" , soullessCultists, "captured " + "cultist".s(soullessCultists) + " blocked by", Soulless.styled(TT))

                // Disaster Looms: gates earn ES instead of Power
                if (disasterLoomsGates > 0) {
                    f.takeES(disasterLoomsGates)
                    f.log("Disaster Looms".styled("nt") + ":", disasterLoomsGates, "gate".s(disasterLoomsGates), "→", disasterLoomsGates.es, "(instead of Power)")
                }

                // Brown Jenkin: Loathsome Titter — bonus Power if BJ is at enemy-controlled gate
                if (f.loyaltyCards.has(BrownJenkinCard)) {
                    f.allInPlay.%(_.uclass == BrownJenkin).foreach { bj =>
                        val r = bj.region
                        val enemyGateOwner = factions.but(f).find(_.gates.has(r))
                        enemyGateOwner.foreach { _ =>
                            // Card: "+1 more power for each enemy Cultist in the area" — ALL enemies
                            // BB Fix 83, v2.4.31 — When non-BB faction holds Brown Jenkin, EarthCats
                            // count as enemy "Cultists" for Loathsome Titter (BB's Cultist-equivalent).
                            // When BB holds Brown Jenkin, EarthCats do NOT count (BB cannot generate
                            // Loathsome Titter power off its own EarthCats).
                            val enemyCultists =
                                if (f != BB)
                                    factions.but(f)./~(_.at(r).%(u => u.uclass.utype == Cultist || u.uclass == EarthCat)).num
                                else
                                    factions.but(f)./~(_.at(r).%(_.uclass.utype == Cultist)).num
                            val bonus = 2 + enemyCultists
                            f.power += bonus
                            f.log("Loathsome Titter".styled("nt") + ":", bonus.power, "in", r)
                        }
                    }
                }

                // Great Race of Yith: Possession — +1 Power per captive during Gather Power
                if (f.loyaltyCards.has(GreatRaceOfYithCard) && f.allInPlay.%(_.uclass == GreatRaceOfYith).any) {
                    val groyBonus = allCaptured
                    if (groyBonus > 0) {
                        f.power += groyBonus
                        f.log("Possession".styled("nt") + ":", groyBonus.power, "from", groyBonus, "captive".s(groyBonus))
                    }
                }

                // Bubastis (BB): per-Cat unit Special — each Cat (Earth, Mars, Saturn, Uranus)
                // generates +1 Power during the Gather Power Phase (rules: every Cat unit's
                // "Special: Generates 1 Power during the Gather Power Phase"). This is NOT
                // Catabolism — Catabolism is the Recruit-via-Earth-Cats spellbook gated in
                // recruits(f). High Priests bonus: BB has no HP unit/card, so the HP option
                // grants +1 Power per Gather instead (task 3.8.2).
                if (f == BB) {
                    // Per-Cat Special applies regardless of Moon vs. Earth area: the Moon counts
                    // as a land Area with a Bubastis-Controlled Gate "for all purposes" except
                    // control-seizure (source/bubastis.txt line 108). The unit card Special has
                    // no "non-Moon" qualifier (contrast Ailurophobia which does). Count every BB
                    // Monster Cat that is in play (i.e., has a region — not in pool/reserve and
                    // not eliminated). f.allInPlay already gives in-play units.
                    val bbCats = f.allInPlay.%(u => u.uclass.utype == Monster).num
                    if (bbCats > 0) {
                        f.power += bbCats
                        f.log("gained", bbCats.power, "from", bbCats, "Cat".s(bbCats), "(per-Cat Special)")
                    }
                    if (options.has(HighPriests)) {
                        f.power += 1
                        f.log("gained", 1.power, "High Priests bonus")
                    }
                }
            }

            // Gla'aki IGOO Tomb Herd: BEFORE captured cultists returned, gain power = pool cultists count
            factions.foreach { f =>
                if (f.has(GlaakiIGOO)) {
                    val poolCultists = f.pool.%(_.uclass.utype == Cultist).num
                    if (poolCultists > 0) {
                        f.power += poolCultists
                        f.log("gained", poolCultists.power, "from", poolCultists, "cultist".s(poolCultists), "in their pool via", "Tomb Herd".styled("nt"))
                    }
                }
            }

            factions.foreach { f =>
                val captured = factions./~(_.at(f.prison))

                if (captured.any) {
                    captured.foreach(eliminate)

                    // Log release — split Soulless-blocked TT cultists out for clarity
                    val soullessReleased = (factions.has(TT) && TT.can(Soulless)).??(captured.%(u => u.faction == TT && u.cultist))
                    val otherReleased = captured.diff(soullessReleased)
                    if (soullessReleased.any)
                        f.log("released", soullessReleased.mkString(", "), "(Soulless: provided no power)")
                    if (otherReleased.any)
                        f.log("released", otherReleased.mkString(", "))
                }
            }
            } // end of `if (!shepherdDoneThisGather)` — closes the initial-entry guard

            // TS Shepherd of the Crypt: must run BEFORE raise-to-half.
            // Gather Power ordering: power calc → Shepherd → raise-to-half → triggers → AfterPowerGatherAction (MaoCeremony).
            // Nothing should grant power AFTER raise-to-half.
            // [2026-05-23] Guarded by !shepherdDoneThisGather so the re-entry
            // path from TSExpansion.TSShepherdGatherAction (after Shepherd
            // completes) doesn't loop. `last` is stashed into TSExpansion so
            // the re-entry can rebuild PowerGatherAction(last). See FactionTS.scala.
            if (factions.has(TS) && TS.onMap(Glaaki).any && TS.onMap(TombHerd).any && !TSExpansion.shepherdDoneThisGather) {
                val replayBlocksShepherd = nextReplayActionHint.exists(h => !h.startsWith("TSShepherdGather"))
                if (replayBlocksShepherd) {
                    // noop
                } else if (ElderThingMindControl.suppresses(TS.onMap(Glaaki).head)) {
                    TS.log("Shepherd of the Crypt".styled("nt"), "blocked by", "Elder Thing".styled("nt"))
                } else {
                    val regions = areas.nex.%(r => TS.at(r, TombHerd).any)
                    if (regions.any) {
                        TSExpansion.pgrLastFaction = last
                        return Force(TSShepherdGatherPhaseAction(TS, regions))
                    }
                }
            }

            val max = factions./(_.power).max
            val min = (max + 1) / 2

            if (min == 0) {
                log("Humanity won")

                return GameOver($)
            }

            factions.foreach { f =>
                if (f.power < min) {
                   f.log("power increased to", min.power)
                   f.power = min
                }

                f.active = true
            }

            gatherPowerPhase = true
            triggers()
            gatherPowerPhase = false

            AfterPowerGatherAction // Then(...)

        case TSShepherdDoneAction =>
            TSExpansion.shepherdDoneThisGather = true

            val max = factions./(_.power).max
            val min = (max + 1) / 2

            if (min == 0) {
                log("Humanity won")
                return GameOver($)
            }

            factions.foreach { f =>
                if (f.power < min) {
                   f.log("power increased to", min.power)
                   f.power = min
                }
                f.active = true
            }

            gatherPowerPhase = true
            triggers()
            gatherPowerPhase = false

            AfterPowerGatherAction

        // AfterPowerGatherAction runs AFTER raise-to-half.
        // MaoCeremony MUST be after raise-to-half — do not move it before.
        case AfterPowerGatherAction =>
            // [2026-05-23] Reset the Shepherd guard so the NEXT gather-power
            // round fires Shepherd again. The flag stayed true from Shepherd
            // completion through the re-entry to PowerGatherAction so the
            // inline dispatch could be skipped on re-entry; now we're past
            // raise-to-half and can safely clear it.
            TSExpansion.shepherdDoneThisGather = false
            factions.foreach { f =>
                if (f.want(MaoCeremony)) {
                    f.cultists.onMap.some.foreach { l =>
                        return Ask(f).each(l)(c => MaoCeremonyAction(f, c.region, c.uclass)).add(MaoCeremonyDoneAction(f))
                    }
                }
            }

            factions.foreach(_.ignorePerInstant = $)

            BeforeFirstPlayerAction(factions)

        case BeforeFirstPlayerAction(l) =>
            val max = factions./(_.power).max
            val asks = l./~{ f =>
                implicit val asking = Asking(f)

                + GroupAction("Before First Player determination")

                if (f.power + 2 >= max && f.power <= f.enemies./(_.power).max)
                    if (f.onMap(HighPriest).any)
                        if (f.commands.has(UnspeakableOathOpportunityFirstPlayer) || f.commands.has(UnspeakableOathPrompt))
                            + SacrificeHighPriestPromptAction(f, BeforeFirstPlayerAction(factions)).as("Sacrifice", HighPriest.styled(f))("Unspeakable Oath".hl)

                f << [OW] { f =>
                    if (f.can(DragonAscending) && f.power < max)
                        + DragonAscendingAction(f, None, None, max, BeforeFirstPlayerAction(factions))
                }

                |(asking.ask).%(_.actions.%!(_.isInfo).any)./{ _
                    .add(NeedOk)
                    .add(OutOfTurnRefresh(BeforeFirstPlayerAction(l)))
                    .add(SacrificeHighPriestAllowedAction)
                    .group(" ")
                    .skip(BeforeFirstPlayerAction(l.but(f)))
                }
            }

            if (asks.any)
                MultiAsk(asks)
            else
                FirstPlayerDeterminationAction // Then(...)

        case DoomPhaseAction =>
            doomPhase = true

            // Library at Celaeno: distribute Silence Tokens at start of Doom Phase
            if (board.isLibraryMap) {
                factions.foreach { f =>
                    val max = board.silenceTokenMax(f)
                    val current = silenceTokens.getOrElse(f, 0)
                    if (current < max) {
                        silenceTokens = silenceTokens + (f -> (current + 1))
                        f.log("received", "Silence Token".styled("lb"))
                    }
                }
                // Auto-flip Library Tomes face-up at Doom Phase start
                tomeFaceUp = tomeFaceUp + (TomeGuardian -> true) + (TomeLarvae -> true) + (TomeYr -> true)
            }

            factions.foreach { f =>
                // Round 8 Bug 40: also check facedown state for IGOO spellbooks
                val brood = f.enemies.%(e => e.has(TheBrood) && !e.oncePerGame.has(TheBrood))
                val yogDoomSuppressed = f.unitGate.exists(u => ElderThingMindControl.suppresses(u))
                val gates = f.gates ++ (if (yogDoomSuppressed) $ else f.unitGate./(_.region))
                val valid = gates.%!(r => brood.exists(_.at(r)(Filth).any))

                f.doom += valid.num
                f.log("got", valid.num.doom)

                if (f.loyaltyCards.has(GnorriCard)) {
                    val gnorriCount = f.all(Gnorri).num
                    if (gnorriCount >= 3) {
                        f.doom += 2
                        f.log("gained", 2.doom, "from", "Grottos".styled("neutral"), "(3 Gnorri in play)")
                    } else if (gnorriCount >= 2) {
                        f.doom += 1
                        f.log("gained", 1.doom, "from", "Grottos".styled("neutral"), "(2 Gnorri in play)")
                    }
                }

                if (f.has(Byatis) && f.allInPlay.%(_.uclass == Byatis).any) {
                    if (ElderThingMindControl.suppresses(f.goo(Byatis))) {
                        f.log(ToadOfBerkeley, "blocked by", "Elder Thing".styled("nt"))
                    } else {
                        val r = f.goo(Byatis).region
                        if (factionlike.but(f).exists(_.present(r)).not) {
                            f.log("gained", 1.es, "from", Byatis.styled(f), "and", ToadOfBerkeley)
                            f.takeES(1)
                        }
                    }
                }

                // Round 8 Bug 40: also check facedown state for IGOO spellbooks
                if (f.upgrades.has(TheRevelations) && !f.oncePerGame.has(TheRevelations)) {
                    f.enemies.foreach { e =>
                        e.takeES(1)
                        e.log("gained", 1.es, "from", TheRevelations.styled(f))
                    }
                }
            }

            log(CthulhuWarsSolo.DottedLine)
            showROAT()

            // Firstborn (FB) Bug fix Round 3: after doom is distributed at the start of the
            // doom phase, check FBMostDoomOrMoreGates so the requirement can be satisfied
            // immediately (not delayed until FB's next action-phase turn).
            FBExpansion.checkMostDoomOrGates()

            CheckSpellbooksAction(DoomNextPlayerAction(game.first))

        case ActionPhaseAction =>
            if (factions.%(_.doom >= 30).any || ritualTrack(ritualMarker) == 999)
                return GameOverPhaseAction

            doomPhase = false
            endActionPhasePrompts = false
            fbGhatoLastMoveOrigin = None
            factions = doomOrder

            // Nuclear Chaos (Azathoth spellbook): flip back face-up at start of Action Phase
            factions.foreach { f =>
                if (f.oncePerGame.has(NuclearChaos) && f.upgrades.has(NuclearChaos)) {
                    f.oncePerGame = f.oncePerGame.but(NuclearChaos)
                }
            }

            log(CthulhuWarsSolo.DoubleLine)
            log("Turn", turn)
            log("ACTIONS")

            round = 0

            CheckSpellbooksAction(PreMainAction(game.first))

        case GameOverPhaseAction =>
            factions.%(_.needs(AnytimeGainElderSigns)).foreach { f =>
                f.satisfy(AnytimeGainElderSigns, "Anytime Spellbook", f.enemies.%(_.hasAllSB).num.upTo(3))
                return CheckSpellbooksAction(GameOverPhaseAction)
            }

            factions.%(_.es.any).foreach { f =>
                val sum = f.es./(_.value).sum
                f.log("revealed", f.es./(_.short).mkString(" "), "for", sum.doom)
                f.doom += sum
                f.revealed ++= f.es
                f.es = $
            }

            // Tombstalker (TS) Cursed Tomes: at final revelation, each face-down tome costs its holder 1 Doom
            // TS itself is exempt since tomes are their pool/resource, never a penalty for them
            if (ritualTrack(ritualMarker) != 999) {
                factions.%(f => f != TS).foreach { f =>
                    val faceDownCount = cursedTomesOwned.get(f).|(Nil).count { case (_, fd) => fd }
                    if (faceDownCount > 0) {
                        f.doom -= faceDownCount
                        f.log("lost", faceDownCount.doom, "from", faceDownCount, "face-down " + "Cursed Tome".s(faceDownCount).styled(TS))
                        val remaining = cursedTomesOwned.get(f).|(Nil).filter { case (_, fd) => !fd }
                        if (remaining.any)
                            cursedTomesOwned = cursedTomesOwned + (f -> remaining)
                        else
                            cursedTomesOwned = cursedTomesOwned - f
                    }
                }
            }

            val contenders = factions.%(_.hasAllSB)
            val winners = contenders.%(_.doom == contenders./(_.doom).max)

            if (winners.none)
                log("Humanity won")
            else
                log(winners.mkString(", "), "won")

            GameOver(winners)

        case FirstPlayerDeterminationAction =>
            val max = factions./(_.power).max
            val fs = factions.%(f => f.power == max)

            if (fs.num > 1) {
                Ask(game.first).each(fs)(f => FirstPlayerAction(game.first, f))
            }
            else {
                val old = game.first

                game.first = fs.only

                if (old != game.first)
                    game.first.log("became the first player")

                PlayOrderAction // Then(...)
            }

        case FirstPlayerAction(f, first) =>
            game.first = first

            if (f == first)
                f.log("decided to remain the first player")
            else
                f.log("chose", first, "as the first player")

            PlayOrderAction // Then(...)

        case PlayOrderAction =>
            game.first.satisfy(FirstPlayer, "Become First Player")

            val forward = setup.dropWhile(_ != game.first) ++ setup.takeWhile(_ != game.first)
            val backward = forward.take(1) ++ forward.drop(1).reverse

            Ask(game.first)
                .add(PlayDirectionAction(game.first, forward))
                .add(PlayDirectionAction(game.first, backward))

        case PlayDirectionAction(_, l) =>
            factions = l
            doomOrder = l

            log("Play order", factions.mkString(", "))

            if (turn == 1)
                ActionPhaseAction // Then(...)
            else {
                log(CthulhuWarsSolo.DottedLine)
                log("DOOM PHASE")

                // Clear moonbeast placed-this-doom tracker at start of doom phase
                moonbeastPlacedThisDoom = Set()

                // Ghatanothoa IGOO: un-mummify all cultists at Doom Phase
                if (mummifiedCultists.any) {
                    mummifiedCultists = $
                    log("Mummify".styled("nt") + ": all mummified cultists are freed at Doom Phase")
                }

                factions.foreach(f => f.satisfyIf(FirstDoomPhase, "The first Doom phase", turn == 2))
                factions.foreach(f => f.satisfyIf(FiveSpellbooks, "Have five spellbooks", f.unfulfilled.num == 1))

                CheckSpellbooksAction(DoomPhaseAction) // Then(...)
            }

        // SPELLBOOK
        case CheckSpellbooksAction(next) =>
            // Bubastis (BB): check auto-satisfiable requirements
            if (factions.has(BB)) {
                BB.satisfyIf(NoEarthCatsOnMoon,        "No Earth Cats on the Moon",                   BB.at(BB.moon).%(_.uclass == EarthCat).none)
                val catInEveryStart = factions.but(BB).forall(e => starting.get(e).exists(r => BB.at(r).%(u => u.uclass == EarthCat || u.uclass == CatFromMars || u.uclass == CatFromSaturn || u.uclass == CatFromUranus).any))
                if (BB.needs(CatInEveryEnemyStart) && catInEveryStart) {
                    val bonus = factions.but(BB).num
                    BB.satisfy(CatInEveryEnemyStart, "A Cat in every enemy faction's Start Area")
                    if (bonus > 0) {
                        BB.power += bonus
                        BB.log("gained", bonus.power, "(Cat in every Start Area bonus)")
                    }
                }
            }

            val fs = factions.%(f => f.unfulfilled.num + f.spellbooks.num < f.library.num)
            val fe = factions.%(f => f.es.%(_.value == 0).any)

            if (fs.any) {
                val f = fs(0)
                // Map library names for buff option spellbook swaps before filtering
                val effectiveLibrary = f.library.map { sb => (f, sb) match {
                    case (DS, Traitors) if options.has(DSAlternateSpellbooks) => Omnipotence
                    case (DS, FiendishGrowth) if options.has(DSAlternateSpellbooks) => FiendishSpawn
                    case (DS, UndirectedEnergy) if options.has(DSAlternateSpellbooks) => DirectedEnergy
                    case (SL, EnergyNexus) if options.has(SleeperEnergyNexusPreBattle) => EnergyNexusPB
                    // BUBASTIS: alt-variant swaps Catabolism→Syzygy and Ailurophobia→Carnivore
                    case (BB, Catabolism)   if options.has(BBAlternateSpellbooks) => Syzygy
                    case (BB, Ailurophobia) if options.has(BBAlternateSpellbooks) => Carnivore
                    case _ => sb
                }}
                var bs = (effectiveLibrary.%!(f.has) ++ neutralSpellbooks).diff(f.ignorePerInstant)
                // TT tribe spellbook filtering: only offer the 6 active library books (3 shared + 3 tribal)
                if (f == TT && TTExpansion.ttActiveLibrary.any)
                    bs = bs.%(b => TTExpansion.ttActiveLibrary.has(b) || neutralSpellbooks.has(b))
                // OW lost-2nd-spellbook fix: attach an OutOfTurnRefresh so that if an
                // out-of-turn power (e.g. OW Dragon Ascending) is taken between two owed
                // spellbook awards, returning via OutOfTurnReturn re-enters CheckSpellbooks
                // and re-prompts any still-owed book instead of silently dropping it.
                Ask(f).each(bs)(b => SpellbookAction(f, b, next)).add(OutOfTurnRefresh(CheckSpellbooksAction(next)))
            }
            else
            if (fe.any) {
                val f = fe(0)
                val n = f.es.%(_.value == 0).num
                val es = factions./~(f => f.es ++ f.revealed)

                DrawES("" + f + " gets " + n.es, 18 - es.%(_.value == 1).num, 12 - es.%(_.value == 2).num, 6 - es.%(_.value == 3).num, (x, public) => ElderSignAction(f, n, x, public, next))
            }
            else {
                Then(next)
            }

        case SpellbookAction(f, sb, next) =>
            f.spellbooks = f.spellbooks :+ sb

            // Moonbeast: if a Moonbeast was on this slot (unfulfilled), it now blocks the earned spellbook
            if (moonbeastOnSpellbook.values.exists(t => t._1 == f && t._2 == sb)) {
                f.oncePerGame :+= sb
                f.log("received", sb, "but blocked by", "Moonbeast".styled("nt"))
            } else {
                f.log("received", sb)
            }

            neutralSpellbooks = neutralSpellbooks.but(sb)

            if (f.hasAllSB)
                factions.foreach(_.satisfy(AnotherFactionAllSpellbooks, "Another faction has all spellbooks"))

            f.ignorePerInstant = $

            // TT Hierophants: when TT earns a Faction Spellbook, place HP at gate or grow counter
            if (f == TT && f.has(Hierophants) && sb.isInstanceOf[FactionSpellbook]) {
                val sbNext = CheckSpellbooksAction(next)
                // If Hierophants was just earned and HP expansion active, also prompt all other factions first
                if (sb == Hierophants && options.has(HighPriests)) {
                    val others = factions.but(TT).%(_.canRecruitHP).%(_.pool(HighPriest).any)
                    if (others.any)
                        return Force(TTHierophantsOtherFactionsAction(f, others, sbNext))
                }
                return Force(TTHierophantsPlaceHPAction(f, f.gates, sbNext))
            }

            CheckSpellbooksAction(next)

        case ElderSignAction(f, _, v, public, next) =>
            if (v == 0) {
                val n = f.es.%(_.value == 0).num
                f.es = f.es.%(_.value > 0)
                f.doom += n
                log("No more", "Elder Signs".styled("es") + ",", f, "got", n.doom, "instead")
            }
            else {
                f.es = f.es.%(_.value > 0) ++ f.es.%(_.value == 0).drop(1) :+ ElderSign(v)
                if (public)
                    f.log("got", 1.es, "worth", v.doom)
            }
            CheckSpellbooksAction(next)


        // REVEAL
        case RevealESMainAction(f, then) =>
            Ask(f).each(f.es.%(_.value > 0).any.$($()) ++ f.es.%(_.value > 0)./(e => $(e)))(RevealESAction(f, _ , doomPhase && f.has(StarsAreRight), then)).cancel

        case RevealESOutOfTurnAction(f) =>
            Ask(f).each(f.es.%(_.value > 0).any.$($()) ++ f.es.%(_.value > 0)./(e => $(e)))(RevealESAction(f, _ , doomPhase && f.has(StarsAreRight), OutOfTurnReturn)).cancel

        case InfoESOutOfTurnAction(f) =>
            Ask(f).each(f.es.%(_.value > 0).any.$($()) ++ f.es.%(_.value > 0)./(e => $(e)))(InfoESAction(f, _ , doomPhase && f.has(StarsAreRight), OutOfTurnReturn)).add(NeedOk).cancel

        case RevealESAction(f, Nil, power, next) =>
            Force(RevealESAction(f, f.es.%(_.value > 0), power, next))

        case RevealESAction(f, es, power, next) =>
            val sum = es./(_.value).sum
            f.doom += sum

            f.revealed ++= es
            f.es = f.es.diff(es)

            if (power)
                f.power += sum

            f.log("revealed", es./(_.short).mkString(" "), "for", sum.doom, power.??("and " + sum.power))

            Force(next)

        // DOOM
        case DoomAction(f) =>
            implicit val asking = Asking(f)

            game.rituals(f)

            game.reveals(f)

            game.highPriests(f)

            game.hires(f)

            game.doomDone(f)

            asking

        case DoomDoneAction(f) =>
            // Tombstalker (TS) Hecatomb
            if (f == TS && factions.has(TS) && TS.can(Hecatomb) && game.deathsHead > 0) {
                TS.log("Hecatomb: remaining", game.deathsHead.toString.styled("kill"), "Death's Head discarded")
                game.deathsHead = 0
            }

            FBExpansion.checkMostDoomOrGates()

            if (game.factions.has(FB) && FB.needs(FBTwoFacedownSpellbooks) && FBExpansion.faceDownCount >= 2)
                FB.satisfy(FBTwoFacedownSpellbooks, "2 Facedown Spellbooks")

            f.acted = false
            f.hired = false

            // Use saved doomOrder to find the next doom player. The action phase
            // rotates `factions` via NextPlayerAction; by the time the doom phase
            // runs, factions can be in a corrupted order. doomOrder is set once
            // per turn by PlayDirectionAction and never mutated.
            val doomIdx = doomOrder.indexOf(f)
            val next = doomOrder((doomIdx + 1) % doomOrder.size)
            factions = doomOrder.drop(doomIdx + 1) ++ doomOrder.take(doomIdx + 1)

            if (next != game.first) {
                pendingLine = |(CthulhuWarsSolo.DottedLine)

                DoomNextPlayerAction(next)
            }
            else {
                // Library at Celaeno: place gates in Special Collection rooms at end of first Doom Phase
                if (board.isLibraryMap && !libraryFirstDoomGatesDone) {
                    libraryFirstDoomGatesDone = true
                    import LibraryCelaeno55._
                    val tomeRegions = $(YrAndTheNhhngr, GuardianUnderLake, BarrierOfNaachTith, LarvaeOfOuterGods)
                    tomeRegions.foreach { r =>
                        if (!gates.has(r) && board.regions.has(r)) {
                            gates :+= r
                            log("Gate placed in", r, "(first Doom Phase)")
                        }
                    }
                }

                factions.foreach(_.borrowed = $)

                // Bokrug: Ghosts of Ib + Doom that Came to Sarnath fire after moonbeast return (only if Bokrug is in game)
                val bokrugOwner = factions.find(_.loyaltyCards.has(BokrugCard))
                val postMoonbeast = bokrugOwner./(owner => GhostsOfIbPlaceAction(owner, DoomSarnathMainAction(owner, CheckSpellbooksAction(ActionPhaseAction)))).|(CheckSpellbooksAction(ActionPhaseAction))

                // Moonbeast: return moonbeasts from spellbooks to map after ALL doom turns
                // Exclude moonbeasts placed THIS doom phase — they return next doom phase
                val returnable = moonbeastOnSpellbook.filter { case (ref, _) => !moonbeastPlacedThisDoom.contains(ref) }
                moonbeastPlacedThisDoom = Set()
                if (returnable.any) {
                    val entries = returnable.toList
                    return Force(MoonbeastDoomReturnAction(factions.first, entries, postMoonbeast))
                }

                Force(postMoonbeast)
            }

        case DoomNextPlayerAction(f) =>
            DoomAction(f)

        // RITUAL
        case RitualAction(f, cost, k) =>
            f.power -= cost

            // Round 8 Bug 40: also check facedown state for IGOO spellbooks
            val brood = f.enemies.%(e => e.has(TheBrood) && !e.oncePerGame.has(TheBrood))
            val gates = f.allGates
            val valid = gates.%!(r => brood.exists(_.at(r)(Filth).any))

            val doom = valid.num * k

            val es = f.goos.factionGOOs.num + f.can(Consecration).??($(0, 1, 1, 1, 2)(cathedrals.num))

            // TT Sycophancy: when an ENEMY performs a ritual, pause before doom resolves and prompt the ritualer
            if (factions.has(TT) && f != TT && TT.has(Sycophancy)) {
                // power already deducted above; doom/es not yet applied — pass them to the prompt continuation
                return Force(TTSycophancyPromptAction(f, doom, es))
            }

            Force(TTSycophancyResumeRitualAction(f, doom, es))

        // TT Sycophancy resume point — shared by normal ritual and Sycophancy-adjusted doom values
        case TTSycophancyResumeRitualAction(f, doom, es) =>
            f.doom += doom

            log(CthulhuWarsSolo.DottedLine)
            f.log("performed the ritual and gained", doom.doom, (es > 0).??("and " + es.es))

            f.takeES(es)

            // TT Tablets of the Gods: when TT rituals, gain bonus ES (1 per gate with any HP), then eliminate all HPs
            if (factions.has(TT) && f == TT && f.can(TabletsOfTheGods)) {
                val tabletsBonus = f.gates.count(r => f.at(r, HighPriest).any)
                if (tabletsBonus > 0) {
                    f.takeES(tabletsBonus)
                    f.log(TabletsOfTheGods.styled(TT), ": gained", tabletsBonus.es, "bonus (HP at gates)")
                }
                f.all(HighPriest).onMap.foreach(eliminate)
                f.log(TabletsOfTheGods.styled(TT), ": eliminated all High Priests")
            }

            // TT Inerrant: when TT rituals, gain bonus ES (1 per enemy-controlled gate where TT has a GOO)
            if (factions.has(TT) && f == TT && f.can(Inerrant)) {
                val inerrantBonus = areas.count(r =>
                    f.enemies.exists(e => e.gates.has(r)) && f.at(r).goos.any
                )
                if (inerrantBonus > 0) {
                    f.takeES(inerrantBonus)
                    f.log(Inerrant.styled(TT), ": gained", inerrantBonus.es, "bonus (GOOs at enemy gates)")
                }
            }

            f.acted = true

            ritualHistory :+= f
            ritualHistoryCeremony :+= false

            triggers()

            if (ritualTrack(ritualMarker) != 999)
                ritualMarker += 1

            showROAT()

            f.satisfy(PerformRitual, "Perform Ritual of Annihilation")

            val faceDownTomes = if (f == TS) Nil else cursedTomesOwned.get(f).|(Nil).filter { case (_, fd) => fd }
            if (faceDownTomes.any) {
                implicit val asking = Asking(f)
                faceDownTomes.foreach { case (n, _) => + TSRemoveTomeAction(f, n) }
                + TSSkipRemoveTomeAction(f)
                asking
            } else
                CheckSpellbooksAction(DoomAction(f))

        // Bubastis Requires Attention: doom-phase ritual via Bastet + enemy Cultist
        case RequiresAttentionMainAction(self) =>
            val bastetRegions = self.allInPlay.%(_.uclass == Bastet).%(_.region.onMap)./(_.region)
            // BB Fix 78, v2.4.30 — skip regions where any Elder Thing is co-located with Bastet (combat unaffected).
            // Defense-in-depth filter mirrors the menu-gate suppression at neutralSpellbooks(); even if a stale
            // RequiresAttentionMainAction reaches this handler from a queue / replay, the Elder-Thing-blocked region
            // must not appear as a target.
            val eligible = bastetRegions.%(r => factions.but(self).exists(e => e.at(r).%(u => u.uclass.utype == Cultist).any))
                                       .%(r => !factions.exists(e => e.at(r).%(_.uclass == ElderThing).any))
            Ask(self).each(eligible)(r => RequiresAttentionTargetAction(self, r)).cancel

        case RequiresAttentionSkipAction(self) =>
            self.log(RequiresAttention.styled(BB) + ": declined")
            EndAction(self)

        case RequiresAttentionTargetAction(self, r) =>
            // BB v2.4.17: Requires Attention is now treated as a true Ritual of
            // Annihilation for cost / track / glyph purposes — it pays the
            // current RoA Power cost (Herald discount honoured), places BB on
            // the current RoA-track slot (ritualHistory) and advances the
            // track by one — exactly like every other faction's Ritual.
            // The ONLY thing that stays bespoke is the doom/Elder-Sign reward
            // calc (flat 4 doom + enemy-gate/enemy-GOO ES bonuses), which is
            // unchanged from prior behaviour.
            val cost = self.can(Herald).?(5).|(ritualCost)
            self.power -= cost
            val enemyGate  = factions.but(self).exists(_.gates.has(r))
            val enemyGOO   = factions.but(self).exists(e => e.at(r).%(_.uclass.isGOO).any)
            val esBonus    = enemyGate.??(1) + enemyGOO.??(2)
            self.doom += 4
            self.log(RequiresAttention.styled(BB) + ": ritual in", r, "— paid", cost.power, "— gained", 4.doom)
            if (esBonus > 0)
                self.log(RequiresAttention.styled(BB) + ":", esBonus.es, "bonus (" + enemyGate.??("enemy gate") + (enemyGate && enemyGOO).??(", ") + enemyGOO.??("enemy GOO") + ")")
            self.takeES(esBonus)
            self.acted = true
            ritualHistory :+= self
            ritualHistoryCeremony :+= false
            triggers()
            if (ritualTrack(ritualMarker) != 999)
                ritualMarker += 1
            showROAT()
            self.satisfy(PerformRitual, "Perform Ritual of Annihilation")
            CheckSpellbooksAction(DoomAction(self))

        // MAIN
        case PreMainAction(f) if factions.exists(f => f.unfulfilled.num + f.spellbooks.num < f.library.num) =>
            CheckSpellbooksAction(PreMainAction(f))

        case PreMainAction(f) if f.active.not && f.hibernating.not =>
            implicit val asking = Asking(f)

            + GroupAction("Before " + f + " turn")

            if (f.all(HighPriest).any) {
                var reasons = f.commands.has(UnspeakableOathPrompt).$("always prompted")

                f.enemies.%(e => e.active || e.all(HighPriest).any).foreach { e =>
                    val canAct = true // e.acted.not
                    val canBattle = true // canAct || e.hasAllSB
                    def canBattleIn(r : Region) = true // canBattle && e.battled.has(r).not

                    if (f.commands.has(UnspeakableOathThreatOfHPCapture) && canAct)
                        f.onMap(HighPriest)./(_.region).distinct.%(r => e.canCapture(r)(f) && f.at(r).acolytes.none).some./{ l =>
                            reasons :+= "" + e + " might capture " + HighPriest.styled(f) + " " + ("in " + l.mkString(", ")).inline
                        }

                    if (f.commands.has(UnspeakableOathThreatOfAttackOnHighPriest) && canBattle)
                        f.onMap(HighPriest)./(_.region).distinct.%(r => canBattleIn(r) && e.canAttack(r)(f) && e.strength(e.at(r), f) / 2 + 1 > f.at(r).notGOOs.not(Yothan).not(HighPriest).num).some./{ l =>
                            reasons :+= "" + e + " might kill " + HighPriest.styled(f) + " " + ("in " + l.mkString(", ")).inline
                        }

                    if (f.commands.has(UnspeakableOathThreatOfAcolyteCapture) && canAct)
                        f.onMap(Acolyte)./(_.region).distinct.%(r => e.canCapture(r)(f)).some./{ l =>
                            reasons :+= "" + e + " might capture " + Acolyte.styled(f) + " " + ("in " + l.mkString(", ")).inline
                        }

                    if (f.commands.has(UnspeakableOathThreatOfAttackOnGate) && canBattle)
                        f.gates.%(r => canBattleIn(r) && e.canAttack(r)(f) && e.strength(e.at(r), f) / 2 + 1 > f.at(r).notGOOs.not(Yothan).not(HighPriest).num).some./{ l =>
                            reasons :+= "" + e + " might take the gate" + " " + ("in " + l.mkString(", ")).inline
                        }

                    if (f.commands.has(UnspeakableOathThreatOfAttackOnGate) && canBattle)
                        if (e.can(BeyondOne))
                            f.gates.intersect(areas).%(r => f.at(r).goos.none && e.at(r).%(_.uclass.cost >= 3).any).some./{ l =>
                                reasons :+= "" + e + " might " + BeyondOne + " " + ("from " + l.mkString(", ")).inline
                            }

                    if (f.commands.has(UnspeakableOathThreatOfGhroth))
                        if (e == BG && e.can(Ghroth) && e.power >= 2 && e.all(Fungi)./(_.region).distinct.num > f.acolytes.%!(_.onGate).num && f.acolytes.%(_.onGate).any)
                            reasons :+= "" + e + " might " + Ghroth + " an " + Acolyte.styled(f) + " on the Gate"

                    if (f.commands.has(UnspeakableOathThreatOfCatnapping))
                        if (e == BB && e.can(Catnapping))
                            BB.onMap(Bastet)./(_.region).%(r => f.at(r).any).some./{ l =>
                                reasons :+= "" + Bastet.styled(BB) + " might " + Catnapping.styled(BB) + " your units " + ("in " + l.mkString(", ")).inline
                            }

                    if (f.commands.has(UnspeakableOathOpportunityOfDreadCurse))
                        if (f == OW && f.can(DreadCurse) && (f.all(Abomination).any || f.all(SpawnOW).any))
                            if (e.onMap(GOO).exists(u => (f.all(Abomination).num + f.all(SpawnOW).num) / 2 + 1 > e.at(u.region).notGOOs.num))
                                reasons :+= "" + f + " might " + DreadCurse + " a GOO of " + e

                    if (f == GC && canAct)
                        if (e == SL && e.can(CursedSlumber) && e.gates.has(game.starting(f)))
                            reasons :+= "" + e + " might whisk away the gate " + ("from " + game.starting(f)).inline

                    if (f == GC && canAct)
                        if (e.can(BeyondOne) && f.at(game.starting(f)).goos.none && e.at(game.starting(f)).%(_.uclass.cost >= 3).any)
                            reasons :+= "" + e + " might whisk away the gate " + ("from " + game.starting(f)).inline

                    if (f.commands.has(UnspeakableOathThreatOfAttackOnGOO) && canBattle)
                        f.onMap(GOO)./(_.region).%(r => canBattleIn(r) && e.canAttack(r)(f) && (e.strength(e.at(r), f) / 2 + 1 > f.at(r).notGOOs.not(Yothan).not(HighPriest).num || e.at(r).got(Hastur))).some./{ l =>
                            reasons :+= "GOO might be in danger from " + e + " " + ("in " + l.mkString(", ")).inline
                        }
                }

                if (reasons.any)
                    + SacrificeHighPriestPromptAction(f, PreMainAction(f)).as("Sacrifice", HighPriest.styled(f))("Unspeakable Oath".hl, reasons./("<br/>(" + _ + ")").mkString(""))
            }

            asking.ask.useIf(_.actions.exists(_.isInfo.not))(_.add(NeedOk).add(OutOfTurnRefresh(PreMainAction(f))).add(SacrificeHighPriestAllowedAction).group(" ")).skip(MainAction(f))

        case PreMainAction(f) if f.active =>
            PreActionPromptsAction(f, f.enemies)

        case PreActionPromptsAction(e, l) =>
            val canAct = e.acted.not
            val canBattle = canAct || e.hasAllSB
            def canBattleIn(r : Region) = canBattle && e.battled.has(r).not

            val asks = l./~{ f =>
                implicit val asking = Asking(f)

                + GroupAction("After " + e + " action")

                if (f.onMap(HighPriest).any) {
                    var reasons = f.commands.has(UnspeakableOathPrompt).$("always prompted")

                    if (f.commands.has(UnspeakableOathThreatOfHPCapture) && canAct)
                        f.all(HighPriest)./(_.region).distinct.%(r => e.canCapture(r)(f) && f.at(r).acolytes.none).some./{ l =>
                            reasons :+= "" + e + " might capture " + HighPriest.styled(f) + " " + ("in " + l.mkString(", ")).inline
                        }

                    if (f.commands.has(UnspeakableOathThreatOfGhroth) && canAct)
                        if (e == BG && e.can(Ghroth) && e.power >= 2 && e.all(Fungi)./(_.region).distinct.num > f.acolytes.num)
                            reasons :+= "" + e + " might " + Ghroth + " " + HighPriest.styled(f)

                    if (f.commands.has(UnspeakableOathThreatOfCatnapping) && canAct)
                        if (e == BB && e.can(Catnapping))
                            BB.onMap(Bastet)./(_.region).%(r => f.at(r).any).some./{ l =>
                                reasons :+= "" + Bastet.styled(BB) + " might " + Catnapping.styled(BB) + " your units " + ("in " + l.mkString(", ")).inline
                            }

                    if (f.commands.has(UnspeakableOathThreatOfAttackOnHighPriest) && canBattle)
                        f.all(HighPriest)./(_.region).distinct.%(r => canBattleIn(r) && e.canAttack(r)(f) && e.strength(e.at(r), f) / 2 + 1 > f.at(r).notGOOs.not(Yothan).not(HighPriest).num).some./{ l =>
                            reasons :+= "" + e + " might kill " + HighPriest.styled(f) + " " + ("in " + l.mkString(", ")).inline
                        }

                    if (f.commands.has(UnspeakableOathThreatOfDryEternal) && canBattle)
                        if (f.has(RhanTegoth) && f.power == 0)
                            f.goos./(_.region).%(r => canBattleIn(r) && e.canAttack(r)(f) && (e.strength(e.at(r), f) / 2 + 1 > f.at(r).notGOOs.not(Yothan).not(HighPriest).num || e.at(r).got(Hastur))).some./{ l =>
                                reasons :+= "" + RhanTegoth + " might not have power for " + Eternal
                            }

                    if (f.commands.has(UnspeakableOathThreatOfThousandForms) && canAct)
                        if (e == CC && e.has(Nyarlathotep) && e.can(ThousandForms) && f.power < 6)
                            reasons :+= "" + e + " might roll for " + ThousandForms + " unopposed"

                    if (reasons.any)
                        + SacrificeHighPriestPromptAction(f, PreMainAction(e)).as("Sacrifice", HighPriest.styled(f))("Unspeakable Oath".hl, reasons./("<br/>(" + _ + ")").mkString(""))
                }

                if (f.can(Devolve) && f.onMap(Acolyte).any && f.pool(DeepOne).any) {
                    var reasons = f.commands.has(DevolvePrompt).$("always prompted")
                    var areas = f.onMap(Acolyte)./(_.region).distinct

                    if (f.commands.has(DevolveThreatOfCapture) && canAct)
                        areas.%(r => e.canCapture(r)(f)).some./{ l =>
                            reasons :+= "" + e + " might capture " + Acolyte.styled(f) + " " + ("in " + l.mkString(", ")).inline
                        }

                    if (f.commands.has(DevolveThreatOfZingaya) && canAct)
                        if (e == YS && e.can(Zingaya) && e.pool(Undead).any)
                            areas.%(r => e.at(r).got(Undead)).some./{ l =>
                                reasons :+= "" + e + " might " + Zingaya + " " + ("in " + l.mkString(", ")).inline
                            }

                    if (f.commands.has(DevolveThreatOfBeyondOne) && canAct)
                        if (e.can(BeyondOne))
                            f.gates.intersect(areas).%(r => f.at(r).goos.none && e.at(r).%(_.uclass.cost >= 3).any).some./{ l =>
                                reasons :+= "" + e + " might " + BeyondOne + " " + ("from " + l.mkString(", ")).inline
                            }

                    if (f.commands.has(DevolveThreatOfAttackOnGate) && canBattle)
                        f.gates.intersect(areas).%(r => canBattleIn(r) && e.canAttack(r)(f) && e.strength(e.at(r), f) * 3 / 4 + 1 >= f.at(r).num).some./{ l =>
                            reasons :+= "" + e + " might attack the gate" + " " + ("in " + l.mkString(", ")).inline
                        }

                    if (f.commands.has(DevolveThreatOfAttackOnGOO) && canBattle)
                        f.onMap(GOO)./(_.region).intersect(areas).%(r => canBattleIn(r) && e.canAttack(r)(f) && (e.strength(e.at(r), f) / 2 + 1 > f.at(r).notGOOs.not(Yothan).not(HighPriest).num || e.at(r).got(Hastur))).some./{ l =>
                            reasons :+= "GOO might be in danger from " + e + " " + ("in " + l.mkString(", ")).inline
                        }

                    if (reasons.any)
                        + DevolvePromptAction(f.sure[GC], PreMainAction(e)).as("Acolytes".styled(f), "to", "Deep Ones".styled(f))(Devolve, reasons./("<br/>(" + _ + ")").mkString(""))
                }

                if (f.can(DragonAscending) && f.enemies.exists(_.power > f.power))
                    if (factions./(_.power).max - f.power >= f.commands.of[DragonAscendingPower].single./(_.power).|(1))
                        + DragonAscendingPromptAction(f.sure[OW], e, PreMainAction(e)).as("Rise to", factions./(_.power).max.power)(DragonAscending)

                // DS deploy: always show when "always prompt" is set, or when conditions match
                if (f.loyaltyCards.has(DimensionalShamblerCard) && f.at(ShamblerHold(f), DimensionalShamblerUnit).any) {
                    if (f.commands.has(ShamblerPrompt))
                        + ShamblerDeployPromptAction(f, CheckSpellbooksAction(PreMainAction(e))).as(DimensionalShamblerUnit.styled(f), "to Map")("(always prompted)")
                    else {
                        val dsReason = dsDeployReason(f)
                        if (dsReason.any)
                            + ShamblerDeployPromptAction(f, CheckSpellbooksAction(PreMainAction(e))).as(DimensionalShamblerUnit.styled(f), "to Map")("(" + dsReason.get + ")")
                    }
                }

                |(asking.ask).%(_.actions.%!(_.isInfo).any)./(_.add(NeedOk).add(OutOfTurnRefresh(PreMainAction(e))).add(SacrificeHighPriestAllowedAction).group(" ").skip(PreActionPromptsAction(e, l.but(f))))
            }

            // Also check active player's own DS (not in enemy loop)
            val selfDsAsk = {
                implicit val asking = Asking(e)

                + GroupAction("After " + e + " action")

                if (e.loyaltyCards.has(DimensionalShamblerCard) && e.at(ShamblerHold(e), DimensionalShamblerUnit).any) {
                    if (e.commands.has(ShamblerPrompt))
                        + ShamblerDeployPromptAction(e, CheckSpellbooksAction(PreMainAction(e))).as(DimensionalShamblerUnit.styled(e), "to Map")("(always prompted)")
                    else {
                        val dsReason = dsDeployReason(e)
                        if (dsReason.any)
                            + ShamblerDeployPromptAction(e, CheckSpellbooksAction(PreMainAction(e))).as(DimensionalShamblerUnit.styled(e), "to Map")("(" + dsReason.get + ")")
                    }
                }

                |(asking.ask).%(_.actions.%!(_.isInfo).any)./(_.add(NeedOk).add(OutOfTurnRefresh(PreMainAction(e))).add(SacrificeHighPriestAllowedAction).group(" ").skip(MainGatesAction(e)))
            }

            val allAsks = asks ++ selfDsAsk

            if (allAsks.any)
                MultiAsk(allAsks)
            else
                Then(MainGatesAction(e))

        case PreMainAction(f) if f.active.not =>
            MainAction(f)

        case PreMainAction(f) =>
            MainGatesAction(f)

        case MainGatesAction(f) =>
            checkGatesGained(f)

            CheckSpellbooksAction(MainAction(f))

        case MainAction(f) if f.active.not =>
            implicit val asking = Asking(f)

            game.reveals(f)

            + NextPlayerAction(f).as("Skip")

            asking

        case MainAction(f) if f.acted =>
            implicit val asking = Asking(f)

            game.controls(f)

            if (f.hasAllSB)
                game.battles(f)

            game.reveals(f)

            game.endTurn(f)(true)

            asking

        case EndAction(self) =>
            self.acted = true
            mindParasiteCaptureRejected = $
            elderThingBlockGuard = $
            AfterAction(self)

        case AfterAction(self) =>
            checkGatesGained(self)

            if (self.power == 0) {
                self.log("ran out of power")
                // Gla'aki IGOO SBR: track first faction to reach 0 power during action phase
                if (reachedZeroPowerFirst.none)
                    reachedZeroPowerFirst = |(self)
            }

            if (self.power == -1)
                self.power = 0

            if (self.power < 0) {
                self.log("somehow ran into negative power")

                self.power = 0
            }

            expansions.foreach(_.afterAction())

            factions.foreach(_.oncePerAction = $)

            // Atlach-Nacha: Cosmic Web — immediate victory
            factions.find(f => f.upgrades.has(CosmicWeb)).foreach { winner =>
                log(winner.full, "won the game via", "Cosmic Web".styled("nt"))
                return GameOver($(winner))
            }

            // Two-pass CG: fire pending CG sources AFTER triggers/SBRs resolved
            if (fbCyclopeanGazePendingSources.any && fbCyclopeanGazePendingActor.isDefined) {
                val sources = fbCyclopeanGazePendingSources
                val actor = fbCyclopeanGazePendingActor.get
                fbCyclopeanGazePendingSources = $
                fbCyclopeanGazePendingActor = None
                Force(FBCyclopeanGazePhaseAction(FB, actor, sources, fromBattle = false))
            }
            else {
                // Brown Jenkin Familiar: forced respawn for any faction with BJ in pool + 2 Power
                val bjFaction = factions.find(f => f.loyaltyCards.has(BrownJenkinCard) && f.allInPlay.%(_.uclass == BrownJenkin).none && f.pool(BrownJenkin).any && f.power >= 2 && f.allGates.onMap.any)
                bjFaction match {
                    case Some(f) => BrownJenkinFamiliarCheckAction(f, CheckSpellbooksAction(PreMainAction(self)))
                    case None => CheckSpellbooksAction(PreMainAction(self))
                }
            }

        case EndTurnAction(f) =>
            f.acted = true

            NextPlayerAction(f)

        case NextPlayerAction(_) if queue.any =>
            ProceedBattlesAction

        case NextPlayerAction(_) if factions.%(_.doom >= 30).any =>
            CheckSpellbooksAction(GameOverPhaseAction)

        case NextPlayerAction(prev) =>
            factions.foreach(_.acted = false)
            factions.foreach(_.battled = $)
            factions.foreach(_.oncePerRound = $)

            round += 1

            factions.foreach { f =>
                f.active = f.power > 0 && f.hibernating.not
            }

            factions = factions.drop(1) ++ factions.take(1)

            val next = factions.first

            if (factions.exists(_.active)) {
                if (next.active)
                    log(CthulhuWarsSolo.DottedLine)

                CheckSpellbooksAction(PreMainAction(next))
            }
            else
                CheckSpellbooksAction(DragonAscendingInstantAction(DragonAscendingUpAction("power gather", EndPhasePromptsAction(next, factions))))

        case EndPhasePromptsAction(next, Nil) =>
            Then(PowerGatherAction(next))

        case EndPhasePromptsAction(next, l) =>
            endActionPhasePrompts = true
            val asks = l./~{ f =>
                implicit val asking = Asking(f)

                + GroupAction("Before the end of Action Phase")

                if (f.onMap(HighPriest).any)
                    if (f.commands.has(UnspeakableOathOpportunityEndOfPhase) || f.commands.has(UnspeakableOathPrompt))
                        + SacrificeHighPriestPromptAction(f, PreMainAction(next)).as("Sacrifice", HighPriest.styled(f))("Unspeakable Oath".hl)

                if (f.loyaltyCards.has(DimensionalShamblerCard) && f.at(ShamblerHold(f), DimensionalShamblerUnit).any)
                    if (f.commands.has(ShamblerOpportunityEndOfPhase) || f.commands.has(ShamblerPrompt))
                        + ShamblerDeployPromptAction(f, CheckSpellbooksAction(PreMainAction(next))).as(DimensionalShamblerUnit.styled(f), "to Map")("End of Action Phase")

                |(asking.ask).%(_.actions.%!(_.isInfo).any)./(_.add(NeedOk).add(OutOfTurnRefresh(EndPhasePromptsAction(next, l))).add(SacrificeHighPriestAllowedAction).group(" ").skip(EndPhasePromptsAction(next, l.but(f))))
            }

            if (asks.any)
                MultiAsk(asks)
            else
                Then(PowerGatherAction(next))


        // PASS
        case PassAction(self) =>
            val p = self.power

            self.power = -1

            self.log("passed and forfeited", p.power)

            EndAction(self)

        // CONTROL
        case AdjustGateControlAction(f, changed, then) if !inActionPhase =>
            // Bug fix: outside the action phase (Doom Phase, Gather Power, or
            // end-of-action-phase prompt window where endActionPhasePrompts=true),
            // the gate-control menu should be a no-op pass-through. Returning
            // UnknownContinue when changed=false caused a hard crash
            // ("unknown continue on AdjustGateControlAction(...,false,MainAction(...))")
            // when this option was reached after Action Phase wound down (e.g.
            // pending in continue queue or re-entered via OutOfTurnRefresh after
            // Tombstalker's Shepherd of the Crypt resolved during the gather→doom
            // transition). Match sibling cases ControlGateAction / AbandonGateAction
            // (lines below) which both Force(then) unconditionally outside action phase.
            Force(then)

        case AdjustGateControlAction(f, changed, then) =>
            // Library at Celaeno: custodian/librarian block control of uncontrolled gates
            val libraryBlockedRegions : $[Region] = if (board.isLibraryMap) $(custodianRegion, librarianRegion).flatten else $()
            // Determine which neutral unit blocks each region (for abandon warning)
            def libraryBlockerName(r : Region) : |[String] = if (board.isLibraryMap) {
                if (librarianRegion.has(r)) |("Librarian")
                else if (custodianRegion.has(r)) |("Custodian")
                else None
            } else None

            Ask(f)
                .some(areas.%(r => f.gates.has(r) || (abandonedGates.has(r) && (DS.chaosGateRegions.has(r).not || f == DS) && !libraryBlockedRegions.has(r)))) { r =>
                    val blocked = libraryBlockedRegions.has(r)
                    val l = f.at(r).%(_.canControlGate).sortBy(_.onGate.not).distinctBy(_.uclass)
                    val g = $[Any](f.gates.has(r).not.?("Abandoned gate").|("Gate"), "in", r)

                    // Don't show ControlGateAction if custodian/librarian blocks this region
                    val controlOptions = if (blocked) l./(u => Info(u.ref.full)(g : _*))
                    else l./(u =>
                        if (l.%(_.onGate).single./(_.uclass).has(u.uclass))
                            Info(u.ref.full)(g : _*)
                        else
                        if (l.%(_.onGate).single./(_.uclass).has(Acolyte) && u.uclass == HighPriest && f.commands.has(HighPriestGatesSkip))
                            Info(u.ref.full)(g : _*)
                        else
                            ControlGateAction(f, r, u, then).as(u.ref.full)(g : _*)
                    )

                    // Abandon option: add warning if custodian/librarian would block re-control
                    val abandonLabel = libraryBlockerName(r) match {
                        case Some(name) => "Abandon - " + name.styled("lb") + " blocks taking control again"
                        case None => "Abandon"
                    }
                    controlOptions ++
                    (f.gates.has(r) && f.clings.not && f.commands.has(GateDiplomacySkipAbandon).not).$(AbandonGateAction(f, r, then).as(abandonLabel)(""))
                }
                .useIf(_ => inActionPhase)(_.add(OutOfTurnRefresh(AdjustGateControlAction(f, changed, then))))
                .group(" ")
                .doneIf(changed)(then)
                .cancelIf(changed.not)

        case ControlGateAction(f, r, u, then) if !inActionPhase =>
            Force(then)

        case ControlGateAction(f, r, u, then) if u.onGate =>
            Force(AdjustGateControlAction(f, true, then))

        case ControlGateAction(f, r, u, then) =>
            f.at(r).foreach(_.onGate = false)
            u.onGate = true

            if (f.gates.has(r).not) {
                f.gates :+= r
                f.abandoned :-= r

                f.log("took control of the gate in", r, "with", u)
            }
            else
                f.log("changed control of the gate in", r, "to", u)

            // Library at Celaeno: immediate tome acquisition when controlling a gate in a tome region
            if (board.isLibraryMap)
                LibraryExpansion.checkTomeAcquisition()

            // Gate occupy/abandon are unlimited free actions (place/remove
            // cultist on gate). They do NOT trigger CG — no unit movement occurs.
            Force(AdjustGateControlAction(f, true, then))

        case AbandonGateAction(f, r, then) if !inActionPhase =>
            Force(then)

        case AbandonGateAction(f, r, then) =>
            f.at(r).foreach(_.onGate = false)

            f.gates :-= r
            f.abandoned :+= r

            f.log("abandoned gate in", r)

            Force(AdjustGateControlAction(f, true, then))

        // MOVE
        case MoveMainAction(self) =>
            MoveContinueAction(self, false)

        case MoveContinueAction(self, moved) =>
            if (self.power == 0)
                Then(MoveDoneAction(self))
            else {
                // Fix 52 (v2.4.19): see `moves` above — any faction with units on the
                // Moon may enumerate them for departure, not just BB. The TO-Moon Fix 45
                // block in MoveAction prevents non-BB units from voluntarily entering.
                val moonUnits = self.at(BB.moon).not(Moved).%(_.canMove)
                val ownUnits = (self.units.nex.onMap.not(Moved).%(_.canMove) ++ moonUnits).sortA
                // Mind Parasite: include parasitized enemy acolytes that this faction controls
                val parasitized : $[UnitFigure] = if (self.loyaltyCards.has(InsectsFromShaggaiCard))
                    factions.but(self)./~(e => e.units.nex.onMap.not(Moved).%(u => MindParasite.controller(u)(game).has(self)))
                else $
                val units = ownUnits ++ parasitized

                if (units.none)
                    Then(MoveDoneAction(self))
                else
                if (moved)
                    Ask(self)
                        .group("Moved", self.units.tag(Moved).mkString(", "))
                        .add(MoveDoneAction(self).as("Done"))
                        .each(units)(u => MoveSelectAction(u.faction, u, u.region, 1).as(u.ref.full, "from", u.region)("Move", "another".hl, "unit"))
                else
                    Ask(self)
                        .group("Move unit")
                        .each(units)(u => MoveSelectAction(u.faction, u, u.region, 1).as(u.ref.full, "from", u.region))
                        .cancel
            }

        case MoveSelectAction(self, u, from, cost) =>
            var destinations = board.connected(from)

            if (self.has(Flight))
                destinations = areas.but(from).intersect(destinations ++ destinations./~(board.connected))

            if (u.uclass == Shantak)
                destinations = areas.but(from)

            // BB Fix 82, v2.4.31 — Only BB-owned Shantak may go to the Moon.
            // Shantak's "anywhere" reach must exclude Moon for non-BB factions.
            if (u.uclass == Shantak && self != BB)
                destinations = destinations.but(BB.moon)

            // Fix 52 (v2.4.19): the Moon is adjacent to every region per the user's
            // explicit Moon-Tile semantics ("the moon is adjacent to all regions").
            // ANY unit on the Moon may depart to any map area — there is no legal
            // way for a non-BB unit to be on the Moon other than BB's Catnapping
            // action (Fix 45 enforces that), so allowing departure for all factions
            // simply unblocks catnapped units that would otherwise be stranded with
            // zero legal moves. BB units additionally may move TO the Moon; non-BB
            // units cannot voluntarily enter the Moon (Fix 45 TO-Moon block in
            // MoveAction below stays untouched). Apologies for the prior gap.
            if (from == BB.moon)
                destinations = areas
            else if (self == BB)
                destinations = destinations :+ BB.moon

            val arriving = self.units.%(_.region.glyph.onMap).tag(Moved)./(_.region).distinct

            val l1 = destinations.%(arriving.contains) ++ destinations.%!(arriving.contains)

            val isMoonMove = from == BB.moon || destinations.has(BB.moon)
            val l2 = isMoonMove.?(destinations).|( destinations.sortBy(to => direction(from, to)) )

            Ask(self)
                .each(l2.%(self.affords(cost)))(to => MoveAction(self, u, from, to, cost).as
                    (ConnectionGlyph(from, to) + to.toString, self.iced(to),
                     (isMoonMove || to == BB.moon).?("").|( s"""<img class=direction src="${Overlays.imageSource("move-deg-" + direction(from, to))}" />""" ))
                    ("Move", u, (cost == 0).??("for free"), "from", from, "to")
                )
                .cancelIf(cost > 0)
                .skipIf(cost == 0)(MoveContinueAction(self, true))

        case MoveDoneAction(self) =>
            if (self.can(Burrow) && self.units.%(u => u.tag(Moved))./(u => 1 - u.count(MovedForFree) + u.count(MovedForExtra)).sum > 1) {
                self.power += 1
                self.log("recovered", 1.power, "from", Burrow)
            }

            self.units.foreach(_.remove(Moved))
            self.units.foreach(_.remove(MovedForFree))
            self.units.foreach(_.remove(MovedForExtra))

            EndAction(self)

        // Bubastis Moon Guard (Fix 45, v2.4.13): block any non-BB unit move
        // whose destination is the Moon. MoveSelectAction already filters
        // Moon out of destination lists for non-BB factions, but this is a
        // belt-and-suspender check at the action-execution layer so any
        // future spell/ability that constructs a MoveAction directly cannot
        // sneak a non-BB unit onto the Moon. Catnapping is the sole way for
        // a non-BB unit to enter the Moon; Catnapping mutates u.region
        // directly in FactionBB.scala, not via MoveAction.
        case MoveAction(self, u, o, r, cost) if self != BB && r == BB.moon =>
            self.log("move of", u, "to", BB.moon, "blocked: only BB units may enter the Moon (Catnapping is the sole exception)")
            EndAction(self)

        case MoveAction(self, u, o, r, cost) =>
            val t = self.payTax(r)

            if (cost > 0)
                self.power -= cost

            u.region = r
            u.add(Moved)
            u.onGate = false

            if (cost + t > 1)
                u.add((cost + t - 1).times(MovedForExtra))

            if (cost + t == 0)
                u.add(MovedForFree)

            // Catnapping refund: if a non-BB unit moves OFF the Moon and its owner paid Power,
            // BB gains that same amount of Power.
            if (factions.has(BB) && BB.can(Catnapping) && self != BB && o == BB.moon && cost > 0) {
                BB.power += cost
                BB.log(Catnapping.styled(BB) + ": gained", cost.power, "from", self.full, "moving off", BB.moon)
            }

            // Track Ghato paid moves for anti-ping-pong
            if (self == FB && u.uclass == Ghatanothoa && cost > 0)
                fbGhatoLastMoveOrigin = Some(o)

            self.log("moved", u, "from", o, "to", r)

            logElderThingMovementBlocks(u, r)

            // Universal CG trigger: any non-FB unit moving into a region with
            // an FB Revenant or Ghatanothoa must fire CG. The snapshot-delta
            // path in FBExpansion can miss sub-chain moves (multi-move actions,
            // unlimited BG Dark Young relocations, etc.); record the destination
            // region unconditionally so FB's AfterAction picks it up via the
            // fbCyclopeanGazeActionRegions list.
            if (self != FB && u.uclass.utype != Building && fbHasCGActive &&
                FB.units.exists(uf => uf.region == r && (uf.uclass == RevenantOfKnaa || uf.uclass == Ghatanothoa)))
                fbCyclopeanGazeActionRegions :+= r

            MovedAction(self, u, o, r)

        case MovedAction(self, u, o, r) =>
            MoveContinueAction(self, true)

        // ATTACK
        case AttackMainAction(f, l, effect) =>
            val ee = factionlike.but(f)
            val ll = l.sortBy(r => ee.%(_.present(r))./(e => f.strength(f.at(r), e)).maxOr(0)).reverse

            val variants = ll./~(r => ee.%(_.present(r)).%(f.canAttack(r)).sortBy(e => -e.strength(e.at(r), f))./(e => r -> e))

            Ask(f)
                .each(variants)((r, e) =>
                    if (board.isLibraryMap && tomeHolders.get(TomeBarrier).flatten.has(e) && !tomeOverdue.getOrElse(TomeBarrier, false))
                        BarrierMenuOpenAction(f, r, e, effect)
                    else
                        AttackAction(f, r, e, effect))
                .group(" ")
                .cancelIf(effect.none)
                .cancelIf(effect.has(EnergyNexus))
                .skipIf(effect.has(FromBelow))(ProceedBattlesAction)

        case AttackAction(self, r, f, effect) =>
            // Library at Celaeno: safety-net check (BarrierMenuOpenAction is the normal
            // entry from the picker; this catches any caller bypassing the picker).
            if (board.isLibraryMap && !barrierPaid && tomeHolders.get(TomeBarrier).flatten.has(f) && !tomeOverdue.getOrElse(TomeBarrier, false)) {
                barrierPaid = true
                return Force(BarrierCheckAction(self, f, AttackAction(self, r, f, effect)))
            }
            barrierPaid = false

            // Round 8 Bug 60: snapshot FB.power BEFORE the 1-power attack cost is deducted,
            // so Ghatanothoa's combat strength uses the pre-battle power (not post-cost).
            // The original snapshot was in ProceedBattlesAction (line ~2570), but that runs
            // AFTER this handler — by which time `self.power -= 1` has already executed.
            // For FB-attacker: snapshot here is pre-deduct (correct). For non-FB attacker:
            // FB.power isn't being changed by this action, so snapshot is also correct.
            if (factions.has(FB))
                fbPowerAtBattleStart = FB.power

            if (effect.has(FromBelow).not)
                self.power -= 1

            self.payTax(r)
            self.log("battled", f, "in", r, effect./("with " + _).|(""))

            if (effect.has(FromBelow))
                game.queue = game.queue.take(1) ++ $(new Battle(r, self, f, effect)) ++ game.queue.drop(1)
            else
                game.queue = game.queue.take(0) ++ $(new Battle(r, self, f, effect)) ++ game.queue.drop(0)

            // Round 8 Bug 40: also check facedown state for IGOO spellbooks
            if (effect.has(FromBelow).not && self.has(FromBelow) && !self.oncePerGame.has(FromBelow) && self.at(r)(Nyogtha).any && !self.all(Nyogtha).exists(u => ElderThingMindControl.suppresses(u))) {
                val l = self.all(Nyogtha)./(_.region).but(r).diff(self.battled)
                if (l.any)
                    return Force(AttackMainAction(self, l, |(FromBelow)))
            }

            ProceedBattlesAction

        case ProceedBattlesAction =>
            factions.%(f => game.nexed.none && f.can(EnergyNexus) && queue.exists(b => f.at(b.arena)(Wizard).any) && f.acted.not).foreach { f =>
                game.nexed = queue.%(_.attacker == queue.first.attacker)./(_.arena).%(r => f.at(r)(Wizard).any)
                f.log("interrupted battle", queue.exists(_.effect.has(EnergyNexus)).??("again"), "with", EnergyNexus)
                return Force(PreMainAction(f))
            }

            battle = queue.starting

            // Round 8 Bug 60: removed the fbPowerAtBattleStart snapshot here. It used to
            // capture FB.power at battle proceed time, but that's AFTER AttackAction deducted
            // the 1 power for the attack. The snapshot is now taken in AttackAction BEFORE
            // the deduct (line ~2537), giving the correct pre-battle value.

            queue = queue.drop(1)

            if (game.nexed.any) {
                game.nexed = $

                battle.get.attacker.log("proceeded to battle", battle.get.defender, "in", battle.get.arena)

                // Energy Nexus PB: resume at PreRoll (skip pre-battle to avoid double Devour)
                if (game.battleResumePhase.any) {
                    game.battleResumePhase = None
                    battle.get.jump(PreRoll)
                } else {
                    battle.get.proceed()
                }
            } else {
                battle.get.proceed()
            }

        // CAPTURE
        case CaptureMainAction(self, l, effect) =>
            val variants = l./~ { r =>
                self.enemies.%(self.canCapture(r))./ { f =>
                    CaptureAction(self, r, f, effect).as(f.at(r).cultists./(_.uclass).distinct.single./(_.name).|(Cultist.name).styled(f))("Capture", for1PowerWithTax(r, self), "in", r, self.iced(r))
                }
            }

            Ask(self)
                .list(variants)
                .group(" ")
                .cancelIf(effect.none)
                .skipIf(effect.has(FromBelow))(EndAction(self))

        case CaptureAction(self, r, f, effect) =>
            if (effect.has(FromBelow).not)
                self.power -= 1

            if (effect.has(FromBelow).not || self.all(Nyogtha)./(_.region).but(r).any)
                self.payTax(r)

            // MindParasiteCultist: insect owner and original faction can't use normal capture (separate flow for insect owner)
            val l = f.at(r).cultists.%(u => u.uclass != MindParasiteCultist || (self != f && !mindParasiteOriginalFaction.get(u.ref).has(self))).sortBy(u => u.uclass.cost * 10 + u.onGate.??(5))
            if (l.none) {
                EndAction(self)
            } else {
                val ll = f.clings.?(l.take(1)).|(l)
                Ask(f).each(ll)(u => CaptureTargetAction(f, r, self, u, effect).as(u.ref.full)(self, "captures in", r))
            }

        case CaptureTargetAction(self, r, f, u, effect) =>
            val unitFig = unit(u)
            // Great Race of Yith: detect Possession-overridden captures
            val yithPresent = f.at(r, GreatRaceOfYith).any
            // [2026-05-23] Possession overrides normal blockers (enemy GOO/Terror/Monster)
            // Enemy faction here is the unit's owner (not necessarily self)
            val victimOwner = unitFig.faction
            val overrideUsed = yithPresent && (victimOwner.at(r, GOO).any || victimOwner.at(r, Terror).any || victimOwner.at(r, Monster).any)
            // If MPC captured by third faction, unparasitize first — restore to original faction, then capture that acolyte
            if (unitFig.uclass == MindParasiteCultist) {
                val origFac = mindParasiteOriginalFaction.get(u)
                MindParasite.unparasitize(unitFig)
                val restored = origFac./~(of => of.at(r, Acolyte).lastOption)
                if (restored.any) {
                    val victim = restored.get
                    eliminate(victim)
                    victim.region = f.prison
                    f.log("captured", victim, "in", r, effect./("with " + _).|(""))
                } else {
                    f.log("captured an Acolyte in", r, effect./("with " + _).|(""))
                }
            } else {
                eliminate(unitFig)
                unitFig.region = f.prison
                f.log("captured", unitFig, "in", r, effect./("with " + _).|(""))
            }

            if (yithPresent) {
                if (overrideUsed)
                    f.log("Possession".styled("nt") + ":", GreatRaceOfYith.styled(f), "captured in", r, "(overrode normal capture restrictions)")
                else
                    f.log("Possession".styled("nt") + ":", GreatRaceOfYith.styled(f), "captured in", r)
            }

            f.satisfy(CaptureCultist, "Capture Cultist")

            if (factions.has(FB))
                fbCyclopeanGazeActionRegions :+= r

            if (effect.has(FromBelow).not)
                f.all(Nyogtha)./(_.region).diff($(r)).foreach { x =>
                    return Force(CaptureMainAction(f, $(x), |(FromBelow)))
                }

            EndAction(f)

        // Mind Parasite: separate capture flow — single menu with region-grouped targets
        case MindParasiteCaptureMainAction(self) =>
            val mpTargets = self.units.%(_.uclass == MindParasiteCultist).%(_.region.onMap).%(u => !mindParasiteCaptureRejected.has(u.ref))
            val variants = mpTargets./(_.region).distinct./~ { r =>
                self.at(r).%(_.uclass == MindParasiteCultist).%(u => !mindParasiteCaptureRejected.has(u.ref))./(u =>
                    MindParasiteCaptureTargetAction(self, r, u.ref).as(u.ref.full)("Capture", for1PowerWithTax(r, self), "in", r, self.iced(r))
                )
            }
            Ask(self)
                .list(variants)
                .group(" ")
                .cancel

        case MindParasiteCaptureTargetAction(self, r, ur) =>
            self.power -= 1
            self.payTax(r)
            val unitFig = unit(ur)
            val origFac = mindParasiteOriginalFaction.get(ur)
            if (origFac.any) {
                Ask(origFac.get)
                    .add(MindParasiteBlockCaptureAction(origFac.get, self, r, ur))
                    .add(MindParasiteAllowCaptureAction(origFac.get, self, r, ur))
            } else {
                eliminate(unitFig)
                unitFig.region = self.prison
                self.log("captured", unitFig, "in", r)
                self.satisfy(CaptureCultist, "Capture Cultist")
                EndAction(self)
            }

        // Mind Parasite: original faction blocks capture — refund power, back to main menu
        case MindParasiteBlockCaptureAction(self, captor, r, ur) =>
            self.log("did not allow their mind parasited Acolyte to be captured in", r)
            captor.power += 1
            mindParasiteCaptureRejected :+= ur
            Force(MainAction(captor))

        // Mind Parasite: original faction allows capture
        case MindParasiteAllowCaptureAction(self, captor, r, ur) =>
            self.log("allowed their mind parasited Acolyte to be captured in", r)
            val unitFig = unit(ur)
            MindParasite.unparasitize(unitFig)
            val restored = self.at(r, Acolyte)
            if (restored.any) {
                val victim = restored.last
                eliminate(victim)
                victim.region = captor.prison
                captor.log("captured", victim, "in", r)
                captor.satisfy(CaptureCultist, "Capture Cultist")
                if (factions.has(FB))
                    fbCyclopeanGazeActionRegions :+= r
            }
            EndAction(captor)

        // BUILD
        case BuildGateMainAction(self, locations) =>
            Ask(self).each(locations.sortBy(self.taxIn))(r => BuildGateAction(self, r)).cancel

        case BuildGateAction(self, r) if r != BB.moon =>
            self.power -= 3 - self.has(UmrAtTawil).??(1)
            self.payTax(r)
            gates :+= r
            self.oncePerAction :+= UmrAtTawil
            self.log("built a gate in", r)
            // Firstborn (FB): Crater building ability -- any gate built in a Crater region is immediately destroyed
            if (factions.has(FB) && fbCraters.has(r)) {
                FBExpansion.checkCraterDestroysGate(r)
            }
            // Round 8 Bug 54: building a gate is a zero-delta action (no unit movement),
            // so it doesn't trigger CG via the snapshot delta. Register it as a CG edge case.
            if (factions.has(FB))
                fbCyclopeanGazeActionRegions :+= r
            EndAction(self)

        // RECRUIT
        case RecruitMainAction(self, uc, l) =>
            val (a, b) = l.partition(abandonedGates.contains)
            Ask(self).each(a ++ b)(r => RecruitAction(self, uc, r)).cancel

        case RecruitAction(self, uc, r) =>
            val cost = self.recruitCost(uc, r)
            self.power -= cost
            self.payTax(r)
            // Bloated Woman: check if unit is on a VelvetFanHold — pay BW owner instead
            val onCard = self.units.%(u => u.uclass == uc && u.region.is[VelvetFanHold])
            if (onCard.any) {
                val u = onCard.head
                val bwOwner = u.region.asInstanceOf[VelvetFanHold].faction
                u.region = r
                // Card: "no one will be paid Power for this while she is out of play"
                val bwInPlay = bwOwner.allInPlay.%(_.uclass == BloatedWoman).any
                if (bwInPlay) {
                    bwOwner.power += cost
                    self.log("recruited", uc.styled(self), "in", r, "from", "Velvet Fan".styled("nt"), "— paid", cost.power, "to", bwOwner.full)
                } else {
                    self.log("recruited", uc.styled(self), "in", r, "from", "Velvet Fan".styled("nt"), "— Bloated Woman out of play, no payment")
                }
            } else {
                self.place(uc, r)
                self.log("recruited", uc.styled(self), "in", r)
            }

            if (uc === HighPriest)
                initHighPriestPlans(self)

            EndAction(self)

        // SUMMON
        case SummonMainAction(self, uc, l) =>
            // Servitor of the Outer Gods: block non-terror monster summons inside this sub-menu
            val servitorBlocking = self.loyaltyCards.has(ServitorCard) && self.pool(ServitorUnit).any
            if (servitorBlocking && uc.utype == Monster && uc != ServitorUnit)
                Ask(self).add(GroupAction(uc.name + " summon blocked by " + "Servitor of the Outer Gods".styled("nt"))).cancel
            else
                Ask(self).each(l)(r => SummonAction(self, uc, r)).cancel

        case SummonAction(self, uc, r) =>
            // Bloated Woman: units on VelvetFanHold count as available for summoning
            val hasVelvetFanUnit = self.units.%(u => u.uclass == uc && u.region.is[VelvetFanHold]).any
            if ((self.pool(uc).none && !hasVelvetFanUnit) || self.affords(self.summonCost(uc, r))(r).not)
                EndAction(self)
            else {
                val cost = self.summonCost(uc, r)
                self.power -= cost
                self.payTax(r)
                // Bloated Woman: check if unit is on a VelvetFanHold — pay BW owner instead
                val onCard = self.units.%(u => u.uclass == uc && u.region.is[VelvetFanHold])
                if (onCard.any) {
                    val u = onCard.head
                    val bwOwner = u.region.asInstanceOf[VelvetFanHold].faction
                    u.region = r
                    // Card: "no one will be paid Power for this while she is out of play"
                    val bwInPlay = bwOwner.allInPlay.%(_.uclass == BloatedWoman).any
                    if (bwInPlay) {
                        bwOwner.power += cost
                        self.log("summoned", uc.styled(self), "in", r, "from", "Velvet Fan".styled("nt"), "— paid", cost.power, "to", bwOwner.full)
                    } else {
                        self.log("summoned", uc.styled(self), "in", r, "from", "Velvet Fan".styled("nt"), "— Bloated Woman out of play, no payment")
                    }
                } else {
                    self.place(uc, r)
                    self.log("summoned", uc.styled(self), "in", r)
                }

                // Universal CG trigger: non-FB summon into a gaze region.
                if (self != FB && uc.utype != Building && fbHasCGActive &&
                    FB.units.exists(uf => uf.region == r && (uf.uclass == RevenantOfKnaa || uf.uclass == Ghatanothoa)))
                    fbCyclopeanGazeActionRegions :+= r

                SummonedAction(self, uc, r, $)
            }

        case SummonedAction(self, uc, r, l) =>
            EndAction(self)

        // AWAKEN
        case AwakenMainAction(self, uc, locations) =>
            Ask(self).some(locations)(r => self.awakenCost(uc, r)./(cost => AwakenAction(self, uc, r, cost))).cancel

        case AwakenAction(self, uc, r, cost) =>
            if (self.pool(uc).none || self.affords(cost)(r).not)
                EndAction(self)
            else {
                self.power -= cost

                self.payTax(r)
                self.place(uc, r)

                self.log("awakened", uc.styled(self), "in", r)

                AwakenedAction(self, uc, r, cost)
            }

        // HIGH PRIESTS
        case SacrificeHighPriestDoomAction(self) =>
            Ask(self).each(self.all(HighPriest))(u => SacrificeHighPriestAction(self, u.region, DoomAction(self))).cancel

        case SacrificeHighPriestMainAction(self) =>
            Ask(self).each(self.all(HighPriest))(u => SacrificeHighPriestAction(self, u.region, PreMainAction(self))).cancel

        case SacrificeHighPriestPromptAction(self, then) =>
            Ask(self).each(self.all(HighPriest))(u => SacrificeHighPriestAction(self, u.region, then)).cancel

        case SacrificeHighPriestOutOfTurnMainAction(self) =>
            Ask(self).each(self.all(HighPriest))(u => SacrificeHighPriestAction(self, u.region, OutOfTurnReturn)).cancel

        case SacrificeHighPriestAction(self, r, then) =>
            // Bug fix: only grant the +2 Power Unspeakable Oath bonus when the High Priest is alive on the
            // map at the time of sacrifice. Battle death, capture, or any other elimination route does NOT
            // trigger the Oath +2 — only the owning player's explicit choice to sacrifice an alive HP does.
            val candidates = self.at(r).%(_.uclass == HighPriest).%(_.health == Alive)
            if (candidates.none) {
                log("Unspeakable Oath".hl + ": no Alive High Priest in " + r + " — Oath not granted")
                if (then == OutOfTurnReturn) return then else return CheckSpellbooksAction(then)
            }
            val c = candidates.head

            eliminate(c)

            self.oncePerAction :-= Passion

            self.power += 2

            log(CthulhuWarsSolo.DottedLine)

            self.log("sacrificed", c, "in", r)

            if (self.hibernating.not)
                self.active = true

            triggers()

            checkGatesLost()

            if (then == OutOfTurnReturn)
                then
            else
                CheckSpellbooksAction(then)

        // COMMANDS
        case CommandsMainAction(f) =>
            // Safety catch for games saved before HP plans were initialized (any faction).
            // Also re-init plans if they were cleared when last HP was eliminated but commands were not (HP re-acquired via Hierophants).
            if (options.has(HighPriests) && f.onMap(HighPriest).any && (f.commands.of[UnspeakableOathPlan].none || f.plans.of[UnspeakableOathPlan].none))
                initHighPriestPlans(f)

            val visiblePlans = f.plans
                .%(p => p.is[ShamblerPlan].not || f.at(ShamblerHold(f), DimensionalShamblerUnit).any)
                .%(p => inActionPhase || (p.is[GateDiplomacyPlan].not && p.is[HighPriestGatesPlan].not))
            Ask(f)
                .each(visiblePlans) { p =>
                    if (f.commands.has(p))
                        CommandsRemoveAction(f, p)
                    else
                    if (p.requires.exists(_.forall(f.commands.has)))
                        CommandsAddAction(f, p)
                    else
                        CommandsInfoAction(f, p)
                }
                .group(" ")
                .cancel

        case CommandsAddAction(f, plan) =>
            plan.as[OneOfPlan]./(p => f.commands = f.commands.%!(_.as[OneOfPlan].?(_.group == p.group)))

            f.commands = f.commands.diff(plan.unfollowers)

            f.commands :+= plan

            def on() = plan.followers.intersect(f.plans).diff(f.commands).%(_.requires.exists(_.forall(f.commands.has)))

            while (on().any)
                f.commands ++= on()

            def off() = f.commands.%(_.requires.exists(_.forall(f.commands.has)).not)

            while (off().any)
                f.commands = f.commands.diff(off())

            // f.log("plan added", plan.selected)
            // if (plan.followers.any)
            //     f.log("plan added extra", plan.followers.mkString(" "))

            Then(OutOfTurnRepeat(f, CommandsMainAction(f)))

        case CommandsRemoveAction(f, plan) =>
            if (plan.is[OnlyOnPlan].not) {
                f.commands :-= plan

                def off() = f.commands.%(_.requires.exists(_.forall(f.commands.has)).not)

                while (off().any)
                    f.commands = f.commands.diff(off())
            }

            // f.log("plan removed", plan.unselected)

            Then(OutOfTurnRepeat(f, CommandsMainAction(f)))

        // OW Dragon Ascending defaults
        case DragonAscendingInstantAction(then) =>
            then

        case DragonAscendingUpAction(reason, then) =>
            then

        case DragonAscendingDownAction(f, reason, then) =>
            then

        // BATTLE
        case action if battle.any =>
            battle.get.perform(action)

        // Defensive: battle-context actions performed when game.battle is None.
        // Replay or out-of-order continuation can leave a PreBattleQuestion-typed
        // action queued after the battle that owned it has already cleared. Logging
        // it and continuing is safer than an uncaught MatchError that kills the
        // whole engine on a fresh page load. Real bug remains: figure out WHY the
        // battle was cleared before the continuation ran, but at least the
        // remaining log replays.
        case action : PreBattleQuestion =>
            log("[warn] battle action " + action.getClass.getSimpleName + " skipped — no active battle")
            UnknownContinue
    }

}
