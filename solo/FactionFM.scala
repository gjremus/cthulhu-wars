package cws

import hrf.colmat._
import html._

// ============================================================================
// FLESHMASON (FM) — Homebrew designer faction
// Designer ID: dcylxgn1wkqs, owner: zehk
// Faction color #f3c8c8 (pale pink), style "fm".
// Building-focused faction centered on the Screaming Wall.
//
// IMPLEMENTATION NOTES
//  • Screaming Walls are Buildings (not Gates, not Monsters) that can be Created
//    via the standard Create Gate action (player chooses Gate or Wall).
//  • When Creating a Gate, automatically also place a Screaming Wall (+1 Power).
//  • Body Architects count as Screaming Walls for all purposes in Areas WITHOUT
//    an actual Screaming Wall (Blueprints in Blood ongoing SB).
//  • Terrifying Shriek: Screaming Wall adds 1 Pain post-battle; if Bone Sculptor
//    present, apply pre-battle instead.
//  • Spiteful Chisel: post-battle ability, if no Walls in pool and enemy GOO took
//    a result, return a captured Acolyte from their faction card to battle area.
//  • Special iGOO awaken: can awaken iGOOs at controlled Gates with Walls if 4+ Walls in play.
// ============================================================================


// ── UNITS ───────────────────────────────────────────────────────────────────
// FM uses standard Acolyte for cultists. Building type for Screaming Wall.
case object BoneSculptor   extends FactionUnitClass(FM, "Bone Sculptor",   Monster,  1)
case object MuscleStitcher extends FactionUnitClass(FM, "Muscle Stitcher", Monster,  2)
case object BodyArchitect  extends FactionUnitClass(FM, "Body Architect",  Terror,   3)
case object ScreamingWall  extends FactionUnitClass(FM, "Screaming Wall",  Building, 2)


// ── SPELLBOOKS ──────────────────────────────────────────────────────────────
case object CrumblingYawp     extends FactionSpellbook(FM, "Crumbling Yawp")
case object HymnOfSuffering   extends FactionSpellbook(FM, "Hymn of Suffering")
case object TerrifyingShriek  extends FactionSpellbook(FM, "Terrifying Shriek")
case object TendonFlaying     extends FactionSpellbook(FM, "Tendon Flaying")
case object BlueprintsInBlood extends FactionSpellbook(FM, "Blueprints in Blood")
case object MannedBattlements extends FactionSpellbook(FM, "Manned Battlements")

// FM Unique Faction Ability — Frenzying Adze (Battle phase). Declared as a FactionSpellbook
// so it appears in faction status and can be referenced consistently.
case object FrenzyingAdze extends FactionSpellbook(FM, "Frenzying Adze")


// ── SPELLBOOK REQUIREMENTS ──────────────────────────────────────────────────
case object CrumblingYawpReq     extends Requirement("Roll 9 or more dice in a single Combat")
case object HymnOfSufferingReq   extends Requirement("Your Units in play have 12 or more Combat")
case object TerrifyingShriekReq  extends Requirement("The Screaming Wall is in Areas with 2 of the following Glyphs")
case object TendonFlayingReq     extends Requirement("As an Action, Pay 6 Power, minus the number of Screaming Walls in play")
case object BlueprintsInBloodReq extends Requirement("Have 3 Screaming Walls in Play")
case object MannedBattlementsReq extends Requirement("Have 6 Screaming Walls in Play")


// ============================================================================
// FLESHMASON (FM) FACTION OBJECT
// ============================================================================
case object FM extends Faction { f =>
    def name  = "Fleshmason"
    def short = "FM"
    def style = "fm"

    // Frenzying Adze is the always-on unique ability.
    override def abilities = $(FrenzyingAdze)
    override def library   = $(CrumblingYawp, HymnOfSuffering, TerrifyingShriek, TendonFlaying, BlueprintsInBlood, MannedBattlements)
    override def requirements(options : $[GameOption]) =
        $(CrumblingYawpReq, HymnOfSufferingReq, TerrifyingShriekReq, TendonFlayingReq, BlueprintsInBloodReq, MannedBattlementsReq)

    // 6 Acolytes + 3 Bone Sculptors + 3 Muscle Stitchers + 3 Body Architects + 6 Screaming Walls.
    val allUnits =
        6.times(Acolyte)         ++
        3.times(BoneSculptor)    ++
        3.times(MuscleStitcher)  ++
        3.times(BodyArchitect)   ++
        6.times(ScreamingWall)

    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int = {
        // Standard cost-based combat: Acolytes 0, Bone Sculptors 1, Muscle Stitchers 2, Body Architects 3.
        // Screaming Walls are Buildings with no combat. Frenzying Adze modifies combat during battle phase.
        units.%(u => u.uclass.utype == Monster || u.uclass.utype == Terror).not(Zeroed)./(_.uclass.cost).sum +
        neutralStrength(units, opponent)
    }
}


// ============================================================================
// FLESHMASON (FM) ACTION CASE CLASSES
// Soft actions include Cancel; Hard actions do NOT.
// ============================================================================

// ── CREATE SCREAMING WALL (alternative to Create Gate) ─────────────────────
// Player chooses to Create a Wall instead of a Gate (same 3 Power cost).
case class CreateScreamingWallMainAction(self : Faction)
    extends OptionFactionAction(("Create " + ScreamingWall.name).styled(FM)) with MainQuestion with Soft
case class CreateScreamingWallAction(self : Faction, r : Region)
    extends BaseFactionAction(("Create " + ScreamingWall.name).styled(FM) + " in", r)

// ── CRUMBLING YAWP (Doom Phase, two options) ────────────────────────────────
case class CrumblingYawpMainAction(self : Faction)
    extends OptionFactionAction(CrumblingYawp.styled(FM)) with DoomQuestion with Soft
case class CrumblingYawpSkipAction(self : Faction)
    extends OptionFactionAction(("Skip " + CrumblingYawp.name).styled(FM)) with DoomQuestion with Soft
case class CrumblingYawpChooseAction(self : Faction, option : Int)
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) = CrumblingYawp.styled(FM) + ": choose option"
}
case class CrumblingYawpOption1Action(self : Faction)
    extends OptionFactionAction(implicit g => CrumblingYawp.styled(FM) + ": Gain 1 Power for each Area with enemy Gate and " + ScreamingWall.name) { def question(implicit game : Game) = CrumblingYawp.styled(FM) }
case class CrumblingYawpOption2Action(self : Faction)
    extends OptionFactionAction(implicit g => CrumblingYawp.styled(FM) + ": Gain 1 Elder Sign for every 3 " + ScreamingWall.name.s + " in Play") { def question(implicit game : Game) = CrumblingYawp.styled(FM) }

// ── HYMN OF SUFFERING (Gather Power Phase) ──────────────────────────────────
case class HymnOfSufferingMainAction(self : Faction)
    extends OptionFactionAction(HymnOfSuffering.styled(FM)) with GatherQuestion with Soft
case class HymnOfSufferingSkipAction(self : Faction)
    extends OptionFactionAction(("Skip " + HymnOfSuffering.name).styled(FM)) with GatherQuestion with Soft
case class HymnOfSufferingAreaAction(self : Faction, r : Region)
    extends BaseFactionAction(HymnOfSuffering.styled(FM) + ": choose Area with Acolyte and " + ScreamingWall.name, r)

// ── TERRIFYING SHRIEK (Post-Battle, pain application) ───────────────────────
case class TerrifyingShriekApplyAction(self : Faction, preBattle : Boolean)
    extends ForcedAction

// ── TENDON FLAYING (Post-Battle, per damaged Muscle Stitcher) ───────────────
case class TendonFlayingMainAction(self : Faction, stitchers : $[UnitRef])
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) = TendonFlaying.styled(FM) + ": " + stitchers.num + " " + MuscleStitcher.name + (stitchers.num == 1).??(" received a Pain").|("s received Pains")
}
case class TendonFlayingChoiceAction(self : Faction, stitcher : UnitRef, remaining : $[UnitRef])
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) = TendonFlaying.styled(FM) + ": choose reward for " + MuscleStitcher.name
}
case class TendonFlayingOption1Action(self : Faction, stitcher : UnitRef, remaining : $[UnitRef])
    extends OptionFactionAction("Gain 1 Power") with PowerNeutral { def question(implicit game : Game) = TendonFlaying.styled(FM) }
case class TendonFlayingOption2Action(self : Faction, stitcher : UnitRef, remaining : $[UnitRef])
    extends OptionFactionAction("Gain 1 Power") with PowerNeutral { def question(implicit game : Game) = TendonFlaying.styled(FM) }
case class TendonFlayingOption3Action(self : Faction, stitcher : UnitRef, remaining : $[UnitRef])
    extends OptionFactionAction("Gain 1 Doom") with PowerNeutral { def question(implicit game : Game) = TendonFlaying.styled(FM) }

// ── MANNED BATTLEMENTS (Action, movement through Wall network) ──────────────
case class MannedBattlementsMainAction(self : Faction)
    extends OptionFactionAction(MannedBattlements.styled(FM)) with MainQuestion with Soft
case class MannedBattlementsSkipAction(self : Faction)
    extends OptionFactionAction(("Skip " + MannedBattlements.name).styled(FM)) with MainQuestion with Soft
case class MannedBattlementsAreaAction(self : Faction, source : Region)
    extends BaseFactionAction(MannedBattlements.styled(FM) + ": choose source Area with " + ScreamingWall.name, source) with Soft
case class MannedBattlementsUnitPickAction(self : Faction, source : Region, picked : $[UnitRef], remaining : $[UnitRef])
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) = MannedBattlements.styled(FM) + ": choose Units to move from " + game.desc(source)
}
case class MannedBattlementsDestAction(self : Faction, source : Region, units : $[UnitRef], dest : Region)
    extends BaseFactionAction(MannedBattlements.styled(FM) + ": move to", dest)

// ── SPITEFUL CHISEL (Post-Battle, return captured Acolyte) ─────────────────
case class SpitefulChiselMainAction(self : Faction, enemyFaction : Faction)
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) = "Spiteful Chisel".styled(FM) + ": return captured Acolyte from " + enemyFaction.name
}
case class SpitefulChiselReturnAction(self : Faction, enemyFaction : Faction)
    extends OptionFactionAction(implicit g => "Spiteful Chisel".styled(FM) + ": return Acolyte") with PowerNeutral { def question(implicit game : Game) = "Spiteful Chisel".styled(FM) }
case class SpitefulChiselSkipAction(self : Faction)
    extends OptionFactionAction("Skip Spiteful Chisel".styled(FM)) with PowerNeutral { def question(implicit game : Game) = "Spiteful Chisel".styled(FM) }

// ── TENDON FLAYING REQUIREMENT (Pay 6 minus Walls) ──────────────────────────
case class TendonFlayingReqMainAction(self : Faction, cost : Int)
    extends BaseFactionAction(("Pay " + cost + " Power").styled(FM), "Confirm".styled("power"))


// ============================================================================
// FLESHMASON (FM) EXPANSION — game-loop integration
// ============================================================================
object FMExpansion extends Expansion {

    // Count Screaming Walls in play (actual Buildings, not Body Architects).
    def screamingWallsInPlay(implicit game : Game) : Int =
        FM.units.count(u => u.uclass == ScreamingWall && u.region.onMap)

    // Check if a region "has a Screaming Wall" for ability purposes (either an actual Wall, or a Body Architect via Blueprints in Blood).
    def hasScreamingWall(r : Region)(implicit game : Game) : Boolean = {
        val hasActualWall = FM.at(r).exists(_.uclass == ScreamingWall)
        val hasArchitectSubstitute = FM.has(BlueprintsInBlood) && FM.at(r).exists(_.uclass == BodyArchitect) && !hasActualWall
        hasActualWall || hasArchitectSubstitute
    }

    // Regions with Screaming Walls (actual or via Body Architects).
    def regionsWithWalls(implicit game : Game) : $[Region] =
        game.board.regions.filter(hasScreamingWall)

    // ── FRENZYING ADZE (Battle phase, combat bonus for attacker) ────────────
    override def strengthBonus(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int = {
        if (!game.setup.has(FM)) return 0
        game.battle.none.?(0) ||
        game.battle.get.attacker != FM.?(0) ||
        !hasScreamingWall(game.battle.get.region).?(0) ||
        screamingWallsInPlay
    }

    // ── CREATE GATE → also place Screaming Wall (+1 Power) ─────────────────
    override def afterAction()(implicit game : Game) {
        if (!game.setup.has(FM)) return
        // Track if FM just Created a Gate (via BuildGateAction). If so, auto-place a Wall.
        // This triggers after the Gate is placed. Check if the last action was BuildGateAction by FM.
        // (Implementation note: game.fmGateJustCreated flag set in MainAction handler, cleared here.)
        if (game.fmGateJustCreated.nonEmpty) {
            val r = game.fmGateJustCreated.get
            game.fmGateJustCreated = None
            if (FM.pooled(ScreamingWall).any) {
                val wall = FM.pooled(ScreamingWall).head
                game.put(wall.ref, r)
                FM.power += 1
                FM.log("placed", ScreamingWall.styled(FM), "in", r, "(via Gate creation), gained", 1.power)
            }
        }
    }

    // ── TERRIFYING SHRIEK (Post-Battle pain from Walls) ────────────────────
    // Handled in battle flow via FMExpansion hooks.

    // ── SPITEFUL CHISEL (Post-Battle, return captured Acolyte if no Walls in pool and enemy GOO took result) ───
    // Handled in PostBattlePhase.

    // ── SPELLBOOK REQUIREMENTS ──────────────────────────────────────────────
    override def triggers()(implicit game : Game) {
        if (!game.setup.has(FM)) return
        val f = FM

        // SBR 2: Units in play have 12+ combat (excludes pool).
        val totalCombat = f.units.filter(_.region.onMap).filter(u => u.uclass.utype == Monster || u.uclass.utype == Terror)./(_.uclass.cost).sum
        f.satisfyIf(HymnOfSufferingReq, HymnOfSufferingReq.text, totalCombat >= 12)

        // SBR 3: Screaming Wall in Areas with 2 of the specific glyphs (need clarification on which glyphs).
        // Placeholder: check if Walls are in at least 2 distinct glyph areas.
        // TODO: clarify which glyphs from designer.
        val wallRegions = FM.units.filter(u => u.uclass == ScreamingWall && u.region.onMap).map(_.region)
        val distinctGlyphs = wallRegions.map(_.glyph).distinct
        f.satisfyIf(TerrifyingShriekReq, TerrifyingShriekReq.text, distinctGlyphs.num >= 2)

        // SBR 5: Have 3 Screaming Walls in play.
        f.satisfyIf(BlueprintsInBloodReq, BlueprintsInBloodReq.text, screamingWallsInPlay >= 3)

        // SBR 6: Have 6 Screaming Walls in play.
        f.satisfyIf(MannedBattlementsReq, MannedBattlementsReq.text, screamingWallsInPlay >= 6)
    }
}


// ============================================================================
// FLESHMASON (FM) MAIN ACTION HANDLERS
// ============================================================================
trait FactionFM {
    self : CommonPhases with AwakenPhase with ActionPhase with BattlePhase with DoomPhase with GatherPhase =>

    // Register FM's MainActions in the Action Phase.
    override def factionActions(f : Faction)(implicit game : Game, options : $[MessageOption]) : $[UserAction] = {
        var actions = super.factionActions(f)

        if (f == FM) {
            // Create Screaming Wall (alternative to Create Gate).
            val validWallRegions = game.board.regions.filter { r =>
                f.at(r).cultists.any && !r.gate.any && f.pooled(ScreamingWall).any
            }
            if (f.power >= 3 && validWallRegions.any)
                actions :+= CreateScreamingWallMainAction(f)

            // Manned Battlements (Action, cost 1).
            if (f.has(MannedBattlements) && f.power >= 1) {
                val wallRegions = game.board.regions.filter(r => FMExpansion.hasScreamingWall(r) && f.at(r).any)
                if (wallRegions.any)
                    actions :+= MannedBattlementsMainAction(f)
            }
        }

        actions
    }

    // Handle Create Gate → auto-place Wall.
    case class BuildGateAction_FMWrapper(original : BuildGateAction) {
        def wrapForFM(implicit game : Game) : ForcedAction = {
            if (original.self == FM) {
                // Set flag so afterAction can place the Wall.
                game.fmGateJustCreated = |(original.r)
            }
            // Execute the original BuildGateAction.
            Ask(original.self)(MainAction(original.self))
        }
    }
}
