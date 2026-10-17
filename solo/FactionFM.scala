package cws

import hrf.colmat._
import html._

// ============================================================================
// FLESHMASON (FM) — Homebrew designer faction
// Designer ID: dcylxgn1wkqs, owner: zehk
// Faction color #f3c8c8 (pale pink), style "fm".
// Building-focused faction centered on the Screaming Wall.
// ============================================================================


// ── UNITS ───────────────────────────────────────────────────────────────────
case object BoneSculptor   extends FactionUnitClass(FM, "Bone Sculptor",   Monster,  FMValues.BoneSculptor_cost)
case object MuscleStitcher extends FactionUnitClass(FM, "Muscle Stitcher", Monster,  FMValues.MuscleStitcher_cost)
case object BodyArchitect  extends FactionUnitClass(FM, "Body Architect",  Terror,   FMValues.BodyArchitect_cost)
case object ScreamingWall  extends FactionUnitClass(FM, "Screaming Wall",  Building, FMValues.ScreamingWall_cost)


// ── SPELLBOOKS ──────────────────────────────────────────────────────────────
case object CrumblingYawp     extends FactionSpellbook(FM, "Crumbling Yawp")
case object HymnOfSuffering   extends FactionSpellbook(FM, "Hymn of Suffering")
case object TerrifyingShriek  extends FactionSpellbook(FM, "Terrifying Shriek")
case object TendonFlaying     extends FactionSpellbook(FM, "Tendon Flaying")
case object BlueprintsInBlood extends FactionSpellbook(FM, "Blueprints in Blood")
case object MannedBattlements extends FactionSpellbook(FM, "Manned Battlements")
case object FrenzyingAdze     extends FactionSpellbook(FM, "Frenzying Adze")

// FM Unit Ability — Spiteful Chisel (Post-Battle, part of Screaming Wall).
// Declared as a FactionSpellbook so it can be used as a once-per-action marker
// (oncePerAction requires Spellbook type), even though it's not an actual SB.
case object SpitefulChisel extends FactionSpellbook(FM, "Spiteful Chisel")


// ── SPELLBOOK REQUIREMENTS ──
// Every number below comes from FMValues (solo/faction-values/fm.json) - change it there.────────────────────────────────────────────────
case object CrumblingYawpReq     extends Requirement("Roll " + FMValues.CrumblingYawpReq_dice + " or more dice in a single Combat")
case object HymnOfSufferingReq   extends Requirement("Your Units in play have " + FMValues.HymnOfSufferingReq_combat + " or more Combat")
case object TerrifyingShriekReq  extends Requirement("The Screaming Wall is in Areas with " + FMValues.TerrifyingShriekReq_glyphs + " of the 3 Glyph types")
case object TendonFlayingReq     extends Requirement("As an Action, Pay " + FMValues.TendonFlayingReq_power + " Power, minus the number of Screaming Walls in play")
case object BlueprintsInBloodReq extends Requirement("Have " + FMValues.BlueprintsInBloodReq_walls + " Screaming Walls in Play")
case object MannedBattlementsReq extends Requirement("Have " + FMValues.MannedBattlementsReq_walls + " Screaming Walls in Play")


// ============================================================================
// FLESHMASON (FM) FACTION OBJECT
// ============================================================================
case object FM extends Faction { f =>
    def name  = "Fleshmason"
    def short = "FM"
    def style = "fm"

    override def abilities = $(FrenzyingAdze)
    override def library   = $(CrumblingYawp, HymnOfSuffering, TerrifyingShriek, TendonFlaying, BlueprintsInBlood, MannedBattlements)
    override def requirements(options : $[GameOption]) =
        $(CrumblingYawpReq, HymnOfSufferingReq, TerrifyingShriekReq, TendonFlayingReq, BlueprintsInBloodReq, MannedBattlementsReq)

    val allUnits =
        FMValues.Acolyte_count.times(Acolyte)                ++
        FMValues.BoneSculptor_count.times(BoneSculptor)      ++
        FMValues.MuscleStitcher_count.times(MuscleStitcher)  ++
        FMValues.BodyArchitect_count.times(BodyArchitect)    ++
        FMValues.ScreamingWall_count.times(ScreamingWall)

    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int = {
        units.%(u => u.uclass.utype == Monster || u.uclass.utype == Terror).not(Zeroed)./(u => FMExpansion.combatOf(u.uclass)).sum +
        neutralStrength(units, opponent) +
        FMExpansion.strengthBonus(units, opponent)
    }
}


// ── ACTION CASE CLASSES ─────────────────────────────────────────────────────
case class CreateScreamingWallMainAction(self : Faction)
    extends OptionFactionAction(("Create " + ScreamingWall.name).styled(FM)) with MainQuestion with Soft
case class CreateScreamingWallAction(self : Faction, r : Region)
    extends BaseFactionAction(("Create " + ScreamingWall.name).styled(FM) + " in", r)

case class CreateGateWithWallMainAction(self : Faction, l : $[Region])
    extends OptionFactionAction(("Create Gate with " + ScreamingWall.name).styled(FM)) with MainQuestion with Soft
case class CreateGateWithWallAction(self : Faction, r : Region)
    extends BaseFactionAction(("Create Gate with " + ScreamingWall.name).styled(FM) + " in", r)

case class CrumblingYawpMainAction(self : Faction)
    extends OptionFactionAction(CrumblingYawp.styled(FM)) with DoomQuestion with Soft
case class CrumblingYawpSkipAction(self : Faction)
    extends OptionFactionAction(("Skip " + CrumblingYawp.name).styled(FM)) with DoomQuestion with Soft
case class CrumblingYawpOption1Action(self : Faction)
    extends OptionFactionAction(implicit g => {
        val count = g.board.regions.count(r => g.gates.contains(r) && g.factions.find(_.gates.has(r)).exists(_ != FM) && FMExpansion.hasScreamingWall(r)(g)) * FMValues.CrumblingYawp_powerPerArea
        CrumblingYawp.styled(FM) + ": Gain " + count + " Power"
    }) { def question(implicit game : Game) = CrumblingYawp.styled(FM) }
case class CrumblingYawpOption2Action(self : Faction)
    extends OptionFactionAction(implicit g => {
        val es = FMExpansion.screamingWallsInPlay / FMValues.CrumblingYawp_wallsPerSign
        CrumblingYawp.styled(FM) + ": Gain " + es + " Elder Sign" + (es != 1).??("s")
    }) { def question(implicit game : Game) = CrumblingYawp.styled(FM) }

case class HymnOfSufferingMainAction(self : Faction)
    extends OptionFactionAction(HymnOfSuffering.styled(FM)) with Soft {
    def question(implicit game : Game) = HymnOfSuffering.styled(FM)
}
case class HymnOfSufferingSkipAction(self : Faction)
    extends OptionFactionAction(("Skip " + HymnOfSuffering.name).styled(FM)) with Soft {
    def question(implicit game : Game) = HymnOfSuffering.styled(FM)
}
case class HymnOfSufferingAreaAction(self : Faction, r : Region)
    extends BaseFactionAction(HymnOfSuffering.styled(FM), implicit g => {
        val adj = g.board.connected(r).count(FMExpansion.hasScreamingWall) * FMValues.HymnOfSuffering_powerPerArea
        g.desc(r) + " (+" + adj + " Power)"
    })

case class TerrifyingShriekApplyAction(self : Faction, preBattle : Boolean)
    extends ForcedAction

case class TendonFlayingMainAction(self : Faction, stitchers : $[UnitRef])
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) = TendonFlaying.styled(FM)
}
case class TendonFlayingChoiceAction(self : Faction, stitcher : UnitRef, remaining : $[UnitRef], availableOptions : $[Int])
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) = TendonFlaying.styled(FM)
}
case class TendonFlayingOption1Action(self : Faction, stitcher : UnitRef, remaining : $[UnitRef], availableOptions : $[Int])
    extends OptionFactionAction("Gain " + FMValues.TendonFlaying_option1Power + " Power (option 1)") with PowerNeutral { def question(implicit game : Game) = TendonFlaying.styled(FM) }
case class TendonFlayingOption2Action(self : Faction, stitcher : UnitRef, remaining : $[UnitRef], availableOptions : $[Int])
    extends OptionFactionAction("Gain " + FMValues.TendonFlaying_option2Power + " Power (option 2)") with PowerNeutral { def question(implicit game : Game) = TendonFlaying.styled(FM) }
case class TendonFlayingOption3Action(self : Faction, stitcher : UnitRef, remaining : $[UnitRef], availableOptions : $[Int])
    extends OptionFactionAction("Gain " + FMValues.TendonFlaying_option3Doom + " Doom") with PowerNeutral { def question(implicit game : Game) = TendonFlaying.styled(FM) }

case class MannedBattlementsMainAction(self : Faction)
    extends OptionFactionAction(MannedBattlements.styled(FM)) with MainQuestion with Soft
case class MannedBattlementsSkipAction(self : Faction)
    extends OptionFactionAction("Skip") with MainQuestion with Soft
case class MannedBattlementsAreaAction(self : Faction, source : Region)
    extends BaseFactionAction(MannedBattlements.styled(FM), source) with Soft
case class MannedBattlementsUnitPickAction(self : Faction, source : Region, picked : $[UnitRef], remaining : $[UnitRef])
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) = MannedBattlements.styled(FM)
}
case class MannedBattlementsUnitDoneAction(self : Faction, source : Region, units : $[UnitRef])
    extends OptionFactionAction("Done") with PowerNeutral { def question(implicit game : Game) = MannedBattlements.styled(FM) }
case class MannedBattlementsDestPickAction(self : Faction, source : Region, units : $[UnitRef])
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) = MannedBattlements.styled(FM)
}
case class MannedBattlementsDestAction(self : Faction, source : Region, units : $[UnitRef], dest : Region)
    extends BaseFactionAction(MannedBattlements.styled(FM), dest)

case class SpitefulChiselMainAction(self : Faction, enemyFaction : Faction)
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) = "Spiteful Chisel".styled(FM)
}
case class SpitefulChiselReturnAction(self : Faction, enemyFaction : Faction)
    extends OptionFactionAction("Return Acolyte") with PowerNeutral { def question(implicit game : Game) = "Spiteful Chisel".styled(FM) }
case class SpitefulChiselSkipAction(self : Faction)
    extends OptionFactionAction("Skip") with PowerNeutral { def question(implicit game : Game) = "Spiteful Chisel".styled(FM) }

case class TendonFlayingReqMainAction(self : Faction, cost : Int)
    extends BaseFactionAction(("Pay " + cost + " Power").styled(FM), "Confirm")


// ============================================================================
// FLESHMASON (FM) EXPANSION
// ============================================================================
object FMExpansion extends Expansion {

    // Combat dice per unit type. Read from FMValues (faction-values/fm.json) so combat
    // can be changed separately from cost. Defaults equal the old cost-as-combat rule.
    def combatOf(uc : UnitClass) : Int = uc match {
        case BoneSculptor   => FMValues.BoneSculptor_combat
        case MuscleStitcher => FMValues.MuscleStitcher_combat
        case BodyArchitect  => FMValues.BodyArchitect_combat
        case other          => other.cost
    }

    def screamingWallsInPlay(implicit game : Game) : Int =
        FM.units.count(u => u.uclass == ScreamingWall && u.region.onMap)

    def hasScreamingWall(r : Region)(implicit game : Game) : Boolean = {
        val hasActualWall = FM.at(r).exists(_.uclass == ScreamingWall)
        val hasArchitectSubstitute = FM.has(BlueprintsInBlood) && FM.at(r).exists(_.uclass == BodyArchitect) && !hasActualWall
        hasActualWall || hasArchitectSubstitute
    }

    def strengthBonus(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int = {
        if (!game.setup.has(FM)) return 0
        game.battle match {
            case None => 0
            case Some(b) if b.attacker != FM => 0
            case Some(b) if !hasScreamingWall(b.arena) => 0
            case Some(b) => screamingWallsInPlay
        }
    }

    def onRoll(dice : Int)(implicit game : Game) {
        if (!game.setup.has(FM)) return
        game.battle.foreach { b =>
            if ((b.attacker == FM || b.defender == FM) && dice >= FMValues.CrumblingYawpReq_dice && FM.needs(CrumblingYawpReq))
                FM.satisfy(CrumblingYawpReq, CrumblingYawpReq.text)
        }
    }

    override def triggers()(implicit game : Game) {
        if (!game.setup.has(FM)) return
        val f = FM

        val totalCombat = f.units.filter(_.region.onMap).filter(u => u.uclass.utype == Monster || u.uclass.utype == Terror)./(u => combatOf(u.uclass)).sum
        f.satisfyIf(HymnOfSufferingReq, HymnOfSufferingReq.text, totalCombat >= FMValues.HymnOfSufferingReq_combat)

        val wallRegions = FM.units.filter(u => u.uclass == ScreamingWall && u.region.onMap).map(_.region)
        val distinctGlyphs = wallRegions.map(_.glyph).%(g => g == GlyphAA || g == GlyphOO || g == GlyphWW).distinct
        f.satisfyIf(TerrifyingShriekReq, TerrifyingShriekReq.text, distinctGlyphs.num >= FMValues.TerrifyingShriekReq_glyphs)

        f.satisfyIf(BlueprintsInBloodReq, BlueprintsInBloodReq.text, screamingWallsInPlay >= FMValues.BlueprintsInBloodReq_walls)
        f.satisfyIf(MannedBattlementsReq, MannedBattlementsReq.text, screamingWallsInPlay >= FMValues.MannedBattlementsReq_walls)
    }

    def perform(action : Action, soft : VoidGuard)(implicit game : Game) = action @@ {

        case StartingRegionAction(f : FM.type, r) =>
            (1 to FMValues.ScreamingWall_setupCount).foreach { _ =>
                if (f.pool.%(_.uclass == ScreamingWall).any) {
                    f.place(ScreamingWall, r)
                    f.log("placed", ScreamingWall.styled(FM), "in", r, "(setup)")
                }
            }
            UnknownContinue

        case DoomAction(f : FM.type) =>
            implicit val asking = Asking(f)
            game.rituals(f)
            if (f.can(CrumblingYawp) && !f.oncePerAction.has(CrumblingYawp))
                + CrumblingYawpMainAction(f)
            game.reveals(f)
            game.highPriests(f)
            game.hires(f)
            game.recruitsWhateley(f)
            game.motherOfMonsters(f)
            game.doomDone(f)
            asking

        case CrumblingYawpMainAction(self) =>
            Ask(self).add(CrumblingYawpOption1Action(self)).add(CrumblingYawpOption2Action(self)).add(CrumblingYawpSkipAction(self))

        case CrumblingYawpSkipAction(self) =>
            self.oncePerAction :+= CrumblingYawp
            Force(DoomAction(self))

        case CrumblingYawpOption1Action(self) =>
            val areas = game.board.regions.filter(r => game.gates.contains(r) && game.factions.find(_.gates.has(r)).exists(_ != FM) && hasScreamingWall(r))
            val power = areas.num * FMValues.CrumblingYawp_powerPerArea
            if (power > 0) {
                self.power += power
                self.log(CrumblingYawp.styled(FM) + ": gained", power.power)
            } else {
                self.log(CrumblingYawp.styled(FM) + ": no eligible areas")
            }
            self.oncePerAction :+= CrumblingYawp
            Force(DoomAction(self))

        case CrumblingYawpOption2Action(self) =>
            val walls = screamingWallsInPlay
            val es = walls / FMValues.CrumblingYawp_wallsPerSign
            if (es > 0) {
                self.es ++= es.times(ElderSign(0))
                self.log(CrumblingYawp.styled(FM) + ": gained", es.es)
            } else {
                self.log(CrumblingYawp.styled(FM) + ": not enough Walls")
            }
            self.oncePerAction :+= CrumblingYawp
            Force(DoomAction(self))

        case AfterPowerGatherAction if game.setup.has(FM) && FM.can(HymnOfSuffering) && !FM.oncePerAction.has(HymnOfSuffering) =>
            val eligibleAreas = game.board.regions.filter(r =>
                FM.at(r).exists(_.uclass == Acolyte) && hasScreamingWall(r))
            if (eligibleAreas.any)
                Force(HymnOfSufferingMainAction(FM))
            else {
                FM.oncePerAction :+= HymnOfSuffering
                Force(AfterPowerGatherAction)
            }

        case HymnOfSufferingMainAction(self) =>
            val eligibleAreas = game.board.regions.filter(r =>
                self.at(r).exists(_.uclass == Acolyte) && hasScreamingWall(r))
            Ask(self).each(eligibleAreas)(r => HymnOfSufferingAreaAction(self, r)).add(HymnOfSufferingSkipAction(self))

        case HymnOfSufferingSkipAction(self) =>
            self.oncePerAction :+= HymnOfSuffering
            Force(AfterPowerGatherAction)

        case HymnOfSufferingAreaAction(self, r) =>
            val wall = self.at(r).%(_.uclass == ScreamingWall).head
            val adjacent = game.board.connected(r).count(hasScreamingWall) * FMValues.HymnOfSuffering_powerPerArea
            // Log BEFORE eliminate (P5 pattern compliance)
            if (adjacent > 0) {
                self.log(HymnOfSuffering.styled(FM) + ": removed Wall in", r, ", gained", adjacent.power)
                self.power += adjacent
            } else {
                self.log(HymnOfSuffering.styled(FM) + ": removed Wall in", r, "(no adjacent Walls)")
            }
            game.eliminate(wall)
            self.oncePerAction :+= HymnOfSuffering
            Force(AfterPowerGatherAction)

        case MainAction(f : FM.type) if f.active.not =>
            UnknownContinue

        case MainAction(f : FM.type) if f.acted =>
            UnknownContinue

        case MainAction(f : FM.type) =>
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

            val wallAreas = game.board.regions.filter { r =>
                f.at(r).cultists.any && f.pool.%(_.uclass == ScreamingWall).any && !hasScreamingWall(r)
            }
            if (f.power >= FMValues.ScreamingWall_createCost && wallAreas.any)
                + CreateScreamingWallMainAction(f)

            val gateWallAreas = game.board.regions.nex.filter { r =>
                !game.gates.contains(r) && !hasScreamingWall(r) && f.at(r).%(_.canControlGate).any
            }.%(r => f.affords(FMValues.GateWithWall_cost)(r))
            if (f.power >= FMValues.GateWithWall_cost && f.pool.%(_.uclass == ScreamingWall).any && gateWallAreas.any)
                + CreateGateWithWallMainAction(f, gateWallAreas)

            if (f.can(MannedBattlements) && f.power >= FMValues.MannedBattlements_cost) {
                val sourceAreas = game.board.regions.filter(r => hasScreamingWall(r) && f.at(r).any)
                if (sourceAreas.any)
                    + MannedBattlementsMainAction(f)
            }

            val wallCount = screamingWallsInPlay
            val cost = math.max(0, FMValues.TendonFlayingReq_power - wallCount)
            if (f.needs(TendonFlayingReq) && f.power >= cost)
                + TendonFlayingReqMainAction(f, cost)

            game.neutralSpellbooks(f)
            game.libraryActions(f)
            game.highPriests(f)
            game.reveals(f)

            game.endTurn(f)(f.battled.any || game.nexed.any)

            asking

        case CreateScreamingWallMainAction(self) =>
            val wallAreas = game.board.regions.filter { r =>
                self.at(r).cultists.any && self.pool.%(_.uclass == ScreamingWall).any
            }
            Ask(self).each(wallAreas)(r => CreateScreamingWallAction(self, r)).cancel

        case CreateScreamingWallAction(self, r) =>
            self.power -= FMValues.ScreamingWall_createCost
            val wall = self.pool.%(_.uclass == ScreamingWall).head
            self.place(ScreamingWall, r)
            self.log("created", ScreamingWall.styled(FM), "in", r)
            Then(MainAction(self))

        case CreateGateWithWallMainAction(self, locations) =>
            Ask(self).each(locations)(r => CreateGateWithWallAction(self, r)).cancel

        case CreateGateWithWallAction(self, r) =>
            self.power -= FMValues.GateWithWall_cost
            game.gates :+= r
            val wall = self.pool.%(_.uclass == ScreamingWall).head
            self.place(ScreamingWall, r)
            // Place the cultist that paid for the action on the gate
            val cultist = self.at(r).%(_.canControlGate).head
            cultist.onGate = true
            self.log("created a gate with", ScreamingWall.styled(FM), "in", r, "(placed", cultist.uclass.styled(FM), "on gate)")
            Then(MainAction(self))

        case MannedBattlementsMainAction(self) =>
            val sourceAreas = game.board.regions.filter(r => hasScreamingWall(r) && self.at(r).any)
            Ask(self).each(sourceAreas)(r => MannedBattlementsAreaAction(self, r)).add(MannedBattlementsSkipAction(self))

        case MannedBattlementsSkipAction(self) =>
            Force(MainAction(self))

        case MannedBattlementsAreaAction(self, source) =>
            val units = self.at(source)./(_.ref)
            Force(MannedBattlementsUnitPickAction(self, source, $, units))

        case MannedBattlementsUnitPickAction(self, source, picked, remaining) =>
            implicit val asking = Asking(self)
            remaining.foreach { ur =>
                val u = game.unit(ur)
                + MannedBattlementsUnitPickAction(self, source, picked :+ ur, remaining.but(ur))
                    .as("Add " + u.full)
            }
            if (picked.any)
                + MannedBattlementsUnitDoneAction(self, source, picked)
            + CancelAction
            asking

        case MannedBattlementsUnitDoneAction(self, source, units) =>
            Force(MannedBattlementsDestPickAction(self, source, units))

        case MannedBattlementsDestPickAction(self, source, units) =>
            val destAreas = game.board.connected(source).filter(hasScreamingWall)
            Ask(self).each(destAreas)(r => MannedBattlementsDestAction(self, source, units, r)).cancel

        case MannedBattlementsDestAction(self, source, units, dest) =>
            self.power -= FMValues.MannedBattlements_cost
            units.foreach { ur =>
                val u = game.unit(ur)
                u.region = dest
                u.onGate = false
                u.add(Moved)
            }
            self.log(MannedBattlements.styled(FM) + ": moved", units.num, "units from", source, "to", dest)
            Then(MainAction(self))

        case TendonFlayingReqMainAction(self, cost) =>
            self.power -= cost
            self.log("paid", cost.power, "for", TendonFlaying.styled(FM), "requirement")
            self.satisfy(TendonFlayingReq, TendonFlayingReq.text)
            Then(MainAction(self))

        case BuildGateAction(f : FM.type, r) if game.setup.has(FM) =>
            f.power -= FMValues.BuildGate_cost
            game.gates :+= r
            f.log("built a gate in", r)
            Then(MainAction(f))

        case TendonFlayingMainAction(self, stitchers) =>
            if (stitchers.isEmpty)
                UnknownContinue
            else
                Then(TendonFlayingChoiceAction(self, stitchers.head, stitchers.drop(1), $(1, 2, 3)))

        case TendonFlayingChoiceAction(self, stitcher, remaining, availableOptions) =>
            val ask = Ask(self)
            var result = ask
            if (availableOptions.has(1))
                result = result.add(TendonFlayingOption1Action(self, stitcher, remaining, availableOptions))
            if (availableOptions.has(2))
                result = result.add(TendonFlayingOption2Action(self, stitcher, remaining, availableOptions))
            if (availableOptions.has(3))
                result = result.add(TendonFlayingOption3Action(self, stitcher, remaining, availableOptions))
            result

        case TendonFlayingOption1Action(self, stitcher, remaining, availableOptions) =>
            self.power += FMValues.TendonFlaying_option1Power
            self.log(TendonFlaying.styled(FM) + ": gained", FMValues.TendonFlaying_option1Power.power, "(Muscle Stitcher pained, option 1)")
            val newAvailable = availableOptions.but(1)
            if (remaining.any && newAvailable.any)
                Then(TendonFlayingChoiceAction(self, remaining.head, remaining.drop(1), newAvailable))
            else if (remaining.any)
                // No more unique options available, reset for next stitcher
                Then(TendonFlayingChoiceAction(self, remaining.head, remaining.drop(1), $(1, 2, 3)))
            else
                UnknownContinue

        case TendonFlayingOption2Action(self, stitcher, remaining, availableOptions) =>
            self.power += FMValues.TendonFlaying_option2Power
            self.log(TendonFlaying.styled(FM) + ": gained", FMValues.TendonFlaying_option2Power.power, "(Muscle Stitcher pained, option 2)")
            val newAvailable = availableOptions.but(2)
            if (remaining.any && newAvailable.any)
                Then(TendonFlayingChoiceAction(self, remaining.head, remaining.drop(1), newAvailable))
            else if (remaining.any)
                // No more unique options available, reset for next stitcher
                Then(TendonFlayingChoiceAction(self, remaining.head, remaining.drop(1), $(1, 2, 3)))
            else
                UnknownContinue

        case TendonFlayingOption3Action(self, stitcher, remaining, availableOptions) =>
            self.doom += FMValues.TendonFlaying_option3Doom
            self.log(TendonFlaying.styled(FM) + ": gained", FMValues.TendonFlaying_option3Doom.doom, "(Muscle Stitcher pained)")
            val newAvailable = availableOptions.but(3)
            if (remaining.any && newAvailable.any)
                Then(TendonFlayingChoiceAction(self, remaining.head, remaining.drop(1), newAvailable))
            else if (remaining.any)
                // No more unique options available, reset for next stitcher
                Then(TendonFlayingChoiceAction(self, remaining.head, remaining.drop(1), $(1, 2, 3)))
            else
                UnknownContinue

        case SpitefulChiselMainAction(self, enemyFaction) =>
            Ask(self).add(SpitefulChiselReturnAction(self, enemyFaction)).add(SpitefulChiselSkipAction(self))

        case SpitefulChiselReturnAction(self, enemyFaction) =>
            // Find FM's Acolyte that is captured by the enemy (in the enemy's prison)
            val acolyte = self.units.%(u => u.region == enemyFaction.prison && u.uclass == Acolyte).head
            implicit val battle = game.battle.get
            val region = battle.arena
            acolyte.region = region
            acolyte.onGate = false
            self.log("Spiteful Chisel".styled(FM) + ": returned", Acolyte.styled(FM), "from", enemyFaction.short, "Faction Card to", region)
            UnknownContinue

        case SpitefulChiselSkipAction(self) =>
            self.log("Spiteful Chisel".styled(FM) + ": declined")
            UnknownContinue

        case _ =>
            UnknownContinue
    }
}
