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
case object FrenzyingAdze     extends FactionSpellbook(FM, "Frenzying Adze")


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

    override def abilities = $(FrenzyingAdze)
    override def library   = $(CrumblingYawp, HymnOfSuffering, TerrifyingShriek, TendonFlaying, BlueprintsInBlood, MannedBattlements)
    override def requirements(options : $[GameOption]) =
        $(CrumblingYawpReq, HymnOfSufferingReq, TerrifyingShriekReq, TendonFlayingReq, BlueprintsInBloodReq, MannedBattlementsReq)

    val allUnits =
        6.times(Acolyte)         ++
        3.times(BoneSculptor)    ++
        3.times(MuscleStitcher)  ++
        3.times(BodyArchitect)   ++
        6.times(ScreamingWall)

    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int = {
        units.%(u => u.uclass.utype == Monster || u.uclass.utype == Terror).not(Zeroed)./(_.uclass.cost).sum +
        neutralStrength(units, opponent)
    }
}


// ── ACTION CASE CLASSES ─────────────────────────────────────────────────────
case class CreateScreamingWallMainAction(self : Faction)
    extends OptionFactionAction(("Create " + ScreamingWall.name).styled(FM)) with MainQuestion with Soft
case class CreateScreamingWallAction(self : Faction, r : Region)
    extends BaseFactionAction(("Create " + ScreamingWall.name).styled(FM) + " in", r)

case class CrumblingYawpMainAction(self : Faction)
    extends OptionFactionAction(CrumblingYawp.styled(FM)) with DoomQuestion with Soft
case class CrumblingYawpSkipAction(self : Faction)
    extends OptionFactionAction(("Skip " + CrumblingYawp.name).styled(FM)) with DoomQuestion with Soft
case class CrumblingYawpOption1Action(self : Faction)
    extends OptionFactionAction(implicit g => {
        val count = g.board.regions.count(r => g.gates.contains(r) && g.factions.find(_.gates.has(r)).exists(_ != FM) && FMExpansion.hasScreamingWall(r)(g))
        CrumblingYawp.styled(FM) + ": Gain " + count + " Power"
    }) { def question(implicit game : Game) = CrumblingYawp.styled(FM) }
case class CrumblingYawpOption2Action(self : Faction)
    extends OptionFactionAction(implicit g => {
        val es = FMExpansion.screamingWallsInPlay / 3
        CrumblingYawp.styled(FM) + ": Gain " + es + " Elder Sign" + (es != 1).??("s")
    }) { def question(implicit game : Game) = CrumblingYawp.styled(FM) }

case class HymnOfSufferingMainAction(self : Faction)
    extends OptionFactionAction(HymnOfSuffering.styled(FM)) with Soft
case class HymnOfSufferingSkipAction(self : Faction)
    extends OptionFactionAction(("Skip " + HymnOfSuffering.name).styled(FM)) with Soft
case class HymnOfSufferingAreaAction(self : Faction, r : Region)
    extends BaseFactionAction(HymnOfSuffering.styled(FM), implicit g => {
        val adj = g.board.connected(r).count(FMExpansion.hasScreamingWall)
        g.desc(r) + " (+" + adj + " Power)"
    })

case class TerrifyingShriekApplyAction(self : Faction, preBattle : Boolean)
    extends ForcedAction

case class TendonFlayingMainAction(self : Faction, stitchers : $[UnitRef])
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) = TendonFlaying.styled(FM)
}
case class TendonFlayingChoiceAction(self : Faction, stitcher : UnitRef, remaining : $[UnitRef])
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) = TendonFlaying.styled(FM)
}
case class TendonFlayingOption1Action(self : Faction, stitcher : UnitRef, remaining : $[UnitRef])
    extends OptionFactionAction("Gain 1 Power") with PowerNeutral { def question(implicit game : Game) = TendonFlaying.styled(FM) }
case class TendonFlayingOption2Action(self : Faction, stitcher : UnitRef, remaining : $[UnitRef])
    extends OptionFactionAction("Gain 1 Power") with PowerNeutral { def question(implicit game : Game) = TendonFlaying.styled(FM) }
case class TendonFlayingOption3Action(self : Faction, stitcher : UnitRef, remaining : $[UnitRef])
    extends OptionFactionAction("Gain 1 Doom") with PowerNeutral { def question(implicit game : Game) = TendonFlaying.styled(FM) }

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

    def screamingWallsInPlay(implicit game : Game) : Int =
        FM.units.count(u => u.uclass == ScreamingWall && u.region.onMap)

    def hasScreamingWall(r : Region)(implicit game : Game) : Boolean = {
        val hasActualWall = FM.at(r).exists(_.uclass == ScreamingWall)
        val hasArchitectSubstitute = FM.has(BlueprintsInBlood) && FM.at(r).exists(_.uclass == BodyArchitect) && !hasActualWall
        hasActualWall || hasArchitectSubstitute
    }

    override def strengthBonus(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int = {
        if (!game.setup.has(FM)) return 0
        game.battle match {
            case None => 0
            case Some(b) if b.attacker != FM => 0
            case Some(b) if !hasScreamingWall(b.arena) => 0
            case Some(b) => screamingWallsInPlay
        }
    }

    override def onRoll(dice : Int)(implicit game : Game) {
        if (!game.setup.has(FM)) return
        game.battle.foreach { b =>
            if ((b.attacker == FM || b.defender == FM) && dice >= 9 && FM.needs(CrumblingYawpReq))
                FM.satisfy(CrumblingYawpReq, CrumblingYawpReq.text)
        }
    }

    override def triggers()(implicit game : Game) {
        if (!game.setup.has(FM)) return
        val f = FM

        val totalCombat = f.units.filter(_.region.onMap).filter(u => u.uclass.utype == Monster || u.uclass.utype == Terror)./(_.uclass.cost).sum
        f.satisfyIf(HymnOfSufferingReq, HymnOfSufferingReq.text, totalCombat >= 12)

        val wallRegions = FM.units.filter(u => u.uclass == ScreamingWall && u.region.onMap).map(_.region)
        val distinctGlyphs = wallRegions.map(_.glyph).distinct
        f.satisfyIf(TerrifyingShriekReq, TerrifyingShriekReq.text + " (any 2 distinct glyphs)", distinctGlyphs.num >= 2)

        f.satisfyIf(BlueprintsInBloodReq, BlueprintsInBloodReq.text, screamingWallsInPlay >= 3)
        f.satisfyIf(MannedBattlementsReq, MannedBattlementsReq.text, screamingWallsInPlay >= 6)
    }

    def perform(action : Action, soft : VoidGuard)(implicit game : Game) = action @@ {

        case StartingRegionAction(f : FM.type, r) =>
            val wall = f.pool.%(_.uclass == ScreamingWall).head
            f.place(ScreamingWall, r)
            f.log("placed", ScreamingWall.styled(FM), "in", r, "(setup)")
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
            val power = areas.num
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
            val es = walls / 3
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
            else
                Force(AfterPowerGatherAction)

        case HymnOfSufferingMainAction(self) =>
            val eligibleAreas = game.board.regions.filter(r =>
                self.at(r).exists(_.uclass == Acolyte) && hasScreamingWall(r))
            Ask(self).each(eligibleAreas)(r => HymnOfSufferingAreaAction(self, r)).add(HymnOfSufferingSkipAction(self))

        case HymnOfSufferingSkipAction(self) =>
            self.oncePerAction :+= HymnOfSuffering
            Force(AfterPowerGatherAction)

        case HymnOfSufferingAreaAction(self, r) =>
            val wall = self.at(r).%(_.uclass == ScreamingWall).head
            game.eliminate(wall)
            val adjacent = game.board.connected(r).count(hasScreamingWall)
            if (adjacent > 0) {
                self.power += adjacent
                self.log(HymnOfSuffering.styled(FM) + ": removed Wall in", r, ", gained", adjacent.power)
            } else {
                self.log(HymnOfSuffering.styled(FM) + ": removed Wall in", r, "(no adjacent Walls)")
            }
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
                f.at(r).cultists.any && !r.gate && f.pool.%(_.uclass == ScreamingWall).any
            }
            if (f.power >= 3 && wallAreas.any)
                + CreateScreamingWallMainAction(f)

            if (f.can(MannedBattlements) && f.power >= 1) {
                val sourceAreas = game.board.regions.filter(r => hasScreamingWall(r) && f.at(r).any)
                if (sourceAreas.any)
                    + MannedBattlementsMainAction(f)
            }

            val wallCount = screamingWallsInPlay
            val cost = math.max(0, 6 - wallCount)
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
                self.at(r).cultists.any && !r.gate && self.pool.%(_.uclass == ScreamingWall).any
            }
            Ask(self).each(wallAreas)(r => CreateScreamingWallAction(self, r)).cancel

        case CreateScreamingWallAction(self, r) =>
            self.power -= 3
            val wall = self.pool.%(_.uclass == ScreamingWall).head
            self.place(ScreamingWall, r)
            self.log("created", ScreamingWall.styled(FM), "in", r)
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
            self.power -= 1
            units.foreach { ur => game.move(ur, dest) }
            self.log(MannedBattlements.styled(FM) + ": moved", units.num, "units from", source, "to", dest)
            Then(MainAction(self))

        case TendonFlayingReqMainAction(self, cost) =>
            self.power -= cost
            self.log("paid", cost.power, "for", TendonFlaying.styled(FM), "requirement")
            self.satisfy(TendonFlayingReq, TendonFlayingReq.text)
            Then(MainAction(self))

        case BuildGateAction(f : FM.type, r) if game.setup.has(FM) =>
            f.power -= 3
            f.doom += 1
            game.build(f, r)
            f.log("created", "Gate".styled(f), "in", r)

            if (f.power >= 1 && f.pool.%(_.uclass == ScreamingWall).any) {
                f.power -= 1
                val wall = f.pool.%(_.uclass == ScreamingWall).head
                f.place(ScreamingWall, r)
                f.log("also placed", ScreamingWall.styled(FM), "in", r)
            }

            Then(MainAction(f))

        case PreBattlePhase(b) if game.setup.has(FM) && FM.has(TerrifyingShriek) && (b.attacker == FM || b.defender == FM) =>
            val fmUnits = if (b.attacker == FM) b.attackers else b.defenders
            val hasBoneSculptor = fmUnits.exists(_.uclass == BoneSculptor)
            val hasWall = hasScreamingWall(b.arena)

            if (hasBoneSculptor && hasWall)
                Force(TerrifyingShriekApplyAction(FM, preBattle = true))
            else
                UnknownContinue

        case TerrifyingShriekApplyAction(self, preBattle) =>
            implicit val battle = game.battle.get
            val enemyFaction = if (battle.attacker == self) battle.defender else battle.attacker
            self.log(TerrifyingShriek.styled(FM) + ":", ScreamingWall.name, "adds 1 Pain", preBattle.??("(pre-battle)").|("(post-battle)"))
            game.pains(enemyFaction, $, 1)

        case PostBattlePhase(b) if game.setup.has(FM) && (b.attacker == FM || b.defender == FM) =>
            val fmUnits = if (b.attacker == FM) b.attackers else b.defenders
            val hasBoneSculptor = fmUnits.exists(_.uclass == BoneSculptor)
            val hasWall = hasScreamingWall(b.arena)

            if (FM.has(TerrifyingShriek) && hasWall && !hasBoneSculptor)
                Force(TerrifyingShriekApplyAction(FM, preBattle = false))
            else if (FM.has(TendonFlaying)) {
                val damagedStitchers = fmUnits.filter(_.uclass == MuscleStitcher).filter(u =>
                    game.damages.exists(_._1 == u.ref))
                if (damagedStitchers.any)
                    Force(TendonFlayingMainAction(FM, damagedStitchers./(_.ref)))
                else if (FM.pooled(ScreamingWall).none) {
                    val enemyFaction = if (b.attacker == FM) b.defender else b.attacker
                    val enemyGOOs = (if (b.attacker == enemyFaction) b.attackers else b.defenders).filter(_.goo)
                    val gooHit = enemyGOOs.exists(u => game.assignments.exists(_._1 == u.ref))
                    if (gooHit && FM.factionCard(enemyFaction).exists(_.uclass == Acolyte))
                        Force(SpitefulChiselMainAction(FM, enemyFaction))
                    else
                        UnknownContinue
                } else
                    UnknownContinue
            } else if (FM.pooled(ScreamingWall).none) {
                val enemyFaction = if (b.attacker == FM) b.defender else b.attacker
                val enemyGOOs = (if (b.attacker == enemyFaction) b.attackers else b.defenders).filter(_.goo)
                val gooHit = enemyGOOs.exists(u => game.assignments.exists(_._1 == u.ref))
                if (gooHit && FM.factionCard(enemyFaction).exists(_.uclass == Acolyte))
                    Force(SpitefulChiselMainAction(FM, enemyFaction))
                else
                    UnknownContinue
            } else
                UnknownContinue

        case TendonFlayingMainAction(self, stitchers) =>
            if (stitchers.isEmpty)
                UnknownContinue
            else
                Force(TendonFlayingChoiceAction(self, stitchers.head, stitchers.drop(1)))

        case TendonFlayingChoiceAction(self, stitcher, remaining) =>
            Ask(self)
                .add(TendonFlayingOption1Action(self, stitcher, remaining))
                .add(TendonFlayingOption2Action(self, stitcher, remaining))
                .add(TendonFlayingOption3Action(self, stitcher, remaining))

        case TendonFlayingOption1Action(self, stitcher, remaining) =>
            self.power += 1
            self.log(TendonFlaying.styled(FM) + ": gained", 1.power)
            if (remaining.any)
                Force(TendonFlayingChoiceAction(self, remaining.head, remaining.drop(1)))
            else
                UnknownContinue

        case TendonFlayingOption2Action(self, stitcher, remaining) =>
            self.power += 1
            self.log(TendonFlaying.styled(FM) + ": gained", 1.power)
            if (remaining.any)
                Force(TendonFlayingChoiceAction(self, remaining.head, remaining.drop(1)))
            else
                UnknownContinue

        case TendonFlayingOption3Action(self, stitcher, remaining) =>
            self.doom += 1
            self.log(TendonFlaying.styled(FM) + ": gained", 1.doom)
            if (remaining.any)
                Force(TendonFlayingChoiceAction(self, remaining.head, remaining.drop(1)))
            else
                UnknownContinue

        case SpitefulChiselMainAction(self, enemyFaction) =>
            Ask(self).add(SpitefulChiselReturnAction(self, enemyFaction)).add(SpitefulChiselSkipAction(self))

        case SpitefulChiselReturnAction(self, enemyFaction) =>
            val acolyte = self.factionCard(enemyFaction).%(_.uclass == Acolyte).head
            implicit val battle = game.battle.get
            val region = battle.arena
            game.move(acolyte.ref, region)
            self.log("Spiteful Chisel".styled(FM) + ": returned Acolyte from", enemyFaction.name, "to", region)
            UnknownContinue

        case SpitefulChiselSkipAction(self) =>
            UnknownContinue

        case _ =>
            UnknownContinue
    }
}
