package cws

import hrf.colmat._

import html._


sealed trait BattleRoll extends Record
case object Miss extends BattleRoll {
    override def toString = "Miss".styled("miss")
}
case object Pain extends BattleRoll {
    override def toString = "Pain".styled("pain")
}
case object Kill extends BattleRoll {
    override def toString = "Kill".styled("kill")
}

object BattleRoll {
    def roll() = randomInRange(1, 6) @@ {
        case 6 => Kill
        case 5 | 4 => Pain
        case 3 | 2 | 1 => Miss
    }
}

sealed abstract class UnitHealth(val text : String) {
    override def toString = text
}

sealed trait BaseUnitHealth extends UnitHealth

case object Alive extends UnitHealth("alive") with BaseUnitHealth
case object Killed extends UnitHealth("killed") with BaseUnitHealth
case object Pained extends UnitHealth("pained") with BaseUnitHealth
case class DoubleHP(left : BaseUnitHealth, right : BaseUnitHealth) extends UnitHealth((left, right) match {
    case (Killed, Alive) => "half-killed"
    case (Alive, Killed) => "half-killed"
    case (Pained, Alive) => "half-pained"
    case (Alive, Pained) => "half-pained"
    case (Alive, Alive) => "alive"
    case (a, b) => "" + a + "/" + b
})
case class Spared(now : BaseUnitHealth) extends UnitHealth("spared-" + now)


sealed trait BattlePhase extends Record
case object BattleStart extends BattlePhase
case object AttackerPreBattle extends BattlePhase
case object DefenderPreBattle extends BattlePhase
case class YigSnakebiteAssignAction(self : Faction, yigOwner : Faction, uc : UnitClass) extends BaseFactionAction("Snakebite".styled("nt") + " — assign extra " + "Kill".styled("kill"), implicit g => uc.styled(self))
case object CthughaCombatChoicePhase extends BattlePhase
case class CthughaCombatChooseGOOAction(self : Faction, enemy : Faction, goo : UnitClass, combat : Int) extends BaseFactionAction(implicit g => "Cthugha".styled("nt") + " — match enemy GOO combat", implicit g => goo.styled(enemy) + " (Combat " + combat + ")")
case object PreRoll extends BattlePhase
case object RollAttackers extends BattlePhase
case object RollDefenders extends BattlePhase
case object ChannelPowerPhase extends BattlePhase
case object NecrophagyPhase extends BattlePhase
case object PostRoll extends BattlePhase
case object AssignDefenderKills extends BattlePhase
case object AssignAttackerKills extends BattlePhase
case object AllKillsAssignedPhase extends BattlePhase
case object HarbingerKillPhase extends BattlePhase
case object EternalKillPhase extends BattlePhase
case object YgolonacOrificesPhase extends BattlePhase
case object CosmicRulerPhase extends BattlePhase
case object DholePlanetaryDestructionPhase extends BattlePhase
case object LaughingstockPhase extends BattlePhase
case object LengSpiderBloodthirstPhase extends BattlePhase
case object YigSnakebitePhase extends BattlePhase
case object QuachilDustToDustPhase extends BattlePhase
case object ElderShoggothPrimeCausePhase extends BattlePhase
case object CthughaFireVampiresPhase extends BattlePhase
case object BloatedWomanVelvetFanPhase extends BattlePhase

case class VelvetFanCaptureAction(self : Faction, uRef : UnitRef) extends BaseFactionAction(
    implicit g => "The Velvet Fan".styled("nt"), implicit g => "Capture " + g.unit(uRef).uclass.styled(g.unit(uRef).faction) + " onto Loyalty Card") {
    override def question(implicit game : Game) = self.full + " — " + "The Velvet Fan".styled("nt")
}
case class VelvetFanSkipAction(self : Faction) extends BaseFactionAction("The Velvet Fan".styled("nt"), "Skip") {
    override def question(implicit game : Game) = self.full + " — " + "The Velvet Fan".styled("nt")
}

// Cthugha: Fire Vampires — spare killed enemy for 1 Power
case class FireVampiresSpareAction(self : Faction, uRef : UnitRef) extends BaseFactionAction(
    implicit g => "Fire Vampires".styled("nt"), implicit g => "Spare " + g.unit(uRef).uclass.styled(g.unit(uRef).faction) + " for " + "1 Power".styled("power")) {
    override def question(implicit game : Game) = self.full + " — " + "Fire Vampires".styled("nt")
}
case class FireVampiresSkipAction(self : Faction) extends BaseFactionAction("Fire Vampires".styled("nt"), "Skip") {
    override def question(implicit game : Game) = self.full + " — " + "Fire Vampires".styled("nt")
}

// Elder Shoggoth: Prime Cause choices
// [2026-05-23 REVERT] Earlier this tick I added `with Soft` thinking it would
// fix the Cancel button on the replacement sub-menu. It DID get Cancel firing
// but broke Unholy Ground (and likely other post-battle powers) — adding Soft
// to a battle-phase Choose action shifts how `proceed()` re-enters the battle
// phase chain, skipping later phases for the same side. None of the other
// post-battle Choose actions in this file (FireVampiresSpareAction,
// QuachilDustToDustRemoveAction, DholePlanetaryDestructionDoomAction, etc.)
// are Soft — that's the established pattern. Reverting to Hard. Cancel button
// remains a known follow-up: use `.cancel` on the sub-Ask plus a non-Soft
// rewind path (TODO).
case class PrimeCauseChooseUnitAction(self : Faction, uRef : UnitRef) extends BaseFactionAction(
    implicit g => "Prime Cause".styled("nt"), implicit g => "Replace " + g.unit(uRef).uclass.styled(self)) {
    override def question(implicit game : Game) = self.full + " — " + "Prime Cause".styled("nt")
}
case class PrimeCauseChooseReplacementAction(self : Faction, oldRef : UnitRef, newUC : UnitClass) extends BaseFactionAction(
    implicit g => "",
    implicit g => {
        val oldU = g.unit(oldRef)
        val r = oldU.region
        // [2026-05-23] Per user: cost = current cost of the NEW unit / 2 (rounded
        // down). Use awakenCost for faction GOOs (e.g., GC re-awakening Cthulhu
        // for 2 = 4/2), and summonCost for everything else (which already
        // applies discounts like BG Brainless → Reanimated cost 0, Yothan
        // without Extinction → 6, with Extinction → 4, etc.). Replacing an
        // Elder Shoggoth itself still costs 0 (free swap-out).
        val cost = if (oldU.uclass == ElderShoggoth) 0
                   else if (newUC.utype == GOO) self.awakenCost(newUC, r)(g).getOrElse(newUC.cost) / 2
                   else self.summonCost(newUC, r)(g) / 2
        "Replace with " + newUC.styled(self) + (cost > 0).??(" (" + cost.power + ")")
    }) {
    override def question(implicit game : Game) = self.full + " — " + "Prime Cause".styled("nt") + " — choose replacement from Pool"
}
case class PrimeCauseSkipAction(self : Faction) extends BaseFactionAction("Prime Cause".styled("nt"), "Skip") {
    override def question(implicit game : Game) = self.full + " — " + "Prime Cause".styled("nt")
}
case class PrimeCauseCancelReplacementAction(self : Faction) extends BaseFactionAction("Prime Cause".styled("nt"), "Cancel") {
    override def question(implicit game : Game) = self.full + " — " + "Prime Cause".styled("nt") + " — choose Unit to replace"
}

// Quachil Uttaus: Dust to Dust choices
case class QuachilDustToDustRemoveAction(self : Faction, uRef : UnitRef, quOwner : Faction) extends BaseFactionAction(
    implicit g => "Dust to Dust".styled("nt"), implicit g => "Permanently remove " + g.unit(uRef).uclass.styled(self) + " from the game") {
    override def question(implicit game : Game) = self.full + " — " + "Dust to Dust".styled("nt")
}
case class QuachilDustToDustESAction(self : Faction, uRef : UnitRef, quOwner : Faction) extends BaseFactionAction(
    implicit g => "Dust to Dust".styled("nt"), implicit g => "Give " + quOwner.full + " 1 " + "Elder Sign".styled("es")) {
    override def question(implicit game : Game) = self.full + " — " + "Dust to Dust".styled("nt")
}
case object EliminatePhase extends BattlePhase
case object BerserkergangPhase extends BattlePhase
case object UnholyGroundPhase extends BattlePhase
case object AssignDefenderPains extends BattlePhase
case object AssignAttackerPains extends BattlePhase
case object AllPainsAssignedPhase extends BattlePhase
// Tombstalker (TS): Oleaginous post-battle phase where Gla'aki/Deep Tendril pains become retreats
case object OleaginousPhase extends BattlePhase
case object HarbingerPainPhase extends BattlePhase
case object EternalPainPhase extends BattlePhase
case object MadnessPhase extends BattlePhase
case object AttackerDefenderRetreats extends BattlePhase
case object DefenderAttackerRetreats extends BattlePhase
case object AzathothCombatDiePhase extends BattlePhase
case object PostBattlePhase extends BattlePhase
case object BattleEnd extends BattlePhase


trait PreBattleQuestion extends FactionAction {
    def question(implicit game : Game) = (game.battle./(_.attacker).has(self)).?("Attacker").|("Defender") + " pre-battle"
}

case class BattleDoneAction(self : Faction) extends ForcedAction
// Dhole: Planetary Destruction — owner chooses whether opponent gains 2 Doom or 2 Power
case class DholePlanetaryDestructionDoomAction(self : Faction, opponent : Faction) extends BaseFactionAction(implicit g => "Planetary Destruction".styled("nt"), implicit g => "Opponent gains " + 2.doom) { override def question(implicit game : Game) = "Planetary Destruction".styled("nt") + " — choose what opponent gains" }
case class DholePlanetaryDestructionPowerAction(self : Faction, opponent : Faction) extends BaseFactionAction(implicit g => "Planetary Destruction".styled("nt"), implicit g => "Opponent gains " + "2 Power".styled("power")) { override def question(implicit game : Game) = "Planetary Destruction".styled("nt") + " — choose what opponent gains" }
// Leng Spider: Bloodthirst — choose which faction's pains to convert
case class BloodthirstChooseFactionAction(self : Faction, target : Faction) extends BaseFactionAction(implicit g => "Bloodthirst".styled("nt"), implicit g => "Convert " + target.full + "'s 2 " + Pain + " → 1 " + Kill) { override def question(implicit game : Game) = self.full + " — " + "Bloodthirst".styled("nt") + ": choose faction" }
case class BloodthirstDoneAction(self : Faction) extends BaseFactionAction(implicit g => "Bloodthirst".styled("nt"), "Skip") { override def question(implicit game : Game) = self.full + " — " + "Bloodthirst".styled("nt") }
// Albino Penguins: Laughingstock pre-battle movement + side choice
case class LaughingstockMoveAction(self : Faction, uRef : UnitRef) extends BaseFactionAction(implicit g => "Laughingstock".styled("nt") + " — choose Penguins to involve", implicit g => { val u = g.unit(uRef); AlbinoPenguins.styled(self) + " in " + u.region })
case class LaughingstockDoneAction(self : Faction) extends BaseFactionAction("Laughingstock".styled("nt") + " — choose Penguins to involve", "Done — skip Penguins")
case class LaughingstockSideAction(self : Faction, side : Faction) extends BaseFactionAction("Laughingstock".styled("nt") + " — choose side", implicit g => "Fight for " + side.full)
case class PreBattleDoneAction(self : Faction, next : BattlePhase) extends OptionFactionAction("Done") with PreBattleQuestion
case class BattleProceedAction(next : BattlePhase) extends ForcedAction

case class BattleRollAction(f : Faction, rolls : $[BattleRoll], next : BattlePhase) extends ForcedAction
case class AzathothDaemonSultanKillRollAction(self : Faction, roll : Int) extends ForcedAction

case class AssignKillAction(self : Faction, count : Int, faction : Faction, ur : UnitRef) extends BaseFactionAction("Assign " + (count > 1).??(count.styled("highlight") + " ") + ("Kill" + (count > 1).??("s")).styled("kill"), implicit g => g.unit(ur).full + (ur.faction == TT && ur.uclass == HighPriest && TT.can(Martyrdom)).??(" — all other kills to Pains with " + Martyrdom.styled(TT)))
case class AssignPainAction(self : Faction, count : Int, faction : Faction, ur : UnitRef) extends BaseFactionAction("Assign " + (count > 1).??(count.styled("highlight") + " ") + ("Pain" + (count > 1).??("s")).styled("pain"), ur.full)

case class RetreatOrderAction(self : Faction, a : Faction, b : Faction) extends BaseFactionAction("Retreat order", "" + a + " then " + b)

case class EliminateNoWayAction(self : Faction, ur : UnitRef) extends ForcedAction

case class RetreatAllAction(self : Faction, f : Faction, r : Region) extends BaseFactionAction("Retreat all pained " + f + " units to", r)
case class RetreatSeparatelyAction(self : Faction, f : Faction, destinations : $[Region]) extends BaseFactionAction(None, "Retreat separately") with More

case class RetreatUnitAction(self : Faction, ur : UnitRef, r : Region) extends ForcedAction

// Tombstalker (TS): Oleaginous retreat action — pained Gla'aki/Deep Tendrils retreat to adjacent area instead
case class OleaginousRetreatAction(self : Faction, ur : UnitRef, r : Region) extends BaseFactionAction(implicit g => "Retreat " + ur.faction.full + " " + g.unit(ur).full + " with " + Oleaginous + " to", r)


// GC
case class DevourPreBattleAction(self : Faction) extends OptionFactionAction(Devour) with PreBattleQuestion
case class DevourAction(self : Faction, ur : UnitRef) extends ForcedAction

case class AbsorbPreBattleAction(self : Faction) extends OptionFactionAction(Absorb) with PreBattleQuestion with Soft
case class AbsorberAction(self : Faction, ur : UnitRef) extends ForcedAction
case class AbsorbeeAction(self : Faction, ur : UnitRef, tr : UnitRef) extends ForcedAction

// CC
case class AbductPreBattleAction(self : Faction) extends OptionFactionAction(Abduct) with PreBattleQuestion
case class AbductAction(self : Faction, ur : UnitRef, tr : UnitRef) extends BaseFactionAction(Abduct, tr.full)

case class InvisibilityPreBattleAction(self : Faction) extends OptionFactionAction(Invisibility) with PreBattleQuestion with Soft
case class InvisibilityAction(self : Faction, ur : UnitRef, tr : UnitRef) extends ForcedAction

case class SeekAndDestroyPreBattleAction(self : Faction) extends OptionFactionAction(SeekAndDestroy) with PreBattleQuestion with Soft
case class SeekAndDestroyAction(self : Faction, uc : UnitClass, r : Region) extends BaseFactionAction("Bring with " + SeekAndDestroy, uc.styled(self) + " from " + r)

case class HarbingerPowerAction(self : Faction, ur : UnitRef, n : Int) extends ForcedAction
case class HarbingerESAction(self : Faction, ur : UnitRef, e : Int) extends ForcedAction
case class HarbingerAction(self : Faction, ur : UnitRef) extends ForcedAction

// BG
case class NecrophagyAction(self : Faction, ur : UnitRef, r : Region) extends ForcedAction

// SL
case class DemandSacrificePreBattleAction(self : Faction) extends OptionFactionAction(DemandSacrifice) with PreBattleQuestion
case class DemandSacrificeProvideESAction(self : Faction) extends ForcedAction
case class DemandSacrificeKillsArePainsAction(self : Faction) extends ForcedAction

// Energy Nexus as standard pre-battle (variant option)
case class EnergyNexusPreBattleAction(self : Faction) extends OptionFactionAction(EnergyNexus) with PreBattleQuestion

// WW
case class HowlPreBattleAction(self : Faction) extends OptionFactionAction(Howl) with PreBattleQuestion
case class HowlUnitAction(self : Faction, ur : UnitRef) extends ForcedAction
case class HowlAction(self : Faction, ur : UnitRef, r : Region) extends ForcedAction

case class EternalPayAction(self : Faction, u : UnitRef, result : BattleRoll) extends ForcedAction

case class BerserkergangAction(self : Faction, n : Int, u : UnitRef) extends ForcedAction

case class CannibalismAction(self : Faction, r : Region, uc : UnitClass) extends BaseFactionAction("Spawn with " + Cannibalism + " in " + r, uc.styled(self))

// OW
case class ChannelPowerAction(self : Faction, n : Int) extends BaseFactionAction(ChannelPower, "Reroll " + n + " " + (n > 1).?("Misses").|("Miss").styled("miss") + " for " + 1.power)

case class MillionFavoredOnesAction(self : Faction, r : Region, uc : UnitClass, nw : $[UnitClass]) extends BaseFactionAction(MillionFavoredOnes, uc.styled(self) + " in " + r + " to " + ((nw.num > 1).?("" + nw.num + " " + nw(0).plural).|(nw(0).name)).styled(self))
case class MillionFavoredOnesXAction(self : Faction, r : Region, u : UnitRef, nw : $[UnitClass]) extends ForcedAction

// AN
case class UnholyGroundAction(self : Faction, o : Faction, cr : Region) extends ForcedAction
case class UnholyGroundEliminateAction(self : Faction, f : Faction, ur : UnitRef) extends ForcedAction

// Independent Great Old Ones
case class CosmicUnityPreBattleAction(self : Faction) extends OptionFactionAction(CosmicUnity.styled(self)) with PreBattleQuestion
case class CosmicUnityAction(self : Faction, ur : UnitRef) extends ForcedAction

// Neutral Spellbooks
case class ShrivelingPreBattleAction(self : Faction) extends OptionFactionAction(Shriveling) with PreBattleQuestion
case class ShrivelingAction(self : Faction, ur : UnitRef) extends ForcedAction


trait BattleImplicits {
    implicit class BattleFactionEx(f : Faction) {
        def opponent(implicit battle : Battle) : Faction =
            f match {
                case f if f == battle.attacker => battle.defender
                case f if f == battle.defender => battle.attacker
                case _ => throw new Error("faction " + f.name + " is not a side in the battle")
            }

    }

    implicit def factionToSide(f : Faction)(implicit battle : Battle) : Side =
        f match {
            case f if f == battle.attacker => battle.attackers
            case f if f == battle.defender => battle.defenders
            // Non-battle faction (e.g. Penguin owner, Mind Parasite controller) —
            // return attacker side as fallback to prevent crash
            case _ => battle.attackers
        }

}


class Side(private val self : Faction, var forces : $[UnitFigure], var str : Int, var rolls : $[BattleRoll], var effects : $[BattleSpellbook])(implicit val game : Game) {
    def tag(s : BattleSpellbook) = effects.has(s)
    def add(s : BattleSpellbook) { effects :+= s }
    def remove(s : BattleSpellbook) { effects = effects.but(s) }
    def count(s : BattleSpellbook) = effects.count(s)
    var bloodthirstUsed : Int = 0
    var cthughaCombatBonus : Int = 0
}

class Battle(val arena : Region, val attacker : Faction, val defender : Faction, val effect : |[Spellbook])(implicit val game : Game) {
    implicit val battle : Battle = this

    val attackers = new Side(attacker, $, 0, $, $)
    val defenders = new Side(defender, $, 0, $, $)

    val sides = $(attacker, defender)

    var phase : BattlePhase = AzathothCombatDiePhase

    var exempted : $[UnitFigure] = $
    // Albino Penguins: track original owner for post-battle return
    var penguinOriginalOwner : Map[UnitRef, Faction] = Map()
    // Quachil Uttaus: track units already processed by Dust to Dust (ES chosen)
    var dustToDustProcessed : $[Faction] = $
    // Elder Shoggoth: track sides that already used Prime Cause this battle (1 replacement max)
    var primeCauseUsed : $[Faction] = $
    // Cthugha: track sides that already saw the Fire Vampires prompt this battle
    // (so Done/Skip properly advances past the phase instead of re-offering the same units)
    var fireVampiresUsed : $[Faction] = $
    // Dhole: track sides that already triggered Planetary Destruction (prevents re-trigger on proceed)
    var dholePlanetaryProcessed : $[Faction] = $
    // Azathoth: track whether Azathoth received a kill this battle (Daemon Sultan absorbed it).
    // If so, Azathoth cannot also be assigned a pain in the same battle.
    var azathothReceivedKill : Boolean = false
    var azathothNeedsKillRoll : Boolean = false

    // Round 8 Bug 45: flag preventing the post-battle Cyclopean Gaze hook from
    // re-firing each time the battle re-enters PostBattlePhase. After CG completes,
    // it calls proceed() which resumes the battle from PostBattlePhase, which would
    // otherwise hit the CG hook again with the same Ghatanothoa/Revenant sources.
    var fbCyclopeanGazeFiredThisBattle : Boolean = false

    def exempt(u : UnitFigure) {
        exempted :+= u
        sides.foreach(_.forces :-= u)
    }

    var eliminated : $[UnitFigure] = $

    def eliminate(u : UnitFigure) {
        exempt(u)
        eliminated :+= u
        game.eliminate(u)
        u.faction.satisfy(LoseUnitInBattle, "Lose " + u.short + " in battle")
    }

    def retreat(u : UnitFigure, r : Region) = {
        game.fbSuppressCGForPlacement = true
        u.region = r
        game.fbSuppressCGForPlacement = false
        u.onGate = false
        u.add(Retreated)
        u.health = Alive
    }

    def assignedKills(unit : UnitFigure) : Int =
        if (unit.uclass == AzathothIGOO && azathothReceivedKill && unit.health != Killed) 1
        else unit.health match {
            case Killed => 1
            case DoubleHP(Killed, Killed) => 2
            case DoubleHP(Killed, _) => 1
            case DoubleHP(_, Killed) => 1
            case _ => 0
        }

    def canAssignKills(unit : UnitFigure) : Int =
        // Hound of Tindalos: Angles of Time — cannot be assigned a Kill
        if (unit.uclass == HoundOfTindalos) 0
        else if (unit.uclass == AzathothIGOO && azathothReceivedKill) 0
        else unit.health match {
            case DoubleHP(Killed, Killed) => 0
            case DoubleHP(Killed, _) => 1
            case DoubleHP(_, Killed) => 1
            case DoubleHP(_, _) => 2
            case Alive => 1
            case _ => 0
        }

    def assignKill(unit : UnitFigure) {
        // Hound of Tindalos: should never reach here — canAssignKills returns 0
        // If somehow reached, log error (Angles of Time prevents Kill assignment entirely)
        if (unit.uclass == HoundOfTindalos) {
            log("ERROR: Kill assigned to Hound of Tindalos — Angles of Time should prevent this")
            return
        }
        if (unit.uclass == AzathothIGOO && azathothReceivedKill) return
        // Azathoth IGOO: Daemon Sultan — roll 1d6, lower glyph position instead of Kill
        // Elder Thing suppresses Daemon Sultan — Azathoth "can be killed with one kill result"
        if (unit.uclass == AzathothIGOO && ElderThingMindControl.suppresses(unit)) {
            log("Daemon Sultan".styled("nt"), "blocked by", "Elder Thing".styled("nt"), "—", "Azathoth".styled(unit.faction), "can be killed normally")
        }
        if (unit.uclass == AzathothIGOO && !ElderThingMindControl.suppresses(unit)) {
            azathothReceivedKill = true
            // If replaying an old game (next action isn't AzathothDaemonSultanKillRollAction),
            // do inline roll for backward compat. Otherwise set flag for hard roll via RollD6.
            if (game.nextReplayActionHint.exists(h => !h.contains("AzathothDaemonSultanKillRoll"))) {
                val roll = (1::2::3::4::5::6).shuffleSeed(game.azathothGlyphPosition * 7 + 31).first
                game.azathothGlyphPosition -= roll
                log("Azathoth Daemon Sultan".styled("nt") + ":", "Azathoth".styled(unit.faction), "hit — rolled", s"$roll, glyph now at", game.azathothGlyphPosition)
                if (game.azathothGlyphPosition <= 0) {
                    game.azathothGlyphPosition = 0
                    log("Azathoth".styled(unit.faction), "glyph reached 0 — eliminated!")
                    unit.health = Killed
                }
            }
            else {
                azathothNeedsKillRoll = true
            }
            return
        }
        unit.health = unit.health match {
            case Killed => log("ERROR - Assign KILL to KILLED"); Killed
            case Spared(_) => log("ERROR - Assign KILL to SPARED"); Killed
            case DoubleHP(Killed, Killed) => log("ERROR - Assign KILL to DBL-KILLED"); Killed
            case DoubleHP(Killed, _) => DoubleHP(Killed, Killed)
            case DoubleHP(_, Killed) => log("ERROR - Assign KILL to HALF-PAINED"); DoubleHP(Killed, Killed)
            case DoubleHP(Pained, _) => log("ERROR - Assign KILL to HALF-PAINED"); DoubleHP(Pained, Killed)
            case DoubleHP(_, Pained) => DoubleHP(Killed, Pained)
            case DoubleHP(Alive, Alive) => DoubleHP(Killed, Alive)
            case Pained => log("ERROR - Assign KILL to PAINED"); Killed
            case Alive => Killed
        }
        // TT Fulmination: if Ubbo-Sathla just transitioned to Killed in battle and TT has Fulmination,
        // set pending flag — prompt fires at PostBattlePhase. Hook here (not in AssignKillAction case)
        // because bulk kills (assignKills line ~618) call assignKill directly, bypassing the action handler.
        // v2.5.4 fix: previously only the AssignKillAction case set this flag, so when an enemy rolled enough
        // kills to wipe out all targets (auto-bulk-assignment), the flag was never set and Fulmination
        // never fired. Hooking inside assignKill itself catches both interactive and bulk paths.
        if (unit.health == Killed && unit.faction == TT && unit.uclass == UbboSathla && TT.can(Fulmination) && !TTExpansion.ttUbboFulminated) {
            TTExpansion.ttFulminationPending = true
        }
    }

    def assignedPains(unit : UnitFigure) : Int =
        unit.health match {
            case Pained => 1
            case DoubleHP(Pained, Pained) => 2
            case DoubleHP(Pained, _) => 1
            case DoubleHP(_, Pained) => 1
            case _ => 0
        }

    def canAssignPains(unit : UnitFigure) : Int =
        // Azathoth: if it already received a kill this battle (Daemon Sultan absorbed it),
        // it cannot also be assigned a pain in the same battle.
        if (unit.uclass == AzathothIGOO && azathothReceivedKill) 0
        else unit.health match {
            case DoubleHP(Alive, Alive) => 2
            case DoubleHP(Alive, _) => 1
            case DoubleHP(_, Alive) => 1
            case Alive => 1
            case _ => 0
        }

    def assignPain(unit : UnitFigure) {
        if (unit.uclass == AzathothIGOO && azathothReceivedKill) return
         unit.health = unit.health match {
            case Killed => log("ERROR - Assign PAIN to KILLED"); Pained
            case Spared(_) => log("ERROR - Assign PAIN to SPARED"); Killed
            case Pained => log("ERROR - Assign PAIN to PAINED"); Pained
            case DoubleHP(Killed, Killed) => log("ERROR - Assign PAIN to DBL-KILLED"); Pained
            case DoubleHP(Pained, Killed) => log("ERROR - Assign PAIN to PAINED-KILLED"); Pained
            case DoubleHP(Killed, Pained) => log("ERROR - Assign PAIN to KILLED-PAINED"); Pained
            case DoubleHP(Pained, Pained) => log("ERROR - Assign PAIN to DBL-PAINED"); Pained
            case DoubleHP(Killed, Alive) => DoubleHP(Killed, Pained)
            case DoubleHP(Alive, Killed) => DoubleHP(Pained, Killed)
            case DoubleHP(Pained, Alive) => DoubleHP(Pained, Pained)
            case DoubleHP(Alive, Pained) => DoubleHP(Pained, Pained)
            case DoubleHP(Alive, Alive) => DoubleHP(Pained, Alive)
            case Alive => Pained
        }
    }

    def prebattle(s : Faction, next : BattlePhase) : Continue = {
        if (attackers.forces.none || defenders.forces.none) {
            if (attackers.forces.none)
                log(attacker, "had no units remaining")

            if (defenders.forces.none)
                log(defender, "had no units remaining")

            checkKillSpellbooks(attacker)
            checkKillSpellbooks(defender)

            sides.foreach { s =>
                s.forces.foreach(_.remove(Absorbed))
                s.forces.foreach(_.remove(Invised))
            }

            return jump(PostBattlePhase)
        }

        var options : $[FactionAction] = $

        // Elder Thing Mind Control: suppress Devour if Cthulhu shares area with enemy Elder Thing
        if (s.has(Devour) && s.tag(Devour).not && s.forces(Cthulhu).any && s.opponent.forces.vulnerable.any && !s.forces(Cthulhu).exists(u => ElderThingMindControl.suppresses(u)))
            options :+= DevourPreBattleAction(s)

        if (s.can(Shriveling) && s.tag(Shriveling).not && s.opponent.forces.vulnerable.any)
            options :+= ShrivelingPreBattleAction(s)

        if (s.can(Absorb) && s.forces(Shoggoth).any && s.forces.vulnerable.num > 1)
            options :+= AbsorbPreBattleAction(s)

        if (s.can(Howl) && s.tag(Howl).not && s.forces(Wendigo).any && s.opponent.forces.%(_.canBeMoved).any)
            options :+= HowlPreBattleAction(s)

        if (s.can(Abduct) && s.forces(Nightgaunt).any && s.opponent.forces.vulnerable.any)
            options :+= AbductPreBattleAction(s)

        if (s.can(SeekAndDestroy) && s.all(HuntingHorror).%(_.region != arena).any)
            options :+= SeekAndDestroyPreBattleAction(s)

        if (s.can(Invisibility) && s.forces(FlyingPolyp).not(Invised).any)
            options :+= InvisibilityPreBattleAction(s)

        if (s.can(DemandSacrifice) && s.tag(DemandSacrifice).not && s.opponent.tag(KillsArePains).not)
            if (game.options.has(DemandTsathoggua).?(s.forces(Tsathoggua).any).|(s.has(Tsathoggua)))
                options :+= DemandSacrificePreBattleAction(s)

        // Energy Nexus pre-battle variant: handled after all other pre-battle powers (see PreBattleDoneAction)

        // Fiendish Spawn (DS alternate) — pre-battle, requires Avatar Antithesis in battle + larva in pool
        if (s.can(FiendishSpawn) && s.tag(FiendishSpawn).not && s.forces(AvatarAntithesis).any && (s.pool(LarvaThesis) ++ s.pool(LarvaAntithesis) ++ s.pool(LarvaSynthesis)).any)
            options :+= FiendishSpawnPreBattleAction(s)

        // Round 8 Bug 40: also check facedown state for IGOO spellbooks
        if (s.can(CosmicUnity) && s.tag(CosmicUnity).not && s.forces(Daoloth).any && s.opponent.forces.goos.any && !s.forces(Daoloth).exists(u => ElderThingMindControl.suppresses(u)))
            options :+= CosmicUnityPreBattleAction(s)

        // Log blocked abilities (Elder Thing or Moonbeast)
        if (s.forces(Cthulhu).any && s.forces(Cthulhu).exists(u => ElderThingMindControl.suppresses(u)))
            log(s, "Devour".styled("nt"), "blocked by", "Elder Thing".styled("nt"))
        if (s.forces(Daoloth).any && s.forces(Daoloth).exists(u => ElderThingMindControl.suppresses(u)))
            log(s, "Cosmic Unity".styled("nt"), "blocked by", "Elder Thing".styled("nt"))
        if (s.forces(Hastur).any && s.forces(Hastur).exists(u => ElderThingMindControl.suppresses(u)))
            log(s, "Vengeance".styled("nt"), "blocked by", "Elder Thing".styled("nt"))
        if (s.forces(RhanTegoth).any && s.forces(RhanTegoth).exists(u => ElderThingMindControl.suppresses(u)))
            log(s, "Eternal".styled("nt"), "blocked by", "Elder Thing".styled("nt"))

        // TT Terror: if TT has Proto-Shoggoths in battle and has Terror spellbook, offer pre-battle choice
        if (s == TT && s.can(TerrorSB) && !s.tag(TTReduceEnemyCombat) && !s.tag(TTBoostOwnCombat) && s.forces(ProtoShoggoth).any)
            options :+= TTTerrorPreBattleAction(TT)

        Ask(s).list(options).add(PreBattleDoneAction(s, next))
    }

    def preroll(s : Faction) {
        val side = if (s == attacker) attackers else defenders
        // Cthugha combat bonus is a pre-battle choice not included in neutralStrength
        val str = s.strength(s.forces, s.opponent) + side.cthughaCombatBonus

        if (str != s.str) {
            log(s, "strength", (str > s.str).?("increased").|("decreased"), "to", str.str)
            s.str = str
        }

        // Safety: Albino Penguins -2 per penguin
        val penguinCount = s.forces.%(_.uclass == AlbinoPenguins).num
        if (penguinCount > 0 && s.str > 0) {
            val withoutPenguins = s.strength(s.forces.%(_.uclass != AlbinoPenguins), s.opponent) + side.cthughaCombatBonus
            val expected = withoutPenguins + penguinCount * -2
            if (s.str != expected && s.str == withoutPenguins) {
                s.str = math.max(0, expected)
                log(s, "Laughingstock".styled("nt") + ": strength adjusted to", s.str.str, "(", penguinCount, "Penguin".s(penguinCount), "-2 each)")
            }
        }

        // Safety: Servitor of the Outer Gods -1 per servitor
        val servitorCount = s.forces.%(_.uclass == ServitorUnit).num
        if (servitorCount > 0 && s.str > 0) {
            val withoutServitors = s.strength(s.forces.%(_.uclass != ServitorUnit), s.opponent) + side.cthughaCombatBonus
            val expected = withoutServitors + servitorCount * -1
            if (s.str != expected && s.str == withoutServitors) {
                s.str = math.max(0, expected)
                log(s, "Servitor of the Outer Gods".styled("nt") + ": strength adjusted to", s.str.str, "(", servitorCount, "Servitor".s(servitorCount), "-1 each)")
            }
        }

        // Elder Thing Mind Control: suppress GOO special abilities
        if (s.has(Harbinger) && s.forces(Nyarlathotep).any && !s.forces(Nyarlathotep).exists(u => ElderThingMindControl.suppresses(u)))
            s.add(Harbinger)
        else if (s.has(Harbinger) && !s.has(Harbinger) && game.moonbeastOnSpellbook.values.exists(t => t._1 == s && t._2 == Harbinger))
            log(s, Harbinger.styled(s), "blocked by", "Moonbeast".styled("nt"))

        if (s.can(Emissary) && s.forces(Nyarlathotep).any && s.opponent.forces.goos.none)
            s.add(Emissary)
        else if (s.has(Emissary) && !s.can(Emissary) && s.forces(Nyarlathotep).any && game.moonbeastOnSpellbook.values.exists(t => t._1 == s && t._2 == Emissary))
            log(s, Emissary.styled(s), "blocked by", "Moonbeast".styled("nt"))

        if (s.has(Vengeance) && s.forces(Hastur).any && !s.forces(Hastur).exists(u => ElderThingMindControl.suppresses(u)))
            s.add(Vengeance)
        else if (s.has(Vengeance) && !s.has(Vengeance) && s.forces(Hastur).any && game.moonbeastOnSpellbook.values.exists(t => t._1 == s && t._2 == Vengeance))
            log(s, Vengeance.styled(s), "blocked by", "Moonbeast".styled("nt"))

        if (s.can(ChannelPower))
            s.add(ChannelPower)

        if (s.has(Eternal) && s.forces(RhanTegoth).any && !s.forces(RhanTegoth).exists(u => ElderThingMindControl.suppresses(u)))
            s.add(Eternal)
        else if (s.has(Eternal) && !s.has(Eternal) && s.forces(RhanTegoth).any && game.moonbeastOnSpellbook.values.exists(t => t._1 == s && t._2 == Eternal))
            log(s, Eternal.styled(s), "blocked by", "Moonbeast".styled("nt"))

        if (s.can(Regenerate) && s.forces(Starspawn).any)
            s.add(Regenerate)

        if (s.can(MillionFavoredOnes))
            s.add(MillionFavoredOnes)

        if (s.has(UnholyGround))
            s.add(UnholyGround)

        // TT Terror: apply Proto-Shoggoth combat modifier
        val ttSide = if (attacker == TT) attackers else if (defender == TT) defenders else null
        val ttOppSide = if (attacker == TT) defenders else if (defender == TT) attackers else null
        if (ttSide != null) {
            val n = ttSide.forces(ProtoShoggoth).not(Zeroed).num
            if (ttSide.tag(TTBoostOwnCombat) && s == TT)
                s.str = s.str + n
            if (ttSide.tag(TTReduceEnemyCombat) && s != TT && ttOppSide != null)
                s.str = math.max(0, s.str - n)
        }
    }

    def postroll(s : Faction) {
        s.forces.foreach(_.remove(Absorbed))
        s.forces.foreach(_.remove(Invised))

        if (s.tag(Regenerate))
            s.forces(Starspawn).foreach(_.health = DoubleHP(Alive, Alive))

        if (s.tag(KillsArePains)) {
            if (s.rolls.exists(_ == Kill)) {
                s.rolls = s.rolls./(_.useIf(_ == Kill)(_ => Pain))
            }
        }
    }

    def assigner(s : Faction) : Faction =
        if (s.opponent.tag(Vengeance))
            s.opponent
        else
        if (s.neutral)
            s.opponent
        else
            s

    def assignKills(s : Faction, next : BattlePhase) : Continue = {
        val kills = s.opponent.rolls.count(_ == Kill)
        val assigned = s.forces./(assignedKills).sum
        val canAssign = s.forces./(canAssignKills).sum

        if (kills <= assigned)
            return BattleProceedAction(next)

        if (kills >= assigned + canAssign) {
            s.forces.foreach(u => 1.to(canAssignKills(u)).foreach(_ => assignKill(u)))
            if (azathothNeedsKillRoll) {
                azathothNeedsKillRoll = false
                val azUnit = s.forces.%(_.uclass == AzathothIGOO).head
                if (game.nextReplayActionHint.exists(h => !h.contains("AzathothDaemonSultanKillRoll"))) {
                    val roll = (1::2::3::4::5::6).shuffleSeed(game.azathothGlyphPosition * 7 + 31).first
                    game.azathothGlyphPosition -= roll
                    log("Azathoth Daemon Sultan".styled("nt") + ":", "Azathoth".styled(azUnit.faction), "hit — rolled", s"$roll, glyph now at", game.azathothGlyphPosition)
                    if (game.azathothGlyphPosition <= 0) {
                        game.azathothGlyphPosition = 0
                        log("Azathoth".styled(azUnit.faction), "glyph reached 0 — eliminated!")
                        azUnit.health = Killed
                    }
                }
                else {
                    return RollD6(_ => "Daemon Sultan — roll for Azathoth glyph reduction", roll => AzathothDaemonSultanKillRollAction(azUnit.faction, roll))
                }
            }
            return DelayedContinue(100, Then(BattleProceedAction(next)))
        }

        val f = assigner(s)

        if (f != s && assigned == 0)
            log(f, "assigned kills with", Vengeance)

        val eligible = s.forces.%(u => canAssignKills(u) > 0)
        return DelayedContinue(50, Ask(f, eligible.sortP./(u => AssignKillAction(f, kills - assigned, s, u))))
    }

    def assignPains(s : Faction, next : BattlePhase) : Continue = {
        val pains = s.opponent.rolls.count(_ == Pain)
        val assigned = s.forces./(assignedPains).sum
        val canAssign = s.forces./(canAssignPains).sum

        if (pains <= assigned)
            return BattleProceedAction(next)

        if (pains >= assigned + canAssign) {
            s.forces.foreach(u => 1.to(canAssignPains(u)).foreach(_ => assignPain(u)))
            return DelayedContinue(100, Then(BattleProceedAction(next)))
        }

        val f = assigner(s)

        if (f != s && assigned == 0 && s.real)
            log(f, "assigned pains with", Vengeance)

        return DelayedContinue(50, Ask(f, s.forces.%(u => canAssignPains(u) > 0).sortP./(u => AssignPainAction(f, pains - assigned, s, u))))
    }

    def retreater(s : Faction) : Faction = {
        factions.%(_.can(Madness)).starting | {
            if (s.neutral)
                s.opponent
            else
                s
        }
    }

    def retreat(s : Faction) : Continue = {
        val refugees = s.forces.%(_.health == Pained)

        if (refugees.none)
            return proceed()

        if (s.can(Oleaginous)) {
            val oleagPained = refugees.%(u => u.uclass == Glaaki || u.uclass == DeepTendril)
            if (oleagPained.any) {
                val u = oleagPained.first
                val destinations = arena.connected
                if (destinations.any)
                    return Ask(s).each(destinations)(r => OleaginousRetreatAction(TS, u, r))
            }
        }

        val destinations = arena.connectedForRetreat.%(r => s.opponent.at(r).none)

        val chooser : Faction = retreater(s)

        if (destinations.none)
            Ask(s).each(refugees.sortA)(u => EliminateNoWayAction(s, u).as(u)("Nowhere to retreat, a pained unit is eliminated"))
        else
        if (destinations.num == 1) {
            val r = destinations.only

            refugees.foreach(u => retreat(u, r))

            log(refugees./(_.short).mkString(", "), "retreated to", r)

            proceed()
        }
        else
        if (refugees.num == 1 || s.forces.tag(Retreated).any)
            Ask(chooser).each(destinations)(d => RetreatUnitAction(chooser, refugees.first, d).as(d)("Retreat", refugees.first, "to"))
        else
            Ask(chooser).each(destinations)(d => RetreatAllAction(chooser, s, d)).add(RetreatSeparatelyAction(chooser, s, destinations))
    }

    def checkKillSpellbooks(s : Faction) {
        if (s.needs(KillDevour1) || s.needs(KillDevour2)) {
            var devoured = s.count(Devour)
            var kills = s.opponent.forces.count(_.health == Killed)

            if (devoured + kills >= 2) {
                if (s.needs(KillDevour2)) {
                    if (kills >= 2) {
                        s.satisfy(KillDevour2, "Kill two enemy units in a battle")
                        kills -= 2
                    }
                    else {
                        s.satisfy(KillDevour2, "Kill and Devour two enemy units in a battle")
                        devoured -= 1
                        kills -= 1
                    }
                }
            }

            if (devoured + kills >= 1) {
                if (s.needs(KillDevour1)) {
                    if (devoured == 1) {
                        s.satisfy(KillDevour1, "Devour an enemy unit in a battle")
                        devoured -= 1
                    }
                    else {
                        s.satisfy(KillDevour1, "Kill an enemy unit in a battle")
                        kills -= 1
                    }
                }
            }
        }

        // Cthugha Firestorm: "Kill an enemy GOO in Battle"
        if (s.has(Cthugha) && s.upgrades.has(Firestorm).not) {
            val killedGOOs = s.opponent.forces.%(u => u.uclass.utype == GOO && u.health == Killed)
            if (killedGOOs.any) {
                s.upgrades :+= Firestorm
                s.log("gained", Firestorm.styled(s), "for killing enemy GOO with", Cthugha.styled(s))
            }
        }
    }

    def checkByatisSpellbook(s : Faction) : Unit = {
        if (s.upgrades.has(GodOfForgetfulness).not)
            s.forces(Byatis).foreach { u =>
                if (u.health != Killed && s.opponent.forces.exists(_.health == Killed)) {
                    s.upgrades :+= GodOfForgetfulness

                    log(s, "gained", GodOfForgetfulness.styled(s), "for", Byatis.styled(s))
                }
            }
    }

    def checkDaolothSpellbook() : Unit = {
        factions.foreach { f =>
            if (f.upgrades.has(Interdimensional).not && f.has(Daoloth)) {
                if (sides.exists(s => s.forces.exists(u => u.goo && u.health == Killed))) {
                    f.upgrades :+= Interdimensional

                    log(f, "gained", Interdimensional.styled(f), "for", Daoloth.styled(f))
                }
            }
        }
    }

    def jump(bp : BattlePhase) : Continue = {
        phase = bp
        proceed()
    }

    def proceed() : Continue = {
        phase match {
            case AzathothCombatDiePhase =>
                if (sides.has(DS) && sides.exists(_.at(arena).%(_.uclass == AvatarSynthesis).any))
                    return RollD6(_ => "Roll Azathoth die for " + AvatarSynthesis.styled(DS) + " combat", roll => AzathothCombatDieRollAction(DS, roll))
                jump(LaughingstockPhase)

            case LaughingstockPhase =>
                // Albino Penguins Laughingstock:
                // Choice to involve penguins or not, then which penguins from which region
                // Done exits entirely. Cancel goes back to main choice.
                factions.%(f => f.loyaltyCards.has(AlbinoPenguinsCard)).foreach { owner =>
                    val penguinsElsewhere = owner.allInPlay.%(_.uclass == AlbinoPenguins).%(_.region != arena)
                    if (penguinsElsewhere.any) {
                        return Ask(owner)
                            .each(penguinsElsewhere)(u => LaughingstockMoveAction(owner, u.ref))
                            .add(LaughingstockDoneAction(owner))
                    }
                    // Penguins in arena but not yet assigned to a side (owner NOT in battle)
                    if (!sides.has(owner)) {
                        val unassigned = owner.allInPlay.%(_.uclass == AlbinoPenguins).%(_.region == arena).%(u => !penguinOriginalOwner.contains(u.ref))
                        if (unassigned.any) {
                            return Ask(owner)
                                .add(LaughingstockSideAction(owner, attacker))
                                .add(LaughingstockSideAction(owner, defender))
                        }
                    }
                }
                jump(BattleStart)

            case BattleStart =>
                if (attacker.hasAllSB.not)
                    attacker.acted = true

                attacker.forces = attacker.at(arena)

                if (attacker.forces.none) {
                    log("No attackers left to battle")

                    return jump(PostBattlePhase)
                }

                defender.forces = defender.at(arena)

                if (defender.forces.none) {
                    log("No defenders left to battle")

                    return jump(PostBattlePhase)
                }

                // Albino Penguins: transferred penguins are now in the battle side's units list
                // (transferred during LaughingstockPhase before BattleStart)
                // so they're picked up naturally by attacker.at(arena) / defender.at(arena)

                // Insects from Shaggai Mind Parasite: parasitized acolytes fight on insect owner's side
                sides.foreach { s =>
                    val parasitized = s.forces.%(u => MindParasite.isParasitized(u))
                    parasitized.foreach { u =>
                        val controller = MindParasite.controller(u).get
                        if (controller != s && sides.has(controller)) {
                            // Move from true owner's forces to insect owner's forces
                            s.forces :-= u
                            controller.forces :+= u
                            log(u.uclass.styled(u.faction), "fights for", controller.full, "(Mind Parasite)")
                        }
                    }
                }

                // Tombstalker (TS) Grasping Dead: only TombHerds participate in battle, exempt all other TS units
                if (effect == |(GraspingDead)) {
                    sides.%(f => f == TS).foreach { s =>
                        s.forces.%(_.uclass != TombHerd).foreach { u => exempt(u) }
                    }
                    if (attackers.forces.none) {
                        log("No Tomb-Herds left for Grasping Dead battle")
                        return jump(PostBattlePhase)
                    }
                }

                sides.foreach(s => s.forces.foreach(u => u.health = Alive))

                sides.foreach(s => s.str = s.strength(s.forces, s.opponent))

                // Albino Penguins: -2 combat per penguin is handled in neutralStrength
                // Log the penalty for clarity
                sides.foreach { s =>
                    val penguinCount = s.forces.%(_.uclass == AlbinoPenguins).num
                    if (penguinCount > 0) {
                        log(s, "Laughingstock".styled("nt") + ":", penguinCount * 2, "dice removed (", penguinCount, "Penguin".s(penguinCount) + ")")
                    }
                }

                // Servitor of the Outer Gods: -1 combat per servitor is handled in neutralStrength
                // Log the penalty for clarity
                sides.foreach { s =>
                    val servitorCount = s.forces.%(_.uclass == ServitorUnit).num
                    if (servitorCount > 0) {
                        log(s, "Servitor of the Outer Gods".styled("nt") + ":", servitorCount, "dice removed (", servitorCount, "Servitor".s(servitorCount) + ")")
                    }
                }

                // Cthugha combat: already set in CthughaCombatChoicePhase (pre-battle)
                // s.str already includes the chosen GOO's combat from the pre-battle phase

                sides.foreach { s =>
                    if (s.upgrades.has(NightmareWeb).not)
                        s.forces(Nyogtha).foreach { u =>
                            if (s.opponent.forces.goos.any)
                                s.oncePerAction :+= NyogthaPrimed
                        }
                }

                log(attacker, "attacked with", attacker.forces./(_.short).mkString(", "), "" + attacker.str.str)

                attacker.forces(Nyogtha).foreach { u =>
                    log(u, "had its strength at", 4.str, "while attacking")
                }

                if (attacker.forces.%(_.uclass == AvatarSynthesis).any)
                    log(AvatarSynthesis.styled(DS), "Azathoth die", "[" + DS.azathothDieRoll.styled("doom") + "]", "— strength", DS.azathothDieRoll.max(1).str)

                log(defender, "defended with", defender.forces./(_.short).mkString(", "), "" + defender.str.str)

                if (defender.forces.%(_.uclass == AvatarSynthesis).any)
                    log(AvatarSynthesis.styled(DS), "Azathoth die", "[" + DS.azathothDieRoll.styled("doom") + "]", "— strength", DS.azathothDieRoll.max(1).str)

                jump(AttackerPreBattle)

            case AttackerPreBattle =>
                prebattle(attacker, DefenderPreBattle)

            case DefenderPreBattle =>
                prebattle(defender, CthughaCombatChoicePhase)

            case CthughaCombatChoicePhase =>
                // Cthugha pre-battle: choose which enemy GOO's combat to copy
                sides.foreach { s =>
                    val mySide = if (s == attacker) attackers else defenders
                    val enemySide = if (s == attacker) defenders else attackers
                    val enemy = s.opponent
                    if (mySide.forces.%(_.uclass == Cthugha).any) {
                        val enemyGOOs = enemySide.forces.%(_.uclass.utype == GOO)
                        if (enemyGOOs.num > 1) {
                            val gooTypes = enemyGOOs./(_.uclass).distinct
                            val combats = gooTypes./(goo => (goo, enemy.strength(enemySide.forces.%(_.uclass == goo), s)))
                            return Ask(s).each(combats)((goo, c) => CthughaCombatChooseGOOAction(s, enemy, goo, c))
                        } else if (enemyGOOs.num == 1) {
                            val goo = enemyGOOs.head
                            val combat = enemy.strength($(goo), s)
                            mySide.str += combat
                            mySide.cthughaCombatBonus = combat
                            log(s, "Cthugha".styled("nt") + ": combat =", combat, "(matching", goo.uclass.styled(enemy) + ")")
                        }
                    }
                }
                jump(PreRoll)

            case PreRoll =>
                preroll(attacker)
                preroll(defender)

                jump(RollAttackers)

            case RollAttackers =>
                RollBattle(attacker, "attack", attacker.str, x => BattleRollAction(attacker, x, RollDefenders))

            case RollDefenders =>
                RollBattle(defender, "defense", defender.str, x => BattleRollAction(defender, x, ChannelPowerPhase))

            case ChannelPowerPhase =>
                sides.of[OW].foreach { s =>
                    if (s.tag(ChannelPower)) {
                        s.remove(ChannelPower)

                        if (s.rolls.%(_ == Miss).any)
                            if (s.rolls.%(_ == Kill).num < s.opponent.forces./(canAssignKills).sum)
                                if (s.power > 0)
                                    return Ask(s).add(ChannelPowerAction(s, s.rolls.%(_ == Miss).num)).skip(BattleDoneAction(s))
                                else
                                if (s.want(DragonAscending) && factions.%(_.power > 0).any)
                                    return DragonAscendingAskAction(s, |(s), "" + ChannelPower, BattleDoneAction(s))
                    }
                }

                jump(LengSpiderBloodthirstPhase)

            case LengSpiderBloodthirstPhase =>
                // Leng Spider: Bloodthirst — only the spider-owning faction chooses.
                // They pick which faction's pains to convert (theirs or opponent's).
                // One conversion per spider per battle.
                val spiderOwnerOpt = sides.find { s =>
                    val spiderCount : Int = s.forces.%(_.uclass == LengSpiderUnit).num
                    spiderCount > 0 && s.bloodthirstUsed < spiderCount
                }
                spiderOwnerOpt match {
                    case Some(s) =>
                        val sPains : Int = s.rolls.count(_ == Pain)
                        val oPains : Int = s.opponent.rolls.count(_ == Pain)
                        if (sPains >= 2 || oPains >= 2) {
                            var ask = Ask(s)
                            if (sPains >= 2)
                                ask = ask.add(BloodthirstChooseFactionAction(s, s))
                            if (oPains >= 2)
                                ask = ask.add(BloodthirstChooseFactionAction(s, s.opponent))
                            return ask.add(BloodthirstDoneAction(s))
                        } else
                            jump(PostRoll)
                    case None =>
                        jump(PostRoll)
                }

           case PostRoll =>
    postroll(attacker)
    postroll(defender)

    // VOONITH - Vicious
    sides.foreach { s =>
        val vooniths = s.forces.%(_.uclass == Voonith).num
        if (vooniths > 0) {
            val kills = s.rolls.count(_ == Kill)
            val extra = max(0, vooniths - kills)
            if (extra > 0) {
                s.rolls ++= extra.times(Kill)
                log(Voonith.styled(s), "Vicious".styled("nt") + ": added", extra, "Kill".s(extra).styled("kill"))
            }
        }
    }

    // Tombstalker (TS): check spellbook requirements after dice roll (Kill, 3 Pains, Gla'aki vs enemy GOO)
    sides.%(f => f == TS).foreach { ts =>
        if (ts.rolls.has(Kill))
            ts.satisfy(TSRollKill, "Rolled a Kill in battle")
        if (ts.rolls.count(_ == Pain) >= 3)
            ts.satisfy(TSRoll3Pains, "Rolled 3 Pains in battle")
        if (ts.forces.%(_.uclass == Glaaki).any && ts.opponent.forces.%(_.uclass.utype == GOO).any)
            ts.satisfy(TSGlaakiBattlesGOO, "Gla'aki battled enemy GOO")
    }

    // Firstborn (FB) Augury spellbook: after dice are rolled, offer FB the option to
    // replace some of their Miss results with Kill results drawn from the stored augury pool.
    // Round 5 bug fix: explicitly handle BOTH FB-as-attacker and FB-as-defender cases.
    // The offer must trigger whenever FB is any participant in the battle and rolled at least
    // one Miss, not only when FB is the attacker. Pick FB's own Side directly via attacker/
    // defender comparison rather than relying on an implicit Faction->Side conversion inside
    // a sides.filter loop, so the behaviour is obviously symmetric across attacker/defender.
    if (factions.has(FB) && sides.has(FB) && FB.can(Augury) && game.fbAuguryKills > 0) {
        val fbSide : Side = if (attacker == FB) attackers else defenders
        if (fbSide.rolls.has(Miss)) {
            val misses = fbSide.rolls.count(_ == Miss)
            val maxReplace = min(misses, game.fbAuguryKills)
            implicit val asking = Asking(FB)
            (1 to maxReplace).reverse.foreach { n =>
                + FBAuguryBattleReplaceAction(FB, n)
            }
            + FBAuguryBattleCancelAction(FB)
            return asking
        }
    }

    jump(AssignDefenderKills)

            case AssignDefenderKills =>
                assignKills(defender, AssignAttackerKills)

            case AssignAttackerKills =>
                assignKills(attacker, AllKillsAssignedPhase)

            case AllKillsAssignedPhase =>
                sides.foreach { s =>
                    if (s.tag(Emissary)) {
                        s.forces.%(_.health == Killed)(Nyarlathotep).foreach { u =>
                            log(u.uclass, "survived the kill as an", Emissary)
                            u.health = Spared(Pained)
                        }
                    }

                    val doubleKilled = s.forces.%(_.health == DoubleHP(Killed, Killed))
                    val killed = s.forces.%(_.health == Killed)

                    doubleKilled.foreach { u =>
                        log(u, "was", "killed".styled("kill"), "with two", "Kills".styled("kill"))
                        u.health = Killed
                    }

                    if (killed.any)
                        log(killed./(_.short).mkString(", ") + (killed.num > 1).?(" were ").|(" was ") + "killed".styled("kill"))
                }

                jump(YigSnakebitePhase)

            case YigSnakebitePhase =>
                // Yig Snakebite: if Yig owner's Cultists were Killed, enemy gets 1 extra Kill
                // Card: "Your Cultists are now poisonous" — ongoing, fires in ANY battle, not just battles with Yig
                // Elder Thing suppresses Snakebite when sharing area with Yig
                sides.foreach { s =>
                    if (s.loyaltyCards.has(YigCard) && s.allInPlay.%(_.uclass == Yig).exists(u => ElderThingMindControl.suppresses(u)))
                        log(s, "Snakebite".styled("nt"), "blocked by", "Elder Thing".styled("nt"))
                    if (s.loyaltyCards.has(YigCard) && !s.allInPlay.%(_.uclass == Yig).exists(u => ElderThingMindControl.suppresses(u))) {
                        val killedCultists = s.forces.%(u => u.uclass.utype == Cultist && u.health == Killed)
                        if (killedCultists.any) {
                            val enemy = s.opponent
                            val eligible = enemy.forces.%(u => canAssignKills(u) > 0)
                            if (eligible.any) {
                                log(Yig.styled(s), "Snakebite".styled("nt") + ": poisoned cultist killed,", enemy.full, "receives 1 extra", "Kill".styled("kill"))
                                if (eligible.num == 1) {
                                    // Only one target — auto-assign
                                    assignKill(eligible.head)
                                    log(eligible.head.uclass.styled(enemy), "was", "killed".styled("kill"), "by", "Snakebite".styled("nt"))
                                    if (azathothNeedsKillRoll) {
                                        azathothNeedsKillRoll = false
                                        val azUnit = enemy.forces.%(_.uclass == AzathothIGOO).head
                                        if (game.nextReplayActionHint.exists(h => !h.contains("AzathothDaemonSultanKillRoll"))) {
                                            val roll = (1::2::3::4::5::6).shuffleSeed(game.azathothGlyphPosition * 7 + 31).first
                                            game.azathothGlyphPosition -= roll
                                            log("Azathoth Daemon Sultan".styled("nt") + ":", "Azathoth".styled(azUnit.faction), "hit — rolled", s"$roll, glyph now at", game.azathothGlyphPosition)
                                            if (game.azathothGlyphPosition <= 0) {
                                                game.azathothGlyphPosition = 0
                                                log("Azathoth".styled(azUnit.faction), "glyph reached 0 — eliminated!")
                                                azUnit.health = Killed
                                            }
                                        }
                                        else {
                                            return RollD6(_ => "Daemon Sultan — roll for Azathoth glyph reduction", roll => AzathothDaemonSultanKillRollAction(azUnit.faction, roll))
                                        }
                                    }
                                    return jump(HarbingerKillPhase)
                                } else {
                                    // Multiple targets — enemy chooses
                                    return Ask(enemy).each(eligible./(_.uclass).distinct)(uc => YigSnakebiteAssignAction(enemy, s, uc))
                                }
                            }
                        }
                    }
                }
                jump(HarbingerKillPhase)

            case HarbingerKillPhase =>
                sides.foreach { s =>
                    if (s.tag(Harbinger)) {
                        s.opponent.units.goos.%(_.health == Killed).not(Harbinged).some.foreach { l =>
                            val u = l.first
                            val cost = u.uclass match {
                                case AvatarThesis     => DS.azathothTrack
                                case AvatarAntithesis => (8 - DS.azathothTrack).max(0)
                                case _                => u.uclass.cost
                            }
                            val n = (cost + 1) / 2
                            return Ask(s)
                                .add(HarbingerPowerAction(s, u, n).as("Get", n.power)(Harbinger, "for", u))
                                .add(HarbingerESAction(s, u, 2).as("Gain", 2.es)(Harbinger, "for", u))
                        }
                    }
                }

                jump(EternalKillPhase)

            case EternalKillPhase =>
                sides.foreach { s =>
                    if (s.tag(Eternal) && s.power > 0) {
                        val rt = s.forces(RhanTegoth).%(_.health == Killed)

                        if (rt.any) {
                            if (s.power > s.enemies./(_.power).max && s.enemies.exists(_.want(DragonAscending)))
                                return DragonAscendingInstantAction(DragonAscendingDownAction(s, "" + Eternal, BattleDoneAction(s)))

                            s.remove(Eternal)

                            return Ask(s).each(rt)(u => EternalPayAction(s, u, Kill).as("Pay", 1.power, "for", Eternal)("Save", u, "from", Kill)).skip(BattleDoneAction(s))
                        }
                    }
                }

                jump(YgolonacOrificesPhase)

            // Firstborn (FB): Ygolonac Orifices phase
            case YgolonacOrificesPhase =>
                sides.foreach { s =>
                    val killed = s.forces(Ygolonac).%(_.health == Killed)
                    if (killed.any) {
                        if (killed.exists(u => ElderThingMindControl.suppresses(u))) {
                            log(s, "Orifices".styled("nt"), "blocked by", "Elder Thing".styled("nt"))
                        } else {
                            val targets = s.opponent.forces.%(u => u.health != Killed && (u.uclass.utype == Terror || u.uclass.utype == Monster || u.uclass.utype == Cultist))
                            if (targets.any)
                                return Ask(s).each(targets.sortA)(t => YgolonacOrificesAction(s, t).as(t)(Ygolonac, "Orifices")).skip(BattleProceedAction(CosmicRulerPhase))
                        }
                    }
                }

                jump(CosmicRulerPhase)

            // Daemon Sultan (DS): Cosmic Ruler phase
            // Elder Thing Mind Control: suppress if Avatar Synthesis shares area with enemy Elder Thing
            case CosmicRulerPhase =>
                if (sides.has(DS) && DS.all(AvatarSynthesis).any && DS.all(AvatarSynthesis).exists(u => ElderThingMindControl.suppresses(u)))
                    log(DS, "Cosmic Ruler".styled("nt"), "blocked by", "Elder Thing".styled("nt"))
                if (sides.has(DS) && DS.all(AvatarSynthesis).any && !DS.all(AvatarSynthesis).exists(u => ElderThingMindControl.suppresses(u))) {
                    val killedAvatars = DS.forces.%(u => u.goo && u.health == Killed && u.uclass.isInstanceOf[FactionUnitClass])
                    if (killedAvatars.any) {
                        // Exclude already-killed/eliminated units — a GOO sacrificed in a prior CR trigger
                        // this same battle is no longer Alive and must not be offered again
                        val sacrificeOptions = DS.goos.%(o => killedAvatars.has(o).not && o.health == Alive && o.uclass.isInstanceOf[FactionUnitClass])
                        if (sacrificeOptions.any) {
                            val options = killedAvatars./~(saved =>
                                sacrificeOptions./(sacrificed =>
                                    CosmicRulerSacrificeAction(DS, saved, sacrificed)
                                        .as("Eliminate", sacrificed)(CosmicRuler.styled(DS), "save", saved, "from", "Kill".styled("kill"))
                                )
                            )
                            return Ask(DS).list(options).skip(CosmicRulerDeclineAction(DS))
                        }
                    }
                }

                jump(BloatedWomanVelvetFanPhase)

            case BloatedWomanVelvetFanPhase =>
                // Elder Thing suppresses Velvet Fan (Special Ability)
                sides.foreach { s =>
                    if (s.forces.exists(_.uclass == BloatedWoman) && s.forces(BloatedWoman).exists(u => ElderThingMindControl.suppresses(u)))
                        log(s, "Velvet Fan".styled("nt"), "blocked by", "Elder Thing".styled("nt"))
                    if (s.forces.exists(_.uclass == BloatedWoman) && !s.forces(BloatedWoman).exists(u => ElderThingMindControl.suppresses(u))) {
                        // Card: "choose one Killed or Eliminated enemy Monster or Cultist" — NOT Terror
                        // Check forces (Killed) AND eliminated list (Eliminated by Devour/Abduct/etc.)
                        val killedInForces = s.opponent.forces.%(u => u.health == Killed && (u.uclass.utype == Monster || u.uclass.utype == Cultist))
                        val eliminatedByAbility = eliminated.%(u => u.faction == s.opponent && (u.uclass.utype == Monster || u.uclass.utype == Cultist))
                        val killed = killedInForces ++ eliminatedByAbility
                        if (killed.any) {
                            return Ask(s)
                                .each(killed)(u => VelvetFanCaptureAction(s, u.ref))
                                .add(VelvetFanSkipAction(s))
                        }
                    }
                }
                jump(CthughaFireVampiresPhase)

            case CthughaFireVampiresPhase =>
                // Elder Thing suppresses Fire Vampires (Special Ability)
                sides.foreach { s =>
                    if (s.forces.exists(_.uclass == Cthugha) && s.forces(Cthugha).exists(u => ElderThingMindControl.suppresses(u)))
                        log(s, "Fire Vampires".styled("nt"), "blocked by", "Elder Thing".styled("nt"))
                    if (s.forces.exists(_.uclass == Cthugha) && !s.forces(Cthugha).exists(u => ElderThingMindControl.suppresses(u)) && !fireVampiresUsed.has(s)) {
                        val killed = s.opponent.forces.%(_.health == Killed)
                        if (killed.any) {
                            return Ask(s)
                                .each(killed)(u => FireVampiresSpareAction(s, u.ref))
                                .add(FireVampiresSkipAction(s))
                        }
                    }
                }
                jump(ElderShoggothPrimeCausePhase)

            case ElderShoggothPrimeCausePhase =>
                sides.foreach { s =>
                    if (s.forces.exists(_.uclass == ElderShoggoth) && s.forces.exists(_.health != Killed) && !primeCauseUsed.has(s)) {
                        val units = s.forces.%(_.health != Killed)
                        return Ask(s)
                            .each(units)(u => PrimeCauseChooseUnitAction(s, u.ref))
                            .add(PrimeCauseSkipAction(s))
                    }
                }
                jump(DholePlanetaryDestructionPhase)

            case DholePlanetaryDestructionPhase =>
                // Card: "If the Dhole is Killed or Eliminated in Battle"
                // Check forces (Killed by dice) AND eliminated list (Eliminated by Devour/Abduct)
                sides.foreach { s =>
                    if (!dholePlanetaryProcessed.has(s)) {
                        val killedDholes = s.forces(Dhole).%(_.health == Killed)
                        val eliminatedDholes = eliminated.%(u => u.uclass == Dhole && u.faction == s)
                        val allDead = killedDholes ++ eliminatedDholes
                        if (allDead.any) {
                            val dholeOwner = s
                            val opponent = s.opponent
                            dholePlanetaryProcessed :+= s
                            // Owner gains 2 ES
                            dholeOwner.takeES(2)
                            log(Dhole.styled(dholeOwner), "Planetary Destruction".styled("nt") + ":", dholeOwner.full, "gained", 2.es)
                            // Exempt killed Dholes from forces to prevent re-trigger on re-entry
                            killedDholes.foreach(exempt)
                            // OWNER chooses what opponent gains (card: "your opponent gains your choice")
                            return Ask(dholeOwner)
                                .add(DholePlanetaryDestructionDoomAction(dholeOwner, opponent))
                                .add(DholePlanetaryDestructionPowerAction(dholeOwner, opponent))
                        }
                    }
                }
                jump(QuachilDustToDustPhase)

            case QuachilDustToDustPhase =>
                // Quachil Uttaus: ONE ask per battle TOTAL (not per killed unit, fixed 2026-07-16).
                // If any enemy unit was killed in this battle and ANY side has QU, the victim
                // picks ONE killed unit to permanently remove from the game OR gives QU owner 1 ES
                // instead. If BOTH sides have QU, only the FIRST side's QU triggers.
                //
                // Replay protection (2026-07-16): dustToDustProcessed is marked in the action
                // perform handlers, so on replay the action executes and marks the battle
                // as processed before we reach this phase check. This prevents re-generating
                // the Ask during replay.
                if (dustToDustProcessed.isEmpty) {
                    sides.foreach { s =>
                        if (s.forces.exists(_.uclass == QuachilUttaus)) {
                            val quOwner = s
                            val killedEnemies = s.opponent.forces.%(u => u.health == Killed)
                            if (killedEnemies.any) {
                                val victim = s.opponent
                                // Mark battle as processed so we skip remaining sides
                                dustToDustProcessed :+= s
                                // One Ask offering: for each killed unit, "permanently
                                // remove this one" + a single "no, give QU owner 1 ES"
                                // action. Use first killed enemy in the ES action since
                                // we just need a unit ref for the log message.
                                var ask = Ask(victim)
                                killedEnemies.foreach { u =>
                                    ask = ask.add(QuachilDustToDustRemoveAction(victim, u.ref, quOwner))
                                }
                                return ask.add(QuachilDustToDustESAction(victim, killedEnemies.head.ref, quOwner))
                            }
                        }
                    }
                }
                jump(EliminatePhase)

            case EliminatePhase =>
                checkKillSpellbooks(attacker)
                checkKillSpellbooks(defender)

                factions.%(_.can(Cannibalism)).%(f => sides.but(f).%(_.forces.%(_.health == Killed).any).any).foreach { f =>
                    f.oncePerAction :+= Cannibalism
                }

                sides.foreach { s =>
                    if (s.can(Berserkergang))
                        s.forces(GnophKeh).%(_.health == Killed).foreach(_ => s.add(Berserkergang))

                    checkByatisSpellbook(s)
                }

                checkDaolothSpellbook()

                sides.foreach { s =>
                    s.forces.%(_.health == Killed).foreach(eliminate)
                }
                // Also eliminate exempted units that were killed (e.g. Dhole from Planetary Destruction)
                exempted.%(_.health == Killed).foreach { u =>
                    if (!eliminated.has(u)) eliminate(u)
                }

                jump(BerserkergangPhase)

            case BerserkergangPhase =>
                sides.foreach { s =>
                    if (s.tag(Berserkergang)) {
                        val count = s.count(Berserkergang)
                        s.remove(Berserkergang)
                        val targets = s.opponent.forces.vulnerable
                        if (targets.any)
                            if (count >= targets.num) {
                                log(targets./(_.short).mkString(", ") + (targets.num > 1).?(" were ").|(" was ") + "eliminated with", Berserkergang)
                                targets.foreach(eliminate)
                            }
                            else
                                return Ask(s.opponent).each(targets)(u => BerserkergangAction(s.opponent, count, u).as(u.ref.full)(Berserkergang, "eliminates", (count > 1).?("" + count + " units")))
                    }
                }

                jump(UnholyGroundPhase)

            case UnholyGroundPhase =>
                sides.foreach { s =>
                    if (s.tag(UnholyGround)) {
                        s.remove(UnholyGround)

                        if (game.cathedrals.has(arena))
                            return Ask(s).each(game.cathedrals)(r => UnholyGroundAction(s, s.opponent, r).as(r)("Remove a cathedral with", UnholyGround)).skip(BattleDoneAction(s))
                    }
                }

                factions.%(_.can(Necrophagy)).foreach { f =>
                    f.oncePerAction :+= Necrophagy
                }

                jump(NecrophagyPhase)

            case NecrophagyPhase =>
                factions.%(_.oncePerAction.has(Necrophagy)).foreach { f =>
                    f.oncePerAction :-= Necrophagy

                    return Ask(f)
                        .each(f.all(Ghoul).diff(attacker.forces).diff(defender.forces).diff(exempted))(u => NecrophagyAction(f, u, u.region).as(u, "from", u.region)(Necrophagy, "to", arena))
                        .done(BattleDoneAction(f))
                }

                jump(AssignDefenderPains)

            case AssignDefenderPains =>
                assignPains(defender, AssignAttackerPains)

            case AssignAttackerPains =>
                assignPains(attacker, AllPainsAssignedPhase)

            case AllPainsAssignedPhase =>
                sides.foreach { s =>
                    s.forces.foreach(u => u.health match {
                        case DoubleHP(Alive, Alive) =>
                        case DoubleHP(Pained, Pained) => log(u, "was assigned two", "" + Pain + "s".styled("pain"))
                        case DoubleHP(Killed, Alive) => log(u, "was assigned", Kill, "and was", "pained".styled("pain"))
                        case DoubleHP(Pained, Alive) => log(u, "was assigned", Pain, "and was", "pained".styled("pain"))
                        case DoubleHP(Killed, Pained) => log(u, "was assigned", Kill, "and", Pain, "and was", "pained".styled("pain"))
                        case DoubleHP(l, r) => log(u, "was assigned", $(l, r).%(_ != Alive).mkString(" and "), "and was", "pained".styled("pain"))
                        case _ =>
                    })

                    s.forces.foreach(u => u.health = u.health match {
                        case DoubleHP(Alive, Alive) => Alive
                        case DoubleHP(_, _) => Pained
                        case Spared(now) => now
                        case s => s
                    })

                    val pained = s.forces.%(_.health == Pained)

                    if (pained.any)
                        log(pained./(_.short).mkString(", ") + (pained.num > 1).?(" were ").|(" was ") + "pained".styled("pain"))
                }

                jump(OleaginousPhase)

            case OleaginousPhase =>
                // Oleaginous now handled during normal retreat phase (attacker-first ordering preserved)
                jump(HarbingerPainPhase)

            case HarbingerPainPhase =>
                sides.foreach { s =>
                    if (s.tag(Harbinger)) {
                        s.opponent.units.goos.%(_.health == Pained).not(Harbinged).some.foreach { l =>
                            val u = l.first
                            val cost = u.uclass match {
                                case AvatarThesis     => DS.azathothTrack
                                case AvatarAntithesis => (8 - DS.azathothTrack).max(0)
                                case _                => u.uclass.cost
                            }
                            val n = (cost + 1) / 2
                            return Ask(s)
                                .add(HarbingerPowerAction(s, u, n).as("Get", n.power)(Harbinger, "for", u))
                                .add(HarbingerESAction(s, u, 2).as("Gain", 2.es)(Harbinger, "for", u))
                        }
                    }
                }

                jump(EternalPainPhase)

            case EternalPainPhase =>
                sides.foreach { s =>
                    if (s.tag(Eternal) && s.power > 0) {
                        val rt = s.forces(RhanTegoth).%(_.health == Pained)

                        if (rt.any) {
                            if (s.power > s.enemies./(_.power).max && s.enemies.exists(_.want(DragonAscending)))
                                return DragonAscendingInstantAction(DragonAscendingDownAction(s, "" + Eternal, BattleDoneAction(s)))

                            s.remove(Eternal)

                            return Ask(s).each(rt)(u => EternalPayAction(s, u, Pain).as("Pay", 1.power, "for", Eternal)("Save", u, "from", Pain)).skip(BattleDoneAction(s))
                        }
                    }
                }

                jump(MadnessPhase)

            case MadnessPhase =>
                sides.foreach { s =>
                    s.forces.foreach(u => u.health = u.health match {
                        case Spared(now) => now
                        case s => s
                    })
                }

                sides.foreach { s =>
                    s.forces.foreach(u => u.remove(Harbinged))
                }

                if (retreater(attacker) == retreater(defender) && sides.forall(_.units.exists(_.health == Pained))) {
                    val f = retreater(attacker)

                    Ask(f).add(RetreatOrderAction(f, attacker, defender)).add(RetreatOrderAction(f, defender, attacker))
                }
                else {
                    jump(AttackerDefenderRetreats)
                }

            case AttackerDefenderRetreats =>
                if (attackers.forces.%(_.health == Pained).any)
                    retreat(attacker)
                else
                if (defenders.forces.%(_.health == Pained).any)
                    retreat(defender)
                else
                    jump(PostBattlePhase)

            case DefenderAttackerRetreats =>
                if (defenders.forces.%(_.health == Pained).any)
                    retreat(defender)
                else
                if (attackers.forces.%(_.health == Pained).any)
                    retreat(attacker)
                else
                    jump(PostBattlePhase)

            case PostBattlePhase =>
                game.checkGatesLost()

                sides.foreach { s =>
                    if (s.tag(MillionFavoredOnes)) {
                        s.remove(MillionFavoredOnes)

                        val options = s.forces.sortA./~(u => u.uclass match {
                            case Acolyte if s.pool(Mutant).any      => |(MillionFavoredOnesXAction(s, u.region, u, $(Mutant)).as(u.ref.full, "in", u.region, "to", Mutant)(MillionFavoredOnes))
                            case Mutant if s.pool(Abomination).any  => |(MillionFavoredOnesXAction(s, u.region, u, $(Abomination)).as(u.ref.full, "in", u.region, "to", Abomination)(MillionFavoredOnes))
                            case Abomination if s.pool(SpawnOW).any => |(MillionFavoredOnesXAction(s, u.region, u, $(SpawnOW)).as(u.ref.full, "in", u.region, "to", SpawnOW)(MillionFavoredOnes))
                            case SpawnOW if s.pool(Mutant).any      => |(MillionFavoredOnesXAction(s, u.region, u, s.pool(Mutant).num.times(Mutant)).as(u.ref.full, "in", u.region, "to", "Mutant".s(s.pool(Mutant).num).styled(OW))(MillionFavoredOnes))
                            case _ => None
                        })

                        return Ask(s).list(options).done(BattleDoneAction(s))
                    }
                }

                factions.%(_.oncePerAction.has(Cannibalism)).foreach { f =>
                    f.oncePerAction :-= Cannibalism

                    return Ask(f)
                        .when(f.pool(Acolyte).any)(CannibalismAction(f, arena, Acolyte))
                        .when(f.pool(Wendigo).any)(CannibalismAction(f, arena, Wendigo))
                        .skip(BattleDoneAction(f))
                }

                // DS Directed Energy: post-battle, Avatar Thesis survived, gain power per Chaos Gate
                if (factions.has(DS) && DS.can(DirectedEnergy) && sides.has(DS) && !DS.tag(DirectedEnergy)) {
                    val dsSide = if (attacker == DS) attackers else defenders
                    val thesisSurvived = dsSide.forces(AvatarThesis).any
                    if (thesisSurvived) {
                        val chaosGates = DS.chaosGateRegions.%(r => DS.gates.has(r)).num
                        DS.add(DirectedEnergy)
                        return Ask(DS).add(DirectedEnergyPostBattleAction(DS, chaosGates)).add(DirectedEnergySkipAction(DS))
                    }
                }

                // Firstborn (FB) Carnage spellbook: if both FB and the opponent lost units in
                // this battle, FB may pay 1 power or flip a spellbook facedown to gain an Elder Sign
                // Carnage must not trigger if it has been flipped facedown (tracked in oncePerGame)
                if (factions.has(FB) && FB.can(Carnage) && sides.has(FB) && !FB.oncePerAction.has(Carnage)) {
                    val opponent = if (attacker == FB) defender else attacker
                    val opKilled = eliminated.%(_.faction == opponent).any
                    val fbKilled = eliminated.%(_.faction == FB).any
                    if (opKilled && fbKilled) {
                        FB.oncePerAction :+= Carnage
                        return Force(FBCarnagePostBattleAction(FB))
                    }
                }

                // Bug fix Round 4 (Bug 1): Cyclopean Gaze must also fire when an enemy battles FB
                // in a region containing FB's Revenants/Ghatanothoa. The action that triggered the
                // battle "ends" in the arena, so any enemy units still present in the arena after
                // the battle qualify as targets. Build the per-source pain queue from the FB
                // Revenants and Ghatanothoa in the arena and dispatch FBCyclopeanGazePhaseAction
                // with fromBattle=true so it returns to battle flow via FBCyclopeanGazeBattleDoneAction.
                // Round 8 Bug 45: guard with fbCyclopeanGazeFiredThisBattle flag so the
                // CG hook only fires ONCE per battle. After CG completes, FBCyclopeanGazeBattleDoneAction
                // calls proceed() which resumes the battle from PostBattlePhase — this hook would
                // otherwise re-fire with the same sources, causing CG to loop until the enemy
                // had no units left to pain. Set the flag before dispatching CG.
                //
                // Round 8 Bug 59: removed the `sides.has(FB)` check. CG now fires when ANY
                // battle happens in a gaze region (a region with FB Revenants/Ghatanothoa),
                // not just battles where FB is one of the two sides. Per the user: if two
                // non-FB factions battle in a gaze region, CG should still trigger against
                // the attacker. The arena's rev/ghato presence (`revsHere + ghatosHere > 0`)
                // is the only requirement for CG; FB doesn't need to be a battle participant.
                // The `attacker != FB` check remains so FB can't pain themselves if they
                // somehow attacked into a gaze region they own.
                if (factions.has(FB) && FB.can(CyclopeanGaze) && attacker != FB && !fbCyclopeanGazeFiredThisBattle) {
                    // Only count surviving (non-Zeroed) FB Revenants/Ghatanothoa as pain sources.
                    val revsHere = FB.at(arena, RevenantOfKnaa).not(Zeroed).num
                    val ghatosHere = FB.at(arena, Ghatanothoa).not(Zeroed).num
                    val enemyHere = attacker.at(arena).not(Zeroed).%(u => u.uclass.utype != Building).any
                    if ((revsHere + ghatosHere) > 0 && enemyHere) {
                        fbCyclopeanGazeFiredThisBattle = true
                        // One source per Revenant + one per Ghatanothoa, all in the arena
                        val sources : $[FBCyclopeanGazeSource] =
                            revsHere.times(FBCyclopeanGazeSource(arena, RevenantOfKnaa)) ++
                            ghatosHere.times(FBCyclopeanGazeSource(arena, Ghatanothoa))
                        return Force(FBCyclopeanGazePhaseAction(FB, attacker, sources, fromBattle = true))
                    }
                }

                // TT Fulmination: if Ubbo was killed this battle and TT has Fulmination, offer permanent removal
                // Fire here (after EliminatePhase + assign-pains + post-battle abilities) so totalKills is final.
                // v2.5.4 fix: count `eliminated` (all units removed via eliminate() this battle), NOT
                // `sides.forces.count(_.health == Killed)`. By PostBattlePhase, EliminatePhase has already
                // moved every Killed unit out of forces and game.eliminate() reset their health back to Alive
                // (Game.scala line ~1666), so the old forces-based count was always 0 — even when the flag
                // was set, the "X" in the prompt would render as 0 ES. Switching to `eliminated.num` gives
                // the true total per the spell rule: "1 ES for each Unit Killed (by either side) in that
                // Battle". Also wraps the Ask with `.group(Fulmination.styled(TT))` so the menu is presented
                // as a single TT-colored "Fulmination" header with two options below.
                if (TTExpansion.ttFulminationPending) {
                    TTExpansion.ttFulminationPending = false
                    val totalKills = eliminated.num
                    return DelayedContinue(50, Ask(TT)
                        .group(Fulmination.styled(TT))
                        .add(TTFulminationTakeAction(TT, totalKills))
                        .add(TTFulminationDeclineAction(TT)))
                }

                jump(BattleEnd)

            case BattleEnd =>
                // Firstborn (FB): reset the Carnage once-per-battle flag at end of battle
                if (factions.has(FB))
                    FB.oncePerAction :-= Carnage

                // Firstborn (FB) Augury: after battle ends, count Kill results that were not applied
                // (e.g. more kills than enemy units) and store them on the Augury spellbook for later use.
                // Round 5 bug fix: surplus kills must be computed PER SIDE SEPARATELY, then summed,
                // with each per-side surplus clamped at zero so one side's undercount doesn't cancel
                // the other side's surplus. By the time BattleEnd runs, EliminatePhase has already moved
                // every killed unit into the battle-level `exempted` list (see EliminatePhase, line ~768),
                // so counting `exempted` units by faction gives the true per-side kill/elimination total.
                if (factions.has(FB) && FB.can(Augury) && sides.has(FB)) {
                    // Attacker side: kills rolled minus defender units actually killed/eliminated
                    val attackerKillsRolled = attackers.rolls.count(_ == Kill)
                    val defenderUnitsKilled = exempted.count(_.faction == defender)
                    val attackerSurplus = max(0, attackerKillsRolled - defenderUnitsKilled)

                    // Defender side: kills rolled minus attacker units actually killed/eliminated
                    val defenderKillsRolled = defenders.rolls.count(_ == Kill)
                    val attackerUnitsKilled = exempted.count(_.faction == attacker)
                    val defenderSurplus = max(0, defenderKillsRolled - attackerUnitsKilled)

                    val unapplied = attackerSurplus + defenderSurplus
                    if (unapplied > 0) {
                        game.fbAuguryKills += unapplied
                        log(FB, Augury.styled(FB) + ": stored", unapplied, "Kill" + (unapplied > 1).?("s").|(("")), "(" + game.fbAuguryKills, "total)")
                    }
                }

                sides.foreach(_.forces.foreach(u => u.health = Alive))
                sides.foreach(_.forces.foreach(_.remove(Retreated)))
                sides.foreach(_.forces.foreach(_.remove(Zeroed)))
                game.factions.foreach(_.units.foreach(u => if (u.health == Pained) u.health = Alive))
                game.factions.foreach(_.units.foreach(_.remove(Retreated)))

                exempted.foreach(_.remove(Hidden))
                exempted.foreach(_.remove(Absorbed))

                attacker.battled :+= arena

                if (game.nexed.none && attacker.hasAllSB.not)
                    attacker.acted = true

                // Albino Penguins: return surviving transferred penguins to original owner
                // Check each battle side for penguins that have an original owner recorded
                val processedRefs = scala.collection.mutable.Set[UnitRef]()
                sides.foreach { battleSide =>
                    battleSide.units.%(_.uclass == AlbinoPenguins).foreach { u =>
                        penguinOriginalOwner.get(u.ref).foreach { originalOwner =>
                            if (originalOwner != battleSide && !processedRefs.contains(u.ref)) {
                                processedRefs += u.ref
                                // Transfer back: remove from battle side, create under original owner
                                battleSide.units :-= u
                                val returned = new UnitFigure(originalOwner, AlbinoPenguins, u.index, u.region)
                                originalOwner.units :+= returned
                                log("Laughingstock".styled("nt") + ":", AlbinoPenguins.styled(originalOwner), "returned to", originalOwner.full)
                            }
                        }
                    }
                }

                game.battle = None

                if (game.queue.starting.?(_.effect.has(FromBelow)))
                    ProceedBattlesAction
                else
                    AfterAction(attacker)

        }
    }

    def perform(a : Action) : Continue = a match {
        // PROCEED
        case VelvetFanCaptureAction(self, uRef) =>
            val u = game.unit(uRef)
            // Card: "place it on this card; that Unit is considered to be out of play"
            // Penguin: if transferred, return to original owner first
            val origOwner = penguinOriginalOwner.get(u.ref)
            if (origOwner.any) {
                penguinOriginalOwner -= u.ref
                u.faction.units :-= u
                val returned = new UnitFigure(origOwner.get, u.uclass, u.index, VelvetFanHold(self))
                origOwner.get.units :+= returned
                exempt(u)
                log(self.full, "Velvet Fan".styled("nt") + ": captured", u.uclass.styled(origOwner.get), "onto Loyalty Card")
            } else {
                exempt(u)
                u.region = VelvetFanHold(self)
                u.health = Alive
                log(self.full, "Velvet Fan".styled("nt") + ": captured", u.uclass.styled(u.faction), "onto Loyalty Card")
            }
            // ONE unit only — jump past Velvet Fan phase
            jump(CthughaFireVampiresPhase)

        case VelvetFanSkipAction(self) =>
            jump(CthughaFireVampiresPhase)

        case FireVampiresSpareAction(self, uRef) =>
            val u = game.unit(uRef)
            u.health = Spared(Pained)
            self.power += 1
            // Firestorm spellbook: also gain 1 ES per spare
            if (self.upgrades.has(Firestorm) && !self.oncePerGame.has(Firestorm)) {
                self.takeES(1)
                log(self.full, "Fire Vampires".styled("nt") + " + " + Firestorm.styled(self) + ": spared", u.uclass.styled(u.faction), "for", 1.power, "and", 1.es)
            } else {
                log(self.full, "Fire Vampires".styled("nt") + ": spared", u.uclass.styled(u.faction), "for", 1.power)
            }
            // Re-offer remaining killed
            val moreKilled = self.opponent.forces.%(_.health == Killed)
            if (moreKilled.any)
                Ask(self)
                    .each(moreKilled)(u2 => FireVampiresSpareAction(self, u2.ref))
                    .add(FireVampiresSkipAction(self))
            else {
                fireVampiresUsed :+= self
                proceed()
            }

        case FireVampiresSkipAction(self) =>
            fireVampiresUsed :+= self
            proceed()

        case PrimeCauseChooseUnitAction(self, uRef) =>
            // [2026-05-23] Show pool units AS WELL AS faction GOOs whose awaken
            // conditions are met in the battle region. Per user: faction GOOs
            // that have been awakened once (and are now in pool) can be
            // re-awakened via Prime Cause IF the standard awaken conditions
            // for THIS region are met. `self.awakenCost(uc, r)` returns Some
            // only when the awaken conditions are satisfied; we filter on that.
            val battleRegion = game.unit(uRef).region
            val poolNonGoo  = self.pool.%(_.uclass.utype != GOO)./(_.uclass).distinct
            val poolGooKnown = self.pool.%(_.uclass.utype == GOO).%(u =>
                u.uclass.isInstanceOf[FactionUnitClass] && self.awakenCost(u.uclass, battleRegion).nonEmpty
            )./(_.uclass).distinct
            val poolUnits = (poolNonGoo ++ poolGooKnown).distinct
            Ask(self).each(poolUnits)(uc => PrimeCauseChooseReplacementAction(self, uRef, uc)).add(PrimeCauseCancelReplacementAction(self))

        case PrimeCauseChooseReplacementAction(self, oldRef, newUC) =>
            primeCauseUsed :+= self
            val oldUnit = game.unit(oldRef)
            val r = oldUnit.region
            val isES = oldUnit.uclass == ElderShoggoth
            // [2026-05-23] Cost is half the CURRENT cost of the NEW unit:
            //   awakenCost(newUC, r)/2 for faction GOOs (so GC re-awakening
            //   Cthulhu for 2 = 4/2; awakening conditions enforced by the
            //   filter in PrimeCauseChooseUnitAction).
            //   summonCost(newUC, r)/2 for everything else (already applies
            //   faction discounts: BG Brainless drops Reanimated to 0; Yothan
            //   without Extinction → 3 = 6/2; with Extinction → 2 = 4/2; etc.).
            //   Replacing an Elder Shoggoth itself still costs 0.
            val cost = if (isES) 0
                       else if (newUC.utype == GOO) self.awakenCost(newUC, r).getOrElse(newUC.cost) / 2
                       else self.summonCost(newUC, r) / 2
            // [2026-05-23] Prime Cause logs split into 4 separate events per user spec:
            //   1) unit removed, 2) unit replaced, 3) ES given (GOO penalty), 4) doom given (Terror penalty)
            //   Cost-paid log kept as a 5th (cost > 0 only).
            // Remove old unit
            game.eliminate(oldUnit)
            self.forces :-= oldUnit
            log(self.full, "Prime Cause".styled("nt") + ": removed", oldUnit.uclass.styled(self), "from", r)
            // Place new unit (guard: ensure pool has the unit)
            val newUnit = if (self.pool(newUC).any) {
                val u = self.pool(newUC).first
                self.place(newUC, r)
                u
            } else {
                val u = new UnitFigure(self, newUC, self.units.%(_.uclass == newUC).num + 1, r)
                self.units :+= u
                u
            }
            self.forces :+= newUnit
            log(self.full, "Prime Cause".styled("nt") + ": replaced with", newUC.styled(self), "in", r)
            // Cost
            if (cost > 0) {
                self.power -= cost
                log(self.full, "Prime Cause".styled("nt") + ": paid", cost.power)
            }
            // Penalties (based on the REPLACED unit's type; ES replacement = no penalty).
            if (!isES) {
                if (oldUnit.uclass.utype == Terror) {
                    val enemy = self.opponent
                    enemy.doom += 1
                    log(enemy.full, "Prime Cause".styled("nt") + ": gained", 1.doom, "(Terror replaced)")
                }
                if (oldUnit.uclass.utype == GOO) {
                    val enemy = self.opponent
                    enemy.takeES(1)
                    log(enemy.full, "Prime Cause".styled("nt") + ": gained", 1.es, "(GOO replaced)")
                }
            }
            proceed()

        case PrimeCauseSkipAction(self) =>
            // Mark this side as having used (declined) Prime Cause so the re-evaluated
            // ElderShoggothPrimeCausePhase advances past it instead of re-asking the same
            // menu forever. Without this the Skip option loops back to the choose-unit menu.
            primeCauseUsed :+= self
            proceed()

        case PrimeCauseCancelReplacementAction(self) =>
            val units = self.forces.%(_.health != Killed)./(_.ref)
            Ask(self)
                .each(units)(u => PrimeCauseChooseUnitAction(self, u))
                .add(PrimeCauseSkipAction(self))

        case QuachilDustToDustRemoveAction(self, uRef, quOwner) =>
            val u = game.unit(uRef)
            // Remove from battle forces first
            exempt(u)
            // Permanently remove from game
            self.units = self.units.%(_.ref != uRef)
            log(self.full, "permanently removed", u.uclass.styled(self), "from the game via", "Dust to Dust".styled("nt"))
            // Mark as processed so replay doesn't re-trigger (2026-07-16 FIX)
            dustToDustProcessed :+= quOwner
            proceed()

        case QuachilDustToDustESAction(self, uRef, quOwner) =>
            val u = game.unit(uRef)
            quOwner.takeES(1)
            log(quOwner.full, "gained", 1.es, "from", "Dust to Dust".styled("nt"), "(", u.uclass.styled(self), "killed)")
            // Per-battle, not per-unit (user 2026-05-24): the ES choice means
            // "don't remove any unit permanently"; all killed units stay in
            // their normal killed state and will be eliminated by
            // EliminatePhase (which also triggers checkKillSpellbooks).
            // Mark as processed so replay doesn't re-trigger (2026-07-16 FIX)
            dustToDustProcessed :+= quOwner
            proceed()

        case DholePlanetaryDestructionDoomAction(self, opponent) =>
            opponent.doom += 2
            log(self.full, "chose", 2.doom, "for", opponent.full, "from", "Planetary Destruction".styled(self))
            proceed()

        case DholePlanetaryDestructionPowerAction(self, opponent) =>
            opponent.power += 2
            log(self.full, "chose", "2 Power".styled("power"), "for", opponent.full, "from", "Planetary Destruction".styled(self))
            proceed()

        case BloodthirstChooseFactionAction(self, target) =>
            // Convert 2 of target's Pains to 1 Kill (implicit converts target Faction to Side)
            target.rolls = target.rolls.diff($(Pain, Pain))
            target.rolls :+= Kill
            self.bloodthirstUsed += 1
            log(self.full, "Bloodthirst".styled("nt") + ": converted", target.full + "'s 2", Pain, "→ 1", Kill)
            proceed()

        case BloodthirstDoneAction(self) =>
            // Skip remaining Bloodthirst for this side
            val spiders = self.forces.%(_.uclass == LengSpiderUnit).num
            self.bloodthirstUsed = spiders
            proceed()

        case LaughingstockMoveAction(self, uRef) =>
            val u = game.unit(uRef)
            u.region = arena
            log(self.full, "Laughingstock".styled("nt") + ": moved", AlbinoPenguins.styled(self), "to battle area")
            if (sides.has(self)) {
                // Owner IS in battle — penguin stays with owner, loop back for more
                jump(LaughingstockPhase)
            } else {
                // Owner NOT in battle — choose which faction to transfer penguin to
                Ask(self)
                    .add(LaughingstockSideAction(self, attacker))
                    .add(LaughingstockSideAction(self, defender))
            }

        case LaughingstockSideAction(self, side) =>
            // Full faction transfer (same pattern as Y'Golonac Orifices)
            // Penguin "belongs to that player until the battle's end"
            val penguinsInArena = self.at(arena).%(_.uclass == AlbinoPenguins).%(u => !penguinOriginalOwner.contains(u.ref))
            penguinsInArena.headOption.foreach { u =>
                // Record original owner for post-battle return
                val origRef = u.ref
                penguinOriginalOwner += (origRef -> self)
                // Remove from original owner
                self.units :-= u
                // Create under new owner (same pattern as Y'Golonac line 645)
                val transferred = new UnitFigure(side, AlbinoPenguins, u.index, arena)
                side.units :+= transferred
                // Also record the new ref for return lookup
                penguinOriginalOwner += (transferred.ref -> self)
                log(self.full, "Laughingstock".styled("nt") + ":", AlbinoPenguins.styled(side), "transferred to", side.full, "for battle")
            }
            // Loop back for more penguins
            jump(LaughingstockPhase)

        case LaughingstockDoneAction(self) =>
            jump(BattleStart)

        case BattleDoneAction(self) =>
            proceed()

        case BattleProceedAction(bf) =>
            jump(bf)

        case PreBattleDoneAction(self, bf) =>
            // Energy Nexus Pre-Battle variant: fires after all other pre-battle powers
            // Gives SL a full turn, then battle resumes at PreRoll (skipping pre-battle)
            sides.foreach { f =>
                val side = if (f == attacker) attackers else defenders
                if (f.can(EnergyNexusPB) && side.tag(EnergyNexusPB).not && side.forces(Wizard).any && !f.oncePerTurn.has(EnergyNexusPB)) {
                    side.add(EnergyNexusPB)
                    f.oncePerTurn :+= EnergyNexusPB
                    game.nexed = $(arena)
                    game.battleResumePhase = |("PreRoll")
                    // Clear stale Retreated flags from all units (fix for phantom retreat display)
                    game.factions.foreach(_.units.foreach(_.remove(Retreated)))
                    f.log("used", EnergyNexus, "in", arena)
                    return Force(PreMainAction(f))
                }
            }
            jump(bf)

        // ROLL
        case BattleRollAction(f, rolls, next) =>
            f.rolls ++= rolls

            val sv = f.forces(StarVampire)

            if (rolls.num > sv.num)
                log(f, "rolled", rolls.drop(sv.num).mkString(" "))

            0.until(sv.num).foreach { i =>
                log(StarVampire.styled(f), "rolled", rolls(i))

                if (f.opponent.real)
                    rolls(i) match {
                        case Pain if f.opponent.power > 0 =>
                            f.opponent.power -= 1
                            f.power += 1
                            log(StarVampire.styled(f), "Sapping".styled("nt") + ": drained", 1.power, "from", f.opponent, "with a", "Pain".styled("pain"))
                        case Kill if f.opponent.doom > 0 =>
                            f.opponent.doom -= 1
                            f.doom += 1
                            log(StarVampire.styled(f), "Sapping".styled("nt") + ": drained", 1.doom, "from", f.opponent, "with a", "Kill".styled("kill"))
                        case _ =>
                    }
            }

            if (rolls.num >= 6)
                f.satisfy(Roll6DiceInBattle, "Roll " + rolls.num + " dice in Battle")

            jump(next)

        // ASSIGN
        case AssignKillAction(_, _, _, u) =>
            assignKill(u)
            // TT Martyrdom: if TT's High Priest is killed, convert all kills on TT's other units to pains
            // Bug fix: also convert any UNASSIGNED Kill rolls on the attacker's side to Pain rolls so the
            // assign-kills loop does not re-prompt for the remaining Kill rolls. Without this, dropping
            // assigned-kill count (because the converted units are now Pained, not Killed) causes the
            // assignKills loop to re-fire AssignKillAction prompts for the leftover kills, which would
            // wrongly let the attacker keep landing kills on TT despite Martyrdom being in effect.
            if (u.faction == TT && u.uclass == HighPriest && (u : UnitFigure).health == Killed && TT.can(Martyrdom)) {
                val ttSide = if (attacker == TT) attackers else defenders
                ttSide.forces.%(u2 => u2.ref != u && u2.health == Killed).foreach { u2 =>
                    u2.health = Pained
                    log(Martyrdom.styled(TT) + ": kill on", u2.uclass.styled(TT), "converted to Pain")
                }
                // Convert all remaining (unassigned) Kill rolls on the attacker side to Pain rolls so
                // the assignKills loop terminates without further Kill prompts against TT's units.
                val attackerSide = if (attacker == TT) defenders else attackers
                if (attackerSide.rolls.exists(_ == Kill)) {
                    attackerSide.rolls = attackerSide.rolls./(_.useIf(_ == Kill)(_ => Pain))
                    log(Martyrdom.styled(TT) + ": remaining attacker Kill rolls become Pain rolls")
                }
            }
            // TT Fulmination: trigger flag is now set inside assignKill() itself (see line ~397 in this file).
            // That covers both this AssignKillAction path AND the bulk auto-assign path (assignKills line ~618).
            // v2.5.4 fix: previously this case-only check missed bulk kills, so Fulmination silently failed
            // when an enemy rolled enough kills to wipe out the side without prompting per-unit assignment.
            if (azathothNeedsKillRoll) {
                azathothNeedsKillRoll = false
                RollD6(_ => "Daemon Sultan — roll for Azathoth glyph reduction", roll => AzathothDaemonSultanKillRollAction(u.faction, roll))
            }
            else
                proceed()

        case AssignPainAction(_, _, _, u) =>
            assignPain(u)
            proceed()

        case AzathothDaemonSultanKillRollAction(self, roll) =>
            game.azathothGlyphPosition -= roll
            log("Azathoth Daemon Sultan".styled("nt") + ":", "Azathoth".styled(self), "hit — rolled", s"$roll, glyph now at", game.azathothGlyphPosition)
            if (game.azathothGlyphPosition <= 0) {
                game.azathothGlyphPosition = 0
                log("Azathoth".styled(self), "glyph reached 0 — eliminated!")
                val azUnit = sides.flatMap(_.forces).%(_.uclass == AzathothIGOO).head
                azUnit.health = Killed
            }
            proceed()

        // RETREAT
        case RetreatOrderAction(self, a, b) =>
            if (a == attacker)
                jump(AttackerDefenderRetreats)
            else
                jump(DefenderAttackerRetreats)

        case RetreatUnitAction(self, u, r) =>
            // Guard: unit may have been eliminated mid-battle (e.g. Dhole Planetary Destruction)
            val exists = u.faction.units.exists(uf => uf.uclass == u.uclass && uf.index == u.index)
            if (exists) {
                retreat(u, r)
                log(u, "retreated to", r)
            }
            proceed()

        case RetreatAllAction(self, f, r) =>
            val refugees = f.forces.%(_.health == Pained)

            if (refugees.any) {
                refugees.foreach(u => retreat(u, r))
                log(refugees./(_.short).mkString(", "), "retreated to", r)
            }

            proceed()

        case RetreatSeparatelyAction(self, f, l) =>
            val u = f.forces.%(_.health == Pained).first

            Ask(self).each(l)(r => RetreatUnitAction(self, u, r).as(r)("Retreat", u, "to"))

        // Tombstalker (TS) Oleaginous: execute retreat for a pained Gla'aki or Deep Tendril, then re-check for more
        case OleaginousRetreatAction(self, ur, r) =>
            val u = game.unit(ur)
            retreat(u, r)
            log(u, "retreated to", r, "with", Oleaginous, "(Pain became Retreat)")
            proceed()

        case EliminateNoWayAction(self, u) =>
            if (self == DS && u.goo && u.uclass.isInstanceOf[FactionUnitClass] && DS.all(AvatarSynthesis).any) {
                val sacrificeOptions = DS.goos.%(o => o.ref != u && o.health == Alive && o.uclass.isInstanceOf[FactionUnitClass])
                if (sacrificeOptions.any) {
                    val options = sacrificeOptions./(sacrificed =>
                        CosmicRulerSacrificeAction(DS, u, sacrificed)
                            .as("Eliminate", sacrificed)(CosmicRuler.styled(DS), "save", u, "from elimination")
                    )
                    return Ask(DS).list(options).skip(CosmicRulerDeclineNoWayAction(DS, u))
                }
            }
            if (self.tag(Emissary) && u.uclass == Nyarlathotep) {
                self.log("had nowhere to retreat but", u, "remained as an", Emissary)
            }
            else {
                self.log("had nowhere to retreat and eliminated", u)
                eliminate(u)
            }
            self.forces.foreach(_.health = Alive)
            proceed()

        case CosmicRulerDeclineAction(_) =>
            jump(BloatedWomanVelvetFanPhase)

        case CosmicRulerDeclineNoWayAction(self, u) =>
            self.log("had nowhere to retreat and eliminated", u)
            eliminate(u)
            self.forces.foreach(_.health = Alive)
            proceed()

        // DEVOUR
        case DevourPreBattleAction(self) =>
            Ask(self.opponent).each(self.opponent.forces.vulnerable.sortP)(u => DevourAction(self.opponent, u).as(u.ref.full)(Devour))

        case DevourAction(self, u) =>
            u.faction.opponent.add(Devour)
            eliminate(u)
            log(u, "was devoured by", self.opponent)
            proceed()

        // ABSORB
        case AbsorbPreBattleAction(self) =>
            val shoggoths = self.forces(Shoggoth)
            val actions = shoggoths./(u => AbsorberAction(self, u).as(u.ref.full)("Absorb with"))

            if (shoggoths./(_.state.sorted).distinct.num == 1)
                Ask(self).list(actions.take(1))
            else
                Ask(self).list(actions).cancel

        case AbsorberAction(self, u) =>
            Ask(self).each(self.forces.but(u).vulnerable)(t => AbsorbeeAction(self, u, t).as((t.uclass == Shoggoth).??("Another"), t)("Absorb with", u.ref.full)).cancel

        case AbsorbeeAction(self, u, t) =>
            0.to(t.count(Absorbed)).foreach(_ => u.add(Absorbed))
            eliminate(t)
            log(u, "absorbed", t, "and increased its strength by", (3 + t.count(Absorbed) * 3).str)
            proceed()

        // ABDUCT
        case AbductPreBattleAction(self) =>
            val u = self.forces(Nightgaunt).head
            Ask(self.opponent).each(self.opponent.forces.vulnerable)(t => AbductAction(self.opponent, u, t))

        case AbductAction(self, u, t) =>
            eliminate(u)
            eliminate(t)
            log(t, "was abducted by", u)
            proceed()

        // INVISIBILITY
        case InvisibilityPreBattleAction(self) =>
            val u = self.forces(FlyingPolyp).not(Invised).head
            Ask(self).each((self.opponent.forces ++ self.forces).vulnerable)(t => InvisibilityAction(self, u, t).as(t.ref.full, (u == t).?("(self)"))(u, "makes invisible")).cancel

        case InvisibilityAction(self, u, t) =>
            t.add(Hidden)
            exempt(t)

            u.add(Invised)

            if (u == t)
                log(u, "hid itself")
            else
                log(t, "was hidden by", u)

            proceed()

        // SEEK AND DESTROY
        case SeekAndDestroyPreBattleAction(self) =>
            val us = self.all(HuntingHorror).%(_.region != arena)
            Ask(self).each(us)(u => SeekAndDestroyAction(self, u.uclass, u.region)).cancel

        case SeekAndDestroyAction(self, uc, r) =>
            val u = self.at(r).one(uc)
            u.region = arena
            self.forces :+= u
            log(u, "flew from", r)
            proceed()

        // YIG SNAKEBITE — enemy assigns extra kill
        case YigSnakebiteAssignAction(self, yigOwner, uc) =>
            val u = self.forces.%(_.uclass == uc).%(u => canAssignKills(u) > 0).sortBy(_.uclass.cost).head
            assignKill(u)
            log(u.uclass.styled(self), "was", "killed".styled("kill"), "by", "Snakebite".styled("nt"))
            if (azathothNeedsKillRoll) {
                azathothNeedsKillRoll = false
                if (game.nextReplayActionHint.exists(h => h.contains("AzathothDaemonSultanKillRoll"))) {
                    RollD6(_ => "Daemon Sultan — roll for Azathoth glyph reduction", roll => AzathothDaemonSultanKillRollAction(u.faction, roll))
                }
                else {
                    val roll = (1::2::3::4::5::6).shuffleSeed(game.azathothGlyphPosition * 7 + 31).first
                    game.azathothGlyphPosition -= roll
                    log("Azathoth Daemon Sultan".styled("nt") + ":", "Azathoth".styled(u.faction), "hit — rolled", s"$roll, glyph now at", game.azathothGlyphPosition)
                    if (game.azathothGlyphPosition <= 0) {
                        game.azathothGlyphPosition = 0
                        log("Azathoth".styled(u.faction), "glyph reached 0 — eliminated!")
                        u.health = Killed
                    }
                    jump(HarbingerKillPhase)
                }
            }
            else
                jump(HarbingerKillPhase)

        // CTHUGHA COMBAT CHOICE (pre-battle)
        case CthughaCombatChooseGOOAction(self, enemy, goo, combat) =>
            val mySide = if (self == attacker) attackers else defenders
            mySide.str += combat
            mySide.cthughaCombatBonus = combat
            log(self, "Cthugha".styled("nt") + ": combat =", combat, "(matching", goo.styled(enemy) + ")")
            jump(PreRoll)

        // DIRECTED ENERGY (DS alternate post-battle)
        case DirectedEnergyPostBattleAction(self, n) =>
            self.power += n
            self.oncePerTurn :+= DirectedEnergy
            self.log("gained", n.power, "from", DirectedEnergy.styled(self), "(" + n + " Chaos Gates)")
            proceed()

        case DirectedEnergySkipAction(self) =>
            // Skipping Directed Energy must NOT flip the spellbook facedown: the facedown flip
            // is the cost of USING the ability. Declining leaves it face-up/available for a later
            // battle this turn. The per-battle re-trigger guard is the Side.effects tag added at
            // the offer site (DS.add(DirectedEnergy)), not oncePerTurn, so removing the flip here
            // is safe within the current battle.
            proceed()

        // FIENDISH SPAWN (DS alternate pre-battle)
        case FiendishSpawnPreBattleAction(self) =>
            val pool = self.pool(LarvaThesis) ++ self.pool(LarvaAntithesis) ++ self.pool(LarvaSynthesis)
            val types = pool./(_.uclass).distinct
            Ask(self).each(types)(uc => FiendishSpawnChooseAction(self, uc, 0)).add(FiendishSpawnDoneAction(self))

        case FiendishSpawnChooseAction(self, uc, placed) =>
            val u = self.pool(uc).head
            u.region = arena
            // Add to battle forces so the larva participates in combat
            self.forces :+= u
            self.log("placed", uc.styled(self), "in", arena, "via", FiendishSpawn.styled(self))
            val totalPlaced = placed + 1
            if (totalPlaced >= 2) {
                // Max 2 larva placed
                self.add(FiendishSpawn)
                self.oncePerTurn :+= FiendishSpawn
                proceed()
            } else {
                // Can place one more
                val pool = self.pool(LarvaThesis) ++ self.pool(LarvaAntithesis) ++ self.pool(LarvaSynthesis)
                if (pool.any) {
                    val types = pool./(_.uclass).distinct
                    Ask(self).each(types)(uc2 => FiendishSpawnChooseAction(self, uc2, totalPlaced)).add(FiendishSpawnDoneAction(self))
                } else {
                    self.add(FiendishSpawn)
                    self.oncePerTurn :+= FiendishSpawn
                    proceed()
                }
            }

        case FiendishSpawnDoneAction(self) =>
            self.add(FiendishSpawn)
            self.oncePerTurn :+= FiendishSpawn
            proceed()

        // DEMAND SACRIFICE
        case DemandSacrificePreBattleAction(self) =>
            Ask(self.opponent)
                .add(DemandSacrificeKillsArePainsAction(self.opponent).as("Rolled", "Kills".styled("kill"), "become", "Pains".styled("pain"))(DemandSacrifice))
                .add(DemandSacrificeProvideESAction(self.opponent).as(self, "gains", 1.es)(DemandSacrifice))

        case DemandSacrificeProvideESAction(self) =>
            self.opponent.takeES(1)
            self.opponent.add(DemandSacrifice)
            self.opponent.log("got", 1.es, "from", DemandSacrifice)
            proceed()

        case DemandSacrificeKillsArePainsAction(self) =>
            self.add(KillsArePains)
            self.log("will roll", "Kills".styled("kill"), "as", "Pains".styled("pain"), "due to", DemandSacrifice)
            proceed()

        // HOWL
        case HowlPreBattleAction(self) =>
            self.add(Howl)

            val e = self.opponent
            val l = e.forces.%(_.canBeMoved)

            Ask(e.real.?(e).|(self)).each(e.forces.%(_.canBeMoved))(u => HowlUnitAction(e, u).as(u)("Retreat unit from", Howl))

        case HowlUnitAction(self, u) =>
            Ask(self).each(arena.connectedForRetreat)(r => HowlAction(self, u, r).as(r)("Retreat", u.ref.full, "to"))

        case HowlAction(self, u, r) =>
            self.forces :-= u
            game.fbSuppressCGForPlacement = true
            u.region = r
            game.fbSuppressCGForPlacement = false
            u.onGate = false
            log(u, "was howled to", r)
            proceed()

        // HARBINGER
        case HarbingerPowerAction(self, u, n) =>
            self.power += n
            self.log("got", n.power, "as", Harbinger)

            HarbingerAction(self, u)

        case HarbingerESAction(self, u, e) =>
            self.takeES(e)
            self.log("gained", e.es, "as", Harbinger)

            HarbingerAction(self, u)

        case HarbingerAction(self, u) =>
            u.add(Harbinged)

            if (u.uclass == Nyogtha)
                u.faction.forces(Nyogtha).but(u).foreach(_.add(Harbinged))

            proceed()

        // NECROPHAGY
        case NecrophagyAction(self, u, r) =>
            self.oncePerAction :+= Necrophagy

            // Parallel-guide Fix 38: Necrophagy is a Pain-driven forced move; it must NOT trigger
            // FB Cyclopean Gaze.
            game.fbSuppressCGForPlacement = true
            u.region = arena
            game.fbSuppressCGForPlacement = false
            exempt(u)
            sides.foreach(_.rolls :+= Pain)
            log(u, "came from", "" + r + ",", "causing additonal", Pain, "to both sides")

            proceed()

        // ETERNAL
        case EternalPayAction(self, u, result) =>
            self.power -= 1
            u.health = Spared(Alive)
            self.log("payed", 1.power, "for", Eternal, "to cancel", result, "on", u)
            proceed()

        // BERSERKERGANG
        case BerserkergangAction(self, n, u) =>
            eliminate(u)
            log(u, "was eliminated with", Berserkergang)
            if (n > 1)
                Ask(self).each(self.forces.vulnerable)(t => BerserkergangAction(self, n - 1, t))
            else
                proceed()

         case CannibalismAction(self, r, uc) =>
             self.log("spawned", uc.styled(self), "in", r, "with", Cannibalism)
             self.place(uc, r)
             proceed()

        // CHANNEL POWER
        case ChannelPowerAction(self, n) =>
            self.add(ChannelPower)
            self.power -= 1
            self.rolls = self.rolls.%(_ != Miss)
            self.log("rerolled", (n > 0).?("Misses").|("Miss").styled("miss"), "with", ChannelPower)
            RollBattle(self, "" + ChannelPower, n, x => BattleRollAction(self, x, ChannelPowerPhase))

        // MILLION FAVORED ONES
        case MillionFavoredOnesAction(self, r, uc, nw) =>
            self.add(MillionFavoredOnes)
            val t = self.forces(uc).%(_.region == r).first
            exempt(t)
            game.eliminate(t)
            nw.foreach(n => self.place(n, r))
            self.log("promoted", t, "in", r, "to", nw./(_.styled(self)).mkString(", "))
            proceed()

        case MillionFavoredOnesXAction(self, r, u, nw) =>
            self.add(MillionFavoredOnes)
            exempt(u)
            // MF1 is a REPLACEMENT, not a kill/elimination — move to reserve directly
            // without calling game.eliminate() which triggers Death March, Passion, etc.
            u.region = self.reserve
            u.onGate = false
            u.health = Alive
            u.state = $
            nw.foreach(n => self.place(n, r))
            self.log("promoted", u, "in", r, "to", nw./(_.styled(self)).mkString(", "))
            proceed()

        // UNHOLY GROUND
        case UnholyGroundAction(self, o, cr) =>
            self.add(UnholyGround)
            game.cathedrals :-= cr
            log("Cathedral".styled(self), "in", cr, "was removed with", UnholyGround)
            Ask(o)
                .each(o.forces.goos.distinctBy(_.uclass))(u => UnholyGroundEliminateAction(o, self, u).as(u)(UnholyGround, "eliminates in", arena))
                .bail(Then(BattleDoneAction(self)))

        case UnholyGroundEliminateAction(self, f, u) =>
            // Round 8 Bug 68: log BEFORE eliminate. For IGOO units (Nyogtha,
            // Tulzscha, Ygolonac), `IGOOsExpansion.eliminate()` actually REMOVES
            // the unit from `f.units` (it doesn't just move it to reserve like
            // normal eliminate). So if we logged after eliminating, the log's
            // attempt to render the UnitRef back to a unit (`Game.unit(ur)`)
            // would throw `None.get` from `.only` because the unit is no longer
            // in `f.units`. Pre-format the log line first, then eliminate.
            if (u.uclass == Nyogtha && self.all(Nyogtha).num > 1) {
                log("All", u, "were eliminated with", UnholyGround)

                self.forces(Nyogtha).foreach(eliminate)

                self.all(Nyogtha).foreach(game.eliminate)
            }
            else {
                log(u, "was eliminated with", UnholyGround)

                eliminate(u)
            }

            proceed()

        // SHRIVELING
        case ShrivelingPreBattleAction(self) =>
            Ask(self).each(self.opponent.forces.vulnerable.sortP)(u => ShrivelingAction(self, u).as(u)(Shriveling)).cancel

        case ShrivelingAction(self, u) =>
            self.add(Shriveling)

            val p = u.cultist.?(self.opponent.recruitCost(u.uclass, arena)).|(self.opponent.summonCost(u.uclass, arena))

            eliminate(u)

            if (self.opponent.real) {
                self.opponent.power += p

                log(u, "was shriveled and", self.opponent, "got", p.power)
            }
            else
                log(u, "was shriveled")

            proceed()

        // COSMIC UNITY
        case CosmicUnityPreBattleAction(self) =>
            Ask(self).each(self.opponent.forces.goos.distinctBy(_.uclass).sortA)(u => CosmicUnityAction(self, u).as(u)(CosmicUnity, "unites")).cancel

        case CosmicUnityAction(self, u) =>
            self.add(CosmicUnity)

            if (u.uclass == Nyogtha && self.opponent.forces(Nyogtha).num > 1) {
                self.opponent.forces(Nyogtha).foreach(_.add(Zeroed))
                self.log("targeted both", Nyogtha.styled(u.faction), "units with", CosmicUnity.styled(self))
            } else {
                u.add(Zeroed)
                self.log("targeted", u, "with", CosmicUnity.styled(self))
            }

            proceed()

        // Firstborn (FB) Augury in battle: replace n Miss dice with Kills from the augury pool
        case FBAuguryBattleReplaceAction(self, n) =>
            val fbSide : Side = if (attacker == FB) attackers else defenders
            var remaining = n
            fbSide.rolls = fbSide.rolls.map { r =>
                if (r == Miss && remaining > 0) { remaining -= 1; Kill }
                else r
            }
            // Skip back to PostRoll re-entry would loop; jump directly to kill assignment
            jump(AssignDefenderKills)

        // Firstborn (FB) Augury: player declined to use augury in battle
        case FBAuguryBattleCancelAction(self) =>
            jump(AssignDefenderKills)

        // Firstborn (FB) Carnage post-battle: FBExpansion handles the Ask/ES logic and returns
        // UnknownContinue; these cases resume battle flow by calling proceed()
        case FBCarnagePayPowerAction(self) =>
            proceed()

        case FBCarnageChooseSpellbookAction(self, _) =>
            proceed()

        case FBCarnageCancelAction(self) =>
            proceed()

        // Bug fix Round 4 (Bug 1): Cyclopean Gaze battle-mode dispatchers. The FB expansion
        // handles the actual chain (FBCyclopeanGazePhaseAction etc.); we just resume battle
        // flow when the marker FBCyclopeanGazeBattleDoneAction fires at the end of the chain.
        case FBCyclopeanGazeBattleDoneAction(self) =>
            proceed()

        // TT TERROR: pre-battle choice — reduce enemy OR boost own combat (1 per Proto-Shoggoth)
        case TTTerrorPreBattleAction(self) =>
            val n = (self : Side).forces(ProtoShoggoth).not(Zeroed).num
            Ask(self)
                .add(TTTerrorReduceEnemyAction(self, n))
                .add(TTTerrorBoostOwnAction(self, n))

        case TTTerrorReduceEnemyAction(self, n) =>
            (self : Side).add(TTReduceEnemyCombat)
            self.log(TerrorSB.styled(TT), ": will reduce enemy combat by", n)
            proceed()

        case TTTerrorBoostOwnAction(self, n) =>
            (self : Side).add(TTBoostOwnCombat)
            self.log(TerrorSB.styled(TT), ": will boost own combat by", n)
            proceed()

        // TT FULMINATION: offer when Ubbo-Sathla is assigned a Kill in battle
        case TTFulminationOfferAction(self, totalKills) =>
            Ask(self)
                .add(TTFulminationTakeAction(self, totalKills))
                .add(TTFulminationDeclineAction(self))

        case TTFulminationTakeAction(self, totalKills) =>
            TTExpansion.ttUbboFulminated = true
            game.factions.%(f => f == self).foreach { f =>
                f.units = f.units.%(u => u.uclass != UbboSathla)
            }
            self.takeES(totalKills)
            self.log(Fulmination.styled(TT), ": removed", UbboSathla.styled(TT), "permanently for", totalKills.es)
            proceed()

        case TTFulminationDeclineAction(self) =>
            self.log(Fulmination.styled(TT), ": declined")
            proceed()

    }
}
