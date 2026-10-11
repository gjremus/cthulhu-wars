package cws

import hrf.colmat._
import html._

// ============================================================================
// PERMEATION (PRM) — Homebrew faction
// Designer: zehk (faction id icyvsw2jgh)
// Faction color #9d1b0c (deep red), style "prm".
// Led by the Great Old One Ycgn'àgnnisssz. A faction focused on sharing power
// and abilities with other factions, with scaling monsters and spellbook manipulation.
//
// IMPLEMENTATION NOTES
//  • Celestial Fledgling has variable Cost and Combat = number of Fledglings in
//    play plus one. This is computed dynamically in cost() and strength().
//  • Ycgn'àgnnisssz has variable Combat = owner's earned spellbooks + all
//    face-down spellbooks across ALL factions.
//  • Mother's Aid allows opponents to pay PRM 1 Power to control Ycgn'àgnnisssz
//    for their turn. The control state is tracked on Game.scala as
//    game.prmYcgnaTemporaryController : |[Faction] (set at turn start, cleared at turn end).
//  • Make as One (unique ability) allows enemies to use PRM's spellbooks. When
//    this causes a flip, PRM gains 1 Doom. Tracked via prmMakeAsOneFlips.
//  • Face-down spellbooks are temporarily flipped down until Doom Phase, then
//    automatically restored. Tracked in game.prmFlippedDownSpellbooks : $[Spellbook].
// ============================================================================


// ── UNITS (design.units) ────────────────────────────────────────────────────
// PRM uses the standard Acolyte UnitClass for its 6 Acolytes. Astral Egg: Monster
// cost 1, combat 1. Celestial Fledgling: Monster with VARIABLE cost/combat =
// (count in play + 1). Ycgn'àgnnisssz: GOO with special awaken (replace Egg + 7 Power)
// and variable combat.
case object AstralEgg          extends FactionUnitClass(PRM, "Astral Egg",          Monster, 1)
case object CelestialFledgling extends FactionUnitClass(PRM, "Celestial Fledgling", Monster, 1) // base cost, overridden
case object Ycgnagnnisssz      extends FactionUnitClass(PRM, "Ycgn'àgnnisssz",      GOO,     7) // awaken cost


// ── SPELLBOOKS (design.sb) ──────────────────────────────────────────────────
case object AtomicUnity    extends FactionSpellbook(PRM, "Atomic Unity")
case object EldritchLove   extends FactionSpellbook(PRM, "Eldritch Love")
case object Osmosis        extends FactionSpellbook(PRM, "Osmosis")
case object Protogenesis   extends FactionSpellbook(PRM, "Protogenesis")
case object SpeakReality   extends FactionSpellbook(PRM, "Speak Reality")
case object ZealousSpark   extends FactionSpellbook(PRM, "Zealous Spark")

// PRM Unique Ability — Make as One (Ongoing). Declared as a FactionSpellbook so
// every reference uses the FCG-blessed MakeAsOne.styled(PRM) form and so it can
// be shown in the faction status panel / borrowed via Ancient Sorcery.
case object MakeAsOne extends FactionSpellbook(PRM, "Make as One")


// ── SPELLBOOK REQUIREMENTS (design.sbr) ─────────────────────────────────────
case object Control2LandGatesReq            extends Requirement("Control 2 Gates in Land Areas")
case object Pay2DoomFlipSpellbookReq        extends Requirement("As an Action, Pay 2 Doom and Flip one of your Face Up Spellbooks Face Down until the Doom Phase")
case object NoFledglingsInPoolReq           extends Requirement("Have no Celestial Fledglings in your Pool")
case object EnemyHas3SpellbooksReq          extends Requirement("Another Faction has 3 or more Spellbooks on its Faction Card")
case object YcgnaAttackerInBattleReq        extends Requirement("Ycgn'àgnnisssz is an Attacker in a Battle")
case object AwakenYcgnaReq                  extends Requirement("Awaken Ycgn'àgnnisssz")


// ============================================================================
// PERMEATION (PRM) FACTION OBJECT
// ============================================================================
case object PRM extends Faction { f =>
    def name  = "Permeation"
    def short = "PRM"
    def style = "prm"

    // Make as One is the always-on unique ability.
    override def abilities = $(MakeAsOne)
    override def library   = $(AtomicUnity, EldritchLove, Osmosis, Protogenesis, SpeakReality, ZealousSpark)
    override def requirements(options : $[GameOption]) =
        $(Control2LandGatesReq, Pay2DoomFlipSpellbookReq, NoFledglingsInPoolReq, EnemyHas3SpellbooksReq, YcgnaAttackerInBattleReq, AwakenYcgnaReq)

    // 6 Acolytes + 4 Astral Eggs + 4 Celestial Fledglings + 1 Ycgn'àgnnisssz
    val allUnits =
        6.times(Acolyte)            ++
        4.times(AstralEgg)          ++
        4.times(CelestialFledgling) ++
        1.times(Ycgnagnnisssz)

    // Ycgn'àgnnisssz awakens via a CUSTOM replacement procedure (replace an Astral Egg
    // + pay 7 Power), NOT the standard Power-cost awaken. Returning None here keeps
    // game.awakens() from offering Ycgn'àgnnisssz through the standard AwakenMainAction
    // path; the custom YcgnaAwakenMainAction is offered from PRM's MainAction instead.
    override def awakenCost(u : UnitClass, r : Region)(implicit game : Game) : |[Int] = None

    override def cost(u : UnitClass)(implicit game : Game) : Int = u match {
        case CelestialFledgling =>
            // Variable cost: number of Fledglings in play plus one
            f.allInPlay.%(_.uclass == CelestialFledgling).num + 1
        case _ => u.cost
    }

    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int = {
        // Ycgn'àgnnisssz: combat = owner's earned spellbooks + all face-down spellbooks
        val ycgnaCount = units.%(_.uclass == Ycgnagnnisssz).not(Zeroed).num
        val ycgnaStr = if (ycgnaCount > 0) {
            val earnedSBs = f.gained.num
            val allFlippedDown = game.factions./(_.flippedDown.num).sum
            earnedSBs + allFlippedDown
        } else 0

        // Celestial Fledgling: combat = number of Fledglings in play plus one
        val fledglings = units.%(_.uclass == CelestialFledgling).not(Zeroed)
        val fledglingsInPlay = f.allInPlay.%(_.uclass == CelestialFledgling).num
        val fledglingStr = fledglings.num * (fledglingsInPlay + 1)

        // Astral Eggs have combat 1
        val eggStr = units.%(_.uclass == AstralEgg).not(Zeroed).num

        // Acolytes contribute 0 combat
        ycgnaStr + fledglingStr + eggStr + neutralStrength(units, opponent)
    }
}


// ============================================================================
// PERMEATION (PRM) ACTION CASE CLASSES
// All Soft (navigation-only) sub-menus include Cancel; Hard (state-mutating)
// actions do NOT.
// ============================================================================

// ── AWAKEN YCGN'ÀGNNISSSZ (design.units yg4kP9qZ) ───────────────────────────
// Custom awaken: replace an Astral Egg + pay 7 Power
case class YcgnaAwakenMainAction(self : Faction)
    extends OptionFactionAction(("Awaken " + Ycgnagnnisssz.name).styled(PRM)) with MainQuestion with Soft
case class YcgnaAwakenPickEggAction(self : Faction, eggs : $[UnitRef])
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) = ("Awaken " + Ycgnagnnisssz.name).styled(PRM) + ": choose Astral Egg to replace"
}
case class YcgnaAwakenAction(self : Faction, egg : UnitRef)
    extends ForcedAction


// ── ATOMIC UNITY (design.sb oMygUypd) ───────────────────────────────────────
// Action, Cost 1: Eliminate a Monster to gain Power equal to its Cost minus one.
// Flip if gained 2+ Power.
case class AtomicUnityMainAction(self : Faction)
    extends OptionFactionAction(AtomicUnity.styled(PRM) + " (Cost " + 1.power + ")") with MainQuestion with Soft
case class AtomicUnityPickMonsterAction(self : Faction, monsters : $[UnitRef])
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) = AtomicUnity.styled(PRM) + ": choose Monster to Eliminate"
}
case class AtomicUnityAction(self : Faction, monster : UnitRef)
    extends ForcedAction


// ── ELDRITCH LOVE (design.sb bKgvlWki) ──────────────────────────────────────
// Action, Cost 1: Gain ES if you have 2+ Face Down Spellbooks. Flip this spellbook.
case class EldritchLoveAction(self : Faction)
    extends OptionFactionAction(EldritchLove.styled(PRM) + " (Cost " + 1.power + ")") with MainQuestion


// ── OSMOSIS (design.sb T-iXgz5Y) ────────────────────────────────────────────
// Action, Cost 2: Choose two adjacent Areas. Each faction with units in at least
// one of those Areas, beginning with you and proceeding in turn order, evenly
// distributes their Units present between those Areas.
case class OsmosisMainAction(self : Faction)
    extends OptionFactionAction(Osmosis.styled(PRM) + " (Cost " + 2.power + ")") with MainQuestion with Soft
case class OsmosisPickFirstAreaAction(self : Faction)
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) = Osmosis.styled(PRM) + ": choose first Area"
}
case class OsmosisPickSecondAreaAction(self : Faction, first : Region, adjacentToFirst : $[Region])
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) = Osmosis.styled(PRM) + ": choose second Area (adjacent to " + first + ")"
}
case class OsmosisDistributeAction(self : Faction, f : Faction, area1 : Region, area2 : Region, units : $[UnitRef], placed : $[(UnitRef, Region)])
    extends ForcedAction with PowerNeutral {
    override def question(implicit game : Game) = Osmosis.styled(PRM) + ": " + f.name + " distributes Units between " + area1 + " and " + area2
}
case class OsmosisPlaceUnitAction(self : Faction, f : Faction, area1 : Region, area2 : Region, u : UnitRef, remaining : $[UnitRef], placed : $[(UnitRef, Region)], destArea : Region)
    extends BaseFactionAction(implicit g => Osmosis.styled(PRM) + ": place " + g.unit(u).uclass.styled(f), destArea)
case class OsmosisCommitAction(self : Faction, area1 : Region, area2 : Region, distributions : $[(Faction, $[(UnitRef, Region)])])
    extends ForcedAction


// ── PROTOGENESIS (design.sb zYRp7m6e) ───────────────────────────────────────
// Action, Cost 2: Place the lowest cost Monster from your Pool into any Area
// already containing your Units. Flip if no Monsters remain in Pool.
case class ProtogenesisMainAction(self : Faction)
    extends OptionFactionAction(Protogenesis.styled(PRM) + " (Cost " + 2.power + ")") with MainQuestion with Soft
case class ProtogenesisPickAreaAction(self : Faction, monster : UnitRef)
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) = Protogenesis.styled(PRM) + ": place " + game.unit(monster).uclass.styled(PRM) + " in which Area?"
}
case class ProtogenesisAction(self : Faction, monster : UnitRef, dest : Region)
    extends BaseFactionAction(Protogenesis.styled(PRM) + ": place Monster in", dest)


// ── SPEAK REALITY (design.sb ASFx5E4-) ──────────────────────────────────────
// Action, Cost X: Choose a Faction with Units in Ycgn'àgnnisssz's Area and name
// a Common Action. They must spend at least as much power as you did performing
// that Action on their next turn if able. If you spend less than the required
// Cost, nothing happens.
case class SpeakRealityMainAction(self : Faction)
    extends OptionFactionAction(SpeakReality.styled(PRM) + " (Cost X)") with MainQuestion with Soft
case class SpeakRealityChoosePowerAction(self : Faction)
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) = SpeakReality.styled(PRM) + ": how much Power do you spend?"
}
case class SpeakRealityChooseFactionAction(self : Faction, powerSpent : Int, eligibleFactions : $[Faction])
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) = SpeakReality.styled(PRM) + ": choose Faction with Units in " + Ycgnagnnisssz.name + "'s Area"
}
case class SpeakRealityChooseActionAction(self : Faction, powerSpent : Int, targetFaction : Faction)
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) = SpeakReality.styled(PRM) + ": name a Common Action for " + targetFaction.name
}
case class SpeakRealityAction(self : Faction, powerSpent : Int, targetFaction : Faction, namedAction : String)
    extends ForcedAction


// ── ZEALOUS SPARK (design.sb L9gu0vGJ) ──────────────────────────────────────
// Unlimited Action, Cost 1 Doom: Move ANY Monster from its Area to an Adjacent Area.
case class ZealousSparkMainAction(self : Faction)
    extends OptionFactionAction(ZealousSpark.styled(PRM) + " (Cost " + 1.doom + ")") with MainQuestion with Soft
case class ZealousSparkPickMonsterAction(self : Faction, monsters : $[UnitRef])
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) = ZealousSpark.styled(PRM) + ": move which Monster?"
}
case class ZealousSparkPickDestAction(self : Faction, monster : UnitRef, adjacentAreas : $[Region])
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) = ZealousSpark.styled(PRM) + ": move " + game.unit(monster).uclass.styled(game.unit(monster).faction) + " in " + game.unit(monster).region + " to which Adjacent Area?"
}
case class ZealousSparkAction(self : Faction, monster : UnitRef, dest : Region)
    extends BaseFactionAction(implicit g => ZealousSpark.styled(PRM) + ": move " + g.unit(monster).uclass.styled(g.unit(monster).faction) + " to", dest)


// ── SPELLBOOK REQUIREMENT: PAY 2 DOOM AND FLIP (design.sbr aUZO1VDt) ────────
case class Pay2DoomFlipSpellbookMainAction(self : Faction)
    extends OptionFactionAction("Pay " + 2.doom + " and Flip a Spellbook".styled(PRM)) with MainQuestion with Soft
case class Pay2DoomFlipSpellbookPickAction(self : Faction, eligibleSBs : $[Spellbook])
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) = "Flip which Spellbook Face Down until the Doom Phase?".styled(PRM)
}
case class Pay2DoomFlipSpellbookAction(self : Faction, sb : Spellbook)
    extends ForcedAction


// ── MAKE AS ONE: ENEMY ACTIVATION (design.ufa + design.menus rYKsiRQF) ──────
// During an enemy's turn, they may activate one of PRM's face-up spellbooks by
// paying its Power cost. If this causes the spellbook to flip, PRM gains 1 Doom.
case class MakeAsOneOfferAction(self : Faction, enemy : Faction, eligibleSBs : $[Spellbook])
    extends ForcedAction with PowerNeutral {
    override def question(implicit game : Game) = MakeAsOne.styled(PRM) + ": activate which Spellbook by paying its Power Cost?"
}
case class MakeAsOneActivateAction(self : Faction, enemy : Faction, sb : Spellbook)
    extends OptionFactionAction(implicit g => MakeAsOne.styled(PRM) + ": activate " + sb.styled(PRM) + "?") { override def question(implicit game : Game) = MakeAsOne.styled(PRM) }
case class MakeAsOneSkipAction(self : Faction, enemy : Faction)
    extends OptionFactionAction(implicit g => MakeAsOne.styled(PRM) + ": skip") { override def question(implicit game : Game) = MakeAsOne.styled(PRM) }


// ── MOTHER'S AID: OPPONENT CONTROL (design.units yg4kP9qZ abilityText) ──────
// On their turn, any opponent may pay PRM 1 Power to control Ycgn'àgnnisssz this
// turn. Moving her costs 0 Power for them. While controlled by another player,
// Ycgn'àgnnisssz cannot be assigned Combat results or Eliminated.
case class MothersAidOfferAction(self : Faction, enemy : Faction)
    extends OptionFactionAction(implicit g => "Mother's Aid".styled(PRM) + ": pay " + PRM.name + " " + 1.power + " to control " + Ycgnagnnisssz.name + " this turn?") with MainQuestion
case class MothersAidActivateAction(self : Faction, enemy : Faction)
    extends OptionFactionAction(implicit g => "Mother's Aid".styled(PRM) + ": pay " + 1.power + " to control " + Ycgnagnnisssz.name) with MainQuestion


// ============================================================================
// PERMEATION (PRM) EXPANSION
// ============================================================================
object PRMExpansion extends Expansion {
    def name = "Permeation"
    def factions = $(PRM)

    // No special elimination hooks needed for PRM
    override def eliminate(g : Game, u : UnitFigure) {}

    // Restore flipped-down spellbooks at Doom Phase
    override def doom(g : Game, f : Faction) {
        if (f == PRM) {
            g.prmFlippedDownSpellbooks.foreach { sb =>
                if (f.flippedDown.contains(sb)) {
                    f.flippedDown :-= sb
                }
            }
            g.prmFlippedDownSpellbooks = $()
        }
    }

    // Clear temporary Ycgn'àgnnisssz controller at end of turn
    override def afterTurn(g : Game, f : Faction) {
        if (g.prmYcgnaTemporaryController.contains(f)) {
            g.prmYcgnaTemporaryController = None
        }
    }
}
