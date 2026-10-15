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
case object AstralEgg          extends FactionUnitClass(PRM, "Astral Egg",          Monster, PRMValues.AstralEgg_cost)
case object CelestialFledgling extends FactionUnitClass(PRM, "Celestial Fledgling", Monster, 1) // base cost, overridden
case object Ycgnagnnisssz      extends FactionUnitClass(PRM, "Ycgn'àgnnisssz",      GOO,     PRMValues.Ycgnagnnisssz_awakenCost) // awaken cost


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
case object Control2LandGatesReq            extends Requirement("Control " + PRMValues.Control2LandGatesReq_gates + " Gates in Land Areas")
case object Pay2DoomFlipSpellbookReq        extends Requirement("As an Action, Pay " + PRMValues.Pay2DoomFlipSpellbookReq_doom + " Doom and Flip one of your Face Up Spellbooks Face Down until the Doom Phase")
case object NoFledglingsInPoolReq           extends Requirement("Have no Celestial Fledglings in your Pool")
case object EnemyHas3SpellbooksReq          extends Requirement("Another Faction has " + PRMValues.EnemyHas3SpellbooksReq_spellbooks + " or more Spellbooks on its Faction Card")
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
        PRMValues.Acolyte_count.times(Acolyte)                       ++
        PRMValues.AstralEgg_count.times(AstralEgg)                   ++
        PRMValues.CelestialFledgling_count.times(CelestialFledgling) ++
        PRMValues.Ycgnagnnisssz_count.times(Ycgnagnnisssz)

    // Ycgn'àgnnisssz awakens via a CUSTOM replacement procedure (replace an Astral Egg
    // + pay 7 Power), NOT the standard Power-cost awaken. Returning None here keeps
    // game.awakens() from offering Ycgn'àgnnisssz through the standard AwakenMainAction
    // path; the custom YcgnaAwakenMainAction is offered from PRM's MainAction instead.
    override def awakenCost(u : UnitClass, r : Region)(implicit game : Game) : |[Int] = None

    // Override summonCost to handle Celestial Fledgling's variable cost
    override def summonCost(u : UnitClass, r : Region)(implicit game : Game) : Int = u match {
        case CelestialFledgling =>
            // Variable cost: number of Fledglings in play plus one
            f.allInPlay.%(_.uclass == CelestialFledgling).num + 1
        case _ => u.cost
    }

    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int = {
        // Ycgn'àgnnisssz: combat = owner's earned spellbooks + all face-down spellbooks
        val ycgnaCount = units.%(_.uclass == Ycgnagnnisssz).not(Zeroed).num
        val ycgnaStr = if (ycgnaCount > 0) {
            // Count of earned spellbooks (on faction card)
            val earnedSBs = f.spellbooks.num
            // Count ALL face-down spellbooks across all factions:
            // - PRM's temporarily flipped-down spellbooks
            val prmFlippedDown = game.prmFlippedDownSpellbooks.num
            // - DC's Dark Bargain (if face-down this round)
            val dcFlippedDown = game.dcDarkBargainFacedown.?(1).|(0)
            earnedSBs + prmFlippedDown + dcFlippedDown
        } else 0

        // Celestial Fledgling: combat = number of Fledglings in play plus one
        val fledglings = units.%(_.uclass == CelestialFledgling).not(Zeroed)
        val fledglingsInPlay = f.allInPlay.%(_.uclass == CelestialFledgling).num
        val fledglingStr = fledglings.num * (fledglingsInPlay + 1)

        // Astral Eggs have combat 1
        val eggStr = units.%(_.uclass == AstralEgg).not(Zeroed).num * PRMValues.AstralEgg_combat

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
    extends OptionFactionAction(AtomicUnity.styled(PRM) + " (Cost " + PRMValues.AtomicUnity_cost.power + ")") with MainQuestion with Soft
case class AtomicUnityPickMonsterAction(self : Faction, monsters : $[UnitRef])
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) = AtomicUnity.styled(PRM) + ": choose Monster to Eliminate"
}
case class AtomicUnityAction(self : Faction, monster : UnitRef)
    extends ForcedAction


// ── ELDRITCH LOVE (design.sb bKgvlWki) ──────────────────────────────────────
// Action, Cost 1: Gain ES if you have 2+ Face Down Spellbooks. Flip this spellbook.
case class EldritchLoveAction(self : Faction)
    extends OptionFactionAction(EldritchLove.styled(PRM) + " (Cost " + PRMValues.EldritchLove_cost.power + ")") with MainQuestion


// ── OSMOSIS (design.sb T-iXgz5Y) ────────────────────────────────────────────
// Action, Cost 2: Choose two adjacent Areas. Each faction with units in at least
// one of those Areas, beginning with you and proceeding in turn order, evenly
// distributes their Units present between those Areas.
case class OsmosisMainAction(self : Faction)
    extends OptionFactionAction(Osmosis.styled(PRM) + " (Cost " + PRMValues.Osmosis_cost.power + ")") with MainQuestion with Soft
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
    extends OptionFactionAction(Protogenesis.styled(PRM) + " (Cost " + PRMValues.Protogenesis_cost.power + ")") with MainQuestion with Soft
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
    extends OptionFactionAction(ZealousSpark.styled(PRM) + " (Cost " + PRMValues.ZealousSpark_doomCost.doom + ")") with MainQuestion with Soft
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
    extends OptionFactionAction("Pay " + PRMValues.Pay2DoomFlipSpellbookReq_doom.doom + " and Flip a Spellbook".styled(PRM)) with MainQuestion with Soft
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
    extends OptionFactionAction(implicit g => "Mother's Aid".styled(PRM) + ": pay " + PRM.name + " " + PRMValues.MothersAid_power.power + " to control " + Ycgnagnnisssz.name + " this turn?") with MainQuestion
case class MothersAidActivateAction(self : Faction, enemy : Faction)
    extends OptionFactionAction(implicit g => "Mother's Aid".styled(PRM) + ": pay " + PRMValues.MothersAid_power.power + " to control " + Ycgnagnnisssz.name) with MainQuestion


// ============================================================================
// PERMEATION (PRM) EXPANSION
// ============================================================================
// Battle mechanics:
// - Celestial Fledgling combat: each gets (# in play + 1) strength
// - Ycgn'àgnnisssz combat: owner's earned SBs + ALL face-down SBs (PRM + DC if present)
// - Mother's Aid protection: when controlled by enemy, Ycgn'àgnnisssz cannot be assigned
//   combat results or eliminated (requires combat phase integration)
object PRMExpansion extends Expansion {
    def name = "Permeation"
    def factions = $(PRM)

    override def afterAction()(implicit game : Game) : Unit = {
        if (!game.setup.has(PRM)) return

        // Check for No Fledglings in Pool requirement after any action
        val fledglingsInPool = PRM.pool.%(_.uclass == CelestialFledgling)
        if (fledglingsInPool.none && PRM.allInPlay.%(_.uclass == CelestialFledgling).any) {
            PRM.satisfy(NoFledglingsInPoolReq, "Have no Celestial Fledglings in Pool")
        }
    }

    def perform(a : Action, soft : VoidGuard)(implicit game : Game) : Continue = {
        if (!game.setup.has(PRM)) return null

        a match {
            // ── SETUP ────────────────────────────────────────────────
            case SetupFactionsAction if !game.starting.contains(PRM) =>
                val otherStarts = game.factions.but(PRM)./~(fx => game.starting.get(fx).toList)
                val seaAreas = game.board.regions.%(_.glyph == Ocean)
                val emptySeaAreas = seaAreas.%(r => !otherStarts.contains(r) && game.board.connected(r).%(otherStarts.contains).none)

                implicit val asking = Asking(PRM)
                if (emptySeaAreas.any)
                    Ask(PRM, emptySeaAreas./(r => StartingRegionAction(PRM, r).as(r)(PRM, "starts in")))
                else
                    Ask(PRM, seaAreas.%(r => !otherStarts.contains(r))./(r => StartingRegionAction(PRM, r).as(r)(PRM, "starts in")))

            // ── YCGN'ÀGNNISSSZ AWAKEN ──────────────────────────────────
            case YcgnaAwakenMainAction(f) =>
                val eggs = f.allInPlay.%(_.uclass == AstralEgg)
                if (eggs.none) {
                    f.log("No", AstralEgg.styled(f), "in play")
                    EndAction(f)
                } else if (f.power < PRMValues.Ycgnagnnisssz_awakenCost) {
                    f.log("Not enough Power to awaken", Ycgnagnnisssz.styled(f))
                    EndAction(f)
                } else {
                    implicit val asking = Asking(f)
                    Ask(f).each(eggs)(u => YcgnaAwakenAction(f, u.ref))
                }

            case YcgnaAwakenAction(f, egg) =>
                val eggFig = game.unit(egg)
                val region = eggFig.region
                f.log("Awakened", Ycgnagnnisssz.styled(f), "in", region, "replacing", eggFig.uclass.styled(f))
                f.power -= PRMValues.Ycgnagnnisssz_awakenCost
                game.eliminate(eggFig)
                val goo = f.goo(Ycgnagnnisssz)
                goo.region = region
                f.satisfy(AwakenYcgnaReq, "Awakened Ycgn'àgnnisssz")
                Then(EndAction(f))

            // ── ATOMIC UNITY ────────────────────────────────────────────
            case AtomicUnityMainAction(f) =>
                val monsters = f.allInPlay.%(_.uclass.utype == Monster)
                if (monsters.none) {
                    f.log("No Monsters in play")
                    EndAction(f)
                } else if (f.power < PRMValues.AtomicUnity_cost) {
                    f.log("Not enough Power for", AtomicUnity.styled(f))
                    EndAction(f)
                } else {
                    implicit val asking = Asking(f)
                    Ask(f, $(AtomicUnityPickMonsterAction(f, monsters./(_.ref))))
                }

            case AtomicUnityPickMonsterAction(f, monsters) =>
                implicit val asking = Asking(f)
                Ask(f).each(monsters)(m => AtomicUnityAction(f, m))

            case AtomicUnityAction(f, monsterRef) =>
                val monster = game.unit(monsterRef)
                val monsterCost = f.summonCost(monster.uclass, monster.region)
                val powerGained = math.max(0, monsterCost - 1)

                f.log(AtomicUnity.styled(f) + ":", "Eliminated", monster.uclass.styled(f), "in", monster.region)
                f.power -= PRMValues.AtomicUnity_cost
                game.eliminate(monster)

                if (powerGained > 0) {
                    f.power += powerGained
                    f.log("Gained", powerGained.power)
                }

                // Flip if gained PRMValues.AtomicUnity_flipPower or more Power
                if (powerGained >= PRMValues.AtomicUnity_flipPower) {
                    f.oncePerAction :+= AtomicUnity
                    game.prmFlippedDownSpellbooks :+= AtomicUnity
                    f.log(AtomicUnity.styled(f), "flipped Face Down until Doom Phase")
                }

                Then(EndAction(f))

            // ── PROTOGENESIS ────────────────────────────────────────────
            case ProtogenesisMainAction(f) =>
                val poolMonsters = f.pool.%(_.uclass.utype == Monster)
                if (poolMonsters.none) {
                    f.log("No Monsters in pool")
                    EndAction(f)
                } else if (f.power < PRMValues.Protogenesis_cost) {
                    f.log("Not enough Power for", Protogenesis.styled(f))
                    EndAction(f)
                } else {
                    // Find lowest-cost monster
                    // Use ocean as reference region for cost calculation since region doesn't matter for finding minimum
                    val oceanRef = game.board.regions.%(_.glyph == Ocean).head
                    val lowestCostMonster = poolMonsters.minBy(u => f.summonCost(u.uclass, oceanRef))

                    val areasWithUnits = f.allInPlay./(_.region).distinct
                    if (areasWithUnits.none) {
                        f.log("No Areas with", PRM.name, "Units")
                        EndAction(f)
                    } else {
                        implicit val asking = Asking(f)
                        Ask(f, $(ProtogenesisPickAreaAction(f, lowestCostMonster.ref)))
                    }
                }

            case ProtogenesisPickAreaAction(f, monsterRef) =>
                val areasWithUnits = f.allInPlay./(_.region).distinct
                implicit val asking = Asking(f)
                Ask(f).each(areasWithUnits)(r => ProtogenesisAction(f, monsterRef, r))

            case ProtogenesisAction(f, monsterRef, dest) =>
                val monster = game.unit(monsterRef)
                f.power -= PRMValues.Protogenesis_cost
                monster.region = dest
                f.log(Protogenesis.styled(f) + ":", "placed", monster.uclass.styled(f), "in", dest)

                // Flip if no monsters remain in pool
                val remainingPoolMonsters = f.pool.%(_.uclass.utype == Monster)
                if (remainingPoolMonsters.none) {
                    f.oncePerAction :+= Protogenesis
                    game.prmFlippedDownSpellbooks :+= Protogenesis
                    f.log(Protogenesis.styled(f), "flipped Face Down until Doom Phase")
                }

                // Check for No Fledglings in Pool requirement
                val fledglingsInPool = f.pool.%(_.uclass == CelestialFledgling)
                if (fledglingsInPool.none) {
                    f.satisfy(NoFledglingsInPoolReq, "Have no Celestial Fledglings in Pool")
                }

                Then(EndAction(f))

            // ── ZEALOUS SPARK ───────────────────────────────────────────
            case ZealousSparkMainAction(f) =>
                if (f.doom >= PRMValues.ZealousSpark_doomCost) {
                    // Find ALL monsters on the map (any faction)
                    val allMonsters = game.factions./~(fx => fx.allInPlay.%(_.uclass.utype == Monster)./(_.ref))
                    if (allMonsters.none) {
                        f.log("No Monsters on the map")
                        EndAction(f)
                    } else {
                        implicit val asking = Asking(f)
                        Ask(f, $(ZealousSparkPickMonsterAction(f, allMonsters)))
                    }
                } else {
                    f.log("No Doom for", ZealousSpark.styled(f))
                    EndAction(f)
                }

            case ZealousSparkPickMonsterAction(f, monsters) =>
                implicit val asking = Asking(f)
                Ask(f).each(monsters)(m => {
                    val monster = game.unit(m)
                    val adjacentAreas = game.board.connected(monster.region)
                    ZealousSparkPickDestAction(f, m, adjacentAreas)
                })

            case ZealousSparkPickDestAction(f, monsterRef, adjacentAreas) =>
                implicit val asking = Asking(f)
                Ask(f).each(adjacentAreas)(r => ZealousSparkAction(f, monsterRef, r))

            case ZealousSparkAction(f, monsterRef, dest) =>
                val monster = game.unit(monsterRef)
                val origin = monster.region
                f.doom -= PRMValues.ZealousSpark_doomCost
                monster.region = dest
                f.log(ZealousSpark.styled(f) + ":", "moved", monster.uclass.styled(monster.faction), "from", origin, "to", dest)
                Then(EndAction(f))

            // ── OSMOSIS ─────────────────────────────────────────────────
            case OsmosisMainAction(f) =>
                if (f.power < PRMValues.Osmosis_cost) {
                    f.log("Not enough Power for", Osmosis.styled(f))
                    EndAction(f)
                } else {
                    implicit val asking = Asking(f)
                    Ask(f, $(OsmosisPickFirstAreaAction(f)))
                }

            case OsmosisPickFirstAreaAction(f) =>
                implicit val asking = Asking(f)
                Ask(f).each(game.board.regions)(r => OsmosisPickSecondAreaAction(f, r, game.board.connected(r)))

            case OsmosisPickSecondAreaAction(f, first, adjacentToFirst) =>
                implicit val asking = Asking(f)
                Ask(f).each(adjacentToFirst)(r => {
                    // Find all factions with units in either area
                    val factionsWithUnits = game.factions.%(fx => fx.allInPlay.%(u => u.region == first || u.region == r).any)
                    // Start with PRM, then continue in turn order
                    val orderedFactions = ($(f) ++ game.factions.but(f)).%(factionsWithUnits.contains)

                    if (orderedFactions.any) {
                        val firstFaction = orderedFactions.head
                        val units = firstFaction.allInPlay.%(u => u.region == first || u.region == r)./(_.ref)
                        OsmosisDistributeAction(f, firstFaction, first, r, units, $())
                    } else {
                        f.log(Osmosis.styled(f) + ":", "no Units to distribute")
                        EndAction(f)
                    }
                })

            case OsmosisDistributeAction(self, fx, area1, area2, units, placed) =>
                // Deduct power on first execution (when this is the first faction starting distribution)
                if (fx == self && placed.none) {
                    self.power -= PRMValues.Osmosis_cost
                }

                if (units.none) {
                    // Done with this faction, move to next
                    val factionsWithUnits = game.factions.%(f => f.allInPlay.%(u => u.region == area1 || u.region == area2).any)
                    val orderedFactions = ($(self) ++ game.factions.but(self)).%(factionsWithUnits.contains)
                    val currentIndex = orderedFactions.indexOf(fx)

                    if (currentIndex + 1 < orderedFactions.num) {
                        val nextFaction = orderedFactions(currentIndex + 1)
                        val nextUnits = nextFaction.allInPlay.%(u => u.region == area1 || u.region == area2)./(_.ref)
                        // Collect placements
                        val distributions = $()  // This would need to be accumulated properly
                        Force(OsmosisDistributeAction(self, nextFaction, area1, area2, nextUnits, $()))
                    } else {
                        // All factions done
                        self.log(Osmosis.styled(self) + ":", "distribution complete")
                        Force(EndAction(self))
                    }
                } else {
                    // Place next unit
                    implicit val asking = Asking(fx)
                    val unit = units.head
                    val remaining = units.drop(1)
                    Ask(fx).add(OsmosisPlaceUnitAction(self, fx, area1, area2, unit, remaining, placed, area1))
                             .add(OsmosisPlaceUnitAction(self, fx, area1, area2, unit, remaining, placed, area2))
                }

            case OsmosisPlaceUnitAction(self, fx, area1, area2, u, remaining, placed, destArea) =>
                val unit = game.unit(u)
                unit.region = destArea
                val newPlaced = placed :+ (u, destArea)
                fx.log("Placed", unit.uclass.styled(fx), "in", destArea)
                Force(OsmosisDistributeAction(self, fx, area1, area2, remaining, newPlaced))

            case OsmosisCommitAction(self, area1, area2, distributions) =>
                // This action would commit all the moves at once
                // For now, moves are done immediately in OsmosisPlaceUnitAction
                Then(EndAction(self))

            // ── PAY 2 DOOM AND FLIP SPELLBOOK ───────────────────────────
            case Pay2DoomFlipSpellbookMainAction(f) =>
                if (f.doom < PRMValues.Pay2DoomFlipSpellbookReq_doom) {
                    f.log("Not enough Doom to flip a Spellbook")
                    EndAction(f)
                } else {
                    // Find spellbooks that are earned but not already flipped
                    val eligibleSBs = f.spellbooks.%(sb => !f.oncePerAction.has(sb) && !game.prmFlippedDownSpellbooks.has(sb))
                    if (eligibleSBs.none) {
                        f.log("No Spellbooks available to flip")
                        EndAction(f)
                    } else {
                        implicit val asking = Asking(f)
                        Ask(f, $(Pay2DoomFlipSpellbookPickAction(f, eligibleSBs)))
                    }
                }

            case Pay2DoomFlipSpellbookPickAction(f, eligibleSBs) =>
                implicit val asking = Asking(f)
                Ask(f).each(eligibleSBs)(sb => Pay2DoomFlipSpellbookAction(f, sb))

            case Pay2DoomFlipSpellbookAction(f, sb) =>
                f.doom -= PRMValues.Pay2DoomFlipSpellbookReq_doom
                f.oncePerAction :+= sb
                game.prmFlippedDownSpellbooks :+= sb
                f.log("Paid", PRMValues.Pay2DoomFlipSpellbookReq_doom.doom, "to flip", sb.styled(f), "Face Down until Doom Phase")
                f.satisfy(Pay2DoomFlipSpellbookReq, "Paid " + PRMValues.Pay2DoomFlipSpellbookReq_doom + " Doom and Flipped Spellbook")
                Then(EndAction(f))

            // ── SPEAK REALITY ───────────────────────────────────────────
            case SpeakRealityMainAction(f) =>
                val ycgna = f.goo(Ycgnagnnisssz)
                if (ycgna.region == null) {
                    f.log(Ycgnagnnisssz.styled(f), "is not on the map")
                    EndAction(f)
                } else {
                    implicit val asking = Asking(f)
                    Ask(f, $(SpeakRealityChoosePowerAction(f)))
                }

            case SpeakRealityChoosePowerAction(f) =>
                implicit val asking = Asking(f)
                // Offer power amounts from 0 to current power
                Ask(f).each(0 to f.power)(p => SpeakRealityChooseFactionAction(f, p, $()))

            case SpeakRealityChooseFactionAction(f, powerSpent, _) =>
                val ycgna = f.goo(Ycgnagnnisssz)
                val factionsInArea = game.factions.%(fx => fx != f && fx.allInPlay.%(_.region == ycgna.region).any)

                if (factionsInArea.none) {
                    f.log("No other Factions in", ycgna.region)
                    EndAction(f)
                } else {
                    implicit val asking = Asking(f)
                    Ask(f).each(factionsInArea)(target => SpeakRealityChooseActionAction(f, powerSpent, target))
                }

            case SpeakRealityChooseActionAction(f, powerSpent, targetFaction) =>
                implicit val asking = Asking(f)
                val commonActions = $("Move", "Summon", "Battle", "Build Gate", "Recruit")
                Ask(f).each(commonActions)(action => SpeakRealityAction(f, powerSpent, targetFaction, action))

            case SpeakRealityAction(f, powerSpent, targetFaction, namedAction) =>
                f.power -= powerSpent
                game.prmSpeakRealityTarget = Some(targetFaction)
                game.prmSpeakRealityAction = namedAction
                game.prmSpeakRealityPower = powerSpent

                f.log(SpeakReality.styled(f) + ":", "spent", powerSpent.power, "commanding", targetFaction.name, "to perform", namedAction)
                targetFaction.log("Must perform", namedAction, "spending at least", powerSpent.power, "next turn")

                Then(EndAction(f))

            // ── ELDRITCH LOVE ───────────────────────────────────────────
            case EldritchLoveAction(f) =>
                f.power -= PRMValues.EldritchLove_cost
                val flippedCount = game.prmFlippedDownSpellbooks.num
                if (flippedCount >= PRMValues.EldritchLove_faceDownNeeded) {
                    f.log(EldritchLove.styled(f) + ":", "gained", PRMValues.EldritchLove_elderSigns.es)
                    f.es ++= PRMValues.EldritchLove_elderSigns.times(ElderSign(0))
                }
                f.oncePerAction :+= EldritchLove
                game.prmFlippedDownSpellbooks :+= EldritchLove
                f.log(EldritchLove.styled(f), "flipped Face Down until Doom Phase")
                Then(EndAction(f))

            // ── DOOM PHASE ──────────────────────────────────────────────
            case DoomAction(f : PRM.type) =>
                implicit val asking = Asking(f)

                // Restore flipped-down spellbooks at start of Doom Phase
                game.prmFlippedDownSpellbooks.foreach { sb =>
                    f.oncePerAction = f.oncePerAction.but(sb)
                    f.log(sb.styled(f), "restored (Face Up)")
                }
                game.prmFlippedDownSpellbooks = $()

                game.rituals(f)
                game.reveals(f)
                game.highPriests(f)
                game.hires(f)
                game.recruitsWhateley(f)
                game.motherOfMonsters(f)
                game.doomDone(f)

                asking

            // ── MAKE AS ONE ─────────────────────────────────────────────
            case MakeAsOneOfferAction(self, enemy, eligibleSBs) =>
                // Offer enemy the choice to activate PRM spellbooks
                implicit val asking = Asking(enemy)
                val actions = eligibleSBs./(sb => MakeAsOneActivateAction(self, enemy, sb)) :+ MakeAsOneSkipAction(self, enemy)
                Ask(enemy, actions)

            case MakeAsOneActivateAction(self, enemy, sb) =>
                // Enemy activates PRM spellbook
                // Cost is the spellbook's power cost (varies by SB)
                val powerCost = sb match {
                    case AtomicUnity => PRMValues.AtomicUnity_cost
                    case Protogenesis => PRMValues.Protogenesis_cost
                    case Osmosis => PRMValues.Osmosis_cost
                    case ZealousSpark => 0  // Costs doom, not power
                    case SpeakReality => 0  // Cost X, enemy chooses
                    case _ => 0
                }

                if (enemy.power < powerCost) {
                    enemy.log("Not enough Power for", sb.styled(self))
                    Force(MakeAsOneSkipAction(self, enemy))
                } else {
                    enemy.power -= powerCost
                    PRM.power += powerCost  // PRM receives the power
                    game.prmMakeAsOneFlips += 1

                    // Check if this causes flip
                    val shouldFlip = sb match {
                        case AtomicUnity => PRM.allInPlay.%(_.uclass.utype == Monster).none
                        case Protogenesis => PRM.pool.%(_.uclass.utype == Monster).none
                        case _ => false  // Others have different flip conditions or don't flip
                    }

                    if (shouldFlip) {
                        PRM.oncePerAction :+= sb
                        game.prmFlippedDownSpellbooks :+= sb
                        PRM.doom += PRMValues.MakeAsOne_doomPerFlip
                        PRM.log("Gained", PRMValues.MakeAsOne_doomPerFlip.doom, "from", sb.styled(PRM), "flipping")
                    }

                    enemy.log("Activated", sb.styled(PRM))
                    // The actual execution of the spellbook action would be handled separately
                    Then(EndAction(enemy))
                }

            case MakeAsOneSkipAction(self, enemy) =>
                enemy.log("Declined", MakeAsOne.styled(self))
                Then(EndAction(enemy))

            // ── MOTHER'S AID ────────────────────────────────────────────
            case MothersAidOfferAction(self, enemy) =>
                // Offer enemy the choice to control Ycgn'àgnnisssz
                val ycgna = self.goo(Ycgnagnnisssz)
                if (ycgna.region == null) {
                    enemy.log(Ycgnagnnisssz.styled(self), "is not on the map")
                    EndAction(enemy)
                } else if (enemy.power < PRMValues.MothersAid_power) {
                    enemy.log("Not enough Power for Mother's Aid")
                    EndAction(enemy)
                } else {
                    implicit val asking = Asking(enemy)
                    Ask(enemy, $(MothersAidActivateAction(self, enemy)))
                }

            case MothersAidActivateAction(self, enemy) =>
                // Enemy pays PRM 1 power to control Ycgn'àgnnisssz
                enemy.power -= PRMValues.MothersAid_power
                PRM.power += PRMValues.MothersAid_power
                game.prmYcgnaTemporaryController = Some(enemy)
                PRM.log(enemy.name, "paid", PRMValues.MothersAid_power.power, "to control", Ycgnagnnisssz.styled(PRM), "this turn")
                enemy.log("Controlling", Ycgnagnnisssz.styled(PRM), "this turn (moves cost 0, protected from combat/elimination)")
                Then(EndAction(enemy))

            // ── MAIN ACTION ──────────────────────────────────────────────
            // MAIN ACTION — the engine has NO generic active-player action menu; each faction's
            // expansion assembles its own. Defer the passive/acted cases to Game's own handlers
            // (UnknownContinue), and build the standard action menu for PRM's own turn. PRM-specific
            // spellbook actions are appended alongside the stock builders. Without this, MainAction(PRM)
            // matches nothing and the game crashes on PRM's first turn.
            case MainAction(f : PRM.type) if f.active.not =>
                UnknownContinue

            case MainAction(f : PRM.type) if f.acted =>
                UnknownContinue

            case MainAction(f : PRM.type) =>
                implicit val asking = Asking(f)

                game.moves(f)
                game.recruits(f)
                game.battles(f)
                game.captures(f)
                game.builds(f)
                game.summons(f)
                game.awakens(f)
                game.independents(f)

                // PRM spellbook actions
                // Ycgna awaken (custom, offered from main menu, not standard awakens())
                if (f.pool(Ycgnagnnisssz).any)
                    + YcgnaAwakenMainAction(f)

                // Atomic Unity: eliminate a monster to gain power
                if (f.can(AtomicUnity) && f.allInPlay.%(_.uclass.utype == Monster).any && f.power >= PRMValues.AtomicUnity_cost)
                    + AtomicUnityMainAction(f)

                // Eldritch Love: gain ES if 2+ face-down spellbooks, flip this spellbook
                if (f.can(EldritchLove) && f.power >= PRMValues.EldritchLove_cost)
                    + EldritchLoveAction(f)

                // Osmosis: distribute units in adjacent areas
                if (f.can(Osmosis) && f.power >= PRMValues.Osmosis_cost)
                    + OsmosisMainAction(f)

                // Protogenesis: place lowest-cost monster
                if (f.can(Protogenesis) && f.power >= PRMValues.Protogenesis_cost && f.pool.%(_.uclass.utype == Monster).any)
                    + ProtogenesisMainAction(f)

                // Speak Reality: spend power to command an enemy
                if (f.can(SpeakReality) && f.goo(Ycgnagnnisssz).region != null)
                    + SpeakRealityMainAction(f)

                // Zealous Spark: move any monster (costs 1 doom)
                if (f.can(ZealousSpark) && f.doom >= PRMValues.ZealousSpark_doomCost)
                    + ZealousSparkMainAction(f)

                // Pay 2 Doom and Flip Spellbook (requirement action)
                if (f.needs(Pay2DoomFlipSpellbookReq) && f.doom >= PRMValues.Pay2DoomFlipSpellbookReq_doom)
                    + Pay2DoomFlipSpellbookMainAction(f)

                game.neutralSpellbooks(f)
                game.libraryActions(f)
                game.highPriests(f)
                game.reveals(f)

                game.endTurn(f)(f.battled.any || game.nexed.any)

                asking

            // ── END TURN ────────────────────────────────────────────────
            // Note: Mother's Aid and Speak Reality cleanup now handled in Game.scala NextPlayerAction

            case _ => UnknownContinue
        }
    }
}
