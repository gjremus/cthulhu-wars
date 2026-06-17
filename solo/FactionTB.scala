package cws

import hrf.colmat._

import html._


// ============================================================================
// THE BURROWERS BENEATH (TB) — Homebrew faction
// Spec: "The Burrowers Beneath (TB) Faction Implementation Guide" (all sections Creator Audited)
// Faction color #FFCC66 (gold), style "tb".
// Led by the Great Old One Shudde M'ell (multi-part: Head + up to 3 Segments).
// A widely-distributed faction that gains Power from Tentacle-Area spread,
// uses Ensnare/Psychic Shriek to disrupt enemies, and farms Elder Signs via
// Autotomy in chained Unlimited Battles.
//
// IMPLEMENTATION NOTES
//  * The Mantle is a Land Area (FactionRegion) that enters play when SBR-1 is
//    fulfilled. It is adjacent to player-nominated Areas (tbMantleAreas) and,
//    via Subterrane, to every TB-Tentacle's Area. Only TB units may occupy it.
//    Mirrors the Bubastis Moon pattern (MoonHold) with three differences:
//    (a) it is a Land Area with player-chosen adjacency, (b) it enters play
//    mid-game, (c) it does NOT change main-map adjacency for other factions.
//  * Shudde M'ell is ONE GOO modelled as up to 4 separate UnitFigures
//    (1 Head + up to 3 Segments) that Move/Pain/Kill independently but
//    collectively count as a single GOO for Elder-Sign/Harbinger accounting.
//  * Tentacles are Cultists that CANNOT Build/Control Gates nor be Captured
//    (Thousand Writhing Maws). Modelled via canControlGate override + canCapture
//    override on the UnitClass.
//  * Faction state lives on Game.scala (tbMantleInPlay, tbMantleAreas,
//    tbShuddeMellEverAwakened, tbEnsnareTargetedThisPhase, tbShriekTargetedThisPhase)
//    per the undo HARD RULE. NOT on TBExpansion singleton.
//  * TB sets up FIRST: 8 Power, 8 Tentacles in 8 distinct Areas, no Gate, no Acolyte.
// ============================================================================


// -- UNITS (§1.7 / §2.1) -----------------------------------------------------
// Cadavolyte: Cultist cost 2, combat 0. TB's gate-controlling Cultist.
// Tentacle: Cultist cost 2, combat 0. CANNOT Build/Control Gates nor be Captured.
// Chthonian: Monster cost 2, combat 1.
// ShuddeMellHead: GOO cost 8, combat = 3 x Parts in play.
// ShuddeMellSegment: GOO cost 0, combat 0. Placed via Behemoth, never recruited normally.
case object Cadavolyte extends FactionUnitClass(TB, "Cadavolyte", Cultist, 2) {
    override def canControlGate(u : UnitFigure)(implicit game : Game) : Boolean = true
    override def canCapture(u : UnitFigure)(implicit game : Game) : Boolean = false
}
case object Tentacle extends FactionUnitClass(TB, "Tentacle", Cultist, 2) {
    // Thousand Writhing Maws: Tentacles cannot Build nor Control Gates, nor be Captured.
    override def canControlGate(u : UnitFigure)(implicit game : Game) : Boolean = false
    override def canCapture(u : UnitFigure)(implicit game : Game) : Boolean = false
}
case object Chthonian extends FactionUnitClass(TB, "Chthonian", Monster, 2)
case object ShuddeMellHead extends FactionUnitClass(TB, "Shudde M'ell (Head)", GOO, 8)
case object ShuddeMellSegment extends FactionUnitClass(TB, "Shudde M'ell (Segment)", GOO, 0)


// -- SPELLBOOKS (§1.10) -------------------------------------------------------
case object Stalk         extends FactionSpellbook(TB, "Stalk")
case object Autotomy      extends FactionSpellbook(TB, "Autotomy")
case object Subterrane    extends FactionSpellbook(TB, "Subterrane")
case object Grasp         extends FactionSpellbook(TB, "Grasp")         with BattleSpellbook
case object Ensnare       extends FactionSpellbook(TB, "Ensnare")
case object PsychicShriek extends FactionSpellbook(TB, "Psychic Shriek")

// TB Unique Abilities — Thousand Writhing Maws + Behemoth (Ongoing). Declared as
// FactionSpellbook objects so they appear in the faction status panel and can be
// borrowed via Ancient Sorcery.
case object ThousandWrithingMaws extends FactionSpellbook(TB, "Thousand Writhing Maws")
case object Behemoth             extends FactionSpellbook(TB, "Behemoth")


// -- SPELLBOOK REQUIREMENTS (§1.9) --------------------------------------------
case object OverlayMantleReq           extends Requirement("Control Gates in 2 adjacent Areas, overlay the Mantle")
case object TenTentaclesReq            extends Requirement("There are 10 Tentacles in play")
case object RemoveGatePlaceChthonianReq extends Requirement("Gain 2 Power, remove a Gate, place a Chthonian")
case object GatesAtGOOsReq             extends Requirement("Pay 8 Power, place Gates at every GOO Area")
case object AwakenShuddeMellReq        extends Requirement("Awaken Shudde M'ell")
case object ShuddeMellInThreeGlyphsReq extends Requirement("Shudde M'ell in Glyph Areas /^\\ and (*) and |||")


// -- MANTLE REGION (§1.2 / §2.3) ---------------------------------------------
// The Mantle is a Land Area (FactionRegion) that is adjacent to player-nominated
// Map Areas. It enters play via SBR-1. Only TB units may occupy it.
case class MantleHold(faction : Faction) extends FactionRegion {
    val glyph = NoGlyph
    val id    = "MantleHold"
    val name  = "the Mantle"
}


// ============================================================================
// THE BURROWERS BENEATH (TB) FACTION OBJECT
// ============================================================================
case object TB extends Faction { f =>
    def name  = "The Burrowers Beneath"
    def short = "TB"
    def style = "tb"

    // Thousand Writhing Maws + Behemoth are the always-on ongoing abilities (§1.5).
    override def abilities = $(ThousandWrithingMaws, Behemoth)
    override def library   = $(Stalk, Autotomy, Subterrane, Grasp, Ensnare, PsychicShriek)
    override def requirements(options : $[GameOption]) =
        $(OverlayMantleReq, TenTentaclesReq, RemoveGatePlaceChthonianReq, GatesAtGOOsReq, AwakenShuddeMellReq, ShuddeMellInThreeGlyphsReq)

    // TB's special region: the Mantle (mirrors BB.moon)
    lazy val mantle : MantleHold = MantleHold(TB)

    // 6 Cadavolytes + 10 Tentacles + 5 Chthonians + 1 Head + 3 Segments (§1.7)
    val allUnits =
        6.times(Cadavolyte)        ++
        10.times(Tentacle)         ++
        5.times(Chthonian)         ++
        1.times(ShuddeMellHead)    ++
        3.times(ShuddeMellSegment)

    // Shudde M'ell Head: cost 8, may ONLY be placed on the Mantle (§1.8).
    // Segments are placed via Behemoth (never via the standard awaken path).
    override def awakenCost(u : UnitClass, r : Region)(implicit game : Game) : |[Int] = u match {
        case ShuddeMellHead =>
            (r == TB.mantle && game.tbMantleInPlay).?(8)
        case _ => None
    }

    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int = {
        // Shudde M'ell Head: combat = 3 x (number of Parts in play), excluding
        // mid-battle eliminations (FCG #12: .not(Zeroed))
        val headCount = units.%(_.uclass == ShuddeMellHead).not(Zeroed).num
        val partsInPlay = f.all(ShuddeMellHead).not(Zeroed).num + f.all(ShuddeMellSegment).not(Zeroed).num
        val headStr = headCount * (3 * partsInPlay)
        // Chthonians: 1 combat each (§1.7)
        val chthonianStr = units.%(_.uclass == Chthonian).not(Zeroed).num
        // Cadavolytes, Tentacles, Segments: 0 combat
        headStr + chthonianStr + neutralStrength(units, opponent)
    }
}


// ============================================================================
// THE BURROWERS BENEATH (TB) ACTION CASE CLASSES
// All Soft (navigation-only) sub-menus include Cancel; Hard (state-mutating)
// actions do NOT (per §3 / FCG Cancel discipline).
// ============================================================================

// -- SETUP: Place 8 Tentacles (§1.6 / §3.8.1 / §4.1) -------------------------
case class TBSetupPlaceTentacleAction(self : Faction, remaining : Int)
    extends ForcedAction {
    override def question(implicit game : Game) = "Place " + Tentacle.styled(TB) + " (" + (9 - remaining) + " of 8)"
}
case class TBSetupPlaceTentacleInAction(self : Faction, r : Region, remaining : Int)
    extends BaseFactionAction("Place " + Tentacle.styled(TB) + " in", r)

// -- THOUSAND WRITHING MAWS: 2-Power double recruit/summon (§1.5.1 / §3.6.3 / §4.3) --
case class TBWrithingMawsMainAction(self : Faction)
    extends OptionFactionAction(ThousandWrithingMaws.styled(TB) + ": recruit/summon two 2-cost units (" + 2.power + ")") with MainQuestion with Soft
case class TBWrithingMawsTypeAction(self : Faction, uc : UnitClass)
    extends BaseFactionAction(ThousandWrithingMaws.styled(TB) + ": two", uc.styled(TB)) with Soft
case class TBWrithingMawsPlaceFirstAction(self : Faction, uc : UnitClass)
    extends ForcedAction with Soft {
    override def question(implicit game : Game) = ThousandWrithingMaws.styled(TB) + ": place first " + uc.styled(TB)
}
case class TBWrithingMawsPlaceSecondAction(self : Faction, uc : UnitClass, r1 : Region)
    extends ForcedAction with Soft {
    override def question(implicit game : Game) = ThousandWrithingMaws.styled(TB) + ": place second " + uc.styled(TB)
}
case class TBWrithingMawsAction(self : Faction, uc : UnitClass, r1 : Region, r2 : Region)
    extends ForcedAction

// -- BEHEMOTH: Move any Part to the Mantle (0-Cost Unlimited, §1.8 / §3.4.3 / §4.6) --
case class TBMovePartToMantleMainAction(self : Faction)
    extends OptionFactionAction(Behemoth.styled(TB) + ": move a Part to " + TB.mantle) with MainQuestion with Soft
case class TBMovePartToMantlePickAction(self : Faction, parts : $[UnitRef])
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) = Behemoth.styled(TB) + ": move which Part to " + TB.mantle
}
case class TBMovePartToMantleAction(self : Faction, part : UnitRef)
    extends ForcedAction with PowerNeutral

// -- BEHEMOTH: Segment-on-zero-Power (automatic, §1.8 / §3.4.3) ---------------
case class TBBehemothSegmentAction(self : Faction)
    extends ForcedAction

// -- STALK: Post-Move relocation (§1.10 SB1 / §3.10.1 / §4.4) ----------------
case class TBStalkMainAction(self : Faction, movedRegions : $[Region])
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) = Stalk.styled(TB) + ": relocate a Cultist?"
}
case class TBStalkUseAction(self : Faction, movedRegions : $[Region])
    extends OptionFactionAction("Use " + Stalk.styled(TB)) with Soft {
    override def question(implicit game : Game) = Stalk.styled(TB) + ": relocate a Cultist?"
}
case class TBStalkSkipAction(self : Faction)
    extends OptionFactionAction("Skip " + Stalk.styled(TB)) with Soft {
    override def question(implicit game : Game) = Stalk.styled(TB)
}
case class TBStalkPickCultistAction(self : Faction, movedRegions : $[Region])
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) = Stalk.styled(TB) + ": choose Cultist to relocate"
}
case class TBStalkDestAction(self : Faction, cultist : UnitRef, movedRegions : $[Region])
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) = Stalk.styled(TB) + ": choose destination"
}
case class TBStalkAction(self : Faction, cultist : UnitRef, dest : Region)
    extends ForcedAction with PowerNeutral

// -- AUTOTOMY: Post-Battle Kill transfer (§1.10 SB2 / §3.10.2 / §4.4) --------
case class TBAutotomyMainAction(self : Faction)
    extends ForcedAction with PowerNeutral {
    override def question(implicit game : Game) = Autotomy.styled(TB) + ": transfer a received Kill to a Segment?"
}
case class TBAutotomyUseAction(self : Faction)
    extends OptionFactionAction("Use " + Autotomy.styled(TB)) with PostBattleQuestion with Soft {
    override def question(implicit game : Game) = Autotomy.styled(TB)
}
case class TBAutotomySkipAction(self : Faction)
    extends OptionFactionAction("Skip " + Autotomy.styled(TB)) with PostBattleQuestion with Soft {
    override def question(implicit game : Game) = Autotomy.styled(TB)
}
case class TBAutotomyPickSegmentAction(self : Faction, segments : $[UnitRef])
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) = Autotomy.styled(TB) + ": choose Segment to take the Kill"
}
case class TBAutotomyRetreatAction(self : Faction, segment : UnitRef, arena : Region)
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) = Autotomy.styled(TB) + ": retreat all Units to which adjacent Area?"
}
case class TBAutotomyAction(self : Faction, segment : UnitRef, retreatDest : Region, arena : Region)
    extends ForcedAction
case class TBAutotomyTurnEndAction(self : Faction)
    extends ForcedAction

// -- ENSNARE (§1.10 SB5 / §3.10.5 / §4.4) ------------------------------------
case class TBEnsnareMainAction(self : Faction)
    extends OptionFactionAction(Ensnare.styled(TB) + " (Cost " + 1.power + ")") with MainQuestion with Soft
case class TBEnsnarePickEnemyAction(self : Faction, targets : $[(Faction, Region)])
    extends ForcedAction with Soft {
    override def question(implicit game : Game) = Ensnare.styled(TB) + ": choose enemy in a Tentacle Area"
}
case class TBEnsnareTargetAction(self : Faction, enemy : Faction, area : Region)
    extends BaseFactionAction(Ensnare.styled(TB) + ": target", implicit g => enemy.full + " in " + area) with Soft
// Roll result baked in for replay safety
case class TBEnsnareRollAction(self : Faction, enemy : Faction, area : Region, roll : Int)
    extends ForcedAction
case class TBEnsnareRelocateAction(self : Faction, enemy : Faction, area : Region, headArea : Region, count : Int, relocated : $[UnitRef], remaining : $[UnitRef])
    extends ForcedAction with PowerNeutral {
    override def question(implicit game : Game) = Ensnare.styled(TB) + ": " + enemy.full + " relocate " + count + " units to " + headArea
}
case class TBEnsnareRelocatePickAction(self : Faction, enemy : Faction, u : UnitRef, area : Region, headArea : Region, count : Int, relocated : $[UnitRef], remaining : $[UnitRef])
    extends BaseFactionAction(implicit g => Ensnare.styled(TB) + ": relocate", implicit g => g.unit(u).uclass.styled(enemy))

// -- PSYCHIC SHRIEK (§1.10 SB6 / §3.10.6 / §4.4) -----------------------------
case class TBPsychicShriekMainAction(self : Faction)
    extends OptionFactionAction(PsychicShriek.styled(TB) + " (Cost " + 1.power + ")") with MainQuestion with Soft
case class TBPsychicShriekPickEnemyAction(self : Faction, enemies : $[Faction])
    extends ForcedAction with Soft {
    override def question(implicit game : Game) = PsychicShriek.styled(TB) + ": choose enemy (not Hibernating)"
}
case class TBPsychicShriekTargetAction(self : Faction, enemy : Faction)
    extends BaseFactionAction(PsychicShriek.styled(TB) + ": target", implicit g => enemy.full) with Soft
// Roll result baked in for replay safety
case class TBPsychicShriekRollAction(self : Faction, enemy : Faction, roll : Int)
    extends ForcedAction
case class TBPsychicShriekRetreatAction(self : Faction, enemy : Faction, count : Int, priorAreas : $[Region], retreated : $[UnitRef], remaining : $[UnitRef])
    extends ForcedAction with PowerNeutral {
    override def question(implicit game : Game) = PsychicShriek.styled(TB) + ": " + enemy.full + " retreat " + count + " units"
}
case class TBPsychicShriekRetreatPickAction(self : Faction, enemy : Faction, u : UnitRef, dest : Region, count : Int, priorAreas : $[Region], retreated : $[UnitRef], remaining : $[UnitRef])
    extends BaseFactionAction(implicit g => PsychicShriek.styled(TB) + ": retreat", implicit g => g.unit(u).uclass.styled(enemy) + " to " + dest)

// -- SBR-1: OVERLAY THE MANTLE (§1.9 / §3.12.1 / §4.5) -----------------------
case class TBOverlayMantleMainAction(self : Faction)
    extends OptionFactionAction("Overlay " + TB.mantle + " on adjacent Gated Areas") with MainQuestion with Soft
case class TBOverlayMantlePickGatesAction(self : Faction, adjacentGatedPairs : $[(Region, Region)])
    extends ForcedAction with Soft {
    override def question(implicit game : Game) = "Choose 2 adjacent Gated Areas for " + TB.mantle
}
case class TBOverlayMantleGatePairAction(self : Faction, r1 : Region, r2 : Region)
    extends BaseFactionAction("Overlay " + TB.mantle + " on", implicit g => r1 + " and " + r2) with Soft
case class TBOverlayMantleAreasAction(self : Faction, r1 : Region, r2 : Region, chosen : $[Region], remaining : $[Region])
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) = "Choose Areas " + TB.mantle + " is touching (must include " + r1 + " and " + r2 + ")"
}
case class TBOverlayMantleAreaToggleAction(self : Faction, r : Region, r1 : Region, r2 : Region, chosen : $[Region], remaining : $[Region])
    extends BaseFactionAction("Add Area to " + TB.mantle + " adjacency:", r)
case class TBOverlayMantleDoneAction(self : Faction, r1 : Region, r2 : Region, chosen : $[Region])
    extends OptionFactionAction("Done".styled("power")) with Soft {
    override def question(implicit game : Game) = "Confirm " + TB.mantle + " adjacency"
}
case class TBOverlayMantleTransferGateAction(self : Faction, r1 : Region, r2 : Region, chosen : $[Region])
    extends ForcedAction with Soft {
    override def question(implicit game : Game) = "Transfer which Gate (and its Cultist) to " + TB.mantle + "?"
}
case class TBOverlayMantleTransferAction(self : Faction, gateRegion : Region, chosen : $[Region])
    extends BaseFactionAction("Transfer Gate from", implicit g => gateRegion + " to " + TB.mantle)

// -- SBR-3: REMOVE GATE, PLACE CHTHONIAN (§1.9 / §3.12.3 / §4.5) -------------
case class TBRemoveGatePlaceChthonianMainAction(self : Faction)
    extends OptionFactionAction(RemoveGatePlaceChthonianReq.text.styled(TB)) with MainQuestion with Soft
case class TBRemoveGatePlaceChthonianPickGateAction(self : Faction)
    extends ForcedAction with Soft {
    override def question(implicit game : Game) = "Remove which Gate you Control?"
}
case class TBRemoveGatePlaceChthonianGateAction(self : Faction, r : Region)
    extends BaseFactionAction("Remove Gate in", r) with Soft
case class TBRemoveGatePlaceChthonianPickAreaAction(self : Faction, removedGate : Region)
    extends ForcedAction with Soft {
    override def question(implicit game : Game) = "Place " + Chthonian.styled(TB) + " in which Area your Unit occupies?"
}
case class TBRemoveGatePlaceChthonianAreaAction(self : Faction, removedGate : Region, dest : Region)
    extends BaseFactionAction("Place " + Chthonian.styled(TB) + " in", dest)

// -- SBR-4: GATES AT EVERY GOO AREA (§1.9 / §3.12.4 / §4.5) ------------------
case class TBGatesAtGOOsMainAction(self : Faction)
    extends OptionFactionAction(GatesAtGOOsReq.text.styled(TB) + " (" + 8.power + ")") with MainQuestion with Soft
case class TBGatesAtGOOsAction(self : Faction)
    extends ForcedAction

// -- SBR-6: SHUDDE M'ELL IN THREE GLYPHS — alt 6-Power Action (§1.9 / §3.12.6 / §4.5) --
case class TBThreeGlyphsPayAction(self : Faction)
    extends OptionFactionAction("Pay " + 6.power + " to satisfy " + ShuddeMellInThreeGlyphsReq.text.styled(TB)) with MainQuestion with Soft
case class TBThreeGlyphsPayConfirmAction(self : Faction)
    extends ForcedAction


// ============================================================================
// THE BURROWERS BENEATH (TB) EXPANSION — game-loop integration
// ============================================================================
object TBExpansion extends Expansion {

    // -- CONDITION-BASED SBR evaluation (§3.12) --------------------------------
    override def triggers()(implicit game : Game) {
        if (!game.setup.has(TB)) return
        val f = TB

        // SBR-2: 10 Tentacles in play (§3.12.2)
        f.satisfyIf(TenTentaclesReq, TenTentaclesReq.text, f.all(Tentacle).num >= 10)

        // SBR-6: Shudde M'ell Parts simultaneously in Glyph Areas /^\ and (*) and ||| (§3.12.6)
        // Uses the same glyph-area definitions as Yellow Sign's Desecrate requirements.
        val partRegions = (f.all(ShuddeMellHead) ++ f.all(ShuddeMellSegment))./(_.region).distinct
        val hasAA = partRegions.exists(_.glyph == GlyphAA)
        val hasOO = partRegions.exists(_.glyph == GlyphOO)
        val hasWW = partRegions.exists(_.glyph == GlyphWW)
        f.satisfyIf(ShuddeMellInThreeGlyphsReq, ShuddeMellInThreeGlyphsReq.text, hasAA && hasOO && hasWW)
    }

    def perform(action : Action, soft : VoidGuard)(implicit game : Game) = action @@ {

        // ====================================================================
        // SETUP (§1.6 / §2.0a / §3.8.1)
        // TB sets up FIRST: 8 Power, 8 Tentacles in 8 distinct Areas, no Gate, no Acolyte.
        // ====================================================================
        case SetupFactionsAction if game.setup.has(TB) && !game.starting.contains(TB) =>
            val f = TB
            f.power = 8
            // Sentinel start — Mantle is TB's Start Area but not yet in play
            game.starting = game.starting + (TB -> TB.mantle)
            f.log("starts with", 8.power, "and places 8", Tentacle.styled(TB), "in 8 different Areas")
            Force(TBSetupPlaceTentacleAction(f, 8))

        case TBSetupPlaceTentacleAction(self, remaining) =>
            if (remaining <= 0)
                Force(SetupFactionsAction)
            else {
                // Eligible: any map Area not already containing one of TB's Tentacles
                val placed = self.all(Tentacle)./(_.region)
                val eligible = game.board.regions.%!(placed.has)
                Ask(self).each(eligible)(r => TBSetupPlaceTentacleInAction(self, r, remaining))
            }

        case TBSetupPlaceTentacleInAction(self, r, remaining) =>
            self.place(Tentacle, r)
            self.log("placed", Tentacle.styled(TB), "in", r)
            Force(TBSetupPlaceTentacleAction(self, remaining - 1))

        // ====================================================================
        // DOOM PHASE (§3.11 — clear Ensnare/Shriek targeted-faction tags)
        // ====================================================================
        case DoomAction(f : TB.type) =>
            implicit val asking = Asking(f)

            game.rituals(f)

            game.reveals(f)

            game.highPriests(f)

            game.hires(f)

            + DoomDoneAction(f)

            asking

        case DoomDoneAction(f) if f == TB =>
            // Clear per-Action-Phase faction tags (§2.4 / §3.11)
            game.tbEnsnareTargetedThisPhase = $
            game.tbShriekTargetedThisPhase = $
            UnknownContinue

        // ====================================================================
        // MAIN ACTION (§2.7 / §3.10)
        // ====================================================================
        case MainAction(f : TB.type) if f.active.not =>
            UnknownContinue

        case MainAction(f : TB.type) if f.acted =>
            UnknownContinue

        case MainAction(f : TB.type) =>
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

            // Thousand Writhing Maws: 2-Power double recruit/summon (§1.5.1 / §3.6.3)
            if (f.power >= 2) {
                val eligible2CostTypes : $[UnitClass] = {
                    var types : $[UnitClass] = $
                    if (f.pool(Cadavolyte).num >= 2)  types :+= Cadavolyte
                    if (f.pool(Tentacle).num >= 2)    types :+= Tentacle
                    if (f.pool(Chthonian).num >= 2)   types :+= Chthonian
                    types
                }
                if (eligible2CostTypes.any)
                    + TBWrithingMawsMainAction(f)
            }

            // Behemoth: Move any Part to the Mantle (0-Cost Unlimited, §1.8 / §3.4.3)
            if (game.tbMantleInPlay && game.tbShuddeMellEverAwakened) {
                val partsNotOnMantle = (f.all(ShuddeMellHead) ++ f.all(ShuddeMellSegment)).%(_.region != TB.mantle)
                if (partsNotOnMantle.any)
                    + TBMovePartToMantleMainAction(f)
            }

            // Ensnare (§1.10 SB5): Action Cost 1, requires Head in play
            if (f.can(Ensnare) && f.power >= 1 && f.all(ShuddeMellHead).any) {
                val tentacleAreas = f.onMap(Tentacle)./(_.region).distinct
                val validTargets = tentacleAreas./~(r =>
                    game.factions.but(f).%(e => !game.tbEnsnareTargetedThisPhase.has(e) && e.at(r).any)./(e => (e, r)))
                if (validTargets.any)
                    + TBEnsnareMainAction(f)
            }

            // Psychic Shriek (§1.10 SB6): Action Cost 1, requires Mantle in play
            if (f.can(PsychicShriek) && f.power >= 1 && game.tbMantleInPlay) {
                val validEnemies = game.factions.but(f).%(e =>
                    !game.tbShriekTargetedThisPhase.has(e) && e.allInPlay.any && !e.hibernating)
                if (validEnemies.any)
                    + TBPsychicShriekMainAction(f)
            }

            // SBR-1: Overlay the Mantle (opt-in when Controlling 2 adjacent Gated Areas)
            if (f.needs(OverlayMantleReq) && !game.tbMantleInPlay) {
                val ownGates = f.gates
                val adjacentPairs = ownGates./~(r1 =>
                    ownGates.%(r2 => r1 != r2 && game.board.connected(r1).has(r2))./(r2 => (r1, r2)))
./{ case (a, b) => if (a.hashCode <= b.hashCode) (a, b) else (b, a) }.distinct
                if (adjacentPairs.any)
                    + TBOverlayMantleMainAction(f)
            }

            // SBR-3: Remove a Gate, place a Chthonian (opt-in any time until earned)
            if (f.needs(RemoveGatePlaceChthonianReq) && f.gates.any && f.pool(Chthonian).any) {
                val occupiedAreas = f.allInPlay./(_.region).distinct
                if (occupiedAreas.any)
                    + TBRemoveGatePlaceChthonianMainAction(f)
            }

            // SBR-4: Gates at every GOO Area (Action, pay 8 Power)
            if (f.needs(GatesAtGOOsReq) && f.power >= 8) {
                val gooAreas = game.factions./~(fx => fx.all(GOO)./(_.region)).distinct
                val ungated = gooAreas.%!(r => f.gates.has(r))
                if (ungated.any)
                    + TBGatesAtGOOsMainAction(f)
            }

            // SBR-6: 3-Glyph alt 6-Power Action
            if (f.needs(ShuddeMellInThreeGlyphsReq) && f.power >= 6)
                + TBThreeGlyphsPayAction(f)

            game.neutralSpellbooks(f)
            game.libraryActions(f)
            game.highPriests(f)
            game.reveals(f)

            game.endTurn(f)(f.battled.any || game.nexed.any)

            asking

        // ====================================================================
        // THOUSAND WRITHING MAWS: 2-Power double recruit/summon (§3.6.3)
        // ====================================================================
        case TBWrithingMawsMainAction(self) =>
            implicit val asking = Asking(self)
            if (self.pool(Cadavolyte).num >= 2)  + TBWrithingMawsTypeAction(self, Cadavolyte)
            if (self.pool(Tentacle).num >= 2)    + TBWrithingMawsTypeAction(self, Tentacle)
            if (self.pool(Chthonian).num >= 2)   + TBWrithingMawsTypeAction(self, Chthonian)
            + CancelAction
            asking

        case TBWrithingMawsTypeAction(self, uc) =>
            Force(TBWrithingMawsPlaceFirstAction(self, uc))

        case TBWrithingMawsPlaceFirstAction(self, uc) =>
            // Eligible regions: standard recruit/summon placement rules
            val areas = if (uc.utype == Cultist)
                game.board.regions.%(r => self.at(r).any || self.gates.has(r))
            else
                self.gates
            if (areas.num == 1)
                Force(TBWrithingMawsPlaceSecondAction(self, uc, areas.head))
            else
                Ask(self).each(areas)(r => TBWrithingMawsPlaceSecondAction(self, uc, r)).cancel

        case TBWrithingMawsPlaceSecondAction(self, uc, r1) =>
            val areas = if (uc.utype == Cultist)
                game.board.regions.%(r => self.at(r).any || self.gates.has(r))
            else
                self.gates
            if (areas.num == 1)
                Force(TBWrithingMawsAction(self, uc, r1, areas.head))
            else
                Ask(self).each(areas)(r => TBWrithingMawsAction(self, uc, r1, r)).cancel

        case TBWrithingMawsAction(self, uc, r1, r2) =>
            self.power -= 2
            self.place(uc, r1)
            self.place(uc, r2)
            self.log(ThousandWrithingMaws.styled(TB) + ": placed two", uc.styled(TB), "in", r1, "and", r2)
            // Check Behemoth: if Power hit 0
            tbCheckBehemoth()
            EndAction(self)

        // ====================================================================
        // BEHEMOTH: Move Part to Mantle (0-Cost Unlimited, §3.4.3)
        // ====================================================================
        case TBMovePartToMantleMainAction(self) =>
            val parts = (self.all(ShuddeMellHead) ++ self.all(ShuddeMellSegment)).%(_.region != TB.mantle)
            if (parts.num == 1)
                Force(TBMovePartToMantleAction(self, parts.head.ref))
            else
                Force(TBMovePartToMantlePickAction(self, parts./(_.ref)))

        case TBMovePartToMantlePickAction(self, parts) =>
            implicit val asking = Asking(self)
            parts.foreach { ur =>
                + TBMovePartToMantleAction(self, ur).as(game.unit(ur).uclass.styled(TB) + " in " + game.unit(ur).region)
            }
            + CancelAction
            asking

        case TBMovePartToMantleAction(self, part) =>
            val u = game.unit(part)
            val from = u.region
            u.region = TB.mantle
            self.log(Behemoth.styled(TB) + ": moved", u.uclass.styled(TB), "from", from, "to", TB.mantle)
            // Unlimited — return to main action (FCG #19)
            Force(MainAction(self))

        // Behemoth: Segment-on-zero-Power (automatic)
        case TBBehemothSegmentAction(self) =>
            if (game.tbShuddeMellEverAwakened && game.tbMantleInPlay && self.pool(ShuddeMellSegment).any) {
                self.place(ShuddeMellSegment, TB.mantle)
                self.log(Behemoth.styled(TB) + ": Power reached 0 — placed", ShuddeMellSegment.styled(TB), "on", TB.mantle)
            }
            UnknownContinue

        // ====================================================================
        // STALK: Post-Move relocation (§3.10.1)
        // ====================================================================
        case TBStalkMainAction(self, movedRegions) =>
            implicit val asking = Asking(self)
            + TBStalkUseAction(self, movedRegions)
            + TBStalkSkipAction(self)
            asking

        case TBStalkUseAction(self, movedRegions) =>
            Force(TBStalkPickCultistAction(self, movedRegions))

        case TBStalkSkipAction(self) =>
            UnknownContinue

        case TBStalkPickCultistAction(self, movedRegions) =>
            val cultists = self.allInPlay.%(u => u.uclass.utype == Cultist)
            if (cultists.none)
                UnknownContinue
            else
                Ask(self).each(cultists)(u =>
                    TBStalkDestAction(self, u.ref, movedRegions)
                        .as(u.uclass.styled(TB) + " in " + u.region)).cancel

        case TBStalkDestAction(self, cultist, movedRegions) =>
            // Destination = any of the just-Moved Units' Areas
            Ask(self).each(movedRegions)(r =>
                TBStalkAction(self, cultist, r)).cancel

        case TBStalkAction(self, cultist, dest) =>
            val u = game.unit(cultist)
            val from = u.region
            u.region = dest
            self.log(Stalk.styled(TB) + ": relocated", u.uclass.styled(TB), "from", from, "to", dest)
            UnknownContinue

        // ====================================================================
        // AUTOTOMY: Post-Battle Kill transfer (§3.10.2)
        // ====================================================================
        case TBAutotomyUseAction(self) =>
            val segments = (self.all(ShuddeMellSegment))./(_.ref)
            if (segments.num == 1)
                Force(TBAutotomyPickSegmentAction(self, segments))
            else
                Force(TBAutotomyPickSegmentAction(self, segments))

        case TBAutotomySkipAction(self) =>
            self.log(Autotomy.styled(TB) + ": declined")
            UnknownContinue

        case TBAutotomyPickSegmentAction(self, segments) =>
            if (segments.num == 1) {
                val arena = game.battle.map(_.arena).|(TB.mantle)
                Force(TBAutotomyRetreatAction(self, segments.head, arena))
            } else {
                Ask(self).each(segments)(ur =>
                    TBAutotomyRetreatAction(self, ur, game.battle.map(_.arena).|(TB.mantle))
                        .as(ShuddeMellSegment.styled(TB) + " in " + game.unit(ur).region))
            }

        case TBAutotomyRetreatAction(self, segment, arena) =>
            // Choose 1 adjacent Area to retreat all TB units (not killed/eliminated) to
            val adjacent = game.board.connected(arena) ++ tbMantleEdges(arena)
            Ask(self).each(adjacent.distinct)(r =>
                TBAutotomyAction(self, segment, r, arena))

        case TBAutotomyAction(self, segment, retreatDest, arena) =>
            // Kill the Segment
            val segUnit = game.unit(segment)
            game.eliminate(segUnit)
            self.log(Autotomy.styled(TB) + ": Kill transferred to", ShuddeMellSegment.styled(TB))

            // Gain 1 Elder Sign per Segment in pool (at the moment of transfer)
            val segmentsInPool = self.pool(ShuddeMellSegment).num
            if (segmentsInPool > 0) {
                self.takeES(segmentsInPool)
                self.log(Autotomy.styled(TB) + ": gained", segmentsInPool.es, "for", segmentsInPool, "Segment".s(segmentsInPool), "in pool")
            }

            // Ignore Pains: retreat all TB units not killed/eliminated from battle area
            val toRetreat = self.at(arena)
            toRetreat.foreach { u => u.region = retreatDest }
            if (toRetreat.any)
                self.log(Autotomy.styled(TB) + ": retreated", toRetreat.num, "unit".s(toRetreat.num), "to", retreatDest)

            // Queue turn-end action to place all pool Segments on Mantle
            Force(TBAutotomyTurnEndAction(self))

        case TBAutotomyTurnEndAction(self) =>
            // TURN-END: place all Segments from pool on the Mantle
            val poolSegments = self.pool(ShuddeMellSegment).num
            if (poolSegments > 0 && game.tbMantleInPlay) {
                1.to(poolSegments).foreach(_ => self.place(ShuddeMellSegment, TB.mantle))
                self.log(Autotomy.styled(TB) + ": placed", poolSegments, "Segment".s(poolSegments), "from pool on", TB.mantle)
            }
            UnknownContinue

        // ====================================================================
        // ENSNARE (§3.10.5)
        // ====================================================================
        case TBEnsnareMainAction(self) =>
            val tentacleAreas = self.onMap(Tentacle)./(_.region).distinct
            val validTargets = tentacleAreas./~(r =>
                game.factions.but(self).%(e => !game.tbEnsnareTargetedThisPhase.has(e) && e.at(r).any)./(e => (e, r)))
            Force(TBEnsnarePickEnemyAction(self, validTargets))

        case TBEnsnarePickEnemyAction(self, targets) =>
            implicit val asking = Asking(self)
            targets.foreach { case (enemy, area) =>
                + TBEnsnareTargetAction(self, enemy, area)
            }
            + CancelAction
            asking

        case TBEnsnareTargetAction(self, enemy, area) =>
            // Pay 1 Power
            self.power -= 1
            // Mark this faction as targeted this phase
            game.tbEnsnareTargetedThisPhase :+= enemy
            self.log(Ensnare.styled(TB) + ": targeting", enemy.full, "in", area)
            // Check Behemoth
            tbCheckBehemoth()
            // Roll 1 D6
            RollD6(g => Ensnare.styled(TB) + ": roll for " + enemy.full, roll => TBEnsnareRollAction(self, enemy, area, roll))

        case TBEnsnareRollAction(self, enemy, area, roll) =>
            val headArea = self.all(ShuddeMellHead).headOption.map(_.region).|(TB.mantle)
            val count = math.min(roll, enemy.power)
            self.log(Ensnare.styled(TB) + ": rolled", roll, "— relocate", count, "unit".s(count))
            if (count <= 0)
                EndAction(self)
            else {
                val enemyUnits = enemy.at(area)./(_.ref)
                Force(TBEnsnareRelocateAction(enemy, enemy, area, headArea, count, $, enemyUnits))
            }

        case TBEnsnareRelocateAction(self, enemy, area, headArea, count, relocated, remaining) =>
            if (count <= 0 || remaining.none)
                EndAction(TB)
            else if (remaining.num == 1)
                Force(TBEnsnareRelocatePickAction(self, enemy, remaining.head, area, headArea, count, relocated, remaining))
            else {
                // FCG #26: enemy picks which units relocate (self = enemy for border color)
                Ask(self).each(remaining)(ur =>
                    TBEnsnareRelocatePickAction(self, enemy, ur, area, headArea, count, relocated, remaining))
            }

        case TBEnsnareRelocatePickAction(self, enemy, u, area, headArea, count, relocated, remaining) =>
            val unit = game.unit(u)
            unit.region = headArea
            TB.log(Ensnare.styled(TB) + ": relocated", unit.uclass.styled(enemy), "to", headArea)
            Force(TBEnsnareRelocateAction(self, enemy, area, headArea, count - 1, relocated :+ u, remaining.but(u)))

        // ====================================================================
        // PSYCHIC SHRIEK (§3.10.6)
        // ====================================================================
        case TBPsychicShriekMainAction(self) =>
            val validEnemies = game.factions.but(self).%(e =>
                !game.tbShriekTargetedThisPhase.has(e) && e.allInPlay.any)
            Force(TBPsychicShriekPickEnemyAction(self, validEnemies))

        case TBPsychicShriekPickEnemyAction(self, enemies) =>
            implicit val asking = Asking(self)
            enemies.foreach { enemy =>
                + TBPsychicShriekTargetAction(self, enemy)
            }
            + CancelAction
            asking

        case TBPsychicShriekTargetAction(self, enemy) =>
            // Pay 1 Power
            self.power -= 1
            // Mark this faction as targeted this phase
            game.tbShriekTargetedThisPhase :+= enemy
            self.log(PsychicShriek.styled(TB) + ": targeting", enemy.full)
            // Check Behemoth
            tbCheckBehemoth()
            // Roll 2 D6
            RollD6(g => PsychicShriek.styled(TB) + ": first die for " + enemy.full, roll1 =>
                TBPsychicShriekRollAction(self, enemy, roll1))

        case TBPsychicShriekRollAction(self, enemy, roll1) =>
            // Need second die — chain another roll
            RollD6(g => PsychicShriek.styled(TB) + ": second die for " + enemy.full, roll2 => {
                val totalRoll = roll1 + roll2
                val count = math.min(totalRoll, 2 * enemy.power)
                self.log(PsychicShriek.styled(TB) + ": rolled", totalRoll, "— retreat", count, "unit".s(count))
                // Snapshot enemy's current occupied areas (before retreat)
                val priorAreas = enemy.allInPlay./(_.region).distinct
                val enemyUnits = enemy.allInPlay./(_.ref)
                TBPsychicShriekRetreatAction(enemy, enemy, count, priorAreas, $, enemyUnits)
            })

        case TBPsychicShriekRetreatAction(self, enemy, count, priorAreas, retreated, remaining) =>
            if (count <= 0 || remaining.none)
                EndAction(TB)
            else {
                // Valid destinations: Areas NOT in priorAreas AND without TB Gates
                val validDests = game.board.regions.%(r => !priorAreas.has(r) && !TB.gates.has(r))
                if (validDests.none) {
                    // FCG #23: auto-eliminate any unit with no legal destination
                    remaining.foreach { ur =>
                        val u = game.unit(ur)
                        game.eliminate(u)
                    }
                    TB.log(PsychicShriek.styled(TB) + ": no legal destinations — eliminated remaining units")
                    EndAction(TB)
                } else if (remaining.num == 1) {
                    // Only one unit — still need destination pick
                    // FCG #26: enemy picks (self = enemy)
                    Ask(self).each(validDests)(r =>
                        TBPsychicShriekRetreatPickAction(self, enemy, remaining.head, r, count, priorAreas, retreated, remaining))
                } else {
                    // Enemy picks which unit + destination
                    implicit val asking = Asking(self)
                    remaining.foreach { ur =>
                        validDests.foreach { r =>
                            + TBPsychicShriekRetreatPickAction(self, enemy, ur, r, count, priorAreas, retreated, remaining)
                        }
                    }
                    asking
                }
            }

        case TBPsychicShriekRetreatPickAction(self, enemy, u, dest, count, priorAreas, retreated, remaining) =>
            val unit = game.unit(u)
            unit.region = dest
            TB.log(PsychicShriek.styled(TB) + ": retreated", unit.uclass.styled(enemy), "to", dest)
            Force(TBPsychicShriekRetreatAction(self, enemy, count - 1, priorAreas, retreated :+ u, remaining.but(u)))

        // ====================================================================
        // SBR-1: OVERLAY THE MANTLE (§3.12.1)
        // ====================================================================
        case TBOverlayMantleMainAction(self) =>
            val ownGates = self.gates
            val adjacentPairs = ownGates./~(r1 =>
                ownGates.%(r2 => r1 != r2 && game.board.connected(r1).has(r2))./(r2 => (r1, r2)))
                .map { case (a, b) => if (a.hashCode <= b.hashCode) (a, b) else (b, a) }.distinct
            if (adjacentPairs.num == 1)
                Force(TBOverlayMantleGatePairAction(self, adjacentPairs.head._1, adjacentPairs.head._2))
            else
                Force(TBOverlayMantlePickGatesAction(self, adjacentPairs))

        case TBOverlayMantlePickGatesAction(self, pairs) =>
            implicit val asking = Asking(self)
            pairs.foreach { case (r1, r2) =>
                + TBOverlayMantleGatePairAction(self, r1, r2)
            }
            + CancelAction
            asking

        case TBOverlayMantleGatePairAction(self, r1, r2) =>
            // Nominate Areas the Mantle touches (must include r1 and r2)
            // Available: all connected areas of r1 and r2, plus r1 and r2 themselves
            val candidates = (game.board.connected(r1) ++ game.board.connected(r2) ++ $(r1, r2)).distinct
            Force(TBOverlayMantleAreasAction(self, r1, r2, $(r1, r2), candidates.but(r1).but(r2)))

        case TBOverlayMantleAreasAction(self, r1, r2, chosen, remaining) =>
            implicit val asking = Asking(self)
            // Can add more areas or finish (minimum = the 2 gated areas, already in chosen)
            remaining.foreach { r =>
                + TBOverlayMantleAreaToggleAction(self, r, r1, r2, chosen, remaining)
            }
            // Done option — at least the 2 gated areas are chosen
            + TBOverlayMantleDoneAction(self, r1, r2, chosen)
            + CancelAction
            asking

        case TBOverlayMantleAreaToggleAction(self, r, r1, r2, chosen, remaining) =>
            Force(TBOverlayMantleAreasAction(self, r1, r2, chosen :+ r, remaining.but(r)))

        case TBOverlayMantleDoneAction(self, r1, r2, chosen) =>
            Force(TBOverlayMantleTransferGateAction(self, r1, r2, chosen))

        case TBOverlayMantleTransferGateAction(self, r1, r2, chosen) =>
            // Choose which of the 2 gates (+ its Cultist) transfers to the Mantle
            Ask(self)
                .add(TBOverlayMantleTransferAction(self, r1, chosen))
                .add(TBOverlayMantleTransferAction(self, r2, chosen))

        case TBOverlayMantleTransferAction(self, gateRegion, chosen) =>
            // Set Mantle in play
            game.tbMantleInPlay = true
            game.tbMantleAreas = chosen
            // Transfer the gate to the Mantle
            self.gates = self.gates.but(gateRegion) :+ TB.mantle
            // Move the Cultist that was on-gate to the Mantle
            val cultistOnGate = self.at(gateRegion).%(u => u.onGate && u.uclass.utype == Cultist).headOption
            cultistOnGate.foreach { u =>
                u.region = TB.mantle
                u.onGate = true
            }
            self.log("Overlaid", TB.mantle, "on", chosen.mkString(", "))
            self.log("Transferred Gate and Cultist from", gateRegion, "to", TB.mantle)
            self.satisfy(OverlayMantleReq, "Overlay the Mantle")
            EndAction(self)

        // ====================================================================
        // SBR-3: REMOVE GATE, PLACE CHTHONIAN (§3.12.3)
        // ====================================================================
        case TBRemoveGatePlaceChthonianMainAction(self) =>
            // Gain 2 Power first
            self.power += 2
            self.log(RemoveGatePlaceChthonianReq.text.styled(TB) + ": gained", 2.power)
            Force(TBRemoveGatePlaceChthonianPickGateAction(self))

        case TBRemoveGatePlaceChthonianPickGateAction(self) =>
            val gates = self.gates
            if (gates.num == 1)
                Force(TBRemoveGatePlaceChthonianGateAction(self, gates.head))
            else
                Ask(self).each(gates)(r => TBRemoveGatePlaceChthonianGateAction(self, r))

        case TBRemoveGatePlaceChthonianGateAction(self, r) =>
            Force(TBRemoveGatePlaceChthonianPickAreaAction(self, r))

        case TBRemoveGatePlaceChthonianPickAreaAction(self, removedGate) =>
            val occupiedAreas = self.allInPlay./(_.region).distinct
            if (occupiedAreas.num == 1)
                Force(TBRemoveGatePlaceChthonianAreaAction(self, removedGate, occupiedAreas.head))
            else
                Ask(self).each(occupiedAreas)(r => TBRemoveGatePlaceChthonianAreaAction(self, removedGate, r))

        case TBRemoveGatePlaceChthonianAreaAction(self, removedGate, dest) =>
            // Remove the gate
            self.gates = self.gates.but(removedGate)
            self.log("Removed Gate in", removedGate)
            // Place Chthonian
            self.place(Chthonian, dest)
            self.log("Placed", Chthonian.styled(TB), "in", dest)
            self.satisfy(RemoveGatePlaceChthonianReq, "Remove a Gate, place a Chthonian")
            EndAction(self)

        // ====================================================================
        // SBR-4: GATES AT EVERY GOO AREA (§3.12.4)
        // ====================================================================
        case TBGatesAtGOOsMainAction(self) =>
            Force(TBGatesAtGOOsAction(self))

        case TBGatesAtGOOsAction(self) =>
            // Pay 8 Power
            self.power -= 8
            // Check Behemoth
            tbCheckBehemoth()
            // Place a Gate in each Area containing a GOO Unit (any faction) without a TB Gate
            val gooAreas = game.factions./~(fx => fx.all(GOO)./(_.region)).distinct
            val ungated = gooAreas.%!(r => self.gates.has(r))
            var gatesPlaced = 0
            ungated.foreach { r =>
                self.gates :+= r
                // Place a cultist on gate if possible, otherwise just mark the gate
                val cultistAtRegion = self.at(r).%(u => u.uclass.utype == Cultist && !u.onGate).headOption
                cultistAtRegion.foreach(_.onGate = true)
                gatesPlaced += 1
            }
            // Gain 1 Power per Gate just placed
            self.power += gatesPlaced
            self.log(GatesAtGOOsReq.text.styled(TB) + ": placed", gatesPlaced, "Gate".s(gatesPlaced), "— gained", gatesPlaced.power)
            self.satisfy(GatesAtGOOsReq, "Gates at every GOO Area")
            EndAction(self)

        // ====================================================================
        // SBR-6: 3-GLYPH alt 6-Power Action (§3.12.6)
        // ====================================================================
        case TBThreeGlyphsPayAction(self) =>
            Force(TBThreeGlyphsPayConfirmAction(self))

        case TBThreeGlyphsPayConfirmAction(self) =>
            self.power -= 6
            self.log("Paid", 6.power, "to satisfy", ShuddeMellInThreeGlyphsReq.text.styled(TB))
            // Check Behemoth
            tbCheckBehemoth()
            self.satisfy(ShuddeMellInThreeGlyphsReq, "Shudde M'ell in three Glyph Areas (paid 6 Power)")
            EndAction(self)

        // ====================================================================
        // AWAKEN SHUDDE M'ELL (§3.4.1)
        // ====================================================================
        case AwakenedAction(self : TB.type, ShuddeMellHead, _, _) =>
            game.tbShuddeMellEverAwakened = true
            self.satisfy(AwakenShuddeMellReq, "Awaken Shudde M'ell")
            // Check Behemoth: Power just dropped to pay 8
            tbCheckBehemoth()
            EndAction(self)

        // ====================================================================
        // CATCH-ALL
        // ====================================================================
        case _ => UnknownContinue
    }

    // -- HELPER: Behemoth Segment-on-zero-Power (§3.4.3) ----------------------
    // Called after every TB Power debit. If Power == 0 and Head has ever been
    // awakened and Mantle is in play and pool has Segments, place one Segment.
    def tbCheckBehemoth()(implicit game : Game) : Unit = {
        val f = TB
        if (f.power == 0 && game.tbShuddeMellEverAwakened && game.tbMantleInPlay && f.pool(ShuddeMellSegment).any) {
            f.place(ShuddeMellSegment, TB.mantle)
            f.log(Behemoth.styled(TB) + ": Power reached 0 — placed", ShuddeMellSegment.styled(TB), "on", TB.mantle)
        }
    }

    // -- HELPER: Mantle adjacency edges (§2.3 / §3.2.2) -----------------------
    // For TB units only: Mantle <-> tbMantleAreas, plus (with Subterrane)
    // Mantle <-> every TB-Tentacle's Area.
    def tbMantleEdges(from : Region)(implicit game : Game) : $[Region] = {
        if (!game.tbMantleInPlay) return $
        val tentacleAreas = TB.has(Subterrane).??(TB.onMap(Tentacle)./(_.region).distinct)
        if (from == TB.mantle)
            (game.tbMantleAreas ++ tentacleAreas).distinct
        else if (game.tbMantleAreas.has(from) || tentacleAreas.has(from))
            $(TB.mantle)
        else
            $
    }
}
