package cws

import hrf.colmat._

import html._


// ============================================================================
// XYRIOUS STORM (XSS) — Homebrew faction
// Spec: "XyriousStorm Faction Implementation Guide" (all sections Creator Audited)
// Faction color #3371d7 (storm blue), style "xss".
// Led by the Great Old One Petrichor. An elemental storm faction with terrain-
// sensitive Monsters and Pain-assignment ordering dominance via Precipitation.
//
// IMPLEMENTATION NOTES
//  * Faction-Card Monster holding zone (Cloud Of Ashes, SB 3) lives on Game.scala
//    as game.xssFactionCardMonsters : $[UnitRef] (undo/replay-safe — mirrors the
//    DC dcSin / FBE fbeCardDice pattern that keeps faction state on Game.scala).
//    Appended in eliminate() hook; drained by CloudOfAshesDoomReturnAction at Doom.
//  * Precipitation (§1.5) reorders Battle pain-assignment so XSS picks first,
//    regardless of Attacker/Defender role. Fully cancelled by CC Madness.
//  * Petrichor Combat = count of XSS Units in play with Cost >= 2 (includes self).
//  * Twister Combat = 3 in Land Areas, 1 elsewhere. Eye of the Storm = 4 in Sea, 1 elsewhere.
//  * XSS sets startArea at Setup (not on first Awaken, unlike DC).
//  * XSS has NO hard 2-player restriction, NO special map tile, NO unique resource.
// ============================================================================


// -- UNITS (§1.7 / §2.1) -----------------------------------------------------
// XSS uses the standard Acolyte UnitClass for its 6 Acolytes (no XSS-specific
// Cultist subclass). AmphibianCrawler: Monster cost 1, combat 0. Twister: Monster
// cost 2, combat 1+ (3 in Land). EyeOfTheStorm: Monster cost 3, combat 1+ (4 in Sea).
// Petrichor: GOO cost 8, combat = count of XSS Units in play with Cost >= 2.
case object AmphibianCrawler extends FactionUnitClass(XSS, "Amphibian Crawler", Monster, 1)
case object Twister          extends FactionUnitClass(XSS, "Twister",           Monster, 2)
case object EyeOfTheStorm    extends FactionUnitClass(XSS, "Eye of the Storm",  Monster, 3)
case object Petrichor        extends FactionUnitClass(XSS, "Petrichor",         GOO,     8)


// -- SPELLBOOKS (§1.10) -------------------------------------------------------
case object Whirlwind           extends FactionSpellbook(XSS, "Whirlwind")
case object StaticAccumulator   extends FactionSpellbook(XSS, "Static Accumulator") with BattleSpellbook
case object CloudOfAshes        extends FactionSpellbook(XSS, "Cloud Of Ashes")
case object Tsunami             extends FactionSpellbook(XSS, "Tsunami")
case object FrozenSolid         extends FactionSpellbook(XSS, "Frozen Solid")
case object TorrentialDownpour  extends FactionSpellbook(XSS, "Torrential Downpour")

// XSS Unique Ability -- Precipitation (Ongoing). Declared as a FactionSpellbook so
// it can be shown in the faction status panel / borrowed via Ancient Sorcery.
case object Precipitation extends FactionSpellbook(XSS, "Precipitation")


// -- SPELLBOOK REQUIREMENTS (§1.9) --------------------------------------------
case object PetrichorBattlesAloneReq extends Requirement("Petrichor Battles Alone")
case object FourGlyphFootprintReq    extends Requirement("Have Units in 4 Areas with Faction Glyphs")
case object SeaGatesReq              extends Requirement("Control 3 Gates in Sea Areas OR Control 3 Gates in Areas with Faction Glyphs")
case object LandGatesReq             extends Requirement("Control 3 Gates in Land Areas OR Control 3 Gates in Areas with Faction Glyphs")
case object MonsterMassReq           extends Requirement("Have 10 Cost worth of Monsters in play")
case object AwakenPetrichorReq       extends Requirement("Awaken Petrichor")


// ============================================================================
// XYRIOUS STORM (XSS) FACTION OBJECT
// ============================================================================
case object XSS extends Faction { f =>
    def name  = "Xyrious Storm"
    def short = "XSS"
    def style = "xss"

    // Precipitation is the always-on unique ability (§1.5).
    override def abilities = $(Precipitation)
    override def library   = $(Whirlwind, StaticAccumulator, CloudOfAshes, Tsunami, FrozenSolid, TorrentialDownpour)
    override def requirements(options : $[GameOption]) =
        $(PetrichorBattlesAloneReq, FourGlyphFootprintReq, SeaGatesReq, LandGatesReq, MonsterMassReq, AwakenPetrichorReq)

    // 6 Acolytes + 2 Amphibian Crawlers + 4 Twisters + 3 Eyes of the Storm + 1 Petrichor (§1.7)
    val allUnits =
        6.times(Acolyte)           ++
        2.times(AmphibianCrawler)  ++
        4.times(Twister)           ++
        3.times(EyeOfTheStorm)     ++
        1.times(Petrichor)

    // Petrichor awakens via standard path with fixed cost 8, gated on controlling
    // a Unit with Cost >= 3 in the target region (§1.8).
    override def awakenCost(u : UnitClass, r : Region)(implicit game : Game) : |[Int] = u match {
        case Petrichor =>
            // Requires XSS to control at least one Unit with Cost >= 3 in region r
            f.at(r).%(_.uclass.cost >= 3).any.?(8)
        case _ => None
    }

    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int = {
        // Petrichor: dice = count of XSS Units in play with Cost >= 2 (includes himself, §1.8)
        val petrichorCount = units.%(_.uclass == Petrichor).not(Zeroed).num
        val petrichorStr   = (petrichorCount > 0).??(f.allInPlay.%(_.uclass.cost >= 2).num)
        // Twister: 3 in Land, 1 elsewhere (§1.7)
        val twisters = units.%(_.uclass == Twister).not(Zeroed)
        val twisterStr = twisters./(u => (u.region.glyph != Ocean).?(3).|(1)).sum
        // Eye of the Storm: 4 in Sea, 1 elsewhere (§1.7)
        val eyes = units.%(_.uclass == EyeOfTheStorm).not(Zeroed)
        val eyeStr = eyes./(u => (u.region.glyph == Ocean).?(4).|(1)).sum
        // Amphibian Crawlers and Acolytes contribute 0 combat
        twisterStr + eyeStr + petrichorStr + neutralStrength(units, opponent)
    }
}


// ============================================================================
// XYRIOUS STORM (XSS) ACTION CASE CLASSES
// All Soft (navigation-only) sub-menus include Cancel; Hard (state-mutating)
// actions do NOT (per §3 / FCG Cancel discipline).
// ============================================================================

// -- AWAKEN PETRICHOR (§1.8 / §3.4.1 / §4.6) ---------------------------------
// The standard awaken path applies (AwakenAction), but we need a host-picker
// when multiple eligible regions exist. Petrichor appears in the Area of a
// Cost-3+ Unit (§1.8). The standard AwakenAction path handles the region pick.

// -- TSUNAMI (§1.10 SB4 / §3.10.4 / §4.4.4) ----------------------------------
// Soft area/eye pick -> Soft land pick -> Soft extras pick -> Hard commit
case class TsunamiMainAction(self : Faction)
    extends OptionFactionAction(Tsunami.styled(XSS) + " (Cost " + 1.power + ")") with MainQuestion with Soft
case class TsunamiEyePickAction(self : Faction, eyes : $[UnitRef])
    extends ForcedAction with Soft
case class TsunamiDestPickAction(self : Faction, eye : UnitRef, source : Region)
    extends ForcedAction with Soft {
    override def question(implicit game : Game) = Tsunami.styled(XSS) + ": choose Land Area"
}
case class TsunamiExtrasPickAction(self : Faction, eye : UnitRef, source : Region, dest : Region, picked : $[UnitRef], remaining : $[UnitRef])
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) = Tsunami.styled(XSS) + ": bring other Units from " + source + " to " + dest
}
case class TsunamiAction(self : Faction, eye : UnitRef, source : Region, dest : Region, extras : $[UnitRef])
    extends ForcedAction

// -- STATIC ACCUMULATOR (§1.10 SB2 / §3.10.2 / §4.4.2) -----------------------
// Pre-Battle reinforcement: Soft opt-in -> Soft unit pick (from any adjacent areas) -> Hard commit
case class StaticAccumulatorPreBattleMainAction(self : Faction)
    extends OptionFactionAction(StaticAccumulator.styled(XSS)) with PreBattleQuestion with Soft
case class StaticAccumulatorSkipAction(self : Faction)
    extends OptionFactionAction(("Skip " + StaticAccumulator.name).styled(XSS)) with PreBattleQuestion with Soft
case class StaticAccumulatorUnitPickAction(self : Faction, arena : Region, picked : $[UnitRef], remaining : $[UnitRef], costLeft : Int)
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) = StaticAccumulator.styled(XSS) + ": pick Units from adjacent Areas (Cost left: " + costLeft + ")"
}
case class StaticAccumulatorDoneAction(self : Faction, arena : Region, picked : $[UnitRef])
    extends ForcedAction with PowerNeutral
case class StaticAccumulatorAction(self : Faction, arena : Region, picked : $[UnitRef])
    extends ForcedAction

// -- CLOUD OF ASHES (§1.10 SB3 / §3.10.3 / §4.4.3) ---------------------------
// Kill-reroute prompt (per Monster killed)
case class CloudOfAshesHoldAction(self : Faction, u : UnitRef)
    extends OptionFactionAction(implicit g => CloudOfAshes.styled(XSS) + ": send " + g.unit(u).uclass.styled(XSS) + " to Faction Card?") with Soft { override def question(implicit game : Game) = CloudOfAshes.styled(XSS) }
case class CloudOfAshesDeclineAction(self : Faction, u : UnitRef)
    extends OptionFactionAction(implicit g => CloudOfAshes.styled(XSS) + ": return " + g.unit(u).uclass.styled(XSS) + " to Pool") with Soft { override def question(implicit game : Game) = CloudOfAshes.styled(XSS) }
// Doom Phase return
case class CloudOfAshesDoomReturnMainAction(self : Faction)
    extends OptionFactionAction(CloudOfAshes.styled(XSS) + ": return a Monster") with DoomQuestion with Soft
case class CloudOfAshesDoomReturnPickAction(self : Faction, held : $[UnitRef])
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) = CloudOfAshes.styled(XSS) + ": choose Monster to return"
}
case class CloudOfAshesDoomReturnAreaAction(self : Faction, monster : UnitRef)
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) = CloudOfAshes.styled(XSS) + ": choose Area with Controlled Gate"
}
case class CloudOfAshesDoomReturnAction(self : Faction, monster : UnitRef, dest : Region)
    extends BaseFactionAction(CloudOfAshes.styled(XSS) + ": return Monster to", dest)

// -- WHIRLWIND (§1.10 SB1 / Retreat expansion) --------------------------------
// Post-Battle: Twisters in Land Areas may Retreat to Sea Areas containing enemy
// Units. This is a passive retreat-destination expansion — no action needed.
// Hooked into Battle.scala retreat logic (retreat destination computation).

// -- DISTANT THUNDERCLAP (§1.8 / §3.4.3 / §4.6) ------------------------------
// Post-Battle: assign XSS's own excess Pains to XSS's own units (adds to normal
// pain pool — pained units retreat in the standard retreat phase, no separate mini-retreat).
// If after resolution no units remain in the battle area AND opponent had a non-Cultist, gain Elder Sign.
case class DistantThunderclapOfferAction(self : Faction, excessPains : Int, arena : Region, opponent : Faction, opponentHadNonCultist : Boolean)
    extends OptionFactionAction("Distant Thunderclap".styled(XSS) + " (assign " + excessPains + " excess Pain to self)") with PostBattleQuestion with Soft
case class DistantThunderclapSkipAction(self : Faction)
    extends OptionFactionAction("Skip " + "Distant Thunderclap".styled(XSS)) with PostBattleQuestion with Soft
case class DistantThunderclapPainAction(self : Faction, remaining : Int, candidates : $[UnitRef], arena : Region, opponent : Faction, opponentHadNonCultist : Boolean)
    extends ForcedAction with PowerNeutral {
    override def question(implicit game : Game) = "Distant Thunderclap".styled(XSS) + ": assign excess Pain (" + remaining + " left)"
}
case class DistantThunderclapPainTargetAction(self : Faction, target : UnitRef, remaining : Int, candidates : $[UnitRef], arena : Region, opponent : Faction, opponentHadNonCultist : Boolean)
    extends BaseFactionAction(implicit g => "Distant Thunderclap".styled(XSS) + ": Pain", implicit g => g.unit(target).uclass.styled(XSS))
case class DistantThunderclapElderSignAction(self : Faction)
    extends ForcedAction


// ============================================================================
// XYRIOUS STORM (XSS) EXPANSION -- game-loop integration
// ============================================================================
object XSSExpansion extends Expansion {

    // -- FROZEN SOLID helper: determine if a region is a Faction Glyph Area ----
    // A Faction Glyph Area = any region with a faction glyph PRINTED on the map
    // (physical map property — GC/CC/BG/YS/SL/WW starting areas regardless of
    // whether that faction is in the game) OR any player's chosen Start Area.
    def isFactionGlyphArea(r : Region)(implicit game : Game) : Boolean = {
        val coreFactions = $(GC, CC, BG, YS, SL, WW)
        val printedGlyphs = coreFactions./~(fx => game.board.starting(fx)).distinct
        val chosenAreas = game.factions./~(fx => game.starting.get(fx).toList)
        val factionGlyphAreas = (printedGlyphs ++ chosenAreas).distinct
        factionGlyphAreas.has(r)
    }

    // -- CLOUD OF ASHES kill-path hook (§3.10.3 / §3.14.8) --------------------
    // Called on every Unit elimination. If an XSS Monster dies and SB 3 is on
    // the sheet, intercept the kill: remove Eliminated tag (prevents pool-return),
    // move to XSSFactionCardHold, record in xssFactionCardMonsters. A per-kill
    // Yes/No prompt fires post-EliminatePhase so the player can decline.
    override def eliminate(u : UnitFigure)(implicit game : Game) {
        if (!game.setup.has(XSS)) return
        if (u.faction == XSS && u.uclass.utype == Monster && XSS.can(CloudOfAshes)) {
            // Intercept: remove Eliminated tag so Game.eliminate does NOT send to pool.
            u.remove(Eliminated)
            // Park on the faction card holding region.
            u.region = XSSFactionCardHold(XSS)
            u.onGate = false
            u.health = Alive
            u.state = $
            // Record ref for Doom Phase return logic.
            game.xssFactionCardMonsters :+= u.ref
        }
    }

    // -- CONDITION-BASED SBR evaluation (§3.12) --------------------------------
    override def triggers()(implicit game : Game) {
        if (!game.setup.has(XSS)) return
        val f = XSS

        // Faction-Glyph Areas = any region with a faction glyph PRINTED on the
        // map (GC/CC/BG/YS/SL/WW starting areas regardless of game membership)
        // OR any player's chosen Start Area. NOT XSS-own glyphs (§1.9).
        val coreFactions = $(GC, CC, BG, YS, SL, WW)
        val factionGlyphAreas = {
            val printedGlyphs = coreFactions./~(fx => game.board.starting(fx)).distinct
            val chosenAreas = game.factions./~(fx => game.starting.get(fx).toList)
            (printedGlyphs ++ chosenAreas).distinct
        }

        // SBR 2 -- Four-Glyph Footprint (§3.12.2): count distinct Faction-Glyph
        // Areas containing at least one XSS Unit.
        val xssInGlyphAreas = factionGlyphAreas.%(r => f.at(r).any).num
        f.satisfyIf(FourGlyphFootprintReq, FourGlyphFootprintReq.text, xssInGlyphAreas >= 4)

        // SBR 3 -- Sea Gates (§3.12.3): 3 XSS Gates in Sea Areas OR 3 XSS Gates
        // in Faction-Glyph Areas.
        val seaGates = f.gates.%(_.glyph == Ocean).num
        val glyphGates = f.gates.%(r => factionGlyphAreas.has(r)).num
        f.satisfyIf(SeaGatesReq, SeaGatesReq.text, seaGates >= 3 || glyphGates >= 3)

        // SBR 4 -- Land Gates (§3.12.4): 3 XSS Gates in Land Areas OR 3 XSS Gates
        // in Faction-Glyph Areas.
        val landGates = f.gates.%(_.glyph != Ocean).num
        f.satisfyIf(LandGatesReq, LandGatesReq.text, landGates >= 3 || glyphGates >= 3)

        // SBR 5 -- Monster Mass (§3.12.5): sum of Costs of XSS Monsters on map >= 10.
        val monsterCostSum = f.onMap(AmphibianCrawler).num * 1 +
                             f.onMap(Twister).num * 2 +
                             f.onMap(EyeOfTheStorm).num * 3
        f.satisfyIf(MonsterMassReq, MonsterMassReq.text, monsterCostSum >= 10)
    }

    def perform(action : Action, soft : VoidGuard)(implicit game : Game) = action @@ {

        // -- SETUP (§1.6 / §2.0a / §3.8.1) ------------------------------------
        // XSS Setup: pick an empty Sea Area with no Faction Glyph, place 6 Acolytes
        // + Controlled Gate. Starting Power = 8 (set by faction power table in Game.scala).
        // XSS uses StartingRegionAction flow with region filtering handled by Game.scala.
        case SetupFactionsAction if game.setup.has(XSS) && !game.starting.contains(XSS) =>
            val f = XSS
            // Find eligible Sea Areas: empty of Units, no Faction Glyph (§1.6 / §2.0a)
            // A Faction Glyph Area = any region with a faction glyph PRINTED on the map
            // (physical map property — doesn't change based on who's in the game).
            // Bug fix: previously used inGame/offBoard split which missed cases where
            // a faction (e.g. WW) is in the game but chose a DIFFERENT starting area
            // (e.g. Antarctica), leaving the other printed-glyph area (ArcticOcean)
            // incorrectly unexcluded. Now we simply collect ALL starting regions for
            // ALL core factions from the board definition, plus any already-chosen areas.
            val coreFactions = $(GC, CC, BG, YS, SL, WW)
            val glyphAreas = {
                val printedGlyphs = coreFactions./~(fx => game.board.starting(fx)).distinct
                val chosenAreas = game.factions./~(fx => game.starting.get(fx).toList)
                (printedGlyphs ++ chosenAreas).distinct
            }
            val eligible = game.board.regions.%(r =>
                r.glyph == Ocean &&
                game.factions.forall(e => e.at(r).none) &&
                !glyphAreas.has(r))
            if (eligible.num == 1)
                Force(StartingRegionAction(f, eligible.head))
            else
                Ask(f).each(eligible)(r => StartingRegionAction(f, r).as(r)(f, "starts in")).cancel

        case StartingRegionAction(self : XSS.type, r) =>
            // Place 6 Acolytes and a Controlled Gate in the chosen Sea Area (§1.6)
            game.starting = game.starting + (XSS -> r)
            1.to(6).foreach(_ => self.place(Acolyte, r))
            game.gates :+= r
            self.gates :+= r
            self.at(r).one(Acolyte).onGate = true
            self.log("starts in", r, "with", 6, "Acolytes and a Controlled Gate")
            Force(SetupFactionsAction)

        // -- DOOM PHASE (§3.10.3 Cloud Of Ashes Doom return) -------------------
        case DoomAction(f : XSS.type) =>
            implicit val asking = Asking(f)

            // Cloud Of Ashes: find Monsters held on the Faction Card by scanning the
            // hold region directly. xssFactionCardMonsters is cleared during the
            // per-kill Hold prompts, so Doom Phase scans the region instead.
            val heldOnCard = f.units.%(_.region == XSSFactionCardHold(f))./(_.ref)
            if (f.can(CloudOfAshes) && heldOnCard.any) {
                // If XSS has zero Controlled Gates, return all to Pool immediately
                if (f.gates.none) {
                    heldOnCard.foreach { ur => game.unit(ur).region = f.reserve }
                    f.log(CloudOfAshes.styled(XSS) + ": no Controlled Gates — all Monsters return to Pool")
                } else {
                    + CloudOfAshesDoomReturnMainAction(f)
                }
            }

            game.rituals(f)

            game.reveals(f)

            game.highPriests(f)

            game.hires(f)

            game.doomDone(f)

            asking

        // -- CLOUD OF ASHES DOOM RETURN (§3.10.3 / §4.4.3) --------------------
        case CloudOfAshesDoomReturnMainAction(self) =>
            val held = self.units.%(_.region == XSSFactionCardHold(self))./(_.ref)
            if (held.num == 1)
                Force(CloudOfAshesDoomReturnAreaAction(self, held.head))
            else
                Force(CloudOfAshesDoomReturnPickAction(self, held))

        case CloudOfAshesDoomReturnPickAction(self, held) =>
            Ask(self).each(held)(ur =>
                CloudOfAshesDoomReturnAreaAction(self, ur)
                    .as(game.unit(ur).uclass.styled(XSS)))

        case CloudOfAshesDoomReturnAreaAction(self, monster) =>
            val gateAreas = self.gates
            if (gateAreas.num == 1)
                Force(CloudOfAshesDoomReturnAction(self, monster, gateAreas.head))
            else
                Ask(self).each(gateAreas)(r => CloudOfAshesDoomReturnAction(self, monster, r))

        case CloudOfAshesDoomReturnAction(self, monster, dest) =>
            game.unit(monster).region = dest
            self.log(CloudOfAshes.styled(XSS) + ": returned", game.unit(monster).uclass.styled(XSS), "to", dest)
            val remaining = self.units.%(_.region == XSSFactionCardHold(self))./(_.ref).but(monster)
            remaining.foreach { ur => game.unit(ur).region = self.reserve }
            if (remaining.any)
                self.log(CloudOfAshes.styled(XSS) + ": remaining Monsters returned to Pool")
            Force(DoomAction(self))

        // -- MAIN ACTION (§2.7 / §3.10) ----------------------------------------
        case MainAction(f : XSS.type) if f.active.not =>
            UnknownContinue

        case MainAction(f : XSS.type) if f.acted =>
            UnknownContinue

        case MainAction(f : XSS.type) =>
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

            // Tsunami (§1.10 SB4): Action, Cost 1. Requires an Eye of the Storm in
            // a Sea Area with an adjacent Land Area.
            if (f.can(Tsunami) && f.power >= 1 &&
                f.onMap(EyeOfTheStorm).%(u => u.region.glyph == Ocean &&
                    game.board.connected(u.region).%(_.glyph != Ocean).any).any)
                + TsunamiMainAction(f)

            game.neutralSpellbooks(f)
            game.libraryActions(f)
            game.highPriests(f)
            game.reveals(f)

            game.endTurn(f)(f.battled.any || game.nexed.any)

            asking

        // -- TSUNAMI (§1.10 SB4 / §3.10.4) ------------------------------------
        case TsunamiMainAction(self) =>
            val eligibleEyes = self.onMap(EyeOfTheStorm).%(u =>
                u.region.glyph == Ocean &&
                game.board.connected(u.region).%(_.glyph != Ocean).any)
            if (eligibleEyes.num == 1)
                Force(TsunamiDestPickAction(self, eligibleEyes.head.ref, eligibleEyes.head.region))
            else
                Force(TsunamiEyePickAction(self, eligibleEyes./(_.ref)))

        case TsunamiEyePickAction(self, eyes) =>
            Ask(self).each(eyes)(ur =>
                TsunamiDestPickAction(self, ur, game.unit(ur).region)
                    .as(EyeOfTheStorm.styled(XSS) + " in " + game.unit(ur).region)).cancel

        case TsunamiDestPickAction(self, eye, source) =>
            val landAreas = game.board.connected(source).%(_.glyph != Ocean)
            if (landAreas.num == 1)
                Force(TsunamiExtrasPickAction(self, eye, source, landAreas.head, $,
                    self.at(source).%(u => u.ref != eye)./(_.ref)))
            else
                Ask(self).each(landAreas)(r =>
                    TsunamiExtrasPickAction(self, eye, source, r, $,
                        self.at(source).%(u => u.ref != eye)./(_.ref))
                        .as(r)).cancel

        case TsunamiExtrasPickAction(self, eye, source, dest, picked, remaining) =>
            implicit val asking = Asking(self)
            remaining.foreach { ur =>
                + TsunamiExtrasPickAction(self, eye, source, dest, picked :+ ur, remaining.but(ur))
                    .as("Add " + game.unit(ur).uclass.styled(XSS) + " in " + source)
            }
            // Done (commit with current picks or no extras)
            + TsunamiAction(self, eye, source, dest, picked).as("Done".styled("power"))
            + CancelAction
            asking

        case TsunamiAction(self, eye, source, dest, extras) =>
            // Pay Cost 1 Power
            self.power -= 1
            // Move Eye of the Storm
            game.unit(eye).region = dest
            // Move extras
            val dcAcolyteMoved = extras.exists(ur => game.unit(ur).faction == DC && game.unit(ur).uclass == Acolyte)
            extras.foreach { ur => game.unit(ur).region = dest }
            self.log(Tsunami.styled(XSS) + ": moved", EyeOfTheStorm.styled(XSS),
                (extras.any).??(" and " + extras.num + " other Unit" + (extras.num > 1).??("s")),
                "from", source, "to", dest)
            if (dcAcolyteMoved && game.factions.has(DC) && DC.can(Proselytize))
                Force(DCProselytizeCheckAction(source, dest, EndAction(self)))
            else
                EndAction(self)

        // -- STATIC ACCUMULATOR (§1.10 SB2 / §3.10.2) -------------------------
        // Pre-Battle: handled via PreBattleQuestion. Perform logic here for the
        // Soft chain and Hard commit. Allows picking units from ANY adjacent areas
        // (multiple sources) as long as total cost <= 4.
        case StaticAccumulatorPreBattleMainAction(self) =>
            game.battle match {
                case Some(b) =>
                    // Gather all XSS units in all adjacent areas
                    val adjacent = game.board.connected(b.arena).%(r => self.at(r).any)
                    val allUnits = adjacent./~(r => self.at(r)./(_.ref))
                    if (allUnits.none)
                        UnknownContinue
                    else
                        Force(StaticAccumulatorUnitPickAction(self, b.arena, $, allUnits, 4))
                case None => UnknownContinue
            }

        case StaticAccumulatorSkipAction(self) =>
            self.log(StaticAccumulator.styled(XSS) + ": declined")
            UnknownContinue

        case StaticAccumulatorUnitPickAction(self, arena, picked, remaining, costLeft) =>
            implicit val asking = Asking(self)
            remaining.%(ur => game.unit(ur).uclass.cost <= costLeft).foreach { ur =>
                val u = game.unit(ur)
                + StaticAccumulatorUnitPickAction(self, arena,
                    picked :+ ur, remaining.but(ur), costLeft - u.uclass.cost)
                    .as(u.uclass.styled(XSS) + " in " + u.region + " (Cost " + u.uclass.cost + ")")
            }
            if (picked.any)
                + StaticAccumulatorDoneAction(self, arena, picked)
                    .as("Done".styled("power"))
            + CancelAction
            asking

        case StaticAccumulatorDoneAction(self, arena, picked) =>
            Force(StaticAccumulatorAction(self, arena, picked))

        case StaticAccumulatorAction(self, arena, picked) =>
            // Move picked Units into the Battle Area
            val sources = picked./(ur => game.unit(ur).region).distinct
            picked.foreach { ur => game.unit(ur).region = arena }
            self.log(StaticAccumulator.styled(XSS) + ": moved", picked.num,
                "Unit" + (picked.num > 1).??("s"), "from",
                sources.mkString(", "), "to", arena)
            UnknownContinue

        // -- DISTANT THUNDERCLAP (§1.8 / §3.4.3) ------------------------------
        // Post-Battle: OPTIONAL. Assign XSS's own excess Pains to XSS's own units.
        // Elder Sign if no units remain in battle area AND opponent had non-Cultist.
        case DistantThunderclapOfferAction(self, excessPains, arena, opponent, opponentHadNonCultist) =>
            val b = game.battle.get
            val xssSide = if (b.attacker == XSS) b.attackers else b.defenders
            val candidates = xssSide.forces.%(_.faction == XSS).%(_.health == Alive)./(_.ref)
            Force(DistantThunderclapPainAction(self, excessPains, candidates, arena, opponent, opponentHadNonCultist))

        case DistantThunderclapSkipAction(self) =>
            self.log("Distant Thunderclap".styled(XSS) + ": declined")
            Force(BattleProceedAction(AssignDefenderPains))

        case DistantThunderclapPainAction(self, remaining, candidates, arena, opponent, opponentHadNonCultist) =>
            if (remaining <= 0 || candidates.none)
                Force(BattleProceedAction(AssignDefenderPains))
            else if (candidates.num == 1)
                Force(DistantThunderclapPainTargetAction(self, candidates.head, remaining, candidates, arena, opponent, opponentHadNonCultist))
            else
                Ask(self).each(candidates)(ur =>
                    DistantThunderclapPainTargetAction(self, ur, remaining, candidates, arena, opponent, opponentHadNonCultist))

        case DistantThunderclapPainTargetAction(self, target, remaining, candidates, arena, opponent, opponentHadNonCultist) =>
            val u = game.unit(target)
            u.health = Pained
            self.log("Distant Thunderclap".styled(XSS) + ": excess Pain assigned to", u.uclass.styled(XSS))
            Force(DistantThunderclapPainAction(self, remaining - 1, candidates.but(target), arena, opponent, opponentHadNonCultist))

        // -- AWAKEN PETRICHOR SBR (§3.12.6) ------------------------------------
        case AwakenedAction(self : XSS.type, Petrichor, _, _) =>
            self.satisfy(AwakenPetrichorReq, "Awaken Petrichor")
            EndAction(self)

        // -- CLOUD OF ASHES per-kill hold prompt (§3.14.8) ---------------------
        // These actions fire from Battle's CloudOfAshesPromptPhase.
        // Must return to CloudOfAshesPromptPhase to check for more pending monsters.
        case CloudOfAshesHoldAction(self, u) =>
            // Unit is already on XSSFactionCardHold from the eliminate hook; confirm hold.
            // Remove from the pending-prompt list so CloudOfAshesPromptPhase does not re-ask;
            // Doom Phase will find it by scanning XSSFactionCardHold(XSS) directly.
            game.xssFactionCardMonsters = game.xssFactionCardMonsters.but(u)
            self.log(CloudOfAshes.styled(XSS) + ":", game.unit(u).uclass.styled(XSS), "placed on Faction Card")
            Force(BattleProceedAction(CloudOfAshesPromptPhase))

        case CloudOfAshesDeclineAction(self, u) =>
            // Remove from holding, return to Pool
            game.xssFactionCardMonsters = game.xssFactionCardMonsters.but(u)
            game.unit(u).region = self.reserve
            self.log(CloudOfAshes.styled(XSS) + ":", game.unit(u).uclass.styled(XSS), "returned to Pool")
            Force(BattleProceedAction(CloudOfAshesPromptPhase))

        // -- CATCH-ALL ---------------------------------------------------------
        case _ => UnknownContinue
    }
}
