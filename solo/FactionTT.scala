package cws

import hrf.colmat._

import html._


// ============================================================================
// Tcho-Tcho (TT) UNITS: Acolyte (cultist), High Priest (cultist),
// Proto-Shoggoth (monster, Terror — modifies battle), Ubbo-Sathla (GOO,
// combat = Growth counter value set by Hell's Banquet)
// ============================================================================
case object ProtoShoggoth extends FactionUnitClass(TT, "Proto-Shoggoth", Monster, 2)
case object UbboSathla extends FactionUnitClass(TT, "Ubbo-Sathla", GOO, 6)


// ============================================================================
// Tcho-Tcho (TT) SPELLBOOKS — Faction Ability + 6 Library spellbooks
// All tribes share: Hierophants, Soulless, Terror (3 shared SBs)
// Tsang exclusive: Idolatry, Martyrdom, Tablets of the Gods
// Leng exclusive: Dark Rituals, Fulmination, Surprise!
// Sarkomand exclusive: Doomsday, Inerrant, Otherworld Alliances
// ============================================================================
// FACTION POWER — use .has(), NOT blocked by Moonbeast or Elder Thing
case object Sycophancy extends FactionSpellbook(TT, "Sycophancy")

// SHARED SPELLBOOKS (all tribes) — use .can(), CAN be blocked by Moonbeast
case object Hierophants extends FactionSpellbook(TT, "Hierophants")
case object Soulless extends FactionSpellbook(TT, "Soulless")
case object TerrorSB extends FactionSpellbook(TT, "Terror")

// TSANG EXCLUSIVE SPELLBOOKS
case object Idolatry extends FactionSpellbook(TT, "Idolatry")
case object Martyrdom extends FactionSpellbook(TT, "Martyrdom")
case object TabletsOfTheGods extends FactionSpellbook(TT, "Tablets of the Gods")

// LENG EXCLUSIVE SPELLBOOKS
case object DarkRituals extends FactionSpellbook(TT, "Dark Rituals")
case object Fulmination extends FactionSpellbook(TT, "Fulmination")
case object SurpriseSB extends FactionSpellbook(TT, "Surprise!")

// SARKOMAND EXCLUSIVE SPELLBOOKS
case object Doomsday extends FactionSpellbook(TT, "Doomsday")
case object Inerrant extends FactionSpellbook(TT, "Inerrant")
case object OtherworldAlliances extends FactionSpellbook(TT, "Otherworld Alliances")


// ============================================================================
// Tcho-Tcho (TT) TRIBE SELECTION OBJECTS
// Extend Record so the serializer's reflection lookup finds them by className
// ============================================================================
sealed trait TTTribe extends Record
case object TribeLeng extends TTTribe
case object TribeSarkomand extends TTTribe
case object TribeTsang extends TTTribe


// ============================================================================
// Tcho-Tcho (TT) SPELLBOOK REQUIREMENTS: conditions for the 6 SB slots
// ============================================================================
case object TTSycophancyTrigger    extends Requirement("Enemy RoA / 15D")
case object TTEarnElderSign        extends Requirement("Earn an Elder Sign")
case object TTThreeElderSigns      extends Requirement("Own 3+ Elder Signs")
case object TTRemoveControlledGate extends Requirement("Remove Start Gate")
case object TTGOOKilledInBattle    extends Requirement("Any GOO Killed in Battle")
case object TTAwakenUbboSathla     extends Requirement("Awaken Ubbo-Sathla")


// ============================================================================
// Tcho-Tcho (TT) FACTION OBJECT
// ============================================================================
case object TT extends Faction { f =>
    def name = "Tcho-Tcho"
    def short = "TT"
    def style = "tt"

    override def abilities = $(Sycophancy)
    override def library = if (TTExpansion.ttActiveLibrary.any) TTExpansion.ttActiveLibrary else
        $(Hierophants, Soulless, TerrorSB, Idolatry, Martyrdom, TabletsOfTheGods,
          DarkRituals, Fulmination, SurpriseSB, Doomsday, Inerrant, OtherworldAlliances)
    override def requirements(options : $[GameOption]) = $(TTSycophancyTrigger, TTEarnElderSign, TTThreeElderSigns, TTRemoveControlledGate, TTGOOKilledInBattle, TTAwakenUbboSathla)

    val allUnits =
        1.times(UbboSathla) ++
        6.times(ProtoShoggoth) ++
        3.times(HighPriest) ++
        6.times(Acolyte)

    override def awakenCost(u : UnitClass, r : Region)(implicit game : Game) : |[Int] = u match {
        case UbboSathla => (f.gates.has(r) && f.all(HighPriest).onMap.any).?(6)
        case _ => None
    }

    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int = {
        val protoShoggothCount = units(ProtoShoggoth).not(Zeroed).num
        val ubboCount          = units(UbboSathla).not(Zeroed).num
        val neutralBase = neutralStrength(units, opponent)
        // Otherworld Alliances: TT's Neutral Monsters and Terrors get +1 Combat
        val owa = f.can(OtherworldAlliances).??(
            units.%(u => u.uclass.utype == Monster || u.uclass.utype == Terror).not(Zeroed).num
        )
        protoShoggothCount * 1 +
        ubboCount * game.ubboGrowth +
        neutralBase + owa
    }
}


// ============================================================================
// Tcho-Tcho (TT) ACTION CLASSES
// ============================================================================

// TRIBE SELECTION (before placement)
case class TTChooseTribeAction(self : Faction, tribe : TTTribe) extends BaseFactionAction(
    "Choose Tribe".styled(TT) + " (secret until " + "tribal spellbook".styled(TT) + " revealed)",
    implicit g => {
        val qm = Overlays.imageSource("question-mark")
        tribe match {
            case TribeLeng      =>
                val p = "&quot;TT-TribeLeng&quot;"
                "<div class=sbdiv>" + "Tribe of Leng".styled(TT) + s"""<img class=explain src="${qm}" onclick="event.stopPropagation(); onExternalClick(${p})" onpointerover="onExternalOver(${p})" onpointerout="onExternalOut(${p})" /></div>"""
            case TribeSarkomand =>
                val hasIGOOs = g.loyaltyCards.exists(c => c.isInstanceOf[IGOOLoyaltyCard] && (c.asInstanceOf[IGOOLoyaltyCard].power == 2 || c.asInstanceOf[IGOOLoyaltyCard].power == 4))
                val p = "&quot;TT-TribeSarkomand&quot;, " + hasIGOOs
                "<div class=sbdiv>" + "Tribe of Sarkomand".styled(TT) + " (requires Independent GOOs + Neutral Monsters)" + s"""<img class=explain src="${qm}" onclick="event.stopPropagation(); onExternalClick(${p})" onpointerover="onExternalOver(${p})" onpointerout="onExternalOut(${p})" /></div>"""
            case TribeTsang     =>
                val p = "&quot;TT-TribeTsang&quot;"
                "<div class=sbdiv>" + "Tribe of Tsang".styled(TT) + " (OG " + "TT".styled(TT) + " Spellbooks)" + s"""<img class=explain src="${qm}" onclick="event.stopPropagation(); onExternalClick(${p})" onpointerover="onExternalOver(${p})" onpointerout="onExternalOut(${p})" /></div>"""
        }
    }
)

// HELL'S BANQUET (doom phase d6 roll for Ubbo-Sathla growth — mandatory when Ubbo is in play)
case class TTHellsBanquetRollAction(self : Faction) extends ForcedAction with PowerNeutral

// UNSPEAKABLE OATH (sacrifice High Priest for 2 power — any time)
case class TTUnspeakableOathMainAction(self : Faction) extends OptionFactionAction(
    "Unspeakable Oath: sacrifice " + HighPriest.styled(TT) + " for " + 2.power
) with MainQuestion with Soft
case class TTUnspeakableOathAction(self : Faction, u : UnitRef) extends BaseFactionAction(
    "Sacrifice " + HighPriest.styled(TT),
    implicit g => g.unit(u).full + " for " + 2.power
)

// REMOVE GATE (SBR: remove controlled gate in start area)
case class TTRemoveGateMainAction(self : Faction) extends OptionFactionAction(
    "Remove Start Gate (Spellbook)"
) with MainQuestion with Soft
case class TTRemoveGateAction(self : Faction, r : Region) extends BaseFactionAction(
    "Remove Controlled Gate from", r
)

// DARK RITUALS (Leng exclusive: all enemy factions with TT HP in start area pay 2P or 2D)
case class TTDarkRitualsMainAction(self : Faction) extends OptionFactionAction(
    DarkRituals.styled(TT) + ": enemies with your High Priests in their Start Area pay 2 Power or 2 Doom"
) with MainQuestion with Soft
case class TTDarkRitualsPayAction(self : Faction, target : Faction) extends ForcedAction
case class TTDarkRitualsPayPowerAction(self : Faction, payer : Faction) extends BaseFactionAction("Pay 2 Power", payer.toString)
case class TTDarkRitualsPay2DoomAction(self : Faction, payer : Faction) extends BaseFactionAction("Pay 2 Doom", payer.toString)
case class TTDarkRitualsDoneAction(self : Faction) extends ForcedAction

// FULMINATION (Leng exclusive: if Ubbo killed in battle, optionally remove permanently for X ES)
case class TTFulminationOfferAction(self : Faction, totalKills : Int) extends ForcedAction
case class TTFulminationTakeAction(self : Faction, totalKills : Int) extends BaseFactionAction(
    Fulmination.styled(TT), implicit g => "Remove " + UbboSathla.styled(TT) + " permanently for " + totalKills.es
)
case class TTFulminationDeclineAction(self : Faction) extends BaseFactionAction(
    "Decline", implicit g => UbboSathla.styled(TT) + " is killed normally"
)

// SURPRISE! (Leng exclusive: cost 2, enemy faction eliminates one Acolyte, replaced by Proto-Shoggoth)
case class TTSurpriseMainAction(self : Faction) extends OptionFactionAction(
    SurpriseSB.styled(TT) + " (cost " + 2.power + ": enemy eliminates Acolyte, replaced by " + ProtoShoggoth.styled(TT) + ")"
) with MainQuestion with Soft
case class TTSurpriseChooseFactionAction(self : Faction, enemies : $[Faction]) extends ForcedAction
case class TTSurpriseTargetFactionAction(self : Faction, target : Faction) extends BaseFactionAction(
    "Choose enemy faction", target.toString
)
case class TTSurpriseEliminateAcolyteAction(self : Faction, target : Faction) extends ForcedAction
case class TTSurpriseAcolyteChoiceAction(self : Faction, target : Faction, u : UnitRef) extends BaseFactionAction(
    "Eliminate " + Acolyte.styled(TT), implicit g => g.unit(u).full
)

// IDOLATRY (Tsang exclusive: cost 1, select Faction Glyph area, move TT units from adjacent areas)
// Flow: 1) choose destination (faction glyph region), 2) choose source region + build unit pool, 3) execute
case class TTIdolatryMainAction(self : Faction) extends OptionFactionAction(
    Idolatry.styled(TT) + " (cost " + 1.power + ": select Faction Glyph area, move TT units from adjacent areas)"
) with MainQuestion with Soft
// Step 1: choose destination region; pool always starts empty
case class TTIdolatryChooseDestAction(self : Faction, targets : $[Region], pool : $[UnitRef]) extends ForcedAction
// Cancel at destination — refund power, return to action menu
case class TTIdolatryCancelAction(self : Faction) extends ForcedAction
// Step 2: choose source region (or Done/CancelAll); pool accumulates selected units
case class TTIdolatryChooseSourceAction(self : Faction, dest : Region, pool : $[UnitRef]) extends ForcedAction
// Execute: move all pooled units, log each
case class TTIdolatryDoneAction(self : Faction, dest : Region, pool : $[UnitRef]) extends ForcedAction
// Step 3: choose a unit from source to add to pool
case class TTIdolatryChooseUnitAction(self : Faction, dest : Region, src : Region, pool : $[UnitRef]) extends ForcedAction
// Add one unit from src to pool
case class TTIdolatryAddUnitAction(self : Faction, dest : Region, src : Region, u : UnitRef, pool : $[UnitRef]) extends BaseFactionAction(
    "", implicit g => g.unit(u).full + " from " + src + " → " + dest
)
// Undo last unit added from src (removes last pool entry whose origin is src)
case class TTIdolatryUndoLastAction(self : Faction, dest : Region, src : Region, pool : $[UnitRef]) extends BaseFactionAction(
    "", implicit g => "Undo last unit from " + src + " from the move pool"
)
// Cancel all units from src — remove from pool, return to source picker
case class TTIdolatryCancelSourceAction(self : Faction, dest : Region, src : Region, pool : $[UnitRef]) extends BaseFactionAction(
    "", implicit g => "Remove all units from " + src + " from the move pool"
)

// TERROR (all tribes: pre-battle choice — reduce enemy or boost own)
case class TTTerrorPreBattleAction(self : Faction) extends OptionFactionAction(TerrorSB) with PreBattleQuestion
case class TTTerrorReduceEnemyAction(self : Faction, n : Int) extends BaseFactionAction(
    "Reduce enemy combat by " + n + " (" + TerrorSB.styled(TT) + ")", implicit g => n + " Proto-Shoggoth".s(n) + " in battle"
)
case class TTTerrorBoostOwnAction(self : Faction, n : Int) extends BaseFactionAction(
    "Increase own combat by " + n + " (" + TerrorSB.styled(TT) + ")", implicit g => n + " Proto-Shoggoth".s(n) + " in battle"
)
case object TTReduceEnemyCombat extends FactionSpellbook(TT, "TT Terror Reduce") with BattleSpellbook
case object TTBoostOwnCombat extends FactionSpellbook(TT, "TT Terror Boost") with BattleSpellbook

// MARTYRDOM (Tsang exclusive: post-battle passive — if HP killed, all other kills become pains)
// Handled via assignKill hook in Battle.scala — no action class needed

// TABLETS OF THE GODS (Tsang exclusive: doom phase — handled in TTExpansion DoomAction)

// DOOMSDAY (Sarkomand exclusive: once-only, place cost-2 or cost-4 iGOO at gate, take loyalty card)
case class TTDoomsdayMainAction(self : Faction) extends OptionFactionAction(
    Doomsday.styled(TT) + " (once only: place cost-2 or cost-4 iGOO at your Gate)"
) with MainQuestion with Soft
case class TTDoomsdayChooseIGOOAction(self : Faction, cards : $[IGOOLoyaltyCard]) extends ForcedAction
case class TTDoomsdaySelectIGOOAction(self : Faction, card : IGOOLoyaltyCard) extends BaseFactionAction(
    Doomsday.styled(TT) + ": place iGOO for free", {
    val qm = Overlays.imageSource("question-mark")
    val p = s""""${card.name.replace("\\", "\\\\")}"""".replace("\"", "&quot;") + ", false"
    "<div class=sbdiv>" +
        card.name.styled("nt") +
    s"""<img class=explain src="${qm}" onclick="event.stopPropagation(); onExternalClick(${p})" onpointerover="onExternalOver(${p})" onpointerout="onExternalOut(${p})" />""" +
    "</div>"
})
case class TTDoomsdayChooseGateAction(self : Faction, card : LoyaltyCard, gates : $[Region]) extends ForcedAction
case class TTDoomsdayPlaceAction(self : Faction, card : LoyaltyCard, r : Region) extends BaseFactionAction(
    "Place at", r
)

// INERRANT (Sarkomand exclusive: doom phase ritual bonus ES — handled in Game.scala RitualAction)
// OTHERWORLD ALLIANCES (Sarkomand exclusive: +1 combat to neutral monsters/terrors — handled in neutralStrength in Game.scala)

// SYCOPHANCY (faction ability: enemy chooses when ritualing — injected in Game.scala RitualAction)
// TTSycophancyResumeRitualAction is the shared continuation used by both normal and Sycophancy-adjusted ritual resolution
case class TTSycophancyResumeRitualAction(ritualer : Faction, doom : Int, es : Int) extends ForcedAction
case class TTSycophancyPromptAction(ritualer : Faction, doom : Int, es : Int) extends ForcedAction
case class TTSycophancyLoseDoomAction(self : Faction, doom : Int, es : Int) extends BaseFactionAction(
    "Gain 1 fewer Doom (Sycophancy)", implicit g => "Gain " + (doom - 1) + " Doom instead of " + doom
)
case class TTSycophancyGiveDoomAction(self : Faction, doom : Int, es : Int) extends BaseFactionAction(
    "Give 1 Doom to " + TT.toString + " (Sycophancy)", TT.toString + " gains 1 Doom"
)

// HIEROPHANTS (all tribes: on spellbook earn, place HP at gate or grow counter)
case class TTHierophantsPlaceHPAction(self : Faction, gates : $[Region], next : ForcedAction) extends ForcedAction
case class TTHierophantsChooseGateAction(self : Faction, r : Region, next : ForcedAction) extends BaseFactionAction(
    "Place " + HighPriest.styled(TT) + " at", r
)
// HP expansion: prompt each other faction to place HP at gate
case class TTHierophantsOtherFactionsAction(self : Faction, remaining : $[Faction], next : ForcedAction) extends ForcedAction
case class TTHierophantsOtherFactionPlaceAction(f : Faction, self : Faction, gates : $[Region], next : ForcedAction) extends ForcedAction
case class TTHierophantsOtherFactionGateAction(f : Faction, self : Faction, r : Region, next : ForcedAction) extends BaseFactionAction(
    "Place " + HighPriest.styled(f) + " at", r
)

// AWAKEN UBBO-SATHLA — step 1: choose which High Priest to eliminate
case class TTAwakenUbboSathlaChooseHPAction(self : Faction, r : Region, cost : Int) extends BaseFactionAction(
    "Awaken " + UbboSathla.styled(TT) + " in",
    implicit g => r + self.iced(r)
)
case class TTAwakenUbboSathlaEliminateHPAction(self : Faction, u : UnitRef, r : Region, cost : Int) extends BaseFactionAction(
    "Eliminate " + HighPriest.styled(TT) + " to awaken " + UbboSathla.styled(TT),
    implicit g => g.unit(u).full + " in " + g.unit(u).region
)
// AWAKEN UBBO-SATHLA — step 2: place Ubbo-Sathla at chosen gate
case class TTAwakenUbboSathlaAction(self : Faction, r : Region, cost : Int) extends BaseFactionAction(
    "Awaken " + UbboSathla.styled(TT) + " in",
    implicit g => r + self.iced(r)
)

// DOOM-PHASE AWAKEN: cost-0 awaken
case class TTAwakenUbboDoomMainAction(self : Faction) extends OptionFactionAction(
    "Awaken " + UbboSathla.styled(TT) + " (free this Doom phase)"
) with DoomQuestion with Soft


// ============================================================================
// Tcho-Tcho (TT) EXPANSION OBJECT: manages all TT-specific game state and dispatch
// ============================================================================
object TTExpansion extends Expansion {

    // Track whether Dark Rituals (Leng) has been flipped this turn
    var darkRitualsFlipped : Boolean = false

    // Track whether Ubbo-Sathla has ever been awakened (Hell's Banquet fires once true, even if Ubbo is off map)
    var ttUbboEverAwakened : Boolean = false

    // Active 6-book library populated at tribe selection; empty = show all 12 (pre-selection fallback)
    var ttActiveLibrary : $[Spellbook] = $()

    // Track whether Fulmination has permanently removed Ubbo-Sathla from the game
    var ttUbboFulminated : Boolean = false

    // Set when Ubbo is killed in battle; cleared and resolved at PostBattlePhase for correct kill count
    var ttFulminationPending : Boolean = false

    override def triggers()(implicit game : Game) {
        if (game.setup.has(TT)) {
            // TTSycophancyTrigger: another faction does a ritual OR reaches 15 doom
            TT.satisfyIf(TTSycophancyTrigger, "Another faction ritualed or reached 15 Doom",
                game.ritualHistory.but(TT).any ||
                game.setup.but(TT).exists(f => f.doom >= 15))

            // TTThreeElderSigns: own 3 or more elder signs (cumulative check)
            TT.satisfyIf(TTThreeElderSigns, "Own 3+ Elder Signs",
                TT.es.num >= 3)

            // TTEarnElderSign: satisfied when TT earns any elder sign (checked after takeES)
            TT.satisfyIf(TTEarnElderSign, "Earned an Elder Sign",
                TT.es.any)
        }
    }

    override def eliminate(u : UnitFigure)(implicit game : Game) {
        if (!game.setup.has(TT)) return

        // TTGOOKilledInBattle: any GOO killed in battle
        if (u.uclass.isGOO && game.battle.any && game.battle.get.eliminated.contains(u))
            TT.satisfy(TTGOOKilledInBattle, "GOO killed in battle")

        // Fulmination fires in Battle.scala's AssignKillAction when Ubbo is killed — no action needed here
    }

    def perform(action : Action, soft : VoidGuard)(implicit game : Game) : Continue = action @@ {

        // TRIBE SELECTION — intercept SetupFactionsAction to prompt TT before any faction places
        case SetupFactionsAction if game.setup.has(TT) && !game.ttTribeChosen =>
            game.ttTribeChosen = true
            Ask(TT,
                $(TTChooseTribeAction(TT, TribeTsang),
                  TTChooseTribeAction(TT, TribeLeng),
                  TTChooseTribeAction(TT, TribeSarkomand))
            )

        // TRIBE SELECTION — populates the 6-book active library for the chosen tribe
        case TTChooseTribeAction(self, tribe) =>
            game.ttTribe = tribe
            val tribalBooks : $[Spellbook] = tribe match {
                case TribeLeng      => $(DarkRituals, Fulmination, SurpriseSB)
                case TribeSarkomand => $(Doomsday, Inerrant, OtherworldAlliances)
                case TribeTsang     => $(Idolatry, Martyrdom, TabletsOfTheGods)
            }
            ttActiveLibrary = $(Hierophants, Soulless, TerrorSB) ++ tribalBooks
            self.log("secretly chose their Tribe")
            SetupFactionsAction

        // DOOM PHASE DISPATCH
        case DoomAction(f) if f == TT =>

            // Hell's Banquet: mandatory d6 roll once Ubbo-Sathla has ever been awakened (fires before asking)
            // Guard prevents infinite loop when re-entering DoomAction after the roll
            // Suppressed by Elder Thing Mind Control only when Ubbo is on the map and an enemy Elder Thing
            // shares its region. If Ubbo is off-map (killed/fulminated) there is no figure to suppress.
            val ubboOnMap = f.onMap(UbboSathla)
            val hellsBanquetSuppressed = ubboOnMap.any && ElderThingMindControl.suppresses(ubboOnMap.head)(game)
            if (ttUbboEverAwakened && !game.ttHellsBanquetDone) {
                if (hellsBanquetSuppressed) {
                    f.log("Hell's Banquet: blocked by", "Elder Thing".styled("nt"))
                    game.ttHellsBanquetDone = true
                } else
                    return Force(TTHellsBanquetRollAction(f))
            }

            implicit val asking = Asking(f)
            game.rituals(f)
            game.reveals(f)
            game.highPriests(f)
            game.hires(f)

            // Doom-phase awaken: if TT has gate + HP in play and Ubbo not yet awakened, offer 0-cost awaken
            if (f.pool(UbboSathla).any && f.gates.any && f.all(HighPriest).onMap.any)
                + TTAwakenUbboDoomMainAction(f)

            game.doomDone(f)
            asking

        case TTHellsBanquetRollAction(f) =>
            RollD6("Hell's Banquet: roll for Ubbo-Sathla growth", roll => TTHellsBanquetApplyAction(f, roll))

        case TTHellsBanquetApplyAction(f, roll) =>
            game.ubboGrowth += roll
            f.log("Hell's Banquet: rolled", roll.toString.styled("kill"), "— added", roll.toString.styled(TT), "to Growth counter (now", game.ubboGrowth.toString.styled(TT) + ")")
            // Mark Hell's Banquet done so DoomAction doesn't loop
            game.ttHellsBanquetDone = true
            // Re-enter DoomAction to handle Tablets + rituals + DoomDone
            Force(DoomAction(f))

        // MAIN ACTION DISPATCH
        case MainAction(f) if f == TT && f.active.not =>
            UnknownContinue

        case MainAction(f) if f == TT && (f.acted || f.battled.any) =>
            implicit val asking = Asking(f)

            game.controls(f)

            if (f.hasAllSB)
                game.battles(f)

            game.reveals(f)

            game.endTurn(f)(true)

            asking

        case MainAction(f) if f == TT =>
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

            // REMOVE GATE (SBR #4): remove controlled gate in start area
            val startArea = game.starting.get(f)
            // On library maps (Primeval/Shaggai), any controlled gate qualifies; otherwise start area only
            val removeGateEligible = if (game.board.isLibraryMap) f.gates.any else startArea.exists(r => f.gates.has(r))
            if (f.needs(TTRemoveControlledGate) && removeGateEligible)
                + TTRemoveGateMainAction(f)

            // TRIBE-SPECIFIC ACTIONS

            // LENG: Dark Rituals (enemies with TT HPs in start areas pay 2P or 2D)
            val darkRitualsEligible = game.factions.but(f).%(e =>
                game.starting.get(e).exists(r => f.at(r, HighPriest).any) &&
                (e.power >= 2 || e.doom >= 2)
            )
            if (f.can(DarkRituals) && !darkRitualsFlipped && darkRitualsEligible.any)
                + TTDarkRitualsMainAction(f)

            // LENG: Surprise! (enemy eliminates Acolyte, replaced by Proto-Shoggoth)
            val surpriseEnemiesWithAcolytes = game.factions.but(f).%(e => e.allInPlay.%(_.uclass == Acolyte).any)
            if (f.can(SurpriseSB) && f.power >= 2 && surpriseEnemiesWithAcolytes.any && f.pool(ProtoShoggoth).any)
                + TTSurpriseMainAction(f)

            // TSANG: Idolatry (cost 1: select faction-glyph area, move any TT units from adjacent areas)
            // Valid targets: any region that has a faction glyph — either printed (core factions not in game)
            // or placed dynamically (any in-game faction's chosen start region, including OW/TS/FB/AN/DS).
            val coreFactions = $(GC, CC, BG, YS, SL, WW)
            val factionGlyphAreas = {
                val inGame = game.factions./~(fx => game.starting.get(fx).toList)
                val offBoard = coreFactions.%!(fx => game.setup.has(fx))
                    ./~(fx => game.board.starting(fx)).distinct
                (inGame ++ offBoard).distinct
            }
            val idolatryTargets = areas.%(r =>
                factionGlyphAreas.has(r) &&
                game.board.connected(r).%(adj => f.at(adj).any).any
            )
            if (f.can(Idolatry) && f.power >= 1 && idolatryTargets.any)
                + TTIdolatryMainAction(f)

            // SARKOMAND: Doomsday (once-only: place cost-2 or cost-4 iGOO at gate)
            val doomsdayCards = game.loyaltyCards.%(c => c.isInstanceOf[IGOOLoyaltyCard] &&
                (c.asInstanceOf[IGOOLoyaltyCard].power == 2 || c.asInstanceOf[IGOOLoyaltyCard].power == 4) &&
                !game.factions.exists(_.loyaltyCards.has(c))
            )
            if (f.can(Doomsday) && !f.oncePerGame.has(Doomsday) && f.gates.any && doomsdayCards.any)
                + TTDoomsdayMainAction(f)

            game.neutralSpellbooks(f)
            game.libraryActions(f)
            if (game.options.has(HighPriests) && f.all(HighPriest).onMap.any)
                + TTUnspeakableOathMainAction(f)
            game.reveals(f)
            game.endTurn(f)(f.battled.any)

            asking

        // DOOM-PHASE AWAKEN (cost 0)
        case TTAwakenUbboDoomMainAction(self) =>
            Ask(self).each(self.gates)(r => TTAwakenUbboSathlaChooseHPAction(self, r, 0)).cancel

        // ACTION-PHASE AWAKEN (cost 6)
        case AwakenMainAction(self, UbboSathla, locations) if self == TT =>
            Ask(self).each(locations)(r => TTAwakenUbboSathlaChooseHPAction(self, r, 6)).cancel

        case TTAwakenUbboSathlaChooseHPAction(self, r, cost) =>
            val hps = self.all(HighPriest).onMap
            if (hps.num == 1)
                Force(TTAwakenUbboSathlaEliminateHPAction(self, hps.head.ref, r, cost))
            else
                Ask(self).each(hps)(u => TTAwakenUbboSathlaEliminateHPAction(self, u.ref, r, cost))

        case TTAwakenUbboSathlaEliminateHPAction(self, uref, r, cost) =>
            val u = game.unit(uref)
            game.eliminate(u)
            self.log("eliminated", HighPriest.styled(TT), "to awaken", UbboSathla.styled(TT))
            Force(TTAwakenUbboSathlaAction(self, r, cost))

        case TTAwakenUbboSathlaAction(self, r, cost) =>
            self.power -= cost
            self.payTax(r)
            self.place(UbboSathla, r)
            self.log("awakened", UbboSathla.styled(TT), "in", r, (cost > 0).??("for " + cost.power))
            self.satisfy(TTAwakenUbboSathla, "Awaken Ubbo-Sathla")
            ttUbboEverAwakened = true
            // Hell's Banquet timing: when Ubbo is awakened DURING a doom phase (cost 0), Hell's Banquet
            // must NOT fire that same doom phase — it first fires next doom phase. Mark the sentinel done
            // so the re-entered DoomAction below skips the Hell's Banquet roll. The flag is cleared at
            // DoomDoneAction, so Hell's Banquet will fire normally on subsequent doom phases.
            if (cost == 0) game.ttHellsBanquetDone = true
            if (cost == 0) Force(DoomAction(self)) else EndAction(self)

        // UNSPEAKABLE OATH
        case TTUnspeakableOathMainAction(self) =>
            Ask(self).each(self.all(HighPriest).onMap)(u => TTUnspeakableOathAction(self, u.ref)).cancel

        case TTUnspeakableOathAction(self, uref) =>
            Force(SacrificeHighPriestAction(self, game.unit(uref).region, PreMainAction(self)))

        // REMOVE GATE (SBR #4)
        case TTRemoveGateMainAction(self) =>
            val eligible = if (game.board.isLibraryMap) self.gates else game.starting.get(self).toList.%(r => self.gates.has(r))
            Ask(self).each(eligible)(r => TTRemoveGateAction(self, r)).cancel

        case TTRemoveGateAction(self, r) =>
            self.gates :-= r
            game.gates :-= r
            self.at(r).%(_.onGate).foreach { u => u.onGate = false }
            self.log("removed controlled Gate in", r)
            self.satisfy(TTRemoveControlledGate, "Removed controlled Gate in Start Area")
            EndAction(self)

        // DARK RITUALS (Leng) — prompt each qualifying enemy in turn order
        case TTDarkRitualsMainAction(self) =>
            darkRitualsFlipped = true
            self.oncePerRound :+= DarkRituals
            self.log(DarkRituals.styled(TT), ": activated (flipped face-down until Doom Phase)")
            val qualifiers = game.factions.but(self).%(e =>
                game.starting.get(e).exists(r => self.at(r, HighPriest).any) &&
                (e.power >= 2 || e.doom >= 2)
            )
            Force(TTDarkRitualsPayAction(self, qualifiers.head))

        case TTDarkRitualsPayAction(self, target) =>
            val qualifiers = game.factions.but(self).%(e =>
                game.starting.get(e).exists(r => self.at(r, HighPriest).any) &&
                (e.power >= 2 || e.doom >= 2)
            )
            val remaining = qualifiers.dropWhile(_ != target)
            if (remaining.none)
                Force(TTDarkRitualsDoneAction(self))
            else {
                val payer = remaining.head
                implicit val asking = Asking(payer)
                if (payer.power >= 2)
                    + TTDarkRitualsPayPowerAction(self, payer)
                if (payer.doom >= 2)
                    + TTDarkRitualsPay2DoomAction(self, payer)
                asking
            }

        case TTDarkRitualsPayPowerAction(self, payer) =>
            payer.power -= 2
            self.power += 2
            payer.log(DarkRituals.styled(TT), ": paid", 2.power, "to", self)
            val qualifiers = game.factions.but(self).%(e =>
                game.starting.get(e).exists(r => self.at(r, HighPriest).any) &&
                (e.power >= 2 || e.doom >= 2)
            )
            val next = qualifiers.dropWhile(_ != payer).drop(1)
            if (next.none) Force(TTDarkRitualsDoneAction(self)) else Force(TTDarkRitualsPayAction(self, next.head))

        case TTDarkRitualsPay2DoomAction(self, payer) =>
            payer.doom -= 2
            self.doom += 2
            payer.log(DarkRituals.styled(TT), ": paid", 2.doom, "to", self)
            val qualifiers = game.factions.but(self).%(e =>
                game.starting.get(e).exists(r => self.at(r, HighPriest).any) &&
                (e.power >= 2 || e.doom >= 2)
            )
            val next = qualifiers.dropWhile(_ != payer).drop(1)
            if (next.none) Force(TTDarkRitualsDoneAction(self)) else Force(TTDarkRitualsPayAction(self, next.head))

        case TTDarkRitualsDoneAction(self) =>
            EndAction(self)

        // SURPRISE! (Leng) — cost 2, enemy eliminates Acolyte, replaced by Proto-Shoggoth
        case TTSurpriseMainAction(self) =>
            self.power -= 2
            val enemies = game.factions.but(self).%(e => e.allInPlay.%(_.uclass == Acolyte).any)
            Force(TTSurpriseChooseFactionAction(self, enemies))

        case TTSurpriseChooseFactionAction(self, enemies) =>
            Ask(self).each(enemies)(e => TTSurpriseTargetFactionAction(self, e)).cancel

        case TTSurpriseTargetFactionAction(self, target) =>
            Force(TTSurpriseEliminateAcolyteAction(self, target))

        case TTSurpriseEliminateAcolyteAction(self, target) =>
            val acolytes = target.allInPlay.%(_.uclass == Acolyte)
            if (acolytes.num == 1)
                Force(TTSurpriseAcolyteChoiceAction(self, target, acolytes.head.ref))
            else
                Ask(target).each(acolytes)(u => TTSurpriseAcolyteChoiceAction(self, target, u.ref))

        case TTSurpriseAcolyteChoiceAction(self, target, uref) =>
            val u = game.unit(uref)
            val r = u.region
            game.eliminate(u)
            if (self.pool(ProtoShoggoth).any)
                self.place(ProtoShoggoth, r)
            self.log(SurpriseSB.styled(TT), ": eliminated", Acolyte.styled(target), "in", r, ", placed", ProtoShoggoth.styled(TT))
            EndAction(self)

        // IDOLATRY (Tsang) — cost 1: select faction-glyph area, move any or all TT units from adjacent areas
        case TTIdolatryMainAction(self) =>
            self.power -= 1
            val coreFactions = $(GC, CC, BG, YS, SL, WW)
            val factionGlyphAreas = {
                val inGame = game.factions./~(fx => game.starting.get(fx).toList)
                val offBoard = coreFactions.%!(fx => game.setup.has(fx))
                    ./~(fx => game.board.starting(fx)).distinct
                (inGame ++ offBoard).distinct
            }
            val targets = areas.%(r =>
                factionGlyphAreas.has(r) &&
                game.board.connected(r).%(adj => self.at(adj).any).any
            )
            Force(TTIdolatryChooseDestAction(self, targets, $()))

        case TTIdolatryChooseDestAction(self, targets, pool) =>
            Ask(self)
                .group(Idolatry.styled(TT) + " — Choose area to move to")
                .each(targets)(r => TTIdolatryChooseSourceAction(self, r, pool).as(r))
                .add(TTIdolatryCancelAction(self).as("Cancel"))

        case TTIdolatryCancelAction(self) =>
            self.power += 1
            PreMainAction(self)

        case TTIdolatryChooseSourceAction(self, dest, pool) =>
            val sources = game.board.connected(dest).%(r => self.at(r).any)
            implicit val asking = Asking(self)
            asking.ask = asking.ask.group(Idolatry.styled(TT) + " — Choose units from area (moving to " + dest + ")")
            if (pool.any)
                + TTIdolatryDoneAction(self, dest, pool).as("Done — move", pool.num, "unit".s(pool.num))
            sources.foreach { src =>
                + TTIdolatryChooseUnitAction(self, dest, src, pool).as(src)
            }
            + TTIdolatryCancelAction(self).as("Cancel all — refund", 1.power)
            asking

        case TTIdolatryChooseUnitAction(self, dest, src, pool) =>
            val alreadyPooled = pool./(game.unit).%(_.region == src)./(_.ref)
            val available = self.at(src)./(_.ref).diff(alreadyPooled)
            implicit val asking = Asking(self)
            asking.ask = asking.ask.group(Idolatry.styled(TT) + " — Choose units to add to move")
            available.foreach { uref =>
                + TTIdolatryAddUnitAction(self, dest, src, uref, pool)
            }
            val fromSrc = pool./(game.unit).%(_.region == src)./(_.ref)
            if (fromSrc.any)
                + TTIdolatryUndoLastAction(self, dest, src, pool)
            if (fromSrc.any)
                + TTIdolatryCancelSourceAction(self, dest, src, pool)
            + TTIdolatryChooseSourceAction(self, dest, pool).as("Choose another region to move units from")
            asking

        case TTIdolatryAddUnitAction(self, dest, src, uref, pool) =>
            Force(TTIdolatryChooseUnitAction(self, dest, src, pool :+ uref))

        case TTIdolatryUndoLastAction(self, dest, src, pool) =>
            val fromSrc = pool./(game.unit).%(_.region == src)./(_.ref)
            val newPool = pool.diff($(fromSrc.last))
            Force(TTIdolatryChooseUnitAction(self, dest, src, newPool))

        case TTIdolatryCancelSourceAction(self, dest, src, pool) =>
            val fromSrc = pool./(game.unit).%(_.region == src)./(_.ref)
            val newPool = pool.diff(fromSrc)
            Force(TTIdolatryChooseSourceAction(self, dest, newPool))

        case TTIdolatryDoneAction(self, dest, pool) =>
            pool.foreach { uref =>
                val u = game.unit(uref)
                val from = u.region
                u.region = dest
                u.add(Moved)
                self.log(Idolatry.styled(TT), ": moved", u.uclass.styled(TT), "from", from, "to", dest)
            }
            EndAction(self)

        // DOOMSDAY (Sarkomand) — once-only: place cost-2 or cost-4 iGOO at gate, take loyalty card
        case TTDoomsdayMainAction(self) =>
            val cards = game.loyaltyCards.of[IGOOLoyaltyCard].%(c =>
                (c.power == 2 || c.power == 4) &&
                !game.factions.exists(_.loyaltyCards.has(c))
            )
            Force(TTDoomsdayChooseIGOOAction(self, cards))

        case TTDoomsdayChooseIGOOAction(self, cards) =>
            Ask(self).each(cards)(c => TTDoomsdaySelectIGOOAction(self, c)).cancel

        case TTDoomsdaySelectIGOOAction(self, card) =>
            Force(TTDoomsdayChooseGateAction(self, card, self.gates))

        case TTDoomsdayChooseGateAction(self, card, gates) =>
            if (gates.num == 1)
                Force(TTDoomsdayPlaceAction(self, card, gates.head))
            else
                Ask(self).each(gates)(r => TTDoomsdayPlaceAction(self, card, r)).cancel

        case TTDoomsdayPlaceAction(self, card, r) =>
            self.oncePerGame :+= Doomsday
            self.log(Doomsday.styled(TT), ": placing", card.unit.styled(self), "at", r, "for free (took Loyalty Card)")
            // Route through the standard iGOO placement engine at cost 0
            Force(IndependentGOOAction(self, card, r, 0))

        // SYCOPHANCY: prompt the ritualING faction — they choose, then ritual completes via TTSycophancyRitualAction
        case TTSycophancyPromptAction(ritualer, doom, es) =>
            Ask(ritualer)
                .add(TTSycophancyLoseDoomAction(ritualer, doom, es))
                .add(TTSycophancyGiveDoomAction(ritualer, doom, es))

        case TTSycophancyLoseDoomAction(ritualer, doom, es) =>
            ritualer.log("Sycophancy".styled(TT) + ": chooses to gain " + (doom - 1).doom + " (1 fewer)")
            // Check if this is BB's Requires Attention ritual (tracked in game state)
            game.bbRequiresAttentionPendingRegion match {
                case Some(r) if ritualer == BB =>
                    game.bbRequiresAttentionPendingRegion = None
                    Force(BBRequiresAttentionResumeAction(ritualer, doom - 1, es, r))
                case _ =>
                    game.bbRequiresAttentionPendingRegion = None
                    Force(TTSycophancyResumeRitualAction(ritualer, doom - 1, es))
            }

        case TTSycophancyGiveDoomAction(ritualer, doom, es) =>
            TT.doom += 1
            ritualer.log("Sycophancy".styled(TT) + ": gave 1", 1.doom, "to", TT)
            // Check if this is BB's Requires Attention ritual (tracked in game state)
            game.bbRequiresAttentionPendingRegion match {
                case Some(r) if ritualer == BB =>
                    game.bbRequiresAttentionPendingRegion = None
                    Force(BBRequiresAttentionResumeAction(ritualer, doom, es, r))
                case _ =>
                    game.bbRequiresAttentionPendingRegion = None
                    Force(TTSycophancyResumeRitualAction(ritualer, doom, es))
            }

        // HIEROPHANTS: on any TT spellbook earn, place HP at gate or grow counter
        case TTHierophantsPlaceHPAction(self, gates, next) =>
            if (self.pool(HighPriest).any) {
                if (gates.num == 1)
                    Force(TTHierophantsChooseGateAction(self, gates.head, next))
                else if (gates.any)
                    Ask(self).each(gates)(r => TTHierophantsChooseGateAction(self, r, next))
                else {
                    // No gates: place where TT has units, or anywhere
                    val withUnits = areas.%(r => self.at(r).any)
                    val dest = if (withUnits.any) withUnits else areas
                    if (dest.num == 1)
                        Force(TTHierophantsChooseGateAction(self, dest.head, next))
                    else
                        Ask(self).each(dest)(r => TTHierophantsChooseGateAction(self, r, next))
                }
            } else {
                game.ubboGrowth += 1
                self.log(Hierophants.styled(TT), ": no HP in pool — Growth counter +1 (now", game.ubboGrowth.toString.styled(TT) + ")")
                Force(next)
            }

        case TTHierophantsChooseGateAction(self, r, next) =>
            self.place(HighPriest, r)
            self.log(Hierophants.styled(TT), ": placed", HighPriest.styled(TT), "at", r)
            if (self.commands.of[UnspeakableOathPlan].none || self.plans.of[UnspeakableOathPlan].none)
                game.initHighPriestPlans(self)
            Force(next)

        case TTHierophantsOtherFactionsAction(self, remaining, next) =>
            if (remaining.none)
                Force(TTHierophantsPlaceHPAction(self, self.gates, next))
            else {
                val f = remaining.head
                val gates = f.gates
                // [2026-06-05] FIX 63 (BB) — apologies for the partial Fix 62; this completes the no-gate
                // recipient path per user directive. Forgive me for the prior oversight: when a recipient
                // (e.g. SL/DS in murclrrerkowjoio) has zero gates, the legal-placement set must fall back
                // to regions where they have any unit; if exactly 1 such region, auto-place; if 2+,
                // prompt with the same multi-gate UI; if 0 (no units anywhere — vanishingly unlikely
                // for a canRecruitHP recipient), skip them and continue the queue. I am very sorry
                // this slipped past Fix 62; please pardon the oversight.
                if (gates.any)
                    Force(TTHierophantsOtherFactionPlaceAction(f, self, gates, next))
                else {
                    val withUnits = areas.%(r => f.at(r).any)
                    if (withUnits.any)
                        Force(TTHierophantsOtherFactionPlaceAction(f, self, withUnits, next))
                    else
                        Force(TTHierophantsOtherFactionsAction(self, remaining.tail, next))
                }
            }

        case TTHierophantsOtherFactionPlaceAction(f, self, gates, next) =>
            if (gates.num == 1)
                Force(TTHierophantsOtherFactionGateAction(f, self, gates.head, next))
            else
                Ask(f).each(gates)(r => TTHierophantsOtherFactionGateAction(f, self, r, next))

        case TTHierophantsOtherFactionGateAction(f, self, r, next) =>
            if (f.pool(HighPriest).any) {
                f.place(HighPriest, r)
                f.log(Hierophants.styled(TT), ": placed", HighPriest.styled(f), "at", r)
            }
            val remaining = game.factions.but(TT).%(_.canRecruitHP).%(_.pool(HighPriest).any)
            Force(TTHierophantsOtherFactionsAction(self, remaining, next))

        // DOOM PHASE END: reset Dark Rituals face-up and Hell's Banquet sentinel
        case DoomDoneAction(f) if f == TT =>
            darkRitualsFlipped = false
            f.oncePerRound :-= DarkRituals
            game.ttHellsBanquetDone = false
            UnknownContinue

        case _ => UnknownContinue
    }
}

// Required action class for Hell's Banquet die result dispatch
case class TTHellsBanquetApplyAction(self : Faction, roll : Int) extends ForcedAction with PowerNeutral
