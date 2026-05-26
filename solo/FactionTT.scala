package cws

import hrf.colmat._

import html._


// ============================================================================
// Tcho-Tcho (TT) UNITS: Acolyte (cultist), High Priest (cultist),
// Proto-Shoggoth (monster, Terror — modifies battle), Ubbo-Sathla (GOO,
// combat = Growth counter value set by Hell's Banquet)
// ============================================================================
case object ProtoShoggoth extends FactionUnitClass(TT, "Proto-Shoggoth", Terror, 3)
case object UbboSathla extends FactionUnitClass(TT, "Ubbo-Sathla", GOO, 8)


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
case object TTSycophancyTrigger    extends Requirement("Another Faction Rituals or Reaches 15 Doom")
case object TTEarnElderSign        extends Requirement("Earn an Elder Sign")
case object TTThreeElderSigns      extends Requirement("Own 3+ Elder Signs")
case object TTRemoveControlledGate extends Requirement("Remove Controlled Gate in Start Area")
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
    override def library = $(Hierophants, Soulless, TerrorSB, Idolatry, Martyrdom, TabletsOfTheGods,
                              DarkRituals, Fulmination, SurpriseSB, Doomsday, Inerrant, OtherworldAlliances)
    override def requirements(options : $[GameOption]) = $(TTSycophancyTrigger, TTEarnElderSign, TTThreeElderSigns, TTRemoveControlledGate, TTGOOKilledInBattle, TTAwakenUbboSathla)

    val allUnits =
        1.times(UbboSathla) ++
        3.times(ProtoShoggoth) ++
        6.times(Acolyte)

    override def awakenCost(u : UnitClass, r : Region)(implicit game : Game) : |[Int] = u match {
        case UbboSathla => f.gates.has(r).?(8)
        case _ => None
    }

    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int = {
        val protoShoggothCount = units(ProtoShoggoth).not(Zeroed).num
        val ubboCount          = units(UbboSathla).not(Zeroed).num
        // Proto-Shoggoth: Terror type, adds combat as a terror unit
        // Ubbo-Sathla: combat = current Growth counter value
        protoShoggothCount * 3 +
        ubboCount * game.ubboGrowth +
        neutralStrength(units, opponent)
    }
}


// ============================================================================
// Tcho-Tcho (TT) ACTION CLASSES
// ============================================================================

// TRIBE SELECTION (first Doom Phase)
case class TTChooseTribeAction(self : Faction, tribe : TTTribe) extends BaseFactionAction(
    "Choose Tribe (secret until tribal spellbook revealed)",
    tribe match {
        case TribeLeng      => "Tribe of Leng".styled(TT)
        case TribeSarkomand => "Tribe of Sarkomand".styled(TT)
        case TribeTsang     => "Tribe of Tsang".styled(TT)
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
    "Remove Controlled Gate in Start Area (Spellbook)"
) with MainQuestion with Soft
case class TTRemoveGateAction(self : Faction, r : Region) extends BaseFactionAction(
    "Remove Controlled Gate from", r
)

// DARK RITUALS (Leng exclusive: flip face-down, use toward ritual, resets each doom phase)
case class TTDarkRitualsMainAction(self : Faction) extends OptionFactionAction(
    DarkRituals.styled(TT) + ": gain " + 2.power + " (flips face-down until next Doom Phase)"
) with MainQuestion with Soft

// FULMINATION (Leng exclusive: gain doom from casualties — modeled as post-battle trigger)
// Handled via triggers() in TTExpansion

// SURPRISE! (Leng exclusive: move unit before battle)
case class TTSurpriseMainAction(self : Faction, l : $[Region]) extends OptionFactionAction(SurpriseSB) with MainQuestion with Soft
case class TTSurpriseAction(self : Faction, u : UnitRef, r : Region) extends BaseFactionAction(
    implicit g => "Move " + g.unit(u).full + " to",
    r
)

// IDOLATRY (Tsang exclusive: place acolyte at gate — unlimited action)
case class TTIdolatryMainAction(self : Faction) extends OptionFactionAction(Idolatry.styled(TT) + " (place Acolyte at Gate for free)") with MainQuestion with Soft
case class TTIdolatryAction(self : Faction, r : Region) extends BaseFactionAction("Place Acolyte at", r)

// MARTYRDOM (Tsang exclusive: sacrifice cultist to heal Ubbo-Sathla)
case class TTMartyrdomMainAction(self : Faction) extends OptionFactionAction(Martyrdom.styled(TT) + ": sacrifice Cultist to restore " + UbboSathla.styled(TT)) with MainQuestion with Soft
case class TTMartyrdomAction(self : Faction, u : UnitRef) extends BaseFactionAction("Sacrifice", implicit g => g.unit(u).full + " for " + UbboSathla.styled(TT))

// TABLETS OF THE GODS (Tsang exclusive: doom phase — gain 1 doom per 2 gates)
// Handled in doom phase dispatch

// DOOMSDAY (Sarkomand exclusive: if leading in doom, gain power)
case class TTDoomsdayMainAction(self : Faction) extends OptionFactionAction(Doomsday.styled(TT) + ": gain " + 1.power + " per faction you lead in Doom") with MainQuestion with Soft

// INERRANT (Sarkomand exclusive: battle spellbook — reroll all misses once)
// Handled as BattleSpellbook trigger

// OTHERWORLD ALLIANCES (Sarkomand exclusive: recruit neutral monster at reduced cost)
case class TTOtherworldAlliancesMainAction(self : Faction) extends OptionFactionAction(OtherworldAlliances.styled(TT)) with MainQuestion with Soft

// AWAKEN UBBO-SATHLA
case class TTAwakenUbboSathlaAction(self : Faction, r : Region) extends BaseFactionAction(
    "Awaken " + UbboSathla.styled(TT) + " in",
    implicit g => r + self.iced(r)
)


// ============================================================================
// Tcho-Tcho (TT) EXPANSION OBJECT: manages all TT-specific game state and dispatch
// ============================================================================
object TTExpansion extends Expansion {

    // Track whether Dark Rituals (Leng) has been flipped this turn
    var darkRitualsFlipped : Boolean = false

    override def triggers()(implicit game : Game) {
        if (game.factions.has(TT)) {
            // TTSycophancyTrigger: another faction does a ritual OR reaches 15 doom
            TT.satisfyIf(TTSycophancyTrigger, "Another faction ritualed or reached 15 Doom",
                game.ritualHistory.has(TT).not && game.ritualHistory.any ||
                game.factions.but(TT).exists(f => f.doom >= 15))

            // TTThreeElderSigns: own 3 or more elder signs (cumulative check)
            TT.satisfyIf(TTThreeElderSigns, "Own 3+ Elder Signs",
                TT.es.num >= 3)

            // TTEarnElderSign: satisfied when TT earns any elder sign (checked after takeES)
            TT.satisfyIf(TTEarnElderSign, "Earned an Elder Sign",
                TT.es.any)
        }
    }

    override def eliminate(u : UnitFigure)(implicit game : Game) {
        if (!game.factions.has(TT)) return

        // TTGOOKilledInBattle: any GOO killed in battle
        if (u.uclass.utype == GOO && game.battle.any && game.battle.get.eliminated.contains(u))
            TT.satisfy(TTGOOKilledInBattle, "GOO killed in battle")

        // FULMINATION (Leng): gain 1 doom for each enemy unit killed in battle
        if (TT.can(Fulmination) && u.faction != TT &&
            game.battle.any && game.battle.get.eliminated.contains(u)) {
            TT.doom += 1
            TT.log(Fulmination.styled(TT), ": gained", 1.doom, "for eliminating", u.uclass.styled(u.faction))
        }
    }

    def perform(action : Action, soft : VoidGuard)(implicit game : Game) : Continue = action @@ {

        // TRIBE SELECTION — bot picks at DoomAction entry (first doom phase); human picks at start
        case TTChooseTribeAction(self, tribe) =>
            game.ttTribe = tribe
            val tribeName = tribe match {
                case TribeLeng      => "Leng"
                case TribeSarkomand => "Sarkomand"
                case TribeTsang     => "Tsang"
            }
            self.log("chose Tribe", tribeName.styled(TT))
            CheckSpellbooksAction(DoomAction(self))

        // DOOM PHASE DISPATCH
        case DoomAction(f) if f == TT =>
            // TRIBE SELECTION: on first DoomAction (ttTribeChosen = false), ask TT to pick a tribe
            if (!game.ttTribeChosen) {
                game.ttTribeChosen = true
                return Ask(f,
                    $(TTChooseTribeAction(f, TribeLeng),
                      TTChooseTribeAction(f, TribeSarkomand),
                      TTChooseTribeAction(f, TribeTsang))
                )
            }

            // Hell's Banquet: mandatory d6 roll when Ubbo-Sathla is in play (fires before asking)
            // Guard prevents infinite loop when re-entering DoomAction after the roll
            if (f.onMap(UbboSathla).any && !game.ttHellsBanquetDone)
                return Force(TTHellsBanquetRollAction(f))

            // Tablets of the Gods (Tsang): gain 1 doom per 2 controlled gates during doom phase
            if (f.can(TabletsOfTheGods) && f.gates.any) {
                val tabletsDoom = f.gates.num / 2
                f.doom += tabletsDoom
                f.log(TabletsOfTheGods.styled(TT), ": gained", tabletsDoom.doom, "from", f.gates.num, "Gates")
            }

            implicit val asking = Asking(f)
            game.rituals(f)
            game.reveals(f)
            game.highPriests(f)
            game.hires(f)
            + DoomDoneAction(f)
            asking

        case TTHellsBanquetRollAction(f) =>
            RollD6("Hell's Banquet: roll for Ubbo-Sathla growth", roll => TTHellsBanquetApplyAction(f, roll))

        case TTHellsBanquetApplyAction(f, roll) =>
            if (roll >= 4) {
                game.ubboGrowth += 1
                f.log("Hell's Banquet: rolled", roll.toString.styled("kill"), "— Ubbo-Sathla Growth counter now", game.ubboGrowth.toString.styled(TT))
            } else {
                f.log("Hell's Banquet: rolled", roll.toString, "— no growth (need 4+)")
            }
            // Mark Hell's Banquet done so DoomAction doesn't loop
            game.ttHellsBanquetDone = true
            // Re-enter DoomAction to handle Tablets + rituals + DoomDone
            Force(DoomAction(f))

        // MAIN ACTION DISPATCH
        case MainAction(f) if f == TT && f.active.not =>
            UnknownContinue

        case MainAction(f) if f == TT && f.acted =>
            UnknownContinue

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

            // UNSPEAKABLE OATH: sacrifice High Priest for 2 power (available any time HP is on map)
            if (f.all(HighPriest).onMap.any)
                + TTUnspeakableOathMainAction(f)

            // REMOVE GATE (SBR #4): remove controlled gate in start area
            val startArea = game.starting.get(f)
            if (f.needs(TTRemoveControlledGate) && startArea.exists(r => f.gates.has(r)))
                + TTRemoveGateMainAction(f)

            // TRIBE-SPECIFIC ACTIONS

            // LENG: Dark Rituals (flip face-down for 2 power)
            if (f.can(DarkRituals) && !darkRitualsFlipped)
                + TTDarkRitualsMainAction(f)

            // LENG: Surprise! (move a unit before battle)
            val surpriseTargets = areas.%(r => f.at(r).any)
            if (f.can(SurpriseSB) && surpriseTargets.any)
                + TTSurpriseMainAction(f, surpriseTargets)

            // TSANG: Idolatry (place acolyte at gate for free)
            if (f.can(Idolatry) && f.pool(Acolyte).any && f.gates.any)
                + TTIdolatryMainAction(f)

            // TSANG: Martyrdom (sacrifice cultist to interact with Ubbo-Sathla)
            if (f.can(Martyrdom) && f.onMap(UbboSathla).any && f.cultists.onMap.any)
                + TTMartyrdomMainAction(f)

            // SARKOMAND: Doomsday (gain power if leading)
            if (f.can(Doomsday) && game.factions.but(f).exists(e => f.doom > e.doom))
                + TTDoomsdayMainAction(f)

            game.neutralSpellbooks(f)
            game.libraryActions(f)
            game.highPriests(f)
            game.reveals(f)
            game.endTurn(f)(f.battled.any)

            asking

        // AWAKEN UBBO-SATHLA
        case AwakenMainAction(self, UbboSathla, locations) if self == TT =>
            Ask(self).each(locations)(r => TTAwakenUbboSathlaAction(self, r)).cancel

        case TTAwakenUbboSathlaAction(self, r) =>
            self.power -= 8
            self.payTax(r)
            self.place(UbboSathla, r)
            self.log("awakened", UbboSathla.styled(TT), "in", r, "for", 8.power)
            self.satisfy(TTAwakenUbboSathla, "Awaken Ubbo-Sathla")
            EndAction(self)

        // UNSPEAKABLE OATH
        case TTUnspeakableOathMainAction(self) =>
            Ask(self).each(self.all(HighPriest).onMap)(u => TTUnspeakableOathAction(self, u.ref)).cancel

        case TTUnspeakableOathAction(self, uref) =>
            game.eliminate(game.unit(uref))
            self.power += 2
            self.log("Unspeakable Oath: sacrificed", HighPriest.styled(TT), "for", 2.power)
            EndAction(self)

        // REMOVE GATE (SBR #4)
        case TTRemoveGateMainAction(self) =>
            val eligible = game.starting.get(self).toList.%(r => self.gates.has(r))
            Ask(self).each(eligible)(r => TTRemoveGateAction(self, r)).cancel

        case TTRemoveGateAction(self, r) =>
            self.gates :-= r
            game.gates :-= r
            self.at(r).%(_.onGate).foreach { u => u.onGate = false }
            self.log("removed controlled Gate in", r)
            self.satisfy(TTRemoveControlledGate, "Removed controlled Gate in Start Area")
            EndAction(self)

        // DARK RITUALS (Leng)
        case TTDarkRitualsMainAction(self) =>
            darkRitualsFlipped = true
            self.power += 2
            self.log(DarkRituals.styled(TT), ": gained", 2.power, "(flipped face-down until Doom Phase)")
            EndAction(self)

        // SURPRISE! (Leng) — move a unit to an adjacent area (free, before battle)
        case TTSurpriseMainAction(self, _) =>
            // Generate all (unit, destination) pairs for the player to choose from
            val pairs = areas./~{ r =>
                self.at(r).%(_.canMove)./~{ u =>
                    game.board.connected(r).%(d => d.glyph.onMap)./(d => TTSurpriseAction(self, u.ref, d))
                }
            }
            Ask(self).list(pairs).cancel

        case TTSurpriseAction(self, uref, r) =>
            val u = game.unit(uref)
            val from = u.region
            u.region = r
            u.add(Moved)
            u.add(MovedForFree)
            self.log(SurpriseSB.styled(TT), ": moved", u.uclass.styled(TT), "from", from, "to", r)
            EndAction(self)

        // IDOLATRY (Tsang) — place acolyte at gate
        case TTIdolatryMainAction(self) =>
            Ask(self).each(self.gates)(r => TTIdolatryAction(self, r)).cancel

        case TTIdolatryAction(self, r) =>
            self.place(Acolyte, r)
            self.log(Idolatry.styled(TT), ": placed free", Acolyte.styled(TT), "in", r)
            EndAction(self)

        // MARTYRDOM (Tsang) — sacrifice cultist to do something with Ubbo-Sathla
        case TTMartyrdomMainAction(self) =>
            Ask(self).each(self.cultists.onMap)(u => TTMartyrdomAction(self, u.ref)).cancel

        case TTMartyrdomAction(self, uref) =>
            val u = game.unit(uref)
            game.eliminate(u)
            // Martyrdom: sacrifice cultist — Ubbo-Sathla grows
            game.ubboGrowth += 1
            self.log(Martyrdom.styled(TT), ": sacrificed", u.uclass.styled(TT), "—", UbboSathla.styled(TT), "Growth now", game.ubboGrowth.toString.styled(TT))
            EndAction(self)

        // DOOMSDAY (Sarkomand) — gain power for each faction you lead in doom
        case TTDoomsdayMainAction(self) =>
            val leading = game.factions.but(self).count(e => self.doom > e.doom)
            self.power += leading
            self.log(Doomsday.styled(TT), ": gained", leading.power, "for leading", leading, "factions in Doom")
            EndAction(self)

        // DOOM PHASE END: reset Dark Rituals face-up and Hell's Banquet sentinel
        case DoomDoneAction(f) if f == TT =>
            darkRitualsFlipped = false
            game.ttHellsBanquetDone = false
            UnknownContinue

        case _ => UnknownContinue
    }
}

// Required action class for Hell's Banquet die result dispatch
case class TTHellsBanquetApplyAction(self : Faction, roll : Int) extends ForcedAction with PowerNeutral
