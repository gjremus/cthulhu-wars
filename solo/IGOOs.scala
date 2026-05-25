package cws

import hrf.colmat._

import html._

// IGOOs
case object ByatisCard extends IGOOLoyaltyCard(ByatisIcon, Byatis, power = 4, combat = 4)
case object AbhothCard extends IGOOLoyaltyCard(AbhothIcon, Abhoth, power = 4)
case object DaolothCard extends IGOOLoyaltyCard(DaolothIcon, Daoloth, power = 6)
case object NyogthaCard extends IGOOLoyaltyCard(NyogthaIcon, Nyogtha, power = 6, quantity = 2, combat = 4)

case object ByatisIcon extends UnitClass(Byatis.name + " Icon", Token, 0)
case object AbhothIcon extends UnitClass(Abhoth.name + " Icon", Token, 0)
case object DaolothIcon extends UnitClass(Daoloth.name + " Icon", Token, 0)
case object NyogthaIcon extends UnitClass(Nyogtha.name + " Icon", Token, 0)

// ── TULZSCHA ──
// Cost 4 to awaken, combat 1. Tulzscha's ongoing ability "Undying Flame" fires once per
// turn at Gather Power: if any enemy has more Doom, ES (face-down only), or Power than
// the Tulzscha owner, the owner gains +1 of whichever they're behind on (each checked
// independently). Spellbook requirement: give each enemy 2 Power. Spellbook "Ceremony of
// Annihilation": during the doom phase, the owner may take the current ritual cost as Power
// instead of performing a normal ritual (no Doom or ES gained, but ritual track advances).
case object TulzschaCard extends IGOOLoyaltyCard(TulzschaIcon, Tulzscha, power = 4, combat = 1)
case object TulzschaIcon extends UnitClass(Tulzscha.name + " Icon", Token, 0)

// ── Y'GOLONAC ──
// Cost 2 to awaken, combat 1. Y'Golonac's battle ability "Orifices" triggers when Y'Golonac
// is killed in battle: before elimination, the owner may choose an enemy Monster, Terror, or
// Cultist in the same battle. That unit is eliminated and Y'Golonac is placed under the
// target's faction's control with the loyalty card and spellbook transferred. Spellbook
// "The Revelations" (ongoing): during the doom phase, every enemy faction gains 1 Elder Sign.
// The spellbook requirement is satisfied automatically when a faction receives Y'Golonac
// (either by awakening or via Orifices transfer).
case object YgolonacCard extends IGOOLoyaltyCard(YgolonacIcon, Ygolonac, power = 2, combat = 1)
case object YgolonacIcon extends UnitClass(Ygolonac.name + " Icon", Token, 0)

case object Byatis extends UnitClass("Byatis", GOO, 4) with IGOO {
    override def canMove(u : UnitFigure)(implicit game : Game) = ElderThingMindControl.suppresses(u)
    override def canBeMoved(u : UnitFigure)(implicit game : Game) = ElderThingMindControl.suppresses(u)
}

case object Abhoth extends UnitClass("Abhoth", GOO, 4) with IGOO
case object Daoloth extends UnitClass("Daoloth", GOO, 6) with IGOO
case object Nyogtha extends UnitClass("Nyogtha", GOO, 6) with IGOO
case object Tulzscha extends UnitClass("Tulzscha", GOO, 4) with IGOO
case object Ygolonac extends UnitClass("Y'Golonac", GOO, 2) with IGOO

// ── NEW IGOOs ──
case object AzathothIGOOCard extends IGOOLoyaltyCard(AzathothIGOOIcon, AzathothIGOO, power = 8)
case object AzathothIGOOIcon extends UnitClass("Azathoth Icon", Token, 0)
case object AzathothIGOO extends UnitClass("Azathoth", GOO, 0) with IGOO  // combat = glyph position

case object CthughaCard extends IGOOLoyaltyCard(CthughaIcon, Cthugha, power = 6, combat = 0)
case object CthughaIcon extends UnitClass("Cthugha Icon", Token, 0)
case object Cthugha extends UnitClass("Cthugha", GOO, 6) with IGOO  // combat = enemy GOO combat

case object MotherHydraCard extends IGOOLoyaltyCard(MotherHydraIcon, MotherHydra, power = 4, combat = 0)
case object MotherHydraIcon extends UnitClass("Mother Hydra Icon", Token, 0)
case object MotherHydra extends UnitClass("Mother Hydra", GOO, 4) with IGOO  // combat = max(1, 6-enemies)

case object YigCard extends IGOOLoyaltyCard(YigIcon, Yig, power = 4, combat = 2)
case object YigIcon extends UnitClass("Yig Icon", Token, 0)
case object Yig extends UnitClass("Yig", GOO, 4) with IGOO

case object FatherDagonCard extends IGOOLoyaltyCard(FatherDagonIcon, FatherDagon, power = 4, combat = 2)
case object FatherDagonIcon extends UnitClass("Father Dagon Icon", Token, 0)
case object FatherDagon extends UnitClass("Father Dagon", GOO, 4) with IGOO  // combat = 2 land / 6 ocean

case object GhatanotoaIGOOCard extends IGOOLoyaltyCard(GhatanotoaIGOOIcon, GhatanotoaIGOO, power = 4, combat = 0)
case object GhatanotoaIGOOIcon extends UnitClass("Ghatanothoa (IGOO) Icon", Token, 0)
case object GhatanotoaIGOO extends UnitClass("Ghatanothoa", GOO, 4) with IGOO  // combat = enemy cultists on map

case object BloatedWomanCard extends IGOOLoyaltyCard(BloatedWomanIcon, BloatedWoman, power = 4, combat = 1)
case object BloatedWomanIcon extends UnitClass("The Bloated Woman Icon", Token, 0)
case object BloatedWoman extends UnitClass("The Bloated Woman", GOO, 4) with IGOO

case object AtlachNachaCard extends IGOOLoyaltyCard(AtlachNachaIcon, AtlachNacha, power = 4, combat = 0)
case object AtlachNachaIcon extends UnitClass("Atlach-Nacha Icon", Token, 0)
case object AtlachNacha extends UnitClass("Atlach-Nacha", GOO, 4) with IGOO

case object BokrugCard extends IGOOLoyaltyCard(BokrugIcon, Bokrug, power = 6, combat = 0)
case object BokrugIcon extends UnitClass("Bokrug Icon", Token, 0)
case object Bokrug extends UnitClass("Bokrug", GOO, 6) with IGOO

case object GlaakiIGOOCard extends IGOOLoyaltyCard(GlaakiIGOOIcon, GlaakiIGOO, power = 6, combat = 0)
case object GlaakiIGOOIcon extends UnitClass("Gla'aki (IGOO) Icon", Token, 0)
case object GlaakiIGOO extends UnitClass("Gla'aki", GOO, 6) with IGOO  // combat = 0 (Tomb Herd provides power, not combat)

case object Filth extends UnitClass("Filth", Monster, 1) {
    override def canMove(u : UnitFigure)(implicit game : Game) = false
    override def canBattle(u : UnitFigure)(implicit game : Game) = false
    override def canCapture(u : UnitFigure)(implicit game : Game) = false
    override def canBeSummoned(f : Faction)(implicit game : Game) = f.has(Fertility)
}

// Byatis
// SPELLBOOK — use .can(), CAN be blocked by Moonbeast
case object ToadOfBerkeley extends NeutralSpellbook("Toad of Berkeley")
// SPELLBOOK — use .can(), CAN be blocked by Moonbeast
case object GodOfForgetfulness extends NeutralSpellbook("God of Forgetfulness")

// Abhoth
// SPELLBOOK — use .can(), CAN be blocked by Moonbeast
case object LostAbhoth extends NeutralSpellbook("Lost Abhoth")
// SPELLBOOK — use .can(), CAN be blocked by Moonbeast
case object TheBrood extends NeutralSpellbook("The Brood")

// Daoloth
// SPELLBOOK — use .can(), CAN be blocked by Moonbeast
case object CosmicUnity extends NeutralSpellbook("Cosmic Unity") with BattleSpellbook
// SPELLBOOK — use .can(), CAN be blocked by Moonbeast
case object Interdimensional extends NeutralSpellbook("Interdimensional")

// Nyogtha
// SPELLBOOK — use .can(), CAN be blocked by Moonbeast
case object FromBelow extends NeutralSpellbook("From Below")
// SPELLBOOK — use .can(), CAN be blocked by Moonbeast
case object NyogthaPrimed extends NeutralSpellbook("Nyogtha Primed")
// SPELLBOOK — use .can(), CAN be blocked by Moonbeast
case object NyogthaMourning extends NeutralSpellbook("Nyogtha Mourning")

// SPELLBOOK — use .can(), CAN be blocked by Moonbeast
case object NightmareWeb extends NeutralSpellbook("Nightmare Web")

// Tulzscha spellbook: during the doom phase, the owner may take the current ritual cost
// as Power instead of performing a normal ritual (no Doom or ES, but ritual track advances)
// SPELLBOOK — use .can(), CAN be blocked by Moonbeast
case object CeremonyOfAnnihilation extends NeutralSpellbook("Ceremony of Annihilation")

// Y'Golonac spellbook: during the doom phase, every enemy faction gains 1 Elder Sign.
// Auto-satisfied when a faction receives Y'Golonac (via awakening or Orifices transfer).
// SPELLBOOK — use .can(), CAN be blocked by Moonbeast
case object TheRevelations extends NeutralSpellbook("The Revelations")

// ── NEW IGOO SPELLBOOKS ──
// Yig: Messenger of Yig — Doom Phase, each enemy donates 1 Power or Yig owner gains 1 Doom
// Req: Remove a Controlled Gate from the map (Cultist does not die)
// SPELLBOOK — use .can(), CAN be blocked by Moonbeast
case object MessengerOfYig extends NeutralSpellbook("Messenger of Yig")

// Mother Hydra: The Zygote — Action: Cost 1, place all Cultists from Pool onto board
// Req: Have no GOOs in Ocean, or enemy has no GOOs in Ocean
// SPELLBOOK — use .can(), CAN be blocked by Moonbeast
case object TheZygote extends NeutralSpellbook("The Zygote")

// Father Dagon: The Innsmouth Look — Doom Phase, remove Acolyte permanently, gain 6 Power
// Req: Have 8 Units in Ocean Areas
// SPELLBOOK — use .can(), CAN be blocked by Moonbeast
case object TheInnsmouthLook extends NeutralSpellbook("The Innsmouth Look")

// Cthugha: Firestorm — Post-Battle, sparing also gives 1 ES
// Req: Kill an enemy GOO in Battle
// SPELLBOOK — use .can(), CAN be blocked by Moonbeast
case object Firestorm extends NeutralSpellbook("Firestorm") with BattleSpellbook

// Ghatanothoa IGOO: Execration of Mu — Ongoing, Mummify is no longer an Action (instant)
// Req: Control 2 or fewer Gates, or pay 3 Power
// SPELLBOOK — use .can(), CAN be blocked by Moonbeast
case object ExecrationOfMu extends NeutralSpellbook("Execration of Mu")

// Bloated Woman: Disaster Looms — Gather Power, Gates earn 1 ES instead of 2 Power
// Req: Have all 6 Faction Spellbooks
// SPELLBOOK — use .can(), CAN be blocked by Moonbeast
case object DisasterLooms extends NeutralSpellbook("Disaster Looms")

// Atlach-Nacha: Cosmic Web — immediate victory when 6 web tokens placed
// Req: Place 6 web tokens in 6 different areas
// SPELLBOOK — use .can(), CAN be blocked by Moonbeast
case object CosmicWeb extends NeutralSpellbook("Cosmic Web")

// Bokrug: Doom that Came to Sarnath — Doom Phase, owner chooses enemy to eliminate unit or discard ES
// Req: Give Bokrug to another player (SBR)
// SPELLBOOK — use .can(), CAN be blocked by Moonbeast
case object DoomThatCameToSarnath extends NeutralSpellbook("Doom that Came to Sarnath")

// Gla'aki IGOO: Green Decay — captured enemy cultists give ES instead of power during gather power
// Req: Reach 0 power during action phase before any other faction (SBR)
// SPELLBOOK — use .can(), CAN be blocked by Moonbeast
case object GlaakiGreenDecay extends NeutralSpellbook("Green Decay")

// Azathoth: Nuclear Chaos — Action: Cost 0, all players roll dice, highest gets Power, lowest gets ES
// Req: Every Faction must have a GOO in play
// SPELLBOOK — use .can(), CAN be blocked by Moonbeast
case object NuclearChaos extends NeutralSpellbook("Nuclear Chaos")


trait NeutralFaction extends Faction

case object NeutralAbhoth extends NeutralFaction {
    def name = "Neutral Abhoth"
    def short = "NA"
    def style = "nt"

    override def abilities = $
    override def library = $
    override def requirements(options : $[GameOption]) = $

    val allUnits = 12.times(Filth)

    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int = 0
}


case class AwakenIGOOMainAction(self : Faction) extends OptionFactionAction("Awaken " + "Independent GOO".styled("nt")) with MainQuestion with Soft

case class IndependentGOOMainAction(self : Faction, lc : IGOOLoyaltyCard, l : $[Region]) extends OptionFactionAction(g => {
    val qm = Overlays.imageSource("question-mark")
    val p = s""""${lc.name.replace('\\'.toString, '\\'.toString + '\\'.toString)}", false""".replace('"'.toString, "&quot;")
    "<div class=sbdiv>" +
        "Awaken " + lc.name.styled("nt") +
    s"""<img class=explain src="${qm}" onclick="event.stopPropagation(); onExternalClick(${p})" onpointerover="onExternalOver(${p})" onpointerout="onExternalOut(${p})" />""" +
    "</div>"
}) with MainQuestion with Soft
case class IndependentGOOAction(self : Faction, lc : LoyaltyCard, r : Region, cost : Int) extends BaseFactionAction(g => "Awaken " + lc.unit.styled(self) + g.forNPowerWithTax(r, self, cost) + " in", implicit g => r + self.iced(r))

case class GodOfForgetfulnessMainAction(self : Faction, d : Region, l : $[Region]) extends OptionFactionAction("God of Forgetfulness".styled("nt")) with MainQuestion with Soft
case class GodOfForgetfulnessAction(self : Faction, d : Region, r : Region) extends BaseFactionAction(g => "Move all enemy Cultists to " + Byatis.styled(self) + " " + g.forNPowerWithTax(d, self, 1) + " from", r)

case class FilthMainAction(self : Faction, l : $[Region]) extends OptionFactionAction("Place " + Filth.styled(self)) with MainQuestion with Soft
case class FilthAction(self : Faction, r : Region) extends BaseFactionAction(g => "Place " + Filth.styled(self) + " " + g.forNPowerWithTax(r, self, 1) + " in", r)

case class NightmareWebMainAction(self : Faction, l : $[Region]) extends OptionFactionAction("Awaken " + Nyogtha.styled(self) + " with " + NightmareWeb.styled(self)) with MainQuestion with Soft
case class NightmareWebAction(self : Faction, r : Region) extends BaseFactionAction(g => "Awaken " + Nyogtha.styled(self) + g.forNPowerWithTax(r, self, 2) + " in", implicit g => r + self.iced(r))

// Y'Golonac Orifices: when Y'Golonac is killed in battle, this action lets the owner
// choose an enemy unit to replace. The target is eliminated, Y'Golonac transfers to the
// target's faction, and the loyalty card + spellbook transfer with it.
case class YgolonacOrificesAction(self : Faction, target : UnitRef) extends ForcedAction

// Tulzscha spellbook requirement: the owner voluntarily gives each enemy faction 2 Power.
// Once done, the owner gains the Ceremony of Annihilation spellbook.
case class TulzschaGivePowerMainAction(self : Faction) extends OptionFactionAction("Tulzscha SBR".styled("nt") + " — give each enemy " + "2 Power".styled("power")) with MainQuestion with Soft
case class TulzschaGivePowerAction(self : Faction) extends BaseFactionAction(g => "Give each enemy " + "2 Power".styled("power") + " for " + Tulzscha.styled(self) + " Spellbook Requirement", g => "Give " + "2 Power".styled("power"))

// Ceremony of Annihilation: doom-phase option. Instead of a normal ritual, the owner
// earns Power equal to the current ritual cost. No Doom or Elder Signs are gained, but
// the ritual track still advances. Offered in the DoomAction handler alongside normal rituals.
case class CeremonyOfAnnihilationChoiceAction(self : Faction) extends OptionFactionAction(
    g => "Use " + CeremonyOfAnnihilation.styled(self) + " (earn " + g.ritualCost.power + ", no Doom/ES)"
) with DoomQuestion


// Yig: spellbook requirement — remove a Controlled Gate from map
case class YigRemoveGateMainAction(self : Faction) extends OptionFactionAction(implicit g => "Remove Controlled Gate (Yig SBR)".styled("nt")) with MainQuestion with Soft
case class YigRemoveGateAction(self : Faction, r : Region) extends BaseFactionAction(implicit g => "Remove Gate in", r) {
    override def question(implicit game : Game) = self.full + " — " + "Yig SBR".styled("nt") + " — remove Controlled Gate"
}

// Ghatanothoa IGOO: spellbook requirement — control 2 or fewer Gates, or pay 3 Power
case class GhatanotoaSBRPayAction(self : Faction) extends OptionFactionAction(implicit g => "Pay " + "3 Power".styled("power") + " (Ghatanothoa IGOO SBR)".styled("nt")) with MainQuestion

// Azathoth IGOO: Nuclear Chaos spellbook action
case class NuclearChaosMainAction(self : Faction) extends OptionFactionAction(implicit g => "Nuclear Chaos".styled("nt") + " (Cost 0)") with MainQuestion
case class NuclearChaosRollAction(self : Faction, rolls : Map[Faction, Int]) extends ForcedAction
case class NuclearChaosAdjustAction(self : Faction, rolls : Map[Faction, Int], adjust : Int) extends BaseFactionAction(implicit g => "Nuclear Chaos".styled("nt"), implicit g => {
    val myRoll = rolls.getOrElse(self, 0) + adjust
    s"Adjust to $myRoll (" + (if (adjust > 0) "+1" else "-1") + ")"
}) {
    override def question(implicit game : Game) = self.full + " — " + "Nuclear Chaos".styled("nt") + " — adjust your roll?"
}
case class NuclearChaosKeepAction(self : Faction, rolls : Map[Faction, Int]) extends BaseFactionAction(implicit g => "Nuclear Chaos".styled("nt"), implicit g => "Keep roll as-is") {
    override def question(implicit game : Game) = self.full + " — " + "Nuclear Chaos".styled("nt") + " — adjust your roll?"
}

// Azathoth IGOO: custom awakening
// Cthugha: custom awakening with per-GOO replacement
// Cthugha: main menu entry with ? overlay (same pattern as IndependentGOOMainAction)
case class CthughaAwakenMainAction(self : Faction) extends OptionFactionAction(g => {
    val qm = Overlays.imageSource("question-mark")
    val p = s""""Cthugha", false""".replace('"'.toString, "&quot;")
    "<div class=sbdiv>" +
        "Awaken " + "Cthugha".styled("nt") +
    s"""<img class=explain src="${qm}" onclick="event.stopPropagation(); onExternalClick(${p})" onpointerover="onExternalOver(${p})" onpointerout="onExternalOut(${p})" />""" +
    "</div>"
}) with MainQuestion with Soft
// Cthugha: sub-menu GOO selection
case class CthughaAwakenAction(self : Faction, r : Region, replacedGOO : UnitClass, cost : Int) extends BaseFactionAction(
    implicit g => "Replace with " + "Cthugha".styled(self),
    implicit g => replacedGOO.styled(self) + " in " + r + " — " + (if (cost >= 0) cost + " " + "Power".styled("power") else "Gain " + (-cost) + " " + "Power".styled("power"))
)

case class AzathothAwakenMainAction(self : Faction) extends OptionFactionAction(g => {
    val qm = Overlays.imageSource("question-mark")
    val p = s""""Azathoth", false""".replace('"'.toString, "&quot;")
    "<div class=sbdiv>" +
        "Awaken " + "Azathoth".styled("nt") + " (8+ " + "Power".styled("power") + ")" +
    s"""<img class=explain src="${qm}" onclick="event.stopPropagation(); onExternalClick(${p})" onpointerover="onExternalOver(${p})" onpointerout="onExternalOut(${p})" />""" +
    "</div>"
}) with MainQuestion with Soft
case class AzathothAwakenCommitAction(self : Faction, powerCost : Int, gateRegion : Region) extends ForcedAction
case class AzathothEnemyChoiceAction(self : Faction, face : Int, remaining : $[Faction], choices : Map[Faction, Int]) extends ForcedAction
case class AzathothEnemyChooseAction(self : Faction, face : Int, chooser : Faction) extends BaseFactionAction(implicit g => "Azathoth".styled("nt") + " Awakening", implicit g => {
    val qm = Overlays.imageSource("question-mark")
    val p = s""""Azathoth", false""".replace('"'.toString, "&quot;")
    "<div class=sbdiv>" +
        "Die face: " + face +
    s"""<img class=explain src="${qm}" onclick="event.stopPropagation(); onExternalClick(${p})" onpointerover="onExternalOver(${p})" onpointerout="onExternalOut(${p})" />""" +
    "</div>"
}) {
    override def question(implicit game : Game) = chooser.full + " — " + "Azathoth".styled("nt") + " Awakening — choose die face"
}
case class AzathothResolveAction(self : Faction, choices : Map[Faction, Int], gateRegion : Region) extends ForcedAction

// Ghatanothoa IGOO: Mummify action
case class GhatanotoaMummifyAction(self : Faction) extends OptionFactionAction(implicit g => "Mummify".styled("nt") + " " + "(" + "1 Power".styled("power") + ")") with MainQuestion

// Atlach-Nacha: Place Spinneret — place web token in Atlach-Nacha's region
case class PlaceSpinneretMainAction(self : Faction) extends OptionFactionAction(implicit g => "Place Spinneret".styled("nt") + " " + "(" + "1 Power".styled("power") + ")") with MainQuestion

// Mother Hydra: The Zygote spellbook action
// The Innsmouth Look: forced Acolyte removal at Doom Phase (player chooses which)
case class InnsmouthLookRemoveAction(self : Faction, then : Action) extends ForcedAction
case class InnsmouthLookDoomDoneAction(self : Faction) extends ForcedAction
case class InnsmouthLookChooseAction(self : Faction, u : UnitFigure, then : Action) extends BaseFactionAction(implicit g => "The Innsmouth Look".styled("nt"), implicit g => "Remove " + Acolyte.styled(self) + (if (u.onGate) " (on gate)" else "") + " from " + u.region) {
    override def question(implicit game : Game) = self.full + " — " + "The Innsmouth Look".styled("nt") + " — choose Acolyte to remove permanently"
}

case class TheZygoteMainAction(self : Faction) extends OptionFactionAction(implicit g => "The Zygote".styled("nt") + " — place Acolytes from Pool one by one " + "(" + "1 Power".styled("power") + ")") with MainQuestion with Soft
case class TheZygoteTargetAction(self : Faction, r : Region, remaining : Int) extends BaseFactionAction(implicit g => "The Zygote".styled("nt") + " — place " + Acolyte.styled(self) + " in", r) {
    override def question(implicit game : Game) = self.full + " — " + "The Zygote".styled("nt") + " — choose Area for " + Acolyte.styled(self) + " (" + remaining + " remaining)"
}
case class TheZygoteContinueAction(self : Faction) extends ForcedAction

// ── BOKRUG: Actions ──
// GiveBokrug: ever-present out-of-turn action (like Elder Signs / moonbeast pay 1 doom)
case class GiveBokrugMainAction(self : Faction) extends OptionFactionAction("Give " + "Bokrug".styled("nt") + " to another player") with MainQuestion with Soft
case class GiveBokrugAction(self : Faction, target : Faction) extends BaseFactionAction(
    implicit g => "Give " + Bokrug.styled(self) + " to", target.full)

// Ghosts of Ib: during doom phase, if Bokrug NOT on map, place to any area without enemy units
case class GhostsOfIbPlaceAction(self : Faction, then : Action) extends ForcedAction
case class GhostsOfIbChooseAction(self : Faction, r : Region, then : Action) extends BaseFactionAction(
    implicit g => "Ghosts of Ib".styled("nt") + " — place " + Bokrug.styled(self) + " in", r) {
    override def question(implicit game : Game) = self.full + " — " + "Ghosts of Ib".styled("nt") + " — choose Area"
}

// Doom that Came to Sarnath: fires after Ghosts of Ib
case class DoomSarnathMainAction(self : Faction, then : Action) extends ForcedAction
case class DoomSarnathChooseOption(self : Faction, option : Int, then : Action) extends BaseFactionAction(
    implicit g => "Doom that Came to Sarnath".styled("nt"),
    implicit g => if (option == 1) "An enemy chooses a monster or cultist of yours to eliminate" else "An enemy chooses one of your elder signs to discard") {
    override def question(implicit game : Game) = self.full + " — " + "Doom that Came to Sarnath".styled("nt")
}
case class DoomSarnathChooseFactionAction(self : Faction, option : Int, target : Faction, then : Action) extends BaseFactionAction(
    implicit g => "Doom that Came to Sarnath".styled("nt"), target.full)
case class DoomSarnathEliminateUnit(self : Faction, owner : Faction, u : UnitFigure, then : Action) extends BaseFactionAction(
    implicit g => "Choose a " + owner.full + " unit to eliminate", implicit g => u.uclass.styled(owner) + " in " + u.region) {
    override def question(implicit game : Game) = self.full + " — " + "Choose a ".styled("nt") + owner.full + " cultist or monster to eliminate"
}
case class DoomSarnathDiscardES(self : Faction, owner : Faction, index : Int, then : Action) extends BaseFactionAction(
    implicit g => "Choose an elder sign of " + owner.full + " to discard", implicit g => "Elder Sign #" + (index + 1)) {
    override def question(implicit game : Game) = self.full + " — " + "Choose an elder sign of ".styled("nt") + owner.full + " to discard"
}

// Tsunami/Agony Sting: per-owner cultist movement choice
case class TsunamiMoveCultistAction(self : Faction, u : UnitFigure, dest : Region, remaining : $[UnitFigure], then : Action) extends BaseFactionAction(implicit g => "Tsunami".styled("nt") + " — move " + u.uclass.styled(self), dest) {
    override def question(implicit game : Game) = self.full + " — " + "Tsunami".styled("nt") + " — choose destination for " + u.uclass.styled(self)
}
case class TsunamiProcessAction(self : Faction, sourceRegion : Region, remaining : $[(Faction, $[UnitFigure])], oceanDest : Boolean, then : Action) extends ForcedAction

// Yig: Messenger of Yig — doom phase donation choice
case class MessengerOfYigDonateAction(self : Faction, yigOwner : Faction) extends BaseFactionAction(
    implicit g => "Messenger of Yig".styled("nt"), implicit g => "Donate " + "1 Power".styled("power") + " to " + yigOwner.full) {
    override def question(implicit game : Game) = self.full + " — " + "Messenger of Yig".styled("nt")
}
case class MessengerOfYigRefuseAction(self : Faction, yigOwner : Faction) extends BaseFactionAction(
    implicit g => "Messenger of Yig".styled("nt"), implicit g => "Refuse (" + yigOwner.full + " gains " + "1 Doom".styled("doom") + ")") {
    override def question(implicit game : Game) = self.full + " — " + "Messenger of Yig".styled("nt")
}

// Father Dagon: Tsunami action
case class FatherDagonTsunamiMainAction(self : Faction) extends OptionFactionAction(implicit g => "Tsunami".styled("nt") + " " + "(" + "1 Power".styled("power") + ")") with MainQuestion with Soft
case class FatherDagonTsunamiTargetAction(self : Faction, r : Region) extends BaseFactionAction(implicit g => "Tsunami".styled("nt"), r) {
    override def question(implicit game : Game) = self.full + " — " + "Tsunami".styled("nt") + " — choose Land Area"
}

// Mother Hydra: Agony Sting action
case class MotherHydraAgonyStingMainAction(self : Faction) extends OptionFactionAction(implicit g => "The Agony Sting".styled("nt") + " " + "(" + "1 Power".styled("power") + ")") with MainQuestion with Soft
case class MotherHydraAgonyStingTargetAction(self : Faction, r : Region) extends BaseFactionAction(implicit g => "The Agony Sting".styled("nt"), r) {
    override def question(implicit game : Game) = self.full + " — " + "The Agony Sting".styled("nt") + " — choose Ocean Area"
}

object IGOOsExpansion extends Expansion {
    def checkAbhothSpellbook()(implicit game : Game) {
        factions.foreach { f =>
            if (f.has(Abhoth) && f.upgrades.has(TheBrood).not) {
                val monsters = f.units.monsters.inPlay

                if (monsters./(_.uclass).distinct.num >= 4 || monsters.num >= 8) {
                    f.upgrades :+= TheBrood
                    f.log("gained", TheBrood.styled(f), "for", Abhoth.styled(f))
                }
            }
        }
    }

    def checkInterdimensional()(implicit game : Game) {
        factions.foreach { f =>
            // Round 8 Bug 40: check facedown state — IGOO spellbooks flipped via
            // FB Infernal Pact are disabled (tracked in oncePerGame)
            if (f.has(Interdimensional) && !f.oncePerGame.has(Interdimensional)) {
                val r = f.goo(Daoloth).region

                if (game.lastDaolothRegion.has(r).not) {
                    game.lastDaolothRegion = |(r)

                    if (r.onMap && game.gates.has(r).not) {
                        game.gates :+= r
                        log(Daoloth.styled(f), "placed a Gate in", r, "with", Interdimensional.styled(f))
                    }
                }
            }
        }
    }

    // ── TULZSCHA: UNDYING FLAME (ongoing ability) ──
    // Fires once per turn during Gather Power (called via triggers()). Compares the
    // Tulzscha owner's Doom, Elder Signs, and Power against all enemies. For each
    // category where any enemy is strictly ahead, the owner gains +1 of that resource.
    // Guard: tulzschaFlameTurn tracks the last turn this fired. triggers() is called
    // from multiple places (gate checks, power gather, ritual), but the guard ensures
    // Undying Flame only activates once per turn — at the first triggers() call after
    // the turn counter increments (which happens in PowerGatherAction).
    def checkTulzschaUndyingFlame()(implicit game : Game) {
        if (game.tulzschaFlameTurn >= game.turn) return
        game.tulzschaFlameTurn = game.turn
        factions.foreach { f =>
            if (f.has(Tulzscha) && !f.allInPlay.%(_.uclass == Tulzscha).exists(u => ElderThingMindControl.suppresses(u))) {
                val others = factions.but(f)
                // Doom: if any enemy has strictly more doom, gain +1 doom
                if (others.exists(_.doom > f.doom)) {
                    f.doom += 1
                    f.log(Tulzscha.styled(f), "gained 1 Doom from", "Undying Flame".styled(f))
                }
                // Elder Signs: compare only face-down (unspent) ES via es.num.
                // Bug fix: was (o.es.num + o.revealed.num) which counted revealed ES that
                // had already been converted to Doom, inflating both sides' totals.
                if (others.exists(o => o.es.num > f.es.num)) {
                    f.takeES(1)
                    f.log(Tulzscha.styled(f), "gained 1 Elder Sign from", "Undying Flame".styled(f))
                }
                // Power: if any enemy has strictly more power, gain +1 power
                if (others.exists(_.power > f.power)) {
                    f.power += 1
                    f.log(Tulzscha.styled(f), "gained 1 Power from", "Undying Flame".styled(f))
                }
            }
        }
    }

    override def triggers()(implicit game : Game) {
        checkAbhothSpellbook()
        checkInterdimensional()
        checkTulzschaUndyingFlame()

        // Execration of Mu (Ghatanothoa IGOO spellbook): Mummify is instant/ongoing
        // Any enemy Cultist sharing Area with Ghatanothoa is immediately mummified
        factions.foreach { f =>
            if (f.has(ExecrationOfMu) && !f.oncePerGame.has(ExecrationOfMu) && f.has(GhatanotoaIGOO)) {
                val ghat = f.allInPlay.%(_.uclass == GhatanotoaIGOO)
                if (ghat.any && !ElderThingMindControl.suppresses(ghat.head)) {
                    val r = ghat.head.region
                    f.enemies./~(_.at(r).%(_.uclass.utype == Cultist)).foreach { u =>
                        if (!game.mummifiedCultists.has(u.ref)) {
                            game.mummifiedCultists :+= u.ref
                            log("Execration of Mu".styled("nt") + ":", u.uclass.styled(u.faction), "auto-mummified in", r)
                        }
                    }
                }
            }
        }

        // Ghatanothoa IGOO: un-mummify cultists no longer sharing area with Ghatanothoa
        if (game.mummifiedCultists.any) {
            val ghatRegions = factions./~(f => f.allInPlay.%(_.uclass == GhatanotoaIGOO)./(_.region)).toSet
            val freed = game.mummifiedCultists.%(ref => {
                val u = game.unitOpt(ref)
                u.exists(unit => !ghatRegions.contains(unit.region))
            })
            freed.foreach { ref =>
                val u = game.unit(ref)
                game.mummifiedCultists = game.mummifiedCultists.but(ref)
                log(u.uclass.styled(u.faction), "in", u.region, "no longer mummified (left", "Ghatanothoa".styled("nt") + "'s area)")
            }
        }

        // ── iGOO SPELLBOOK REQUIREMENTS (automatic checks) ──

        // Mother Hydra: "Have no GOOs in Ocean, or AN enemy has no GOOs in Ocean"
        factions.foreach { f =>
            if (f.has(MotherHydra) && f.upgrades.has(TheZygote).not) {
                val ownerGOOsInOcean = f.allInPlay.%(_.uclass.utype == GOO).%(_.region.glyph == Ocean).any
                val anyEnemyNoGOOInOcean = f.enemies.exists(e => e.allInPlay.%(_.uclass.utype == GOO).%(_.region.glyph == Ocean).none)
                if (!ownerGOOsInOcean || anyEnemyNoGOOInOcean) {
                    f.upgrades :+= TheZygote
                    f.log("gained", TheZygote.styled(f), "for", MotherHydra.styled(f))
                }
            }
        }

        // Father Dagon: "Have 8 Units in Ocean Areas"
        factions.foreach { f =>
            if (f.has(FatherDagon) && f.upgrades.has(TheInnsmouthLook).not) {
                val oceanUnits = f.allInPlay.%(_.region.glyph == Ocean).num
                if (oceanUnits >= 8) {
                    f.upgrades :+= TheInnsmouthLook
                    f.log("gained", TheInnsmouthLook.styled(f), "for", FatherDagon.styled(f))
                }
            }
        }

        // Bloated Woman: "Have all 6 Faction Spellbooks"
        factions.foreach { f =>
            if (f.has(BloatedWoman) && f.upgrades.has(DisasterLooms).not) {
                if (f.hasAllSB) {
                    f.upgrades :+= DisasterLooms
                    f.log("gained", DisasterLooms.styled(f), "for", BloatedWoman.styled(f))
                }
            }
        }

        // Azathoth: "Every Faction must have a GOO in play"
        factions.foreach { f =>
            if (f.has(AzathothIGOO) && f.upgrades.has(NuclearChaos).not) {
                if (factions.forall(e => e.allInPlay.goos.any)) {
                    f.upgrades :+= NuclearChaos
                    f.log("gained", NuclearChaos.styled(f), "for", AzathothIGOO.styled(f))
                }
            }
        }

        // Atlach-Nacha: Cosmic Web — 6 web tokens in 6 different areas
        factions.foreach { f =>
            if (f.has(AtlachNacha) && f.upgrades.has(CosmicWeb).not) {
                if (game.webTokens.num >= 6) {
                    f.upgrades :+= CosmicWeb
                    f.log("gained", CosmicWeb.styled(f), "for", AtlachNacha.styled(f))
                }
            }
        }

        // Gla'aki IGOO: SBR — faction must reach 0 power during action phase before any other faction
        factions.foreach { f =>
            if (f.has(GlaakiIGOO) && f.upgrades.has(GlaakiGreenDecay).not) {
                if (game.reachedZeroPowerFirst.has(f)) {
                    f.upgrades :+= GlaakiGreenDecay
                    f.log("gained", GlaakiGreenDecay.styled(f), "for", GlaakiIGOO.styled(f))
                }
            }
        }
    }

    override def eliminate(u : UnitFigure)(implicit game : Game) {
        val f = u.faction

        u.uclass @@ {
            case Byatis =>
                f.units :-= u
                f.upgrades :-= GodOfForgetfulness

                f.loyaltyCards :-= ByatisCard
                game.loyaltyCards :+= ByatisCard

            case Abhoth =>
                f.units :-= u
                f.upgrades :-= TheBrood

                f.loyaltyCards :-= AbhothCard
                game.loyaltyCards :+= AbhothCard

                f.oncePerAction :+= LostAbhoth

            case Daoloth =>
                f.units :-= u
                f.upgrades :-= CosmicUnity
                f.upgrades :-= Interdimensional

                f.loyaltyCards :-= DaolothCard
                game.loyaltyCards :+= DaolothCard

                game.lastDaolothRegion = None

            case Nyogtha if f.all(Nyogtha).but(u).any =>
                f.oncePerAction :+= NyogthaMourning

            case Nyogtha =>
                f.units = f.units.%!(_.uclass == Nyogtha)
                f.upgrades :-= FromBelow
                f.upgrades :-= NightmareWeb
                f.loyaltyCards :-= NyogthaCard
                game.loyaltyCards :+= NyogthaCard


            // Tulzscha eliminated: remove unit, revoke Ceremony of Annihilation spellbook,
            // return loyalty card to the available pool so another faction can awaken it later
            case Tulzscha =>
                f.units :-= u
                f.upgrades :-= CeremonyOfAnnihilation
                f.loyaltyCards :-= TulzschaCard
                game.loyaltyCards :+= TulzschaCard

            // Y'Golonac eliminated: remove unit, revoke The Revelations spellbook,
            // return loyalty card to pool. Guard on loyaltyCards.has because Orifices
            // can transfer the card to another faction — if the card already moved,
            // don't try to return it from the original owner.
            case Ygolonac =>
                f.units :-= u
                f.upgrades :-= TheRevelations
                if (f.loyaltyCards.has(YgolonacCard)) {
                    f.loyaltyCards :-= YgolonacCard
                    game.loyaltyCards :+= YgolonacCard
                }

            // Azathoth IGOO eliminated: remove unit, reset glyph, revoke spellbook, return card
            case AzathothIGOO =>
                f.units :-= u
                game.azathothGlyphPosition = 0
                f.upgrades :-= NuclearChaos
                f.loyaltyCards :-= AzathothIGOOCard
                game.loyaltyCards :+= AzathothIGOOCard

            case Cthugha =>
                f.units :-= u
                f.upgrades :-= Firestorm
                f.loyaltyCards :-= CthughaCard
                game.loyaltyCards :+= CthughaCard

            case MotherHydra =>
                f.units :-= u
                f.upgrades :-= TheZygote
                f.loyaltyCards :-= MotherHydraCard
                game.loyaltyCards :+= MotherHydraCard

            case Yig =>
                f.units :-= u
                f.upgrades :-= MessengerOfYig
                f.loyaltyCards :-= YigCard
                game.loyaltyCards :+= YigCard

            case FatherDagon =>
                f.units :-= u
                f.upgrades :-= TheInnsmouthLook
                f.loyaltyCards :-= FatherDagonCard
                game.loyaltyCards :+= FatherDagonCard

            case GhatanotoaIGOO =>
                f.units :-= u
                f.upgrades :-= ExecrationOfMu
                f.loyaltyCards :-= GhatanotoaIGOOCard
                game.loyaltyCards :+= GhatanotoaIGOOCard

            case BloatedWoman =>
                f.units :-= u
                f.upgrades :-= DisasterLooms
                f.loyaltyCards :-= BloatedWomanCard
                game.loyaltyCards :+= BloatedWomanCard

            case AtlachNacha =>
                f.units :-= u
                f.upgrades :-= CosmicWeb
                game.webTokens = $
                f.loyaltyCards :-= AtlachNachaCard
                game.loyaltyCards :+= AtlachNachaCard

            // Bokrug: on death, stays with owner (NOT returned to global pool)
            // Faction retains the loyalty card and can reawaken for 6 power
            // Move to pool (reserve), not removed from units entirely
            case Bokrug =>
                u.region = f.reserve
                u.onGate = false
                u.health = Alive
                u.state = $

            case GlaakiIGOO =>
                f.units :-= u
                f.upgrades :-= GlaakiGreenDecay
                f.loyaltyCards :-= GlaakiIGOOCard
                game.loyaltyCards :+= GlaakiIGOOCard

            case _ =>
        }

        // §20 fix: if the case above removed u from f.units, the active
        // battle's Side.forces may still hold a reference to the now-orphaned
        // UnitFigure. Clean it up to prevent stale UnitRef crashes in pain/retreat Asks.
        if (!f.units.contains(u)) {
            game.battle.foreach { b =>
                b.attackers.forces = b.attackers.forces.but(u)
                b.defenders.forces = b.defenders.forces.but(u)
            }
        }
    }

    override def afterAction()(implicit game : Game) {
        factions.%(_.oncePerAction.has(LostAbhoth)).foreach { f =>
            NeutralAbhoth.units = f.units(Filth)./(u => new UnitFigure(NeutralAbhoth, u.uclass, u.index, (u.region == f.reserve).?(NeutralAbhoth.reserve).|(u.region), u.onGate, u.state, u.health))

            f.units = f.units.not(Filth)
        }

        factions.%(_.oncePerAction.has(NyogthaPrimed)).%!(_.oncePerAction.has(NyogthaMourning)).foreach { f =>
            if (f.upgrades.has(NightmareWeb).not) {
                f.upgrades :+= NightmareWeb

                f.log("gained", NightmareWeb.styled(f), "for", Nyogtha.styled(f))
            }
        }
    }

    def perform(action : Action, soft : VoidGuard)(implicit game : Game) = action @@ {
        // Cthugha: custom awakening — replace specific GOO, pay per-GOO cost
        case CthughaAwakenMainAction(self) =>
            // Sub-menu: list all GOOs (faction + iGOO) with costs
            val allGOOs = self.allInPlay.%(_.uclass.utype == GOO).%(u => u.uclass != Cthugha)
            var ask = Ask(self)
            allGOOs.foreach { goo =>
                val gooCost = if (goo.uclass.isInstanceOf[FactionUnitClass]) self.awakenCost(goo.uclass, goo.region).|(goo.uclass.cost) else goo.uclass.cost
                val cthughaCost = 6 - gooCost
                if (self.power >= cthughaCost && self.gates.has(goo.region))
                    ask = ask.add(CthughaAwakenAction(self, goo.region, goo.uclass, cthughaCost))
            }
            ask.cancel

        case CthughaAwakenAction(self, r, replacedGOO, cost) =>
            // Guard: the faction GOO must still be in the region
            val factionGOO = self.at(r).%(_.uclass == replacedGOO).starting
            if (factionGOO.none) {
                self.log("cannot awaken", "Cthugha".styled("nt"), "— no", replacedGOO.styled(self), "in", r)
                EndAction(self)
            }
            else {
            self.loyaltyCards :+= CthughaCard
            game.loyaltyCards :-= CthughaCard
            self.power -= cost
            self.units :+= new UnitFigure(self, Cthugha, 1, r)
            self.log("awakened", "Cthugha".styled("nt"), "in", r, (if (cost >= 0) "for" else "gaining"), (if (cost >= 0) cost else -cost).power, "(replacing", replacedGOO.styled(self) + ")")

            // [2026-05-24] If the replaced GOO is an IGOO, fully eliminate it
            // (revoke its LC + SBs, return LC to game pool) per user direction:
            // "the faction that had that iGOO loses the iGOO, loses their
            // powers, and loses access to their spellbooks, even if already earned."
            // For faction GOOs (Cthulhu, Hastur, etc.), return to reserve as before
            // so the faction can re-awaken normally.
            factionGOO.foreach { goo =>
                self.log(goo.uclass.styled(self), "replaced by", Cthugha.styled(self))
                if (goo.uclass.isInstanceOf[IGOO]) {
                    IGOOsExpansion.eliminate(goo)
                    self.log(goo.uclass.styled(self), "loyalty card returned to pool; spellbooks revoked")
                } else {
                    goo.region = self.reserve
                    goo.onGate = false
                    goo.health = Alive
                    goo.state = $
                }
            }

            if (self.has(Immortal)) {
                self.log("gained", 1.es, "as", Immortal)
                self.takeES(1)
            }

            EndAction(self)
            }

        case AwakenIGOOMainAction(self) =>
            // [2026-05-23] Unified iGOO awaken sub-menu. Standard iGOOs go
            // through IndependentGOOMainAction; Azathoth and Cthugha — which
            // used to be top-level entries — now ride here too, sorted by
            // name alongside the others.
            val standardAvailable = game.loyaltyCards.of[IGOOLoyaltyCard]
                .%(igoo => game.igooCost(self, igoo) <= self.power)
                .%(igoo => igoo != AzathothIGOOCard)
                .%(igoo => igoo != CthughaCard)
                .%(igoo => {
                    val cost = game.igooCost(self, igoo)
                    areas.nex.%(self.canAwakenIGOO).%(self.affords(cost)).any
                })

            val cthughaAvailable = game.loyaltyCards.has(CthughaCard) && {
                val allGOOs = self.allInPlay.%(_.uclass.utype == GOO).%(u => u.uclass != Cthugha)
                allGOOs.%(goo => {
                    val gooCost = if (goo.uclass.isInstanceOf[FactionUnitClass]) self.awakenCost(goo.uclass, goo.region).|(goo.uclass.cost) else goo.uclass.cost
                    val cthughaCost = 6 - gooCost
                    self.power >= cthughaCost && self.gates.has(goo.region)
                }).any
            }

            val azathothAvailable = game.loyaltyCards.has(AzathothIGOOCard) && self.power >= 8 &&
                self.allGates.onMap.%(r => self.at(r).goos.any).any

            // Build name-keyed entries (Entry case class) and sort by name.
            // The action is what gets dispatched when the user picks an item.
            case class IGOOEntry(name : String, action : Action)
            val standardEntries : $[IGOOEntry] = standardAvailable./(igoo => {
                val cost = game.igooCost(self, igoo)
                val gates = areas.nex.%(self.canAwakenIGOO).%(self.affords(cost))
                IGOOEntry(igoo.name, IndependentGOOMainAction(self, igoo, gates))
            })
            val cthughaEntry : $[IGOOEntry] =
                cthughaAvailable.?($(IGOOEntry("Cthugha", CthughaAwakenMainAction(self)))).|($)
            val azathothEntry : $[IGOOEntry] =
                azathothAvailable.?($(IGOOEntry("Azathoth", AzathothAwakenMainAction(self)))).|($)
            val sorted = (standardEntries ++ cthughaEntry ++ azathothEntry).sortBy(_.name)

            Ask(self).each(sorted)(_.action).cancel

        case IndependentGOOMainAction(self, lc, l) =>
            val cost = game.igooCost(self, lc)
            Ask(self).each(l)(r => IndependentGOOAction(self, lc, r, cost)).cancel

        case IndependentGOOAction(self, lc, r, cost) =>
            // Bokrug re-awakening: card already in faction's loyaltyCards, skip transfer
            if (!self.loyaltyCards.has(lc)) {
                self.loyaltyCards :+= lc
                game.loyaltyCards :-= lc
            }

            self.power -= cost

            self.log("awakened", lc.unit.name.styled("nt"), "in", r, "for", cost.power)

            // Bokrug re-awakening: move existing pool unit instead of creating new
            if (lc == BokrugCard && self.pool(Bokrug).any) {
                val bokrug = self.pool.one(Bokrug)
                bokrug.region = r
            } else {
                self.units :+= new UnitFigure(self, lc.unit, 1, r)
            }

            lc.unit match {
                case Abhoth =>
                    if (game.neutrals.contains(NeutralAbhoth).not)
                        game.neutrals += NeutralAbhoth -> new Player(NeutralAbhoth)

                    self.units ++= NeutralAbhoth.units./(u => new UnitFigure(self, u.uclass, u.index, (u.region == NeutralAbhoth.reserve).?(self.reserve).|(u.region), u.onGate, u.state, u.health))

                    NeutralAbhoth.units = $

                case Daoloth =>
                    self.upgrades :+= CosmicUnity

                case Tulzscha =>

                case Nyogtha =>
                    self.units :+= new UnitFigure(self, lc.unit, 2, r)

                    self.upgrades :+= FromBelow

                // Cthugha uses CthughaAwakenAction (custom path), not this generic handler

                case _ =>
            }

            if (self.has(Immortal)) {
                self.log("gained", 1.es, "as", Immortal)

                self.takeES(1)
            }

            EndAction(self)

        // BYATIS
        case GodOfForgetfulnessMainAction(self, d, l) =>
            Ask(self).each(l)(r => GodOfForgetfulnessAction(self, d, r)).cancel

        case GodOfForgetfulnessAction(self, d, r) =>
            self.power -= 1
            self.payTax(r)

            self.enemies.foreach { f =>
                f.at(r).cultists.foreach { u =>
                    u.region = d
                    u.onGate = false
                }
            }

            log(Byatis.styled(self), "used", GodOfForgetfulness.name.styled("nt"), "to move all enemy cultist from", r, "to", d)
            EndAction(self)

        // ABHOTH
        case FilthMainAction(self, l) =>
            Ask(self).each(l)(r => FilthAction(self, r)).cancel

        case FilthAction(self, r) =>
            self.power -= 1
            self.payTax(r)

            self.place(Filth, r)
            log(Abhoth.styled(self), "placed", Filth.styled(self), "in", r)

            EndAction(self)

        // DAOLOTH — Round 8 Bug 49 (FB CG ordering): when Daoloth moves to a new region
        // and the owner has Interdimensional, place the gate IMMEDIATELY here in the
        // MovedAction hook (before EndAction → AfterAction). This ensures the gate
        // exists before FB Cyclopean Gaze fires in AfterAction. Otherwise CG would
        // pain Daoloth out of the destination region BEFORE checkInterdimensional runs
        // (which only fires in the base AfterAction handler via checkGatesGained →
        // triggers()), causing the gate to be placed in the WRONG region (the pained-to
        // destination, not the originally intended move destination).
        case MovedAction(self, u, o, r) if u.uclass == Daoloth && self.can(Interdimensional) =>
            checkInterdimensional()
            MoveContinueAction(self, true)

        // NYOGTHA — From Below: second unit moves free (suppressed if Elder Thing shares area with EITHER Nyogtha)
        case MovedAction(self, u, o, r) if u.uclass == Nyogtha =>
            if (self.all(Nyogtha).exists(n => ElderThingMindControl.suppresses(n))) {
                log(self, "From Below".styled("nt"), "blocked by", "Elder Thing".styled("nt"))
                MoveContinueAction(self, true)
            } else
                self.all(Nyogtha).but(u).not(Moved).%(_.region.onMap).single./(n => MoveSelectAction(self, n, n.region, 0)).|(MoveContinueAction(self, true))

        case NightmareWebMainAction(self, regions) =>
            Ask(self).each(regions)(r => NightmareWebAction(self, r)).cancel

        case NightmareWebAction(self, r) =>
            self.power -= 2
            self.payTax(r)

            val ny = self.pool.one(Nyogtha)

            ny.region = r

            self.log("awakened", Nyogtha.styled(self), "in", r, "with", NightmareWeb.styled(self))

            EndAction(self)

        // ── Y'GOLONAC: ORIFICES (battle ability) ──
        // Triggered from Battle.scala YgolonacOrificesPhase when Y'Golonac is killed.
        // The owner chooses an enemy Monster, Terror, or Cultist in the same battle.
        // That unit is eliminated, and Y'Golonac is placed under the victim's faction's
        // control. The loyalty card and spellbook (The Revelations) transfer with it.
        // If the receiving faction didn't already have The Revelations, they gain it
        // automatically (satisfies the spellbook requirement). After the transfer,
        // battle resumes at EliminatePhase to process the original Y'Golonac kill.
        case YgolonacOrificesAction(self, target) =>
            val targetFaction = target.faction
            val region = target.region

            // Eliminate the replaced unit (use battle.eliminate so it's added to
            // the `eliminated` list — that makes the unit available to Velvet Fan
            // capture per "killed or eliminated enemy" card rule).
            val targetFigure = game.unit(target)
            game.battle match {
                case Some(b) => b.eliminate(targetFigure)
                case None    => game.eliminate(targetFigure)
            }

            // Place Y'Golonac in the target's region under the new owner
            val newYg = new UnitFigure(targetFaction, Ygolonac, 1, region)
            targetFaction.units :+= newYg

            // Transfer loyalty card from old owner to new owner
            self.loyaltyCards :-= YgolonacCard
            targetFaction.loyaltyCards :+= YgolonacCard

            // Transfer spellbook if the old owner already earned it
            if (self.upgrades.has(TheRevelations)) {
                self.upgrades :-= TheRevelations
                targetFaction.upgrades :+= TheRevelations
            }

            // Receiving Y'Golonac via Orifices automatically satisfies the SBR
            if (targetFaction.upgrades.has(TheRevelations).not) {
                targetFaction.upgrades :+= TheRevelations
                targetFaction.log("gained", TheRevelations.styled(targetFaction), "for receiving", Ygolonac.styled(targetFaction))
            }

            self.log(Ygolonac.styled(self), "used Orifices:", Ygolonac, "now belongs to", targetFaction)

            // Resume battle at EliminatePhase to process Y'Golonac's original kill
            BattleProceedAction(EliminatePhase)


        // ── CEREMONY OF ANNIHILATION (Tulzscha spellbook, doom-phase action) ──
        // Instead of a normal ritual, the owner gains Power equal to the current ritual
        // cost. No Doom or Elder Signs are awarded. The ritual track still advances (so
        // future rituals cost more). Counts as "performing the ritual" for spellbook
        // requirements (PerformRitual). Offered in the DoomAction handler when the owner
        // has CeremonyOfAnnihilation and hasn't already acted this doom phase.
        case CeremonyOfAnnihilationChoiceAction(self) =>
            val earned = game.ritualCost
            self.power += earned
            if (game.ritualTrack(game.ritualMarker) != 999)
                game.ritualMarker += 1
            game.showROAT()
            self.acted = true
            self.satisfy(PerformRitual, "Perform Ritual of Annihilation")
            self.log("used", CeremonyOfAnnihilation.styled(self), "and earned", earned.power, "(no Doom or Elder Signs)")
            CheckSpellbooksAction(DoomAction(self))

        // ── TULZSCHA SPELLBOOK REQUIREMENT ──
        // The owner voluntarily gives each enemy faction 2 Power. Once done, the owner
        // receives the Ceremony of Annihilation spellbook. Offered in the MainAction
        // handler when the owner has Tulzscha in play but hasn't earned the spellbook yet.
        case TulzschaGivePowerMainAction(self) =>
            Ask(self).add(TulzschaGivePowerAction(self)).cancel

        case TulzschaGivePowerAction(self) =>
            self.enemies.foreach { f =>
                f.power += 2
                log(f, "gained 2 Power from", Tulzscha.styled(self), "Spellbook Requirement")
            }

            if (self.upgrades.has(CeremonyOfAnnihilation).not) {
                self.upgrades :+= CeremonyOfAnnihilation
                self.log("gained", CeremonyOfAnnihilation.styled(self), "for", Tulzscha.styled(self))
            }

            EndAction(self)

        // ...
        // Yig: Messenger of Yig choices — re-enter DoomDoneAction to ask next enemy
        case MessengerOfYigDonateAction(self, yigOwner) =>
            self.power -= 1
            yigOwner.power += 1
            self.log("donated", 1.power, "to", yigOwner.full, "via", "Messenger of Yig".styled("nt"))
            self.oncePerTurn :+= MessengerOfYig
            Force(DoomAction(self))

        case MessengerOfYigRefuseAction(self, yigOwner) =>
            yigOwner.doom += 1
            self.log("refused", "Messenger of Yig".styled("nt") + ";", yigOwner.full, "gained", 1.doom)
            self.oncePerTurn :+= MessengerOfYig
            Force(DoomAction(self))

        // Father Dagon: Tsunami
        case FatherDagonTsunamiMainAction(self) =>
            val landNearOcean = areas.%(r => r.glyph != Ocean && game.board.connected(r).exists(_.glyph == Ocean))
            Ask(self).each(landNearOcean)(r => FatherDagonTsunamiTargetAction(self, r)).cancel

        case FatherDagonTsunamiTargetAction(self, r) =>
            self.power -= 1
            // Each faction's cultists moved by their owner to adjacent ocean areas
            val perFaction = factions./(f => (f, f.at(r).%(_.uclass.utype == Cultist))).%{ case (_, units) => units.any }
            self.log("Tsunami".styled("nt") + ": all Cultists in", r, "must move to adjacent Ocean")
            TsunamiProcessAction(self, r, perFaction, oceanDest = true, EndAction(self))

        // Mother Hydra: Agony Sting
        case MotherHydraAgonyStingMainAction(self) =>
            val oceanAreas = areas.%(_.glyph == Ocean).%(r => self.enemies.exists(_.at(r).%(_.uclass.utype == Cultist).any))
            Ask(self).each(oceanAreas)(r => MotherHydraAgonyStingTargetAction(self, r)).cancel

        case MotherHydraAgonyStingTargetAction(self, r) =>
            self.power -= 1
            // Each enemy's cultists moved by their owner to adjacent land areas (owner's cultists immune)
            val perFaction = self.enemies./(f => (f, f.at(r).%(_.uclass.utype == Cultist))).%{ case (_, units) => units.any }
            self.log("The Agony Sting".styled("nt") + ": all enemy Cultists in", r, "must move to adjacent Land")
            TsunamiProcessAction(self, r, perFaction, oceanDest = false, EndAction(self))

        // Tsunami/Agony Sting: process per-faction cultist movement
        case TsunamiProcessAction(self, sourceRegion, remaining, oceanDest, then) =>
            if (remaining.none) {
                Force(then)
            } else {
                val (faction, cultists) = remaining.head
                val adj = if (oceanDest)
                    game.board.connected(sourceRegion).%(_.glyph == Ocean)
                else
                    game.board.connected(sourceRegion).%(_.glyph != Ocean)
                if (adj.none || cultists.none) {
                    // No valid destination — skip
                    TsunamiProcessAction(self, sourceRegion, remaining.tail, oceanDest, then)
                } else if (adj.num == 1) {
                    // Only one option — auto-move
                    cultists.foreach { u =>
                        u.region = adj.head
                        u.onGate = false
                    }
                    faction.log("moved", cultists.num, "cultist".s(cultists.num), "to", adj.head)
                    TsunamiProcessAction(self, sourceRegion, remaining.tail, oceanDest, then)
                } else {
                    // Multiple destinations — faction chooses for each cultist
                    val u = cultists.head
                    val rest = cultists.tail
                    val nextRemaining = if (rest.any) (faction, rest) +: remaining.tail else remaining.tail
                    Ask(faction).each(adj)(dest => TsunamiMoveCultistAction(faction, u, dest, rest, TsunamiProcessAction(self, sourceRegion, nextRemaining, oceanDest, then)))
                }
            }

        case TsunamiMoveCultistAction(self, u, dest, remaining, then) =>
            u.region = dest
            u.onGate = false
            self.log("moved", u.uclass.styled(self), "to", dest)
            Force(then)

        // Ghatanothoa IGOO: Mummify
        case GhatanotoaMummifyAction(self) =>
            self.power -= 1
            val ghat = self.allInPlay.%(_.uclass == GhatanotoaIGOO).head
            val r = ghat.region
            val targets = self.enemies./~(_.at(r).%(_.uclass.utype == Cultist).%(u => !game.mummifiedCultists.has(u.ref)))
            targets.foreach { u =>
                game.mummifiedCultists :+= u.ref
            }
            self.log("Mummify".styled("nt") + ":", targets.num, "enemy cultist".s(targets.num), "mummified in", r)
            EndAction(self)

        // Atlach-Nacha: Place Spinneret — place web token
        case PlaceSpinneretMainAction(self) =>
            self.power -= 1
            val an = self.allInPlay.%(_.uclass == AtlachNacha).head
            val r = an.region
            game.webTokens :+= r
            self.log("Place Spinneret".styled("nt") + ": placed web token in", r, "(" + game.webTokens.num + "/6)")
            if (game.webTokens.num >= 6) {
                self.log("completed", CosmicWeb.styled(self), "for", AtlachNacha.styled(self))
            }
            EndAction(self)

        // Innsmouth Look doom menu action — chains to acolyte selection, then back to DoomAction
        case InnsmouthLookDoomAction(self) =>
            val acolytes = self.allInPlay.%(_.uclass == Acolyte)
            if (acolytes.any) {
                // Distinguish on-gate vs off-gate in the menu
                Ask(self).each(acolytes)(u => InnsmouthLookChooseAction(self, u, InnsmouthLookDoomDoneAction(self)))
            } else {
                self.oncePerTurn :+= TheInnsmouthLook
                Force(DoomAction(self))
            }

        // After Innsmouth Look resolves during doom, mark resolved and return to doom menu
        case InnsmouthLookDoomDoneAction(self) =>
            self.oncePerTurn :+= TheInnsmouthLook
            Force(DoomAction(self))

        // Messenger of Yig doom menu action — shows donate/refuse choice
        case MessengerOfYigDoomAction(self, yigOwner) =>
            Ask(self)
                .add(MessengerOfYigDonateAction(self, yigOwner))
                .add(MessengerOfYigRefuseAction(self, yigOwner))

        // Father Dagon: The Innsmouth Look — forced Acolyte removal (player chooses which)
        case InnsmouthLookRemoveAction(self, then) =>
            val acolytes = self.allInPlay.%(_.uclass == Acolyte)
            if (acolytes.any) {
                Ask(self).each(acolytes)(u => InnsmouthLookChooseAction(self, u, then))
            } else {
                Force(then)
            }

        case InnsmouthLookChooseAction(self, u, then) =>
            self.units = self.units.%(_.ref != u.ref)
            self.power += 6
            self.log("The Innsmouth Look".styled("nt") + ": removed", Acolyte.styled(self), "from", u.region, "permanently, gained", 6.power)
            Force(then)

        // Mother Hydra: The Zygote spellbook — place Acolytes from Pool one by one
        case TheZygoteMainAction(self) =>
            self.power -= 1
            Force(TheZygoteContinueAction(self))

        case TheZygoteContinueAction(self) =>
            val remaining = self.pool(Acolyte).num
            if (remaining == 0) {
                self.log("The Zygote".styled("nt") + ": all Acolytes placed")
                EndAction(self)
            }
            else {
                val ownAreas = areas.nex.%(r => self.at(r).%(_.uclass.utype != MapUnit).any || self.gates.has(r))
                if (ownAreas.num == 1)
                    Force(TheZygoteTargetAction(self, ownAreas.head, remaining))
                else
                    Ask(self).each(ownAreas)(r => TheZygoteTargetAction(self, r, remaining))
            }

        case TheZygoteTargetAction(self, r, remaining) =>
            self.place(Acolyte, r)
            self.log("The Zygote".styled("nt") + ": placed", Acolyte.styled(self), "in", r, "(" + (remaining - 1) + " remaining)")
            Force(TheZygoteContinueAction(self))

        // ── YIG SPELLBOOK REQUIREMENT ──
        case YigRemoveGateMainAction(self) =>
            Ask(self).each(self.allGates.onMap)(r => YigRemoveGateAction(self, r)).cancel

        case YigRemoveGateAction(self, r) =>
            // Remove the gate (cultist stays)
            self.at(r).foreach(_.onGate = false)
            self.gates = self.gates.but(r)
            game.gates = game.gates.but(r)
            self.log("removed Gate in", r, "(Yig Spellbook Requirement)")
            // Grant spellbook
            if (self.upgrades.has(MessengerOfYig).not) {
                self.upgrades :+= MessengerOfYig
                self.log("gained", MessengerOfYig.styled(self), "for", Yig.styled(self))
            }
            EndAction(self)

        // ── GHATANOTHOA IGOO SBR: pay 3 Power ──
        case GhatanotoaSBRPayAction(self) =>
            self.power -= 3
            self.upgrades :+= ExecrationOfMu
            self.log("paid", 3.power, "for", ExecrationOfMu.styled(self), "for", GhatanotoaIGOO.styled(self))
            EndAction(self)

        // ── NUCLEAR CHAOS (Azathoth spellbook) ──
        case NuclearChaosMainAction(self) =>
            // All players roll 1d6
            val rolls = factions.map(f => f -> (1::2::3::4::5::6).shuffle.first).toMap
            factions.foreach { f => f.log("rolled a " + rolls(f) + " for", "Nuclear Chaos".styled("nt")) }
            // Owner may adjust their roll +/-1
            val myRoll = rolls(self)
            val canPlus = myRoll < 6
            val canMinus = myRoll > 1
            var ask = Ask(self)
            if (canPlus) ask = ask.add(NuclearChaosAdjustAction(self, rolls, 1))
            if (canMinus) ask = ask.add(NuclearChaosAdjustAction(self, rolls, -1))
            ask = ask.add(NuclearChaosKeepAction(self, rolls))
            ask

        case NuclearChaosAdjustAction(self, rolls, adjust) =>
            val adjusted = rolls + (self -> (rolls(self) + adjust))
            self.log("Nuclear Chaos".styled("nt") + ": adjusted roll to", adjusted(self))
            Force(NuclearChaosRollAction(self, adjusted))

        case NuclearChaosKeepAction(self, rolls) =>
            Force(NuclearChaosRollAction(self, rolls))

        case NuclearChaosRollAction(self, rolls) =>
            // Flip facedown now (after adjust/keep choice resolved)
            if (!self.oncePerGame.has(NuclearChaos))
                self.oncePerGame :+= NuclearChaos
            val maxRoll = rolls.values.max
            val minRoll = rolls.values.min
            // Highest roller(s) gain Power equal to their roll
            rolls.foreach { case (f, roll) =>
                if (roll == maxRoll) {
                    f.power += roll
                    f.log("Nuclear Chaos".styled("nt") + s": highest roll ($roll), gained", roll.power)
                }
            }
            // Lowest roller(s) gain that many Elder Signs
            rolls.foreach { case (f, roll) =>
                if (roll == minRoll && roll != maxRoll) {
                    f.takeES(roll)
                    f.log("Nuclear Chaos".styled("nt") + s": lowest roll ($roll), gained", roll.es)
                }
            }
            EndAction(self)

        // ── AZATHOTH IGOO: Custom Awakening ──
        case AzathothAwakenMainAction(self) =>
            // Choose which gate to place Azathoth at (must have own GOO at controlled gate)
            val gooAtGate = self.allGates.onMap.%(r => self.at(r).goos.any)
            Ask(self).each(gooAtGate)(r => AzathothAwakenCommitAction(self, 0, r).as("Awaken Azathoth at", r)).cancel

        case AzathothAwakenCommitAction(self, _, gateRegion) =>
            // Store gate region for later resolve
            game.azathothAwakenGateRegion = |(gateRegion)
            game.azathothAwakenChoices = Map()
            // Roll 1d6+2 for Power cost
            RollD6("Azathoth Awakening — roll for Power cost", roll => {
                val powerCost = roll + 2
                self.power -= powerCost
                self.log("rolled", roll, "+ 2 =", powerCost.power, "to awaken", "Azathoth".styled("nt"))
                // Start enemy choice loop
                val enemies = factions.but(self)
                AzathothEnemyChoiceAction(self, 0, enemies, Map())
            })

        case AzathothEnemyChoiceAction(self, _, remaining, choices) =>
            if (remaining.any) {
                val chooser = remaining.head
                Ask(chooser).each($(1, 2, 3, 4, 5, 6))(n => AzathothEnemyChooseAction(self, n, chooser))
            } else {
                // All enemies have chosen — resolve
                AzathothResolveAction(self, game.azathothAwakenChoices, game.azathothAwakenGateRegion.get)
            }

        case AzathothEnemyChooseAction(self, face, chooser) =>
            // Hidden choice — don't reveal the number yet
            chooser.log("has chosen their dice face")
            // Accumulate choice in game var
            game.azathothAwakenChoices += (chooser -> face)
            // Continue with remaining enemies
            val enemies = factions.but(self)
            val remaining = enemies.%(f => !game.azathothAwakenChoices.contains(f))
            AzathothEnemyChoiceAction(self, 0, remaining, game.azathothAwakenChoices)

        case AzathothResolveAction(self, choices, gateRegion) =>
            // Reveal all choices at once
            choices.foreach { case (f, face) =>
                f.log("chose the", face, "dice face")
                f.power += face
                f.log("gained", face.power)
            }
            // Find lowest roller(s) — they lose 2 Doom
            val minFace = choices.values.min
            choices.foreach { case (f, face) =>
                if (face == minFace) {
                    f.doom = math.max(0, f.doom - 2)
                    f.log("had lowest Azathoth vote (" + face + "), lost", 2.doom)
                }
            }
            // Sum all faces = glyph position = combat value
            val total = choices.values.sum
            game.azathothGlyphPosition = total
            log("Azathoth".styled("nt"), "glyph placed at", total, "on the Doom track")

            // Place Azathoth
            self.loyaltyCards :+= AzathothIGOOCard
            game.loyaltyCards :-= AzathothIGOOCard
            self.units :+= new UnitFigure(self, AzathothIGOO, 1, gateRegion)
            self.log("awakened", "Azathoth".styled("nt"), "in", gateRegion, s"(Combat: $total)")

            if (self.has(Immortal)) {
                self.log("gained", 1.es, "as", Immortal)
                self.takeES(1)
            }

            EndAction(self)

        // ── BOKRUG: Give Bokrug (ever-present out-of-turn action) ──
        case GiveBokrugMainAction(self) =>
            Ask(self).each(self.enemies)(e => GiveBokrugAction(self, e)).cancel

        case GiveBokrugAction(self, target) =>
            // Transfer Bokrug ownership: loyalty card, unit, spellbook
            self.loyaltyCards :-= BokrugCard
            target.loyaltyCards :+= BokrugCard

            // Move all Bokrug units (in play or in pool) to target
            val bokrugs = self.units.%(_.uclass == Bokrug)
            bokrugs.foreach { u =>
                self.units :-= u
                target.units :+= new UnitFigure(target, Bokrug, u.index, u.region, u.onGate, u.state, u.health)
            }

            // Grant Doom that Came to Sarnath spellbook to the receiving faction
            if (target.upgrades.has(DoomThatCameToSarnath).not) {
                target.upgrades :+= DoomThatCameToSarnath
                target.log("gained", DoomThatCameToSarnath.styled(target), "for receiving", Bokrug.styled(target))
            }

            self.log("gave", Bokrug.styled(self), "to", target.full)
            EndAction(self)

        // ── BOKRUG: Ghosts of Ib (doom phase placement) ──
        case GhostsOfIbPlaceAction(self, then) =>
            val bokrugOwner = factions.%(_.loyaltyCards.has(BokrugCard)).headOption
            bokrugOwner match {
                case Some(owner) if owner.allInPlay.%(_.uclass == Bokrug).none =>
                    // Bokrug NOT on map — check for valid regions (no enemy units)
                    val validRegions = areas.nex.%(r => owner.enemies.forall(_.at(r).%(_.uclass.utype != MapUnit).none))
                    if (validRegions.any) {
                        log(Bokrug.styled(owner), "Ghosts of Ib".styled("nt") + ":", owner.full, "must place", Bokrug.styled(owner))
                        Ask(owner).each(validRegions)(r => GhostsOfIbChooseAction(owner, r, then))
                    } else {
                        Force(then)
                    }
                case _ => Force(then)
            }

        case GhostsOfIbChooseAction(self, r, then) =>
            val bokrug = self.pool.one(Bokrug)
            bokrug.region = r
            self.log("Ghosts of Ib".styled("nt") + ": placed", Bokrug.styled(self), "in", r)
            Force(then)

        // ── BOKRUG: Doom that Came to Sarnath (doom phase effect) ──
        case DoomSarnathMainAction(self, then) =>
            val bokrugOwner = factions.%(f => f.loyaltyCards.has(BokrugCard) && f.has(DoomThatCameToSarnath) && !f.oncePerGame.has(DoomThatCameToSarnath)).headOption
            bokrugOwner match {
                case Some(owner) =>
                    val hasUnits = owner.allInPlay.%(u => u.uclass.utype == Monster || u.uclass.utype == Cultist).any
                    val hasES = owner.es.any
                    if (hasUnits || hasES) {
                        var ask = Ask(owner)
                        if (hasUnits)
                            ask = ask.add(DoomSarnathChooseOption(owner, 1, then))
                        if (hasES)
                            ask = ask.add(DoomSarnathChooseOption(owner, 2, then))
                        ask
                    } else {
                        Force(then)
                    }
                case _ => Force(then)
            }

        case DoomSarnathChooseOption(self, option, then) =>
            // Choose enemy faction (with cancel back to first menu)
            Ask(self).each(self.enemies)(e => DoomSarnathChooseFactionAction(self, option, e, then)).cancel

        case DoomSarnathChooseFactionAction(self, option, target, then) =>
            if (option == 1) {
                // [2026-05-23] Doom that Came to Sarnath: only ON-MAP units are eligible.
                // Elimination moves a unit out-of-play into the pool, so pool / slumber /
                // sorcery / deep units (region.onMap == false) cannot be targeted.
                val units = self.units.%(u => u.region.onMap && (u.uclass.utype == Monster || u.uclass.utype == Cultist))
                val regionSorted = units.sortBy(u => u.region.name)
                Ask(target).each(regionSorted)(u => DoomSarnathEliminateUnit(target, self, u, then))
            } else {
                // Enemy chooses one of Bokrug owner's elder signs (face-down)
                var ask = Ask(target)
                self.es.indices.foreach { i =>
                    ask = ask.add(DoomSarnathDiscardES(target, self, i, then))
                }
                ask
            }

        case DoomSarnathEliminateUnit(self, owner, u, then) =>
            game.eliminate(u)
            self.log("Doom that Came to Sarnath".styled("nt") + ":", self.full, "eliminated", u.uclass.styled(owner), "of", owner.full, "in", u.region)
            Force(then)

        case DoomSarnathDiscardES(self, owner, index, then) =>
            val es = owner.es(index)
            val value = es.value
            owner.es = owner.es.patch(index, Nil, 1)
            self.log(self.full, "discarded an elder sign of", owner.full, "worth", value.doom)
            Force(then)

        case _ => UnknownContinue
    }
}
