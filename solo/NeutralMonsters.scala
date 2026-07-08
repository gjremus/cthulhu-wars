package cws

import hrf.colmat._

import html._


// Neutral Monsters
case object GhastCard extends NeutralMonsterLoyaltyCard(GhastIcon, Ghast, cost = 2, quantity = 4, combat = 0)
case object GugCard extends NeutralMonsterLoyaltyCard(GugIcon, Gug, cost = 1, quantity = 2, combat = 3)
case object ShantakCard extends NeutralMonsterLoyaltyCard(ShantakIcon, Shantak, cost = 2, quantity = 2, combat = 2)
case object StarVampireCard extends NeutralMonsterLoyaltyCard(StarVampireIcon, StarVampire, cost = 2, quantity = 3, combat = 1)
case object VoonithCard extends NeutralMonsterLoyaltyCard(VoonithIcon, Voonith, cost = 3, quantity = 2, combat = 1)

case object GhastIcon extends UnitClass(Ghast.name + " Icon", Token, 0)
case object GugIcon extends UnitClass(Gug.name + " Icon", Token, 0)
case object ShantakIcon extends UnitClass(Shantak.name + " Icon", Token, 0)
case object StarVampireIcon extends UnitClass(StarVampire.name + " Icon", Token, 0)
case object VoonithIcon extends UnitClass(Voonith.name + " Icon", Token, 0)

trait NeutralMonster

case object Ghast extends UnitClass("Ghast", Monster, 2) with NeutralMonster { override val priority = 1001 }
case object Gug extends UnitClass("Gug", Monster, 1) with NeutralMonster {
    override def canCapture(u : UnitFigure)(implicit game : Game) = false
}
case object Shantak extends UnitClass("Shantak", Monster, 2) with NeutralMonster
case object StarVampire extends UnitClass("Star Vampire", Monster, 2) with NeutralMonster
case object Voonith extends UnitClass("Voonith", Monster, 3) with NeutralMonster
case object DimensionalShamblerCard extends NeutralMonsterLoyaltyCard(DimensionalShamblerIcon, DimensionalShamblerUnit, cost = 2, quantity = 3, combat = 2)
case object DimensionalShamblerIcon extends UnitClass(DimensionalShamblerUnit.name + " Icon", Token, 0)
case object DimensionalShamblerUnit extends UnitClass("Dimensional Shambler", Monster, 2) with NeutralMonster {
    override def canBeSummoned(f : Faction)(implicit game : Game) : Boolean = false
}
case object DimensionalShamblerHold extends UnitClass("Dimensional Shambler (Hold)", Token, 0)

case object GnorriCard extends NeutralMonsterLoyaltyCard(GnorriIcon, Gnorri, cost = 3, quantity = 3, combat = 2)
case object GnorriIcon extends UnitClass(Gnorri.name + " Icon", Token, 0)
case object Gnorri extends UnitClass("Gnorri", Monster, 3) with NeutralMonster

// ── NEW TERRORS ──
// DHOLE — Cost 4, Combat 5, Terror, Pool 1, Card Cost 2 Doom + 2 Power
// Planetary Destruction (Post-Battle): If the Dhole is Killed or Eliminated in a Battle,
// earn 2 Elder Signs. Additionally, your opponent gains your choice of 2 Doom or 2 Power.
case object DholeCard extends NeutralTerrorLoyaltyCard(DholeIcon, Dhole, cost = 2, powerCost = 2, quantity = 1, combat = 5)
case object DholeIcon extends UnitClass(Dhole.name + " Icon", Token, 0)
case object Dhole extends UnitClass("Dhole", Terror, 4) with NeutralMonster

// GREAT RACE OF YITH — Cost 4, Combat 3, Terror, Pool 1, Card Cost 2 Doom + 2 Power
// Possession (Ongoing and Gather Power Phase): If the Great Race of Yith is in an Area with
// an enemy Cultist, you may Capture that Cultist regardless of the presence of any enemy Units
// or Great Old Ones (or whether Windwalker's Ferox ability is in effect). In addition, if the
// Great Race of Yith is in play during the Gather Power Phase, earn 1 Power per Captured
// Cultist in addition to the normal reward of 1 Power per Cultist.
case object GreatRaceOfYithCard extends NeutralTerrorLoyaltyCard(GreatRaceOfYithIcon, GreatRaceOfYith, cost = 2, powerCost = 2, quantity = 1, combat = 3)
case object GreatRaceOfYithIcon extends UnitClass(GreatRaceOfYith.name + " Icon", Token, 0)
case object GreatRaceOfYith extends UnitClass("Great Race of Yith", Terror, 4) with NeutralMonster

case object QuachilUttausCard extends NeutralTerrorLoyaltyCard(QuachilUttausIcon, QuachilUttaus, cost = 2, powerCost = 2, quantity = 1, combat = 1)
case object QuachilUttausIcon extends UnitClass(QuachilUttaus.name + " Icon", Token, 0)
case object QuachilUttaus extends UnitClass("Quachil Uttaus", Terror, 4) with NeutralMonster

case object ShadowPharaohCard extends NeutralTerrorLoyaltyCard(ShadowPharaohIcon, ShadowPharaoh, cost = 2, powerCost = 2, quantity = 1, combat = 0)
case object ShadowPharaohIcon extends UnitClass(ShadowPharaoh.name + " Icon", Token, 0)
case object ShadowPharaoh extends UnitClass("The Shadow Pharaoh", Terror, 2) with NeutralMonster

case object HoundOfTindalosCard extends NeutralTerrorLoyaltyCard(HoundOfTindalosIcon, HoundOfTindalos, cost = 2, powerCost = 2, quantity = 1, combat = 4)
case object HoundOfTindalosIcon extends UnitClass(HoundOfTindalos.name + " Icon", Token, 0)
case object HoundOfTindalos extends UnitClass("Hound of Tindalos", Terror, 4) with NeutralMonster {
    override def canMove(u : UnitFigure)(implicit game : Game) = false
}

// "gate region" predicate for the Hound of Tindalos.
object HoundOfTindalosGates {
    def regions(implicit game : Game) : $[Region] = {
        val realGates = game.gates.%(_.onMap)
        val yogGates = game.factions./~(_.unitGate)./(_.region).%{ r =>
            !game.factions./~(_.allInPlay).%(_.uclass == ElderThing).exists(et => et.region == r)
        }
        realGates ++ yogGates.%(!realGates.has(_))
    }
    // Survival check includes BB.moon when BB is in the game — a Hound dragged
    // to the Moon by Catnapping must not be eliminated by Angles of Time.
    def survivalRegions(implicit game : Game) : $[Region] = {
        val base = regions
        val bbMoon = game.factions.has(BB).??($(BB.moon))
        (base ++ bbMoon).distinct
    }
    def has(r : Region)(implicit game : Game) : Boolean = regions.has(r)
    def survives(r : Region)(implicit game : Game) : Boolean = survivalRegions.has(r)
}

case object BrownJenkinCard extends NeutralTerrorLoyaltyCard(BrownJenkinIcon, BrownJenkin, cost = 2, powerCost = 2, quantity = 1, combat = 0)
case object BrownJenkinIcon extends UnitClass(BrownJenkin.name + " Icon", Token, 0)
case object BrownJenkin extends UnitClass("Brown Jenkin", Terror, 2) with NeutralMonster

case object ElderShoggothCard extends NeutralTerrorLoyaltyCard(ElderShoggothIcon, ElderShoggoth, cost = 2, powerCost = 2, quantity = 1, combat = 2)
case object ElderShoggothIcon extends UnitClass(ElderShoggoth.name + " Icon", Token, 0)
case object ElderShoggoth extends UnitClass("Elder Shoggoth", Terror, 4) with NeutralMonster

// ── NEW MONSTERS ──
case object MoonbeastCard extends NeutralMonsterLoyaltyCard(MoonbeastIcon, MoonbeastUnit, cost = 2, quantity = 4, combat = 0)
case object MoonbeastIcon extends UnitClass("Moonbeast Icon", Token, 0)
case object MoonbeastUnit extends UnitClass("Moonbeast", Monster, 2) with NeutralMonster {
    override def canBeSummoned(f : Faction)(implicit game : Game) : Boolean = false
}

case object AlbinoPenguinsCard extends NeutralMonsterLoyaltyCard(AlbinoPenguinsIcon, AlbinoPenguins, cost = 1, quantity = 2, combat = 0)
case object AlbinoPenguinsIcon extends UnitClass(AlbinoPenguins.name + " Icon", Token, 0)
case object AlbinoPenguins extends UnitClass("Giant Blind Albino Penguins", Monster, 1) with NeutralMonster

case object ElderThingCard extends NeutralMonsterLoyaltyCard(ElderThingIcon, ElderThing, cost = 2, quantity = 3, combat = 2)
case object ElderThingIcon extends UnitClass(ElderThing.name + " Icon", Token, 0)
case object ElderThing extends UnitClass("Elder Thing", Monster, 2) with NeutralMonster

case object LengSpiderCard extends NeutralMonsterLoyaltyCard(LengSpiderIcon, LengSpiderUnit, cost = 2, quantity = 3, combat = 1)
case object LengSpiderIcon extends UnitClass("Leng Spider Icon", Token, 0)
case object LengSpiderUnit extends UnitClass("Leng Spider", Monster, 2) with NeutralMonster

case object SatyrCard extends NeutralMonsterLoyaltyCard(SatyrIcon, Satyr, cost = 2, quantity = 3, combat = 1)
case object SatyrIcon extends UnitClass(Satyr.name + " Icon", Token, 0)
case object Satyr extends UnitClass("Satyr", Monster, 2) with NeutralMonster

case object ServitorCard extends NeutralMonsterLoyaltyCard(ServitorIcon, ServitorUnit, cost = 2, quantity = 3, combat = 0)
case object ServitorIcon extends UnitClass(ServitorUnit.name + " Icon", Token, 0)
case object ServitorUnit extends UnitClass("Servitor of the Outer Gods", Monster, 1) with NeutralMonster

case object InsectsFromShaggaiCard extends NeutralMonsterLoyaltyCard(InsectsFromShaggaiIcon, InsectsFromShaggai, cost = 2, quantity = 3, combat = 0)

// Mind Parasite Cultist — replaces Acolyte when parasitized by Insects from Shaggai
// Properties per user spec:
// - Belongs to insect owner's faction (fights on their side, movable by them)
// - Tracks original faction (for gather power/doom, capture rules)
// - Original faction can block capture (choice)
// - Original faction cannot capture them
// - Generates power/doom for ORIGINAL faction, not insect owner
// - Upon leaving an insect region, immediately replaced with normal Acolyte owned by original faction
case object MindParasiteCultist extends UnitClass("Parasitized Acolyte", Cultist, 1) {
    override def canControlGate(u : UnitFigure)(implicit game : Game) = false
    override def canMove(u : UnitFigure)(implicit game : Game) = !game.mummifiedCultists.has(u.ref)
    override def canBeMoved(u : UnitFigure)(implicit game : Game) = !game.mummifiedCultists.has(u.ref)
}
case object InsectsFromShaggaiIcon extends UnitClass("Insects Icon", Token, 0)
case object InsectsFromShaggai extends UnitClass("Insects from Shaggai", Monster, 2) with NeutralMonster


case class LoyaltyCardDoomAction(self : Faction) extends OptionFactionAction("Obtain " + "Loyalty Card".styled("nt")) with DoomQuestion with Soft with PowerNeutral
case class NeutralMonstersAction(self : Faction, lc : LoyaltyCard) extends BaseFactionAction(g => "", {
    val qm = Overlays.imageSource("question-mark")
    val p = s""""${lc.name.replace('\\'.toString, '\\'.toString + '\\'.toString)}"""".replace('"'.toString, "&quot;")
    "<div class=sbdiv>" +
        lc.short +
        s"""<img class=explain src="${qm}" onclick="event.stopPropagation(); onExternalClick(${p})" onpointerover="onExternalOver(${p})" onpointerout="onExternalOut(${p})" />""" +
    "</div>"
}) with PowerNeutral
case class LoyaltyCardSummonAction(self : Faction, uc : UnitClass, r : Region) extends BaseFactionAction(g => "" + self + " places " + uc.styled(self) + " in", implicit g => r + self.iced(r))

case class FreeSummonAction(self : Faction, uc : UnitClass, r : Region, l : $[Region]) extends BaseFactionAction(g => "" + self + " summons " + uc.styled(self) + " for free in", implicit g => r + self.iced(r))

// Servitor of the Outer Gods: choose faction to give the loyalty card to
case class ServitorAssignFactionAction(self : Faction, target : Faction) extends BaseFactionAction(implicit g => "Servitor of the Outer Gods".styled("nt") + " — assign to faction", implicit g => target.full) {
    override def question(implicit game : Game) = self.full + " — " + "Servitor of the Outer Gods".styled("nt") + " — choose faction to receive card"
}
// Servitor of the Outer Gods: place servitor at gate (tracks original purchaser for DoomAction return)
case class ServitorPlaceAction(self : Faction, target : Faction, r : Region) extends BaseFactionAction(implicit g => target.full + " places " + ServitorUnit.styled(target) + " in", implicit g => r + target.iced(r))

case class ShantakCarryCultistAction(self : Faction, o : Region, ur : UnitRef, r : Region) extends ForcedAction
case class CronophageTeleportAction(self : Faction, houndRef : UnitRef, dest : Region) extends ForcedAction

// Brown Jenkin Familiar: forced gate choice for respawn (not an Action, not optional)
case class BrownJenkinFamiliarCheckAction(self : Faction, then : ForcedAction) extends ForcedAction
case class BrownJenkinFamiliarPlaceAction(self : Faction, r : Region, then : ForcedAction) extends BaseFactionAction(implicit g => "Familiar".styled("nt"), implicit g => "Place " + BrownJenkin.styled(self) + " at " + r)

// Moonbeast: custom summon onto enemy spellbook
case class MoonbeastSummonMainAction(self : Faction) extends OptionFactionAction(implicit g => "Summon " + "Moonbeast".styled("nt") + " onto enemy Spellbook " + "(" + "2 Power".styled("power") + ")") with MainQuestion with Soft
// Moonbeast: initial placement on acquisition (no Power cost, flows to DoomAction)
case class MoonbeastInitialPlaceAction(self : Faction, target : Faction, sb : Spellbook) extends BaseFactionAction(implicit g => "Moonbeast".styled("nt") + " — block " + sb.styled(target) + " on " + target.full, implicit g => sb.styled(target) + " (" + target.short + ")") {
    override def question(implicit game : Game) = self.full + " — " + "Moonbeast".styled("nt") + " — place on Spellbook"
}
// Moonbeast: premature return — victim spends 1 Doom (available any time, like Elder Signs)
case class MoonbeastPrematureReturnAction(self : Faction) extends BaseFactionAction("Moonbeast".styled("nt"), "Remove " + "Moonbeast".styled("nt") + " from Spellbook " + "(" + 1.doom + ")") with Soft
case class MoonbeastReturnChooseGateAction(self : Faction, mbRef : UnitRef, r : Region, then : Action) extends BaseFactionAction(implicit g => "Moonbeast".styled("nt") + " return", implicit g => "Place at " + r) with Soft
case class MoonbeastDoomReturnAction(self : Faction, remaining : $[(UnitRef, (Faction, Spellbook))], then : Action) extends ForcedAction
case class MoonbeastChooseFactionAction(self : Faction, target : Faction) extends BaseFactionAction(implicit g => "Moonbeast".styled("nt") + " — choose enemy", target.full) {
    override def question(implicit game : Game) = self.full + " — " + "Moonbeast".styled("nt") + " — choose enemy Faction"
}
case class MoonbeastChooseSpellbookAction(self : Faction, target : Faction, sb : Spellbook) extends BaseFactionAction(implicit g => "Moonbeast".styled("nt") + " — block Spellbook", implicit g => sb.styled(target) + " (" + target.short + ")") {
    override def question(implicit game : Game) = self.full + " — " + "Moonbeast".styled("nt") + " — choose Spellbook on " + target.full
}


object NeutralMonstersExpansion extends Expansion {
    def perform(action : Action, soft : VoidGuard)(implicit game : Game) = action @@ {
        case LoyaltyCardDoomAction(self) =>
            val monsterCards = game.loyaltyCards.of[NeutralMonsterLoyaltyCard].%(_.doom <= self.doom).%(_.power <= self.power).sortBy(_.name)
            val terrorCards = game.loyaltyCards.of[NeutralTerrorLoyaltyCard].%(_.doom <= self.doom).%(_.power <= self.power).sortBy(_.name)

            var ask = Ask(self).group("Obtain " + "Loyalty Card".styled("nt"))
            if (monsterCards.any) {
                ask = ask.group("Monsters".styled("nt") + " — " + "2 Doom".styled("doom"))
                ask = ask.each(monsterCards)(c => NeutralMonstersAction(self, c))
            }
            if (terrorCards.any) {
                ask = ask.group("Terrors".styled("nt") + " — " + "2 Doom".styled("doom") + " + " + "2 Power".styled("power"))
                ask = ask.each(terrorCards)(c => NeutralMonstersAction(self, c))
            }
            ask.cancel

        case NeutralMonstersAction(self, lc) =>
            self.loyaltyCards :+= lc
            game.loyaltyCards :-= lc

            self.hired = true

            // Shadow Pharaoh CC special: 0 Doom + 2 Power, others gain 1 ES
            if (lc.unit == ShadowPharaoh && self == CC) {
                self.power -= lc.power
                self.log("obtained the", lc.short, "Loyalty Card".styled("nt"), "for", lc.power.power, "(CC special)")
                factions.but(self).foreach { f =>
                    f.takeES(1)
                    f.log("gained", 1.es, "from", ShadowPharaoh.styled(self), "acquisition")
                }
            } else {
                self.doom -= lc.doom
                self.power -= lc.power
                self.log("obtained the", lc.short, "Loyalty Card".styled("nt"), "for", $((lc.doom > 0).??(lc.doom.doom), (lc.power > 0).??(lc.power.power)).but("").mkString(" and "))
            }

            if (lc.unit == DimensionalShamblerUnit) {
                lc.quantity.times(DimensionalShamblerUnit).foreach { u =>
                    self.units :+= new UnitFigure(self, u, self.units.%(_.uclass == u).num + 1, self.reserve)
                }
                self.pool(DimensionalShamblerUnit).head.region = ShamblerHold(self)
                self.log(DimensionalShamblerUnit.styled(self), "placed on Faction Card")
                ShamblerDeployCommandsAction(self, CheckSpellbooksAction(DoomAction(self)))
            } else if (lc.unit == MoonbeastUnit) {
                // Moonbeast: on acquisition, place first Moonbeast onto enemy spellbook/requirement slot
                // Card: "When a Moonbeast is Summoned, place it on a Spellbook on an enemy's Faction Card"
                lc.quantity.times(lc.unit).foreach { u =>
                    self.units :+= new UnitFigure(self, u, self.units.%(_.uclass == u).num + 1, self.reserve)
                }
                // Build list of available spellbook slots across all enemies
                // Rule: target EARNED spellbooks first. Only if no enemy has any earned spellbooks,
                // then target unfulfilled requirement slots (SBRs).
                val blockedSBs = game.moonbeastOnSpellbook.values./(t => t._2).toSet
                val earnedSlots = self.enemies./~{ target =>
                    target.spellbooks.%(sb => !blockedSBs.contains(sb))./(sb => (target, sb))
                }
                val allSlots = if (earnedSlots.any) earnedSlots else {
                    // Fallback: unfulfilled requirement slots
                    self.enemies./~{ target =>
                        target.library.%(sb => !target.spellbooks.has(sb) && !blockedSBs.contains(sb))./(sb => (target, sb))
                    }
                }
                if (allSlots.any)
                    Ask(self).each(allSlots)((target, sb) => MoonbeastInitialPlaceAction(self, target, sb))
                else {
                    self.log("no enemy Spellbooks or slots available for", MoonbeastUnit.styled(self))
                    CheckSpellbooksAction(DoomAction(self))
                }
            } else if (lc.unit == ServitorUnit) {
                // Servitor of the Outer Gods: upon taking loyalty card, immediately prompt
                // player to choose which faction receives the card. Cancel returns to card selection.
                Ask(self).each(self.enemies)(f => ServitorAssignFactionAction(self, f)).cancel
            } else if (lc.unit == InsectsFromShaggai) {
                // Insects from Shaggai: place in any Area
                lc.quantity.times(lc.unit).foreach { u =>
                    self.units :+= new UnitFigure(self, u, self.units.%(_.uclass == u).num + 1, self.reserve)
                }
                Ask(self).each(areas)(r => LoyaltyCardSummonAction(self, lc.unit, r))
            } else if (lc.unit == HoundOfTindalos) {
                // Hound of Tindalos: place at ANY Gate (not just owner's Controlled Gate)
                lc.quantity.times(lc.unit).foreach { u =>
                    self.units :+= new UnitFigure(self, u, self.units.%(_.uclass == u).num + 1, self.reserve)
                }
                if (game.gates.any)
                    Ask(self).each(game.gates)(r => LoyaltyCardSummonAction(self, lc.unit, r))
                else {
                    self.log("had nowhere to place", lc.unit.styled(self))
                    CheckSpellbooksAction(DoomAction(self))
                }
            } else {
                lc.quantity.times(lc.unit).foreach { u =>
                    self.units :+= new UnitFigure(self, u, self.units.%(_.uclass == u).num + 1, self.reserve)
                }
                if (self.allGates.onMap.any)
                    Ask(self).each(self.allGates.onMap)(r => LoyaltyCardSummonAction(self, lc.unit, r))
                else {
                    self.log("had nowhere to place", lc.unit.styled(self))
                    CheckSpellbooksAction(DoomAction(self))
                }
            }

        case LoyaltyCardSummonAction(self, uc, r) =>
            self.place(uc, r)
            self.log("placed", uc.styled(self), "in", r)

            // Mind Parasite: check for conversions after placing insect
            if (uc == InsectsFromShaggai)
                MindParasite.checkConversions()

            // Satyr Fecund: place 1 Acolyte from Pool with every Satyr placed (LC path here;
            // SummonedAction handler below covers subsequent regular-summon placements).
            if (uc == Satyr && self.loyaltyCards.has(SatyrCard) && self.pool(Acolyte).any) {
                self.place(Acolyte, r)
                self.log("Fecund".styled("nt") + ": placed", Acolyte.styled(self), "in", r, "with", Satyr.styled(self))
            }

            if (uc == Ghast && self.pool(Ghast).any)
                Ask(self).each(self.allGates.onMap)(r => LoyaltyCardSummonAction(self, uc, r))
            else
                CheckSpellbooksAction(DoomAction(self))

        // GHAST
        case SummonedAction(self, uc, r, l) if uc == Ghast && self.pool(Ghast).any =>
            Ask(self).each(self.summonRegions)(r => FreeSummonAction(self, uc, r, l))

        case FreeSummonAction(self, uc, r, l) =>
            if (l.has(r).not)
                self.payTax(r)

            self.place(uc, r)
            self.log("Frenzy".styled("nt") + ": summoned", uc.styled(self), "in", r, "for free")

            SummonedAction(self, uc, r, l :+ r)

        // BROWN JENKIN FAMILIAR: gate choice for respawn
        // Case 1: unlimited action on owner's turn (offered in action menu via bjFamiliar())
        // Case 2: post-battle forced respawn (BrownJenkinFamiliarCheckAction)
        case BrownJenkinFamiliarCheckAction(self, then) =>
            if (self.loyaltyCards.has(BrownJenkinCard) && self.allInPlay.%(_.uclass == BrownJenkin).none && self.pool(BrownJenkin).any && self.power >= 2 && self.allGates.onMap.any) {
                Ask(self).each(self.allGates.onMap)(r => BrownJenkinFamiliarPlaceAction(self, r, then))
            } else {
                Force(then)
            }

        case BrownJenkinFamiliarPlaceAction(self, r, then) =>
            self.power -= 2
            self.place(BrownJenkin, r)
            self.log("Familiar".styled("nt") + ":", BrownJenkin.styled(self), "returned to", r, "for", 2.power)
            Force(then)

        // SATYR FECUND: when a Satyr is summoned, also place 1 Acolyte from Pool
        case SummonedAction(self, uc, r, l) if uc == Satyr && self.loyaltyCards.has(SatyrCard) && self.pool(Acolyte).any =>
            self.place(Acolyte, r)
            self.log("Fecund".styled("nt") + ": placed", Acolyte.styled(self), "in", r, "with", Satyr.styled(self))
            EndAction(self)

        // HOUND OF TINDALOS CRONOPHAGE: when owner moves another unit, offer free Hound teleport
        // Card: "teleports directly from an Area with a Gate to another Area with a Gate"
        // Hound must be in a Gate area to teleport; can go to any other Gate area (no ownership required)
        // Exclude Shantak (has own MovedAction handler below) to avoid blocking carry-cultist
        case MovedAction(self, u, o, r) if u.uclass != HoundOfTindalos && u.uclass != Shantak && self.loyaltyCards.has(HoundOfTindalosCard) && self.allInPlay.%(_.uclass == HoundOfTindalos).any =>
            val hound = self.allInPlay.%(_.uclass == HoundOfTindalos).head
            if (HoundOfTindalosGates.has(hound.region)) {
                val gates = HoundOfTindalosGates.regions.%(g => g != hound.region)
                if (gates.any)
                    Ask(self)
                        .each(gates)(g => CronophageTeleportAction(self, hound.ref, g).as("Teleport to", g)("Cronophage".styled("nt") + " — teleport " + HoundOfTindalos.styled(self)))
                        .skip(MoveContinueAction(self, true))
                else
                    UnknownContinue
            } else
                UnknownContinue

        // SHANTAK
        case MovedAction(self, u, o, r) if u.uclass == Shantak =>
            Ask(self)
                .each(self.at(o).not(Moved).cultists.sortA)(u => ShantakCarryCultistAction(self, o, u, r).as(u.ref.full, "from", o)(Shantak, "carries", "Cultist".styled(self), "to", r))
                .skip(MoveContinueAction(self, true))

        case ShantakCarryCultistAction(self, o, u, r) =>
            u.region = r

            u.add(Moved)
            u.add(MovedForFree)

            log(Shantak.styled(self), "Riding the Shantak".styled("nt") + ": carried", u, "to", r)

            val dcProselytize = game.factions.has(DC) && DC.can(Proselytize) && (
                (self == DC && u.uclass == Acolyte) ||
                (u.uclass == MindParasiteCultist && game.mindParasiteOriginalFaction.get(u).has(DC))
            )
            if (dcProselytize)
                Force(DCProselytizeCheckAction(o, r, MoveContinueAction(self, true)))
            else
                MoveContinueAction(self, true)

        // DIMENSIONAL SHAMBLER - dedicated summon to faction card
        case ShamblerSummonMainAction(self) =>
            Ask(self).add(ShamblerSummonAction(self)).cancel

        case ShamblerSummonAction(self) =>
            self.power -= self.summonCost(DimensionalShamblerUnit, self.reserve)
            self.pool(DimensionalShamblerUnit).head.region = ShamblerHold(self)
            self.log("summoned", DimensionalShamblerUnit.styled(self), "to Faction Card")
            EndAction(self)

        // DIMENSIONAL SHAMBLER - deploy from faction card
        case ShamblerDeployCommandsAction(f, then) =>
            f.plans ++= $(
                ShamblerPrompt,
                ShamblerSkip,
                ShamblerThreatOfCapture,
                ShamblerThreatOfAttackOnGate,
                ShamblerThreatOfAttackOnGOO,
                ShamblerThreatOfLosingBattle,
                ShamblerOpportunityEndOfPhase,
            ) ++
            f.enemies.has(OW).$(ShamblerThreatOfBeyondOne) ++
            f.enemies.has(OW).$(ShamblerThreatOfDreadCurse)

            if (options.has(QuickGame)) {
                f.commands :+= ShamblerSkip
                f.commands :+= ShamblerThreatOfCapture
            }
            else
                f.commands :+= ShamblerPrompt

            then

        case ShamblerDeployPromptAction(f, then) =>
            Force(ShamblerDeployMainAction(f, then))

        case ShamblerDeployMainAction(f, then) =>
            Ask(f).each(areas.nex)(r => ShamblerDeployAction(f, r, then)).cancel

        case ShamblerDeployAction(f, r, then) =>
            if (f.at(ShamblerHold(f), DimensionalShamblerUnit).none)
                then
            else {
            val u = f.at(ShamblerHold(f)).one(DimensionalShamblerUnit)
            u.region = r
            log(DimensionalShamblerUnit.styled(f), "deployed to", r)

            if (f.at(ShamblerHold(f), DimensionalShamblerUnit).any)
                Ask(f).each(areas.nex)(r => ShamblerDeployAction(f, r, then)).add(then.as("Done".styled("power")))
            else {
                game.triggers()
                then
            }
            }

        // ...
        // MOONBEAST: custom summon onto enemy spellbook
        // Rule: earned spellbooks first, SBRs only if no enemy has any earned spellbooks
        case MoonbeastSummonMainAction(self) =>
            val blockedSBs = game.moonbeastOnSpellbook.values./(t => t._2).toSet
            val earnedTargets = self.enemies.%(f => f.spellbooks.%(sb => !blockedSBs.contains(sb)).any)
            val targets = if (earnedTargets.any) earnedTargets else {
                self.enemies.%(f => f.library.%(sb => !f.spellbooks.has(sb) && !blockedSBs.contains(sb)).any)
            }
            Ask(self).each(targets)(f => MoonbeastChooseFactionAction(self, f)).cancel

        case MoonbeastChooseFactionAction(self, target) =>
            val blockedSBs = game.moonbeastOnSpellbook.values.%(t => t._1 == target)./(t => t._2).toSet
            // Earned first, SBRs only if no earned available across all enemies
            val allEarned = self.enemies./~(_.spellbooks).%(sb => !blockedSBs.contains(sb))
            val available = if (allEarned.any) {
                target.spellbooks.%(sb => !blockedSBs.contains(sb))
            } else {
                target.library.%(sb => !target.spellbooks.has(sb) && !blockedSBs.contains(sb))
            }
            Ask(self).each(available)(sb => MoonbeastChooseSpellbookAction(self, target, sb)).cancel

        // Moonbeast: premature return — victim spends 1 Doom
        case MoonbeastPrematureReturnAction(self) =>
            self.doom -= 1
            val entries = game.moonbeastOnSpellbook.filter(_._2._1 == self).toList
            entries.foreach { case (mbRef, (target, sb)) =>
                target.oncePerGame = target.oncePerGame.but(sb)
            }
            game.moonbeastOnSpellbook = game.moonbeastOnSpellbook.filter(_._2._1 != self)
            self.log("spent", 1.doom, "to remove Moonbeasts from Spellbooks")
            // Use the same batch return flow as Doom Phase, then return to interrupted action
            MoonbeastDoomReturnAction(self, entries, OutOfTurnReturn)

        case MoonbeastReturnChooseGateAction(self, mbRef, r, then) =>
            val mb = game.unit(mbRef)
            mb.region = r
            self.log(MoonbeastUnit.styled(self), "placed at", r)
            Force(then)

        // Moonbeast: Doom Phase batch return with gate choices
        case MoonbeastDoomReturnAction(self, remaining, then) =>
            if (remaining.none) {
                Force(then)
            } else {
                val (mbRef, (target, sb)) = remaining.head
                val mb = game.unit(mbRef)
                val owner = mb.faction
                target.oncePerGame = target.oncePerGame.but(sb)
                game.moonbeastOnSpellbook -= mbRef
                if (owner.allGates.onMap.any) {
                    // Has controlled gates — place at a gate
                    Ask(owner).each(owner.allGates.onMap)(r => MoonbeastReturnChooseGateAction(owner, mbRef, r, MoonbeastDoomReturnAction(self, remaining.tail, then)))
                } else if (owner.allInPlay.%(_.region.onMap).any) {
                    // No gates but has units on map — place in a region with units
                    val unitRegions = owner.allInPlay.%(_.region.onMap)./(_.region).distinct
                    Ask(owner).each(unitRegions)(r => MoonbeastReturnChooseGateAction(owner, mbRef, r, MoonbeastDoomReturnAction(self, remaining.tail, then)))
                } else {
                    // No gates and no units on map — place in any region
                    Ask(owner).each(areas.nex)(r => MoonbeastReturnChooseGateAction(owner, mbRef, r, MoonbeastDoomReturnAction(self, remaining.tail, then)))
                }
            }

        // Moonbeast: initial placement on acquisition (no Power cost)
        case MoonbeastInitialPlaceAction(self, target, sb) =>
            val mb = self.pool(MoonbeastUnit).%(u => !game.moonbeastOnSpellbook.contains(u.ref)).head
            game.moonbeastOnSpellbook = game.moonbeastOnSpellbook + (mb.ref -> (target, sb))
            game.moonbeastPlacedThisDoom += mb.ref
            if (target.spellbooks.has(sb))
                target.oncePerGame :+= sb
            self.log("placed", MoonbeastUnit.styled(self), "onto", sb.styled(target), "of", target.full)
            CheckSpellbooksAction(DoomAction(self))

        // Moonbeast: summon action (costs 2 Power)
        case MoonbeastChooseSpellbookAction(self, target, sb) =>
            self.power -= 2
            val mb = self.pool(MoonbeastUnit).%(u => !game.moonbeastOnSpellbook.contains(u.ref)).head
            game.moonbeastOnSpellbook = game.moonbeastOnSpellbook + (mb.ref -> (target, sb))
            if (target.spellbooks.has(sb))
                target.oncePerGame :+= sb
            self.log("Summoned", MoonbeastUnit.styled(self), "onto", sb.styled(target), "of", target.full, "(blocked)")
            EndAction(self)

        case CronophageTeleportAction(self, houndRef, dest) =>
            val hound = game.unit(houndRef)
            val from = hound.region
            hound.region = dest
            hound.onGate = false
            self.log("Cronophage".styled("nt") + ":", HoundOfTindalos.styled(self), "teleported from", from, "to", dest)
            MoveContinueAction(self, true)

        // SERVITOR OF THE OUTER GODS: assign loyalty card + units to chosen faction
        case ServitorAssignFactionAction(self, target) =>
            // Transfer the loyalty card from self to target
            self.loyaltyCards :-= ServitorCard
            target.loyaltyCards :+= ServitorCard
            // Create the servitor units under the target faction — go to pool, NOT placed on map
            ServitorCard.quantity.times(ServitorUnit).foreach { u =>
                target.units :+= new UnitFigure(target, u, target.units.%(_.uclass == u).num + 1, target.reserve)
            }
            self.log("assigned", ServitorCard.short, "to", target.full)
            target.log("received", ServitorCard.short, "— Servitors in pool, monster summons blocked until pool is empty")
            CheckSpellbooksAction(DoomAction(self))

        case _ => UnknownContinue
    }

    override def triggers()(implicit game : Game) {
        // Moonbeast: if a Moonbeast is on an SBR and the enemy earns that spellbook, auto-suppress it
        game.moonbeastOnSpellbook.foreach { case (mbRef, (target, sb)) =>
            if (target.spellbooks.has(sb) && !target.oncePerGame.has(sb)) {
                target.oncePerGame :+= sb
                log(MoonbeastUnit.styled(game.unit(mbRef).faction), "blocks newly earned", sb.styled(target))
            }
        }

        // Shadow Pharaoh Hebephrenia: SP's presence blocks gate control and pushes cultists off gates
        // Card: "(Yog-Sothoth is unaffected.)"
        factions./~(f => f.allInPlay.%(_.uclass == ShadowPharaoh)).foreach { sp =>
            val r = sp.region
            // Force lose control — all factions (OW's unitGate is separate field, unaffected)
            factions.foreach { gateOwner =>
                if (gateOwner.gates.has(r)) {
                    gateOwner.at(r).%(u => u.uclass != YogSothoth).foreach(_.onGate = false)
                    gateOwner.gates = gateOwner.gates.but(r)
                    gateOwner.log("Hebephrenia".styled("nt") + ":", "gate lost in", r, "due to", ShadowPharaoh.styled(sp.faction))
                }
            }
            // Push any unit off gate — Yog-Sothoth exempt
            factions.foreach { f =>
                f.at(r).%(u => u.onGate && u.uclass != YogSothoth).foreach(_.onGate = false)
            }
        }

        // Hound of Tindalos Angles of Time: eliminate if in gateless area
        factions.foreach { f =>
            f.allInPlay.%(_.uclass == HoundOfTindalos).foreach { h =>
                if (!HoundOfTindalosGates.survives(h.region)) {
                    game.eliminate(h)
                    f.log("Angles of Time".styled("nt") + ":", HoundOfTindalos.styled(f), "eliminated (no Gate in", h.region + ")")
                }
            }
        }

        // Brown Jenkin Familiar: respawn check moved to action flow (BrownJenkinFamiliarPlaceAction)
        // triggers() no longer auto-places — the game loop offers gate choice

        // Mind Parasite: convert/unconvert acolytes based on insect proximity
        MindParasite.checkConversions()
    }
}


// DIMENSIONAL SHAMBLER PLANS
sealed abstract class ShamblerPlan(val label : String) extends Plan {
    val group = "Dimensional Shambler".styled("neutral")
}
case object ShamblerPrompt extends ShamblerPlan("Always prompt") with DefaultPlan with OneOfPlan
case object ShamblerSkip extends ShamblerPlan("Skip, unless...") with OneOfPlan { override val followers = $(ShamblerThreatOfCapture, ShamblerThreatOfAttackOnGate, ShamblerThreatOfAttackOnGOO, ShamblerThreatOfBeyondOne, ShamblerThreatOfLosingBattle, ShamblerThreatOfDreadCurse, ShamblerOpportunityEndOfPhase) }
trait ShamblerThreat extends ShamblerPlan { override val requires = $($(ShamblerSkip)) }
case object ShamblerThreatOfCapture extends ShamblerPlan("...threat of capture") with ShamblerThreat
case object ShamblerThreatOfAttackOnGate extends ShamblerPlan("...credible threat to a controlled gate") with ShamblerThreat
case object ShamblerThreatOfAttackOnGOO extends ShamblerPlan("...credible threat of battle against GOO") with ShamblerThreat
case object ShamblerThreatOfBeyondOne extends ShamblerPlan("...threat of Beyond One against a gate") with ShamblerThreat
case object ShamblerThreatOfLosingBattle extends ShamblerPlan("...credible threat of losing a battle") with ShamblerThreat
case object ShamblerThreatOfDreadCurse extends ShamblerPlan("...credible threat of Dread Curse killing GOO") with ShamblerThreat
case object ShamblerOpportunityEndOfPhase extends ShamblerPlan("...end of Action Phase") with ShamblerThreat

case class ShamblerSummonMainAction(self : Faction) extends OptionFactionAction("Summon " + DimensionalShamblerUnit.styled(self) + " to Faction Card") with MainQuestion with Soft
case class ShamblerSummonAction(self : Faction) extends BaseFactionAction(implicit g => "Summon " + DimensionalShamblerUnit.styled(self) + g.forNPowerWithTax(self.reserve, self, self.summonCost(DimensionalShamblerUnit, self.reserve)), "to " + "Faction Card".styled(self))
case class ShamblerDeployCommandsAction(self : Faction, then : ForcedAction) extends ForcedAction
case class ShamblerDeployPromptAction(self : Faction, then : ForcedAction) extends ForcedAction with Soft
case class ShamblerDeployMainAction(self : Faction, then : ForcedAction) extends OptionFactionAction("Deploy " + DimensionalShamblerUnit.name.styled("neutral")) with MainQuestion with Soft
case class ShamblerDeployAction(self : Faction, r : Region, then : ForcedAction) extends BaseFactionAction("Deploy " + DimensionalShamblerUnit.name.styled("neutral"), implicit g => r + self.iced(r))
