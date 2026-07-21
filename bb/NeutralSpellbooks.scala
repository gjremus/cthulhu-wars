package cws

import hrf.colmat._

import html._


// SPELLBOOK — use .can(), CAN be blocked by Moonbeast
case object MaoCeremony extends NeutralSpellbook("The Mao Ceremony")
// SPELLBOOK — use .can(), CAN be blocked by Moonbeast
case object Recriminations extends NeutralSpellbook("Recriminations")
// SPELLBOOK — use .can(), CAN be blocked by Moonbeast
case object Shriveling extends NeutralSpellbook("Shriveling") with BattleSpellbook
// SPELLBOOK — use .can(), CAN be blocked by Moonbeast
case object StarsAreRight extends NeutralSpellbook("Stars Are Right")
// SPELLBOOK — use .can(), CAN be blocked by Moonbeast
case object UmrAtTawil extends NeutralSpellbook("Umr at-Tawil")
// SPELLBOOK — use .can(), CAN be blocked by Moonbeast
case object Undimensioned extends NeutralSpellbook("Undimensioned")


case class MaoCeremonyAction(self : Faction, r : Region, uc : UnitClass) extends BaseFactionAction(MaoCeremony, uc.styled(self) + " in " + r)
case class MaoCeremonyDoneAction(self : Faction) extends BaseFactionAction(None, "Done")

case class RecriminationsMainAction(self : Faction) extends OptionFactionAction(Recriminations) with MainQuestion with Soft
case class RecriminationsAction(self : Faction, sb : Spellbook) extends BaseFactionAction("Discard spellbook", sb)

case class UndimensionedMainAction(self : Faction) extends OptionFactionAction(Undimensioned) with MainQuestion with Soft
case class UndimensionedContinueAction(self : Faction, destinations : $[Region], moved : Boolean) extends ForcedAction with Soft
case class UndimensionedSelectAction(self : Faction, destinations : $[Region], uc : UnitClass, r : Region) extends BaseFactionAction(g => "" + Undimensioned + " move unit", uc.styled(self) + " from " + r) with Soft
case class UndimensionedAction(self : Faction, destinations : $[Region], uc : UnitClass, r : Region, dest : Region) extends BaseFactionAction(g => "" + Undimensioned + " move " + uc.styled(self) + " from " + r + " to", implicit g => dest + self.iced(dest))
case class UndimensionedDoneAction(self : Faction) extends BaseFactionAction(None, "Done")
case class UndimensionedCancelAction(self : Faction, destinations : $[Region]) extends BaseFactionAction(None, "Cancel") with Cancel


object NeutralSpellbooksExpansion extends Expansion {
    def perform(action : Action, soft : VoidGuard)(implicit game : Game) = action @@ {
        // MAO CEREMONY
        case MaoCeremonyAction(self, r, uc) =>
            val c = self.at(r).one(uc)
            game.eliminate(c)
            self.power += 1
            self.log("sacrificed", c, "in", r, "for", 1.power)

            game.triggers() // game.checkPowerReached()

            game.checkGatesLost()

            AfterPowerGatherAction

        case MaoCeremonyDoneAction(self) =>
            self.ignorePerInstant :+= MaoCeremony

            AfterPowerGatherAction

        // RECRIMINATIONS
        case RecriminationsMainAction(self) =>
            Ask(self).each(self.spellbooks)(b => RecriminationsAction(self, b))

        case RecriminationsAction(self, sb) =>
            self.power -= 1
            self.spellbooks = self.spellbooks.but(sb)

            if (sb.is[NeutralSpellbook])
                game.neutralSpellbooks :+= sb

            self.log("discarded", sb)

            self.ignorePerInstant :+= sb

            // Moonbeast: if a moonbeast was blocking this spellbook, return it to map
            val moonbeastEntries = game.moonbeastOnSpellbook.filter { case (_, (target, blockedSB)) => target == self && blockedSB == sb }.toList
            if (moonbeastEntries.any) {
                moonbeastEntries.foreach { case (mbRef, _) =>
                    self.oncePerGame = self.oncePerGame.but(sb)
                    game.moonbeastOnSpellbook -= mbRef
                }
                MoonbeastDoomReturnAction(self, moonbeastEntries, EndAction(self))
            } else
                EndAction(self)

        // UNDIMENSIONED
        // BB Fix 77, v2.4.30 — Undimensioned may target Moon only if faction has a unit there
        case UndimensionedMainAction(self) =>
            val mapDestinations = self.units.onMap./(_.region).distinct
            val moonDestination = self.units.%(_.region == BB.moon).any.$(BB.moon)
            UndimensionedContinueAction(self, mapDestinations ++ moonDestination, false)

        case UndimensionedContinueAction(self, destinations, moved) =>
            // BB Fix 77, v2.4.30 — include Moon-resident units in the rearrange
            // pool when Moon is in the destinations set (i.e., faction has a
            // unit on the Moon). Without the inclusion the player could move
            // off-Moon units onto the Moon but not move a Moon-resident unit
            // back to a Map area, which would violate the "rearrange among
            // your areas" rule.
            val pool = if (destinations.has(BB.moon)) self.units.nex.%(_.region.inPlay).not(Moved) else self.units.nex.onMap.not(Moved)
            val units = pool.%(u => destinations.but(u.region).%(self.affords(self.units.onMap.tag(Moved).none.??(2))).any).sortA
            if (units.none)
                Then(UndimensionedDoneAction(self))
            else
            if (moved)
                Ask(self).add(UndimensionedDoneAction(self)).each(units)(u => UndimensionedSelectAction(u.faction, destinations, u.uclass, u.region))
            else
                Ask(self).each(units)(u => UndimensionedSelectAction(u.faction, destinations, u.uclass, u.region)).cancel

        case UndimensionedSelectAction(self, destinations, uc, r) =>
            val options = destinations.but(r).%(self.affords(self.units.onMap.tag(Moved).none.??(2)))./(d => UndimensionedAction(self, destinations, uc, r, d))

            if (self.units.onMap.tag(Moved).any)
                Ask(self).list(options).add(UndimensionedCancelAction(self, destinations))
            else
                Ask(self).list(options).cancel

        case UndimensionedDoneAction(self) =>
            self.units.foreach(_.remove(Moved))
            EndAction(self)

        case UndimensionedAction(self, destinations, uc, o, r) =>
            if (self.units.onMap.tag(Moved).none) {
                self.log("units are", Undimensioned)
                self.power -= 2
            }

            self.payTax(r)

            val u = self.at(o, uc).not(Moved).first
            // BB Fix 77, v2.4.30 — set undimensionedInPlay flag while writing
            // u.region so the Moon-entry place() guard permits rearranges that
            // target BB.moon. The direct u.region = r assignment bypasses
            // place(), but the flag is defense-in-depth for any future place()
            // route through Undimensioned.
            game.undimensionedInPlay = true
            u.region = r
            game.undimensionedInPlay = false
            u.add(Moved)

            log(uc.styled(self), "from", o, "is now in", r)

            UndimensionedContinueAction(self, destinations, true)

        case UndimensionedCancelAction(self, destinations) =>
            UndimensionedContinueAction(self, destinations, true)

        // ...
        case _ => UnknownContinue
    }
}
