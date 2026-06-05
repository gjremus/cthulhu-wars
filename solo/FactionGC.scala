package cws

import hrf.colmat._

import html._


case object DeepOne extends FactionUnitClass(GC, "Deep One", Monster, 1)
case object Shoggoth extends FactionUnitClass(GC, "Shoggoth", Monster, 2)
case object Starspawn extends FactionUnitClass(GC, "Starspawn", Monster, 3)
case object Cthulhu extends FactionUnitClass(GC, "Cthulhu", GOO, 4)


// GOO UNIQUE POWER — use .has(), blocked by Elder Thing, NOT by Moonbeast
case object Devour extends FactionSpellbook(GC, "Devour") with BattleSpellbook
// FACTION POWER — use .has(), NOT blocked by Moonbeast or Elder Thing
case object Immortal extends FactionSpellbook(GC, "Immortal")

// SPELLBOOK — use .can(), CAN be blocked by Moonbeast
case object Absorb extends FactionSpellbook(GC, "Absorb") with BattleSpellbook
// SPELLBOOK — use .can(), CAN be blocked by Moonbeast
case object Regenerate extends FactionSpellbook(GC, "Regenerate") with BattleSpellbook
// SPELLBOOK — use .can(), CAN be blocked by Moonbeast
case object Dreams extends FactionSpellbook(GC, "Dreams")
// SPELLBOOK — use .can(), CAN be blocked by Moonbeast
case object Devolve extends FactionSpellbook(GC, "Devolve")
// SPELLBOOK — use .can(), CAN be blocked by Moonbeast
case object YhaNthlei extends FactionSpellbook(GC, "Y'ha Nthlei")
// SPELLBOOK — use .can(), CAN be blocked by Moonbeast
case object Submerge extends FactionSpellbook(GC, "Submerge")


case object FirstDoomPhase extends Requirement("First Doom Phase", 1)
case object KillDevour1 extends Requirement("Kill/Devour 1 enemy unit")
case object KillDevour2 extends Requirement("Kill/Devour 2 enemy units")
case object AwakenCthulhu extends Requirement("Awaken Cthulhu")
case object OceanGates extends Requirement("Ocean gates")
case object FiveSpellbooks extends Requirement("Five spellbooks", 1)


case object GC extends Faction { f =>
    def name = "Great Cthulhu"
    def short = "GC"
    def style = "gc"

    val deep = Deep(f)

    override def abilities : $[Spellbook] = $(Immortal, Devour)
    override def library : $[Spellbook] = $(Devolve, Absorb, Regenerate, Dreams, YhaNthlei, Submerge)
    override def requirements(options : $[GameOption]) = $(FirstDoomPhase, KillDevour1, KillDevour2, AwakenCthulhu, OceanGates, FiveSpellbooks)

    val allUnits =
        1.times(Cthulhu) ++
        2.times(Starspawn) ++
        2.times(Shoggoth) ++
        4.times(DeepOne) ++
        6.times(Acolyte)

    override def awakenCost(u : UnitClass, r : Region)(implicit game : Game) = u match {
        case Cthulhu => (r == game.starting(f) && game.gates.has(r)).?(f.needs(AwakenCthulhu).?(10).|(4))
        case _ => None
    }

    override def awakenDesc(u : UnitClass) : |[String] = None

    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int =
        units(DeepOne).num * 1 +
        units(Shoggoth).num * 2 + units./(_.count(Absorbed)).sum * 3 +
        units(Starspawn).num * 3 +
        units(Cthulhu).not(Zeroed).num * 6 +
        neutralStrength(units, opponent)
}

case class DevolveCommandsAction(self : GC, then : ForcedAction) extends ForcedAction

case class DevolvePromptAction(self : GC, then : ForcedAction) extends ForcedAction with Soft
case class DevolveMainAction(self : GC, then : ForcedAction) extends OptionFactionAction(Devolve) with MainQuestion with Soft
case class DevolveAction(self : GC, r : Region, then : ForcedAction) extends BaseFactionAction(Devolve, Acolyte.styled(self) + " in " + r)

case class DreamsMainAction(self : GC, l : $[Region]) extends OptionFactionAction(Dreams) with MainQuestion with Soft
case class DreamsAction(self : GC, r : Region, f : Faction) extends BaseFactionAction(Dreams, implicit g => Acolyte.styled(f) + " in " + r + self.iced(r))
case class DreamsTargetAction(self : Faction, f : GC, r : Region, u : UnitRef) extends ForcedAction

case class SubmergeMainAction(self : GC, r : Region) extends OptionFactionAction(Submerge) with MainQuestion
case class SubmergeAction(self : GC, r : Region, uc : UnitClass) extends BaseFactionAction("" + Submerge + " with " + Cthulhu.styled(self) + " from " + r, uc.styled(self))
case class SubmergeDoneAction(self : GC, r : Region) extends BaseFactionAction(None, "Done".styled("power"))

case class UnsubmergeMainAction(self : GC, l : $[Region]) extends OptionFactionAction("Unsubmerge".styled(self)) with MainQuestion with Soft
case class UnsubmergeAction(self : GC, r : Region) extends BaseFactionAction("Unsubmerge".styled(self), implicit g => r + self.iced(r))


object GCExpansion extends Expansion {
    override def triggers()(implicit game : Game) {
        val f = GC
        f.satisfyIf(OceanGates, "Control three Gates in Ocean areas", f.gates.%(_.glyph == Ocean).num >= 3)
        f.satisfyIf(OceanGates, "Four Gates exist in Ocean areas", game.allGates.%(_.glyph == Ocean).num >= 4)
    }

    def perform(action : Action, soft : VoidGuard)(implicit game : Game) = action @@ {
        // ACTIONS
        case MainAction(f : GC) if f.active.not =>
            UnknownContinue

        case MainAction(f : GC) if f.acted =>
            implicit val asking = Asking(f)

            game.controls(f)

            if (f.hasAllSB)
                game.battles(f)

            if (f.can(Devolve) && f.all(Acolyte).any && f.pool(DeepOne).any)
                + DevolveMainAction(f, MainAction(f))

            game.reveals(f)

            game.endTurn(f)(true)

            asking

        case MainAction(f : GC) =>
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

            if (f.can(Dreams) && f.pool(Acolyte).any)
                areas.%(f.affords(2)).%(r => f.enemies.%(e => e.at(r).%(_.targetableAsCultistByEnemy).any).any).some.foreach { l =>
                    + DreamsMainAction(f, l)
                }

            if (f.can(Submerge) && f.has(Cthulhu) && f.goo(Cthulhu).region.glyph == Ocean && f.power >= 1)
                + SubmergeMainAction(f, f.goo(Cthulhu).region)

            if (f.at(GC.deep).any)
                // BB Bullet 50 (v2.4.29): GC may unsubmerge onto the Moon.
                // The Moon is adjacent to all regions for arrival purposes;
                // GC's submerged court resides off-map and may surface
                // anywhere. Append BB.moon to the destination list when GC
                // can afford it. The Moon-entry place() guard (Game.scala)
                // is extended in parallel with a GC-unsubmerge exception
                // via the transient game.gcInUnsubmerge flag (defense-in-
                // depth — UnsubmergeAction assigns u.region = r directly,
                // bypassing place(), but the flag covers any future place()
                // route through Unsubmerge).
                (areas.%(f.affords(0)) ++ f.affords(0)(BB.moon).??($(BB.moon))).some.foreach { l =>
                    + UnsubmergeMainAction(f, l)
                }

            if (f.can(Devolve) && f.all(Acolyte).any && f.pool(DeepOne).any)
                + DevolveMainAction(f, MainAction(f))

            game.neutralSpellbooks(f)

            game.libraryActions(f)

            game.highPriests(f)

            game.reveals(f)

            game.endTurn(f)(f.battled.any)

            asking

        // AWAKEN
        case AwakenedAction(self, Cthulhu, r, cost) =>
            if (self.has(Immortal)) {
                self.log("gained", 1.es, "as", Immortal)
                self.takeES(1)
            }

            self.satisfy(AwakenCthulhu, "Awaken Cthulhu")

            EndAction(self)

        // DEVOLVE
        case DevolvePromptAction(f, then) =>
            Force(DevolveMainAction(f, then))

        case DevolveMainAction(f, then) =>
            Ask(f).some(areas)(r => f.at(r, Acolyte)./(c => DevolveAction(f, c.region, then))).cancel

        case DevolveAction(f, r, then) =>
            if (f.at(r, Monster, GOO).none)
                f.enemies.%(_.at(r, Monster, GOO).none).%(_.at(r, Cultist).any).foreach(_.oncePerAction :-= Devolve)

            val c = f.at(r).one(Acolyte)
            game.eliminate(c)
            f.place(DeepOne, r)

            log(c, "in", r, "devolved into", DeepOne)

            game.checkGatesLost()

            then

        case SpellbookAction(_, _, DevolveCommandsAction(_, _)) =>
            UnknownContinue

        case SpellbookAction(GC, Devolve, then) =>
            Force(SpellbookAction(GC, Devolve, DevolveCommandsAction(GC, then)))

        case DevolveCommandsAction(f, then) =>
            f.plans ++= $(
                DevolvePrompt,
                DevolveSkip,
                DevolveThreatOfCapture,
            ) ++
            game.setup.has(YS).$(DevolveThreatOfZingaya) ++
            game.setup.has(OW).$(DevolveThreatOfBeyondOne) ++
            $(DevolveThreatOfAttackOnGate) ++
            $(DevolveThreatOfAttackOnGOO)

            if (options.has(QuickGame)) {
                f.commands :+= DevolveSkip
                f.commands :+= DevolveThreatOfCapture
            }
            else
                f.commands :+= DevolvePrompt

            then

        // DREAMS
        case DreamsMainAction(f, l) =>
            Ask(f)
                .some(l)(r => f.enemies./~(e => e.at(r).%(_.targetableAsCultistByEnemy).take(1))./(c => DreamsAction(f, c.region, c.faction)))
                .cancel

        case DreamsAction(f, r, e) =>
            f.power -= 2
            f.payTax(r)
            f.log("sent dreams to", e, "in", r)

            val l = e.at(r).%(_.targetableAsCultistByEnemy).preferablyNotOnGate

            Ask(e).each(l)(u => DreamsTargetAction(e, f, r, u).as(u.ref.full)(f, "sent", Dreams, "to", r))

        case DreamsTargetAction(f, e, r, u) =>
            e.log("replaced", u, "with", Acolyte.styled(e))

            game.eliminate(u)
            e.place(Acolyte, r)

            EndAction(e)

        // SUBMERGE
        case SubmergeMainAction(f, r) =>
            f.power -= 1
            Force(SubmergeAction(f, r, Cthulhu))

        case SubmergeAction(f, r, uc) =>
            if (f.at(r).%(_.uclass == uc).none) {
                EndAction(f)
            } else {
                val u = f.at(r).one(uc)
                u.region = GC.deep
                u.onGate = false
                Ask(f).each(f.at(r).%(_.canMove))(u => SubmergeAction(f, r, u.uclass)).add(SubmergeDoneAction(f, r))
            }

        case SubmergeDoneAction(f, r) =>
            val cthulu = f.at(GC.deep).one(Cthulhu)
            val court = f.at(GC.deep).but(cthulu)
            log(cthulu, "submerged in", r, court.any.??("with " + court.mkString(", ")))
            EndAction(f)

        case UnsubmergeMainAction(f, l) =>
            Ask(f).each(l)(r => UnsubmergeAction(f, r)).cancel

        case UnsubmergeAction(f, r) =>
            f.payTax(r)
            val cthulu = f.at(GC.deep).one(Cthulhu)
            val court = f.at(GC.deep).but(cthulu)
            // BB Bullet 50 (v2.4.29): set the GC-unsubmerge transient flag
            // while Cthulhu and his court are surfaced to the destination.
            // This grants the Moon-entry place() guard (Game.scala) an
            // exception when r == BB.moon. The direct u.region = r
            // assignments below bypass place(), so the flag is primarily
            // defense-in-depth for any future place() route through this
            // action.
            game.gcInUnsubmerge = true
            f.at(GC.deep).foreach(_.region = r)
            game.gcInUnsubmerge = false
            log(cthulu, "unsubmerged in", r, court.any.??("with " + court.mkString(", ")))
            EndAction(f)

        // ...
        case _ => UnknownContinue
    }
}
