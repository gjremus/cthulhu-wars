package cws

import hrf.colmat._
import html._

// ============================================================================
// SPHINX WORSHIPPERS (SW) — Homebrew designer faction
// Designer ID: lhhz3sjhivcz, owner: cavysupreme
// Faction color #b8abd5, style "sw".
// Egyptian-themed faction centered on the Hunger track (alternate economy) and
// Black Sphinx (Elder God), Queen Nitokris (Terror), kidnapped enemy monsters.
// Every fixed number below comes from SWValues (solo/faction-values/sw.json).
// ============================================================================


// ── UNITS ───────────────────────────────────────────────────────────────────
// Acolyte is the shared Game.scala case object (cost 1) — no new unit needed for it.
case object HybridMummy    extends FactionUnitClass(SW, "Hybrid Mummy",    Monster,  SWValues.HybridMummy_cost)
case object PharoahKephren extends FactionUnitClass(SW, "Pharoah Kephren", Monster,  SWValues.PharoahKephren_cost)
case object QueenNitokris  extends FactionUnitClass(SW, "Queen Nitokris",  Terror,   SWValues.QueenNitokris_cost)
case object BlackSphinx    extends FactionUnitClass(SW, "Black Sphinx",    ElderGod, SWValues.BlackSphinx_awakenBaseCost) {
    // "Black Sphinx cannot be moved by any means" (design, awakenReq step 3).
    override def canMove(u : UnitFigure)(implicit game : Game) : Boolean = false
    override def canBeMoved(u : UnitFigure)(implicit game : Game) : Boolean = false
}


// ── UNIQUE FACTION ABILITY ──────────────────────────────────────────────────
case object Kidnap extends FactionSpellbook(SW, "Kidnap")


// ── SPELLBOOKS ──────────────────────────────────────────────────────────────
case object FuneralProcession extends FactionSpellbook(SW, "Funeral Procession")
case object BottomlessPit     extends FactionSpellbook(SW, "Bottomless Pit")
case object AncientEmetic     extends FactionSpellbook(SW, "Ancient Emetic")
case object MonstrousForepaw  extends FactionSpellbook(SW, "Monstrous Forepaw")
case object Starvation        extends FactionSpellbook(SW, "Starvation")
// SB6 is ONE dual-power spellbook: Mummies Curse (Post-Battle) / Life from Death (Pre-Battle).
// Modelled as a single FactionSpellbook (one SBR earns both sides); the two abilities are two
// separate actions both gated on SW.can(MummiesCurse), so a Moonbeast on this book blocks both,
// exactly as the design requires. Same-battle mutual exclusion is enforced via
// game.swLifeFromDeathUsedThisBattle (set by Life from Death, checked by Mummies Curse, cleared
// at battle end in Battle.scala).
case object MummiesCurse      extends FactionSpellbook(SW, "Mummies Curse")


// ── SPELLBOOK REQUIREMENTS ──────────────────────────────────────────────────
case object KidnapReq        extends Requirement("Kidnap a monster")
case object BottomlessPitReq extends Requirement("Bottomless Pit is performed")
case object HungerReq2       extends Requirement("The Hunger Track reaches " + SWValues.HungerReq2_threshold)
case object HungerReq4       extends Requirement("The Hunger Track reaches " + SWValues.HungerReq4_threshold)
case object NitokrisReq      extends Requirement("Nitokris is killed or eliminated")
case object AwakenSphinxReq  extends Requirement("Awaken Black Sphinx")


// ============================================================================
// SPHINX WORSHIPPERS (SW) FACTION OBJECT
// ============================================================================
case object SW extends Faction { f =>
    def name  = "Sphinx Worshippers"
    def short = "SW"
    def style = "sw"

    override def abilities = $(Kidnap)
    override def library   = $(FuneralProcession, BottomlessPit, AncientEmetic, MonstrousForepaw, Starvation, MummiesCurse)
    override def requirements(options : $[GameOption]) =
        $(KidnapReq, BottomlessPitReq, HungerReq2, HungerReq4, NitokrisReq, AwakenSphinxReq)

    val allUnits =
        SWValues.Acolyte_count.times(Acolyte)          ++
        SWValues.HybridMummy_count.times(HybridMummy)  ++
        SWValues.PharoahKephren_count.times(PharoahKephren) ++
        SWValues.QueenNitokris_count.times(QueenNitokris)   ++
        SWValues.BlackSphinx_count.times(BlackSphinx)

    // Black Sphinx's awaken cost depends on WHICH monster is sacrificed (8 minus that
    // monster's printed cost, minus 3 more if it was Kidnapped) — too dynamic for the plain
    // awakenCost(uc, r) : |[Int] hook (one flat number per region). Returning None here keeps
    // Black Sphinx OFF the generic Awaken menu; SWExpansion's own SWAwakenSphinxMainAction
    // (offered from MainAction) drives the whole flow instead — same idiom as CS's Tulzscha
    // comment ("the sacrifice... injected by CSExpansion's custom AwakenAction handler"), just
    // one step further since here even the COST is per-target, not just the sacrifice.
    override def awakenCost(u : UnitClass, r : Region)(implicit game : Game) : |[Int] = None

    // Hybrid Mummy / Pharoah Kephren roll dice (combat read from SWValues, same combatOf
    // idiom as FMExpansion.combatOf — combat is NOT always equal to cost, so it must be read
    // separately rather than reusing .uclass.cost). Queen Nitokris and Black Sphinx roll NO
    // dice — they add automatic Pains in Battle.scala's BattleRollAction (same idiom as
    // Hagarg Ryonis), so they contribute 0 here (consistent with Hagarg Ryonis's "* 0" line
    // in neutralStrength).
    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int = {
        units.%(u => u.uclass == HybridMummy || u.uclass == PharoahKephren).not(Zeroed)./(u => SWExpansion.combatOf(u.uclass)).sum +
        neutralStrength(units, opponent)
    }
}


// ── ACTION CASE CLASSES ─────────────────────────────────────────────────────

// Awaken Black Sphinx (custom flow — see awakenCost override above).
case class SWAwakenSphinxMainAction(self : Faction) extends OptionFactionAction(("Awaken " + BlackSphinx.name).styled(SW)) with MainQuestion with Soft
case class SWAwakenSphinxRegionAction(self : Faction, r : Region) extends BaseFactionAction(("Awaken " + BlackSphinx.name).styled(SW) + " in", implicit g => r + self.iced(r)) with Soft
case class SWAwakenSphinxSacrificeAction(self : Faction, r : Region, victim : UnitRef)
    extends BaseFactionAction(implicit g => "Sacrifice " + g.unit(victim).uclass.styled(SW) + " to awaken " + BlackSphinx.name.styled(SW), r)

// Kidnap (UFA, Post-Battle — the Ask(...) is built directly in Battle.scala's post-battle block).
case class SWKidnapAction(self : Faction, enemy : Faction, victim : UnitRef)
    extends OptionFactionAction(implicit g => "Kidnap " + g.unit(victim).uclass.styled(g.unit(victim).faction)) with PowerNeutral {
    def question(implicit game : Game) = Kidnap.styled(SW)
}
case class SWKidnapSkipAction(self : Faction, enemy : Faction) extends OptionFactionAction("Skip") with PowerNeutral {
    def question(implicit game : Game) = Kidnap.styled(SW)
}

// Bottomless Pit — SW's own baseline use AND (once earned) any faction's cross-faction use
// (offered from Game.neutralSpellbooks, same idiom as CS's Effulgent Sacrifice).
case class SWBottomlessPitMainAction(self : Faction, l : $[Region]) extends OptionFactionAction(BottomlessPit.styled(SW)) with MainQuestion with Soft
case class SWBottomlessPitRegionPickAction(self : Faction, r : Region) extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) = BottomlessPit.styled(SW)
}
case class SWBottomlessPitAction(self : Faction, r : Region, victim : UnitRef)
    extends BaseFactionAction(implicit g => BottomlessPit.styled(SW) + ": eliminate " + g.unit(victim).uclass.styled(g.unit(victim).faction), r) with PowerNeutral

// Ancient Emetic — pick a Monster/Terror from the pool (affordable at a 1-power discount),
// then a region with an SW-controlled gate to summon it into; advances Hunger by 1.
case class SWAncientEmeticMainAction(self : Faction, choices : $[UnitClass]) extends OptionFactionAction(AncientEmetic.styled(SW)) with MainQuestion with Soft
case class SWAncientEmeticUnitAction(self : Faction, uc : UnitClass) extends BaseFactionAction(implicit g => AncientEmetic.styled(SW) + ": summon", uc.styled(SW))
case class SWAncientEmeticRegionAction(self : Faction, uc : UnitClass, r : Region)
    extends BaseFactionAction(implicit g => {
        val cost = math.max(0, uc.cost - SWValues.AncientEmetic_costDiscount)
        AncientEmetic.styled(SW) + ": summon " + uc.styled(SW) + g.forNPowerWithTax(r, self, cost) + " in"
    }, implicit g => r + self.iced(r))

// Starvation (Doom Phase ritual discount — simplified standalone action, see FactionSW notes
// in SWExpansion.perform near the Doom Phase section).
case class SWStarvationOfferAction(self : Faction) extends ForcedAction
case class SWStarvationUseAction(self : Faction) extends OptionFactionAction("Use " + Starvation.name.styled(SW)) with PowerNeutral {
    def question(implicit game : Game) = Starvation.styled(SW)
}
case class SWStarvationSkipAction(self : Faction) extends OptionFactionAction("Skip") with PowerNeutral {
    def question(implicit game : Game) = Starvation.styled(SW)
}

// Mummies Curse (Post-Battle) / Life from Death (Pre-Battle) — one dual-power SB.
case class SWMummiesCurseAction(self : Faction, mummy : UnitRef)
    extends OptionFactionAction(implicit g => "Eliminate " + HybridMummy.styled(SW) + " (+" + SWValues.MummiesCurse_pains + " Pains)") with PowerNeutral {
    def question(implicit game : Game) = "Mummies Curse".styled(SW)
}
case class SWMummiesCurseSkipAction(self : Faction) extends OptionFactionAction("Skip") with PowerNeutral {
    def question(implicit game : Game) = "Mummies Curse".styled(SW)
}
case class SWLifeFromDeathMainAction(self : Faction) extends OptionFactionAction("Life from Death".styled(SW)) with PreBattleQuestion with Soft
case class SWLifeFromDeathAction(self : Faction, mummy : UnitRef)
    extends OptionFactionAction(implicit g => "Replace " + HybridMummy.styled(SW) + " with " + SWValues.LifeFromDeath_cultists + " " + Acolyte.plural) with PowerNeutral {
    def question(implicit game : Game) = "Life from Death".styled(SW)
}
case class SWLifeFromDeathSkipAction(self : Faction) extends OptionFactionAction("Skip") with PowerNeutral {
    def question(implicit game : Game) = "Life from Death".styled(SW)
}

// Funeral Procession — free carry of Hybrid Mummies / Kidnapped units alongside a Nitokris move.
case class SWCarryOfferAction(o : Region, to : Region) extends ForcedAction
case class SWCarryAction(o : Region, ur : UnitRef, to : Region) extends ForcedAction
case class SWCarryDoneAction(o : Region, to : Region) extends OptionFactionAction("Done") with PowerNeutral {
    def self = SW
    def question(implicit game : Game) = "Funeral Procession".styled(SW)
}

// Doom Phase — forced Hunger sacrifice (Doom and/or Cultists, paid 1 at a time until the
// obligation is met or SW runs out of both to pay with).
case class SWHungerSacrificeResolveAction(remaining : Int) extends ForcedAction
case class SWHungerSacrificeDoomAction(remaining : Int) extends OptionFactionAction("Lose 1 Doom") with PowerNeutral {
    def self = SW
    def question(implicit game : Game) = "Ceaseless Hunger".styled(SW)
}
case class SWHungerSacrificeCultistAction(remaining : Int, ref : UnitRef)
    extends OptionFactionAction(implicit g => "Sacrifice " + Acolyte.styled(SW) + " in " + g.unit(ref).region) with PowerNeutral {
    def self = SW
    def question(implicit game : Game) = "Ceaseless Hunger".styled(SW)
}


// ============================================================================
// SPHINX WORSHIPPERS (SW) EXPANSION
// ============================================================================
object SWExpansion extends Expansion {

    // "Ceaseless Hunger" (Black Sphinx's Ongoing ability) can be FULLY blocked by an enemy-
    // controlled Elder Thing sharing Black Sphinx's region (ability interaction #2). This
    // gate is checked everywhere Hunger would change due to Ceaseless Hunger (awaken, pained,
    // killed/eliminated, start of Action Phase) — the automatic-Pains part is gated the same
    // way directly in Battle.scala's BattleRollAction.
    // Combat dice per unit type, read from SWValues (faction-values/sw.json) so combat can be
    // changed separately from cost — same idiom as FMExpansion.combatOf.
    def combatOf(uc : UnitClass) : Int = uc match {
        case HybridMummy    => SWValues.HybridMummy_combat
        case PharoahKephren => SWValues.PharoahKephren_combat
        case other          => other.cost
    }

    def ceaselessHungerActive(implicit game : Game) : Boolean =
        SW.onMap(BlackSphinx).not(Zeroed).headOption.map(bs => !ElderThingMindControl.suppresses(bs)).getOrElse(true)

    // Plain Hunger change (Bottomless Pit, Starvation) — NOT gated by Elder Thing, since that
    // only blocks "Ceaseless Hunger" (Black Sphinx's own Ongoing ability), not every Hunger
    // change in the faction.
    def changeHunger(n : Int, why : String)(implicit game : Game) {
        if (n != 0) {
            game.swHunger = math.max(0, game.swHunger + n)
            SW.log("Hunger " + (n > 0).?("+").|("") + n + " (" + why + "), now " + game.swHunger)
            SW.satisfyIf(HungerReq2, HungerReq2.text, game.swHunger >= SWValues.HungerReq2_threshold)
            SW.satisfyIf(HungerReq4, HungerReq4.text, game.swHunger >= SWValues.HungerReq4_threshold)
        }
    }

    // "Ceaseless Hunger" Hunger gain (Black Sphinx awaken/pained/killed/start of Action Phase)
    // — fully blocked by an enemy-controlled Elder Thing sharing Black Sphinx's region.
    def gainHunger(n : Int, why : String)(implicit game : Game) {
        if (ceaselessHungerActive) changeHunger(n, why)
    }

    // Regions where Bottomless Pit can be performed: Black Sphinx's own region, plus (once
    // Monstrous Forepaw is earned) regions adjacent to it.
    def bottomlessPitRegions(implicit game : Game) : $[Region] = {
        val sphinxRegions = SW.onMap(BlackSphinx).not(Zeroed)./(_.region).distinct
        (sphinxRegions ++ (SW.can(MonstrousForepaw).??(sphinxRegions./~(game.board.connected)))).distinct
    }

    override def triggers()(implicit game : Game) {
        if (!game.setup.has(SW)) return
        SW.satisfyIf(NitokrisReq, NitokrisReq.text, game.swNitokrisEliminated)
    }

    def perform(action : Action, soft : VoidGuard)(implicit game : Game) = action @@ {

        // ────────────────────────────────────────────────────────────────
        // SETUP — 6 Acolytes + Controlled Gate are placed by the generic
        // StartingRegionAction handler in Game.scala (same as every base faction);
        // no SW-specific setup code is needed.
        // ────────────────────────────────────────────────────────────────

        // ────────────────────────────────────────────────────────────────
        // START OF ACTION PHASE — Hunger +1 while Black Sphinx is on the map.
        // ────────────────────────────────────────────────────────────────
        case ActionPhaseAction if game.setup.has(SW) && SW.onMap(BlackSphinx).not(Zeroed).any =>
            gainHunger(SWValues.Hunger_actionPhaseGain, "start of Action Phase, " + BlackSphinx.name + " on map")
            UnknownContinue

        // ────────────────────────────────────────────────────────────────
        // DOOM PHASE — forced Hunger sacrifice, then an optional Starvation step, then the
        // normal doom flow. Same re-arm idiom as TI's Sacrament of Flesh: intercept
        // DoomAction(SW) once per Doom Phase (guarded by swHungerSacrificeDoneThisDoom),
        // resolve, then Force DoomAction(SW) again — the guard is now false so this case no
        // longer matches and Game.scala's normal DoomAction(f) flow runs.
        // ────────────────────────────────────────────────────────────────
        case DoomAction(f : SW.type) if !game.swHungerSacrificeDoneThisDoom =>
            game.swHungerSacrificeDoneThisDoom = true
            if (game.swHunger > 0) {
                f.log("Hunger Track at " + game.swHunger + ": must lose Doom and/or sacrifice Cultists totalling " + game.swHunger)
                Force(SWHungerSacrificeResolveAction(game.swHunger))
            } else
                Force(SWStarvationOfferAction(f))

        case SWHungerSacrificeResolveAction(remaining) =>
            if (remaining <= 0)
                Force(SWStarvationOfferAction(SW))
            else {
                implicit val asking = Asking(SW)
                if (SW.doom > 0)
                    + SWHungerSacrificeDoomAction(remaining)
                SW.onMap(Acolyte).not(Zeroed).headOption.foreach(u => + SWHungerSacrificeCultistAction(remaining, u.ref))
                if (asking.ask.actions.none) {
                    // Nothing left to pay with (no Doom, no Cultists on map) — obligation lapses.
                    SW.log("Hunger sacrifice: nothing left to pay with, obligation lapses")
                    Force(SWStarvationOfferAction(SW))
                } else
                    asking // MANDATORY — no cancel, mirrors TISacramentResolveAction
            }

        case SWHungerSacrificeDoomAction(remaining) =>
            SW.doom -= 1
            SW.log("Hunger sacrifice: lost", 1.doom)
            Force(SWHungerSacrificeResolveAction(remaining - 1))

        case SWHungerSacrificeCultistAction(remaining, ref) =>
            val u = game.unit(ref)
            SW.log("Hunger sacrifice: sacrificed", Acolyte.styled(SW), "in", u.region)
            game.eliminate(u)
            Force(SWHungerSacrificeResolveAction(remaining - 1))

        case SWStarvationOfferAction(self) =>
            if (self.can(Starvation) && game.swHunger > 0)
                Ask(self).add(SWStarvationUseAction(self)).add(SWStarvationSkipAction(self))
            else
                Force(DoomAction(self))

        case SWStarvationUseAction(self) =>
            val gain = game.swHunger / SWValues.Starvation_esDivisor
            if (gain > 0) {
                self.es ++= gain.times(ElderSign(0))
                self.log(Starvation.styled(SW) + ": gained", gain.es)
            }
            changeHunger(SWValues.Hunger_discountedRitualGain, "Starvation used")
            Force(DoomAction(self))

        case SWStarvationSkipAction(self) =>
            Force(DoomAction(self))

        // ────────────────────────────────────────────────────────────────
        // MAIN ACTION
        // ────────────────────────────────────────────────────────────────
        case MainAction(f : SW.type) if f.active.not =>
            UnknownContinue

        case MainAction(f : SW.type) if f.acted =>
            UnknownContinue

        case MainAction(f : SW.type) =>
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

            val sphinxRegions = game.board.regions.%(r => f.gates.has(r) && f.pool.%(_.uclass == BlackSphinx).any &&
                f.at(r).%(_.uclass.utype == Monster).not(Zeroed).any)
            // Button gating: Awaken Black Sphinx cost is dynamic (8 - victim.cost - kidnapDiscount),
            // minimum 0. Only offer button if there's a region with a gate, Black Sphinx in pool,
            // and at least one monster available. The specific affordability check happens when the
            // player selects which monster to sacrifice (SWAwakenSphinxRegionAction). This staged
            // gating is acceptable because the cost is too dynamic to pre-calculate efficiently.
            if (sphinxRegions.any)
                + SWAwakenSphinxMainAction(f)

            val pitRegions = bottomlessPitRegions.%(r => game.factions.but(f).exists(e => e.at(r).%(_.uclass.utype == Monster).not(Zeroed).any))
            if (game.swHunger > 0 && f.power >= SWValues.BottomlessPit_cost && pitRegions.any)
                + SWBottomlessPitMainAction(f, pitRegions)

            // Button gating: Ancient Emetic's summon cost is dynamic (uc.cost - 1 discount, min
            // 0), so only pre-filter by whether SW has a controlled gate at all and whether at
            // least one pool Monster/Terror is affordable at that discount.
            if (f.can(AncientEmetic) && f.gates.any) {
                val choices = f.pool.monsterly./(_.uclass).distinct.%(uc => f.canSpend(math.max(0, uc.cost - SWValues.AncientEmetic_costDiscount)))
                if (choices.any)
                    + SWAncientEmeticMainAction(f, choices)
            }

            game.neutralSpellbooks(f)
            game.libraryActions(f)
            game.highPriests(f)
            game.reveals(f)

            game.endTurn(f)(f.battled.any || game.nexed.any)

            asking

        case SWAwakenSphinxMainAction(self) =>
            val regions = game.board.regions.%(r => self.gates.has(r) && self.pool.%(_.uclass == BlackSphinx).any &&
                self.at(r).%(_.uclass.utype == Monster).not(Zeroed).any)
            if (regions.isEmpty)
                EndAction(self)
            else if (regions.num == 1)
                Force(SWAwakenSphinxRegionAction(self, regions.head))
            else
                Ask(self).each(regions)(r => SWAwakenSphinxRegionAction(self, r)).cancel

        case SWAwakenSphinxRegionAction(self, r) =>
            // Button gating: only show sacrifice options where the faction can afford the awakening cost.
            // Cost = 8 - sacrificedMonster.cost - (3 if kidnapped), minimum 0. If no affordable options,
            // the action ends without offering a menu.
            val victims = self.at(r).%(_.uclass.utype == Monster).not(Zeroed)
            val affordableVictims = victims.%(v => {
                val kidnapDiscount = game.swKidnapped.has(v.ref).??(SWValues.BlackSphinx_kidnapDiscount)
                val cost = math.max(0, SWValues.BlackSphinx_awakenBaseCost - v.uclass.cost - kidnapDiscount)
                self.canSpend(cost)
            })
            if (affordableVictims.isEmpty)
                EndAction(self)
            else
                Ask(self).each(affordableVictims)(v => SWAwakenSphinxSacrificeAction(self, r, v.ref)).cancel

        case SWAwakenSphinxSacrificeAction(self, r, victim) =>
            val u = game.unit(victim)
            val kidnapDiscount = game.swKidnapped.has(victim).??(SWValues.BlackSphinx_kidnapDiscount)
            val cost = math.max(0, SWValues.BlackSphinx_awakenBaseCost - u.uclass.cost - kidnapDiscount)
            if (!self.canSpend(cost))
                EndAction(self)
            else {
                self.power -= cost
                self.log("sacrificed", u.uclass.styled(SW), "in", r, "to awaken", BlackSphinx.name.styled(SW), "for", cost.power)
                game.swKidnapped = game.swKidnapped.but(victim)
                game.eliminate(u)
                self.place(BlackSphinx, r)
                self.satisfy(AwakenSphinxReq, AwakenSphinxReq.text)
                gainHunger(SWValues.Hunger_awakenGain, "Black Sphinx awakened")
                Then(MainAction(self))
            }

        // ────────────────────────────────────────────────────────────────
        // BOTTOMLESS PIT (own use AND cross-faction use — see Game.neutralSpellbooks)
        // ────────────────────────────────────────────────────────────────
        case SWBottomlessPitMainAction(self, regions) =>
            Ask(self).each(regions)(r => SWBottomlessPitRegionPickAction(self, r)).cancel

        case SWBottomlessPitRegionPickAction(self, r) =>
            val targets = game.factions.but(self)./~(e => e.at(r).%(_.uclass.utype == Monster).not(Zeroed))
            if (targets.isEmpty)
                EndAction(self)
            else
                Ask(self).each(targets)(v => SWBottomlessPitAction(self, r, v.ref)).cancel

        case SWBottomlessPitAction(self, r, victim) =>
            val u = game.unit(victim)
            val owner = u.faction
            val gain = math.max(0, u.uclass.cost - SWValues.BottomlessPit_costMinus)
            self.power -= SWValues.BottomlessPit_cost
            self.log(BottomlessPit.styled(SW) + ": eliminated", u.uclass.styled(owner), "in", r, "for", gain.es)
            game.eliminate(u)
            self.es ++= gain.times(ElderSign(0))
            // Hunger -1 (fixed, per the designer's explicit "build as fixed costs" note on the
            // ae section) — NOT the dynamic ES amount just gained above.
            changeHunger(-SWValues.Hunger_bottomlessPitSpend, "Bottomless Pit performed")
            if (self != SW) {
                SW.power += SWValues.BottomlessPit_swBonusPower
                SW.es ++= SWValues.BottomlessPit_swBonusES.times(ElderSign(0))
                SW.log(BottomlessPit.styled(SW) + ": gained", SWValues.BottomlessPit_swBonusPower.power, "and", SWValues.BottomlessPit_swBonusES.es, "(" + self.full + " used this ability)")
            }
            SW.satisfy(BottomlessPitReq, BottomlessPitReq.text)
            Then(MainAction(self))

        // ────────────────────────────────────────────────────────────────
        // ANCIENT EMETIC — pick a pool Monster/Terror (affordable at a 1-power discount), then
        // a controlled-gate region to summon it into. Hunger +1 either way (ae row fb1hul33).
        // ────────────────────────────────────────────────────────────────
        case SWAncientEmeticMainAction(self, choices) =>
            if (choices.isEmpty)
                EndAction(self)
            else
                Ask(self).each(choices)(uc => SWAncientEmeticUnitAction(self, uc)).cancel

        case SWAncientEmeticUnitAction(self, uc) =>
            val regions = self.gates
            if (regions.isEmpty)
                EndAction(self)
            else
                Ask(self).each(regions)(r => SWAncientEmeticRegionAction(self, uc, r)).cancel

        case SWAncientEmeticRegionAction(self, uc, r) =>
            val cost = math.max(0, uc.cost - SWValues.AncientEmetic_costDiscount)
            self.payPower(cost, "on " + AncientEmetic.name)
            self.payTax(r)
            self.place(uc, r)
            self.log(AncientEmetic.styled(SW) + ": summoned", uc.styled(SW), "in", r, "for", cost.power)
            changeHunger(SWValues.Hunger_ancientEmeticGain, AncientEmetic.name + " used")
            Then(MainAction(self))

        // ────────────────────────────────────────────────────────────────
        // KIDNAP (UFA, Post-Battle — the Ask(...) itself is built in Battle.scala)
        // ────────────────────────────────────────────────────────────────
        case SWKidnapAction(self, enemy, victim) =>
            game.swKidnapped :+= victim
            self.log(Kidnap.styled(SW) + ": kidnapped", game.unit(victim).uclass.styled(enemy), "from", enemy.full)
            self.satisfy(KidnapReq, KidnapReq.text)
            UnknownContinue

        case SWKidnapSkipAction(self, enemy) =>
            self.log(Kidnap.styled(SW) + ": declined")
            UnknownContinue

        // ────────────────────────────────────────────────────────────────
        // MUMMIES CURSE / LIFE FROM DEATH (dual-power SB6)
        // ────────────────────────────────────────────────────────────────
        case SWMummiesCurseAction(self, mummy) =>
            implicit val battle = game.battle.get
            val u = game.unit(mummy)
            self.log("Mummies Curse".styled(SW) + ": eliminated", HybridMummy.styled(SW), "for", SWValues.MummiesCurse_pains.toString.styled("pain") + " Pains")
            battle.eliminate(u)
            self.rolls ++= SWValues.MummiesCurse_pains.times(Pain)
            UnknownContinue

        case SWMummiesCurseSkipAction(self) =>
            UnknownContinue

        case SWLifeFromDeathMainAction(self) =>
            implicit val battle = game.battle.get
            val mummies = self.forces.%(_.uclass == HybridMummy)./(_.ref)
            if (mummies.isEmpty)
                EndAction(self)
            else
                Ask(self).each(mummies)(m => SWLifeFromDeathAction(self, m)).add(SWLifeFromDeathSkipAction(self))

        case SWLifeFromDeathAction(self, mummy) =>
            val u = game.unit(mummy)
            val r = u.region
            self.log("Life from Death".styled(SW) + ": replaced", HybridMummy.styled(SW), "with", SWValues.LifeFromDeath_cultists, Acolyte.plural, "in", r)
            game.eliminate(u)
            1.to(SWValues.LifeFromDeath_cultists).foreach(_ => if (self.pool.%(_.uclass == Acolyte).any) self.place(Acolyte, r))
            game.swLifeFromDeathUsedThisBattle = true
            UnknownContinue

        case SWLifeFromDeathSkipAction(self) =>
            UnknownContinue

        // ────────────────────────────────────────────────────────────────
        // STARVATION leaf actions are handled above, in the Doom Phase section.
        // ────────────────────────────────────────────────────────────────

        // ────────────────────────────────────────────────────────────────
        // FUNERAL PROCESSION — free carry of Hybrid Mummies / Kidnapped units with Nitokris.
        // ────────────────────────────────────────────────────────────────
        case MovedAction(self, u, o, r) if self == SW && u.uclass == QueenNitokris && SW.can(FuneralProcession) =>
            Force(SWCarryOfferAction(o, r))

        case SWCarryOfferAction(o, to) =>
            val carryable = (SW.at(o).%(_.uclass == HybridMummy).not(Zeroed)./(_.ref) ++
                game.swKidnapped.%(ref => game.unit(ref).region == o)).distinct
            if (carryable.isEmpty)
                UnknownContinue
            else {
                implicit val asking = Asking(SW)
                carryable.foreach { ur => + SWCarryAction(o, ur, to).as(game.unit(ur).uclass.styled(game.unit(ur).faction), "to", to) }
                + SWCarryDoneAction(o, to)
                asking
            }

        case SWCarryAction(o, ur, to) =>
            val u = game.unit(ur)
            u.region = to
            u.onGate = false
            u.add(Moved)
            SW.log("Funeral Procession".styled(SW) + ": carried", u.uclass.styled(u.faction), "from", o, "to", to, "for free")
            Force(SWCarryOfferAction(o, to))

        case SWCarryDoneAction(o, to) =>
            UnknownContinue

        case _ =>
            UnknownContinue
    }
}
