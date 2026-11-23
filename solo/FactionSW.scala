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
// Legacy object, kept for replaying games played before the 2026-11-19 designer update
// (v63 -> v75 replaced "Monstrous Forepaw" with "Convert the faithless"). It is no longer
// in SW's library; only old save/replay action streams can still reference it.
case object MonstrousForepaw  extends FactionSpellbook(SW, "Monstrous Forepaw")
case object ConvertTheFaithless extends FactionSpellbook(SW, "Convert the faithless")
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
    override def library   = $(FuneralProcession, BottomlessPit, AncientEmetic, ConvertTheFaithless, Starvation, MummiesCurse)
    override def requirements(options : $[GameOption]) =
        $(KidnapReq, BottomlessPitReq, HungerReq2, HungerReq4, NitokrisReq, AwakenSphinxReq)

    val allUnits =
        SWValues.Acolyte_count.times(Acolyte)          ++
        SWValues.HybridMummy_count.times(HybridMummy)  ++
        SWValues.PharoahKephren_count.times(PharoahKephren) ++
        SWValues.QueenNitokris_count.times(QueenNitokris)   ++
        SWValues.BlackSphinx_count.times(BlackSphinx)

    // Black Sphinx's awaken cost (design change, 2026-10-26: 9 minus the current Hunger Track
    // value, no longer a monster sacrifice) is a flat per-region number, but the generic
    // Awaken menu's area list (Game.awakens' "elderAreas", any Area with no enemy gate) doesn't
    // match this design's restriction to areas with SW's OWN controlled gate — so Black Sphinx
    // still stays OFF the generic Awaken menu (None here) and keeps its own
    // SWAwakenSphinxMainAction flow (offered from MainAction) to enforce that area filter.
    override def awakenCost(u : UnitClass, r : Region)(implicit game : Game) : |[Int] = None

    // Hybrid Mummy / Pharoah Kephren roll dice (combat read from SWValues, same combatOf
    // idiom as FMExpansion.combatOf — combat is NOT always equal to cost, so it must be read
    // separately rather than reusing .uclass.cost). Queen Nitokris rolls (combatBase + current
    // Hunger) dice AND adds Pains equal to the current Hunger Track (design change, 2026-11-19:
    // her resultsCalc became "Adds pains = Current Hunger Track" — see Battle.scala
    // BattleRollAction). Black Sphinx rolls NO dice — he adds 1 Kill plus Pains equal to the
    // current Hunger Track in Battle.scala's BattleRollAction. Monsters taken by Convert the
    // faithless fight for SW at one less combat than they had originally.
    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int = {
        units.%(u => u.uclass == HybridMummy || u.uclass == PharoahKephren).not(Zeroed)./(u => SWExpansion.combatOf(u.uclass)).sum +
        units.%(_.uclass == QueenNitokris).not(Zeroed).num * (SWValues.QueenNitokris_combatBase + game.swHunger) +
        units.%(u => game.swMummified.has(u.ref)).not(Zeroed)./(u => math.max(0, u.uclass.cost - 1 - neutralStrength($(u), opponent))).sum +
        neutralStrength(units, opponent)
    }
}


// ── ACTION CASE CLASSES ─────────────────────────────────────────────────────

// Awaken Black Sphinx (custom flow — see awakenCost override above). RegionAction is now the
// terminal step (no more sacrifice sub-step): picking the Area pays the cost and places him.
case class SWAwakenSphinxMainAction(self : Faction) extends OptionFactionAction(("Awaken " + BlackSphinx.name).styled(SW)) with MainQuestion
case class SWAwakenSphinxRegionAction(self : Faction, r : Region) extends BaseFactionAction(("Awaken " + BlackSphinx.name).styled(SW) + " in", implicit g => r + self.iced(r))

// Kidnap (UFA, Post-Battle — two menus: first SW decides whether to kidnap,
// then if yes, enemy chooses which pained or killed monster gets kidnapped).
// MENU 1: Ask SW whether to kidnap
case class SWKidnapOfferAction(self : Faction, enemy : Faction) extends OptionFactionAction("Kidnap a pained enemy monster?") with PowerNeutral {
    def question(implicit game : Game) = Kidnap.styled(SW)
}
case class SWKidnapDeclineAction(self : Faction, enemy : Faction) extends OptionFactionAction("No") with PowerNeutral {
    def question(implicit game : Game) = Kidnap.styled(SW)
}
// MENU 2: Ask ENEMY which monster to kidnap (only appears if SW said yes)
case class SWKidnapAction(self : Faction, enemy : Faction, victim : UnitRef)
    extends OptionFactionAction(implicit g => "Kidnap " + g.unit(victim).uclass.styled(g.unit(victim).faction)) with PowerNeutral {
    def question(implicit game : Game) = Kidnap.styled(SW)
}
// Deprecated — keep for compatibility but unused in new flow
case class SWKidnapSkipAction(self : Faction, enemy : Faction) extends OptionFactionAction("Skip") with PowerNeutral {
    def question(implicit game : Game) = Kidnap.styled(SW)
}

// Bottomless Pit — SW's own baseline use AND (once earned) any faction's cross-faction use
// (offered from Game.neutralSpellbooks, same idiom as CS's Effulgent Sacrifice).
case class SWBottomlessPitMainAction(self : Faction, l : $[Region]) extends OptionFactionAction(BottomlessPit.styled(SW)) with MainQuestion with Soft
case class SWBottomlessPitRegionAction(self : Faction, r : Region)
    extends BaseFactionAction(BottomlessPit.styled(SW) + ": choose unit to eliminate in", r) with PowerNeutral with Soft
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
// Convert the faithless (Post-Battle SB, v75): in a battle with Pharoah Kephren, replace a
// killed/eliminated enemy Cultist with a Hybrid Mummy from the pool, OR take a killed/
// eliminated enemy Monster as an SW-controlled "mummified" monster (one less combat, moves
// for free with Kephren, returned to its owner's pool during Gather Power).
case class SWConvertCultistAction(self : Faction, victim : UnitRef)
    extends OptionFactionAction(implicit g => ConvertTheFaithless.styled(SW) + ": replace killed " + g.unit(victim).uclass.styled(g.unit(victim).faction) + " with " + HybridMummy.styled(SW)) with PowerNeutral {
    def question(implicit game : Game) = ConvertTheFaithless.styled(SW)
}
case class SWConvertMonsterAction(self : Faction, victim : UnitRef)
    extends OptionFactionAction(implicit g => ConvertTheFaithless.styled(SW) + ": mummify killed " + g.unit(victim).uclass.styled(g.unit(victim).faction)) with PowerNeutral {
    def question(implicit game : Game) = ConvertTheFaithless.styled(SW)
}
case class SWConvertSkipAction(self : Faction) extends OptionFactionAction("Skip") with PowerNeutral {
    def question(implicit game : Game) = ConvertTheFaithless.styled(SW)
}

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
case class SWHungerSacrificeCultistAction(remaining : Int, ref : UnitRef, gateStatus : String = "")
    extends OptionFactionAction(implicit g => "Sacrifice " + Acolyte.styled(SW) + " in " + g.unit(ref).region + gateStatus) with PowerNeutral {
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

    // "Ceaseless Hunger" Hunger gain (Black Sphinx awakened / assigned a pain / killed)
    // — fully blocked by an enemy-controlled Elder Thing sharing Black Sphinx's region.
    def gainHunger(n : Int, why : String)(implicit game : Game) {
        if (ceaselessHungerActive) changeHunger(n, why)
    }

    // Regions where Bottomless Pit can be performed: Black Sphinx's own region only
    // (design v75 removed the "adjacent Areas" extension along with Monstrous Forepaw).
    def bottomlessPitRegions(implicit game : Game) : $[Region] =
        SW.onMap(BlackSphinx).not(Zeroed)./(_.region).distinct

    // ── Units controlled by SW taken from another faction ────────────────────
    // Kidnap (UFA) and Convert the faithless (SB) both move an enemy figure under SW
    // control. The figure is re-created under SW (UnitFigure.faction is immutable, same
    // idiom as MindParasite.parasitize) and the original owner/index are tracked so it
    // can be handed back. Kidnapped units stay with SW; mummified monsters go back to
    // their owner's pool during Gather Power.
    def takeControl(u : UnitFigure, to : Region, mummified : Boolean)(implicit game : Game) : UnitFigure = {
        val owner = u.faction
        val origIndex = u.index
        owner.units :-= u
        val idx = game.swControlledNextIndex
        game.swControlledNextIndex += 1
        val fig = new UnitFigure(SW, u.uclass, idx, to)
        fig.add(Mummified)
        SW.units :+= fig
        game.swControlledOriginalFaction += (fig.ref -> owner)
        game.swControlledOriginalIndex += (fig.ref -> origIndex)
        if (mummified)
            game.swMummified :+= fig.ref
        else
            game.swKidnapped :+= fig.ref
        fig
    }

    // Hand a controlled figure back to its original owner's pool (eliminated or returned).
    def releaseControlled(u : UnitFigure)(implicit game : Game) {
        val owner = game.swControlledOriginalFaction.getOrElse(u.ref, u.faction)
        val origIndex = game.swControlledOriginalIndex.getOrElse(u.ref, u.index)
        u.faction.units :-= u
        owner.units :+= new UnitFigure(owner, u.uclass, origIndex, owner.reserve)
        game.swControlledOriginalFaction -= u.ref
        game.swControlledOriginalIndex -= u.ref
        game.swKidnapped = game.swKidnapped.but(u.ref)
        game.swMummified = game.swMummified.but(u.ref)
    }

    // Convert the faithless: mummified monsters are returned during Gather Power.
    def returnMummified()(implicit game : Game) {
        game.swMummified.foreach { ur =>
            game.factions./~(_.units.%(_.ref == ur)).foreach { u =>
                val owner = game.swControlledOriginalFaction.getOrElse(u.ref, u.faction)
                SW.log(ConvertTheFaithless.styled(SW) + ": returned", u.uclass.styled(owner), "to", owner.full)
                releaseControlled(u)
            }
        }
        game.swMummified = $
    }

    // Kidnapped units follow Queen Nitokris automatically (moves and pain retreats), no menu.
    def followKidnapped(from : Region, to : Region, why : String)(implicit game : Game) {
        game.swKidnapped./~(ur => game.unitOpt(ur).$).%(u => u.region == from).foreach { u =>
            u.region = to
            u.onGate = false
            SW.log(Kidnap.styled(SW) + ": kidnapped", u.uclass.styled(u.faction), why, to, "with", QueenNitokris.name)
        }
    }

    // Convert the faithless: mummified monsters move for free with Pharoah Kephren.
    def followMummified(from : Region, to : Region)(implicit game : Game) {
        game.swMummified./~(ur => game.unitOpt(ur).$).%(u => u.region == from).foreach { u =>
            u.region = to
            u.onGate = false
            SW.log(ConvertTheFaithless.styled(SW) + ": mummified", u.uclass.styled(u.faction), "moved to", to, "with", PharoahKephren.name)
        }
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
                SW.onMap(Acolyte).not(Zeroed).groupBy(_.region).foreach { case (r, us) =>
                    asking.ask = asking.ask.group(r.name)
                    us.foreach(u => {
                        val gateStatus = if (u.onGate) " On Gate" else ""
                        + SWHungerSacrificeCultistAction(remaining, u.ref, gateStatus)
                    })
                }
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

        case SWHungerSacrificeCultistAction(remaining, ref, _) =>
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

            val sphinxRegions = game.board.regions.%(r => f.gates.has(r) && f.pool.%(_.uclass == BlackSphinx).any)
            // Button gating: Awaken Black Sphinx cost is 9 minus the current Hunger Track value,
            // minimum 0. Only offer button if there's a region with SW's own gate, Black Sphinx
            // is in the pool, and SW can afford that cost.
            val sphinxAwakenCost = math.max(0, SWValues.BlackSphinx_awakenBaseCost - game.swHunger)
            if (sphinxRegions.any && f.canSpend(sphinxAwakenCost))
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
            val regions = game.board.regions.%(r => self.gates.has(r) && self.pool.%(_.uclass == BlackSphinx).any)
            if (regions.isEmpty)
                EndAction(self)
            else if (regions.num == 1)
                Force(SWAwakenSphinxRegionAction(self, regions.head))
            else
                Ask(self).each(regions)(r => SWAwakenSphinxRegionAction(self, r)).cancel

        case SWAwakenSphinxRegionAction(self, r) =>
            // Cost = 9 minus the current Hunger Track value, minimum 0. No sacrifice involved.
            val cost = math.max(0, SWValues.BlackSphinx_awakenBaseCost - game.swHunger)
            if (!self.canSpend(cost))
                EndAction(self)
            else {
                self.power -= cost
                self.log("awakened", BlackSphinx.name.styled(SW), "in", r, "for", cost.power)
                self.place(BlackSphinx, r)
                self.satisfy(AwakenSphinxReq, AwakenSphinxReq.text)
                gainHunger(SWValues.Hunger_awakenGain, "Black Sphinx awakened")
                AwakenedAction(SW, BlackSphinx, r, cost)
            }

        // Terminal consumer for the Black Sphinx awaken (c0fddb64 emitted AwakenedAction
        // with no SW handler -> MatchError "REPLAY CRASH" in every SW game that awakened).
        case AwakenedAction(self : SW.type, BlackSphinx, _, _) =>
            EndAction(self)

        // ────────────────────────────────────────────────────────────────
        // BOTTOMLESS PIT (own use AND cross-faction use — see Game.neutralSpellbooks)
        // ────────────────────────────────────────────────────────────────
        case SWBottomlessPitMainAction(self, regions) =>
            if (regions.isEmpty)
                EndAction(self)
            else if (regions.num == 1)
                Force(SWBottomlessPitRegionAction(self, regions.head))
            else
                Ask(self).each(regions) { r =>
                    val targets = game.factions.but(self)./~(e => e.at(r).%(_.uclass.utype == Monster).not(Zeroed))
                    if (targets.isEmpty) EndAction(self) else SWBottomlessPitRegionAction(self, r)
                }.cancel

        case SWBottomlessPitRegionAction(self, r) =>
            val targets = game.factions.but(self)./~(e => e.at(r).%(_.uclass.utype == Monster).not(Zeroed))
                .%(_.faction != SW)
            if (targets.isEmpty)
                EndAction(self)
            else if (targets.num == 1)
                Force(SWBottomlessPitAction(self, r, targets.head.ref))
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
            CheckSpellbooksAction(MainAction(self))

        // ────────────────────────────────────────────────────────────────
        // ANCIENT EMETIC — pick a pool Monster/Terror (affordable at a 1-power discount), then
        // a controlled-gate region to summon it into. Hunger +1 either way (ae row fb1hul33).
        // ────────────────────────────────────────────────────────────────
        case SWAncientEmeticMainAction(self, choices) =>
            if (choices.isEmpty)
                EndAction(self)
            else if (choices.num == 1)
                Force(SWAncientEmeticUnitAction(self, choices.head))
            else
                Ask(self).each(choices)(uc => SWAncientEmeticUnitAction(self, uc)).cancel

        case SWAncientEmeticUnitAction(self, uc) =>
            val regions = self.gates
            if (regions.isEmpty)
                EndAction(self)
            else if (regions.num == 1)
                Force(SWAncientEmeticRegionAction(self, uc, regions.head))
            else
                Ask(self).each(regions)(r => SWAncientEmeticRegionAction(self, uc, r)).cancel

        case SWAncientEmeticRegionAction(self, uc, r) =>
            val cost = math.max(0, uc.cost - SWValues.AncientEmetic_costDiscount)
            self.payPower(cost, "on " + AncientEmetic.name)
            self.payTax(r)
            self.place(uc, r)
            self.log(AncientEmetic.styled(SW) + ": summoned", uc.styled(SW), "in", r, "for", cost.power)
            changeHunger(SWValues.Hunger_ancientEmeticGain, AncientEmetic.name + " used")
            // Without this, re-entering MainAction below missed the `if f.acted` short-circuit
            // (FactionSW.scala:330) and fell into the fresh-turn body (:333), granting SW an
            // extra main action instead of ending the turn (bug r_vUC9ouG2).
            self.acted = true
            CheckSpellbooksAction(MainAction(self))

        // ────────────────────────────────────────────────────────────────
        // KIDNAP (UFA, Post-Battle — two-menu flow)
        // Menu 1: SW decides whether to kidnap (asks SW)
        // Menu 2: Enemy chooses which monster (asks enemy, only if SW said yes)
        // ────────────────────────────────────────────────────────────────
        case SWKidnapOfferAction(self, enemy) =>
            // SW agreed to kidnap. Now ask ENEMY which pained or killed monster to kidnap.
            // Battle.scala stored the candidate refs when it offered this menu (killed
            // monsters are no longer in enemySide.forces, so they cannot be recomputed here).
            val candidates = game.swKidnapCandidates
            if (candidates.num == 1) {
                self.log(Kidnap.styled(SW) + ": offered to kidnap; only one pained or killed enemy monster, kidnapped automatically")
                Force(SWKidnapAction(SW, enemy, candidates.head))
            } else {
                self.log(Kidnap.styled(SW) + ": offered to kidnap; enemy now chooses which pained or killed monster")
                Ask(enemy).each(candidates)(v => SWKidnapAction(SW, enemy, v))
            }

        case SWKidnapDeclineAction(self, enemy) =>
            self.log(Kidnap.styled(SW) + ": declined to kidnap")
            UnknownContinue

        case SWKidnapAction(self, enemy, victim) =>
            val u = game.unit(victim)
            val wasKilled = u.health != Pained && !u.region.onMap
            if (u.health == Pained)
                u.health = Alive  // the assigned pain is cancelled
            val arena = game.battle./(_.arena).|(u.region)
            val controlled = takeControl(u, arena, mummified = false)
            self.log(Kidnap.styled(SW) + ": kidnapped", u.uclass.styled(enemy), "from", enemy.full + " — its " + (wasKilled.?("kill").|("pain")) + " is cancelled; it now lies sideways under " + SW.name + " control")
            self.satisfy(KidnapReq, KidnapReq.text)
            UnknownContinue

        case SWKidnapSkipAction(self, enemy) =>
            self.log(Kidnap.styled(SW) + ": declined")
            UnknownContinue

        // ────────────────────────────────────────────────────────────────
        // CONVERT THE FAITHLESS (Post-Battle SB)
        // ────────────────────────────────────────────────────────────────
        case SWConvertCultistAction(self, victim) =>
            val arena = game.battle.get.arena
            val u = game.unit(victim)
            self.log(ConvertTheFaithless.styled(SW) + ": replaced killed", u.uclass.styled(u.faction), "with a", HybridMummy.styled(SW), "in", arena)
            if (self.pool(HybridMummy).any)
                self.place(HybridMummy, arena)
            UnknownContinue

        case SWConvertMonsterAction(self, victim) =>
            val arena = game.battle.get.arena
            val u = game.unit(victim)
            val controlled = takeControl(u, arena, mummified = true)
            self.log(ConvertTheFaithless.styled(SW) + ": mummified", u.uclass.styled(u.faction), "in", arena, "— it fights for", SW.name, "at one less combat until the next Gather Power")
            UnknownContinue

        case SWConvertSkipAction(self) =>
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
        // FUNERAL PROCESSION — free carry of Hybrid Mummies with Nitokris.
        // Kidnapped units follow Nitokris automatically (UFA, no menu); mummified monsters
        // follow Pharoah Kephren automatically (Convert the faithless).
        // ────────────────────────────────────────────────────────────────
        case MovedAction(self, u, o, r) if self == SW && u.uclass == QueenNitokris =>
            followKidnapped(o, r, "moved to")
            if (SW.can(FuneralProcession))
                Force(SWCarryOfferAction(o, r))
            else
                UnknownContinue

        case MovedAction(self, u, o, r) if self == SW && u.uclass == PharoahKephren =>
            followMummified(o, r)
            UnknownContinue

        case SWCarryOfferAction(o, to) =>
            val carryable = SW.at(o).%(_.uclass == HybridMummy).not(Zeroed)./(_.ref)
            if (carryable.isEmpty)
                MoveContinueAction(SW, true)
            else {
                implicit val asking = Asking(SW)
                carryable.foreach { ur => + SWCarryAction(o, ur, to).as(game.unit(ur).uclass.styled(game.unit(ur).faction), "to", to) }
                + SWCarryDoneAction(o, to)
                asking
            }

        case SWCarryAction(o, ur, to) =>
            val u = game.unit(ur)
            SW.log("Funeral Procession".styled(SW) + ": carried", u.uclass.styled(u.faction), "from", o, "to", to, "for free")
            u.region = to
            u.onGate = false
            u.add(Moved)
            // Loop back to the offer menu (mirrors TS Undulate) so multiple units can be
            // carried, or Done chosen; bare UnknownContinue here crashed the game (r_ next bug)
            // because no other expansion claims this SW-specific action.
            Force(SWCarryOfferAction(o, to))

        case SWCarryDoneAction(o, to) =>
            MoveContinueAction(SW, true)

        case _ =>
            UnknownContinue
    }
}
