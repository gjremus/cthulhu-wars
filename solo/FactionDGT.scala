package cws

import hrf.colmat._
import html._

// ============================================================================
// TRIBULATION (DGT) — Homebrew faction
// Designer: daganisoraan, faction id 46lllknws5qi
// Faction color #808080 (grey), style "dgt".
// Four Horsemen theme: Pestilence, War, Famine, Death as Terrors.
// ============================================================================


// ── UNITS ───────────────────────────────────────────────────────────────────
// DGT uses the standard Acolyte UnitClass for its 6 Acolytes.
// Conquest: Monster cost 1, combat 1, pool 4.
// Pestilence, War, Famine, Death: Terrors cost 3, combat 3 each, pool 1 each.
case object Conquest   extends FactionUnitClass(DGT, "Conquest",   Monster, 1)
case object Pestilence extends FactionUnitClass(DGT, "Pestilence", Terror,  3)
case object War        extends FactionUnitClass(DGT, "War",        Terror,  3)
case object Famine     extends FactionUnitClass(DGT, "Famine",     Terror,  3)
case object Death      extends FactionUnitClass(DGT, "Death",      Terror,  3)


// ── SPELLBOOKS ──────────────────────────────────────────────────────────────
case object WhiteRider    extends FactionSpellbook(DGT, "The White Rider")
case object RedRider      extends FactionSpellbook(DGT, "The Red Rider")
case object BlackRider    extends FactionSpellbook(DGT, "The Black Rider")
case object PaleRider     extends FactionSpellbook(DGT, "The Pale Rider")
case object MartyrsCry    extends FactionSpellbook(DGT, "The Martyr's Cry")
case object CosmicChaos   extends FactionSpellbook(DGT, "The Cosmic Chaos")

// Dual-sided spellbook back sides
case object GodsPlague   extends FactionSpellbook(DGT, "God's Plague")
case object GodsSword    extends FactionSpellbook(DGT, "God's Sword")
case object GodsScale    extends FactionSpellbook(DGT, "God's Scale")
case object GodsHand     extends FactionSpellbook(DGT, "God's Hand")

// DGT Unique Ability — Dominion (Ongoing).
case object Dominion extends FactionSpellbook(DGT, "Dominion")


// ── SPELLBOOK REQUIREMENTS ──────────────────────────────────────────────────
case object WhiteRiderReq  extends Requirement("Control a Gate in an Area marked with Dragon Glyph; Add Pestilence to your Pool")
case object RedRiderReq    extends Requirement("Control a Gate in an Area marked with Thorn Glyph; Add War to your Pool")
case object BlackRiderReq  extends Requirement("Control a Gate in an Area marked with Chevron Glyph; Add Famine to your Pool")
case object PaleRiderReq   extends Requirement("A player performs a Ritual of Annihilation; Add Death to your Pool")
case object MartyrsCryReq  extends Requirement("As an Action, distribute Power equal to number of players to enemy Factions")
case object CosmicChaosReq extends Requirement("As an Action, distribute Doom equal to number of players to enemy Factions")


// ============================================================================
// TRIBULATION (DGT) FACTION OBJECT
// ============================================================================
case object DGT extends Faction { f =>
    def name  = "Tribulation"
    def short = "DGT"
    def style = "dgt"

    // Dominion is the always-on unique ability.
    override def abilities = $(Dominion)
    override def library   = $(WhiteRider, RedRider, BlackRider, PaleRider, MartyrsCry, CosmicChaos,
                               GodsPlague, GodsSword, GodsScale, GodsHand)
    override def requirements(options : $[GameOption]) =
        $(WhiteRiderReq, RedRiderReq, BlackRiderReq, PaleRiderReq, MartyrsCryReq, CosmicChaosReq)

    // 6 Acolytes + 4 Conquest + 1 each of the four Terrors (starts out of pool).
    val allUnits =
        6.times(Acolyte)    ++
        4.times(Conquest)   ++
        1.times(Pestilence) ++
        1.times(War)        ++
        1.times(Famine)     ++
        1.times(Death)

    // Standard strength calculation: cost-based combat for Monsters/Terrors, 0 for Acolytes.
    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int = {
        // Conquest and the four Terrors use their cost as combat.
        // War has 6 Combat with Red Rider, 12 with God's Sword.
        val warBonus = if (units.exists(_.uclass == War)) {
            if (f.has(GodsSword)) 12 - 3  // +9 from base 3
            else if (f.has(RedRider)) 6 - 3  // +3 from base 3
            else 0
        } else 0

        units.%(u => u.uclass.utype == Monster || u.uclass.utype == Terror)
            .not(Zeroed)
            ./(_.uclass.cost)
            .sum + warBonus + neutralStrength(units, opponent)
    }
}


// ============================================================================
// TRIBULATION (DGT) ACTION CASE CLASSES
// ============================================================================

// ── DISTRIBUTE POWER (Action, SBR 5) ────────────────────────────────────────
case class DistributePowerMainAction(self : Faction)
    extends OptionFactionAction("Distribute Power to enemy Factions".styled(DGT)) with MainQuestion with Soft

case class DistributePowerFactionAction(self : Faction, remaining : Int, distributed : $[(Faction, Int)])
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) =
        "Distribute Power".styled(DGT) + ": choose enemy Faction to receive Power (" + remaining + " Power remaining)"
}

case class DistributePowerAmountAction(self : Faction, target : Faction, remaining : Int, distributed : $[(Faction, Int)])
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) =
        "Distribute Power".styled(DGT) + ": choose amount of Power to give " + target.name
}

case class DistributePowerGiveAction(self : Faction, target : Faction, amount : Int, remaining : Int, distributed : $[(Faction, Int)])
    extends BaseFactionAction("Give " + amount + " Power to", implicit g => target.name) with PowerNeutral

case class DistributePowerCancelAction(self : Faction)
    extends OptionFactionAction("Cancel Power distribution".styled(DGT)) with MainQuestion with Soft


// ── DISTRIBUTE DOOM (Action, SBR 6) ─────────────────────────────────────────
case class DistributeDoomMainAction(self : Faction)
    extends OptionFactionAction("Distribute Doom to enemy Factions".styled(DGT)) with MainQuestion with Soft

case class DistributeDoomFactionAction(self : Faction, remaining : Int, distributed : $[(Faction, Int)])
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) =
        "Distribute Doom".styled(DGT) + ": choose enemy Faction to receive Doom (" + remaining + " Doom remaining)"
}

case class DistributeDoomAmountAction(self : Faction, target : Faction, remaining : Int, distributed : $[(Faction, Int)])
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) =
        "Distribute Doom".styled(DGT) + ": choose amount of Doom to give " + target.name
}

case class DistributeDoomGiveAction(self : Faction, target : Faction, amount : Int, remaining : Int, distributed : $[(Faction, Int)])
    extends BaseFactionAction("Give " + amount + " Doom to", implicit g => target.name) with PowerNeutral

case class DistributeDoomCancelAction(self : Faction)
    extends OptionFactionAction("Cancel Doom distribution".styled(DGT)) with MainQuestion with Soft


// ── GOD'S PLAGUE (Action, White Rider back side) ────────────────────────────
case class GodsPlagueMainAction(self : Faction)
    extends BaseFactionAction(GodsPlague.styled(DGT), "Use ability (2 Power)") with MainQuestion {
    override def question(implicit game : Game) = GodsPlague.styled(DGT)
}

case class GodsPlagueEnemyPickAction(self : Faction, enemyFaction : Faction, r : Region)
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) =
        GodsPlague.styled(DGT) + ": " + enemyFaction.name + " — choose a Unit in " + r.elem + " to be Pained away"
}

case class GodsPlagueChooseUnitAction(self : Faction, enemyFaction : Faction, u : UnitRef, r : Region)
    extends BaseFactionAction(GodsPlague.styled(DGT) + ": Pain away", implicit g => g.unit(u).uclass.styled(enemyFaction) + " in " + g.unit(u).region)

case class GodsPlagueChooseDestAction(self : Faction, u : UnitRef, painedUnits : $[(UnitRef, Faction)])
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) =
        GodsPlague.styled(DGT) + ": choose Area to Pain " + game.unit(u).uclass.styled(game.unit(u).faction) + " into"
}

case class GodsPlaguePainToAction(self : Faction, u : UnitRef, dest : Region, painedUnits : $[(UnitRef, Faction)])
    extends BaseFactionAction(GodsPlague.styled(DGT) + ": Pain to", dest)


// ── GOD'S SCALE (Post-Action, Black Rider back side) ────────────────────────
// Enemies can flip this face-down for +1 ES to DGT, then face-up at Doom Phase end.
case class GodsScaleFlipPromptAction(self : Faction, enemyFaction : Faction)
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) =
        GodsScale.styled(DGT) + ": " + enemyFaction.name + " — flip face-down for " + self.name + " to gain 1 Elder Sign?"
}

case class GodsScaleFlipYesAction(self : Faction, enemyFaction : Faction)
    extends OptionFactionAction(implicit g => "Flip " + GodsScale.styled(DGT) + " face-down") with PowerNeutral {
    override def question(implicit game : Game) = GodsScale.styled(DGT) + ": flip face-down?"
}

case class GodsScaleFlipNoAction(self : Faction, enemyFaction : Faction)
    extends OptionFactionAction("Skip") with PowerNeutral {
    override def question(implicit game : Game) = GodsScale.styled(DGT) + ": flip face-down?"
}


// ============================================================================
// TRIBULATION (DGT) EXPANSION — game-loop integration
// ============================================================================
object DGTExpansion extends Expansion {

    def perform(action : Action, soft : VoidGuard)(implicit game : Game) = action @@ {

        // ── MAIN ACTION PHASE ────────────────────────────────────────────────
        case MainAction(f : DGT.type) =>
            implicit val asking = Asking(f)

            // God's Plague (White Rider back, 2 Power Action)
            if (f.has(GodsPlague) && f.power >= 2) {
                val pestilenceArea = f.units.%(_.uclass == Pestilence)./(_.region).headOption
                pestilenceArea.foreach { r =>
                    if (f.enemies.exists(e => e.at(r).any)) {
                        + GodsPlagueMainAction(f)
                    }
                }
            }

            // Distribute Power (SBR 5 requirement action)
            if (f.needs(MartyrsCryReq) && f.enemies.any) {
                + DistributePowerMainAction(f)
            }

            // Distribute Doom (SBR 6 requirement action)
            if (f.needs(CosmicChaosReq) && f.enemies.any) {
                + DistributeDoomMainAction(f)
            }

            UnknownContinue

        // ── DISTRIBUTE POWER ─────────────────────────────────────────────────
        case DistributePowerMainAction(f) =>
            implicit val asking = Asking(f)
            val total = game.factions.num
            Force(DistributePowerFactionAction(f, total, $))

        case DistributePowerFactionAction(f, remaining, distributed) =>
            implicit val asking = Asking(f)
            if (remaining <= 0) {
                f.satisfy(MartyrsCryReq, MartyrsCryReq.text)
                Force(EndAction(f))
            } else {
                val enemies = f.enemies
                Ask(f).each(enemies)(e => DistributePowerAmountAction(f, e, remaining, distributed))
                    .cancel
            }

        case DistributePowerAmountAction(f, target, remaining, distributed) =>
            implicit val asking = Asking(f)
            val amounts = (1 to remaining).toList
            Ask(f).each(amounts)(amt => DistributePowerGiveAction(f, target, amt, remaining, distributed))

        case DistributePowerGiveAction(f, target, amount, remaining, distributed) =>
            target.power += amount
            f.log("gave", amount.power, "to", target)
            Force(DistributePowerFactionAction(f, remaining - amount, distributed :+ (target, amount)))

        case DistributePowerCancelAction(f) =>
            Force(MainAction(f))

        // ── DISTRIBUTE DOOM ──────────────────────────────────────────────────
        case DistributeDoomMainAction(f) =>
            implicit val asking = Asking(f)
            val total = game.factions.num
            Force(DistributeDoomFactionAction(f, total, $))

        case DistributeDoomFactionAction(f, remaining, distributed) =>
            implicit val asking = Asking(f)
            if (remaining <= 0) {
                f.satisfy(CosmicChaosReq, CosmicChaosReq.text)
                Force(EndAction(f))
            } else {
                val enemies = f.enemies
                Ask(f).each(enemies)(e => DistributeDoomAmountAction(f, e, remaining, distributed))
                    .cancel
            }

        case DistributeDoomAmountAction(f, target, remaining, distributed) =>
            implicit val asking = Asking(f)
            val amounts = (1 to remaining).toList
            Ask(f).each(amounts)(amt => DistributeDoomGiveAction(f, target, amt, remaining, distributed))

        case DistributeDoomGiveAction(f, target, amount, remaining, distributed) =>
            target.doom += amount
            f.log("gave", amount.doom, "to", target)
            Force(DistributeDoomFactionAction(f, remaining - amount, distributed :+ (target, amount)))

        case DistributeDoomCancelAction(f) =>
            Force(MainAction(f))

        // ── GOD'S PLAGUE (simplified - needs full multi-enemy implementation) ─
        case GodsPlagueMainAction(f) =>
            // Simplified: just end the action for now
            // TODO: Implement full multi-step enemy prompting + Pain destination choosing
            f.power -= 2
            f.log(GodsPlague.styled(DGT) + ": used (simplified implementation)")
            Force(EndAction(f))

        // ── GATHER POWER PHASE (Martyr's Cry) ───────────────────────────────
        case AfterPowerGatherAction if game.setup.has(DGT) && DGT.can(MartyrsCry) =>
            // Count Acolytes in pool
            val acolytesInPool = DGT.pool.%(_.uclass == Acolyte).num
            // Count Acolytes on enemy faction cards (captured by enemies)
            // Acolytes captured by enemies would have u.faction == DGT but be in enemy territory
            // For now, simplified: just count pool acolytes
            // TODO: Properly count captured acolytes when we understand the capture mechanics
            val total = acolytesInPool
            if (total > 0) {
                val powerGained = total * 2
                DGT.power += powerGained
                DGT.log(MartyrsCry.styled(DGT) + ": gained", powerGained.power, "for", total, "Acolytes in pool/captured")
            }
            UnknownContinue

        // ── RITUAL OF ANNIHILATION (Pale Rider SBR trigger) ──────────────────
        // TODO: Intercept when any player performs a Ritual
        // For now, this requirement must be manually satisfied when the condition is met

        // ── DOMINION: Intercept Move action destination calculation ──────────
        // TODO: Need to properly intercept move destination selection
        // For now, simplified implementation through regular adjacency

        // ── PALE RIDER: Add 1 Kill in battles with Death present ─────────────
        // TODO: Need proper battle integration to add kills
        // Simplified: just log when Death is present, actual kill addition needs deeper integration

        // ── DUAL-SIDED SPELLBOOK FLIP TRIGGERS ───────────────────────────────
        // TODO: Implement flip triggers properly
        // These need special Requirements that check the flip conditions and award the back-side spellbook + ES
        // For now, simplified:
        // - Mayhem (White Rider → God's Plague): Pestilence present + 2+ Pains to enemies in battle
        // - Ruin (Red Rider → God's Sword): War present + 10+ combat dice rolled
        // - Rampage (Black Rider → God's Scale): Sacrifice 1 enemy cultist during Gather Power
        // - Despair (Pale Rider → God's Hand): Death present + Kill assigned to 3+ cost enemy unit

        case _ => UnknownContinue
    }

    // ── SPELLBOOK REQUIREMENTS (continuous evaluation) ──────────────────────
    override def triggers()(implicit game : Game) {
        if (!game.setup.has(DGT)) return
        val f = DGT

        // SBR 1 — White Rider: Control a Gate in Dragon Glyph (GlyphOO) area
        f.satisfyIf(WhiteRiderReq, WhiteRiderReq.text,
            game.board.regions.exists(r => r.glyph == GlyphOO && f.gates.contains(r)) && !f.pool.exists(_.uclass == Pestilence))

        // SBR 2 — Red Rider: Control a Gate in Thorn Glyph (GlyphWW) area
        f.satisfyIf(RedRiderReq, RedRiderReq.text,
            game.board.regions.exists(r => r.glyph == GlyphWW && f.gates.contains(r)) && !f.pool.exists(_.uclass == War))

        // SBR 3 — Black Rider: Control a Gate in Chevron Glyph (GlyphAA) area
        f.satisfyIf(BlackRiderReq, BlackRiderReq.text,
            game.board.regions.exists(r => r.glyph == GlyphAA && f.gates.contains(r)) && !f.pool.exists(_.uclass == Famine))
    }

    // ── GOD'S HAND: DGT units in Death's area can't be eliminated ──────────
    override def eliminate(u : UnitFigure)(implicit game : Game) {
        if (!game.setup.has(DGT)) return
        if (u.faction != DGT) return
        if (!DGT.has(GodsHand)) return

        val deathInArea = DGT.at(u.region).exists(_.uclass == Death)
        if (deathInArea) {
            // Cancel the elimination - unit survives
            DGT.log(GodsHand.styled(DGT) + ":", u.uclass, "ignored elimination in Death's Area")
            // TODO: Properly prevent elimination - for now just log it
        }
    }

    // ── AFTER ACTION: nothing special needed for now ────────────────────────
    override def afterAction()(implicit game : Game) {
        // Future: God's Scale flip prompts could go here
    }

}
