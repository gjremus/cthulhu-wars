package cws

import hrf.colmat._
import html._

// ============================================================================
// TRIBULATION (DGT) — Homebrew faction
// Designer: daganisoraan, faction id 46lllknws5qi
// Faction color #808080 (grey), style "dgt".
// Four Horsemen theme: Pestilence, War, Famine, Death as Terrors.
// ============================================================================

// ============================================================================
// TRIBULATION (DGT) FACTION OBJECT — DEFINED FIRST (before units/spellbooks)
// ============================================================================
case object DGT extends Faction { f =>
    def name  = "Tribulation"
    def short = "DGT"
    def style = "dgt"

    // Dominion is the always-on unique ability (only spellbook available at game start).
    override def abilities = $(Dominion)
    // CRITICAL: The 6 earnable spellbooks. Mayhem/Ruin/Rampage/Despair are NOT separate
    // spellbooks - they're the DUAL POWER NAMES on the Rider spellbooks (earned together
    // with their Rider). GodsPlague/GodsSword/GodsScale/GodsHand are BACK SIDES that
    // replace the Riders when flipped (via flipSpellbook() helper).
    override def library   = $(WhiteRider, RedRider, BlackRider, PaleRider, MartyrsCry, CosmicChaos)
    override def requirements(options : $[GameOption]) =
        $(WhiteRiderReq, RedRiderReq, BlackRiderReq, PaleRiderReq, MartyrsCryReq, CosmicChaosReq)

    // 6 Acolytes + 4 Conquest + 1 each of the four Terrors (starts out of pool).
    val allUnits =
        DGTValues.Acolyte_count.times(Acolyte)       ++
        DGTValues.Conquest_count.times(Conquest)     ++
        DGTValues.Pestilence_count.times(Pestilence) ++
        DGTValues.War_count.times(War)               ++
        DGTValues.Famine_count.times(Famine)         ++
        DGTValues.Death_count.times(Death)

    // Standard strength calculation: cost-based combat for Monsters/Terrors, 0 for Acolytes.
    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int = {
        // Conquest and the four Terrors use their cost as combat.
        // War has 6 Combat with Red Rider, 12 with God's Sword.
        val warBonus = if (units.exists(_.uclass == War)) {
            // Numbers from DGTValues (faction-values/dgt.json)
            if (f.has(GodsSword)) DGTValues.War_combatGodsSword - DGTValues.War_combat
            else if (f.has(RedRider)) DGTValues.War_combatRedRider - DGTValues.War_combat
            else 0
        } else 0

        units.%(u => u.uclass.utype == Monster || u.uclass.utype == Terror)
            .not(Zeroed)
            ./(u => DGTExpansion.combatOf(u.uclass))
            .sum + warBonus + neutralStrength(units, opponent)
    }
}


// ── UNITS ───────────────────────────────────────────────────────────────────
// DGT uses the standard Acolyte UnitClass for its 6 Acolytes.
// Conquest: Monster cost 1, combat 1, pool 4.
// Pestilence, War, Famine, Death: Terrors cost 3, combat 3 each, pool 1 each.
case object Conquest   extends FactionUnitClass(DGT, "Conquest",   Monster, DGTValues.Conquest_cost)
case object Pestilence extends FactionUnitClass(DGT, "Pestilence", Terror,  DGTValues.Pestilence_cost)
case object War        extends FactionUnitClass(DGT, "War",        Terror,  DGTValues.War_cost)
case object Famine     extends FactionUnitClass(DGT, "Famine",     Terror,  DGTValues.Famine_cost)
case object Death      extends FactionUnitClass(DGT, "Death",      Terror,  DGTValues.Death_cost)


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

// Dual-sided spellbook front sides (matched to their back sides)
case object Mayhem       extends FactionSpellbook(DGT, "Mayhem")
case object Ruin         extends FactionSpellbook(DGT, "Ruin")
case object Rampage      extends FactionSpellbook(DGT, "Rampage")
case object Despair      extends FactionSpellbook(DGT, "Despair")

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
    extends BaseFactionAction(GodsPlague.styled(DGT), "Use ability (" + DGTValues.GodsPlague_cost + " Power)") with MainQuestion {
    override def question(implicit game : Game) = GodsPlague.styled(DGT)
}

case class GodsPlagueEnemyPickAction(self : Faction, enemyFaction : Faction, r : Region, priorPicked : $[(UnitRef, Faction)])
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) =
        GodsPlague.styled(DGT) + ": " + enemyFaction.name + " — choose a Unit in " + r.elem + " to be Pained away"
}

case class GodsPlagueChooseUnitAction(self : Faction, enemyFaction : Faction, u : UnitRef, r : Region, priorPicked : $[(UnitRef, Faction)])
    extends BaseFactionAction(GodsPlague.styled(DGT) + ": Pain away", implicit g => g.unit(u).uclass.styled(enemyFaction) + " in " + g.unit(u).region)

case class GodsPlagueChooseDestAction(self : Faction, u : UnitRef, painedUnits : $[(UnitRef, Faction)])
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) =
        GodsPlague.styled(DGT) + ": choose Area to Pain " + game.unit(u).uclass.styled(game.unit(u).faction) + " into"
}

case class GodsPlaguePainToAction(self : Faction, u : UnitRef, dest : Region, painedUnits : $[(UnitRef, Faction)])
    extends BaseFactionAction(GodsPlague.styled(DGT) + ": Pain to", dest)


// ── RAMPAGE (Gather Power, Black Rider front side) ──────────────────────────
// During Gather Power, DGT may sacrifice one captured enemy Cultist to flip and gain ES.
// The `last` parameter is the faction that acted last (for PowerGatherAction continuation).
case class RampageCheckAction(self : Faction, last : Faction)
    extends ForcedAction with PowerNeutral with Soft

case class RampageSacrificeAction(self : Faction, cultist : UnitRef, last : Faction)
    extends BaseFactionAction(
        implicit g => "Sacrifice " + g.unit(cultist).faction.name + "'s " + g.unit(cultist).uclass.styled(g.unit(cultist).faction) + " (Rampage)",
        implicit g => g.unit(cultist).full
    ) with PowerNeutral

case class RampageSkipAction(self : Faction, last : Faction)
    extends OptionFactionAction("Skip") with PowerNeutral {
    override def question(implicit game : Game) = "Skip Rampage"
}

// ── GOD'S SCALE (Out-of-Turn Reaction, Black Rider back side) ───────────────
// Like Devolve, enemies can react after any action to flip God's Scale face-down for +1 ES to DGT.
// It flips back face-up at Doom Phase end.
case class GodsScalePromptAction(self : Faction, then : ForcedAction)
    extends ForcedAction with Soft with OutOfTurn

case class GodsScaleMainAction(self : Faction, then : ForcedAction)
    extends OptionFactionAction(implicit g => "Flip " + GodsScale.styled(DGT) + " face-down") with MainQuestion with Soft with OutOfTurn {
    override def question(implicit game : Game) =
        GodsScale.styled(DGT) + ": flip face-down for " + DGT.name + " to gain " + DGTValues.GodsScale_elderSigns + " Elder Sign" + (DGTValues.GodsScale_elderSigns != 1).??("s") + "?"
}

case class GodsScaleFlipAction(self : Faction, then : ForcedAction)
    extends BaseFactionAction(implicit g => "Flip " + GodsScale.styled(DGT) + " face-down", implicit g => DGT.name + " gains " + DGTValues.GodsScale_elderSigns.es)


// ============================================================================
// TRIBULATION (DGT) EXPANSION — game-loop integration
// ============================================================================
object DGTExpansion extends Expansion {

    // Base combat dice per unit type, from DGTValues (faction-values/dgt.json).
    // War's Red Rider / God's Sword bonus is added separately in DGT.strength.
    def combatOf(uc : UnitClass) : Int = uc match {
        case Conquest   => DGTValues.Conquest_combat
        case Pestilence => DGTValues.Pestilence_combat
        case War        => DGTValues.War_combat
        case Famine     => DGTValues.Famine_combat
        case Death      => DGTValues.Death_combat
        case other      => other.cost
    }

    // Helper: flip a dual-sided spellbook and gain 1 Elder Sign
    def flipSpellbook(f : Faction, front : Spellbook, back : Spellbook, triggerName : String)(implicit game : Game) {
        if (f.has(front) && !f.has(back) && !f.oncePerGame.has(back)) {
            f.spellbooks = f.spellbooks.but(front)
            f.spellbooks = f.spellbooks :+ back
            f.oncePerGame :+= back  // Mark as flipped (once only)
            f.takeES(DGTValues.RiderFlip_elderSigns)
            f.log(triggerName.styled(DGT) + ": flipped", front.styled(DGT), "to", back.styled(DGT), "and gained", DGTValues.RiderFlip_elderSigns.es)
        }
    }

    def perform(action : Action, soft : VoidGuard)(implicit game : Game) = action @@ {

        // ── SETUP: Starting Region placement (no special units) ──────────────
        case StartingRegionAction(f : DGT.type, r) =>
            // DGT places no special starting units; just log setup completion
            f.log("started in", r)
            UnknownContinue

        // ── MAIN ACTION PHASE ────────────────────────────────────────────────
        case MainAction(f : DGT.type) if f.active.not =>
            UnknownContinue

        case MainAction(f : DGT.type) if f.acted =>
            UnknownContinue

        case MainAction(f : DGT.type) =>
            implicit val asking = Asking(f)

            // Standard game actions
            game.moves(f)
            game.captures(f)
            game.recruits(f)
            game.battles(f)
            game.controls(f)
            game.builds(f)
            game.summons(f)
            game.awakens(f)
            game.independents(f)

            // God's Plague (White Rider back, 2 Power Action)
            if (f.has(GodsPlague) && f.power >= DGTValues.GodsPlague_cost) {
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

            game.neutralSpellbooks(f)
            game.highPriests(f)
            game.reveals(f)

            game.endTurn(f)(f.battled.any)

            asking

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

        // ── GOD'S PLAGUE ─────────────────────────────────────────────────────────
        case GodsPlagueMainAction(f) =>
            f.power -= DGTValues.GodsPlague_cost
            val pestilenceArea = f.units.%(_.uclass == Pestilence)./(_.region).headOption
            pestilenceArea match {
                case Some(r) =>
                    // Get enemies with units in Pestilence's area, in turn order
                    val enemiesInArea = f.enemies.%(e => e.at(r).any)
                    if (enemiesInArea.any) {
                        // Start prompting enemies in turn order with empty picked list
                        val firstEnemy = enemiesInArea.head
                        Then(GodsPlagueEnemyPickAction(f, firstEnemy, r, $))
                    } else {
                        Force(EndAction(f))
                    }
                case None =>
                    Force(EndAction(f))
            }

        case GodsPlagueEnemyPickAction(self, enemyFaction, r, priorPicked) =>
            // Enemy picks one of their units in Pestilence's area to be Pained
            implicit val asking = Asking(enemyFaction)
            val units = enemyFaction.at(r)
            Ask(enemyFaction).each(units)(u => GodsPlagueChooseUnitAction(self, enemyFaction, u.ref, r, priorPicked))

        case GodsPlagueChooseUnitAction(self, enemyFaction, u, r, priorPicked) =>
            val allPicked = priorPicked :+ (u, enemyFaction)
            val pestilenceArea = r
            val enemiesInArea = self.enemies.%(e => e.at(pestilenceArea).any)
            val remainingEnemies = enemiesInArea.drop(allPicked.size)

            if (remainingEnemies.any) {
                // More enemies need to pick units
                val nextEnemy = remainingEnemies.head
                Then(GodsPlagueEnemyPickAction(self, nextEnemy, pestilenceArea, allPicked))
            } else {
                // All enemies have picked, now DGT chooses Pain destinations
                // Start with the first picked unit
                val firstPicked = allPicked.head
                Then(GodsPlagueChooseDestAction(self, firstPicked._1, allPicked))
            }

        case GodsPlagueChooseDestAction(self, u, painedUnits) =>
            // DGT chooses the destination for unit u to be Pained to
            implicit val asking = Asking(self)
            val regions = game.board.regions.%(_.onMap)
            Ask(self).each(regions)(dest => GodsPlaguePainToAction(self, u, dest, painedUnits))

        case GodsPlaguePainToAction(self, u, dest, painedUnits) =>
            val unitFig = game.unit(u)
            val oldRegion = unitFig.region
            unitFig.region = dest
            self.log(GodsPlague.styled(DGT) + ": Pained", unitFig.uclass.styled(unitFig.faction), "from", oldRegion, "to", dest)

            // Remove this unit from the painedUnits list and check if there are more
            val remainingUnits = painedUnits.tail
            if (remainingUnits.any) {
                // More units to Pain - prompt for next unit's destination
                val nextUnit = remainingUnits.head._1
                Then(GodsPlagueChooseDestAction(self, nextUnit, remainingUnits))
            } else {
                // All units have been Pained
                Force(EndAction(self))
            }

        // ── GOD'S SCALE (Out-of-Turn Reaction) ──────────────────────────────
        case GodsScalePromptAction(self, then) =>
            Force(GodsScaleMainAction(self, then))

        case GodsScaleMainAction(self, then) =>
            implicit val asking = Asking(self)
            Ask(self)
                .add(GodsScaleFlipAction(self, then))
                .cancel

        case GodsScaleFlipAction(self, then) =>
            // Flip God's Scale face-down and give DGT 1 ES
            DGT.takeES(DGTValues.GodsScale_elderSigns)
            game.dgtGodsScaleFaceDown = true
            self.log("flipped", GodsScale.styled(DGT), "face-down,", DGT.name, "gained", DGTValues.GodsScale_elderSigns.es)
            // Mark that this enemy has reacted (same as Devolve pattern)
            self.oncePerAction :+= GodsScale
            Then(then)

        // ── RAMPAGE (Gather Power Phase) ────────────────────────────────────
        case RampageCheckAction(self, last) =>
            // Offer to sacrifice one captured enemy Cultist.
            // Captured cultists are other factions' cultists sitting in DGT's prison.
            val capturedCultists = game.factions./~(_.at(self.prison)).%(_.cultist)
            if (capturedCultists.any) {
                implicit val asking = Asking(self)
                Ask(self)
                    .each(capturedCultists)(u => RampageSacrificeAction(self, u.ref, last))
                    .add(RampageSkipAction(self, last))
            } else {
                PowerGatherAction(last)
            }

        case RampageSacrificeAction(self, cultist, last) =>
            val unitFig = game.unit(cultist)
            val originalOwner = unitFig.faction
            // The sacrificed cultist already contributed to DGT's power during the Gather Power
            // calculations (at line ~4059), so no extra power is gained here.
            // Return cultist to owner's pool using eliminate() (the proper way to return captured cultists)
            // Log BEFORE eliminate to avoid None.get crash (Pattern #1: eliminate-then-log)
            self.log(BlackRider.styled(DGT) + " (Rampage): sacrificed", originalOwner.name + "'s", unitFig.uclass.styled(originalOwner))
            game.eliminate(unitFig)
            // Flip Black Rider to God's Scale and gain 1 ES
            DGTExpansion.flipSpellbook(DGT, BlackRider, GodsScale, "Rampage")
            // Continue with PowerGatherAction so the release loop handles the rest of the captives
            PowerGatherAction(last)

        case RampageSkipAction(self, last) =>
            // Skip Rampage, continue with PowerGatherAction
            PowerGatherAction(last)

        // ── DOOM PHASE ──────────────────────────────────────────────────────
        case DoomAction(f : DGT.type) =>
            implicit val asking = Asking(f)
            game.rituals(f)
            game.reveals(f)
            game.highPriests(f)
            game.hires(f)
            game.recruitsWhateley(f)
            game.motherOfMonsters(f)
            game.doomDone(f)
            asking

        // ── GATHER POWER PHASE (Martyr's Cry) ───────────────────────────────
        case AfterPowerGatherAction if game.setup.has(DGT) && DGT.can(MartyrsCry) && !game.dgtMartyrsCryDoneThisGather =>
            game.dgtMartyrsCryDoneThisGather = true
            // Count Acolytes in pool
            val acolytesInPool = DGT.pool.%(_.uclass == Acolyte).num
            // Count Acolytes on enemy faction cards (captured by enemies)
            // Captured units have faction == DGT but region == enemy's prison
            val acolytesCaptured = game.factions.but(DGT)./~(enemy =>
                DGT.units.%(u => u.region == enemy.prison && u.uclass == Acolyte)
            ).num
            val total = acolytesInPool + acolytesCaptured
            if (total > 0) {
                val powerGained = total * DGTValues.MartyrsCry_powerPerAcolyte
                DGT.power += powerGained
                val breakdown = if (acolytesInPool > 0 && acolytesCaptured > 0)
                    " (" + acolytesInPool + " in pool, " + acolytesCaptured + " captured)"
                else if (acolytesCaptured > 0)
                    " (" + acolytesCaptured + " captured)"
                else
                    ""
                DGT.log(MartyrsCry.styled(DGT) + ": gained", powerGained.power, "for", total, "Acolyte".s(total) + breakdown)
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
            // Cancel the elimination by removing the Eliminated tag
            u.remove(Eliminated)
            DGT.log(GodsHand.styled(DGT) + ":", u.uclass.styled(DGT), "ignored elimination in Death's Area")
        }
    }

    // ── AFTER ACTION: nothing special needed for now ────────────────────────
    override def afterAction()(implicit game : Game) {
        // Future: God's Scale flip prompts could go here
    }

    // ── OVERLAY INFO FUNCTION — Faction card text HTML ──────────────────────
    // Provides formatted HTML for the faction card display showing faction name,
    // unique ability (Dominion), core spellbooks, units, and key mechanics.
    def overlayInfo : String = {
        s"""
        <div class="faction-card dgt-faction">
            <div class="faction-header">
                <h2 class="faction-name">${DGT.name}</h2>
                <p class="faction-short">(${DGT.short})</p>
            </div>
            <div class="faction-color-indicator" style="background-color: #808080;"></div>
            <div class="unique-ability">
                <strong>Dominion</strong> (Ongoing):
                When performing the Move Action, Areas with Units you control are adjacent to each other.
            </div>
            <div class="spellbooks-section">
                <h3>Core Spellbooks</h3>
                <ul>
                    <li>${WhiteRider.name} — Pestilence theme</li>
                    <li>${RedRider.name} — War theme</li>
                    <li>${BlackRider.name} — Famine theme</li>
                    <li>${PaleRider.name} — Death theme</li>
                    <li>${MartyrsCry.name} — Power distribution</li>
                    <li>${CosmicChaos.name} — Doom distribution</li>
                </ul>
            </div>
            <div class="units-section">
                <h3>Units</h3>
                <ul>
                    <li>6 Acolytes (standard cultist)</li>
                    <li>4 Conquest (Monster, cost 1)</li>
                    <li>Pestilence (Terror, cost 3, unlocked via ${WhiteRider.name})</li>
                    <li>War (Terror, cost 3, unlocked via ${RedRider.name})</li>
                    <li>Famine (Terror, cost 3, unlocked via ${BlackRider.name})</li>
                    <li>Death (Terror, cost 3, unlocked via ${PaleRider.name})</li>
                </ul>
            </div>
            <div class="mechanics-section">
                <h3>Special Mechanics</h3>
                <p>
                    <strong>Four Horsemen Theme:</strong> The faction unlocks special Terror units
                    (Pestilence, War, Famine, Death) by controlling gates in specific Glyph areas.
                    Each rider spellbook has a dual-sided back (Mayhem, Ruin, Rampage, Despair)
                    that flips under specific conditions.
                </p>
                <p>
                    <strong>Dual-Sided Spellbooks:</strong> Front sides (Riders) remain active;
                    back sides (God's Plagues/Swords/Scales/Hands) unlock advanced effects through
                    battle/gate/ritual triggers.
                </p>
            </div>
        </div>
        """
    }

}
