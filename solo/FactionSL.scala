package cws

import hrf.colmat._

import html._


case object Wizard extends FactionUnitClass(SL, "Wizard", Monster, 1)
case object SerpentMan extends FactionUnitClass(SL, "Serpent Man", Monster, 2)
case object FormlessSpawn extends FactionUnitClass(SL, "Formless Spawn", Monster, 3)
case object Tsathoggua extends FactionUnitClass(SL, "Tsathoggua", GOO, 8)

// FACTION POWER — use .has(), NOT blocked by Moonbeast or Elder Thing
case object DeathFromBelow extends FactionSpellbook(SL, "Death from Below")
// GOO UNIQUE POWER — use .has(), blocked by Elder Thing, NOT by Moonbeast
case object Lethargy extends FactionSpellbook(SL, "Lethargy")

// SPELLBOOK — use .can(), CAN be blocked by Moonbeast
case object Burrow extends FactionSpellbook(SL, "Burrow")
// SPELLBOOK — use .can(), CAN be blocked by Moonbeast
case object EnergyNexus extends FactionSpellbook(SL, "Energy Nexus")
// SPELLBOOK — use .can(), CAN be blocked by Moonbeast
case object EnergyNexusPB extends FactionSpellbook(SL, "Energy Nexus") with BattleSpellbook
// SPELLBOOK — use .can(), CAN be blocked by Moonbeast
case object AncientSorcery extends FactionSpellbook(SL, "Ancient Sorcery")
// SPELLBOOK — use .can(), CAN be blocked by Moonbeast
case object CaptureMonster extends FactionSpellbook(SL, "Capture Monster")
// SPELLBOOK — use .can(), CAN be blocked by Moonbeast
case object DemandSacrifice extends FactionSpellbook(SL, "Demand Sacrifice") with BattleSpellbook
// SPELLBOOK — use .can(), CAN be blocked by Moonbeast
case object CursedSlumber extends FactionSpellbook(SL, "Cursed Slumber")

// SPELLBOOK — use .can(), CAN be blocked by Moonbeast
case object KillsArePains extends FactionSpellbook(SL, "Kills are Pains") with BattleSpellbook

case object Pay3SomeoneGains3 extends Requirement("Pay 3, Someone gains 3 Power")
case object Pay3EverybodyGains1 extends Requirement("Pay 3, Everybody gains 1 Power")
case object Pay3EverybodyLoses1 extends Requirement("Pay 3, Everybody loses 1 Power")
case object Roll6DiceInBattle extends Requirement("Roll 6 dice in Battle")
case object PerformRitual extends Requirement("Perform ritual")
case object AwakenTsathoggua extends Requirement("Awaken Tsathoggua")


case object SL extends Faction { f =>
    def name = "Sleeper"
    def short = "SL"
    def style = "sl"

    val slumber = Slumber(f)
    val sorcery = Sorcery(f)

    override def abilities = $(DeathFromBelow, Lethargy)
    override def library = $(Burrow, EnergyNexus, AncientSorcery, CaptureMonster, DemandSacrifice, CursedSlumber)
    override def requirements(options : $[GameOption]) = $(Pay3SomeoneGains3, Pay3EverybodyGains1, Pay3EverybodyLoses1, Roll6DiceInBattle, PerformRitual, AwakenTsathoggua)

    val allUnits =
        1.times(Tsathoggua) ++
        4.times(FormlessSpawn) ++
        3.times(SerpentMan) ++
        2.times(Wizard) ++
        6.times(Acolyte)

    override def awakenCost(u : UnitClass, r : Region)(implicit game : Game) = u match {
        case Tsathoggua => (f.at(r, FormlessSpawn).any).?((f.has(Immortal) && f.needs(AwakenTsathoggua).not).?(4).|(8))
        case _ => None
    }

    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int =
        units(SerpentMan).num * 1 +
        units(FormlessSpawn).num * (f.all(FormlessSpawn).num + f.all(Tsathoggua).num) +
        units(Tsathoggua).not(Zeroed).num * max(2, opponent.power) +
        neutralStrength(units, opponent)
}


case class DeathFromBelowDoomAction(self : SL) extends OptionFactionAction(DeathFromBelow) with DoomQuestion with Soft with PowerNeutral
case class DeathFromBelowSelectMonsterAction(self : SL, uc : UnitClass) extends BaseFactionAction(DeathFromBelow, uc.styled(self)) with Soft
case class DeathFromBelowAction(self : SL, r : Region, uc : UnitClass) extends BaseFactionAction(DeathFromBelow, uc.styled(self) + " in " + r)

case class LethargyMainAction(self : SL) extends OptionFactionAction(Lethargy) with MainQuestion with PowerNeutral

case class Pay3SomeoneGains3MainAction(self : SL) extends OptionFactionAction("Pay " + 3.power + " and another faction gains " + 3.power) with MainQuestion with Soft
case class Pay3SomeoneGains3Action(self : SL, f : Faction) extends BaseFactionAction("Get spellbook for " + 3.power, "" + f + " gets " + 3.power)

case class Pay3EverybodyLoses1MainAction(self : SL) extends OptionFactionAction("Pay " + 3.power + ", other factions lose " + 1.power + " each") with MainQuestion
case class Pay3EverybodyGains1MainAction(self : SL) extends OptionFactionAction("Pay " + 3.power + ", other factions gain " + 1.power + " each") with MainQuestion

// Easier SBR variants
case class PayLastPowerAction(self : SL) extends OptionFactionAction("Pay your last " + 1.power) with MainQuestion
case class PayLast2PowerAction(self : SL) extends OptionFactionAction("Pay your last " + 2.power) with MainQuestion
case class PayLast3PowerAction(self : SL) extends OptionFactionAction("Pay your last " + 3.power) with MainQuestion

case class CaptureMonsterMainAction(self : SL) extends OptionFactionAction(CaptureMonster) with MainQuestion with Soft
case class CaptureMonsterAction(self : SL, r : Region, f : Faction) extends BaseFactionAction(CaptureMonster, "Capture " + f + " Monster in " + r)
case class CaptureMonsterUnitAction(f : SL, r : Region, self : Faction, uc : UnitClass) extends BaseFactionAction("" + CaptureMonster + " in " + r, uc.styled(self))

case class AncientSorceryMainAction(self : SL) extends OptionFactionAction(AncientSorcery) with MainQuestion with Soft
case class AncientSorceryAction(self : SL, a : Spellbook) extends BaseFactionAction(AncientSorcery, implicit g => "" + a + ((a == Tenebrosum || a == Depravity).??(" — Serpentman is not returned"))) with Soft
case class AncientSorceryUnitAction(self : SL, a : Spellbook, r : Region, uc : UnitClass) extends BaseFactionAction("Access " + a + " with", uc.styled(self) + " from " + r)
// REPLAY-COMPAT ONLY (do not remove): the SL/DC Ancient Sorcery flow was reworked
// on 2026-08-05 (single Serpentman per DC power, sent permanently to DC's card).
// The old flow used a two-Serpentman "second pick" and a "DC bundle". New games
// never emit these actions, but games recorded BEFORE the rework (e.g. "Shift
// against Lunacy") have them in their logs, so the classes + handlers MUST remain
// parseable/replayable or those games crash with "unknown class ...".
case class AncientSorceryDCSecondUnitAction(self : SL, a : Spellbook, r1 : Region, r : Region, uc : UnitClass) extends BaseFactionAction("Access " + a + " with second", uc.styled(self) + " from " + r)
case class AncientSorceryDCBundleMainAction(self : SL) extends OptionFactionAction(Tenebrosum.styled(DC) + " + " + Depravity.styled(DC) + " (Requires two serpentmen - PERMANENT FOR THIS GAME)") with MainQuestion with Soft
case class AncientSorceryDCBundleConfirmAction(self : SL) extends BaseFactionAction("Ancient Sorcery: " + Tenebrosum.styled(DC) + " + " + Depravity.styled(DC), "Confirm".styled("power")) with Soft
case class AncientSorceryDCBundleFirstUnitAction(self : SL, r : Region, uc : UnitClass) extends BaseFactionAction("Send first to " + DC.short.styled(DC) + " card from", r) with Soft
case class AncientSorceryDCBundleSecondUnitAction(self : SL, r1 : Region, r : Region, uc : UnitClass) extends BaseFactionAction("Send second to " + DC.short.styled(DC) + " card from", r)
case class AncientSorceryDoomAction(self : SL) extends OptionFactionAction(AncientSorcery) with DoomQuestion with Soft
case class AncientSorceryPlaceAction(self : SL, r : Region, uc : UnitClass) extends BaseFactionAction("Place " + uc + " in", r)

case class CursedSlumberSaveMainAction(self : SL) extends OptionFactionAction(CursedSlumber) with MainQuestion with Soft
case class CursedSlumberSaveAction(self : SL, r : Region) extends BaseFactionAction("Move gate to " + CursedSlumber + " from", r)
case class CursedSlumberLoadMainAction(self : SL, l : $[Region]) extends OptionFactionAction(CursedSlumber) with MainQuestion with Soft
case class CursedSlumberLoadAction(self : SL, r : Region) extends BaseFactionAction("Move gate from " + CursedSlumber + " to", implicit g => r + self.iced(r))


object SLExpansion extends Expansion {
    def perform(action : Action, soft : VoidGuard)(implicit game : Game) = action @@ {
        // DOOM
        case DoomAction(f : SL) =>
            implicit val asking = Asking(f)

            game.rituals(f)

            if (f.can(Dematerialization))
                + DematerializationDoomAction(f)

            if (f.can(DeathFromBelow) && f.pool.monsters.any)
                + DeathFromBelowDoomAction(f)

            game.reveals(f)

            game.highPriests(f)

            game.hires(f)

            val hasSorcery = f.has(AncientSorcery) && f.at(SL.sorcery, SerpentMan).any
            if (hasSorcery)
                + AncientSorceryDoomAction(f)

            game.doomDone(f, blockDone = hasSorcery)

            asking

        // ACTIONS
        case MainAction(f : SL) if f.acted && game.nexed.any =>
            implicit val asking = Asking(f)

            game.controls(f)

            + NextPlayerAction(f).as("Skip")

            asking

        case MainAction(f : SL) if f.active.not =>
            UnknownContinue

        case MainAction(f : SL) if f.acted =>
            UnknownContinue

        case MainAction(f : SL) =>
            implicit val asking = Asking(f)

            if (f.has(Lethargy) && f.onMap(Tsathoggua).any && game.nexed.none && f.enemies.%(e => e.power > 0 && !e.hibernating).any && ElderThingMindControl.suppresses(f.goo(Tsathoggua)))
                + GroupAction("Lethargy".styled("nt") + " blocked by " + "Elder Thing".styled("nt"))
            else if (f.has(Lethargy) && f.onMap(Tsathoggua).any && game.nexed.none && f.enemies.%(e => e.power > 0 && !e.hibernating).any && !ElderThingMindControl.suppresses(f.goo(Tsathoggua)))
                if (game.options.has(IceAgeAffectsLethargy).not || f.affords(0)(f.goo(Tsathoggua).region))
                    + LethargyMainAction(f)

            if (f.can(Hibernate))
                + HibernateMainAction(f, min(f.power, f.enemies./~(_.goos.distinctBy(_.uclass)).num))

            game.moves(f)

            if (f.has(BeyondOne) && game.gates.num < areas.num && areas.diff(game.gates).%(r => f.affords(1)(r) && f.enemies.%(_.at(r, GOO, ElderGod).any).none).any)
                game.gates.%(r => f.at(r).%(u => u.uclass.cost >= 3 && (u.canMove || u.uclass == HoundOfTindalos)).any).some.foreach {
                    + BeyondOneMainAction(f, _)
                }

            game.captures(f)

            // Capture Monster must be offered wherever base cultist-capture (game.captures) is:
            // that region set includes the moon (BB.moon) and the TB mantle, not just the earthly
            // board regions. Using bare areas.nex here excluded the moon, so Tsathoggua could never
            // be offered Capture Monster while on the moon. Mirror the base-capture region set.
            if (f.can(CaptureMonster) && (areas.nex ++ game.factions.has(BB).??($(BB.moon)) ++ game.tbMantleInPlay.??($(TB.mantle))).%(f.affords(1)).%(r => f.at(r, Tsathoggua).any && (f.enemies.exists(e => e.at(r).monsters.any && e.goos.%(_.region == r).none))).any)
                + CaptureMonsterMainAction(f)

            game.recruits(f)

            game.battles(f)

            game.controls(f)

            game.builds(f)

            if (f.can(CursedSlumber) && game.gates.%(_.glyph == Slumber).none && f.gates.nex.onMap.any)
                + CursedSlumberSaveMainAction(f)

            if (f.has(CursedSlumber) && game.gates.%(_.glyph == Slumber).any)
                areas.nex.%!(game.gates.has).%(f.affords(1)).some.foreach { l =>
                    + CursedSlumberLoadMainAction(f, l)
                }

            game.summons(f)

            game.awakens(f)

            game.independents(f)

            if (f.can(AncientSorcery) && f.units.%(u => u.uclass == SerpentMan && (u.region.onMap || u.region == BB.moon)).nex.any && f.borrowed.num < factions.num - 1)
                + AncientSorceryMainAction(f)

            // 2026-06-06 Fix 75 (Fix 1 via SL): if SL has Tenebrosum via the
            // permanent DC bundle, offer "Repeat <action>" using SL's Sin pool.
            // HB Fix 90 (2026-06-07): also gated by Tenebrosum-legal-to-repeat
            // (see FactionDC.tenebrosumLegalToRepeat). Legality re-checks pool /
            // target / region for the recorded action class so Tenebrosum isn't
            // offered after an iGOO awakens with empty pool, etc.
            if (game.slPermanentBorrowed.has(Tenebrosum)) {
                game.dcLastActionForTenebrosum.foreach { case (a, cost, an) =>
                    val minCost = DCExpansion.tenebrosumMinSinCostPublic(f, a, cost, an)
                    if (minCost > 0 && game.slSin >= minCost && !game.dcTenebrosumGuard
                        && !game.slTenebrosumUsedThisTurn
                        && DCExpansion.tenebrosumLegalToRepeatPublic(f, a, cost, an))
                        + DCTenebrosumMainAction(f, cost, an)
                }
            }

            // Round 8 bug fix (Bug 38): Writhe borrowed via Ancient Sorcery. Offered here
            // in SL's MainAction (same pattern as Beyond One on line ~147) because FBExpansion
            // can't add to SLExpansion's Asking context — SLExpansion handles MainAction(SL)
            // before FBExpansion sees it. Non-FB factions have 0 Desiccated in pool, so
            // killed Acolytes are eliminated instead of replaced. No Infernal Pact discount.
            if (f.can(Writhe) && f.power >= 2)
                + FBWritheMainAction(f)

            if (game.options.has(SleeperEasierSBR)) {
                if (f.needs(Pay3SomeoneGains3) && f.power == 1)
                    + PayLastPowerAction(f)
                if (f.needs(Pay3EverybodyGains1) && f.power == 2)
                    + PayLast2PowerAction(f)
                if (f.needs(Pay3EverybodyLoses1) && f.power == 3)
                    + PayLast3PowerAction(f)
            } else {
                if (f.needs(Pay3SomeoneGains3) && f.power >= 3)
                    + Pay3SomeoneGains3MainAction(f)
                if (f.needs(Pay3EverybodyLoses1) && f.power >= 3)
                    + Pay3EverybodyLoses1MainAction(f)
                if (f.needs(Pay3EverybodyGains1) && f.power >= 3)
                    + Pay3EverybodyGains1MainAction(f)
            }

            game.neutralSpellbooks(f)

            game.libraryActions(f)

            game.highPriests(f)

            game.reveals(f)

            game.endTurn(f)(f.battled.any || game.nexed.any)

            asking

        // AWAKEN
        case AwakenedAction(self, Tsathoggua, r, cost) =>
            if (self.has(Immortal)) {
                self.log("gained", 1.es, "as", Immortal)
                self.takeES(1)
            }

            self.satisfy(AwakenTsathoggua, "Awaken Tsathoggua")

            EndAction(self)

        // DEATH FROM BELOW
        case DeathFromBelowDoomAction(self) =>
            val unitClasses = self.pool.monsters./(_.uclass)

            val minCost = unitClasses.map(_.cost).min
            val ucs = unitClasses.filter(_.cost == minCost).distinct

            Ask(self).each(ucs)(uc => DeathFromBelowSelectMonsterAction(self, uc)).cancelIf(ucs.num > 1)

        case DeathFromBelowSelectMonsterAction(self, uc) =>
            // Moon counts as just another region for Death from Below — a Monster
            // may be placed on the Moon (unit placement onto the Moon is allowed).
            val dfbAll = areas ++ game.factions.has(BB).??($(BB.moon))
            Ask(self).each(dfbAll.%(r => self.at(r).any).some.|(dfbAll))(r => DeathFromBelowAction(self, r, uc)).cancel

        case DeathFromBelowAction(self, r, uc) =>
            self.place(uc, r)
            self.log("placed", uc, "in", r, "with", DeathFromBelow)
            self.oncePerTurn :+= DeathFromBelow
            CheckSpellbooksAction(DoomAction(self))

        // LETHARGY
        case LethargyMainAction(self) =>
            if (options.has(IceAgeAffectsLethargy))
                self.payTax(self.goo(Tsathoggua).region)

            // Round 8 Bug 36/54: record Tsathoggua's region for Cyclopean Gaze.
            // Lethargy moves no units (snapshot delta = 0), but CG should fire if
            // Tsathoggua's region has FB Revenants/Ghatanothoa and SL non-Building units.
            // Recorded here (not in FBExpansion) because SLExpansion handles LethargyMainAction
            // before FBExpansion in the expansion dispatch order.
            // Bug 54: now appends to the unified fbCyclopeanGazeActionRegions list.
            if (game.factions.has(FB))
                game.fbCyclopeanGazeActionRegions :+= self.goo(Tsathoggua).region

            self.log("was sleeping")
            self.battled = areas
            EndAction(self)

        // PAY 3 POWER
        case Pay3SomeoneGains3MainAction(self) =>
            Ask(self).each(self.enemies)(e => Pay3SomeoneGains3Action(self, e)).cancel

        case Pay3SomeoneGains3Action(self, f) =>
            self.power -= 3
            f.power += 3
            self.log("spent", 3.power, "and", f, "gained", 3.power)
            self.satisfy(Pay3SomeoneGains3, "Provide 3 Power")
            EndAction(self)

        case Pay3EverybodyLoses1MainAction(self) =>
            self.power -= 3
            self.enemies.%(_.power > 0).foreach(_.power -= 1)
            self.log("spent", 3.power, "and each other faction lost", 1.power)
            self.satisfy(Pay3EverybodyLoses1, "Everybody loses 1 power")
            EndAction(self)

        case Pay3EverybodyGains1MainAction(self) =>
            self.power -= 3
            self.enemies.foreach(_.power += 1)
            self.log("spent", 3.power, "and each other faction gained", 1.power)
            self.satisfy(Pay3EverybodyGains1, "Everybody gains 1 power")
            EndAction(self)

        // EASIER SBR VARIANTS
        case PayLastPowerAction(self) =>
            self.power -= 1
            self.log("paid last", 1.power)
            self.satisfy(Pay3SomeoneGains3, "Pay your last power")
            EndAction(self)

        case PayLast2PowerAction(self) =>
            self.power -= 2
            self.log("paid last", 2.power)
            self.satisfy(Pay3EverybodyGains1, "Pay your last 2 power")
            EndAction(self)

        case PayLast3PowerAction(self) =>
            self.power -= 3
            self.log("paid last", 3.power)
            self.satisfy(Pay3EverybodyLoses1, "Pay your last 3 power")
            EndAction(self)

        // CAPTURE MONSTER
        case CaptureMonsterMainAction(self) =>
            val r = self.goo(Tsathoggua).region
            // Bastet (and any enemy GOO) blocks SL Capture Monster in her region, mirroring how a
            // GOO blocks normal cultist captures. Note: unlike normal captures, Tsathoggua's own
            // presence does NOT override this protection (Capture Monster is not a canCapture path).
            // Faction-level .goos includes ElderGod/Bastet and Holy-Ground cathedrals.
            Ask(self).each(factionlike.but(self).%(e => e.at(r).monsters.any && e.goos.%(_.region == r).none))(e => CaptureMonsterAction(self, r, e)).cancel

        case CaptureMonsterAction(self, r, f) =>
            self.power -= 1

            Ask(f).each(f.at(r).monsters.sortBy(_.uclass.cost))(u => CaptureMonsterUnitAction(self, r, u.faction, u.uclass))

        case CaptureMonsterUnitAction(self, r, f, uc) =>
            val m = f.at(r).one(uc)
            game.eliminate(m)
            m.region = self.prison
            self.log("captured", m, "in", r)
            // Round 8 Bug 76: register as a CG edge case. CaptureMonster is a
            // zero-delta action — only the captured monster's faction loses a
            // unit, and the captor (SL) count is unchanged. The snapshot-delta
            // detector in FB's AfterAction handler won't see any new enemy
            // arrivals, so CG won't fire. Registering here ensures CG triggers
            // when SL captures a monster in a region with FB Revenants/Ghatanothoa.
            if (game.factions.has(FB))
                game.fbCyclopeanGazeActionRegions :+= r
            EndAction(self)

        // ANCIENT SORCERY
        case AncientSorceryMainAction(self) =>
            // DC (Defiler's Court) is unique: it has TWO unique powers (Depravity +
            // Tenebrosum), and Serpentmen used to copy a DC power are NEVER returned
            // (they stay permanently on DC's faction card, and SL never regains them
            // or gets Power from them). Every OTHER faction has a single ability that
            // is borrowed for the Action Phase and the Serpentman returns in the Doom
            // Phase. So: for DC, offer BOTH of its not-yet-copied uniques, each with
            // an appended "Serpentman is not returned" note; once one is copied, only
            // the other remains offered (on a later, separate Ancient Sorcery action).
            // For every other enemy, offer its single head ability as before.
            val dcAbilities = game.setup.has(DC).??(DC.abilities.diff(game.slPermanentBorrowed))
            val otherAbilities = self.enemies.but(DC)./~(_.abilities.headOption).diff(self.borrowed).diff(game.slPermanentBorrowed)
            Ask(self).each(otherAbilities ++ dcAbilities)(a => AncientSorceryAction(self, a)).cancel

        case AncientSorceryAction(self, a) =>
            // Moon counts as just another region for Ancient Sorcery — a Serpentman
            // on the Moon may be used to access a spellbook.
            Ask(self).each(self.units.%(u => u.uclass == SerpentMan && (u.region.onMap || u.region == BB.moon)).nex)(u => AncientSorceryUnitAction(self, a, u.region, u.uclass)).cancel

        // DC uniques: ONE Serpentman, sent PERMANENTLY to DC's faction card (never
        // returned), and the power is copied permanently (slPermanentBorrowed) rather
        // than for the single Action Phase. Only ever ONE Serpentman per action — the
        // second DC power is copied on a separate, later Ancient Sorcery action.
        //
        // REPLAY-COMPAT: games recorded under the OLD two-Serpentman flow have an
        // AncientSorceryDCSecondUnitAction as the NEXT recorded action after this one.
        // If we see that, take the OLD path (chain to a second pick) so the recorded
        // action still applies and the replay stays in sync. Live play (no such next
        // recorded action) uses the new single-Serpentman permanent-copy path.
        case AncientSorceryUnitAction(self, a, r, uc) if isDCAbility(a) =>
            val replayWantsSecond = game.nextReplayActionHint.exists(_.startsWith("AncientSorceryDCSecondUnitAction"))
            if (replayWantsSecond) {
                val remaining = self.units.%(u => u.uclass == SerpentMan && (u.region.onMap || u.region == BB.moon)).nex.%(u => !(u.region == r && self.at(r, SerpentMan).num == 1))
                Ask(self).each(remaining)(u => AncientSorceryDCSecondUnitAction(self, a, r, u.region, u.uclass)).cancel
            }
            else {
                self.power -= 1
                self.at(r).one(uc).region = DC.reserve
                game.slPermanentBorrowed :+= a
                self.log("Ancient Sorcery: permanently copied", a.styled(DC), "(", SerpentMan, "from", r, "sent to", DC.short.styled(DC), "card, not returned )")
                EndAction(self)
            }

        // REPLAY-COMPAT (old two-Serpentman DC copy): second pick sends BOTH Serpentmen
        // to SL.sorcery and borrows the ability for the AP (the pre-rework behaviour).
        case AncientSorceryDCSecondUnitAction(self, a, r1, r2, uc) =>
            self.power -= 1
            self.at(r1).one(SerpentMan).region = SL.sorcery
            self.at(r2).one(SerpentMan).region = SL.sorcery
            self.borrowed :+= a
            self.log("sent", 2, SerpentMan, "(", r1, "+", r2, ") to access", a, "(DC double-cost)")
            EndAction(self)

        case AncientSorceryUnitAction(self, a, r, uc) =>
            self.power -= 1
            self.at(r).one(uc).region = SL.sorcery
            self.borrowed :+= a
            self.log("sent", uc, "from", r, "to access", a)
            EndAction(self)

        // REPLAY-COMPAT (old DC bundle — copied both uniques in one 2-Serpentman cast).
        case AncientSorceryDCBundleMainAction(self) =>
            implicit val asking = Asking(self)
            + AncientSorceryDCBundleConfirmAction(self)
            + CancelAction
            asking

        case AncientSorceryDCBundleConfirmAction(self) =>
            Ask(self).each(self.units.%(u => u.uclass == SerpentMan && (u.region.onMap || u.region == BB.moon)).nex)(u => AncientSorceryDCBundleFirstUnitAction(self, u.region, u.uclass)).cancel

        case AncientSorceryDCBundleFirstUnitAction(self, r, uc) =>
            val remaining = self.units.%(u => u.uclass == SerpentMan && (u.region.onMap || u.region == BB.moon)).nex.%(u => !(u.region == r && self.at(r, SerpentMan).num == 1))
            Ask(self).each(remaining)(u => AncientSorceryDCBundleSecondUnitAction(self, r, u.region, u.uclass)).cancel

        case AncientSorceryDCBundleSecondUnitAction(self, r1, r2, uc) =>
            self.power -= 1
            self.at(r1).one(SerpentMan).region = DC.reserve
            self.at(r2).one(SerpentMan).region = DC.reserve
            game.slPermanentBorrowed :+= Tenebrosum
            game.slPermanentBorrowed :+= Depravity
            game.slDCBundleTaken = true
            self.log("Ancient Sorcery: permanently copied", Tenebrosum.styled(DC), "+", Depravity.styled(DC),
                "(2", SerpentMan, "sent to", DC.short.styled(DC), "card from", r1, "+", r2, ")")
            EndAction(self)

        case AncientSorceryDoomAction(self) =>
            Ask(self).each(areas ++ game.factions.has(BB).??($(BB.moon)))(r => AncientSorceryPlaceAction(self, r, SerpentMan)).cancel

        case AncientSorceryPlaceAction(self, r, uc) =>
            self.at(SL.sorcery).one(uc).region = r
            self.power += 1
            self.log("placed", uc, "in", r, "with", AncientSorcery, "and gained", 1.power)
            CheckSpellbooksAction(DoomAction(self))

        // CURSED SLUMBER
        case CursedSlumberSaveMainAction(self) =>
            Ask(self).each(self.gates.nex)(r => CursedSlumberSaveAction(self, r)).cancel

        case CursedSlumberSaveAction(self, r) =>
            self.power -= 1

            self.gates :-= r
            self.gates :+= SL.slumber
            game.gates :-= r
            game.gates :+= SL.slumber

            self.at(r).%(_.onGate).only.region = SL.slumber

            self.log("moved gate from", r, "to", CursedSlumber)

            EndAction(self)

        case CursedSlumberLoadMainAction(self, l) =>
            Ask(self).each(l)(r => CursedSlumberLoadAction(self, r)).cancel

        case CursedSlumberLoadAction(self, r) if r != BB.moon =>
            self.power -= 1
            self.payTax(r)

            self.gates :-= SL.slumber
            self.gates :+= r
            game.gates :-= SL.slumber
            game.gates :+= r

            if (self.at(SL.slumber, Cultist).any)
                self.at(SL.slumber).one(Cultist).region = r

            self.log("moved gate from", CursedSlumber, "to", r)

            EndAction(self)

        // ...
        case _ => UnknownContinue
    }

    // 2026-06-06 Fix 75 (Fix 3): DC unique abilities (Tenebrosum + Depravity)
    // require TWO Serpentmen per copy via standard Ancient Sorcery.
    private def isDCAbility(a : Spellbook) : Boolean = a == Tenebrosum || a == Depravity
}
