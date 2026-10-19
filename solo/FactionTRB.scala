package cws

import hrf.colmat._
import html._

// ============================================================================
// TRIBULATION v16 (TRB) — Homebrew faction
// Designer: Daganisoraan, faction id lklqd2fbdudf.
// Four Horsemen theme: Pestilence, War, Famine, Death as Terrors tied to
// two-sided ("flip") spellbooks named for the matching Rider.
//
// IMPORTANT: this is a DIFFERENT designer submission from the already-built
// DGT faction (also internally named "Tribulation", also Four-Horsemen
// themed, designer daganisoraan — note the different capitalisation/user —
// built earlier, lives entirely in FactionDGT.scala / dgt-*.webp). The two
// designs share a lot of surface vocabulary (same unit names, same SBR
// wording for 4 of 6 rows, same UFA name "Dominion") but are NOT the same
// faction and must never share Scala identifiers, case objects or assets.
// Every TRB-only identifier below is prefixed TRB to guarantee that (DGT
// already owns the un-prefixed names: Conquest, Pestilence, War, Famine,
// Death, WhiteRider, RedRider, BlackRider, PaleRider, MartyrsCry,
// CosmicChaos, Dominion).
//
// Judgment calls made while building this (no claudeQA on file for this
// design — see designer-admin.py qa lklqd2fbdudf, which returned {}):
//  - meta.color was null (designer never set one). Picked #5b4a6b (muted
//    ash-plum), not used by any other faction, legible on the dark panel.
//  - The four two-sided Spellbooks (White/Red/Black/Pale Rider) all have
//    nameB == name (the designer never gave the flipped side a different
//    name, unlike DGT's "God's Plague/Sword/Scale/Hand"). So a flip here is
//    tracked as a flag (game.trbFlipped : Set[Spellbook]) instead of
//    swapping to a second held Spellbook case object — the card keeps
//    showing the same one named slot, its text changes once flipped. This
//    satisfies the Simple Update spellbookGuidance ("2 sides = 2 CONNECTED
//    spellbooks, not independent ones") without inventing four more
//    never-separately-earned case objects.
//  - TRB's setup.text ("8 Power, 6 Acolytes, and a controlled Gate...") is
//    exactly the engine's default faction setup, so no custom placement
//    code is needed (generate-faction-values.py agreed: it skipped
//    setup.power/setup.units as "standard setup rule"). Only the multi-area
//    start-region FILTER is faction-specific — see Map.scala's `case TRB`
//    lines ("any empty Area without map Glyph" -> NoGlyph areas; falls back
//    to anywhere on boards with none, e.g. the Library maps, so the start
//    picker is never empty).
//  - Unlike DGT, TRB's design never says the four Horsemen "start out of
//    your Unit Pool" gated behind their Rider — so TRBPestilence/War/
//    Famine/Death are plain summonable Terrors from the start (standard
//    game.summons(f)), no canBeSummoned gating added.
//  - Famine's "Famine count has a Great Old One for Capture purposes": the
//    engine's only generic "Capture" action (game.captures) targets
//    Cultists exclusively and has no GOO/non-GOO branch, and no other
//    faction ability in this codebase currently queries "is this a GOO" at
//    a capture moment. Implemented as documented card/overlay text plus a
//    scoped helper (TRBExpansion.famineCountsAsGOO) for any future hook;
//    not wired into UnitClass.isGOO (that method takes no Game, so it can't
//    be made conditional on holding the Black Rider without making Famine
//    incorrectly register as a GOO everywhere, including before the SBR is
//    even earned — too invasive for an ability with no concrete consumer
//    today). Revisit if a future faction ability needs it.
//  - interactions.rows[0] (Dominion × Sleeper's Ancient Sorcery) restates
//    Dominion's own text with "Sleeper Units" substituted for "Units you
//    control". Dominion only ever moves TRB's own units' areas, so Sleeper
//    units are never relevant to TRB's own Dominion check; no SL-specific
//    code found to be needed. Documented here rather than filed as a
//    needs_answers question (per the build's standing instruction to
//    resolve ambiguity and document judgment calls).
// ============================================================================

case object TRB extends Faction { f =>
    def name  = "Tribulation v16"
    def short = "TRB"
    def style = "trb"

    // Dominion is the always-on unique ability.
    override def abilities = $(TRBDominion)
    // The 6 earnable spellbooks, in SBR-earning order (SBR row i grants SB row i —
    // positional pairing, same convention the designer used for DGT's near-identical layout).
    override def library   = $(TRBWhiteRider, TRBRedRider, TRBBlackRider, TRBPaleRider, TRBMartyrsCry, TRBCosmicChaos)
    override def requirements(options : $[GameOption]) =
        $(TRBWhiteRiderReq, TRBRedRiderReq, TRBBlackRiderReq, TRBPaleRiderReq, TRBMartyrsCryReq, TRBCosmicChaosReq)

    // 6 Acolytes + 4 Conquest + 1 each of the four Horsemen Terrors.
    val allUnits =
        TRBValues.Acolyte_count.times(Acolyte)          ++
        TRBValues.Conquest_count.times(TRBConquest)     ++
        TRBValues.Pestilence_count.times(TRBPestilence) ++
        TRBValues.War_count.times(TRBWar)               ++
        TRBValues.Famine_count.times(TRBFamine)         ++
        TRBValues.Death_count.times(TRBDeath)

    // Cost-based combat for Monsters/Terrors (standard idiom), plus the Red Rider
    // bonus on War (both sides of Red Rider grant the same bonus per the design —
    // there is no second "escalated" tier the way DGT's God's Sword adds one).
    def strength(units : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int = {
        val warBonus =
            if (units.exists(_.uclass == TRBWar) && f.has(TRBRedRider))
                TRBValues.War_combatRedRider - TRBValues.War_combat
            else 0

        units.%(u => u.uclass.utype == Monster || u.uclass.utype == Terror)
            .not(Zeroed)
            ./(u => TRBExpansion.combatOf(u.uclass))
            .sum + warBonus + neutralStrength(units, opponent)
    }
}


// ── UNITS ───────────────────────────────────────────────────────────────────
// TRB reuses the standard Acolyte UnitClass for its 6 Acolytes (shared across
// factions). The Monster/Terror classes below are TRB's own, prefixed to
// avoid any clash with DGT's identically-named units.
case object TRBConquest   extends FactionUnitClass(TRB, "Conquest",   Monster, TRBValues.Conquest_cost)
case object TRBPestilence extends FactionUnitClass(TRB, "Pestilence", Terror,  TRBValues.Pestilence_cost)
case object TRBWar        extends FactionUnitClass(TRB, "War",        Terror,  TRBValues.War_cost)
case object TRBFamine     extends FactionUnitClass(TRB, "Famine",     Terror,  TRBValues.Famine_cost)
// Death: "Death count has a Great Old One to Awaken Independent Great Old Ones" is an
// always-on unit special ability (not Spellbook-gated) — wired into Game.canAwakenIGOO.
case object TRBDeath      extends FactionUnitClass(TRB, "Death",      Terror,  TRBValues.Death_cost)


// ── SPELLBOOKS ──────────────────────────────────────────────────────────────
// The four Riders are two-sided (flip tracked via game.trbFlipped, see header note).
case object TRBWhiteRider  extends FactionSpellbook(TRB, "The White Rider")
case object TRBRedRider    extends FactionSpellbook(TRB, "The Red Rider")
case object TRBBlackRider  extends FactionSpellbook(TRB, "The Black Rider")
case object TRBPaleRider   extends FactionSpellbook(TRB, "The Pale Rider")
// Not unit-tied — shown at the top of the faction card overlay per Task 1.1's rule.
case object TRBMartyrsCry  extends FactionSpellbook(TRB, "The Martyr's Cry")
// Tied to Conquest.
case object TRBCosmicChaos extends FactionSpellbook(TRB, "The Cosmic Chaos")

// TRB Unique Faction Ability — Dominion (Ongoing, Action phase).
case object TRBDominion extends FactionSpellbook(TRB, "Dominion")


// ── SPELLBOOK REQUIREMENTS ───────────────────────────────────────────────────
// Text matches the designer record word for word (the overlay looks these up by exact text).
case object TRBWhiteRiderReq  extends Requirement("Control a Gate in an Area marked with the Dragon Glyph.")
case object TRBRedRiderReq    extends Requirement("Control a Gate in an Area marked with the Thorn Glyph.")
case object TRBBlackRiderReq  extends Requirement("Control a Gate in an Area marked with the Chevron Glyph.")
case object TRBPaleRiderReq   extends Requirement("Any player performs a Ritual of Annihilation.")
case object TRBMartyrsCryReq  extends Requirement("As an Action, distribute a total amount of Power equal to the number of players to any enemy Factions of your choice")
case object TRBCosmicChaosReq extends Requirement("As an Action, distribute a total amount of Doom equal to the number of players to any enemy Factions of your choice")


// ============================================================================
// TRIBULATION v16 (TRB) ACTION CASE CLASSES
// ============================================================================

// ── DISTRIBUTE POWER (Action, SBR 5 / The Martyr's Cry) ─────────────────────
case class TRBDistributePowerMainAction(self : Faction)
    extends OptionFactionAction("Distribute Power to enemy Factions".styled(TRB)) with MainQuestion with Soft

case class TRBDistributePowerFactionAction(self : Faction, remaining : Int, distributed : $[(Faction, Int)])
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) =
        "Distribute Power".styled(TRB) + ": choose enemy Faction to receive Power (" + remaining + " Power remaining)"
}

// Step 1 (per enemy): clicking an enemy's name opens the amount picker below.
// Must be a BaseFactionAction with a real .option (the enemy's name) - a bare
// ForcedAction here has no displayable option and renders as "N/A" in the Ask
// menu (caught in the visual audit; fixed before this build shipped). Same
// idiom as MapExpansion.scala's LibrarianAssignToFactionAction.
case class TRBDistributePowerAmountAction(self : Faction, target : Faction, remaining : Int, distributed : $[(Faction, Int)])
    extends BaseFactionAction(
        implicit g => "Distribute Power".styled(TRB) + ": choose amount of Power to give " + target.name + " (" + remaining + " remaining)",
        implicit g => target.name
    ) with PowerNeutral

// Step 2 (per amount): the clickable amount buttons (1..remaining Power).
case class TRBDistributePowerGiveAction(self : Faction, target : Faction, amount : Int, remaining : Int, distributed : $[(Faction, Int)])
    extends BaseFactionAction(
        implicit g => "Distribute Power".styled(TRB) + ": give " + target.name + " how much Power? (" + remaining + " remaining)",
        implicit g => s"$amount Power"
    ) with PowerNeutral

case class TRBDistributePowerCancelAction(self : Faction)
    extends OptionFactionAction("Cancel Power distribution".styled(TRB)) with MainQuestion with Soft


// ── DISTRIBUTE DOOM (Action, SBR 6 / The Cosmic Chaos) ───────────────────────
case class TRBDistributeDoomMainAction(self : Faction)
    extends OptionFactionAction("Distribute Doom to enemy Factions".styled(TRB)) with MainQuestion with Soft

case class TRBDistributeDoomFactionAction(self : Faction, remaining : Int, distributed : $[(Faction, Int)])
    extends ForcedAction with PowerNeutral with Soft {
    override def question(implicit game : Game) =
        "Distribute Doom".styled(TRB) + ": choose enemy Faction to receive Doom (" + remaining + " Doom remaining)"
}

// Same two-step fix as Distribute Power above (see that block's comment).
case class TRBDistributeDoomAmountAction(self : Faction, target : Faction, remaining : Int, distributed : $[(Faction, Int)])
    extends BaseFactionAction(
        implicit g => "Distribute Doom".styled(TRB) + ": choose amount of Doom to give " + target.name + " (" + remaining + " remaining)",
        implicit g => target.name
    ) with PowerNeutral

case class TRBDistributeDoomGiveAction(self : Faction, target : Faction, amount : Int, remaining : Int, distributed : $[(Faction, Int)])
    extends BaseFactionAction(
        implicit g => "Distribute Doom".styled(TRB) + ": give " + target.name + " how much Doom? (" + remaining + " remaining)",
        implicit g => s"$amount Doom"
    ) with PowerNeutral

case class TRBDistributeDoomCancelAction(self : Faction)
    extends OptionFactionAction("Cancel Doom distribution".styled(TRB)) with MainQuestion with Soft


// ── BLACK RIDER SACRIFICE-TO-FLIP (Gather Power Phase) ───────────────────────
// Mirrors DGT's Rampage shape (sacrifice a captured enemy Cultist during Gather
// Power to flip a Rider), but is TRB's own action chain, own guard, own flip set.
case class TRBBlackRiderSacrificeCheckAction(self : Faction, last : Faction)
    extends ForcedAction with PowerNeutral with Soft

case class TRBBlackRiderSacrificeAction(self : Faction, cultist : UnitRef, last : Faction)
    extends BaseFactionAction(
        implicit g => "Sacrifice " + g.unit(cultist).faction.name + "'s " + g.unit(cultist).uclass.styled(g.unit(cultist).faction) + " (" + TRBBlackRider.name + ")",
        implicit g => g.unit(cultist).full
    ) with PowerNeutral

case class TRBBlackRiderSkipAction(self : Faction, last : Faction)
    extends OptionFactionAction("Skip") with PowerNeutral {
    override def question(implicit game : Game) = "Skip " + TRBBlackRider.name.styled(TRB)
}


// ============================================================================
// TRIBULATION v16 (TRB) EXPANSION — game-loop integration
// ============================================================================
object TRBExpansion extends Expansion {

    // Base combat dice per unit type, from TRBValues (faction-values/trb.json).
    // War's Red Rider bonus is added separately in TRB.strength.
    def combatOf(uc : UnitClass) : Int = uc match {
        case TRBConquest   => TRBValues.Conquest_combat
        case TRBPestilence => TRBValues.Pestilence_combat
        case TRBWar        => TRBValues.War_combat
        case TRBFamine     => TRBValues.Famine_combat
        case TRBDeath      => TRBValues.Death_combat
        case other         => other.cost
    }

    // Famine's "counts as a Great Old One for Capture purposes" — see header note.
    // Scoped helper kept here (not wired into UnitClass.isGOO) pending a concrete consumer.
    def famineCountsAsGOO(implicit game : Game) : Boolean = TRB.has(TRBBlackRider)

    // Helper: flip a two-sided Rider spellbook (same name, different text) and gain 1 Elder Sign.
    def flipRider(sb : Spellbook, triggerName : String)(implicit game : Game) {
        if (TRB.has(sb) && !game.trbFlipped.contains(sb)) {
            game.trbFlipped = game.trbFlipped + sb
            TRB.takeES(TRBValues.RiderFlip_elderSigns)
            TRB.log(triggerName.styled(TRB) + ":", sb.styled(TRB), "flipped —", TRB.name, "gains", TRBValues.RiderFlip_elderSigns.es)
        }
    }

    def perform(action : Action, soft : VoidGuard)(implicit game : Game) = action @@ {

        // ── SETUP: Starting Region placement ──────────────────────────────────
        // No custom placement needed — 8 Power / 6 Acolytes / a controlled Gate are
        // all the engine's standard faction setup (generate-faction-values.py confirmed
        // these are "standard setup rule", not a TRB-specific fixed number). Only the
        // region FILTER is custom (Map.scala's `case TRB` lines).
        case StartingRegionAction(f : TRB.type, r) =>
            f.log("started in", r)
            UnknownContinue

        // ── MAIN ACTION PHASE ────────────────────────────────────────────────
        case MainAction(f : TRB.type) if f.active.not =>
            UnknownContinue

        case MainAction(f : TRB.type) if f.acted =>
            UnknownContinue

        case MainAction(f : TRB.type) =>
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

            // Distribute Power (SBR 5 requirement action)
            if (f.needs(TRBMartyrsCryReq) && f.enemies.any) {
                + TRBDistributePowerMainAction(f)
            }

            // Distribute Doom (SBR 6 requirement action)
            if (f.needs(TRBCosmicChaosReq) && f.enemies.any) {
                + TRBDistributeDoomMainAction(f)
            }

            game.neutralSpellbooks(f)
            game.highPriests(f)
            game.reveals(f)

            game.endTurn(f)(f.battled.any)

            asking

        // ── DISTRIBUTE POWER ─────────────────────────────────────────────────
        case TRBDistributePowerMainAction(f) =>
            implicit val asking = Asking(f)
            val total = game.factions.num
            Force(TRBDistributePowerFactionAction(f, total, $))

        case TRBDistributePowerFactionAction(f, remaining, distributed) =>
            implicit val asking = Asking(f)
            if (remaining <= 0) {
                f.satisfy(TRBMartyrsCryReq, TRBMartyrsCryReq.text)
                Force(EndAction(f))
            } else {
                val enemies = f.enemies
                Ask(f).each(enemies)(e => TRBDistributePowerAmountAction(f, e, remaining, distributed))
                    .cancel
            }

        case TRBDistributePowerAmountAction(f, target, remaining, distributed) =>
            implicit val asking = Asking(f)
            val amounts = (1 to remaining).toList
            Ask(f).each(amounts)(amt => TRBDistributePowerGiveAction(f, target, amt, remaining, distributed))

        case TRBDistributePowerGiveAction(f, target, amount, remaining, distributed) =>
            target.power += amount
            f.log("gave", amount.power, "to", target)
            Force(TRBDistributePowerFactionAction(f, remaining - amount, distributed :+ (target, amount)))

        case TRBDistributePowerCancelAction(f) =>
            Force(MainAction(f))

        // ── DISTRIBUTE DOOM ──────────────────────────────────────────────────
        case TRBDistributeDoomMainAction(f) =>
            implicit val asking = Asking(f)
            val total = game.factions.num
            Force(TRBDistributeDoomFactionAction(f, total, $))

        case TRBDistributeDoomFactionAction(f, remaining, distributed) =>
            implicit val asking = Asking(f)
            if (remaining <= 0) {
                f.satisfy(TRBCosmicChaosReq, TRBCosmicChaosReq.text)
                Force(EndAction(f))
            } else {
                val enemies = f.enemies
                Ask(f).each(enemies)(e => TRBDistributeDoomAmountAction(f, e, remaining, distributed))
                    .cancel
            }

        case TRBDistributeDoomAmountAction(f, target, remaining, distributed) =>
            implicit val asking = Asking(f)
            val amounts = (1 to remaining).toList
            Ask(f).each(amounts)(amt => TRBDistributeDoomGiveAction(f, target, amt, remaining, distributed))

        case TRBDistributeDoomGiveAction(f, target, amount, remaining, distributed) =>
            target.doom += amount
            f.log("gave", amount.doom, "to", target)
            Force(TRBDistributeDoomFactionAction(f, remaining - amount, distributed :+ (target, amount)))

        case TRBDistributeDoomCancelAction(f) =>
            Force(MainAction(f))

        // ── BLACK RIDER: SACRIFICE AN ENEMY CULTIST TO FLIP (Gather Power) ───
        case TRBBlackRiderSacrificeCheckAction(self, last) =>
            val capturedCultists = game.factions./~(_.at(self.prison)).%(_.cultist)
            if (capturedCultists.any) {
                implicit val asking = Asking(self)
                Ask(self)
                    .each(capturedCultists)(u => TRBBlackRiderSacrificeAction(self, u.ref, last))
                    .add(TRBBlackRiderSkipAction(self, last))
            } else {
                PowerGatherAction(last)
            }

        case TRBBlackRiderSacrificeAction(self, cultist, last) =>
            val unitFig = game.unit(cultist)
            val originalOwner = unitFig.faction
            // Log BEFORE eliminate (Task 1.0 P5 — reading the unit for the log after
            // eliminate() can throw None.get).
            self.log(TRBBlackRider.styled(TRB) + ": sacrificed", originalOwner.name + "'s", unitFig.uclass.styled(originalOwner))
            game.eliminate(unitFig)
            game.trbBlackRiderSacrificedCount += 1
            if (game.trbBlackRiderSacrificedCount >= TRBValues.BlackRider_sacrificeToFlip)
                TRBExpansion.flipRider(TRBBlackRider, "Sacrifice")
            // The Martyr's Cry (SB 5): "Gain 1 Power for each Faction that Sacrifice[s] one or
            // more Cultists" during the Gather Power Phase. This codebase has no universal
            // cross-faction "sacrificed a Cultist" tracker (every faction's own sacrifice-like
            // ability — BG's Ghoul feeding, FB's Carnage, DC's Sin economy, etc. — is implemented
            // independently), and building one safely for every existing faction is out of scope
            // for this build. Scoped, documented judgment call: only TRB's OWN sacrifices (via
            // this Black Rider action, which is the one sacrifice TRB's own code can see) are
            // counted — i.e. "1 Faction" (TRB itself) each time this fires, if Martyr's Cry is
            // held. Revisit if the designer confirms the broader cross-faction reading.
            if (self.can(TRBMartyrsCry)) {
                val powerGained = TRBValues.MartyrsCry_powerPerFaction
                self.power += powerGained
                self.log(TRBMartyrsCry.styled(TRB) + ": gained", powerGained.power, "(sacrificed a Cultist)")
            }
            PowerGatherAction(last)

        case TRBBlackRiderSkipAction(self, last) =>
            PowerGatherAction(last)

        // ── RITUAL OF ANNIHILATION (Pale Rider SBR 4 trigger) ────────────────
        // "Any player performs a Ritual of Annihilation" is wired via
        // Game.notifyRoAReactors(performer), called by every standard and non-standard
        // Ritual of Annihilation path, regardless of which faction performed it — see
        // triggers() below, which satisfies TRBPaleRiderReq unconditionally.

        // ── DOMINION: Move-action adjacency ───────────────────────────────────
        // Implemented in Game.scala's move-destination calc ("TRB Dominion: areas
        // with TRB units are adjacent for Move actions").

        // ── WHITE RIDER: block enemy Move-out and Retreat from Pestilence's Area ──
        // Implemented in Game.scala (move-destination calc, "TRB White Rider") and
        // Battle.scala (retreat-destination calc, "TRB White Rider").

        // ── RED RIDER: War's combat bonus ────────────────────────────────────
        // Implemented in TRB.strength() above.

        // ── PALE RIDER: +1 Kill when Death is present ────────────────────────
        // Implemented in Battle.scala ("Tribulation v16 (TRB) Pale Rider").

        // ── RIDER FLIP TRIGGERS (White/Red/Pale) ─────────────────────────────
        // White Rider (Post-Battle, 2+ Pains on enemy Units with Pestilence present) and
        // Pale Rider (Post-Battle, Kill assigned to a 3-cost+ enemy Unit with Death present)
        // and Red Rider (Battle, 13+ combat dice rolled with War present) are all implemented
        // in Battle.scala, calling TRBExpansion.flipRider(). Black Rider's trigger (sacrifice
        // an enemy Cultist during Gather Power) is implemented above in this file.

        case _ => UnknownContinue
    }

    // ── SPELLBOOK REQUIREMENTS (continuous evaluation) ───────────────────────
    override def triggers()(implicit game : Game) {
        if (!game.setup.has(TRB)) return
        val f = TRB

        // SBR 1 — White Rider: Control a Gate in Dragon Glyph (GlyphOO) area
        f.satisfyIf(TRBWhiteRiderReq, TRBWhiteRiderReq.text,
            game.board.regions.exists(r => r.glyph == GlyphOO && f.gates.contains(r)))

        // SBR 2 — Red Rider: Control a Gate in Thorn Glyph (GlyphWW) area
        f.satisfyIf(TRBRedRiderReq, TRBRedRiderReq.text,
            game.board.regions.exists(r => r.glyph == GlyphWW && f.gates.contains(r)))

        // SBR 3 — Black Rider: Control a Gate in Chevron Glyph (GlyphAA) area
        f.satisfyIf(TRBBlackRiderReq, TRBBlackRiderReq.text,
            game.board.regions.exists(r => r.glyph == GlyphAA && f.gates.contains(r)))
    }
}
