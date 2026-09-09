package cws

import hrf.colmat._

// ============================================================================
// THE INVASION (TI) BOT — AI evaluation logic for all TI-specific actions.
//
// Structural model: BotX + GameEvaluation (the modern per-faction-bot idiom
// shared by every other recent HomeBrew faction — see BotCS.scala/BotTB.scala),
// NOT the legacy Bot3 case-class shared by GC/CC/BG/YS/BB/TS. This was the
// generic placeholder TI used before this file existed (CthulhuWarsSolo.scala
// dispatched `case TI => Bot3(TI).ask(...)`); this file replaces that.
//
// Model factions chosen (per the Implementation Guide §3.16 hint — "whichever
// existing faction most closely combines an Awaken-by-permanent-unit-removal
// GOO with a token-track-driven gate-like object"):
//   * FactionTT's Ubbo-Sathla (TTAwakenUbboSathlaEliminateHPAction, in
//     FactionTT.scala) is the closest real analogue to Awaken Baphomet: it
//     PERMANENTLY eliminates one of TT's own units (its High Priest) to
//     awaken its GOO. The sacrifice-preference ordering below (cheapest/least
//     flexible unit first, never the GOO itself) mirrors that pattern.
//   * BotWW.scala's Ice Age / Ice track scoring (reward that scales up as a
//     token track approaches its payoff threshold) is the closest analogue to
//     scoring Portend, where each Portent brings TI closer to a free Lord's
//     Shadow + Fiend at the 4th token — see tiPortendScore below, which scales
//     with the current per-Area Portent count exactly the way WW's Ice Age
//     heuristics scale with track position.
//   * File organization/scope (a compact ~300-line, one-object-one-class file
//     covering a small unit roster plus a dozen custom sub-actions) follows
//     BotCS.scala and BotTB.scala most closely among the modern BotX bots.
//
// Cross-faction decisions this file does NOT (and cannot) cover:
//   TIBloodOfferingOfferAction/TIBloodOfferingDeclineAction and
//   TIEntropySiphonPayAction are dispatched with `self = who`, i.e. to
//   whichever ENEMY faction is being asked — the engine routes that faction's
//   OWN bot (via CthulhuWarsSolo.scala's per-faction switch), never BotTI, so
//   scoring them here would be dead code. Per the Guide's explicit call-out
//   ("handling Blood Offering from both sides" / "an ENEMY-side bot decision"
//   for Entropy Siphon), those two decisions are instead added to the shared
//   cross-faction helper `GameEvaluation.fbPromptedEvals` (BotX.scala) — which
//   every BotX-based enemy bot already calls — and mirrored into Bot3.scala's
//   own inlined copy of that helper (for GC/CC/BG/YS/BB/TS), exactly the
//   precedent already set there for DC's Proselytize drag prompt.
// ============================================================================
object BotTI extends BotX(implicit g => new GameEvaluationTI)

class GameEvaluationTI(implicit game : Game) extends GameEvaluation(TI)(game) {
    def eval(a : Action) : $[Evaluation] = {
        var result : $[Evaluation] = $

        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        val power = max(self.power, 0)

        // ---- Shared state snapshots (computed once per decision, mirrors the
        // Bot3/BotX "hot path vals lifted out of eval" convention) ----
        val baphometInPlay = self.onMap(Baphomet).not(Zeroed).any
        val totalUnits    = self.allInPlay.not(Zeroed).num
        val hasEntropySiphon = have(EntropySiphon)

        // Eternal Servitude (§1.5): once Baphomet is in play and at least one
        // other player still has Power, TI is guaranteed a real 2-Power floor
        // the instant it hits 0 (see Game.tiEternalServitudeApplies — this is
        // a simplified, forward-looking version of that same predicate: it
        // asks "if I spend to 0 THIS turn, will I be safety-netted?", not
        // "am I at 0 right now"). Per the Guide: no bot decision is needed to
        // TRIGGER the grant (automatic), but every other spending heuristic
        // below should be LESS afraid of hitting 0 Power while this is true —
        // Power is close to a renewable resource for TI once Baphomet lives.
        val eternalServitudeSafetyNet = baphometInPlay && others.exists(_.power > 0)
        // Soften (never fully remove) the standard "don't spend the last
        // Power" caution used elsewhere in the engine's shared eval helpers.
        val lowPowerCaution = if (eternalServitudeSafetyNet) -40 else -200

        // Baphomet's own Area (Unquenchable Thirst, §1.8): Attacking/Capturing
        // INTO this Area is automatically re-costed from 1 Power to 1 Doom by
        // the engine (Game.scala, unconditional — "not optional"). Power
        // regenerates every Gather Power Phase (and is softened further by
        // Eternal Servitude above); Doom does not — it IS TI's own win
        // condition currency. So, purely as a valuation matter (the bot is
        // never asked HOW to pay, only WHETHER the action is worth it), treat
        // fights inside Baphomet's Area as strictly more expensive than an
        // ordinary Attack/Capture and require a correspondingly better payoff
        // (a gate flip, a GOO kill, a vulnerable capture) to be worth it.
        val baphometRegion = self.onMap(Baphomet).not(Zeroed).headOption.map(_.region)
        def costsDoomInstead(r : Region) : Boolean = baphometRegion.contains(r)

        // Portend cost/value snapshot for a candidate Area (mirrors WW's Ice
        // Age track-position scaling — see file header). tiPortendCost already
        // encodes "free once enough Larvae sit there"; we separately reward
        // getting CLOSE to the 4th-Portent payoff (a free Lord's Shadow + a
        // free Fiend, plus progress on Requirements 5/6), the same way BotWW
        // scores Ice Age higher the nearer it is to flipping a key gate.
        def tiPortendScore(r : Region) : Int = {
            val p = game.tiPortents.getOrElse(r, 0)
            // p == 3 is one Portend away from the payoff — heavily favored.
            val proximityBonus = p match { case 3 => 1600; case 2 => 900; case 1 => 500; case _ => 200 }
            val cost = TIExpansion.tiPortendCost(r)
            val costPenalty = if (cost == 0) 0 else if (power <= 3) -300 else -80 * cost
            proximityBonus + costPenalty
        }

        // ---- FB-awareness (every BotX faction avoids FB craters / gaze regions) ----
        a.unwrap match {
            case MoveAction(_, _, _, r, _) =>
                fbMoveAvoidance(r).foreach(e => true |=> e)
            case BuildGateAction(_, r) =>
                hasFBCrater(r) |=> -8000 -> "cannot build gate on FB crater"
            case RecruitAction(_, _, r) =>
                hasFBCrater(r) |=> -5000 -> "avoid recruiting at FB crater"
            case SummonAction(_, _, r) =>
                hasFBCrater(r) |=> -5000 -> "avoid summoning at FB crater"
                (fbHasCG && isFBGazeRegion(r)) |=> -6000 -> "avoid summoning into FB gaze region"
            case _ =>
        }

        a match {
            // ================================================================
            // ORDER / HOUSEKEEPING (boilerplate, mirrors every other BotX bot)
            // ================================================================
            case FirstPlayerAction(_, f) =>
                f == self && allSB |=> 100 -> "play first all SB"
                f == self |=> -50 -> "stall"

            case PlayDirectionAction(_, order) =>
                order(1).power < order.last.power |=> 100 -> "low power first"

            case RitualAction(_, cost, _) =>
                instantDeathNow |=> 10000 -> "instant death now"
                instantDeathNext && allSB && others.all(!_.allSB) |=> 10000 -> "ritual if ID next and all SB"
                instantDeathNext && !allSB && others.%(_.allSB).any |=> -1000 -> "dont ritual if ID next and not all SB"
                allSB && realDoom + maxDoomGain >= 30 |=> 900 -> "can break 30 and all SB"
                !allSB && self.doom + self.gates.num >= 30 |=> -5000 -> "will break 30 but not all SB"
                numSB >= 5 && cost * 2 <= power |=> 800 -> "5 SB and less than half power"
                numSB >= 3 && aprxDoomGain / cost > 0.75 |=> 400 -> "sweet deal"
                self.pool.goos.any |=> -200 -> "not all goos in play"
                true |=> -250 -> "dont ritual unless have reasons"

            case DoomDoneAction(_) =>
                true |=> 10 -> "doom done"

            case PassAction(_) =>
                true |=> -500 -> "wasting power bad"

            case MoveDoneAction(_) =>
                true |=> 1000 -> "move done"

            case EndTurnAction(_) =>
                self.battled.any |=> 20000 -> "unlimited battle drains power"
                others.%(ofinale).any |=> 666000 -> "extend finale"
                true |=> 500 -> "main done"

            case RevealESAction(_, es, false, _) if self.es != es =>
                true |=> -10000 -> "better reveal all"

            case RevealESAction(_, _, _, _) =>
                allSB && realDoom >= 30 |=> 1100 -> "reveal and try to win"
                !allSB && realDoom >= 30 && others.all(!_.allSB) |=> 1100 -> "reveal break 30 nobody wins"
                true |=> -100 -> "dont reveal"

            // ================================================================
            // PORTEND (§1.7/§2.2) — spend Power growing a Portent vs holding
            // Larvae/Power for Hellgate/Scavenge/other uses. See tiPortendScore
            // above for the track-proximity scaling (WW Ice-Age-style).
            // ================================================================
            case TIPortendMainAction(l) =>
                val best = l./(tiPortendScore).max
                true |=> best -> "portend: best available Area"

            case TIPortendAction(r) =>
                true |=> tiPortendScore(r) -> ("portend in " + r)

            // ================================================================
            // AWAKEN BAPHOMET (§1.8/§3.4.1) — permanent unit sacrifice.
            // Ordering mirrors TT's Ubbo-Sathla sacrifice (see file header):
            // sacrifice the cheapest/least flexible unit, never the GOO, and
            // only pull the trigger when TI has enough of a roster left over
            // that losing one unit does not cripple future Sacrament of Flesh/
            // Scavenge/Portend fuel.
            // ================================================================
            case TIAwakenBaphometMainAction(l) =>
                true |=> 700 -> "awaken baphomet: worth considering baseline"
                need(TIAwakenBaphomet) |=> 600 -> "awaken baphomet satisfies sbr1"
                totalUnits <= 3 |=> -2000 -> "too few units left to spare one"
                totalUnits >= 8 |=> 300 -> "plenty of units to spare"

            case TIAwakenBaphometRegionAction(r) =>
                // Prefer an Area with spare Larvae (cheapest loss) and prefer
                // one already near the action (foes present) so the new
                // Baphomet (combat 4+) is immediately useful, not stranded.
                val larvaeHere = TI.at(r).%(_.uclass == DemonLarvae).not(Zeroed).num
                true |=> larvaeHere * 80 -> "spare larvae here"
                r.foes.any |=> 400 -> "immediately useful against foes"
                game.tiLordsShadowRegions.has(r) |=> 100 -> "home Lord's Shadow"

            case TIAwakenBaphometUnitAction(r, uc) =>
                val spareHere = TI.at(r).%(_.uclass == uc).not(Zeroed).num
                uc == DemonLarvae |=> 900 -> "sacrifice larva - cheapest, cannot control gates anyway"
                uc == Gryllus |=> 300 -> "sacrifice gryllus if no larva available"
                uc == Fiend && hasEntropySiphon |=> -600 -> "keep fiend - can control gates with entropy siphon"
                uc == Fiend && !hasEntropySiphon |=> -200 -> "keep fiend - strongest non-GOO unit"
                uc == Baphomet |=> -1000000 -> "never sacrifice our only GOO"
                true |=> (spareHere - 1).max(0) * 40 -> "spare copies here"

            // ================================================================
            // SPELLBOOK REQUIREMENT 3 (§1.9) — pay 4 Power + 1 Doom as Action.
            // Only offered when affordable and still needed; scale by how much
            // Power TI can spare (Eternal Servitude softens this further).
            // ================================================================
            case TIPayPowerDoomMainAction() =>
                true |=> 500 -> "pay 4 power + 1 doom for spellbook requirement 3"
                power >= 8 |=> 300 -> "plenty of power to spare"
                power == 4 && !eternalServitudeSafetyNet |=> -300 -> "would spend our last power with no safety net"

            // ================================================================
            // SACRAMENT OF FLESH (§1.8/§2.4) — MANDATORY: only WHICH unit is a
            // real choice. Same sacrifice ordering as Awaken Baphomet above:
            // Larvae first (cheapest, cannot control gates regardless), then
            // Gryllus, keep Fiend (especially once Entropy Siphon lets Fiends
            // control gates), and Baphomet is the choice of last resort (the
            // driver only offers it when literally nothing else remains).
            // ================================================================
            case TISacramentRemoveUnitAction(r, uc, toRemove, esGain) =>
                val spareHere = TI.at(r).%(_.uclass == uc).not(Zeroed).num
                uc == DemonLarvae |=> 900 -> "sacrament: remove larva first"
                uc == Gryllus |=> 300 -> "sacrament: remove gryllus if no larva"
                uc == Fiend && hasEntropySiphon |=> -600 -> "sacrament: keep fiend for gate control"
                uc == Fiend && !hasEntropySiphon |=> -200 -> "sacrament: keep fiend, strongest unit"
                uc == Baphomet |=> -1000000 -> "sacrament: never remove our only GOO if avoidable"
                true |=> (spareHere - 1).max(0) * 30 -> "sacrament: spare copies here"

            // ================================================================
            // HELLGATE (§1.10/§2.7) — relocate units between two Lord's
            // Shadows for 2 Power. Worth it when reinforcing a threatened
            // Shadow or bringing idle combat units to a live front; always
            // leave at least one Larva behind at the source so Portend can
            // keep progressing there.
            // ================================================================
            case TIHellgateMainAction(sources) =>
                val anyShadowUnderThreat = game.tiLordsShadowRegions.exists(r => r.foes.any)
                val anyIdleSurplus = sources.exists(s => TI.at(s).not(Zeroed).num > 2 && s.foes.none)
                true |=> 300 -> "hellgate: worth considering baseline"
                anyShadowUnderThreat |=> 700 -> "reinforce a threatened lord's shadow"
                anyIdleSurplus |=> 300 -> "idle surplus units to redeploy"
                power <= 2 && !eternalServitudeSafetyNet |=> -200 -> "hellgate costs power we may need"

            case TIHellgateSourceAction(src) =>
                val surplus = TI.at(src).not(Zeroed).num
                true |=> surplus * 50 -> "surplus units at source"
                src.foes.any |=> -300 -> "dont abandon a contested source"

            case TIHellgateDestAction(src, dst) =>
                dst.foes.any |=> 800 -> "hellgate toward a live front"
                dst.foes.none && game.tiLordsShadowRegions.has(dst) |=> 200 -> "hellgate to reinforce a quiet shadow"

            case TIHellgateMoveAction(src, dst, uc) =>
                uc == Fiend || uc == Gryllus || uc == Baphomet |=> 500 -> "move combat unit toward the front"
                uc == DemonLarvae && TI.at(src).%(_.uclass == DemonLarvae).not(Zeroed).num <= 1 |=> -400 -> "keep at least one larva at source for portend"
                uc == DemonLarvae |=> 100 -> "spare larva can relocate"

            case TIHellgateDoneAction() =>
                true |=> 400 -> "hellgate done"

            // ================================================================
            // ECLIPSE (§1.10/§2.7) — one-shot, no timer cost (returns to menu).
            // Since it can never be replayed, value it by how much enemy gate
            // Doom it denies next Doom Phase: more enemy gates outstanding (or
            // an enemy nearing finale) makes NOW the right time to burn it.
            // ================================================================
            case TIEclipseMainAction() =>
                val enemyGates = others./(_.gates.num).sum
                true |=> enemyGates * 250 -> "eclipse: scales with enemy gate count"
                others.%(ofinale).any |=> 1500 -> "deny finale-bound enemy gate doom"
                enemyGates == 0 |=> -500 -> "no enemy gates to deny yet"

            // ================================================================
            // SCAVENGE (§1.10/§3.10.1) — free move + free Battle/1-Power
            // Capture per Gryllus Area. Efficient whenever a Gryllus can reach
            // (or already threatens) a foe, since the Battle itself is free.
            // ================================================================
            case TIScavengeMainAction() =>
                val gryllusNearFoes = TI.onMap(Gryllus).not(Zeroed).exists(g => g.region.foes.any || g.region.connected.exists(_.foes.any))
                gryllusNearFoes |=> 900 -> "gryllus near foes: free battles available"
                !gryllusNearFoes |=> 150 -> "scavenge: reposition gryllusses"

            case TIScavengeMoveSelectAction(from) =>
                from.foes.any |=> -100 -> "already in contact, maybe dont move"
                true |=> 100 -> "reposition gryllus"

            case TIScavengeMoveAction(from, to) =>
                to.foes.cultists.any && to.foes.monsterly.none && to.foes.goos.none |=> 900 -> "move gryllus toward vulnerable cultists"
                to.foes.any |=> 800 -> "move gryllus into contact"
                to.foes.none |=> 50 -> "move gryllus"

            case TIScavengeMoveDoneAction() =>
                true |=> 500 -> "done moving gryllusses"

            case TIScavengeBattleAction(r, f) =>
                val ownStr = self.strength(TI.at(r), f)
                val enemyStr = f.strength(f.at(r), self)
                ownStr > enemyStr |=> 900 -> "free battle: favorable"
                ownStr <= enemyStr |=> -300 -> "free battle: unfavorable, but still free"

            case TIScavengeCaptureAction(r, f) =>
                power >= 1 |=> 500 -> "1-power capture with scavenge"

            case TIScavengeCaptureTargetAction(r, f, ur) =>
                true |=> 400 -> "capture target"
                game.unit(ur).onGate |=> 300 -> "capture the gatekeeper"

            case TIScavengeDoneAction() =>
                true |=> 500 -> "done scavenging"

            // ================================================================
            // BLOOD OFFERING (§1.10/§3.10.5), TI's own side — free (0 Power),
            // one-shot-until-Gather-Power. Almost pure upside: worst case
            // every enemy declines and the cards just return to the pool.
            // Prioritize when Requirement 4 (2+ factions' Cultists captured)
            // is still open.
            // ================================================================
            case TIBloodOfferingMainAction() =>
                true |=> 700 -> "blood offering: free upside baseline"
                need(TICapturedTwoFactions) |=> 500 -> "progress captured-2-factions requirement"
                others.exists(_.cultists.any) |=> 200 -> "enemies have eligible cultists"

            // TI's choice of which (at most one) offer to accept: minimize the
            // Doom handed to the offering enemy, maximize the value of the
            // Cultist captured, and avoid feeding the current Doom leader.
            case TIBloodOfferingAcceptAction(who, ur, i, drawn) =>
                val u = game.unit(ur)
                val es = drawn(i)
                val isDoomLeader = game.factions.forall(f2 => f2 == who || f2.doom <= who.doom)
                true |=> u.uclass.cost * 200 -> "value of captured cultist"
                u.onGate |=> 400 -> "denies enemy gatekeeper"
                true |=> -(es.value * 150) -> "doom handed to offering enemy"
                isDoomLeader |=> -400 -> "avoid feeding the doom leader"

            case TIBloodOfferingDeclineAllAction(offers) =>
                true |=> 150 -> "decline all: baseline, beats a bad accept"

            // ================================================================
            // INFERNOLATREIA (§1.10) — strictly at-least-as-good as a blind
            // draw whenever it offers a real choice (Lord's Shadow count >= 1,
            // so more cards are drawn than are owed); accept essentially
            // always. Keep the highest-value Elder Signs when narrowing down.
            // ================================================================
            case TIInfernolatreiaAcceptAction(n, next) =>
                game.tiLordsShadowRegions.num >= 1 |=> 1500 -> "real choice among more cards than owed"
                game.tiLordsShadowRegions.num == 0 |=> 200 -> "no real choice yet, but no downside either"

            case TIInfernolatreiaDeclineAction(n, next) =>
                true |=> 0 -> "decline infernolatreia: no reason to prefer this"

            case TIInfernolatreiaKeepOneAction(i, remaining, candidates, kept, next) =>
                true |=> candidates(i).value * 300 -> "keep highest-value elder sign"

            // ================================================================
            // GENERIC MOVE — per TI unit type.
            // ================================================================
            case MoveAction(_, u, o, d, cost) if u.uclass == DemonLarvae =>
                o.ownGate && o.allies.cultists.num == 1 |=> -300 -> "larva alone holding down fort"
                d.foes.goos.any |=> -1000 -> "dont move larva alone into enemy goo"
                game.tiPortents.getOrElse(d, 0) >= 1 |=> 300 -> "larva toward a growing portent"
                d.allies.monsterly.any || d.allies.goos.any |=> 100 -> "larva escorted"

            case MoveAction(_, u, o, d, cost) if u.uclass == Gryllus =>
                d.foes.any |=> 400 -> "gryllus toward contact (free scavenge battles)"
                d.foes.goos.any && self.strength(self.at(d) :+ u, d.foes.head.faction) <= 0 |=> -800 -> "dont walk gryllus alone into a goo"

            case MoveAction(_, u, o, d, cost) if u.uclass == Fiend =>
                d.foes.any |=> 500 -> "fiend toward contact"
                hasEntropySiphon && d.gate && !d.ownGate |=> 600 -> "fiend can now control gates - go take one"
                d.ownGate && hasEntropySiphon |=> 100 -> "fiend defends a gate it can control"

            case MoveAction(_, u, o, d, cost) if u.uclass == Baphomet =>
                power > 1 && d.enemyGate && d.foes.goos.none |=> 3000 -> "baphomet capture enemy gate"
                d.foes.goos.any && self.strength(self.at(d) :+ u, d.foes.head.faction) > 0 |=> 300 -> "baphomet fight goo"
                d.ownGate |=> 30 -> "baphomet defends own gate"
                costsDoomInstead(o) && d != o |=> 50 -> "leaving baphomet's own doom-costed area is fine"

            // ================================================================
            // BUILD GATE — TI has NO unit that can control an ordinary gate
            // until it holds Entropy Siphon (Fiend override); Demon Larvae is
            // a plain Cultist (canControlGate == false) and there is no
            // Acolyte/HighPriest analogue. Building a gate TI cannot hold is
            // wasted tempo, so heavily discourage it until Entropy Siphon is
            // in hand (Lord's Shadows via Portend are TI's real gate engine).
            // ================================================================
            case BuildGateAction(_, r) =>
                !hasEntropySiphon |=> -5000 -> "no unit can control an ordinary gate yet - use portend instead"
                hasEntropySiphon && r.allies(Fiend).any |=> 500 -> "build gate where a fiend can hold it"
                hasEntropySiphon && r.allies(Fiend).none |=> -1000 -> "no fiend here to hold a new gate"

            // ================================================================
            // RECRUIT / SUMMON — Demon Larvae/Gryllus/Fiend.
            // ================================================================
            case RecruitAction(_, DemonLarvae, r) =>
                r.capturers.%(_.power > 0).any |=> -1500 -> "dont recruit larva to be captured"
                r.allies.monsterly.any || r.allies.goos.any |=> 300 -> "recruit larva where escorted"
                game.tiPortents.getOrElse(r, 0) >= 1 |=> 400 -> "recruit larva to fuel portend"
                true |=> 100 -> "recruit larva"

            case SummonAction(_, DemonLarvae, r) =>
                r.capturers.%(_.power > 0).any |=> -1500 -> "dont summon larva to be captured"
                game.tiPortents.getOrElse(r, 0) >= 1 |=> 400 -> "summon larva to fuel portend"
                true |=> 100 -> "summon larva"

            case SummonAction(_, Gryllus, r) =>
                r.foes.any |=> 400 -> "summon gryllus to fight"
                have(Scavenge) |=> 200 -> "gryllus feeds scavenge"
                true |=> 150 -> "summon gryllus"

            case SummonAction(_, Fiend, r) =>
                r.foes.any |=> 500 -> "summon fiend to fight"
                hasEntropySiphon && r.gate |=> 400 -> "fiend can garrison a gate"
                true |=> 250 -> "summon fiend"

            // ================================================================
            // ATTACK / CAPTURE — apply the Unquenchable Thirst Doom-vs-Power
            // deterrent when the target is Baphomet's own Area (see
            // costsDoomInstead above).
            // ================================================================
            case AttackAction(_, r, f, _) if f.neutral =>
                true |=> -100000 -> "dont attack neutrals"

            case AttackAction(_, r, f, _) =>
                val allies = self.at(r)
                val foes = f.at(r)
                val ownStr = self.strength(allies, f)
                val enemyStr = f.strength(foes, self)
                ownStr > enemyStr |=> 400 -> "favorable attack"
                foes.goos.any && ownStr >= enemyStr |=> 500 -> "attack goo"
                costsDoomInstead(r) |=> lowPowerCaution -> "attacking baphomet's own area costs doom, not power"
                costsDoomInstead(r) && foes.goos.any |=> 300 -> "...but worth it against a goo"

            case CaptureAction(_, r, f, _) =>
                true |=> 500 -> "capture"
                r.enemyGate |=> 100 -> "enemy gate"
                costsDoomInstead(r) |=> lowPowerCaution -> "capturing in baphomet's own area costs doom, not power"

            // ================================================================
            // GATE CONTROL — boilerplate (relevant once Entropy Siphon lets
            // Fiends control gates; Lord's Shadow control is automatic and
            // never routes through these actions).
            // ================================================================
            case AbandonGateAction(_, _, _) =>
                true |=> -1000000 -> "never abandon"

            case ControlGateAction(_, r, u, _) =>
                r.allies.%(_.onGate).foreach { c =>
                    c.uclass == u.uclass |=> -1000000 -> "remain calm"
                }
                r.allies.%(_.onGate).none |=> 1000000 -> "claim empty gate"

            case _ =>
        }

        // ================================================================
        // BATTLE — elimination / retreat preferences for TI's own units.
        // Larvae are cheap and expendable (cannot control gates regardless);
        // Fiends and Baphomet are the valuable, hard-to-replace pieces.
        // ================================================================
        if (game.battle.any) {
            if (game.battle./~(_.sides).has(self).not) {
                a match {
                    case _ => true |=> 1000 -> "todo"
                }
            }
            else {
                implicit val battle = game.battle.get

                def elim(u : UnitFigure) {
                    u.is(DemonLarvae) |=> 800 -> "elim larva"
                    u.is(Gryllus)     |=> 400 -> "elim gryllus"
                    u.is(Fiend)       |=> -200 -> "avoid eliminating fiend"
                    u.is(Baphomet)    |=> -100000 -> "never eliminate baphomet"
                }

                def retreat(u : UnitFigure) {
                    u.gateKeeper      |=> -1000 -> "dont retreat gate keeper"
                    u.is(DemonLarvae) |=> 700 -> "retreat larva"
                    u.is(Gryllus)     |=> 500 -> "retreat gryllus"
                    u.is(Fiend)       |=> 300 -> "retreat fiend"
                    u.is(Baphomet)    |=> -5000 -> "baphomet should not retreat"
                }

                a match {
                    case AssignKillAction(_, _, _, u) => elim(u)
                    case AssignPainAction(_, _, _, u) => retreat(u)
                    case EliminateNoWayAction(_, u) => elim(u)

                    case RetreatUnitAction(_, u, r) =>
                        u.cultist && r.allies.goos.any |=> 2000 -> "retreat larva to baphomet"
                        u.cultist && r.freeGate |=> 4000 -> "retreat larva to free gate"
                        u.cultist && r.ownGate |=> 100 -> "retreat larva to own gate"
                        u.is(Fiend) && r.allies.goos.any |=> 500 -> "retreat fiend to baphomet"
                        u.goo && r.allies.num >= 2 |=> 500 -> "retreat baphomet with support"
                        true |=> r.connected.distinct.num -> "reachable"

                    case _ =>
                        true |=> 1000000 -> "todo"
                }
            }
        }

        // Cross-faction prompts shared with every other BotX faction (FB CG/Eye
        // Opens, DC Proselytize, and — see file header — TI's own Blood
        // Offering/Entropy Siphon enemy-side prompts, added there rather than
        // here since those are asked of OTHER factions' bots, not this one).
        result ++= fbPromptedEvals(a)

        result.none |=> 0 -> "none"
        true |=> -((1 + math.random() * 4).round.toInt) -> "random"

        result.sortBy(v => -v.weight.abs)
    }
}
