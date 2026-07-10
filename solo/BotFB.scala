package cws

import hrf.colmat._


// ============================================================================
// Firstborn (FB) BOT: AI evaluation logic for all FB-specific actions.
// Each action is scored with positive values for good plays and negative for bad.
// The BotX framework picks the highest-scored action.
//
// Round 9 rewrite: explicit user-specified tactical scoring rules.
// ============================================================================
object BotFB extends BotX(implicit g => new GameEvaluationFB) {
    // Round 9: traceWeights moved to Bot3.traceFaction (central, parameterized by faction).
}

class GameEvaluationFB(implicit game : Game) extends GameEvaluation(FB)(game) {
    // v5.11 (2026-05-14): Hot-path vals lifted out of `def eval` and computed once
    // per GameEvaluationFB instance. BotX constructs a fresh instance for every
    // askE call (each FB decision point — main action, IP, unlimited sub-action),
    // so these vals naturally invalidate at every decision boundary including
    // post-battle unlimited-action prompts. Within a single decision, the bot
    // evaluates ~50 candidate actions; lifting these out replaces ~50× recomputation
    // with a single pass and dramatic sim speedup.

    // Distance from region to nearest enemy unit
    def distFromEnemies(r : Region) : Int = {
        val enemyRegions = others./~(_.units.%(_.region.onMap))./(_.region).distinct
        if (enemyRegions.none) 999
        else enemyRegions./(er => game.board.distance(r, er)).min
    }

    def loneEnemyCultistRegions : $[Region] =
        areas.%(r => r.glyph != Ocean && r.foes.cultists.any && r.foes.monsterly.none && r.foes.goos.none)

    def emptyLandRegions : $[Region] =
        areas.%(r => r.glyph != Ocean && r.foes.none && self.at(r).none && !r.gate)

    def vulnerableEnemyGates : $[Region] =
        others./~(_.gates).%(r => r.foes.goos.none && r.foes.monsterly.none && r.foes.cultists.num <= 1).distinct

    // ── Leader-finale GOO hunt vals (v5.7) ──
    val enemiesInFinale : $[Faction] = others.%(ofinale)
    val leaderFaction : |[Faction] = {
        val maxDoom = others./(_.aprxDoom).maxOr(0)
        if (maxDoom <= self.aprxDoom + 4) None
        else others.%(f => f.aprxDoom == maxDoom).headOption
    }
    val leaderGooRegion : |[Region] = leaderFaction.flatMap { lf =>
        if (!enemiesInFinale.contains(lf)) None
        else {
            val gooRegions = lf.allInPlay.%(u => u.uclass.utype == GOO && u.region.onMap)./(_.region).distinct
            gooRegions.headOption
        }
    }
    // v5.16 (2026-05-14): combat-viability + non-water guard. If FB can't bring
    // enough combat to plausibly kill the enemy GOO, OR the target is in water
    // (Ghato can't reliably writhe-pain to water gates), revert to normal gate
    // vulnerability targeting.
    val leaderHuntCombatViable : Boolean = leaderGooRegion.exists { r =>
        val tf = leaderFaction.getOrElse(self)
        val foeStr = tf.strength(tf.at(r), self)
        r.glyph != Ocean && foeStr <= 6
    }
    // 2026-05-27 rule: writhe dice = power AFTER paying the writhe cost.
    // Cost paid = max(0, 2 - min(IP, 2)). All writhe-entry gates that test
    // "is this writhe strong enough?" use writheDice, not raw power.
    val writheDice : Int = power - max(0, 2 - min(game.fbInfernalPactDiscount, 2))

    val canHuntLeaderGoo : Boolean = leaderGooRegion.isDefined && leaderHuntCombatViable &&
        self.onMap(Ghatanothoa).any && self.gates.num >= 1 && writheDice >= 4 &&
        self.gates.exists(g => self.at(g).num >= 2)

    // ── Projected-doom hunt vals (v5.8) ──
    def projDoom(f : Faction) : Int = f.doom + math.ceil(f.es.num * 1.66).toInt
    val fbProjDoom : Int = projDoom(self)
    val projThreatFaction : |[Faction] = {
        val threats = others.%(f => projDoom(f) >= 15 && projDoom(f) >= fbProjDoom + 2 &&
            f.allInPlay.%(u => u.uclass.utype == GOO && u.region.onMap).any)
        if (threats.any) Some(threats.maxBy(projDoom(_))) else None
    }
    val huntTargetGooRegion : |[Region] = projThreatFaction.flatMap { tf =>
        val onMapGoos = tf.allInPlay.%(u => u.uclass.utype == GOO && u.region.onMap)
        if (onMapGoos.none) None
        else if (onMapGoos.num == 1) Some(onMapGoos.head.region)
        else {
            val ranked = onMapGoos./(u => (u, tf.strength($(u), self)))
            Some(ranked.minBy(_._2)._1.region)
        }
    }
    // v5.16 (2026-05-14): same combat + non-water guard as leader hunt.
    val projHuntCombatViable : Boolean = huntTargetGooRegion.exists { r =>
        val tf = projThreatFaction.getOrElse(self)
        val foeStr = tf.strength(tf.at(r), self)
        r.glyph != Ocean && foeStr <= 6
    }
    val canHuntProjThreatGoo : Boolean = huntTargetGooRegion.isDefined && projHuntCombatViable &&
        self.onMap(Ghatanothoa).any && self.gates.num >= 1 && writheDice >= 4 &&
        self.gates.exists(g => self.at(g).num >= 2)

    // ── Eye Opens opportunistic setup (v5.8) ──
    val eyeOpensReady : Boolean = self.has(TheEyeOpens) && self.gates.num >= 2
    val eyeOpensSetupRegion : |[Region] = {
        if (!eyeOpensReady) None
        else areas.find(r => r.enemyGate && r.glyph != Ocean &&
            r.foes.cultists.num == 1 && r.foes.goos.none &&
            !game.fbCraters.has(r))
    }

    // ── Gate vulnerability with per-instance memoization (v5.11) ──
    // Memo cache: same Region+painCount lookup returns the cached result.
    // Cleared automatically when a new GameEvaluationFB is constructed.
    private val gvCache = scala.collection.mutable.HashMap.empty[(Region, Int), Int]

    def gateVulnerability(r : Region, painCount : Int = 99) : Int = {
        gvCache.getOrElseUpdate((r, painCount), gateVulnerabilityImpl(r, painCount))
    }

    // v5.19 (2026-05-15): memoize `Faction.strength(f.at(r), self)` and other
    // hot region-keyed lookups. These are called many times per eval (across
    // gateVulnerability, AttackAction scoring, leader-hunt checks, etc.).
    // Caches live on the per-instance level so each fresh GameEvaluationFB
    // gets a clean cache. Same correctness property as `gvCache`.
    private val foeStrCache = scala.collection.mutable.HashMap.empty[(Faction, Region), Int]
    def foeStrAt(f : Faction, r : Region) : Int =
        foeStrCache.getOrElseUpdate((f, r), f.strength(f.at(r), self))

    private val foeCombatSumCache = scala.collection.mutable.HashMap.empty[Region, Int]
    def foeCombatAt(r : Region) : Int =
        foeCombatSumCache.getOrElseUpdate(r, others.map(f => foeStrAt(f, r)).sum)

    private val enemyGooAtCache = scala.collection.mutable.HashMap.empty[Region, Boolean]
    def enemyGooAt(r : Region) : Boolean =
        enemyGooAtCache.getOrElseUpdate(r, others.exists(f => f.at(r, GOO).any))

    private def gateVulnerabilityImpl(r : Region, painCount : Int) : Int = {
            // v5.7 (2026-05-13): leader-finale GOO override — top-priority target.
            // Beats T0a (10000 + glyphBoost). Only fires when canHuntLeaderGoo
            // (FB has Ghato, has gate to spare, etc.) and r is the leader's GOO.
            if (canHuntLeaderGoo && leaderGooRegion.contains(r)) return 11000
            // v5.8 (2026-05-13): proj-doom threat hunt — slightly lower than the
            // finale-leader override (10800) so the finale hunt still wins when
            // both apply. Same gating idea: FB has Ghato, has gate to spare.
            if (canHuntProjThreatGoo && huntTargetGooRegion.contains(r)) return 10800
            // v5.8 (2026-05-13): Eye-Opens setup — boost a land enemy gate with
            // a lone cultist + no GOO so Writhe pain routes Desc/Cult there.
            // Scored at 10500 — above all standard tiers but below the active
            // proj-doom/finale hunts so those still take priority.
            if (eyeOpensSetupRegion.contains(r)) return 10500
            val isLand = r.glyph != Ocean
            val isFree = r.freeGate
            val isEnemy = r.enemyGate
            if (!isFree && !isEnemy) return 0
            if (r.chaosGate) return 0
            // v5.8 (2026-05-13): YS Passion writhe block REMOVED. The block kept
            // FB out of YS gates entirely; instead we let writhe target normally
            // and rely on the AttackAction YS-Passion battle boost to drive FB
            // to battle-for-gate. The CaptureAction-side guard remains.
            val hasGoo = r.foes.goos.any
            // v5.3 (2026-05-13): use the canonical Faction.strength(units, opponent)
            // combat calculator (abstract on Faction trait, implemented per faction
            // — see Game.scala line 315 + each FactionXX.scala). This properly
            // accounts for status effects, GOO scaling, and faction-specific
            // unit values rather than the rough Monster + Terror*4 estimate.
            val foeCombat = foeCombatAt(r)
            // GOO blocker removed; combat threshold scales with Ghato cost.
            //   ghato 6 → threshold 4   ghato 5 → 6   ghato 4 → 8   ghato 3 → 10   ghato <= 2 → none
            val ghatoCost = math.max(1, 11 - game.ritualCost)
            val combatThreshold = if (ghatoCost <= 2) 9999 else 4 + (6 - ghatoCost) * 2
            val lowCombat = foeCombat < combatThreshold
            val lowCultists = r.foes.cultists.num < 3
            val noUnits = r.foes.none
            // Blocks: Ice Age and cathedral with AN combat units present
            val iceAge = WW.exists && WW.iceAge.contains(r)
            val cathedralDanger = game.cathedrals.has(r) && AN.exists &&
                AN.at(r).%(a => a.uclass.utype == Monster || a.uclass.utype == Terror).any
            if (iceAge || cathedralDanger) return -1
            // v5.3 (2026-05-13): starting-glyph boost — any land region that is a
            // faction starting region gets +350; +50 more if it's SL's start
            // (Sleeper's start has less defensive value vs FB's writhe so it's
            // preferred when tied with other glyph regions).
            val isAnyStartGlyph = isLand && game.factions.exists(f => game.starting.get(f).contains(r))
            val isSLStart = SL.exists && game.starting.get(SL).contains(r)
            val glyphBoost = (if (isAnyStartGlyph) 350 else 0) + (if (isSLStart) 50 else 0)
            // v5.16 (2026-05-14): pressure factions with many gates. +200 if the
            // gate-owning enemy has > 2 gates, another +200 if > 3 gates.
            val gateOwnerCount = others.find(_.gates.has(r)).map(_.gates.num).getOrElse(0)
            val ownerBoost = (if (gateOwnerCount > 2) 200 else 0) + (if (gateOwnerCount > 3) 200 else 0)
            // Tiers (highest = most vulnerable = best target). Land tiers get
            // glyphBoost (+350 for any starting glyph, +50 more for SL start).
            if (isFree && isLand && noUnits) 10000 + glyphBoost                   // T0a: free land, empty
            else if (isFree && isLand && lowCombat) 9950 + glyphBoost             // T0b: free land, low combat
            else if (isFree && !isLand && noUnits) 8100                           // T0c: free water, empty (was 9600, -1500)
            else if (isEnemy && isLand && lowCombat && lowCultists) 9500 + glyphBoost + ownerBoost   // T1: enemy land, low combat, < 3 cult
            else if (isEnemy && isLand && lowCombat && !lowCultists) 9200 + glyphBoost + ownerBoost  // T2: enemy land, low combat, 3+ cult
            else if (isEnemy && !isLand && lowCombat && lowCultists) 7400 + ownerBoost               // T3: enemy water, low combat, < 3 cult (was 8900, -1500)
            else if (isEnemy && !isLand && lowCombat && !lowCultists) 7100 + ownerBoost              // T4: enemy water, low combat, 3+ cult (was 8600, -1500)
            else if (isEnemy && isLand && !lowCombat && foeCombat <= 6 && painCount >= 3) 8300 + glyphBoost + ownerBoost // T5: enemy land, high combat, 3+ pains
            else 0
        }

        // Best gate target for Ghato: highest vulnerability score
        def bestGateTarget : |[Region] = {
            val candidates = areas.%(r => gateVulnerability(r) > 0)
            if (candidates.any) Some(candidates.maxBy(r => gateVulnerability(r))) else None
        }

        // SL home gate boost: only SL gates vulnerable + SL has Cursed Slumber but no gate in slumber
        val slHomeGateBoost : |[Region] = {
            val vulnGates = areas.%(r => gateVulnerability(r) > 0)
            val onlySLVuln = vulnGates.nonEmpty && vulnGates.forall(r => SL.exists && SL.gates.has(r))
            if (onlySLVuln && SL.exists && SL.has(CursedSlumber) && !SL.gates.%(r => SL.at(r).cultists.any && SL.at(r).%(_.uclass == Tsathoggua).none).any) {
                game.starting.get(SL).flatMap(home => SL.gates.has(home).?(home))
            } else None
        }

        // Safety ranking for non-gate writhe destinations
        def regionSafety(r : Region) : Int = {
            if (r.glyph == Ocean) return -1000
            if (game.fbCraters.has(r)) return -10000
            val cathedralDanger = game.cathedrals.has(r) && AN.exists &&
                AN.at(r).%(a => a.uclass.utype == Monster || a.uclass.utype == Terror).any
            if (cathedralDanger) return -10000
            val foeCombat = others./~(_.at(r)).%(u => u.uclass.utype == Monster || u.uclass.utype == Terror).num
            val hasGoo = r.foes.goos.any
            val adjGoos = r.near.%(n => others./~(_.at(n)).goos.any)
            val adjGooStr = adjGoos./~(n => others./~(_.at(n)).goos)./(_.uclass.cost).sum
            if (foeCombat < 3 && !hasGoo && adjGoos.none) 4000
            else if (foeCombat < 3 && !hasGoo && adjGooStr < 5) 3000
            else if (foeCombat < 5) 2000
            else 1000
        }

        // Ghato in non-gate region checks
        val ghatoRegion = self.onMap(Ghatanothoa).headOption.map(_.region)
        val ghatoOnNonGate = ghatoRegion.exists(r => !self.gates.has(r) && !r.freeGate)
        val ghatoHasAdjacentVulnGate = ghatoRegion.exists(r =>
            r.near.%(n => gateVulnerability(n) > 0).any)
        val ghatoHasCultist = ghatoRegion.exists(r => self.at(r).cultists.any)
        val noVulnerableGatesAnywhere = areas.%(r => gateVulnerability(r) > 0).none

        // Count FB gate-controlled that have Ghato or Rev present.
        val gatesWithGOOorRev : Int = self.gates.%(r => self.at(r, Ghatanothoa).any || self.at(r, RevenantOfKnaa).any).num

        // Count FB gates without any Ghato/Rev — these need reinforcement.
        val gatesNeedingProtection : Int = self.gates.num - gatesWithGOOorRev

        // True if FB already holds 3 controlled gates (the strategy target).
        val threeGates = self.gates.num >= 3

        // "Total FB doom (including hidden ES)"
        val fbRealDoom = self.doom + self.es./(_.value).sum

        // "Power leader" is highest enemy power
        val powerLeader = others./(_.power).maxOr(0)

        // Tiebreaker faction order for player picking
        val factionOrder : $[Faction] = $(SL, CC, BG, WW, GC, TS, AN, OW)
        def factionRank(f : Faction) : Int = {
            val idx = factionOrder.indexOf(f)
            if (idx >= 0) idx else 99
        }

        // Score a non-FB faction by user's tiebreaker rules:
        //   most power, most doom, most ES, most SBs, then SL>CC>BG>WW>GC>TS>AN>OW
        // Returned as a positive integer where higher = more preferred.
        def tiebreakScore(f : Faction) : Int = {
            f.power * 10000 +
            f.doom * 500 +
            f.es.num * 50 +
            f.spellbooks.num * 10 +
            (10 - factionRank(f))
        }

    def eval(a : Action) : $[Evaluation] = {
        var result : $[Evaluation] = $

        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        val unwrapped = a.unwrap

        // Cheap per-eval state vals (snapshot at eval-call time).
        val desiccatedOnMap = self.onMap(Desiccated).num
        val revenantOnMap = self.onMap(RevenantOfKnaa).num
        val gooOnMap = self.onMap(Ghatanothoa).num
        val craterCount = game.fbCraters.num
        val auguryKills = game.fbAuguryKills
        val infernalDiscount = game.fbInfernalPactDiscount

        val firstAP = game.turn == 1
        val laterAP = game.turn >= 2
        val secondAP = game.turn == 2
        val thirdAP = game.turn == 3
        val fourthOrLaterAP = game.turn >= 4
        val ritualPreserve = self.gates.num >= 3 && gooOnMap > 0 && power >= game.ritualCost && power <= game.ritualCost + 3

        val fbStartRegion : |[Region] = game.starting.get(FB)
        val fbStartGlyphRegion : |[Region] = fbStartRegion

        val idImminent = instantDeathImminent
        val idImminentForDM = instantDeathImminentForDM

        unwrapped match {

            // ──────────────────────────────────────────────────────────────────
            // STARTING REGION — land, far from others. Antarctica very low if WW.
            // ──────────────────────────────────────────────────────────────────
            case StartingRegionAction(_, r) =>
                // Single-best-score conditional (see Faction Bot Builder Guide §
                // "Single Best Score via Conditional Logic"). Each candidate
                // region gets ONE distinct score — no fragile tiebreakers.
                val near1Occupied = r.near.%(nr => game.starting.values.$.has(nr)).num
                val near2Occupied = r.near2.%(nr => game.starting.values.$.has(nr)).num
                val isAntarcticaVsWW = r.name == "Antarctica" && WW.exists
                // User feedback 2026-04-16: baseline NEVER starts adjacent to enemies.
                // Adjacency is a clear negative — enemy captures/attacks on turn 1.
                val startScore =
                    if (isAntarcticaVsWW)              -8000
                    else if (r.glyph == Ocean)         -3000
                    else if (near1Occupied >= 2)       -2000  // adjacent to 2+ enemies: very bad
                    else if (near1Occupied >= 1)       -1000  // adjacent to 1 enemy: bad
                    else if (near2Occupied == 0)        7000  // isolated (no enemies in 2 steps)
                    else                                6500  // one-step buffer
                true |=> startScore -> ("start region tier " + startScore)

            // ──────────────────────────────────────────────────────────────────
            // WRITHE — main activation
            // ──────────────────────────────────────────────────────────────────
            case FBWritheMainAction(_) =>
                // BLOCK: never writhe with < 2 units on map (nothing to pain)
                val fbUnitsOnMap = self.allInPlay.%(_.region.onMap).num
                (fbUnitsOnMap < 2) |=> -15000 -> "BLOCK: < 2 units on map, nothing to writhe"
                // v5.13 (2026-05-14): BLOCK writhe at < 3 dice. Writhe rolling 1-2
                // dice produces too-small rolls + scatters the last cultists with
                // no follow-up. Under 2026-05-27 rule, dice = power - max(0, 2-IP).
                (writheDice < 3) |=> -15000 -> "BLOCK: < 3 writhe dice, too weak"
                // v5.4 (2026-05-13): Writhe behavior when Ghato is at the top
                // gateVulnerability target.
                //  - Default: BLOCK Writhe (-15000) — CaptureAction or AttackAction
                //    should be the next move, not relocating Ghato away.
                //  - Exception (writhe-support): when AttackAction's ghatoVsGooAttack
                //    AND CaptureAction both cannot fire at the top target, AND FB
                //    has surplus units at some controlled gate (units > 3 with a
                //    floor of 1 Rev + 1 Cultist + 1 other), writhe to PAIN those
                //    surplus units toward Ghato's region. Scored just below the
                //    attack tier (9300) so attack always wins when both are
                //    viable simultaneously.
                val topTarget = bestGateTarget
                val ghatoAtTop = topTarget.exists(r => self.at(r, Ghatanothoa).any)
                // Compute attack/capture viability at the top target — must mirror
                // the conditions in AttackAction.ghatoVsGooAttack / CaptureAction.
                val supportNeededAtTop: Boolean = topTarget.exists { r =>
                    val foesAtR = others.flatMap(_.at(r))
                    val enemyGooAtR = foesAtR.exists(_.uclass.utype == GOO)
                    val enemyGateAtR = others.exists(_.gates.has(r))
                    val targetF = others.find(_.gates.has(r)).getOrElse(others.headOption.getOrElse(self))
                    val fbAttackTT = self.strength(self.at(r), targetF)
                    val foeDefenseTT = foeCombatAt(r)
                    val resurrectCostTT = math.max(1, 11 - game.ritualCost)
                    val canResurrectTT = power >= resurrectCostTT + 3
                    val ghatoSurvivesTT = fbAttackTT >= foeDefenseTT
                    val ghatoVsGooAttackTT = enemyGooAtR && enemyGateAtR && (ghatoSurvivesTT || canResurrectTT)
                    // CaptureAction tiers require no enemy GOO at the region.
                    val anyCaptureViable = !enemyGooAtR &&
                        ((self.at(r, Ghatanothoa).any && enemyGateAtR) ||
                         (self.at(r, Desiccated).any && foesAtR.exists(_.cultist)))
                    !ghatoVsGooAttackTT && !anyCaptureViable
                }
                // Surplus floor: each controlled gate must keep 1 Rev + 1 Cultist + 1 other unit.
                val surplusAtAnyGate = self.gates.exists { g =>
                    val total = self.at(g).num
                    val revs = self.at(g, RevenantOfKnaa).num
                    val cults = self.at(g).cultists.num
                    total > 3 && revs >= 1 && cults >= 1
                }
                val writheSupport = ghatoAtTop && supportNeededAtTop && surplusAtAnyGate
                writheSupport |=> 9100 -> "Writhe-support: pain surplus units to Ghato (attack/capture both unviable)"
                val pureBlock = ghatoAtTop && !writheSupport
                pureBlock |=> -15000 -> "BLOCK: Ghato at top target (no writhe-support viable)"
                // v5.7 (2026-05-13): Leader-finale GOO hunt. When the runaway leader
                // is in ofinale, Writhe Ghato to the leader's GOO region for an
                // assault (unlimited combat with 6SB). Top-priority — beats all
                // other writhe tiers. Guards: FB has Ghato on map, power >= 4,
                // at least one FB gate with 2+ units (so writhe-pain doesn't
                // strip the gate's last keeper).
                canHuntLeaderGoo |=> 9700 -> "LEADER HUNT: Writhe Ghato to runaway leader's GOO"
                // v5.8 (2026-05-13): proj-doom threat hunt — slightly below finale.
                canHuntProjThreatGoo |=> 9650 -> "PROJ-DOOM HUNT: Writhe Ghato to threat faction's lower GOO"
                // v5.8 (2026-05-13): Eye-Opens setup — Writhe to set up 2 desc + 1 cult at target.
                eyeOpensSetupRegion.nonEmpty |=> 9600 -> "Writhe to set up Eye Opens target (land gate w/ lone cult)"
                // v5.6 (2026-05-13): Ghato stuck on a water gate when better land
                // target exists — Writhe to relocate Ghato off the water gate.
                val ghWG = self.gates.find(g => g.glyph == Ocean && self.at(g, Ghatanothoa).any)
                (self.gates.num > 2 && self.gates.%(_.glyph == Ocean).num > 1 && ghWG.nonEmpty &&
                    writheDice > 5 &&
                    areas.%(_.glyph != Ocean).map(gateVulnerability(_)).maxOption.getOrElse(0) >
                        ghWG.map(gateVulnerability(_)).getOrElse(0)) |=> 9000 -> "Writhe: Ghato leaves water gate for land"
                // AP1 first-turn opener (user strategy: 2 Writhes in AP1)
                (firstAP && writheDice >= 7 && desiccatedOnMap == 0) |=> 8000 -> "AP1 first Writhe (dice 7+, no desc)"
                // AP1 SECOND Writhe: user's replay shows second Writhe with remaining
                // power after 1st. Allow if we still have 4+ dice and 1-2 desiccated.
                (firstAP && writheDice >= 4 && desiccatedOnMap >= 1 && desiccatedOnMap <= 4) |=> 7000 -> "AP1 second Writhe (dice 4+, desc 1-4)"
                // Re-awaken Writhe: power must cover Writhe cost (2) + awaken cost (minus IP discount).
                val nextAwakenCostMain = math.max(1, 11 - game.ritualCost)
                val effectiveAwakenCost = math.max(1, nextAwakenCostMain - infernalDiscount)
                // Re-awaken writhe: must match kill conditions (ritualCost > 6 || gates >= 3)
                // so the writhe doesn't fire when Ghato can't actually be killed inside it.
                // Re-awaken trigger: ritualCost > 6 OR gates >= 3 OR FB ritualed >= 2
                // (FB's own ritual count catches games where others don't ritual much)
                val unflippedSBsMain = self.spellbooks.count(sb => !self.oncePerGame.has(sb))
                val fbTotalDoomMain = self.doom + (self.es.num * 5 / 3)
                // v5 (2026-05-13): writhe-kill Ghato only at projected doom 30+ AND when
                // at least one of the late awakening SBRs is still unfulfilled. Without
                // those guards the bot was firing writhe-kill chains too early and
                // sacrificing Ghato for marginal SBR progress.
                val projDoomMain = self.doom + ((self.es.num + 2) * 5 / 3) + self.gates.num
                val lateSBRsUnfulfilled = self.needs(FBSecondAwakening) || self.needs(FBThirdAwakening)
                val ghatoKillBase = laterAP && gooOnMap > 0 && self.gates.num >= 1 &&
                    game.fbGhatnothoaAwakenings >= 1 && game.fbGhatnothoaAwakenings < 3 &&
                    self.spellbooks.num >= 2 && (power + infernalDiscount) >= effectiveAwakenCost + 2
                val ap2WritheForGhatoKill = ghatoKillBase &&
                    game.ritualCost > 6 && (power + unflippedSBsMain > 6) &&
                    projDoomMain >= 30 && lateSBRsUnfulfilled
                val ap2WritheForGhatoKillID = ghatoKillBase && idImminent
                ap2WritheForGhatoKill |=> 9200 -> "Writhe for Ghato re-awaken kill"
                (ap2WritheForGhatoKillID && !ap2WritheForGhatoKill) |=> 9300 -> "Writhe for Ghato kill (instant death imminent)"
                // Writhe to steal vulnerable enemy gate — must have an actually vulnerable gate
                val hasVulnGate = areas.%(r => gateVulnerability(r) > 0).any
                val ap2WritheForEnemyGate = laterAP && gooOnMap > 0 && writheDice >= 4 && self.gates.num >= 1 && hasVulnGate
                // Post-awaken at < 3 gates with 2+ gates: writhe-capture beats defensive moves
                val urgentCapture = gooOnMap > 0 && self.gates.num == 2 && game.fbGhatnothoaAwakenings >= 1
                (urgentCapture && ap2WritheForEnemyGate) |=> 9300 -> "Writhe to recapture 3rd gate (2g post-awaken)"
                // RECOVERY: 1 gate + Ghato awakened + writhe dice > 3 — writhe for 2nd gate
                val recoveryWrithe = gooOnMap > 0 && self.gates.num == 1 && game.fbGhatnothoaAwakenings >= 1 && writheDice > 3
                (recoveryWrithe && ap2WritheForEnemyGate) |=> 8000 -> "RECOVERY: Writhe capture for 2nd gate (1g + Ghato)"
                (!urgentCapture && !recoveryWrithe && ap2WritheForEnemyGate) |=> 6000 -> "AP2+ Writhe to steal vulnerable enemy gate"
                // WRITHE-BUILD FALLBACK: no vulnerable enemy gates exist, need a gate
                // Writhe to move Ghato+cultist to empty land (preferably adjacent to own gate), then build
                val noVulnerableGates = laterAP && gooOnMap > 0 && self.gates.num < 3 && writheDice >= 4 && !ap2WritheForEnemyGate
                noVulnerableGates |=> 5000 -> "Writhe-build: no vulnerable gates, writhe to empty land for gate"
                // Writhe to claim unclaimed gate with FB defender (rev/ghato there, need cultist)
                val hasUnclaimedGateWithDefender = laterAP && self.gates.num < 3 && writheDice >= 2 &&
                    game.gates.exists(gr => !self.gates.has(gr) &&
                        others.%(_.gates.has(gr)).none &&
                        (self.at(gr, Ghatanothoa).any || self.at(gr, RevenantOfKnaa).any) &&
                        self.at(gr).%(_.canControlGate).none)
                hasUnclaimedGateWithDefender |=> 9100 -> "Writhe to claim gate with FB defender (send cultist)"
                // General AP2+ Writhe when affordable. Baseline traces show
                // FB Writhing in AP3+ to keep producing desc/pairs even at
                // moderate power. Lowered gate from power>=6 to power>=4
                // (Writhe costs 2, leaving 2 minimum). Score 7000 to BEAT
                // BuildGateAction (8200 for 2nd gate, 7500 for 3rd) when bot
                // already has 2+ gates — keep producing pipeline over building
                // additional gates that don't multiply doom much.
                // Restore baseline writhe scoring (confirmed at 2.3%)
                val ap2PlusGenericWrithe = laterAP && self.gates.num >= 1 && writheDice >= 4 && desiccatedOnMap < 6
                ap2PlusGenericWrithe |=> 5500 -> "AP2+ generic Writhe (gates + dice 4+)"
                val ap2PlusWrithe2Gates = laterAP && self.gates.num >= 2 && writheDice >= 4 && desiccatedOnMap < 6
                ap2PlusWrithe2Gates |=> 7000 -> "AP2+ Writhe with 2+ gates (desc production)"
                val gatesWithDefender = self.gates.%(r =>
                    self.at(r, Ghatanothoa).any || self.at(r, RevenantOfKnaa).any).num
                val needsRedistribute = self.gates.num >= 3 && gatesWithDefender < self.gates.num.min(3)
                val writheFirstHighPower = laterAP && writheDice >= 7 && desiccatedOnMap < 6 &&
                    (self.gates.num < 3 || needsRedistribute)
                writheFirstHighPower |=> 8500 -> "Writhe FIRST at dice 7+ (expand or redistribute)"
                // Tactic 03: writhe to balance at 3 gates (controlled + free with FB units)
                val freeGatesWithFB = game.gates.%(r => !self.gates.has(r) && !others.exists(_.gates.has(r)) && self.at(r).any)
                val allFBGateRegions = self.gates ++ freeGatesWithFB
                val gateUnitCountsMain = allFBGateRegions./(gr => self.at(gr).num)
                val maxGateUnits = if (gateUnitCountsMain.any) gateUnitCountsMain.max else 0
                val minGateUnits = if (gateUnitCountsMain.any) gateUnitCountsMain.min else 0
                val countImbalanced = maxGateUnits - minGateUnits > 2
                // Only writhe balance for SEVERE imbalance (> 2 unit difference), not composition
                // Rev summon (T04) handles undefended gates, not writhe
                val effectiveGateCount = self.gates.num + freeGatesWithFB.num
                val writheForBalance = laterAP && effectiveGateCount >= 3 && writheDice >= 6 && countImbalanced
                writheForBalance |=> 8800 -> "T03: Writhe to BALANCE units across 3 gates (disparity > 2)"
                val ap3WritheFor3rdGate = thirdAP && self.gates.num == 2 && writheDice >= 6 && desiccatedOnMap < 4
                ap3WritheFor3rdGate |=> 7500 -> "AP3 Writhe push for 3rd gate (2 gates, dice 6+)"
                // Block Writhe when FB has no gate AND no Ghato (nothing to do).
                // But ALLOW Writhe at 0 gates when Ghato on map — Writhe-pain Ghato
                // to enemy gate for capture is the recovery path.
                val zeroGatesNoGhato = laterAP && self.gates.num == 0 && gooOnMap == 0
                zeroGatesNoGhato |=> -3000 -> "don't Writhe without gate or Ghato"
                val zeroGatesWithGhato = laterAP && self.gates.num == 0 && gooOnMap > 0 && writheDice >= 4
                zeroGatesWithGhato |=> 7000 -> "URGENT: Writhe at 0 gates with Ghato (pain to steal)"
                // Block third+ Writhe in AP1 when desc already high (diminishing returns)
                (firstAP && desiccatedOnMap >= 3) |=> -5000 -> "AP1 third+ Writhe: already 3+ desiccated"
                // Fix 1: Block writhe when Ghato is on empty/vulnerable enemy gate
                // CoF or recruit should handle claiming the gate instead
                val ghatoOnClaimableGate = laterAP && game.gates.exists(r =>
                    (self.at(r, Ghatanothoa).any || self.at(r, RevenantOfKnaa).any) &&
                    !self.gates.has(r) &&
                    others./~(_.at(r)).%(_.uclass.utype == GOO).none)
                (ghatoOnClaimableGate) |=> -12000 -> "BLOCK writhe: Ghato on claimable gate — use CoF/recruit"
                // Low-power defense writhe: 3 controlled gates, < 3 defended by ghato/rev
                // Worth doing at 1 power if there's a SB to flip for IP
                val defendedGates = self.gates.%(gr =>
                    self.at(gr, Ghatanothoa).any || self.at(gr, RevenantOfKnaa).any).num
                val hasFlippableForWrithe = self.spellbooks.exists(sb =>
                    sb != DevilsMark && sb != CyclopeanGaze && !self.oncePerGame.has(sb))
                // Defense writhe conditions:
                //   1. > 1 controlled gate
                //   2. Controlled gates <= CG defenders on map (enough defenders exist)
                //   3. > 1 controlled gate has no CG defender (redistribution needed)
                // If defenders < gates, summon a rev instead — writhe can't create defenders.
                val cgDefendersOnMap = self.all(Ghatanothoa).num + self.all(RevenantOfKnaa).num
                val undefendedGateCount = self.gates.num - defendedGates
                val defenseWrithe = laterAP && self.gates.num > 1 &&
                    self.gates.num <= cgDefendersOnMap &&
                    undefendedGateCount > 1 &&
                    (power >= 2 || (power >= 1 && hasFlippableForWrithe && infernalDiscount < 1))
                defenseWrithe |=> 8700 -> "writhe defense: redistribute CG defenders across gates"
                // B4: Enemy GOO arrived at Ghato's region → writhe Ghato AWAY to next best gate
                val ghatoThreatenedByGOO = laterAP && gooOnMap > 0 && writheDice >= 4 &&
                    self.all(Ghatanothoa).exists(u => others./~(_.at(u.region)).%(_.uclass.utype == GOO).any)
                ghatoThreatenedByGOO |=> 9300 -> "B4: URGENT writhe — enemy GOO at Ghato's region"
                // Cathedral vacating: Ghato at cathedral with AN combat > 0, and Ghato cost >= 4
                val ghatoAtDangerousCathedral = laterAP && gooOnMap > 0 && power >= 4 &&
                    self.all(Ghatanothoa).exists { u =>
                        game.cathedrals.has(u.region) &&
                        AN.exists && AN.at(u.region).%(a => a.uclass.utype == Monster || a.uclass.utype == Terror).any &&
                        u.uclass.cost >= 4
                    }
                ghatoAtDangerousCathedral |=> 9200 -> "URGENT writhe — Ghato at cathedral with AN combat (vacate)"
                // (T04 writhe block removed — counterproductive)

            // Round 9: pre-action block — if AP1 second action already fires,
            // allow a targeted follow-up Writhe only if power is very high.

            // ──────────────────────────────────────────────────────────────────
            // WRITHE REROLL / KEEP
            // ──────────────────────────────────────────────────────────────────
            case FBWritheRerollAction(_, rolls) =>
                val kills = rolls.count(_ == Kill)
                val pains = rolls.count(_ == Pain)
                val misses = rolls.count(_ == Miss)
                val total = rolls.num
                // Post-awaken capture mode: check if a gate target exists matching tiers 1-5
                val postAwakenCapture = gooOnMap > 0 && self.gates.num < 3
                // Does ANY enemy gate match tiers 1-4 (no GOO, combat < 4)?
                val hasT1to4Target = others.exists(f => f.gates.exists { gr =>
                    val foeUnits = f.at(gr)
                    val foeGoos = foeUnits.%(_.uclass.utype == GOO)
                    val foeMon = foeUnits.%(_.uclass.utype == Monster).num
                    val foeTer = foeUnits.%(_.uclass.utype == Terror).num
                    val combat = foeMon + foeTer * 4
                    foeGoos.none && combat < 4
                })
                // Does ANY enemy gate match tier 5 (combat > 3, need 3+ pains)?
                val hasT5Target = others.exists(f => f.gates.exists { gr =>
                    val foeUnits = f.at(gr)
                    val foeGoos = foeUnits.%(_.uclass.utype == GOO)
                    val foeMon = foeUnits.%(_.uclass.utype == Monster).num
                    val foeTer = foeUnits.%(_.uclass.utype == Terror).num
                    val combat = foeMon + foeTer * 4
                    foeGoos.none && combat >= 4 && gr.glyph != Ocean
                })
                // B3b: DY gate detection — gate with Dark Young but no GOO, needs 2+ pains to send battle force
                val hasDYGateTarget = others.exists(f => f.gates.exists { gr =>
                    val foeUnits = f.at(gr)
                    val hasDY = foeUnits.%(_.uclass == DarkYoung).any
                    val foeGoos = foeUnits.%(_.uclass.utype == GOO)
                    foeGoos.none && hasDY
                })
                // Keep if: target exists AND enough pains to use it
                val canUseT1to4 = hasT1to4Target && pains >= 1
                val canUseT5 = hasT5Target && pains >= 3
                val canUseDY = hasDYGateTarget && pains >= 2
                val hasUsableTarget = canUseT1to4 || canUseT5 || canUseDY
                val shouldRerollCapture = !hasUsableTarget || pains == 0
                // B3b: DY-specific reroll — if ONLY DY gates available, need 2+ pains
                val onlyDYTargets = hasDYGateTarget && !hasT1to4Target && !hasT5Target
                val shouldRerollDY = onlyDYTargets && pains < 2
                // Writhe-kill mode: must have > 1 kill to kill Ghato
                val fbTotalDoomReroll = self.doom + (self.es.num * 5 / 3)
                val inKillMode = game.ritualCost > 6 && fbTotalDoomReroll >= 20 &&
                    gooOnMap > 0 && game.fbGhatnothoaAwakenings >= 1 && game.fbGhatnothoaAwakenings < 3
                val shouldRerollKillMode = inKillMode && kills < 2
                // Default reroll rule for other situations
                // AP1 with no monsters: reroll if < 2 kills or > 4 kills
                val ap1NoMonsters = firstAP && desiccatedOnMap == 0 && gooOnMap == 0
                val shouldRerollAP1NoMon = ap1NoMonsters && (kills < 2 || kills > 4)
                // AP1 with OW in game: need at least 1 kill (Beyond One steals undefended gates)
                val ap1OwNeedsKill = firstAP && OW.exists && kills < 1
                val maxKills = if (firstAP) 5 else 3
                val minPains = if (self.gates.num >= 3) 1 else 2
                val shouldRerollDefault = kills >= maxKills || pains < minPains || misses * 2 > total
                (ap1OwNeedsKill) |=> 9000 -> "AP1 OW in game: reroll (need >= 1 kill for gate defense)"
                (ap1NoMonsters && shouldRerollAP1NoMon) |=> 9000 -> "AP1 no monsters: reroll (need 2-4 kills)"
                (ap1NoMonsters && !shouldRerollAP1NoMon && !ap1OwNeedsKill) |=> -9000 -> "AP1 no monsters: good roll (2-4 kills)"
                // Kill mode overrides default
                (inKillMode && shouldRerollKillMode) |=> 9000 -> "reroll: kill mode needs > 1 kill"
                (inKillMode && !shouldRerollKillMode) |=> -9000 -> "don't reroll: kill mode has enough kills"
                (postAwakenCapture && !inKillMode && shouldRerollDY) |=> 9000 -> "B3b reroll: DY gate target but < 2 pains"
                (postAwakenCapture && !inKillMode && shouldRerollCapture) |=> 9000 -> "reroll: no viable gate target or 0 pains"
                // Recovery reroll: > 50% misses at power > 4
                val recoveryReroll = postAwakenCapture && !inKillMode && power > 4 && misses * 2 > total
                recoveryReroll |=> 9000 -> "reroll: recovery mode, > 50% misses"
                (postAwakenCapture && !inKillMode && !shouldRerollCapture && !shouldRerollDY) |=> -9000 -> "don't reroll: gate target exists + pains available"
                (!postAwakenCapture && !inKillMode && shouldRerollDefault) |=> 9000 -> "reroll: default"
                (!postAwakenCapture && !inKillMode && !shouldRerollDefault) |=> -9000 -> "don't reroll: good roll"
                // v5.16 (2026-05-14): user rule — reroll if < 2 pains (capture/combat
                // chains need at least 2 pains). Additive to other reroll criteria.
                (pains < 2) |=> 9000 -> "reroll: < 2 pains (capture/combat chain needs 2+)"

            case FBWritheKeepAction(_, rolls) =>
                val kills = rolls.count(_ == Kill)
                val pains = rolls.count(_ == Pain)
                val misses = rolls.count(_ == Miss)
                val total = rolls.num
                val postAwakenCapture = gooOnMap > 0 && self.gates.num < 3
                val hasT1to4Target = others.exists(f => f.gates.exists { gr =>
                    val foeUnits = f.at(gr)
                    val foeGoos = foeUnits.%(_.uclass.utype == GOO)
                    val foeMon = foeUnits.%(_.uclass.utype == Monster).num
                    val foeTer = foeUnits.%(_.uclass.utype == Terror).num
                    val combat = foeMon + foeTer * 4
                    foeGoos.none && combat < 4
                })
                val hasT5Target = others.exists(f => f.gates.exists { gr =>
                    val foeUnits = f.at(gr)
                    val foeGoos = foeUnits.%(_.uclass.utype == GOO)
                    val foeMon = foeUnits.%(_.uclass.utype == Monster).num
                    val foeTer = foeUnits.%(_.uclass.utype == Terror).num
                    val combat = foeMon + foeTer * 4
                    foeGoos.none && combat >= 4 && gr.glyph != Ocean
                })
                val hasDYGateTarget2 = others.exists(f => f.gates.exists { gr =>
                    val foeUnits = f.at(gr)
                    val hasDY = foeUnits.%(_.uclass == DarkYoung).any
                    val foeGoos = foeUnits.%(_.uclass.utype == GOO)
                    foeGoos.none && hasDY
                })
                val canUseT1to4 = hasT1to4Target && pains >= 1
                val canUseT5 = hasT5Target && pains >= 3
                val canUseDY2 = hasDYGateTarget2 && pains >= 2
                val hasUsableTarget = canUseT1to4 || canUseT5 || canUseDY2
                val shouldKeepCapture = hasUsableTarget && pains >= 1
                val onlyDYTargets2 = hasDYGateTarget2 && !hasT1to4Target && !hasT5Target
                val shouldKeepDY = onlyDYTargets2 && pains >= 2
                val fbTotalDoomKeep = self.doom + (self.es.num * 5 / 3)
                val inKillModeKeep = game.ritualCost > 6 && fbTotalDoomKeep >= 20 &&
                    gooOnMap > 0 && game.fbGhatnothoaAwakenings >= 1 && game.fbGhatnothoaAwakenings < 3
                val shouldKeepKillMode = inKillModeKeep && kills >= 2
                val maxKillsKeep = if (firstAP) 5 else 3
                val minPainsKeep = if (self.gates.num >= 3) 1 else 2
                val shouldKeepDefault = kills < maxKillsKeep && pains >= minPainsKeep && misses * 2 <= total
                // AP1 OW in game: don't keep if 0 kills (need desc for gate defense)
                val ap1OwNoKillKeep = firstAP && OW.exists && kills < 1
                (ap1OwNoKillKeep) |=> -9000 -> "AP1 OW: don't keep (need >= 1 kill for gate defense)"
                (inKillModeKeep && shouldKeepKillMode) |=> 9000 -> "keep: kill mode has enough kills"
                (inKillModeKeep && !shouldKeepKillMode) |=> -9000 -> "don't keep: kill mode needs > 1 kill"
                (postAwakenCapture && !inKillModeKeep && shouldKeepDY) |=> 9000 -> "B3b keep: DY gate target with 2+ pains"
                (postAwakenCapture && !inKillModeKeep && shouldKeepCapture) |=> 9000 -> "keep: gate target + pains"
                (postAwakenCapture && !inKillModeKeep && !shouldKeepCapture && !shouldKeepDY) |=> -9000 -> "don't keep"
                (!postAwakenCapture && !inKillModeKeep && shouldKeepDefault) |=> 9000 -> "keep: default"
                (!postAwakenCapture && !inKillModeKeep && !shouldKeepDefault) |=> -9000 -> "don't keep: reroll"

            // ──────────────────────────────────────────────────────────────────
            // WRITHE KILL UNIT — Eliminate target selection
            // ──────────────────────────────────────────────────────────────────
            case FBWritheKillUnitAction(_, uRef, _, _) =>
                val u = game.unit(uRef)
                val inStart = fbStartRegion.contains(u.region)
                val isOnlyControl = u.faction == FB && u.region.ownGate &&
                    self.at(u.region).%(_.canControlGate).num <= 1
                // NEVER kill the last gate-keeper — UNLESS it's Ghato for re-awaken.
                val nextAwakenCostGK = math.max(1, 11 - game.ritualCost)
                val effectiveAwakenCostGK = math.max(1, nextAwakenCostGK - infernalDiscount)
                val fbTotalDoomGK = self.doom + (self.es.num * 5 / 3)
                val unflippedSBsGK = self.spellbooks.count(sb => !self.oncePerGame.has(sb))
                val fbTotalDoomGK2 = self.doom + (self.es.num * 5 / 3)
                // Note: power is AFTER writhe cost (-2), so add 2 back for the check
                val powerBeforeWrithe = power + 2
                val ghatoReAwaken = u.uclass == Ghatanothoa && u.faction == FB &&
                    laterAP && self.gates.num >= 1 &&
                    game.fbGhatnothoaAwakenings >= 1 && game.fbGhatnothoaAwakenings < 3 &&
                    self.spellbooks.num >= 2 && power >= effectiveAwakenCostGK &&
                    game.ritualCost > 6 && (powerBeforeWrithe + unflippedSBsGK > 6) && fbTotalDoomGK2 >= 20
                // Gate keeper kill OK if CoF available AND Ghato/Rev on this gate (can replenish)
                val hasCoFForKill = self.has(CallOfTheFaithful) && !self.oncePerGame.has(CallOfTheFaithful)
                val gateHasDefender = u.region.ownGate &&
                    (self.at(u.region, Ghatanothoa).any || self.at(u.region, RevenantOfKnaa).any)
                val canReplenishKeeper = hasCoFForKill && gateHasDefender
                (isOnlyControl && !ghatoReAwaken && !canReplenishKeeper) |=> -12000 -> "NEVER kill last gate keeper (no CoF/defender)"
                (isOnlyControl && canReplenishKeeper) |=> 3000 -> "kill gate keeper OK (CoF + defender can replenish)"
                // Do NOT reduce gate-region keepers below 2 unless CoF can replenish.
                // v5.16 (2026-05-14): only fires when u IS canControlGate (killing this
                // unit actually reduces the keeper count). Was misfiring on Desc kills
                // and making Rev kills (at -3000) outscore Desc kills (at -1000 + -8000),
                // causing the bot to kill Revs even when 4 Descs were available.
                val gateKeepCountAfter = u.faction == FB && u.region.ownGate && u.canControlGate &&
                    self.at(u.region).%(_.canControlGate).num <= 2
                (gateKeepCountAfter && !ghatoReAwaken && !canReplenishKeeper) |=> -8000 -> "keep at least 2 gate-keepers"
                // v5.2 (2026-05-13): AP1 surplus-desc-on-home-gate priority.
                // When the start gate has > 1 Desc, killing one of those is the
                // PREFERRED kill target — it doesn't hurt FB (surplus on a gate
                // with multiple desc is wasted) and avoids destroying remote
                // build-out Desc / dropping cultists below 3.
                val startGateRegion = fbStartRegion.filter(self.gates.contains)
                val descOnStartGate = startGateRegion.map(r => self.at(r, Desiccated).num).getOrElse(0)
                val killSurplusDescOnStart = firstAP && u.uclass == Desiccated &&
                    u.faction == FB && startGateRegion.contains(u.region) && descOnStartGate > 1
                killSurplusDescOnStart |=> 5500 -> "AP1: kill surplus Desc on home gate (preserves build-out)"
                // AP1: never kill a remote Desc (not in start region) — it's the
                // build-out for the 2nd gate.
                val killRemoteDesc = firstAP && u.uclass == Desiccated && u.faction == FB &&
                    !fbStartRegion.contains(u.region)
                killRemoteDesc |=> -8000 -> "AP1: NEVER kill remote Desc (build-out)"
                // AP1: don't kill cultists when total FB cultists on map < 3
                val fbCultistsOnMap = self.allInPlay.%(_.uclass.utype == Cultist).num
                val killCultistLowCount = firstAP && u.cultist && u.faction == FB && fbCultistsOnMap < 3
                killCultistLowCount |=> -8000 -> "AP1: don't kill cultist (< 3 cultists on map)"
                // Kill last cultist in any region LAST (preserve presence)
                val lastInRegion = u.cultist && self.at(u.region).%(_.canControlGate).num == 1
                val notOnGate = !u.onGate
                (lastInRegion && notOnGate) |=> -3000 -> "kill last cultist in region last"
                // AP1 > 4 kills: kill home region cultist first (thin home, preserve outposts)
                val ap1HighKills = firstAP && inStart
                ap1HighKills |=> 2000 -> "AP1: prefer killing home region cultist"
                // Until 6 desiccated on map: prefer FB acolyte in starting region not on gate
                val killStartAcolyte = desiccatedOnMap < 6 && u.uclass == Acolyte && u.faction == FB && inStart && !u.onGate
                killStartAcolyte |=> 5000 -> "kill FB Acolyte in start region (creates Desiccated)"
                val killStartGate = desiccatedOnMap < 6 && u.uclass == Acolyte && u.faction == FB && inStart && u.onGate
                killStartGate |=> -8000 -> "don't kill gate-keeper Acolyte"
                // Re-awaken via Writhe-kill Ghato. Per user formula:
                // power_after_killing_ghato (== current power, kill is free)
                // must be >= current_awaken_cost + 2.
                // Per user constraint: only re-awaken when the OTHER 4 SBRs
                // are already satisfied (i.e., FB has 4 SBs). Otherwise the
                // re-awaken SBR (FBSecondAwakening / FBThirdAwakening) doesn't
                // unlock a new SB pick beyond what the other 4 SBRs would.
                val nextAwakenCost = math.max(1, 11 - game.ritualCost)
                val effectiveAwakenCostKill = math.max(1, nextAwakenCost - infernalDiscount)
                // Strategy 3.1: writhe-kill Ghato when ritual cost > 6 AND power + unflipped SBs > 6
                val unflippedSBs = self.spellbooks.count(sb => !self.oncePerGame.has(sb))
                val fbTotalDoomKill = self.doom + (self.es.num * 5 / 3)
                val fbTotalDoomKill2 = self.doom + (self.es.num * 5 / 3)
                val powerBeforeWrithe2 = power + 2
                val reAwakenBase = laterAP && self.gates.num >= 1 &&
                    game.fbGhatnothoaAwakenings >= 1 && game.fbGhatnothoaAwakenings < 3 &&
                    u.uclass == Ghatanothoa && u.faction == FB &&
                    self.spellbooks.num >= 2 &&
                    power >= effectiveAwakenCostKill
                val reAwakenWindow = reAwakenBase &&
                    game.ritualCost > 6 &&
                    (powerBeforeWrithe2 + unflippedSBs > 6) && fbTotalDoomKill2 >= 20
                val reAwakenWindowID = reAwakenBase && idImminent
                // v5.14 (2026-05-14): intent check — when writhe-MAIN's capture
                // tier (urgentCapture / ap2WritheForEnemyGate at 9300+) would beat
                // the re-awaken-kill tier (9200), the writhe intent was capture.
                // Reflect that in the kill decision: keep Ghato alive (pain to
                // the capture target instead). Mirror the writhe-MAIN conditions.
                val urgentCaptureIntent = laterAP && gooOnMap > 0 && self.gates.num >= 1 &&
                    areas.%(rg => gateVulnerability(rg) > 0).any && !huntTargetGooRegion.isDefined
                val captureIntentBeatsKill = u.uclass == Ghatanothoa && reAwakenWindow &&
                    !reAwakenWindowID && urgentCaptureIntent
                captureIntentBeatsKill |=> -5500 -> "writhe intent is capture — keep Ghato alive for pain-capture"
                (reAwakenWindow && !captureIntentBeatsKill) |=> 9000 -> "Writhe-kill Ghato for re-awaken"
                (reAwakenWindowID && !reAwakenWindow) |=> 9100 -> "Writhe-kill Ghato (instant death imminent)"
                // After 6 desiccated: prefer desiccated
                (desiccatedOnMap >= 6 && u.uclass == Desiccated) |=> 4000 -> "prefer killing desiccated when at cap"
                // Always avoid killing your own valuable units
                (u.uclass == Desiccated && desiccatedOnMap < 6) |=> -1000 -> "avoid killing desiccated below cap"
                (u.uclass == RevenantOfKnaa) |=> -3000 -> "avoid killing revenant"
                (u.uclass == Ghatanothoa && !reAwakenWindow && !reAwakenWindowID) |=> -15000 -> "NEVER kill Ghato (re-awaken conditions not met)"
                (u.uclass == Ghatanothoa && (reAwakenWindow || reAwakenWindowID)) |=> -5000 -> "avoid killing GOO (re-awaken possible)"
                (u.uclass == HighPriest) |=> -2500 -> "avoid killing high priest"

            // ──────────────────────────────────────────────────────────────────
            // WRITHE PAIN UNIT SELECTION
            // Priority: 1 desiccated + 2 cultists. If >3 pains, 2 desiccated + 2 cultists.
            // If >4 pains or only 1 kill, send more cultists.
            // ──────────────────────────────────────────────────────────────────
            case FBWritheChoosePainUnitAction(_, uRef, remainingPains, chosen) =>
                val u = game.unit(uRef)
                val totalPains = chosen.num + remainingPains
                val chosenDesc = chosen.count(r => game.unit(r).uclass == Desiccated)
                val chosenCult = chosen.count(r => game.unit(r).cultist)
                val isDesc = u.uclass == Desiccated
                val isCult = u.cultist
                // Default targets: 1 desc, 2 cult; >3 pains: 2 desc, 2 cult
                val descCap = if (totalPains > 3) 2 else 1
                val cultCap = 2
                val moreCultists = totalPains > 4 || (chosenDesc <= 1 && totalPains > chosenDesc + cultCap)

                // AP1 WRITHE-BUILD: pick the just-killed Desc FIRST so it can be
                // pained out to a new region; cultists follow it. Without this,
                // the standard scoring picked cultists before the Desc, leaving
                // the Desc stuck home and a lone cultist scattered.
                val ap1DescFirst = firstAP && isDesc && chosenDesc < descCap && desiccatedOnMap >= 1
                ap1DescFirst |=> 8000 -> "AP1: pick Desc first (so cultists can follow it)"
                // Cultist-first only AFTER at least 1 desc has been picked — that
                // way cultists follow a desc, not lead.
                val descOffGateRegions = self.onMap(Desiccated)./(_.region).distinct.%(r => !self.gates.has(r) && !fbStartRegion.contains(r))
                (firstAP && isCult && descOffGateRegions.any && chosenCult == 0 && chosenDesc >= 1) |=> 7000 -> "AP1: cultist first to join off-gate desiccated"

                // INSTANT DEATH HOME EVACUATION: when writhe-kill is active via instant death,
                // prioritize paining cultists from the home region first
                val writheKillActive = idImminent && gooOnMap > 0 &&
                    game.fbGhatnothoaAwakenings >= 1 && game.fbGhatnothoaAwakenings < 3
                val inHomeRegion = fbStartRegion.contains(u.region)
                (writheKillActive && isCult && inHomeRegion) |=> 5000 -> "ID: evacuate home region cultist first"

                // PREFER OFF-GATE UNITS: non-gate units pained FIRST before stripping gates
                val onOwnGate = u.region.ownGate && self.gates.has(u.region)
                val notOnAnyGate = !onOwnGate
                // Strong preference: non-gate units always before gate units
                (notOnAnyGate) |=> 4000 -> "prefer non-gate unit for pain (don't strip gates)"
                (onOwnGate) |=> -3000 -> "avoid paining unit from controlled gate"
                // v5.13 (2026-05-14): HARD block — don't pain the LAST monster
                // (Desc/Rev/Ghato) off a controlled gate. At each controlled gate
                // we want at least one monster to stay (rev preferred, desc fallback).
                val uIsMonster = u.uclass == Desiccated || u.uclass == RevenantOfKnaa || u.uclass == Ghatanothoa
                val isLastMonsterAtGate = onOwnGate && uIsMonster &&
                    self.at(u.region).%(m => (m.uclass == Desiccated || m.uclass == RevenantOfKnaa || m.uclass == Ghatanothoa) && m != u).none
                isLastMonsterAtGate |=> -12000 -> "BLOCK: don't pain last monster off controlled gate"

                // GHATO BACKFILL: if Ghato already chosen AND was on a gate with no
                // other CG defender, the NEXT unit MUST be a rev from a non-gate region
                val ghatoAlreadyChosenPain = chosen.exists(r => game.unit(r).uclass == Ghatanothoa)
                val ghatoWasOnUndefendedGate = ghatoAlreadyChosenPain && chosen.exists { r =>
                    val gu = game.unit(r)
                    gu.uclass == Ghatanothoa && self.gates.has(gu.region) &&
                    self.at(gu.region, RevenantOfKnaa).none
                }
                // Rev from non-gate region: highest priority to backfill Ghato's gate
                val isRevOffGate = u.uclass == RevenantOfKnaa && notOnAnyGate
                (ghatoWasOnUndefendedGate && isRevOffGate) |=> 9500 -> "MUST: rev from non-gate to backfill Ghato's gate"
                // If no free rev, rev from double-defended gate
                val isRevFromDoubleGate = u.uclass == RevenantOfKnaa && onOwnGate &&
                    self.at(u.region, RevenantOfKnaa).num >= 2
                (ghatoWasOnUndefendedGate && isRevFromDoubleGate) |=> 9000 -> "rev from double-defended gate to backfill Ghato"

                // SURPLUS REV BACKFILL: when Ghato chosen for writhe-capture and FB has 2 gates,
                // if any region has revs > gates in that region, that surplus rev should be
                // pained 2nd after Ghato, destination = Ghato's controlled gate.
                val surplusRevRegion = self.gates.num == 2 && ghatoAlreadyChosenPain &&
                    u.uclass == RevenantOfKnaa && {
                        val revsHere = self.at(u.region, RevenantOfKnaa).num
                        val gatesHere = self.gates.%(g => g == u.region).num
                        revsHere > gatesHere
                    }
                surplusRevRegion |=> 9500 -> "surplus rev backfill: pain to Ghato's gate"

                // ── v5 (2026-05-13) CONSOLIDATE-AROUND-GHATO MODE ──
                // When capture isn't viable (no T1-T4 target) AND battle isn't viable
                // (FB power-dice < strongest enemy gate's units) AND every FB controlled
                // gate already has a Rev plus >= 2 total units (well-defended baseline),
                // pain SURPLUS units from over-populated gates toward Ghato to set up a
                // bigger battle force. Order: surplus Revs (> 1 Rev) first, then surplus
                // Desiccated. Each controlled gate keeps 1 Rev + 1 Cultist + 1 more unit;
                // pains beyond the surplus stop (keep-in-place is just lack of score).
                val hasCapTarget = areas.exists(rg => gateVulnerability(rg, totalPains) >= 9000)
                val maxEnemyGateUnits = others./~(_.gates)./(rg => rg.foes.num).maxOption.getOrElse(0)
                val fbBattleDice = power  // FB power = dice rolled in battle (Ghato strength uses power)
                val battleViable = fbBattleDice > maxEnemyGateUnits
                val allGatesDefendedBaseline = self.gates.nonEmpty && self.gates.forall(g =>
                    self.at(g, RevenantOfKnaa).num >= 1 && self.at(g).num >= 2)
                val anyGateOverpopulated = self.gates.exists(g => self.at(g).num > 3)
                val consolidateMode = !hasCapTarget && !battleViable &&
                    allGatesDefendedBaseline && anyGateOverpopulated &&
                    self.all(Ghatanothoa).any
                // Calculate surplus for u's region: keep floor = 1 Rev + 1 Cultist + 1 more
                val uOnFBGate = self.gates.contains(u.region)
                val revsAtURegion = self.at(u.region, RevenantOfKnaa).num
                val cultsAtURegion = self.at(u.region).cultists.num
                val totalAtURegion = self.at(u.region).num
                // Surplus Rev = revs > 1 at a controlled gate
                val surplusRevConsolidate = consolidateMode && u.uclass == RevenantOfKnaa &&
                    uOnFBGate && revsAtURegion > 1
                surplusRevConsolidate |=> 9300 -> "v5 consolidate: pain surplus rev to join Ghato"
                // Surplus Desc = total units > 3 AND removing this leaves >= 3
                // (1 rev + 1 cultist + 1 other must remain)
                val surplusDescConsolidate = consolidateMode && u.uclass == Desiccated &&
                    uOnFBGate && totalAtURegion > 3 && revsAtURegion >= 1 && cultsAtURegion >= 1
                surplusDescConsolidate |=> 9100 -> "v5 consolidate: pain surplus desc to join Ghato"
                // Don't pain when removing would drop below baseline (1 rev + 1 cult + 1 more)
                val wouldBreakBaseline = consolidateMode && uOnFBGate && totalAtURegion <= 3
                wouldBreakBaseline |=> -8000 -> "v5 consolidate: keep baseline gate units (>=3)"

                // BALANCE WRITHE: pain from region with MOST units to redistribute
                val gateUnitCountsPain = self.gates./(gr => self.at(gr).num)
                val maxUnitsAtGate = if (gateUnitCountsPain.any) gateUnitCountsPain.max else 0
                val unitFromMostPopulated = onOwnGate && self.at(u.region).num == maxUnitsAtGate && maxUnitsAtGate > 3
                (unitFromMostPopulated && isCult) |=> 3000 -> "balance: pain cultist from most populated gate"
                (unitFromMostPopulated && isDesc) |=> 2500 -> "balance: pain desc from most populated gate"

                // Defense writhe: pain the rev/ghato NOT on a controlled gate, or rev from double-defended gate
                val defGatesPain = self.gates.%(gr =>
                    self.at(gr, Ghatanothoa).any || self.at(gr, RevenantOfKnaa).any).num
                val needsDefenseRedist = self.gates.num >= 3 && defGatesPain < 3
                val isDefenderOffGate = (u.uclass == RevenantOfKnaa || u.uclass == Ghatanothoa) && !onOwnGate
                val isRevOnDoubleDefGate = u.uclass == RevenantOfKnaa && onOwnGate &&
                    (self.at(u.region, Ghatanothoa).any || self.at(u.region, RevenantOfKnaa).num >= 2)
                (needsDefenseRedist && isDefenderOffGate) |=> 8000 -> "defense writhe: pain defender off-gate to undefended gate"
                (needsDefenseRedist && isRevOnDoubleDefGate) |=> 7500 -> "defense writhe: pain rev from double-defended gate"

                // When Ghato already chosen: cultist MUST come next (needed for capture at gate)
                val ghatoAlreadyChosen = chosen.exists(r => game.unit(r).uclass == Ghatanothoa)
                val captureMode2 = gooOnMap > 0 && self.gates.num < 3
                (isCult && ghatoAlreadyChosen && captureMode2 && chosenCult == 0) |=> 6200 -> "pain cultist WITH Ghato (capture needs cultist!)"
                (isDesc && chosenDesc < descCap) |=> 6000 -> "pain desiccated up to cap"
                (isCult && chosenCult < cultCap) |=> 5500 -> "pain cultist up to cap"
                (isCult && chosenCult >= cultCap && moreCultists) |=> 4000 -> "extra cultist (high pain count or low kills)"
                (isDesc && chosenDesc >= descCap) |=> 1000 -> "extra desiccated"
                // Ghato pain: HIGHEST when in capture mode (< 3 gates, Ghato alive).
                val ghatoForGateSteal = u.uclass == Ghatanothoa && gooOnMap > 0 && self.gates.num < 3
                ghatoForGateSteal |=> 6500 -> "pain Ghato for gate-steal (ABOVE desc — ensures Ghato pained)"
                val ghatoReposition = u.uclass == Ghatanothoa && gooOnMap > 0 && self.gates.num >= 3
                ghatoReposition |=> 4500 -> "pain Ghato for repositioning (3+ gates)"
                (u.uclass == Ghatanothoa && !ghatoForGateSteal && !ghatoReposition) |=> -3000 -> "don't pain Ghato (no steal needed)"
                (u.uclass == RevenantOfKnaa) |=> 500 -> "OK to pain Revenant occasionally"

            // ──────────────────────────────────────────────────────────────────
            // WRITHE PAIN DESTINATIONS — strategy from user:
            //   - Don't move cultist solo to enemy region (always pair with desc)
            //   - If FB has desc + cultist, move them as a pair to the same target
            //   - Either MoveAll or MoveSeparately is fine as long as pairing
            //     happens at the destination.
            // ──────────────────────────────────────────────────────────────────
            case FBWritheMoveAllToRegionAction(_, r, chosen) =>
                r.chaosGate |=> -3000 -> "avoid writhe to chaos gate"
                val land = r.glyph != Ocean
                val empty = r.foes.none
                val lonefoe = r.foes.cultists.any && r.foes.monsterly.none && r.foes.goos.none
                val noGate = !r.gate
                val chosenHasDesc = chosen.exists(ur => game.unit(ur).uclass == Desiccated)
                val chosenHasCult = chosen.exists(ur => game.unit(ur).cultist)
                val isPair = chosenHasDesc && chosenHasCult
                // AP1 spread check: if dest already has FB desc+cult (pair complete),
                // MoveAll clusters MORE units there — use MoveSeparately instead.
                val fbDescAtDest = self.at(r, Desiccated).any
                val fbCultAtDest = self.at(r).cultists.any
                val destAlreadyPaired = fbDescAtDest && fbCultAtDest
                val ap1AlreadyPaired = firstAP && destAlreadyPaired
                // AP1: BLOCK MoveAll if it would abandon the home gate
                // MoveAll sends ALL chosen units to one region — if chosen includes
                // home gate keepers and chosen.num > 2, home gate gets abandoned.
                // Force MoveSeparately instead so some units stay home.
                val ap1WouldAbandonHome = firstAP && chosen.num > 2 && fbStartRegion.isDefined &&
                    chosen.exists(ur => game.unit(ur).region == fbStartRegion.get)
                ap1WouldAbandonHome |=> -10000 -> "BLOCK MoveAll AP1: would abandon home gate (use separate)"
                // AP1 only: desc+cult pair to empty land for gate building
                (isPair && !ap1AlreadyPaired && firstAP && land && empty && noGate) |=> 8000 -> "move-all: AP1 desc+cult pair to empty land"
                (isPair && !ap1AlreadyPaired && firstAP && land && lonefoe && noGate) |=> 7500 -> "move-all: AP1 desc+cult pair to lone cultist"
                // AP1 MoveAll: faction adjacency penalties (same as MoveOne)
                if (firstAP && land && empty && noGate) {
                    val adjFactionsMoveAll = r.near.flatMap(nr => others.filter(_.at(nr).any)).distinct
                    (adjFactionsMoveAll.num >= 2) |=> -200 -> "move-all AP1: 2 adj factions"
                    (adjFactionsMoveAll.num >= 3) |=> -200 -> "move-all AP1: 3+ adj factions"
                    adjFactionsMoveAll.foreach { f =>
                        val pen = f match {
                            case YS => -350; case TS => -300; case BG => -250; case OW => -200
                            case CC => -150; case GC => -100; case AN => -50; case _ => 0
                        }
                        if (pen != 0) true |=> pen -> ("move-all AP1: adj " + f.short)
                    }
                }
                (isPair && ap1AlreadyPaired && land) |=> 2000 -> "move-all: AP1 dest already paired (prefer separate)"
                // AP1: cap at 2 FB units per destination (no need for 3+)
                val fbUnitsAtDestAll = self.at(r).num
                (firstAP && fbUnitsAtDestAll >= 2) |=> -2000 -> "move-all AP1: already 2+ FB units at dest"
                // Writhe-capture chain: ONLY when < 3 gates. At 3 gates, pivot to holding.
                val captureMode = gooOnMap > 0 && self.gates.num < 3
                // In capture mode: empty land is LOW (5000) so all gate tiers win
                // At 3 gates: empty land stays low, reinforce own gates instead
                // Post-awaken: BLOCK empty land destinations — units go to gates only
                (gooOnMap > 0 && !firstAP && land && empty && noGate && !r.enemyGate) |=> -10000 -> "BLOCK scatter: post-awaken MoveAll, no empty land"
                (isPair && !firstAP && gooOnMap == 0 && land && empty && noGate) |=> 8000 -> "move-all: no Ghato, pair to empty land"
                // At 3 gates: reinforce own gates with MoveAll
                // At 3 gates: MoveAll to own gate scored by need (fewer units = higher score)
                if (gooOnMap > 0 && self.gates.num >= 3 && r.ownGate) {
                    val unitsAtDest = self.at(r).num
                    val needScore = (7000 - unitsAtDest * 500).max(4000)
                    true |=> needScore -> ("move-all: 3g reinforce gate (" + unitsAtDest + " units, score " + needScore + ")")
                }
                // GATE DEFENSE: MoveAll to gate with cultists but no monster
                val moveAllGateNeedsMonster = r.ownGate &&
                    self.at(r).%(_.canControlGate).any &&
                    self.at(r).%(m => m.uclass == Desiccated || m.uclass == RevenantOfKnaa || m.uclass == Ghatanothoa).none &&
                    chosenHasDesc
                moveAllGateNeedsMonster |=> 8500 -> "DEFEND: MoveAll desc to undefended gate"

                // ── GATE VULNERABILITY SCORING (MoveAll) ──
                // Uses shared gateVulnerability() ranking. Scores: T0a(9800) > T0b(9700) >
                // T0c(9600) > T1(9500) > T2(9200) > T3(8900) > T4(8600) > T5(8300).
                // Returns -1 for blocked gates (Ice Age, cathedral+AN combat).
                // T5 (high combat) requires 3+ pains — blocked here if insufficient.
                val chosenHasGhato = chosen.exists(ur => game.unit(ur).uclass == Ghatanothoa)
                val chosenHasCultist = chosen.exists(ur => game.unit(ur).cultist)
                val fbCultAlreadyThere = self.at(r).cultists.any
                val canHoldGate = chosenHasCultist || fbCultAlreadyThere
                val chosenCount = chosen.num
                val freeGate = r.freeGate
                val isLand = r.glyph != Ocean
                val foeCombat = r.foes.%(_.uclass.utype == Monster).num + r.foes.%(_.uclass.utype == Terror).num * 4
                // Vulnerability score with pain count for T5 check
                val vulnScore = gateVulnerability(r, chosenCount)
                // Apply vulnerability score for Ghato writhe-capture destinations
                (chosenHasGhato && vulnScore > 0 && self.gates.num < 3) |=> vulnScore -> ("gate vuln " + vulnScore)
                // Block gates marked as dangerous (-1 = Ice Age or cathedral+AN combat)
                (vulnScore == -1) |=> -12000 -> "BLOCK: gate blocked (Ice Age/cathedral+AN combat)"

                // WRITHE-BUILD: empty land for gate building (below all gate tiers)
                val writheBuildTarget = isLand && !r.gate && r.foes.none && self.gates.num < 3 && chosenHasGhato
                val adjToOwnGate = self.gates.exists(g => r.near.contains(g))
                (writheBuildTarget && adjToOwnGate) |=> 7000 -> "writhe-build: empty land adjacent to own gate"
                (writheBuildTarget && !adjToOwnGate) |=> 5500 -> "writhe-build: empty land (not adjacent)"

                // SL HOME GATE BOOST: only SL gates vulnerable + cursed slumber
                (chosenHasGhato && slHomeGateBoost.contains(r) && self.gates.num < 3) |=> 9600 -> "SL home gate: only SL vuln + cursed slumber"

                // GATE-LESS WRITHE: no suitable gates, writhe to safe non-gate land
                val noGatesAvailable = areas.%(g => gateVulnerability(g) > 0).none && !writheBuildTarget
                val safety = regionSafety(r)
                val safeWithEscort = chosenHasGhato && (self.at(r).num >= 2 || chosen.num >= 3)
                (noGatesAvailable && isLand && safety >= 3000 && safeWithEscort) |=> safety -> ("gate-less writhe: safe land " + safety)
                (noGatesAvailable && isLand && safety >= 2000 && safeWithEscort) |=> safety -> ("gate-less writhe: moderate land " + safety)
                // 3rd/4th safest + < 3 pains: pain 2 desc only, no ghato
                val lowPainNoGhato = noGatesAvailable && isLand && safety >= 1000 && safety < 3000 &&
                    !chosenHasGhato && chosenCount <= 2
                lowPainNoGhato |=> (safety - 500) -> ("gate-less writhe: desc only to risky land " + safety)

                // All gate tiers beat empty land (5000 post-awaken)
                r.enemyGate |=> 1500 -> "move-all: enemy gate fallback"
                (r.foes.cultists.any && r.foes.monsterly.none) |=> 1000 -> "move-all: lone cultists fallback"

            case FBWritheMoveSeparatelyAction(_, chosen) =>
                // AP1 with 2+ desc+cult: prefer separate to spread to 2 regions.
                val descCount = chosen.count(ur => game.unit(ur).uclass == Desiccated)
                val cultCount = chosen.count(ur => game.unit(ur).cultist)
                val canSplitPairs = firstAP && descCount >= 2 && cultCount >= 2
                canSplitPairs |=> 8500 -> "AP1: move separately (2+ desc+cult for 2-gate spread)"
                // At 3+ gates: prefer separate so units go to different gates for balance
                val gateCountsSep = self.gates./(gr => self.at(gr).num)
                val imbalancedSep = gateCountsSep.any && (gateCountsSep.max - gateCountsSep.min > 1)
                (gooOnMap > 0 && self.gates.num >= 3 && imbalancedSep && chosen.num >= 2) |=>
                    8500 -> "3g balance: move separately to distribute across gates"
                // GATE DEFENSE: when Ghato is being pained FROM an own gate that has no
                // other monster defender, MUST separate so desc/rev can backfill
                val hasGhato = chosen.exists(ur => game.unit(ur).uclass == Ghatanothoa)
                val hasNonGhato = chosen.exists(ur => game.unit(ur).uclass != Ghatanothoa)
                val hasDescOrRev = chosen.exists(ur => {
                    val uc = game.unit(ur).uclass; uc == Desiccated || uc == RevenantOfKnaa
                })
                val ghatoLeavingOwnGate = hasGhato && chosen.exists(ur => {
                    val gu = game.unit(ur)
                    gu.uclass == Ghatanothoa && self.gates.has(gu.region) &&
                    self.at(gu.region, RevenantOfKnaa).none && self.at(gu.region, Desiccated).none
                })
                (ghatoLeavingOwnGate && hasDescOrRev) |=>
                    9500 -> "MUST separate: Ghato leaving own gate, desc/rev must backfill"
                // Strategy guide: separate when Ghato needs enemy gate but desc+cult need different dest
                val enemyGateAvailable = others.exists(f => f.gates.%(r => r.foes.goos.none && r.glyph != Ocean).any)
                (hasGhato && hasNonGhato && enemyGateAvailable && self.gates.num < 3) |=> 8800 -> "separate: Ghato to enemy gate, others to expansion"
                true |=> 2500 -> "move separately"

            case FBWritheMoveOneToRegionAction(_, uRef, r, remaining) =>
                r.chaosGate |=> -3000 -> "avoid writhe to chaos gate"
                val u = game.unit(uRef)
                val totalAfter = remaining.num + 1
                val land = r.glyph != Ocean
                val ocean = r.glyph == Ocean
                val enemyCults = r.foes.cultists
                val lonefoe = enemyCults.any && r.foes.monsterly.none && r.foes.goos.none
                // Prefer land for high-pain scenarios
                val highPain = totalAfter >= 3
                val veryHighPain = totalAfter >= 6
                val lowPain = totalAfter <= 2
                val distFromFoes = distFromEnemies(r)

                // SAME-REGION: unit can stay where it is (pain back to same gate)
                val sameRegion = u.region == r
                val srcOwnGate = self.gates.has(u.region)
                val isLastKeeper = u.canControlGate && srcOwnGate &&
                    self.at(u.region).%(_.canControlGate).num <= 1
                val isLastMonster = (u.uclass == Desiccated || u.uclass == RevenantOfKnaa) && srcOwnGate &&
                    self.at(u.region).%(m => m.uclass == Desiccated || m.uclass == RevenantOfKnaa).num <= 1
                // Last keeper CAN leave if: rev/ghato at gate + < 6 cultists on map + CoF available
                val hasDefenderAtGate = srcOwnGate &&
                    (self.at(u.region, Ghatanothoa).any || self.at(u.region, RevenantOfKnaa).any)
                val cultistsOnMapPain = self.all(Acolyte).num + self.all(HighPriest).num
                val hasCoF = self.has(CallOfTheFaithful) && !self.oncePerGame.has(CallOfTheFaithful)
                val canLeaveLastKeeper = hasDefenderAtGate && cultistsOnMapPain < 6 && hasCoF
                // Last keeper MUST stay unless all 3 conditions met
                (sameRegion && isLastKeeper && !canLeaveLastKeeper) |=> 10000 -> "MUST stay: last gate keeper (no defender/CoF/cultists)"
                (sameRegion && isLastKeeper && canLeaveLastKeeper) |=> 4000 -> "last keeper CAN leave (defender+CoF+<6 cult)"
                (sameRegion && isLastMonster) |=> 9500 -> "MUST stay: last monster pained back to same gate"
                (sameRegion && srcOwnGate) |=> 6000 -> "pain unit back to same gate (stay in place)"

                // GATE BALANCE: don't create 2+ unit imbalance between source and dest gates
                val srcIsOwnGate = self.gates.has(u.region)
                val dstIsOwnGate = self.gates.has(r)
                val srcUnitsAfter = if (srcIsOwnGate && !sameRegion) self.at(u.region).num - 1 else self.at(u.region).num
                val dstUnitsAfter = if (dstIsOwnGate && !sameRegion) self.at(r).num + 1 else self.at(r).num
                val wouldImbalance = srcIsOwnGate && dstIsOwnGate && !sameRegion && (dstUnitsAfter - srcUnitsAfter > 2)
                wouldImbalance |=> -9000 -> "BLOCK: would create 2+ unit imbalance between FB gates"

                // SURPLUS REV DESTINATION: when this rev is being pained during a writhe
                // and FB has 2 gates, send it to the FB gate that lacks a defender
                // (the gate Ghato just vacated for writhe-capture).
                val isSurplusRevDest = u.uclass == RevenantOfKnaa && self.gates.num == 2 && {
                    val revsAtSrc = self.at(u.region, RevenantOfKnaa).num
                    val gatesAtSrc = self.gates.%(g => g == u.region).num
                    revsAtSrc > gatesAtSrc
                }
                val undefendedGate = if (isSurplusRevDest)
                    self.gates.%(g => self.at(g, Ghatanothoa).none && self.at(g, RevenantOfKnaa).none).headOption
                else None
                (isSurplusRevDest && undefendedGate.contains(r)) |=> 9800 -> "surplus rev to undefended gate for backfill"
                (isSurplusRevDest && undefendedGate.isDefined && !undefendedGate.contains(r)) |=> -5000 -> "surplus rev must go to undefended gate, not here"

                // Round 9 CRITICAL: do NOT move the on-gate gate-keeper out of
                // its region. If this unit is the only controller of FB's gate,
                // moving it loses the gate.
                val isGateKeeper = u.onGate && u.region.ownGate &&
                    self.at(u.region).%(_.canControlGate).num <= 1
                isGateKeeper |=> -9000 -> "do NOT move last gate-keeper out"

                // Round 9: if the unit is in FB's start region and is the LAST
                // acolyte/HP there (no other canControlGate), keep it put —
                // moving it loses the home gate.
                val lastInStart = u.cultist && fbStartRegion.contains(u.region) &&
                    self.at(u.region).%(_.canControlGate).num <= 1
                val needsEmptyStart = self.needs(FBNoAcolytesInStart)
                // Allow clearing start when: AP1 + SBR needed, OR crater at start (DM already fired, no gate to protect)
                val craterAtStart = fbStartRegion.exists(game.fbCraters.has)
                val startHasNoGate = fbStartRegion.exists(r2 => !self.gates.has(r2))
                val canClearStart = (firstAP && needsEmptyStart) || (needsEmptyStart && craterAtStart && startHasNoGate)
                (lastInStart && !canClearStart) |=> -9000 -> "do NOT move last cultist out of start region"
                // Encourage clearing start after DM — but only to adjacent safe regions
                val safeAdjacentDest = r.glyph != Ocean && r.foes.monsterly.none && r.foes.goos.none
                (u.cultist && needsEmptyStart && craterAtStart && startHasNoGate &&
                    fbStartRegion.contains(u.region) && safeAdjacentDest) |=> 4000 -> "clear start after DM to safe region (NoAcolytesInStart SBR)"

                // Soft reserve — discourage stripping start below 2 canControlGate,
                // EXCEPT in AP1 when FBNoAcolytesInStart is still unfulfilled.
                val stripStartReserve = u.cultist && fbStartRegion.contains(u.region) &&
                    self.at(u.region).%(_.canControlGate).num <= 2
                (stripStartReserve && !canClearStart) |=> -3000 -> "soft reserve: keep 2+ canControlGate in start"

                // Round 9 strategy: "lone enemy cultist override" — ALWAYS send
                // desiccated + cultist to a land region with a lone enemy cultist
                // (cultist expected to flee → we build gate there).
                (land && lonefoe) |=> 7500 -> "lone enemy cultist override: send unit"

                // PAIRING: if this unit is a cultist AND FB already has a desc/rev/ghato
                // at the destination (placed earlier this Writhe session), boost the
                // pair score. Subset check: FB has monster at dest BEFORE this move.
                val fbMonsterAtDest = self.at(r).%(u => u.uclass == Desiccated || u.uclass == RevenantOfKnaa || u.uclass == Ghatanothoa).num
                val fbCultAtDest = self.at(r).cultists.num
                val pairingWithFbMonster = u.cultist && fbMonsterAtDest >= 1 && u.region != r
                // AP1 spread: if dest already has desc+cult (pair complete for build),
                // DON'T send more cultists there — spread to a DIFFERENT region for
                // 2nd gate. Keeps pairing score high for first pair only.
                val alreadyPaired = fbMonsterAtDest >= 1 && fbCultAtDest >= 1
                val spreadInAP1 = firstAP && alreadyPaired
                // Post-awaken: pair to own gates/Ghato location, not empty land (reduce scatter)
                // When Ghato is alive: non-Ghato units should reinforce gates, not scatter
                val ghatoAlive = gooOnMap > 0
                (pairingWithFbMonster && !spreadInAP1 && !ghatoAlive && land && r.empty) |=> 8500 -> "PAIR cult→desc at empty land (no Ghato)"
                (pairingWithFbMonster && !spreadInAP1 && !ghatoAlive && land && lonefoe) |=> 8800 -> "PAIR cult→desc at lone cultist (no Ghato)"
                (pairingWithFbMonster && !spreadInAP1 && !ghatoAlive && land) |=> 7800 -> "PAIR cult→desc at any land (no Ghato)"
                // Ghato alive: units go to own gates, NEVER scatter to empty land
                val gateUnitCounts = self.gates./(gr => self.at(gr).num)

                // TACTIC 03: Balance units across 3 gates toward ideal (1 ghato/rev, 2 desc, 2 cult)
                // Score each own gate by how much it NEEDS the unit type being placed
                val threeGateBalance = ghatoAlive && self.gates.num >= 3 && r.ownGate
                if (threeGateBalance) {
                    val destCults = self.at(r).%(_.canControlGate).num
                    val destDescs = self.at(r, Desiccated).num
                    val destHasGhato = self.at(r, Ghatanothoa).any
                    val destHasRev = self.at(r, RevenantOfKnaa).any
                    val destHasDefender = destHasGhato || destHasRev
                    // Cultist needed: gate has < 2 cultists
                    val cultDeficit = (2 - destCults).max(0)
                    // Desc needed: gate has < 2 desiccated
                    val descDeficit = (2 - destDescs).max(0)
                    // Defender needed: gate has no ghato/rev
                    (u.cultist && cultDeficit > 0) |=> (7200 + cultDeficit * 200) -> "T03: cult to gate needing cultists"
                    (u.uclass == Desiccated && descDeficit > 0) |=> (7100 + descDeficit * 200) -> "T03: desc to gate needing desiccated"
                    (u.uclass == RevenantOfKnaa && !destHasDefender) |=> 7500 -> "T03: rev to undefended gate"
                    // NEVER stack ghato+rev on same gate when another gate is undefended
                    val otherGateUndefended = self.gates.exists(g2 => g2 != r &&
                        self.at(g2, Ghatanothoa).none && self.at(g2, RevenantOfKnaa).none)
                    (u.uclass == RevenantOfKnaa && destHasGhato && otherGateUndefended) |=> -12000 -> "T03: BLOCK rev stacking with Ghato (other gate undefended)"
                    (u.uclass == Ghatanothoa && destHasRev && otherGateUndefended) |=> -12000 -> "T03: BLOCK Ghato stacking with Rev (other gate undefended)"
                    // Avoid sending more units to already-ideal gates
                    val destTotal = self.at(r).num
                    (destTotal >= 5) |=> -2000 -> "T03: gate already at ideal (5 units)"
                }

                // Post-DM: prioritize clearing cultists from home/crater region to controlled gates (NoAcolytesInStart SBR)
                val homeHasCrater = fbStartRegion.exists(game.fbCraters.has)
                val unitFromHome = fbStartRegion.contains(u.region)
                val needsClearStart = self.needs(FBNoAcolytesInStart) && homeHasCrater
                (u.cultist && unitFromHome && needsClearStart && r.ownGate) |=> 8500 -> "clear home: cult from crater to own gate"
                (u.uclass == Desiccated && unitFromHome && needsClearStart && r.ownGate) |=> 8300 -> "clear home: desc from crater to own gate"

                // Rev to own gate without ghato/rev (CG defense): highest priority for rev pain
                val gateNeedsCGDefender = r.ownGate &&
                    self.at(r, Ghatanothoa).none && self.at(r, RevenantOfKnaa).none
                val revFromNonGate = u.uclass == RevenantOfKnaa && !srcOwnGate
                val revFromDoubleDefended = u.uclass == RevenantOfKnaa && srcOwnGate &&
                    self.at(u.region).%(x => x.uclass == RevenantOfKnaa && x != u).any
                ((revFromNonGate || revFromDoubleDefended) && gateNeedsCGDefender) |=> 9500 -> "Rev to own gate without CG defender (replace Ghato)"

                // GATE DEFENSE: own gate with cultists but no monster — desc reinforces
                val gateHasCultButNoMonster = r.ownGate &&
                    self.at(r).%(_.canControlGate).any &&
                    self.at(r).%(m => m.uclass == Desiccated || m.uclass == RevenantOfKnaa || m.uclass == Ghatanothoa).none
                // Score ABOVE capture tiers (9500) — never leave a gate undefended during writhe-capture
                (u.uclass == Desiccated && gateHasCultButNoMonster) |=> 9700 -> "DEFEND: desc to own gate with no monster (beats capture)"
                ((u.uclass == RevenantOfKnaa) && gateHasCultButNoMonster) |=> 9700 -> "DEFEND: rev to own gate with no monster (beats capture)"
                // Monsters to gates with fewest cultists (general preference)
                if ((u.uclass == Desiccated || u.uclass == RevenantOfKnaa) && r.ownGate) {
                    val destCultsForMonster = self.at(r).%(_.canControlGate).num
                    val monsterCultBoost = (4 - destCultsForMonster).max(0) * 200
                    monsterCultBoost > 0 |=> monsterCultBoost -> ("monster to gate with fewer cultists +" + monsterCultBoost)
                }

                // C2: orphaned units → pain to FB gate with least units
                val isLeastUnitsGate = r.ownGate && gateUnitCounts.any && self.at(r).num == gateUnitCounts.min
                (ghatoAlive && u.cultist && isLeastUnitsGate) |=> 7200 -> "C2: cult to least-populated own gate"
                (ghatoAlive && u.uclass == Desiccated && isLeastUnitsGate) |=> 7100 -> "C2: desc to least-populated own gate"
                (ghatoAlive && u.cultist && r.ownGate) |=> 7000 -> "Ghato alive: cult to own gate (reduce scatter)"
                (ghatoAlive && u.uclass == Desiccated && r.ownGate) |=> 6800 -> "Ghato alive: desc to own gate (reduce scatter)"
                // HARD BLOCK: post-awaken (not AP1), never pain units to empty non-gate regions
                (ghatoAlive && !firstAP && land && r.empty && !r.gate && !r.enemyGate) |=> -10000 -> "BLOCK scatter: post-awaken, no empty land (gates only)"
                // AP1 already-paired dest: lower score to encourage spread
                (pairingWithFbMonster && spreadInAP1 && land) |=> 4000 -> "AP1: dest already paired, spread elsewhere"
                // AP1: cap at 2 FB units per destination region (no need for 3+)
                val fbUnitsAtDest = self.at(r).num
                (firstAP && !sameRegion && fbUnitsAtDest >= 2) |=> -2000 -> "AP1: already 2+ FB units at dest, spread elsewhere"
                // v5.15 (2026-05-14): AP1 SPLIT SCENARIO — gated on first writhe of
                // AP1 only with 2+ kills AND 4+ pains. Each desc to a DIFFERENT
                // non-home region so each pair can build its own gate.
                val killsThisWritheR = game.fbWritheRolls.count(_ == Kill)
                val painsThisWritheR = game.fbWritheRolls.count(_ == Pain)
                val splitScenarioToReg = firstAP && killsThisWritheR >= 2 && painsThisWritheR >= 4 &&
                    desiccatedOnMap == killsThisWritheR
                val descStackingAway = u.uclass == Desiccated && !sameRegion &&
                    self.at(r, Desiccated).any && !fbStartRegion.contains(r)
                (splitScenarioToReg && descStackingAway) |=> -3000 -> "AP1 split: don't stack Desc in same away region"

                // v5.16 (2026-05-14): AP1 writhe target preference — when YS in game,
                // prefer land regions exactly 3 distant from YS start (YS reach
                // limit; FB sits outside YS's screaming/desecrate range).
                val ap1YsDistanceBoost = firstAP && YS.exists && land && !sameRegion &&
                    game.starting.get(YS).exists(ysStart => game.board.distance(r, ysStart) == 3)
                ap1YsDistanceBoost |=> 1500 -> "AP1: prefer land 3 regions from YS"

                // v5.17 (2026-05-14): Ghato writhe-pain to enemy GOO when the
                // pain force has a "likely kill" margin AND FB has >= 6 power.
                // likelyKills formula:
                //   ceil(((pains_rolled - 1) * (desc_combat + ghato_combat)) / 6)
                //   + augury_banked_kills
                // (desc_combat = 1; ghato_combat = FB power)
                val ghatoPainTargetGOO = u.uclass == Ghatanothoa && !sameRegion &&
                    power >= 6 &&
                    enemyGooAt(r)
                if (ghatoPainTargetGOO) {
                    val painsRolledG = game.fbWritheRolls.count(_ == Pain)
                    val descCombatG = 1
                    val ghatoCombatG = power
                    val likelyKillsG = math.ceil(((painsRolledG - 1) * (descCombatG + ghatoCombatG)) / 6.0).toInt + auguryKills
                    val foeUnitsAtGooG = others.flatMap(_.at(r)).num
                    (foeUnitsAtGooG <= likelyKillsG) |=> 9800 -> ("Ghato writhe-pain to GOO (likely kills " + likelyKillsG + " >= foe units " + foeUnitsAtGooG + ")")
                }

                // AP1: concentrate cultists, don't scatter to multiple non-home regions
                if (firstAP && fbStartRegion.isDefined) {
                    // v5.13 (2026-05-14): directly count cultists already away from home
                    // (prior formula `6 - homeCults - desc` undercounted because it didn't
                    // notice partial writhe-moves mid-action).
                    val cultsAwayDirect = self.allInPlay.%(c => c.cultist && !fbStartRegion.contains(c.region)).num
                    // After 2 cultists already away (following the desc), additional
                    // cultists stay home — block non-home destinations + boost home.
                    (u.cultist && !sameRegion && !fbStartRegion.contains(r) && cultsAwayDirect >= 2) |=> -10000 -> "AP1: 2 cultists already away, rest stay home"
                    (u.cultist && sameRegion && srcOwnGate && cultsAwayDirect >= 2) |=> 10000 -> "AP1: stay at home gate (2 already out)"
                    (u.cultist && fbStartRegion.contains(r) && !sameRegion && cultsAwayDirect >= 2) |=> 10000 -> "AP1: return to home gate (2 already out)"
                    // ALL AP1 writhes: send cultist to where FB already has a cultist (concentrate)
                    val fbCultAtDest = !sameRegion && self.at(r).%(_.cultist).any && cultsAwayDirect >= 1
                    (u.cultist && fbCultAtDest) |=> 9000 -> "AP1: join cultist at same dest (concentrate)"
                    // Block scattering: if a non-home region already has FB cultists, penalize OTHER non-home regions
                    val existingNonHomeRegions = areas.%(ar => ar != fbStartRegion.get && self.at(ar).%(_.cultist).any)
                    val destIsNewRegion = !sameRegion && !fbStartRegion.contains(r) && !existingNonHomeRegions.has(r)
                    val alreadyHaveOutpost = existingNonHomeRegions.any
                    (u.cultist && destIsNewRegion && alreadyHaveOutpost) |=> -5000 -> "AP1: don't scatter to new region (outpost exists)"
                }
                // AP1: block enemy gates and enemy-occupied regions — use empty land only
                (firstAP && !sameRegion && r.enemyGate) |=> -10000 -> "AP1: BLOCK enemy gate (5th-6th choice)"
                val hasEnemyMonstersOrGoos = r.foes.%(f => f.uclass.utype == Monster || f.uclass.utype == Terror || f.uclass.utype == GOO).any
                (firstAP && !sameRegion && hasEnemyMonstersOrGoos) |=> -8000 -> "AP1: avoid enemy monsters/GOOs"
                // AP1 adjacency: penalize regions adjacent to enemy monsters/GOOs
                if (firstAP && land && r.empty && !sameRegion) {
                    // 1-region buffer checks
                    val adjMonsterOrGoo = r.near.exists(nr => others.exists(f =>
                        f.at(nr).%(u2 => u2.uclass.utype == Monster || u2.uclass.utype == Terror || u2.uclass.utype == GOO).any))
                    val adjEnemyGate = r.near.exists(nr => nr.enemyGate)
                    adjMonsterOrGoo |=> -1000 -> "AP1: adjacent to enemy monsters/GOOs"
                    adjEnemyGate |=> -500 -> "AP1: adjacent to enemy gate"
                    // 2-region buffer boost: no enemy monsters/GOOs/gates within 2 steps
                    val twoStepDanger = r.near./~(_.near).distinct.exists { nr2 =>
                        nr2.enemyGate || others.exists(f =>
                            f.at(nr2).%(u2 => u2.uclass.utype == Monster || u2.uclass.utype == Terror || u2.uclass.utype == GOO).any)
                    }
                    (!adjMonsterOrGoo && !adjEnemyGate && !twoStepDanger) |=> 1500 -> "AP1: 2-region buffer from all threats"
                }
                // Block scattering to water in AP1
                (firstAP && !sameRegion && !land) |=> -8000 -> "AP1: never scatter to water"

                // v5.18 (2026-05-13): PENALIZE paining units BACK to home/starting region.
                // Writhe pain should scatter units outward, not consolidate at home.
                // Exception: AP1 with 2+ cultists already away (line 1281 handles that case).
                val destIsHome = fbStartRegion.contains(r) && !sameRegion
                val ap1HomeReturnOK = firstAP && {
                    val cultsAway = self.allInPlay.%(c => c.cultist && !fbStartRegion.contains(c.region)).num
                    cultsAway >= 2
                }
                (destIsHome && !ap1HomeReturnOK) |=> -9000 -> "BLOCK: don't pain units back to home region (scatter outward)"

                // WW faction: avoid Antarctica + Ice Age as Writhe pain destinations
                (WW.exists && r.name == "Antarctica") |=> -8000 -> "Writhe: avoid Antarctica when WW in game"
                (WW.exists && WW.iceAge.contains(r)) |=> -12000 -> "BLOCK: Ice Age region (writhe pain)"

                // AN cathedral danger: avoid when AN combat units present (not just global Unholy Ground)
                val hasCathedralHere = game.cathedrals.has(r)
                val anCombatHere = AN.exists && AN.at(r).%(u2 => u2.uclass.utype == Monster || u2.uclass.utype == Terror).any
                (u.uclass == Ghatanothoa && hasCathedralHere && anCombatHere) |=> -12000 -> "BLOCK: Ghato to cathedral with AN combat"

                // SOLO CULTIST BLOCK: don't send a lone cultist INTO an enemy region
                // without an FB monster pair. Magnitude must exceed max positive
                // rule score so it dominates compareEL as the top (negative) element.
                val hasAnyFoe = r.foes.any
                val noPairHere = fbMonsterAtDest == 0
                val soloCultistDanger = u.cultist && hasAnyFoe && noPairHere && u.region != r
                soloCultistDanger |=> -12000 -> "block solo cultist → enemy region without pair (HARD)"

                // 3+ pains: land only, lone cultist target preferred
                (highPain && land && lonefoe) |=> 6500 -> "high-pain: land region with lone unprotected cultist"
                (highPain && ocean) |=> -2000 -> "high-pain: avoid ocean"

                // "Furthest empty land from enemies" — core AP1-AP3 rule. Send
                // desiccated+cultist to the empty land region furthest from foes.
                (firstAP && land && r.empty && distFromFoes >= 2) |=> (5500 + distFromFoes * 300) -> "AP1: empty land far from foes"
                (!firstAP && self.gates.num < 3 && land && r.empty && distFromFoes >= 2) |=> (5200 + distFromFoes * 300) -> "later AP: empty land far from foes (gates<3)"
                // Glyph region boost: regions with a starting glyph are strategically valuable
                val hasGlyph = r.glyph == GlyphAA || r.glyph == GlyphOO || r.glyph == GlyphWW
                (hasGlyph && land && r.empty) |=> 150 -> "glyph region boost"

                // AP1 faction adjacency scoring: prefer fewer adjacent factions, avoid dangerous ones
                if (firstAP && land && r.empty) {
                    // Count adjacent factions (factions with units in adjacent regions)
                    val adjacentFactions = r.near.flatMap(nr => others.filter(_.at(nr).any)).distinct
                    val adjCount = adjacentFactions.num
                    // Fewer adjacent factions = better (200 per extra faction)
                    (adjCount >= 2) |=> -200 -> "AP1: adjacent to 2 factions (-200)"
                    (adjCount >= 3) |=> -200 -> "AP1: adjacent to 3+ factions (-400 total)"
                    // Per-faction danger: YS most dangerous, WW least
                    // Ordering: YS(-350) < TS(-300) < BG(-250) < OW(-200) < CC(-150) < GC(-100) < AN(-50) < WW(0)
                    adjacentFactions.foreach { f =>
                        val penalty = f match {
                            case YS => -350
                            case TS => -300
                            case BG => -250
                            case OW => -200
                            case CC => -150
                            case GC => -100
                            case AN => -50
                            case _  => 0   // WW or unknown
                        }
                        if (penalty != 0) {
                            true |=> penalty -> ("AP1: adjacent to " + f.short + " danger")
                        }
                    }
                }

                // Land region with cultists, prioritize SL,TS,BG,CC,WW,OW,AN,GC; avoid YS w/ Passion
                if (highPain && land && enemyCults.any) {
                    val tsOrder = $(SL, TS, BG, CC, WW, OW, AN, GC)
                    val foesPresent : $[Faction] = enemyCults./(_.faction).distinct
                    val passionYS = YS.exists && YS.has(Passion) && foesPresent.contains(YS)
                    foesPresent.foreach { f =>
                        val rank = tsOrder.indexOf(f)
                        if (rank >= 0) {
                            true |=> (5500 - rank * 100) -> ("high-pain: priority faction " + f)
                        }
                    }
                    if (passionYS && foesPresent == $(YS)) {
                        true |=> -3000 -> "avoid YS with Passion"
                    }
                }

                // Low pain (≤2) OR units after 1st desiccated+cultist: empty land near start
                val nearStartEmpty = lowPain && land && r.empty && fbStartRegion.exists(s => r.near.contains(s) || r == s)
                nearStartEmpty |=> 5000 -> "low-pain: empty land near start region"
                val oceanFallback = lowPain && ocean && areas.%(a => a.glyph != Ocean && a.empty && fbStartRegion.exists(s => a.near.contains(s) || a == s)).none
                oceanFallback |=> 1500 -> "low-pain: ocean fallback when no empty land near start"

                // 6+ pains: 1 cultist adjacent to FB glyph, further from other factions
                if (veryHighPain && u.cultist && fbStartGlyphRegion.exists(s => r.near.contains(s) || r == s)) {
                    val nearOthers = r.near.%(nr => others.exists(_.at(nr).any)).num
                    true |=> (5800 - nearOthers * 200) -> "very-high-pain: cultist adjacent to FB glyph, away from foes"
                }

                // ── GATE VULNERABILITY SCORING (MoveSeparate) ──
                // Uses shared gateVulnerability() with painCount = remaining+1.
                // Blocks (-1) and T5 pain check handled inside the function.
                val isGhatoUnit = u.uclass == Ghatanothoa
                val sepVulnScore = gateVulnerability(r, remaining.num + 1)
                // Apply vulnerability score for Ghato separate-writhe destinations
                (isGhatoUnit && sepVulnScore > 0 && self.gates.num < 3) |=> sepVulnScore -> ("gate vuln " + sepVulnScore)
                // Block dangerous gates (-1 = Ice Age or cathedral+AN combat)
                (sepVulnScore == -1) |=> -12000 -> "BLOCK: gate blocked (Ice Age/cathedral+AN combat)"
                // SL Cursed Slumber threat: avoid SL gates if SL has Cursed Slumber and no gate currently slumbered
                val slCursedThreat = SL.exists && SL.has(CursedSlumber) &&
                    r.enemyGate && SL.gates.has(r) &&
                    game.gates.%(_.glyph == Slumber).none
                (isGhatoUnit && slCursedThreat) |=> -300 -> "avoid SL gate: Cursed Slumber threat"
                // Target doom leader's gates: +300 if leader > 25 total points, +200 more if leader > 3 gates
                val enemyTotals = others./(f => (f, f.doom + f.es.num * 5 / 3))
                val doomLeader = if (enemyTotals.any) enemyTotals.maxBy(_._2) else null
                if (doomLeader != null && doomLeader._2 > 25 && r.enemyGate && doomLeader._1.gates.has(r)) {
                    true |=> 300 -> ("target doom leader " + doomLeader._1.short)
                    (doomLeader._1.gates.num > 3) |=> 200 -> ("doom leader has > 3 gates")
                }
                // 3+ gates: gate recovery after DM — use vulnerability ranking
                val sepVuln3plus = gateVulnerability(r, remaining.num + 1)
                (isGhatoUnit && sepVuln3plus > 0 && self.gates.num >= 3) |=> 7500 -> "Ghato to enemy gate (3+ gates — recovery)"

                // B2c: Writhe pain cultist to empty gate where Ghato/Rev is (to claim it)
                val ghatoOrRevAtGate = (self.at(r, Ghatanothoa).any || self.at(r, RevenantOfKnaa).any) &&
                    game.gates.has(r) && !self.gates.has(r)
                val noEnemyGooAtDest = others./~(_.at(r)).%(_.uclass.utype == GOO).none
                val noFBCultAtDest = self.at(r).%(_.canControlGate).none
                (u.cultist && ghatoOrRevAtGate && noEnemyGooAtDest && noFBCultAtDest) |=> 8000 -> "B2c: pain cultist to Ghato's empty gate (claim it)"

                // B3c: Ghato alone at DY gate → send units TO JOIN Ghato for battle
                val ghatoAloneAtDYGate = self.at(r, Ghatanothoa).any && r.enemyGate &&
                    self.at(r).num == 1 &&
                    others./~(_.at(r)).%(_.uclass == DarkYoung).any &&
                    others./~(_.at(r)).%(_.uclass.utype == GOO).none
                (u.cultist && ghatoAloneAtDYGate) |=> 8500 -> "B3c: cultist to Ghato at DY gate (need battle force)"
                (u.uclass == Desiccated && ghatoAloneAtDYGate) |=> 8300 -> "B3c: desc to Ghato at DY gate (battle support)"

                // Reinforce thin own gates via Writhe pain
                val ownGateThin = r.ownGate && self.at(r).num < 3
                (ownGateThin && u.uclass == Desiccated) |=> 6500 -> "reinforce thin gate with desiccated"
                (ownGateThin && u.cultist) |=> 6000 -> "reinforce thin gate with cultist"
                // Generic backstops
                (r.enemyGate) |=> 1500 -> "writhe to enemy gate"
                (r.ownGate && u.uclass == Desiccated) |=> 800 -> "reinforce gate with desiccated"

            case FBWritheMoveOneJoinAction(_, uRef, r, remaining, joinUnit) =>
                r.chaosGate |=> -3000 -> "avoid writhe join to chaos gate"
                // Single-best-score conditional. JOIN should STRONGLY beat separate
                // moves when FB is trying to pair cultist + desiccated (strategy).
                // Without this, FB acolytes scatter to different regions from the
                // desiccated they should be protecting.
                val u = game.unit(uRef)
                // v5.2 (2026-05-13): AP1 cultist join Desc in a NON-GATE region is
                // the HIGHEST priority — it enables the 2nd-gate build. Beats
                // "pair + reinforce own gate" (8500). The previous same-region
                // block was too aggressive; reverted in favor of this preference.
                val ap1CultistToNewRegionDesc = firstAP && u.cultist &&
                    self.at(r, Desiccated).any && !self.gates.has(r) && r.glyph != Ocean
                ap1CultistToNewRegionDesc |=> 9000 -> "AP1: cultist joins Desc in non-gate region (build 2nd gate)"
                // v5.15 (2026-05-14): AP1 SPLIT SCENARIO — first writhe of AP1 only,
                // with 2+ kills AND 4+ pains. Each desc gets its own cultist (one pair
                // per region) so two gates can be built. Prefer JOIN to unpaired Desc.
                //   - first writhe proxy: desiccatedOnMap == killsThisWrithe
                //     (all desc on map came from THIS writhe's kills — no prior desc)
                val killsThisWritheJ = game.fbWritheRolls.count(_ == Kill)
                val painsThisWritheJ = game.fbWritheRolls.count(_ == Pain)
                val splitScenarioJ = firstAP && killsThisWritheJ >= 2 && painsThisWritheJ >= 4 &&
                    desiccatedOnMap == killsThisWritheJ
                val unpairedDescJoin = u.cultist && self.at(r, Desiccated).any &&
                    self.at(r).cultists.none && !fbStartRegion.contains(r) && r.glyph != Ocean
                val pairedRegionJoin = u.cultist && self.at(r, Desiccated).any &&
                    self.at(r).cultists.any && !fbStartRegion.contains(r)
                (splitScenarioJ && unpairedDescJoin) |=> 9500 -> "AP1 split: cultist pairs with unpaired Desc"
                (splitScenarioJ && pairedRegionJoin) |=> 4000 -> "AP1 split: don't concentrate on already-paired region"
                val land = r.glyph != Ocean
                val lonefoe = r.foes.cultists.any && r.foes.monsterly.none && r.foes.goos.none
                // The existing unit at r that we're joining (from joinUnit string in display).
                // Presence of an FB monster/desc at r means we're pairing.
                val fbDescAtDest = self.at(r, Desiccated).any
                val fbRevAtDest = self.at(r, RevenantOfKnaa).any
                val fbGhatoAtDest = self.at(r, Ghatanothoa).any
                val pairingWithFbMonster = u.cultist && (fbDescAtDest || fbRevAtDest || fbGhatoAtDest)
                // Cultist joining Ghato at enemy gate = enables capture+hold (highest)
                val ghatoAtEnemyGate = fbGhatoAtDest && r.enemyGate && r.foes.goos.none
                // Ghato alive: units should join Ghato at gate OR reinforce own gates — NOT scatter
                val postAwakenJoin = gooOnMap > 0
                val joinScore =
                    if (u.cultist && ghatoAtEnemyGate && self.gates.num < 3)         9200  // cultist to Ghato at enemy gate = HOLD after capture
                    else if (pairingWithFbMonster && ghatoAtEnemyGate)               9000  // any unit joining Ghato at enemy gate
                    else if (pairingWithFbMonster && r.ownGate)                      8500  // pair + reinforce own gate (beats empty land)
                    else if (pairingWithFbMonster && land && lonefoe)                 8000  // pair + lone cultist target
                    else if (postAwakenJoin && r.ownGate)                            7500  // any unit to own gate (reduce scatter)
                    else if (pairingWithFbMonster && r.enemyGate)                    7200  // pair + push enemy gate
                    else if (!postAwakenJoin && pairingWithFbMonster && land && r.empty) 7000  // pair to empty land (AP1 only)
                    else if (postAwakenJoin && !firstAP && land && r.empty && !r.gate && !r.enemyGate) -10000  // BLOCK: post-awaken, no empty land
                    else if (postAwakenJoin && land && r.empty)                      3000  // AP1 post-awaken: empty land OK
                    else if (land && r.empty)                                        5500  // solo join to empty land
                    else if (r.enemyGate)                                            1700  // enemy gate fallback
                    else                                                             500   // generic join
                true |=> joinScore -> ("join tier " + joinScore)
                // Gate defense in join: rev/desc to own gate with no CG defender
                // Fires when Ghato already left (placed first in cost-desc order)
                val joinGateNeedsDefender = r.ownGate &&
                    self.at(r).%(_.canControlGate).any &&
                    self.at(r, Ghatanothoa).none && self.at(r, RevenantOfKnaa).none &&
                    self.at(r, Desiccated).none
                (u.uclass == RevenantOfKnaa && joinGateNeedsDefender) |=> 9700 -> "DEFEND: rev join at own gate with no CG defender"
                (u.uclass == Desiccated && joinGateNeedsDefender) |=> 9700 -> "DEFEND: desc join at own gate with no CG defender"
                // Ice Age + Cathedral blocks for join destinations
                val joinIceAge = WW.exists && WW.iceAge.contains(r)
                joinIceAge |=> -12000 -> "BLOCK: Ice Age region (join)"
                val joinCathedral = game.cathedrals.has(r) && AN.exists &&
                    AN.at(r).%(a => a.uclass.utype == Monster || a.uclass.utype == Terror).any
                (u.uclass == Ghatanothoa && joinCathedral) |=> -12000 -> "BLOCK: Ghato join at cathedral with AN combat"
                // v5.13 (2026-05-14): use direct count of cultists already away.
                if (firstAP && fbStartRegion.isDefined) {
                    val cultsAwayJ = self.allInPlay.%(c => c.cultist && !fbStartRegion.contains(c.region)).num
                    val isHomeJoin = fbStartRegion.contains(r)
                    (u.cultist && !isHomeJoin && cultsAwayJ >= 2) |=> -10000 -> "AP1: block join away from home (2 already out)"
                    (isHomeJoin && cultsAwayJ >= 2) |=> 10000 -> "AP1: join at home (2 already out)"
                }
                (firstAP && !fbStartRegion.contains(r) && !land) |=> -8000 -> "AP1: never join in water"
                // AP1: block joining at enemy gates
                (firstAP && r.enemyGate) |=> -10000 -> "AP1: BLOCK join at enemy gate"
                // v5.18 (2026-05-13): penalize joining back at home region (same as MoveOne)
                val joinDestIsHome = fbStartRegion.contains(r)
                val joinAp1HomeOK = firstAP && {
                    val cultsAwayJH = self.allInPlay.%(c => c.cultist && !fbStartRegion.contains(c.region)).num
                    cultsAwayJH >= 2
                }
                (joinDestIsHome && !joinAp1HomeOK) |=> -9000 -> "BLOCK: don't join back at home region (scatter outward)"

            // ──────────────────────────────────────────────────────────────────
            // CAPTURE — desiccated + unprotected cultist = 7000
            // ──────────────────────────────────────────────────────────────────
            // CaptureMainAction is Soft → bot sees CaptureAction after explosion
            case CaptureAction(_, r, f, _) =>
                r.chaosGate |=> -3000 -> "avoid capturing at chaos gate"
                // v5.8 (2026-05-13): YS Passion danger — block Capture into a
                // YS gate with Hastur/KIY counter-attack threat. (Battle is fine
                // — see AttackAction YS Passion boost — but Capture exposes the
                // fb cultist to immediate Passion kill.)
                val ysPassionDanger = f == YS && YS.exists && YS.has(Passion) &&
                    YS.gates.has(r) && YS.at(r).cultists.num > 2 && YS.power > 0 &&
                    (r.near.%(n => YS.at(n, KingInYellow).any).any ||
                     (YS.has(Hastur) && YS.has(HWINTBN)))
                ysPassionDanger |=> -8000 -> "avoid Capture into YS Passion gate"
                // Strategy 1.2: capture at enemy gate with Ghato. MUST beat Writhe (8500).
                val ghatoHere = self.at(r, Ghatanothoa).any
                val enemyCultsHere = f.at(r).cultists.num
                val noEnemyGoo = f.at(r, GOO).none
                val fbCultHere = self.at(r).cultists.any
                // A/B TEST: removed cultists<=2 check
                val ghatoCanSteal = ghatoHere && r.enemyGate && noEnemyGoo && fbCultHere
                (ghatoCanSteal && self.gates.num < 3) |=> 9500 -> "Ghato captures at enemy gate (< 3 gates — BEATS WRITHE)"
                ghatoCanSteal |=> 8500 -> "Ghato captures at enemy gate"
                // Without FB cultist: still capture — removes enemy gate controller.
                // Must beat Summon (8500) and Recruit (8500) since capture is the strategy priority.
                val ghatoNoCult = ghatoHere && r.enemyGate && noEnemyGoo && !fbCultHere
                (ghatoNoCult && self.gates.num < 3) |=> 9000 -> "Ghato captures (no FB cultist — still removes enemy controller)"
                val descCapture = self.at(r, Desiccated).any && f.at(r).cultists.any && f.at(r).monsterly.none && noEnemyGoo
                descCapture |=> 7000 -> "desiccated captures unprotected cultist"
                // Baseline: any capture beats generic battle (1500)
                val anyCapture = (ghatoHere || self.at(r, Desiccated).any || self.at(r, RevenantOfKnaa).any) &&
                    f.at(r).cultists.any && noEnemyGoo
                anyCapture |=> 3000 -> "baseline capture (beats battle)"

            case CaptureTargetAction(_, r, f, ur, _) =>
                val target = game.unit(ur)
                val unprotected = r.foes.monsterly.none && r.foes.goos.none
                // User strategy: capturing the LAST keeper of an enemy gate
                // flips the gate to FB. Highest-priority capture.
                val isLastKeeperOfEnemyGate = r.enemyGate && target.canControlGate &&
                    f.at(r).%(_.canControlGate).num == 1
                (isLastKeeperOfEnemyGate && self.gates.num < 3) |=> 9500 -> "capture last keeper — steal gate (< 3 gates!)"
                isLastKeeperOfEnemyGate |=> 8000 -> "capture last enemy gate-keeper (steals gate)"
                (target.cultist && unprotected && self.at(r, Desiccated).any) |=> 7000 -> "capture unprotected cultist with desiccated"
                (target.cultist && unprotected) |=> 5000 -> "capture unprotected cultist"
                (target.uclass == HighPriest) |=> 1500 -> "capture HP bonus"

            // ──────────────────────────────────────────────────────────────────
            // BUILD GATE — tiered conditions
            // ──────────────────────────────────────────────────────────────────
            case BuildGateAction(_, r) =>
                // Single-best-score conditional.
                val hasFBDesc = self.at(r, Desiccated).any
                val hasFBCult = self.at(r).cultists.any
                val hasFBRev = self.at(r, RevenantOfKnaa).any
                // v5.6 (2026-05-13): when FB has < 3 gates, a Rev in a non-gate
                // region paired with a cultist there, AND power < 5, building
                // here should win over everything else. Scored above writhe
                // (9900) so it's not pre-empted by the writhe-build chain.
                val urgentRevBuild = self.gates.num < 3 && hasFBRev && hasFBCult &&
                    power < 5 && !r.gate && r.glyph != Ocean && !game.fbCraters.has(r)
                urgentRevBuild |=> 9920 -> "Urgent build: Rev + cultist + <5 power + <3 gates"
                val multiFBCult = self.at(r).cultists.num >= 2
                val emptyFoes = r.foes.none
                val protectedEnemy = r.foes.cultists.any && (r.foes.monsterly.any || r.foes.goos.any)
                val noGate = !r.gate
                val land = r.glyph != Ocean
                val present = self.at(r).any
                val gateCount = self.gates.num
                // Never build in crater region — DM craters destroy any gate built there.
                val hasCrater = game.fbCraters.has(r)
                val noCrater = !hasCrater
                // No vulnerable enemy gates: all enemy gates have GOO
                val noVulnerableEnemyGates = !others.exists(f => f.gates.exists(gr =>
                    f.at(gr).%(_.uclass.utype == GOO).none))
                // Both non-Ghato FB gates have revs
                val fbGatesWithRevs = self.gates.%(gr =>
                    self.at(gr, RevenantOfKnaa).any || self.at(gr, Ghatanothoa).any).num
                val allGatesDefended = gateCount >= 2 && fbGatesWithRevs >= gateCount
                // T04: suppress build when FB has 3+ gates + undefended gate + rev in pool
                val hasUndefendedGate = self.gates.exists(gr =>
                    self.at(gr, Ghatanothoa).none && self.at(gr, RevenantOfKnaa).none)
                val revAvailable = self.pool(RevenantOfKnaa).any
                val shouldDefendNotBuild = gateCount >= 3 && hasUndefendedGate && revAvailable
                val buildScore =
                    if (!noGate || !present || !land || !noCrater)                               0
                    else if (shouldDefendNotBuild)                                               -5000
                    else if (gateCount == 0)                                                     9000
                    else if (gateCount == 2 && secondAP && hasFBDesc && hasFBCult && (emptyFoes || protectedEnemy)) 9900
                    // Build 3rd gate when no vulnerable enemy gates + defended gates (below capture chain 9500)
                    else if (gateCount == 2 && noVulnerableEnemyGates && allGatesDefended && hasFBCult && (emptyFoes || protectedEnemy)) 9100
                    else if (gateCount < 3 && hasFBDesc && hasFBCult && (emptyFoes || protectedEnemy)) 8800
                    else if (gateCount == 1)                                                     8200
                    else if (hasFBCult && multiFBCult && emptyFoes)                              7500
                    else if (gateCount == 2)                                                     7500
                    else if (hasFBCult && emptyFoes)                                             7000
                    else                                                                         3000
                true |=> buildScore -> ("build gate tier " + buildScore)
                // Ghato on non-gate, no vulnerable gates, cultist with Ghato, < 3 gates, power > 2
                val ghatoHereBuild = self.at(r, Ghatanothoa).any && ghatoOnNonGate &&
                    noVulnerableGatesAnywhere && ghatoHasCultist && gateCount < 3 && power > 2 &&
                    noGate && land && noCrater
                ghatoHereBuild |=> 9500 -> "BUILD: Ghato on non-gate, no vuln gates, build here"

            case BuildGateMainAction(_, l) =>
                val any = l.exists { r =>
                    val hasFBDesc = self.at(r, Desiccated).any
                    val hasFBCult = self.at(r).cultists.any
                    val emptyFoes = r.foes.none
                    val protectedEnemy = r.foes.cultists.any && (r.foes.monsterly.any || r.foes.goos.any)
                    val noGate = !r.gate
                    val noCrater = !game.fbCraters.has(r)
                    noGate && noCrater && hasFBCult && (emptyFoes || protectedEnemy || hasFBDesc)
                }
                any |=> 8500 -> "build gate menu: at least one viable region"
                (self.gates.num < 2 && any) |=> 9000 -> "build gate menu critical (< 2 gates)"

            // ──────────────────────────────────────────────────────────────────
            // AN GIVE MONSTER — place free monster using same logic as summon
            // Score region like a summon: prefer own gates, undefended gates,
            // gates with fewest units.
            // ──────────────────────────────────────────────────────────────────
            case GiveBestMonsterAskAction(_, _, uc, r, _) =>
                val ownGateGive = self.gates.has(r)
                val undefendedGive = ownGateGive && self.at(r, Ghatanothoa).none && self.at(r, RevenantOfKnaa).none
                val giveScore =
                    if (ownGateGive && undefendedGive) 8000
                    else if (ownGateGive) (5000 - self.at(r).num * 300).max(2000)
                    else if (r.freeGate && self.at(r).any) 6000
                    else 500
                true |=> giveScore -> ("give monster region " + giveScore)

            case GiveWorstMonsterAskAction(_, _, uc, r, _) =>
                val ownGateGiveW = self.gates.has(r)
                val undefendedGiveW = ownGateGiveW && self.at(r, Ghatanothoa).none && self.at(r, RevenantOfKnaa).none
                val giveScoreW =
                    if (ownGateGiveW && undefendedGiveW) 8000
                    else if (ownGateGiveW) (5000 - self.at(r).num * 300).max(2000)
                    else if (r.freeGate && self.at(r).any) 6000
                    else 500
                true |=> giveScoreW -> ("give monster region " + giveScoreW)

            // ──────────────────────────────────────────────────────────────────
            // SPELLBOOK CHOICE — when FB earns a spellbook
            // ──────────────────────────────────────────────────────────────────
            case SpellbookAction(_, sb, _) =>
                val ghatoOrRevs = gooOnMap > 0 || revenantOnMap > 0
                // Ghato is "imminent" when in pool — bot will awaken soon. CG
                // should be prioritized THEN, not just after Ghato is on map.
                // Baselines pick CG before awakening because they know it's
                // coming. Without this, bot picks TheEyeOpens (4000) over CG
                // (3000) when Ghato in pool and ends up with the wrong SB
                // order at AP3+.
                val ghatoImminent = ghatoOrRevs || self.pool(Ghatanothoa).any
                // v5 (2026-05-13): SB pick order — CG → DM → Augury → Carnage → CoF → Eye.
                // Per user spec: keep CG / DM as is, then Augury 3rd, then Carnage 4th.
                (sb == CyclopeanGaze && ghatoImminent) |=> 7000 -> "pick CG FIRST (Ghato on map or in pool)"
                (sb == CyclopeanGaze && !ghatoImminent) |=> 3000 -> "pick CG (Ghato eliminated entirely)"
                (sb == DevilsMark) |=> 6000 -> "pick Devil's Mark second"
                (sb == Augury) |=> 5000 -> "pick Augury third"
                (sb == Carnage) |=> 4500 -> "pick Carnage fourth"
                (sb == CallOfTheFaithful && ghatoOrRevs) |=> 4000 -> "pick CoF with Ghato/Revs on map"
                (sb == CallOfTheFaithful && !ghatoOrRevs) |=> 3000 -> "pick CoF without Ghato/Revs"
                (sb == TheEyeOpens) |=> 3500 -> "pick Eye Opens"

            // ──────────────────────────────────────────────────────────────────
            // PLAYER ORDER — first player picking
            // FB picks itself if available; otherwise tiebreaker rules.
            // ──────────────────────────────────────────────────────────────────
            case FirstPlayerAction(_, f) =>
                (f == FB) |=> 99000 -> "FB picks FB as first player"
                (f != FB) |=> tiebreakScore(f) -> ("tiebreaker score for " + f)

            // ──────────────────────────────────────────────────────────────────
            // DOOM PHASE — Ritual
            // ──────────────────────────────────────────────────────────────────
            case RitualAction(_, cost, _) =>
                // FB baseline (kept): wait for Ghato awaken; ritual at 3+ gates.
                val powerAfter = power - cost
                val gatesNum = self.gates.num
                val shouldWaitForAwaken = gooOnMap == 0 && self.pool(Ghatanothoa).any && power >= 6
                val ritualScore =
                    if (powerAfter < 0)                       -1000  // can't afford
                    else if (shouldWaitForAwaken)              -2000  // wait, awaken first
                    else if (gatesNum >= 3)                    9500  // 3 gates — strategy says ritual here
                    else                                       -2000 // < 3 gates — do NOT ritual
                true |=> ritualScore -> ("ritual tier " + ritualScore)
                // v5.7 (2026-05-13): end-game ritual overrides — copied from BotGC.
                instantDeathNow |=> 10000 -> "instant death now"
                instantDeathNext && allSB && others.all(!_.allSB) |=> 10000 -> "ritual if ID next and all SB"
                instantDeathNext && !allSB && others.%(_.allSB).any |=> -1000 -> "dont ritual if ID next and not all SB"
                instantDeathNext && !allSB && others.all(!_.allSB) && realDoom < others./(_.aprxDoom).max |=> 900 -> "ritual so ID next and nobody wins"
                allSB && realDoom + maxDoomGain >= 30 |=> 900 -> "can break 30, and all SB"
                !allSB && self.doom + self.gates.num >= 30 |=> -5000 -> "will break 30, but not all SB"
                !allSB && self.doom + self.gates.num < 30 && realDoom <= 29 && realDoom + maxDoomGain >= 29 |=> 700 -> "won't break 30, but come near"
                numSB >= 5 && cost * 2 <= power && self.gates.num >= 3 |=> 800 -> "5 SB and less than half available power"
                numSB >= 2 && aprxDoomGain / cost > 1 |=> 600 -> "very sweet deal"
                numSB >= 3 && aprxDoomGain / cost > 0.75 |=> 400 -> "sweet deal"
                numSB >= 4 && aprxDoomGain / cost > 0.5 |=> 200 -> "ok deal"
                cost == 5 |=> 100 -> "ritual first"
                self.pool.goos.any |=> -200 -> "not all goos in play"
                others.%(ofinale).any |=> 1500 -> "enemy in ofinale — ritual to deny"

            // ──────────────────────────────────────────────────────────────────
            // DOOM PHASE — Devil's Mark
            // ──────────────────────────────────────────────────────────────────
            case FBDevilsMarkDoomAction(_) =>
                // v5.14 (2026-05-14): gates threshold lowered to >= 2 (was >= 3);
                // "must ritual first" requirement removed. Ghato-awakened gate kept.
                val gatesNum = self.gates.num
                val hasAwakened = gooOnMap > 0 || game.fbGhatnothoaAwakenings >= 1
                val dmScore =
                    if (!hasAwakened)                              -15000 // BLOCK: awaken Ghato first
                    else if (gatesNum >= 2)                       6500   // 2+ gates: DM
                    else                                          -15000 // HARD block: never DM below 2 gates
                true |=> dmScore -> ("DM doom tier " + dmScore)

            // ──────────────────────────────────────────────────────────────────
            // DEVIL'S MARK — crater placement
            // ──────────────────────────────────────────────────────────────────
            case FBDevilsMarkPlaceCraterAction(_, r) =>
                // Single-best-score conditional — each candidate region gets one tier.
                val isFBControlled = self.gates.has(r)
                // WW has two potential start areas (Antarctica + Arctic Ocean), both have WW glyph.
                // Antarctica counts as a glyph region even if WW didn't start there.
                val wwGlyphRegion = WW.exists && (r.name == "Antarctica" || r.name == "ArcticOcean")
                val inEnemyGlyph = others.exists(f => game.starting.get(f).contains(r)) || wwGlyphRegion
                val inFBGlyph = fbStartGlyphRegion.contains(r)
                val regionGlyphOwner = game.factions.exists(f => game.starting.get(f).contains(r)) || wwGlyphRegion
                val hasFBCult = self.at(r).cultists.any
                val unprotectedFBCult = hasFBCult && self.at(r).monsterly.none && self.at(r).goos.none
                val protectedFBCult = hasFBCult && (self.at(r).monsterly.any || self.at(r).goos.any)
                // Slight preference for gate with Ghato (tiebreaker — Ghato gate wins ties)
                val hasGhatoHere = self.at(r, Ghatanothoa).any
                hasGhatoHere |=> 100 -> "DM: slight Ghato gate preference"
                // Strategy guide: if NoAcolytesInStart needed, DM the starting gate
                val needsNoAcolytes = self.needs(FBNoAcolytesInStart) && fbStartRegion.contains(r) &&
                    self.at(r).cultists.any
                // v5 (2026-05-13): simplified — glyph regions score higher than non-glyph,
                // no conditionals on crater count. Per user spec: "just score glyph regions
                // higher than other regions, with no conditionals".
                val craterScore =
                    if (!isFBControlled)                                         0
                    // Instant death imminent: home region is highest priority for DM
                    else if (idImminentForDM && inFBGlyph)                       9000
                    else if (inEnemyGlyph)                                       7000
                    else if (inFBGlyph)                                          6000
                    else if (!regionGlyphOwner && unprotectedFBCult)             5000
                    else if (!regionGlyphOwner && protectedFBCult)               4000
                    else                                                         500
                true |=> craterScore -> ("crater tier " + craterScore)

            case FBDevilsMarkDoomCancelAction(_) =>
                // Cancel the DM region picker — only preferred if no region
                // scores well (all candidates score 500 → cancel at 600).
                true |=> 600 -> "cancel DM region picker"

            // ──────────────────────────────────────────────────────────────────
            // DOOM DONE
            // ──────────────────────────────────────────────────────────────────
            case DoomDoneAction(_) =>
                true |=> 3000 -> "doom phase done"

            // ──────────────────────────────────────────────────────────────────
            // AWAKEN GHATANOTHOA — take whenever affordable, starting AP2
            // ──────────────────────────────────────────────────────────────────
            case FBAwakenGhatanothoaAction(_, cost) =>
                // Single-best-score conditional. Awaken must BEAT ritual in AP2 so
                // FB awakens before spending power on rituals (each awakened Ghato
                // adds 1 ES per future ritual). Awaken > Ritual > anything else.
                val powerAfter = power - cost
                val awakenScore =
                    if (firstAP)                                        -3000  // never in AP1
                    else if (gooOnMap > 0 || powerAfter < 0)            -1000  // already awake or unaffordable
                    else if (game.fbGhatnothoaAwakenings >= 3)          -2000  // max 3 awakens total
                    else if (secondAP)                                   9800  // AP2 BEATS ritual (9000-9500)
                    else                                                 9300  // later APs still beat ritual
                true |=> awakenScore -> ("awaken tier " + awakenScore)

            // ──────────────────────────────────────────────────────────────────
            // SUMMON / RECRUIT — AP2+ priorities
            // ──────────────────────────────────────────────────────────────────
            case SummonAction(_, uc, r) =>
                // Single-best-score conditional. Tiered by unit class and context.
                val isRev = uc == RevenantOfKnaa
                val isDesc = uc == Desiccated
                val isAco = uc == Acolyte
                val ownGate = self.gates.has(r)
                val undefendedOwnGate = ownGate && self.at(r, Ghatanothoa).none && self.at(r, RevenantOfKnaa).none
                // Gate at capture risk: adjacent region has enemy monster/goo,
                // and our gate has no Rev/Ghato defender. Rev summon protects.
                val adjacentEnemyMonster = r.near.%(nr => nr.foes.monsterly.any || nr.foes.goos.any).any
                val gateUnderCaptureThreat = ownGate && undefendedOwnGate && adjacentEnemyMonster
                val twoGatesPartialCover = self.gates.num == 2 && undefendedOwnGate &&
                    (self.all(RevenantOfKnaa).num >= 1 || self.all(Ghatanothoa).num >= 1)
                val postDMRecovery = self.gates.num < 3 && craterCount >= 1 && undefendedOwnGate
                // Strategy 1.4-1.6: Summon rules conditioned on power level.
                // Rev: only if CG active AND gate unprotected by ghato/rev, AND power 3-4
                // Desc: 2 power, > 4 cultists on map, < 5 desiccated on map
                // Aco: 2 power and < 5 cultists on map, OR 1 power and < 6 cultists
                val hasCG = self.has(CyclopeanGaze) && !self.oncePerGame.has(CyclopeanGaze)
                val cultistsOnMap = self.all(Acolyte).num + self.all(HighPriest).num
                // v5.13 (2026-05-14): AP1 Rev priority — if FB has >4 cultists on map
                // and >1 controlled gates, summon Rev. Prefer gate with no monster
                // (Ghato/Rev/Desc); if all gates have monsters, prefer gate with
                // fewest units. This fires INSTEAD of the AP1 block on Rev.
                val rHasMonster = ownGate &&
                    (self.at(r, Ghatanothoa).any || self.at(r, RevenantOfKnaa).any || self.at(r, Desiccated).any)
                val anyGateMonsterless = self.gates.exists(g =>
                    self.at(g, Ghatanothoa).none && self.at(g, RevenantOfKnaa).none && self.at(g, Desiccated).none)
                val ap1RevPriority = firstAP && isRev && ownGate &&
                    cultistsOnMap > 4 && self.gates.num > 1
                val gateUnitsHere = self.at(r).num
                val revCritical = isRev && !firstAP && gateUnderCaptureThreat
                val revOK = isRev && !firstAP && undefendedOwnGate
                val revBlocked = isRev && !ap1RevPriority && (firstAP || (!revCritical && !revOK))
                // T04: at power 3-4, summon rev to undefended gate (highest priority at this power level)
                val t04RevAtLowPower = isRev && power >= 3 && power <= 4 && undefendedOwnGate && !firstAP
                // RECOVERY: < 2 gates, Ghato awakened — summon rev to Ghato's gate first
                val recoveryMode = !firstAP && self.gates.num < 2 && gooOnMap > 0
                val ghatoGate = recoveryMode && ownGate && self.at(r, Ghatanothoa).any
                val summonScore =
                    if (isRev && recoveryMode && ghatoGate && self.at(r, RevenantOfKnaa).none) 9000  // RECOVERY: rev to Ghato's gate
                    // v5.13: AP1 Rev priority — gate with no monster preferred
                    else if (ap1RevPriority && !rHasMonster) 9500
                    // v5.13: AP1 Rev priority — all gates have monsters; pick fewest-units
                    else if (ap1RevPriority && rHasMonster && !anyGateMonsterless)
                        (9400 - gateUnitsHere * 100)
                    // T04: Rev at undefended gate beats writhe-capture (9300)
                    else if (t04RevAtLowPower && undefendedOwnGate)                   9400  // T04: Rev at 3-4 power to undefended gate
                    else if (revCritical)                                             9400  // CRITICAL defend vs capture threat
                    else if (revBlocked)                                             -2000 // blocked: no CG or gate protected
                    else if (revOK && self.gates.num >= 2)                           9400  // Rev at undefended gate (beats writhe-capture 9300)
                    else if (revOK)                                                  6500  // Rev at undefended gate (1 gate)
                    else if (isRev && ownGate && !self.at(r, Ghatanothoa).any &&
                        !self.at(r, RevenantOfKnaa).any)                             4000  // Rev fallback: gate without ghato/rev
                    else if (isRev && ownGate)                                       3500  // Rev fallback: any own gate
                    else if (isRev)                                                  -2000 // Rev not at own gate
                    else if (isDesc && firstAP && self.gates.num >= 2 && ownGate &&
                        self.at(r, Desiccated).none)                                     7500  // AP1 2g: desc to gate with no desc (hold gates)
                    else if (isDesc && firstAP)                                       -2000 // discourage Desc in AP1 (not hard block)
                    else if (isDesc && power == 2 && cultistsOnMap > 4 && desiccatedOnMap < 5) 6000 // strategy 1.5: desc at 2P, >4 cult, <5 desc
                    else if (isDesc && ownGate && !self.at(r, Ghatanothoa).any &&
                        !self.at(r, RevenantOfKnaa).any && self.at(r, Desiccated).none) 3800  // Desc fallback: gate without defenders
                    else if (isDesc && ownGate)                                      3300  // Desc fallback: any own gate
                    else if (isDesc)                                                 1500  // Desiccated baseline (not at gate)
                    else if (isAco && power == 2 && cultistsOnMap < 5)               5500  // strategy 1.5: cultist at 2P, <5 on map
                    else if (isAco && power == 1 && cultistsOnMap < 6)               5000  // strategy 1.6: cultist at 1P, <6 on map
                    else if (isAco)                                                  1000  // Acolyte baseline
                    else                                                             0
                true |=> summonScore -> ("summon tier " + summonScore)
                // Rev summon priority: gate without CG defender after writhe-capture
                val gateNeedsCGDef = isRev && ownGate && self.at(r, Ghatanothoa).none &&
                    self.at(r, RevenantOfKnaa).none && gooOnMap > 0 && self.gates.num >= 2
                gateNeedsCGDef |=> 9800 -> "PRIORITY: Rev to undefended gate (post writhe-capture)"
                if (ownGate) {
                    val unitsHere = self.at(r).num
                    val fewestBoost = (6 - unitsHere).max(0) * 200
                    fewestBoost > 0 |=> fewestBoost -> ("summon to gate with fewer units +" + fewestBoost)
                }

            case RecruitAction(_, uc, r) =>
                // Single-best-score conditional. Tiered by unit class + context.
                val ownGate = self.gates.has(r)
                val needsKeeper = ownGate && self.at(r).%(_.canControlGate).num <= 1
                val isAco = uc == Acolyte
                val isHP = uc == HighPriest
                val isRev = uc == RevenantOfKnaa
                val noHPatGate = ownGate && self.at(r, HighPriest).none
                // Only valid recruit block: enemy GOO IN the region with no Ghato defending
                val enemyGooInRegion = r.foes.goos.any
                val ghatoHereDefending = self.at(r, Ghatanothoa).any
                val gooThreat = (isAco || isHP) && enemyGooInRegion && !ghatoHereDefending
                gooThreat |=> -15000 -> "BLOCK: recruit into region with enemy GOO (no Ghato)"
                // Gate defense: maintain 2+ keepers at each gate. When a gate has
                // only 1 keeper, recruit scores CRITICAL even outside capture-threat.
                val thinKeeper = ownGate && self.at(r).%(_.canControlGate).num == 1
                val ownGateWithGhato = ownGate && self.at(r, Ghatanothoa).any
                // B2b: FB monster on empty gate that FB doesn't control → recruit cultist HERE to claim it
                val fbMonsterOnGate = self.at(r, Ghatanothoa).any || self.at(r, RevenantOfKnaa).any || self.at(r, Desiccated).any
                // Gate must be uncontrolled (free/abandoned) — not enemy-controlled
                val unclaimedGateHere = game.gates.has(r) && !self.gates.has(r) &&
                    !others.exists(_.gates.has(r))
                val noEnemyUnitsHere = others./~(_.at(r)).none
                val claimRecruit = isAco && fbMonsterOnGate && unclaimedGateHere &&
                    (noEnemyUnitsHere || others./~(_.at(r)).%(_.uclass.utype == GOO).none)
                claimRecruit |=> 9000 -> "B2b: recruit cultist at FB monster's empty gate to CLAIM it"
                // v5.8 (2026-05-13): Eye-Opens follow-up — a single FB Desc sits
                // on a newly-empty gate (post-Eye-Opens kill); recruit a cultist
                // there so gate-build fires next AP.
                val eyeOpensFollowup = isAco && self.gates.num >= 2 &&
                    self.at(r, Desiccated).num >= 1 && self.at(r).cultists.none &&
                    r.foes.none && unclaimedGateHere
                eyeOpensFollowup |=> 9100 -> "Eye-Opens follow-up: recruit cult to alone-desc empty gate"
                // T04: suppress recruit when rev summon is more urgent
                val anyUndefendedOwnGate = self.gates.exists(g2 =>
                    self.at(g2, Ghatanothoa).none && self.at(g2, RevenantOfKnaa).none)
                val revInPool = self.pool(RevenantOfKnaa).any
                // (T04 recruit suppression removed — was hurting win rate)
                // T05/T06: recruit to gates with fewest cultists first
                val cultsAtThisGate = if (ownGate) self.at(r).%(_.canControlGate).num else 99
                // RECOVERY: < 2 gates, Ghato awakened, power < 4 — recruit at Ghato's gate only
                val recoveryRecruit = !firstAP && self.gates.num < 2 && gooOnMap > 0 && power < 4
                val cultistsInPool = self.pool.cultists.num
                val ghatoAtThisGate = ownGate && self.at(r, Ghatanothoa).any
                val recruitScore =
                    if (isAco && recoveryRecruit && ghatoAtThisGate && cultistsInPool > 2) 7000  // RECOVERY: recruit at Ghato's gate (pool > 2)
                    else if (isAco && recoveryRecruit && ghatoAtThisGate && cultistsInPool <= 2) 6000  // RECOVERY: recruit then summon desc
                    else if (isAco && recoveryRecruit && !ghatoAtThisGate)        -5000  // RECOVERY: only recruit at Ghato's gate
                    else if (isAco && needsKeeper)                                    8500  // gate has 0 keepers — highest priority
                    else if (isHP && noHPatGate)                                 6500  // HP defender at gate
                    else if (isRev && desiccatedOnMap >= 2)                      4000  // Rev with backing
                    else if (isAco && ownGate)                                   (3600 - self.at(r, Acolyte).num * 300).max(1500)  // spread: fewer cultists = higher score
                    // v5.1 (2026-05-13): allow recruit at non-gate region where FB has
                    // a Desiccated (the FB build-out target). A cultist there pairs
                    // with the Desc, enabling the next-turn gate build. Score scales
                    // up when gates are scarce; blocked if enemy GOO present.
                    else if (isAco && !ownGate && self.at(r, Desiccated).any &&
                             r.foes.goos.none && self.gates.num < 3 &&
                             r.glyph != Ocean)                                    7500  // build-out: recruit cultist with Desc to enable new gate
                    // v5.6 (2026-05-13): same build-out, but with a Rev instead of
                    // a Desc. Rev + cultist + power for build at <5 power and <3
                    // gates is high priority — sets up next-turn gate build.
                    else if (isAco && !ownGate && self.at(r, RevenantOfKnaa).any &&
                             self.at(r).cultists.none && self.pool(Acolyte).any &&
                             r.foes.goos.none && self.gates.num < 3 &&
                             power < 5 && r.glyph != Ocean)                       9100  // build-out: recruit cultist with Rev to enable next-turn gate build
                    else if (isAco && self.gates.any && !ownGate && !claimRecruit)  -5000  // BLOCK: don't recruit at non-controlled region when gates exist
                    else if (isAco && !self.gates.any && (self.at(r, Ghatanothoa).any || self.at(r, RevenantOfKnaa).any)) 3000  // 0 gates: recruit at Ghato/Rev region
                    else if (isAco && !self.gates.any && game.gates.has(r))       2500  // 0 gates: recruit at any gate to claim
                    else if (isAco && !self.gates.any)                           -3000  // 0 gates: don't recruit at random regions
                    else if (isRev)                                              2500  // baseline Rev
                    else if (isHP)                                               1500  // baseline HP
                    else                                                         0
                true |=> recruitScore -> ("recruit tier " + recruitScore)
                // Ghato on non-gate, no vuln gates, no cultist, < 3 gates, power > 3: recruit here
                val ghatoHereRecruit = isAco && self.at(r, Ghatanothoa).any && ghatoOnNonGate &&
                    noVulnerableGatesAnywhere && !ghatoHasCultist && self.gates.num < 3 && power > 3
                ghatoHereRecruit |=> 9500 -> "RECRUIT: Ghato on non-gate, no vuln gates, recruit cultist to build"
                if (ownGate) {
                    val unitsHere = self.at(r).num
                    val fewestBoost = (6 - unitsHere).max(0) * 200
                    fewestBoost > 0 |=> fewestBoost -> ("recruit to gate with fewer units +" + fewestBoost)
                }

            // ──────────────────────────────────────────────────────────────────
            // INFERNAL PACT (v4: main-phase only)
            // ──────────────────────────────────────────────────────────────────
            // v4 (2026-05-12): Doom-phase IP was removed at the game-engine level
            // (FactionFB.scala). The pre-v4 scoring relied on doom-phase IP for
            // most of FB's discount fuel; the audit of fb-v2-battle-heavy showed
            // 20 main-phase IP uses (Writhe-cycle discount) in the v3 game vs 0
            // in a v4-bot sim. The old scoring's gates>2 + ritualCost>6 gate is
            // too tight for v4: IP must now fire on Writhe and combat fuel too.
            // See fb-comparison.rtf in the Firstborn folder for the audit.
            case FBInfernalPactMainAction(_) =>
                val gatesNumIPM = self.gates.num
                val projectedDoomIPM = self.doom + ((self.es.num + 2) * 5 / 3) + gatesNumIPM
                // v5.5 (2026-05-13): all ritual-related IP triggers REMOVED per user spec.
                // ritualDiscountIPM and killCondIPM (which referenced ritualCost > 6) gone.
                val hasGateCultistsIPM = self.gates.exists(r => self.at(r, Acolyte).any)
                // v5.9 (2026-05-13): power < 6 gate removed — was blocking IP too often.
                val writheDiscountIPM = hasGateCultistsIPM &&
                    infernalDiscount < 2 && game.turn >= 2
                val combatFuelIPM = areas.exists(r => self.at(r, Acolyte).any && r.foes.any) &&
                    infernalDiscount < 2 && game.turn >= 2
                val needsTwoFDIPM = self.needs(FBTwoFacedownSpellbooks)
                val readyTwoFDIPM = needsTwoFDIPM && self.gates.num >= 2
                val highDoomModeIPM = projectedDoomIPM >= 15 && infernalDiscount < 2
                // v5.12 (2026-05-14): Fling-combo IP — when FB is at 1-2 power with
                // Ghato up, projDoom >= 12, < 2 revs, and an enemy GOO is sitting
                // at a gate (= writhe-combat target), flip all non-CG SBs to fund
                // a Rev summon this AP. Plan: next AP, writhe + combat into the
                // enemy GOO.
                val enemyGooAtGateIPM = others.exists(f => f.allInPlay.exists(u =>
                    u.uclass.utype == GOO && u.region.onMap &&
                    (f.gates.has(u.region) || u.region.gate)))
                val flingComboIPM = (power == 1 || power == 2) &&
                    self.onMap(Ghatanothoa).any &&
                    projDoom(self) >= 12 &&
                    self.onMap(RevenantOfKnaa).num < 2 &&
                    self.pool(RevenantOfKnaa).any &&
                    enemyGooAtGateIPM &&
                    infernalDiscount < 5
                // v5.18 (2026-05-14): Tough-battle prep. Fires when Ghato is in a
                // region where enemy combat > 6× FB units OR YS+Hastur is on
                // the map (likely writhe-combat target). Flip every non-CG /
                // non-Carnage SB for a big-discount battle.
                val ysHasturOnMapIPM = YS.exists && YS.onMap(Hastur).any
                val toughBattleIPM = self.onMap(Ghatanothoa).any && (
                    self.onMap(Ghatanothoa).exists { gh =>
                        val gr = gh.region
                        val foeStr = foeCombatAt(gr)
                        val fbUnits = self.at(gr).num
                        fbUnits >= 1 && others.exists(_.at(gr).any) && foeStr > 6 * fbUnits
                    } || ysHasturOnMapIPM
                )
                val wantIP = writheDiscountIPM || combatFuelIPM || readyTwoFDIPM || highDoomModeIPM || flingComboIPM || toughBattleIPM
                // v5.5 (2026-05-13): base entry score bumped to 11000 per user spec.
                wantIP  |=> 11000 -> "enter IP: discount fuel needed"
                !wantIP |=> -10000 -> "BLOCK IPMain: no flip warranted"
                // v5.12: fling-combo gets its own diagnostic line.
                flingComboIPM |=> 11100 -> "FLING COMBO: flip up to 5 SBs (not CG) to fund Rev summon"
                // v5.18: tough-battle prep slightly above base — so this trigger
                // wins when both fire (battle is more urgent than other IP needs).
                toughBattleIPM |=> 11500 -> "TOUGH BATTLE PREP: enemy combat > 6× FB units, flip max SBs"

            case FBInfernalPactChooseAction(_, sb) =>
                // v5.13 (2026-05-14): FBInfernalPactMainAction is Soft and gets
                // exploded — its 11000 score never applies. Choose actions appear
                // directly in main menu and must beat Recruit/Summon (9000+) when
                // IP is wanted. Apply a wantIP boost to make Choose competitive.
                val ghatoResurrectIPC = math.max(1, 11 - game.ritualCost)
                val nextActionBattleIPC = areas.exists { r =>
                    val ghatoThere = self.at(r, Ghatanothoa).any
                    val enemyGate  = others.exists(_.gates.has(r))
                    val enemyGoo   = r.foes.goos.any
                    val targetF    = others.find(_.gates.has(r)).getOrElse(others.headOption.getOrElse(self))
                    val fbAtk      = self.strength(self.at(r), targetF)
                    val foeDef     = foeCombatAt(r)
                    val ghatoSurv  = fbAtk >= foeDef
                    val canResurr  = power >= ghatoResurrectIPC + 3
                    ghatoThere && enemyGate && enemyGoo && (ghatoSurv || canResurr)
                }
                val canResurrectAfterBattleIPC = power >= ghatoResurrectIPC
                val cgFlipForBattle = sb == CyclopeanGaze && nextActionBattleIPC && canResurrectAfterBattleIPC
                cgFlipForBattle |=> 500 -> "flip CG: battle pending + can resurrect after"
                (sb == CyclopeanGaze && !cgFlipForBattle) |=> -200 -> "avoid flipping CG (no battle warrant)"
                // v5.12 (2026-05-14): fling-combo allows flipping every non-CG SB
                // including DM. Score 2000 to beat normal flips when conditions hold.
                val enemyGooAtGateC = others.exists(f => f.allInPlay.exists(u =>
                    u.uclass.utype == GOO && u.region.onMap &&
                    (f.gates.has(u.region) || u.region.gate)))
                val flingComboC = (power == 1 || power == 2) &&
                    self.onMap(Ghatanothoa).any &&
                    projDoom(self) >= 12 &&
                    self.onMap(RevenantOfKnaa).num < 2 &&
                    self.pool(RevenantOfKnaa).any &&
                    enemyGooAtGateC &&
                    (power + infernalDiscount) < 3
                (flingComboC && sb == DevilsMark) |=> 2000 -> "FLING COMBO: flip DM (fund Rev summon)"
                (flingComboC && sb != CyclopeanGaze && sb != DevilsMark) |=> 2200 -> "FLING COMBO: flip non-CG/DM SB"
                // v5.18 (2026-05-14): TOUGH BATTLE PREP — Ghato in a region where
                // enemy combat > 6× FB units. Flip DM and all other non-CG/non-Carnage
                // SBs to fund the battle. CG stays faceup (fires DURING the battle).
                // Carnage stays faceup (saved as payment when Carnage triggers post-Ghato-death).
                val ysHasturOnMapC = YS.exists && YS.onMap(Hastur).any
                val toughBattleC = self.onMap(Ghatanothoa).any && (
                    self.onMap(Ghatanothoa).exists { gh =>
                        val gr = gh.region
                        val foeStr = foeCombatAt(gr)
                        val fbUnits = self.at(gr).num
                        fbUnits >= 1 && others.exists(_.at(gr).any) && foeStr > 6 * fbUnits
                    } || ysHasturOnMapC
                )
                (toughBattleC && sb == DevilsMark) |=> 2300 -> "TOUGH BATTLE: flip DM (fund battle)"
                (toughBattleC && sb != CyclopeanGaze && sb != DevilsMark && sb != Carnage) |=> 2500 -> "TOUGH BATTLE: flip non-CG/DM/Carnage SB"
                (toughBattleC && sb == Carnage) |=> 600 -> "TOUGH BATTLE: flip Carnage LAST (save for Carnage-ES payment)"
                (sb == DevilsMark && !flingComboC && !toughBattleC) |=> -20000 -> "NEVER flip DM for IP"
                // v5.17 (2026-05-14): Augury-with-kills now scores LOWER than other
                // flips, but still positive enough to fire after preferred SBs are
                // already facedown. User wants it as a last-resort flip option.
                val gatesNumChoose = self.gates.num
                val projectedDoomChoose = self.doom + ((self.es.num + 2) * 5 / 3) + gatesNumChoose
                val hasGateCultistsC = self.gates.exists(r => self.at(r, Acolyte).any)
                // v5.9 (2026-05-13): power < 6 gate removed.
                val writheDiscountC = hasGateCultistsC &&
                    infernalDiscount < 2 && game.turn >= 2
                val combatFuelC = areas.exists(r => self.at(r, Acolyte).any && r.foes.any) &&
                    infernalDiscount < 2 && game.turn >= 2
                val needsTwoFDC = self.needs(FBTwoFacedownSpellbooks)
                val readyForTwoFDC = needsTwoFDC && self.gates.num >= 2
                val highDoomModeC = projectedDoomChoose >= 15 && infernalDiscount < 2
                val shouldFlip = writheDiscountC || combatFuelC || readyForTwoFDC || highDoomModeC
                // v5.13 (2026-05-14): wantIP boost — Choose actions compete in main
                // menu (because IP main is Soft-exploded). Boost makes Choose beat
                // Recruit/Summon (9000+) when IP is desired.
                val wantIPC = shouldFlip
                val ipBoost = if (wantIPC) 11000 else 0
                // Non-FB items first (cheaper to spend) — keep this preference order.
                (shouldFlip && sb.isInstanceOf[NeutralSpellbook]) |=> (1800 + ipBoost) -> "flip neutral/iGOO SB (prefer non-FB)"
                // v5.5 (2026-05-13): Augury 1200, EyeOpens 1500 (swap from prior).
                (shouldFlip && sb == TheEyeOpens) |=> (1500 + ipBoost) -> "flip Eye Opens (discount available)"
                // v5.9 (2026-05-13): swap CoF/Augury/Carnage tiers — CoF up, others down.
                (shouldFlip && sb == CallOfTheFaithful) |=> (1500 + ipBoost) -> "flip CoF (discount available)"
                (shouldFlip && sb == Augury && auguryKills == 0) |=> (1000 + ipBoost) -> "flip Augury (discount available)"
                // v5.17 (2026-05-14): Augury-with-kills allowed but at a lower tier
                // (400 + ipBoost) so it fires only after other flippable SBs are
                // already facedown.
                (shouldFlip && sb == Augury && auguryKills > 0) |=> (400 + ipBoost) -> "flip Augury w/ kills (last resort)"
                (shouldFlip && sb == Carnage) |=> (800 + ipBoost) -> "flip Carnage (discount available)"
                (!shouldFlip) |=> -5000 -> "BLOCK: no IP discount needed"

            // v4 (2026-05-12): Library tome IP flip. Guardian/Larvae/Yr auto-restore at
            // next doom phase so flipping them costs at most the lost AP-use. TomeBarrier
            // is passive and doesn't auto-restore, so flipping it loses the battle-block
            // until Ghato awakens or Silence Token is spent. Tier 1300 puts auto-restoring
            // tomes between Augury (1500) and TheEyeOpens (1200); TomeBarrier scored lower
            // because flipping it has a real defensive cost.
            case FBInfernalPactChooseTomeAction(_, tome) =>
                // v5.5 (2026-05-13): ritualDiscountT trigger removed.
                val hasGateCultistsT = self.gates.exists(r => self.at(r, Acolyte).any)
                // v5.9 (2026-05-13): power < 6 gate removed.
                val writheDiscountT = hasGateCultistsT &&
                    infernalDiscount < 2 && game.turn >= 2
                val combatFuelT = areas.exists(r => self.at(r, Acolyte).any && r.foes.any) &&
                    infernalDiscount < 2 && game.turn >= 2
                val needsTwoFDT = self.needs(FBTwoFacedownSpellbooks)
                val readyForTwoFDT = needsTwoFDT && self.gates.num >= 2
                val shouldFlipT = writheDiscountT || combatFuelT || readyForTwoFDT
                // v4 (2026-05-12): per user rule, prefer non-FB items first. Tomes are non-FB
                // and auto-restoring (Guardian/Larvae/Yr) costs only one AP of tome power,
                // so they tier slightly above NeutralSpellbook (1800). TomeBarrier has a real
                // defensive cost (loses battle-block until Ghato) so it stays in the lower
                // tier next to CoF.
                (shouldFlipT && tome != TomeBarrier) |=> 1900 -> "flip auto-restoring tome (cheapest non-FB flip)"
                (shouldFlipT && tome == TomeBarrier) |=>  900 -> "flip TomeBarrier (lose passive block till Ghato)"
                (!shouldFlipT)                       |=> -5000 -> "BLOCK: no IP discount needed (tome)"

            case FBInfernalPactDoneAction(_) =>
                // v5.13 (2026-05-14): Done competes in main menu with Choose actions
                // (IP main is Soft-exploded). Apply same wantIP boost as Choose.
                // SBR gating: when 2-Facedown SBR is unmet, only exit after 2 flips.
                val enemyGooAtGateD = others.exists(f => f.allInPlay.exists(u =>
                    u.uclass.utype == GOO && u.region.onMap &&
                    (f.gates.has(u.region) || u.region.gate)))
                val flingComboD = (power == 1 || power == 2) &&
                    self.onMap(Ghatanothoa).any &&
                    projDoom(self) >= 12 &&
                    self.onMap(RevenantOfKnaa).num < 2 &&
                    self.pool(RevenantOfKnaa).any &&
                    enemyGooAtGateD
                // Recompute wantIP for boost (same triggers as IP main).
                val hasGateCultistsD = self.gates.exists(r => self.at(r, Acolyte).any)
                val writheDiscountD = hasGateCultistsD && infernalDiscount < 2 && game.turn >= 2
                val combatFuelD = areas.exists(r => self.at(r, Acolyte).any && r.foes.any) &&
                    infernalDiscount < 2 && game.turn >= 2
                val gatesNumD = self.gates.num
                val projectedDoomD = self.doom + ((self.es.num + 2) * 5 / 3) + gatesNumD
                val readyTwoFDD = self.needs(FBTwoFacedownSpellbooks) && gatesNumD >= 2
                val highDoomModeD = projectedDoomD >= 15 && infernalDiscount < 2
                val wantIPD = writheDiscountD || combatFuelD || readyTwoFDD || highDoomModeD
                val doneBoost = if (wantIPD) 11000 else 0
                val needsTwoFDDone = self.needs(FBTwoFacedownSpellbooks)
                if (flingComboD) {
                    ((power + infernalDiscount) >= 3) |=> (2500 + doneBoost) -> "FLING COMBO done: can summon Rev"
                    ((power + infernalDiscount) < 3) |=> -3000 -> "FLING COMBO: keep flipping for Rev summon"
                }
                else {
                    (infernalDiscount >= 2) |=> (10000 + doneBoost) -> "HARD STOP: 2 IP flips is maximum"
                    // SBR gating: if 2-Facedown unmet, don't exit at discount=1 — keep flipping
                    (infernalDiscount >= 1 && !needsTwoFDDone) |=> (2000 + doneBoost) -> "done with discount banked"
                    (infernalDiscount >= 1 && needsTwoFDDone) |=> -3000 -> "keep flipping (2-Facedown SBR unmet)"
                    (infernalDiscount == 0) |=> -500 -> "no discount yet — keep flipping"
                }

            case FBInfernalPactCancelAction(_) =>
                // v5.11c (2026-05-14): reverted v5.10 Cancel-IP refund tier (created
                // flip-cancel loops crashing 20/500 games + WR regression).
                true |=> -500 -> "cancel is usually suboptimal"

            case FBInfernalPactCancelMainAction(_) =>
                true |=> -1000 -> "cancel from main is usually bad"

            // ── DOOM-PHASE IP SCORING (v4: kept for reference, not active) ────
            // v4 (2026-05-12): IP was entirely removed from the doom phase at the
            // game-engine level (FactionFB.scala). These cases would never be
            // reached, but the scoring logic is preserved for reference in case
            // any future rule change re-introduces doom-phase IP, or for comparing
            // against the new main-phase scoring above. Do NOT uncomment without
            // also re-enabling the doom-phase IP entry points in FactionFB.scala.
            /*
            case FBInfernalPactDoomMainAction(_) =>
                val hasFlippable = self.spellbooks.exists(sb2 =>
                    sb2 != DevilsMark && sb2 != CyclopeanGaze && !self.oncePerGame.has(sb2))
                // T13: IP BEFORE ritual — same projected doom check as AP IP
                val gatesNumDoom = self.gates.num
                val projectedDoomDoom = self.doom + ((self.es.num + 2) * 5 / 3) + gatesNumDoom
                val ritualNeedsDiscount = game.ritualCost > 6 && gatesNumDoom > 2 && hasFlippable && projectedDoomDoom > 20
                ritualNeedsDiscount |=> 9600 -> "T13: doom IP BEFORE ritual (projected doom > 20)"
                val canRitualAfterIP = self.gates.num >= 3 && hasFlippable && power + 4 >= game.ritualCost
                val canAffordRitualNow = power >= game.ritualCost && self.gates.num >= 3
                (canRitualAfterIP && !canAffordRitualNow) |=> 7000 -> "doom IP: unlock a 3-gate ritual"

            case FBInfernalPactDoomChooseAction(_, sb) =>
                (sb == CyclopeanGaze) |=> -10000 -> "NEVER flip CG doom IP"
                (sb == DevilsMark) |=> -10000 -> "NEVER flip DM doom IP"
                (sb == Augury && auguryKills > 0) |=> -3000 -> "don't flip Augury with kills"
                // Doom IP fires when: gates > 2 AND projected doom > 20
                val gatesNumDoomC = self.gates.num
                val projectedDoomDoomC = self.doom + ((self.es.num + 2) * 5 / 3) + gatesNumDoomC
                val doomIPCondition = gatesNumDoomC > 2 && game.ritualCost > 6 && projectedDoomDoomC > 20
                // When conditions met: boost scores above ritual (9500)
                val doomBoost = if (doomIPCondition) 9000 else 0
                (sb == Augury && auguryKills == 0) |=> (1500 + doomBoost) -> "flip empty Augury first"
                (sb == TheEyeOpens) |=> (1200 + doomBoost) -> "flip Eye Opens second"
                (sb == Carnage) |=> (1000 + doomBoost) -> "flip Carnage third"
                (sb == CallOfTheFaithful) |=> (800 + doomBoost) -> "flip CoF fourth"
                (!doomIPCondition) |=> -5000 -> "BLOCK doom IP: conditions not met"

            case FBInfernalPactDoomDoneAction(_) =>
                (infernalDiscount >= 1) |=> 2000 -> "done with doom discount"

            case FBInfernalPactDoomCancelAction(_) =>
                true |=> -500 -> "cancel doom IP suboptimal"

            case FBInfernalPactCancelDoomAction(_) =>
                true |=> -1000 -> "cancel doom IP from main is usually bad"
            */

            // ──────────────────────────────────────────────────────────────────
            // THE EYE OPENS — boost when there's a desiccated near enemy cultist
            // ──────────────────────────────────────────────────────────────────
            case FBTheEyeOpensMainAction(_) =>
                val eyeTargets = areas.%(r => self.at(r, Desiccated).any && r.foes.cultists.any)
                eyeTargets.any |=> 6000 -> "Eye Opens: desiccated + enemy cultist target available"
                (eyeTargets.num >= 2) |=> 7000 -> "Eye Opens: 2+ regions to sweep"
                (power <= 2) |=> 2000 -> "stall with eye opens when low power"
                true |=> 1000 -> "eye opens for cultist pressure"
                // v5.8 (2026-05-13): opportunistic trigger — 2+ gates and 2 Desc
                // co-located with exactly 1 cultist at an enemy gate. Beats
                // generic eye-opens scores so this fires when setup is complete.
                val eyeOpportunistic = self.gates.num >= 2 && areas.exists(r =>
                    r.enemyGate && self.at(r, Desiccated).num >= 2 && r.foes.cultists.num == 1 && r.foes.goos.none)
                eyeOpportunistic |=> 9400 -> "Eye Opens (opportunistic): 2 Desc + lone cult at enemy gate"

            case FBTheEyeOpensRegionAction(_, r, _) =>
                r.enemyGate |=> 3000 -> "eye opens: target enemy gate region"
                val soleCultist = others.exists(f => f.gates.has(r) && f.at(r).%(_.canControlGate).num == 1)
                soleCultist |=> 4000 -> "eye opens: sole gate controller in region"
                true |=> 500 -> "eye opens: target region"

            case FBTheEyeOpensFactionAction(_, r, f, _) =>
                // Target faction with most gates first, then power, then doom
                val gateScore = f.gates.num * 100
                val powerScore = (f.power / 3) * 10
                val doomScore = f.doom / 5
                true |=> (1000 + gateScore + powerScore + doomScore) -> ("eye opens: target " + f.short + " (gates=" + f.gates.num + ")")

            case FBTheEyeOpensChooseCultistAction(_, _, _, uRef, _) =>
                val u = game.unit(uRef)
                (u.uclass == HighPriest) |=> -2000 -> "don't sacrifice HP to eye opens"
                (u.onGate) |=> -1500 -> "keep gate keeper"

            case FBTheEyeOpensCancelAction(_) =>
                true |=> -5000 -> "don't cancel eye opens"

            case FBWritheUndoLastKillAction(_, _, _) =>
                true |=> -1000000 -> "bot never undoes writhe kill"

            case FBWritheUndoLastPainAction(_, _, _) =>
                true |=> -1000000 -> "bot never undoes writhe pain"

            case FBWritheUndoLastMoveAction(_, _, _) =>
                true |=> -1000000 -> "bot never undoes writhe move"

            case FBWritheUndoAllAction(_, _, _) =>
                true |=> -1000000 -> "bot never undoes writhe"

            // ──────────────────────────────────────────────────────────────────
            // LIBRARIAN AGONY — FB-specific satisfaction priority
            // ──────────────────────────────────────────────────────────────────
            case LibrarianReturnTomeMainAction(_, _, _, _, _) =>
                true |=> 5000 -> "FB agony: return overdue tome (best option)"

            case LibrarianEliminateUnitMainAction(_, _, _, _, _) =>
                true |=> 3000 -> "FB agony: eliminate units"

            case LibrarianEliminateRegionAction(_, r, _, _, _, _, _) =>
                // Prefer regions with expendable units
                val hasCultOffGate = self.at(r).%(u => u.canControlGate && !u.onGate).any
                val hasDesc = self.at(r, Desiccated).any
                hasCultOffGate |=> 2000 -> "FB agony region: has off-gate cultist"
                hasDesc |=> 1500 -> "FB agony region: has desiccated"
                true |=> 500 -> "FB agony region: default"

            case LibrarianEliminateUnitAction(_, uRef, _, _, _, _, _, _) =>
                val u = game.unit(uRef)
                val isOnGate = u.onGate
                val isCultist = u.canControlGate
                val isDesc = u.uclass == Desiccated
                val isGhato = u.uclass == Ghatanothoa
                val ghatoCost = if (isGhato) u.uclass.cost else 0
                // Priority: off-gate cultist > desiccated > on-gate cultist > cheap ghato > others
                (isCultist && !isOnGate)       |=> 4000 -> "FB agony: eliminate off-gate cultist (best)"
                (isDesc)                       |=> 3000 -> "FB agony: eliminate desiccated"
                (isCultist && isOnGate)        |=> 2000 -> "FB agony: eliminate on-gate cultist"
                (isGhato && ghatoCost < 5)     |=> 11000 -> "FB agony: eliminate cheap ghato (overrides BotMaps GOO block)"
                (isGhato && ghatoCost >= 5)    |=> -5000 -> "FB agony: don't eliminate expensive ghato"
                (u.uclass == RevenantOfKnaa)   |=> -3000 -> "FB agony: don't eliminate revenant"

            case LibrarianEliminateDoneAction(_, _, _, _, _, eliminated) =>
                eliminated.any |=> 2000 -> "FB agony: done eliminating"
                true |=> -100 -> "FB agony: done with nothing"

            case LibrarianLoseDoomAction(_, _, _, _, _) =>
                true |=> -1000 -> "FB agony: lose doom (worst option)"

            // ──────────────────────────────────────────────────────────────────
            // CALL OF THE FAITHFUL — strategy: always use when Rev/Ghato on gate
            // ──────────────────────────────────────────────────────────────────
            case FBCallOfTheFaithfulMainAction(_) =>
                // Fix 1: Ghato/Rev on empty or abandoned gate → CoF HIGHEST (beats writhe)
                // to place cultist and claim gate
                val ghatoOnEmptyEnemyGate = game.gates.exists(r =>
                    !self.gates.has(r) && others.%(_.gates.has(r)).none &&
                    (self.at(r, Ghatanothoa).any || self.at(r, RevenantOfKnaa).any))
                val ghatoOnEnemyGateNoCultists = game.gates.exists(r =>
                    others.%(_.gates.has(r)).any &&
                    (self.at(r, Ghatanothoa).any || self.at(r, RevenantOfKnaa).any) &&
                    others./~(_.at(r)).%(_.cultist).none &&
                    others./~(_.at(r)).%(_.uclass.utype == GOO).none)
                (ghatoOnEmptyEnemyGate) |=> 9600 -> "CoF: Ghato on empty gate — claim it (HIGHEST)"
                (ghatoOnEnemyGateNoCultists) |=> 9600 -> "CoF: Ghato on enemy gate, no cultists — claim it"
                val revGhatoOnGate = self.gates.exists(r => self.at(r, Ghatanothoa).any || self.at(r, RevenantOfKnaa).any)
                revGhatoOnGate |=> 6000 -> "CoF: Rev/Ghato on gate — always use"
                (power <= 1) |=> 3000 -> "CoF: stall with free acolyte"
                true |=> 2500 -> "CoF: baseline free acolyte"

            case FBCallOfTheFaithfulAction(_, r) =>
                // Highest: place at gate where Ghato/Rev is (to claim empty/enemy gate)
                val ghatoHereNoCtrl = (self.at(r, Ghatanothoa).any || self.at(r, RevenantOfKnaa).any) &&
                    self.at(r).%(_.canControlGate).none && game.gates.has(r)
                ghatoHereNoCtrl |=> 9500 -> "CoF: place at Ghato's gate to claim it"
                val unclaimedGateHere = game.gates.has(r) && self.at(r).%(_.onGate).none
                unclaimedGateHere |=> 7000 -> "CoF: unclaimed gate → auto-claim"
                r.ownGate |=> 1000 -> "place near own gate"
                (self.at(r, Ghatanothoa).any) |=> 1500 -> "place near GOO for protection"
                r.enemyGate |=> 2000 -> "place at enemy gate for pressure"

            // ──────────────────────────────────────────────────────────────────
            // CARNAGE
            // ──────────────────────────────────────────────────────────────────
            // v5 (2026-05-13): if Ghato was killed in this battle, prefer the
            // FLIP-SB route to grab the ES — Ghato dying triggers a full
            // awaken-cycle on the next ritual anyway, so spending an SB is
            // cheaper than paying 1 power. If Ghato survived, pay 1 power to
            // bank the ES without burning an SB.
            case FBCarnagePayPowerAction(_) =>
                val ghatoDiedThisBattle = game.battle.exists(b =>
                    b.eliminated.exists(u => u.faction == FB && u.uclass == Ghatanothoa))
                (!ghatoDiedThisBattle && power >= 1) |=> 3000 -> "v5: pay 1 power for Carnage ES (Ghato alive)"
                (ghatoDiedThisBattle) |=> 500 -> "v5: low pay-power priority (Ghato died — flip instead)"
                (power < 1) |=> -1000 -> "no power for carnage"

            case FBCarnageFlipSpellbookAction(_) =>
                val ghatoDiedThisBattle = game.battle.exists(b =>
                    b.eliminated.exists(u => u.faction == FB && u.uclass == Ghatanothoa))
                ghatoDiedThisBattle |=> 4000 -> "v5: flip SB for Carnage ES (Ghato died this battle)"
                (!ghatoDiedThisBattle) |=> 500 -> "v5: low flip priority (Ghato alive — pay power instead)"

            case FBCarnageChooseSpellbookAction(_, sb) =>
                // v5.18 (2026-05-14): prefer flipping Carnage itself — pre-battle
                // IP setup saves Carnage faceup for this moment, so Carnage IS the
                // SB consumed as the Carnage-ES payment. If Ghato died this battle
                // and FB can re-awaken, ANY remaining SB (including CG) is fine
                // — flip whatever's left after Carnage gets used.
                val ghatoDiedCC = game.battle.exists(b =>
                    b.eliminated.exists(u => u.faction == FB && u.uclass == Ghatanothoa))
                val ghatoCostCC = math.max(1, 11 - game.ritualCost)
                val canReAwakenCC = power >= ghatoCostCC
                (sb == Carnage) |=> 3500 -> "flip Carnage ITSELF (saved as Carnage-ES payment)"
                (ghatoDiedCC && canReAwakenCC && sb == CyclopeanGaze) |=> 2500 -> "Ghato died + can re-awaken: CG flip OK for Carnage ES"
                (ghatoDiedCC && canReAwakenCC && sb == DevilsMark) |=> 2500 -> "Ghato died + can re-awaken: DM flip OK for Carnage ES"
                (ghatoDiedCC && canReAwakenCC && sb == TheEyeOpens) |=> 2500 -> "Ghato died + can re-awaken: Eye Opens flip OK for Carnage ES"
                (sb == CallOfTheFaithful) |=> 2000 -> "flip CoF for ES"
                (sb == Augury && auguryKills == 0) |=> 1500 -> "flip empty Augury"
                // v5.17 (2026-05-14): soft penalty for Augury-with-kills (last resort)
                (sb == Augury && auguryKills > 0) |=> 400 -> "flip Augury w/ kills (last resort)"

            case FBCarnageCancelAction(_) =>
                true |=> -500 -> "cancel carnage"

            // ──────────────────────────────────────────────────────────────────
            // HIGH PRIEST SACRIFICE (gain 2 power)
            // ──────────────────────────────────────────────────────────────────
            case SacrificeHighPriestMainAction(_) =>
                (power <= 3 && self.all(HighPriest).any) |=> 5000 -> "sacrifice HP for 2 power (low-power)"

            case SacrificeHighPriestDoomAction(_) =>
                // In doom phase, sacrifice HP to enable a ritual we couldn't otherwise afford
                (power < game.ritualCost && self.all(HighPriest).any) |=> 6500 -> "sacrifice HP to enable ritual"

            case SacrificeHighPriestAction(_, r, _) =>
                // Sacrifice the HP with the most power value (anywhere we have one)
                self.all(HighPriest).any |=> 2000 -> "sacrifice HP"

            // ──────────────────────────────────────────────────────────────────
            // AUGURY
            // ──────────────────────────────────────────────────────────────────
            // v5 (2026-05-13): Augury kills are saved for battle GOO-kills, NOT
            // used for writhe. Per user spec.
            case FBWritheAuguryReplaceAction(_, _, n) =>
                true |=> -3000 -> "v5: don't use augury kills for writhe (save for battle GOO)"

            case FBWritheAuguryCancelAction(_, _) =>
                true |=> 500 -> "v5: skip writhe augury — save for battle GOO kills"

            // v5 (2026-05-13): use augury kills in battle ONLY when enemy GOO is
            // present AND rolledKills + auguryKills > non-GOO enemy unit count.
            // That guarantees the augury kills will reach the GOO (the non-GOOs
            // soak the rolled kills first; the augury surplus goes to the GOO).
            case FBAuguryBattleReplaceAction(_, n) =>
                val battleStats: (Int, Int, Int) = game.battle.map { b =>
                    val fbSide = if (b.attacker == FB) b.attackers else b.defenders
                    val enemySide = if (b.attacker == FB) b.defenders else b.attackers
                    (fbSide.rolls.count(_ == Kill),
                     enemySide.forces.num,
                     enemySide.forces.%(_.uclass.utype == GOO).num)
                }.getOrElse((0, 0, 0))
                val rolledKills = battleStats._1
                val enemyUnits = battleStats._2
                val enemyGOOs = battleStats._3
                val hasEnemyGOO = enemyGOOs > 0
                val bankedKills = auguryKills
                // v5.17 (2026-05-14): User spec — spend EXACTLY enough augury
                // banked kills to guarantee enemy GOO death.
                //   1 GOO: target = enemy_units (must kill everything; GOO dies last)
                //   2+ GOOs: target = enemy_units - 1 (one GOO can survive, one dies)
                // Fire 9500 only when banked kills are enough to reach the target
                // AND `n` equals the EXACT augury amount needed. Smaller n won't
                // reach (waste); larger n is wasted excess.
                val targetKillsAug =
                    if (enemyGOOs == 1) enemyUnits
                    else if (enemyGOOs >= 2) enemyUnits - 1
                    else 0
                val auguryNeededAug = math.max(0, targetKillsAug - rolledKills)
                val canReachAug = hasEnemyGOO && (rolledKills + bankedKills) >= targetKillsAug
                (canReachAug && n == auguryNeededAug && n > 0) |=>
                    9500 -> ("v5.17: spend " + n + " augury kills (target " + targetKillsAug + ", rolled " + rolledKills + ", " + enemyGOOs + " GOO)")
                (hasEnemyGOO && !canReachAug) |=> -1500 -> "v5: don't use augury — can't reach GOO"
                (!hasEnemyGOO) |=> -500 -> "v5: no enemy GOO — save augury kills"

            case FBAuguryBattleCancelAction(_) =>
                val battleStats: (Int, Int, Int) = game.battle.map { b =>
                    val fbSide = if (b.attacker == FB) b.attackers else b.defenders
                    val enemySide = if (b.attacker == FB) b.defenders else b.attackers
                    (fbSide.rolls.count(_ == Kill),
                     enemySide.forces.num,
                     enemySide.forces.%(_.uclass.utype == GOO).num)
                }.getOrElse((0, 0, 0))
                val rolledKills = battleStats._1
                val enemyUnits = battleStats._2
                val enemyGOOs = battleStats._3
                val enemyNonGOOs = enemyUnits - enemyGOOs
                val hasEnemyGOO = enemyGOOs > 0
                // Available augury kills isn't passed to the Cancel action — be
                // conservative: cancel preferred when no GOO is present or when
                // even adding kills wouldn't beat non-GOOs.
                (!hasEnemyGOO) |=> 1500 -> "v5: cancel augury — no enemy GOO to kill"
                (hasEnemyGOO && rolledKills <= enemyNonGOOs) |=> 200 -> "v5: cancel — rolled kills won't reach GOO without augury topup"
                (hasEnemyGOO && rolledKills > enemyNonGOOs) |=> -2000 -> "v5: don't cancel — augury can hit GOO"

            // ──────────────────────────────────────────────────────────────────
            // CYCLOPEAN GAZE
            // ──────────────────────────────────────────────────────────────────
            case FBCyclopeanGazeUseAction(_, _, _, _, _, _) =>
                true |=> 5000 -> "always use CG"

            case FBCyclopeanGazeSkipAction(_, _, _, _, _, _) =>
                true |=> -5000 -> "avoid skipping CG"

            case FBCyclopeanGazePainUnitAction(_, _, uRef, _, _, _, _) =>
                val u = game.unit(uRef)
                (u.goo) |=> 5000 -> "pain enemy GOO"
                (u.gateKeeper) |=> 4000 -> "pain gate controller"
                (u.cultist) |=> 1000 -> "pain cultist"
                (u.monster) |=> 2000 -> "pain monster"

            case FBCyclopeanGazeDestinationAction(_, uRef, r, _, _, _, _) =>
                val u = game.unit(uRef)
                r.foes.none |=> 2000 -> "pain to empty area"
                r.ownGate |=> -2000 -> "don't pain to own gate area"
                r.ocean && !u.goo |=> 1500 -> "pain to ocean (non-GOO)"
                (u.goo && r.enemyGate && r.foes.goos.none) |=> 5500 -> "pain GOO into enemy unprotected gate"
                (u.goo && r.foes.goos.any) |=> 5000 -> "pain GOO into enemy GOO region"
                (u.monster && r.empty) |=> 4500 -> "scatter monster to empty region"
                (u.monster && r.foes.monsterly.any && r.enemyGate) |=> 4000 -> "monster into enemy fortress (die for us)"
                (u.faction == GC && r.glyph != Ocean) |=> 4000 -> "pain GC unit onto land"

            case FBCyclopeanGazeKillChoiceAction(_, _, killRef, _, _, _, _, _) =>
                val k = game.unit(killRef)
                (k.goo) |=> -5000 -> "don't sacrifice GOO to CG kill"
                (k.uclass == HighPriest) |=> -3000 -> "don't sacrifice HP to CG kill"
                (k.gateKeeper) |=> -2000 -> "don't sacrifice gate keeper"
                (k.monster) |=> -1500 -> "avoid sacrificing monster"
                (k.uclass == Acolyte && !k.onGate) |=> 1000 -> "sacrifice off-gate acolyte"
                (k.cultist && !k.onGate) |=> 500 -> "sacrifice off-gate cultist"
                true |=> 0 -> "default CG kill-choice"

            // ──────────────────────────────────────────────────────────────────
            // STANDARD ACTIONS — Move / Attack
            // ──────────────────────────────────────────────────────────────────
            case MoveAction(_, u, from, to, _) =>
                // DS chaos gate penalty: avoid moving to chaos gate regions
                to.chaosGate |=> -3000 -> "avoid chaos gate region"

                // HARD BLOCK: don't strip gate keepers from FB's own gate. If
                // moving this cultist would leave canControlGate < 2 at the origin,
                // block it with magnitude > max positive rule.
                val stripsLastKeeper = u.cultist && from.ownGate &&
                    self.at(from).%(_.canControlGate).num <= 2
                // In AP1 with FBNoAcolytesInStart unmet, the short-term gate loss
                // is worth it — the SB reward + multi-gate setup pays back.
                // Baseline clears start in AP1.
                // User strategy 2026-04-16: NEVER strip own-gate keepers below 2,
                // even in AP1 for FBNoAcolytesInStart SBR. Strip-via-Move chain
                // was losing gates to enemies. Use Writhe-Move only for clearing.
                stripsLastKeeper |=> -15000 -> "HARD BLOCK: don't strip gate keeper from own gate (always)"

                // HARD BLOCK: don't move a cultist solo into a region with an
                // enemy monster/goo unless pairing with an FB monster there.
                // Captures in mid-game are the primary gate-loss vector.
                val enemyMonsterAtDest = to.foes.monsterly.any || to.foes.goos.any
                val fbMonsterAtTo = self.at(to).%(x => x.uclass == Desiccated || x.uclass == RevenantOfKnaa || x.uclass == Ghatanothoa).any
                val moverPairsAtDest = u.cultist && fbMonsterAtTo
                val soloCultistIntoCaptureThreat = u.cultist && enemyMonsterAtDest && !moverPairsAtDest && from != to
                soloCultistIntoCaptureThreat |=> -15000 -> "HARD BLOCK: solo cultist into capture threat"

                // Round 9 DEFENSE: moving Ghato/Rev TO an own gate that lacks a
                // defender is the highest-priority move — BUT only if the origin
                // isn't ALSO an undefended own gate. Otherwise we'd just shuffle
                // Ghato from gate to gate leaving the origin stripped.
                val isGhato = u.uclass == Ghatanothoa
                val isRev = u.uclass == RevenantOfKnaa
                val ownGateNeedsDefense = to.ownGate && self.at(to, Ghatanothoa).none && self.at(to, RevenantOfKnaa).none
                // canLeaveOrigin: Ghato/Rev can leave own gate ONLY if another defender stays
                // Bug fix: u is UnitRef, self.at() returns UnitFigures. Compare via ref matching.
                val fromHasOtherDefender = from.ownGate &&
                    self.at(from).%(x => (x.uclass == Ghatanothoa || x.uclass == RevenantOfKnaa) &&
                        x.ref != u).any
                val totalDefenders = self.all(Ghatanothoa).num + self.all(RevenantOfKnaa).num
                val canLeaveOrigin = !from.ownGate || (fromHasOtherDefender && totalDefenders >= 2)
                // Ghato/Rev to undefended own gate: important but should NOT
                // beat Writhe-first (8500) when Writhe is available. Writhe can
                // pain Ghato to the gate for free. Score below Writhe-first
                // unless Writhe is unavailable (power < 4 or desc >= 6).
                val writheAvailable = writheDice >= 4 && desiccatedOnMap < 6
                // Strategy: after awaken with low power, DON'T move Ghato — save for next AP writhe.
                // Only move Ghato to own gate if writhe not available AND gates >= 3 (defensive).
                val saveForNextAPWrithe = isGhato && !writheAvailable && self.gates.num < 3 && power < 4
                // When writhe is available, prefer writhe over paid moves
                // Ghato move to adjacent vulnerable gate: when < 3 gates, not on a gate, adjacent to target
                // Ghato move to adjacent vulnerable gate: only if not sole defender of current gate
                val ghatoAdjacentVulnGate = isGhato && self.gates.num < 3 &&
                    from.near.contains(to) && gateVulnerability(to) > 0 && canLeaveOrigin
                val destVuln = if (ghatoAdjacentVulnGate) gateVulnerability(to) else 0
                (ghatoAdjacentVulnGate && destVuln > 0) |=> (destVuln - 1000) -> ("Ghato move to adjacent vuln gate " + destVuln)

                // Cathedral vacating via move: Ghato at cathedral with AN combat, power < 3, cost >= 4
                val ghatoVacateCathedral = isGhato && from.glyph != Ocean &&
                    game.cathedrals.has(from) && AN.exists &&
                    AN.at(from).%(a => a.uclass.utype == Monster || a.uclass.utype == Terror).any &&
                    u.uclass.cost >= 4 && power < 3
                (ghatoVacateCathedral && to.ownGate) |=> 9000 -> "URGENT move: Ghato vacates cathedral (AN combat) to own gate"
                (ghatoVacateCathedral && !to.ownGate && to.glyph != Ocean) |=> 7000 -> "URGENT move: Ghato vacates cathedral (AN combat) to land"
                // Anti-ping-pong: block Ghato from moving back to where it came from this AP
                val ghatoPingPong = isGhato && game.fbGhatoLastMoveOrigin.contains(to)
                ghatoPingPong |=> -12000 -> "BLOCK: Ghato ping-pong (moving back to origin)"
                // Also block Ghato from moving at all if it already moved this AP
                val ghatoAlreadyMoved = isGhato && game.fbGhatoLastMoveOrigin.isDefined
                ghatoAlreadyMoved |=> -8000 -> "Ghato already moved this AP (save power)"
                (isGhato && ownGateNeedsDefense && canLeaveOrigin && writheAvailable) |=> 3000 -> "Ghato to undefended gate (low — writhe preferred)"
                (isGhato && ownGateNeedsDefense && canLeaveOrigin && !writheAvailable && !saveForNextAPWrithe) |=> 9000 -> "Ghato to undefended gate (no Writhe, defensive)"
                (saveForNextAPWrithe && ownGateNeedsDefense) |=> 2000 -> "Ghato: save for next AP writhe (low power, < 3 gates)"
                (isRev && ownGateNeedsDefense && canLeaveOrigin && writheAvailable) |=> 3000 -> "Rev to undefended gate (low — writhe preferred)"
                (isRev && ownGateNeedsDefense && canLeaveOrigin && !writheAvailable) |=> 8500 -> "Rev to undefended gate (no Writhe available)"

                // Strategy 3.3: after re-awaken, if < 4 power, move Ghato to weakest FB gate
                val justReawakened = isGhato && game.fbGhatnothoaAwakenings >= 2 && power < 4
                val weakestOwnGate = to.ownGate && ownGateNeedsDefense
                (justReawakened && weakestOwnGate && canLeaveOrigin) |=> 9200 -> "post-reawaken: Ghato to weakest FB gate (power < 4)"

                // Ghato to adjacent free/enemy gate — 2nd best after writhe
                val adjacentGateTarget = to.gate && !to.ownGate && to.foes.goos.none && from.near.contains(to)
                val ghatoToAdjacentGate = isGhato && gooOnMap > 0 && power > 5 && self.gates.num < 3 &&
                    adjacentGateTarget && canLeaveOrigin
                val adjFoeCombat = to.foes.%(f2 => f2.uclass.utype == Monster || f2.uclass.utype == Terror).num
                (ghatoToAdjacentGate && to.freeGate && to.foes.none) |=> 5800 -> "Ghato to adjacent free gate (no units — 2nd after writhe)"
                (ghatoToAdjacentGate && to.freeGate && adjFoeCombat < 4) |=> 5500 -> "Ghato to adjacent free gate (low combat — 2nd after writhe)"
                (ghatoToAdjacentGate && to.enemyGate && adjFoeCombat < 4) |=> 5200 -> "Ghato to adjacent enemy gate (low combat — 2nd after writhe)"

                // Gate-steal cycle (user strategy): after DM destroys own gate,
                // Writhe/Move Ghato to vulnerable enemy gate to restore 3 gates.
                // Fires when bot is BELOW 3 gates with craters on map (sign of
                // recent DM) AND target is a vulnerable enemy gate.
                // ── GATE VULNERABILITY SCORING (MoveAction) ──
                // Uses shared gateVulnerability(). Ghato move to adjacent vulnerable gate
                // when < 3 gates. Also handles post-DM steal and AP2-3 steal.
                // Blocks (Ice Age, cathedral+AN) handled by vulnScore == -1.
                val moveVulnScore = gateVulnerability(to)
                val belowThreeWithCrater = self.gates.num < 3 && craterCount >= 1
                // Ghato to adjacent vulnerable gate: score based on vulnerability ranking
                // Subtracts 1000 so writhe (which can do this for free) is preferred
                val ghatoToVulnGate = isGhato && self.gates.num < 3 && moveVulnScore > 0 &&
                    from.near.contains(to) && canLeaveOrigin
                (ghatoToVulnGate) |=> (moveVulnScore - 1000) -> ("Ghato move to vuln gate " + moveVulnScore)
                // Post-DM Ghato steal: below Writhe-first (8500) so bot Writhes first
                val ghatoStealGate = isGhato && belowThreeWithCrater && moveVulnScore > 0 && canLeaveOrigin
                ghatoStealGate |=> 7500 -> "Ghato steals vulnerable gate (post-DM, Writhe preferred)"
                // Block dangerous gates via move
                (isGhato && moveVulnScore == -1) |=> -12000 -> "BLOCK: move to blocked gate (Ice Age/cathedral+AN)"
                // WW: avoid Antarctica
                (WW.exists && to.name == "Antarctica") |=> -8000 -> "avoid Antarctica when WW in game"

                // Round 9 FACTION-SPECIFIC: OW Dread Curse — don't leave Ghato alone
                // if OW has 3+ Abominations/Spawns and Ghato is the unit being moved out
                val owDreadThreat = OW.exists && OW.has(DreadCurse) &&
                    (OW.all(Abomination).num + OW.all(SpawnOW).num) >= 3
                val ghatoLeavingSupport = isGhato && from.ownGate &&
                    self.at(from).%(u => u.uclass == Ghatanothoa || u.uclass == RevenantOfKnaa).num == 1 &&
                    self.at(to).cultists.none
                (owDreadThreat && ghatoLeavingSupport) |=> -7000 -> "OW Dread Curse threat: keep Ghato with support"
                // HARD BLOCK: never move LAST Ghato/Rev away from own gate to
                // non-gate region. Stripping the only defender invites capture.
                val isLastDefenderAtGate = from.ownGate && (isGhato || isRev) &&
                    self.at(from).%(x => x.uclass == Ghatanothoa || x.uclass == RevenantOfKnaa).num == 1
                (isLastDefenderAtGate && !to.ownGate) |=> -10000 -> "HARD BLOCK: last Ghato/Rev leaving own gate"
                // Softer block: don't move Ghato/Rev out of defended gate to non-gate
                val fromDefendedGate = from.ownGate && self.gates.num <= 2
                (isGhato && fromDefendedGate && !to.ownGate && !isLastDefenderAtGate) |=> -6000 -> "don't strip Ghato from own gate"
                (isRev && fromDefendedGate && !to.ownGate && !isLastDefenderAtGate) |=> -4000 -> "don't strip Rev from own gate"

                // T06: at power 1 with < 6 cultists, block moves — recruit instead
                val cultistsOnMapMove = self.all(Acolyte).num + self.all(HighPriest).num
                (power == 1 && cultistsOnMapMove < 6) |=> -8000 -> "T06: BLOCK move at 1P — recruit cultist instead"
                // Block acolyte moves when writhe is available — writhe pains are free
                val writheAffordable = writheDice >= 4 && desiccatedOnMap < 6 && self.gates.num >= 1
                (u.cultist && writheAffordable) |=> -6000 -> "BLOCK: acolyte move when writhe available (use free writhe-pain)"
                (u.uclass == Desiccated && writheAffordable) |=> -4000 -> "BLOCK: desc move when writhe available"
                // T07: at power 1, 6 cultists, imbalance > 2, adjacent — allow move for balance
                val gateUnitsForMove = self.gates./(gr => self.at(gr).num)
                val moveImbalance = if (gateUnitsForMove.num >= 2) gateUnitsForMove.max - gateUnitsForMove.min else 0
                val t07Balance = power == 1 && cultistsOnMapMove >= 6 && moveImbalance > 2 &&
                    to.ownGate && from.near.contains(to)
                t07Balance |=> 3000 -> "T07: move for balance at 1P (6 cult, imbalance > 2, adjacent)"
                // Desc balance: move desc from gate with 2+ more desc than another adjacent gate
                val isDescMove = u.uclass == Desiccated
                val fromDescCount = if (from.ownGate) self.at(from, Desiccated).num else 0
                val adjOwnGates = self.gates.%(g => from.near.contains(g) && g != from)
                val minAdjDesc = if (adjOwnGates.any) adjOwnGates./(g => self.at(g, Desiccated).num).min else fromDescCount
                val descImbalance = fromDescCount - minAdjDesc
                val descBalance = isDescMove && from.ownGate && to.ownGate && from.near.contains(to) &&
                    descImbalance >= 2 && self.at(to, Desiccated).num < fromDescCount - 1
                descBalance |=> 2800 -> "desc balance: move from gate with 2+ more desc to adjacent gate"
                // Low-purpose movement — Writhe is preferred for ALL repositioning.
                // These fire only when Writhe is unavailable (power < 4).
                // Removed: low move to enemy gate. Use Writhe instead.
                // to.enemyGate |=> 500 -> "move to enemy gate (low — use Writhe)"
                // Block rev/desc movement to anywhere except own gates — use Writhe for repositioning
                val monsterToNonOwnGate = (u.uclass == RevenantOfKnaa || u.uclass == Desiccated) && !to.ownGate
                monsterToNonOwnGate |=> -6000 -> "BLOCK: rev/desc move to non-own-gate (use Writhe)"
                (u.uclass == Desiccated && from.ownGate) |=> -500 -> "don't strip Desiccated from own gate"
                to.ownGate |=> 200 -> "reinforce own gate"
                // User feedback 2026-04-16: when bot has Ghato/Rev at an own gate,
                // moving an acolyte THERE is a strong defensive consolidation play
                // — beats sending acolytes to random empty land. 2200 "move toward
                // second gate" was misfiring: bot sent acolyte to vulnerable region
                // instead of safe own-gate with Ghato.
                // User feedback: Writhe is almost always better than Move for FB.
                // Move scores should be LOW so Writhe (5500-7000) wins when available.
                // Only high-priority defensive moves (Ghato/Rev to gate) stay high.
                // Cultist leaving own gate: generally bad unless strategic
                val cultistLeavingOwnGate = u.cultist && from.ownGate && !to.ownGate
                cultistLeavingOwnGate |=> -6000 -> "don't move cultist off own gate"
                // Post-awaken: block ALL cultist moves to empty non-gate regions
                // Use writhe for repositioning, not paid moves
                val cultistToEmptyNonGate = u.cultist && gooOnMap > 0 && !to.gate && to.foes.none && self.at(to).none
                cultistToEmptyNonGate |=> -6000 -> "BLOCK: cultist to empty non-gate (use writhe)"
                val toOwnGateWithGOO = to.ownGate && (self.at(to, Ghatanothoa).any || self.at(to, RevenantOfKnaa).any)
                (u.cultist && toOwnGateWithGOO) |=> 3500 -> "consolidate cultist at own gate w/ Ghato/Rev"
                // Defensive reinforce: move cultist to thin own gate (1-2 keepers).
                // NOT in AP1 — AP1 should use Writhe for repositioning, not paid Moves.
                val toThinOwnGate = to.ownGate && self.at(to).%(_.canControlGate).num <= 2
                (u.cultist && toThinOwnGate && !toOwnGateWithGOO && !firstAP) |=> 4000 -> "reinforce thin own gate"
                (u.cultist && toThinOwnGate && !toOwnGateWithGOO && firstAP) |=> 1500 -> "AP1 thin gate reinforce (low — use Writhe instead)"
                // Gate expansion via Move — low scores. Writhe (5500-7000) is preferred
                // for repositioning. Move only as last resort when Writhe unavailable.
                (u.cultist && self.gates.num == 0 && to.foes.none && !to.gate && to.glyph != Ocean) |=>
                    2500 -> "move acolyte to empty land to build a gate"
                // Removed: random acolyte moves to empty land. Use Writhe instead.
                // (u.cultist && self.gates.num < 2 && to.foes.none && !to.gate && to.glyph != Ocean) |=>
                //     1500 -> "move acolyte toward second gate"
                // (u.cultist && self.gates.num == 2 && to.foes.none && !to.gate && to.glyph != Ocean) |=>
                //     1500 -> "move acolyte toward third gate"
                // Acolyte out of start: lowered 4500→2800 per user 2026-04-16 to
                // avoid Move-chain strip pattern (3 separate Moves draining start).
                // Writhe (7000) and Build (8800) preferred over single-cult moves.
                // AP1: suppress moves — Writhe handles positioning for free
                firstAP |=> -5000 -> "AP1: no moves, use Writhe for positioning"
                // Removed: acolyte out of start. Use Writhe instead.
                // (u.cultist && !firstAP && fbStartRegion.contains(from) && to.foes.none && !to.gate && to.glyph != Ocean &&
                //     self.at(from).%(_.canControlGate).num >= 3 && self.gates.num < 3) |=>
                //     1500 -> "acolyte out of start to empty land (keepers surplus — use Writhe instead)"
                // AP1 FBNoAcolytesInStart strip-via-Move REMOVED 2026-04-16:
                // bot was using normal Moves (1 power each) to strip start, leading
                // to gate loss + capture. Rely on Writhe-Move for start-clearing
                // instead (1 Writhe = many units). If start can't be cleared via
                // Writhe, accept losing the SB; preserve gate keepers instead.


            case _ =>
                evalBattle()
        }

        // nested def so it gets its own bytecode budget while sharing
        // result var and condToEval implicit via closure.
        def evalBattle() : Unit = unwrapped match {
            case AttackAction(_, r, f, _) =>
                r.chaosGate |=> -3000 -> "avoid attacking at chaos gate"
                // Combat strength adjusted for Daoloth cosmic unity (iGOO)
                val baseAttack = self.strength(self.at(r), f)
                val baseDefense = f.strength(f.at(r), self)
                val fbAttack = adjustedOwnStrengthForCosmicUnity(baseAttack, self.at(r), f.at(r), f)
                val foeDefense = adjustedOwnStrengthForCosmicUnity(baseDefense, f.at(r), self.at(r), self)
                val ghatoHereBattle = self.at(r, Ghatanothoa).any
                val hasDarkYoung = f.at(r, DarkYoung).any
                val enemyGateHere = r.enemyGate && f.gates.has(r)
                val fbUnitsHere = self.at(r).num
                // v5.17 (2026-05-14): "easy kill" — once Ghato is awakened and FB
                // has Augury, any battle where enemy combat < 3 AND FB combat
                // strength > enemy_units × 6 is a free engagement (low risk,
                // high yield). Score 10000 — beats every other battle tier.
                val ghatoAwakenedAttack = gooOnMap > 0 || game.fbGhatnothoaAwakenings >= 1
                val hasAuguryAttack = self.has(Augury)
                val easyKillAttack = ghatoAwakenedAttack && hasAuguryAttack &&
                    foeDefense < 3 && fbAttack > f.at(r).num * 6
                easyKillAttack |=> 10000 -> "easy kill: enemy combat < 3 + FB overwhelming (Ghato+Augury)"
                // Ghato + at least 1 other FB unit vs Dark Young on gate → highest battle score
                (ghatoHereBattle && hasDarkYoung && enemyGateHere && fbUnitsHere >= 2) |=> 9400 -> "Battle DY on gate (Ghato + units)"
                // Enemy GOO at FB's own gate: battle if combat isn't devastating
                val enemyGooAtOwnGate = r.ownGate && f.at(r).goos.any
                val combatNotDevastating = fbAttack >= foeDefense / 2
                // Ofinale: if enemy is about to win, attack their gates/GOOs aggressively
                val enemyFinale = ofinale(f)
                (enemyFinale && f.gates.has(r) && ghatoHereBattle) |=> 600000 -> "FINALE: attack enemy gate to prevent win"
                (enemyFinale && f.at(r).goos.any && ghatoHereBattle) |=> 700000 -> "FINALE: attack enemy GOO to prevent win"
                // Enemy GOO at own gate is URGENT — GOO can capture cultists even with
                // Rev/Desc present. Only Ghato protects against GOO capture.
                (enemyGooAtOwnGate && combatNotDevastating && fbUnitsHere >= 2) |=> 9500 -> "URGENT: battle enemy GOO at own gate"
                (enemyGooAtOwnGate && combatNotDevastating && fbUnitsHere == 1) |=> 9000 -> "URGENT: battle enemy GOO at own gate (alone)"
                (enemyGooAtOwnGate && !combatNotDevastating) |=> -2000 -> "enemy GOO too strong to battle"
                // v5.4 (2026-05-13): Ghato vs GOO at enemy gate — attack initiates when
                // Ghato is likely to survive the exchange OR FB can re-awaken Ghato
                // after the kill with 2+ power left over. Replaces the old narrow
                // "foeDefense < 4" rule that almost never fired in practice.
                //   ghatoSurvives = fb attack power >= enemy defense power (canonical)
                //   canResurrectPlus2 = power >= (1 attack) + (resurrect cost) + 2 leftover
                val ghatoResurrectCost = math.max(1, 11 - game.ritualCost)
                val ghatoSurvives = fbAttack >= foeDefense
                val canResurrectPlus2 = power >= ghatoResurrectCost + 3
                val ghatoVsGooAttack = ghatoHereBattle && f.at(r).goos.any && enemyGateHere &&
                    (ghatoSurvives || canResurrectPlus2)
                ghatoVsGooAttack |=> 9300 -> "Ghato vs GOO at enemy gate (survives or can resurrect+2)"
                // v5.8 (2026-05-13): YS Passion battle boost — replaces the old
                // gateVulnerability writhe block. Battle to take a YS gate when
                // Passion danger is present (Capture is blocked separately).
                val ysPassionBattle = f == YS && YS.exists && YS.has(Passion) &&
                    YS.gates.has(r) && YS.at(r).cultists.num > 2 && YS.power > 0 &&
                    (r.near.%(n => YS.at(n, KingInYellow).any).any ||
                     (YS.has(Hastur) && YS.has(HWINTBN)))
                (ysPassionBattle && fbUnitsHere >= 2) |=> 9200 -> "Battle YS Passion gate (take the gate)"
                // Generic battle scores
                self.at(r, RevenantOfKnaa).any |=> 1000 -> "Revenant attacks"
                self.at(r, Ghatanothoa).any |=> 1500 -> "Ghatanothoa attacks"
                (f.cultists.num > self.strength(self.at(r), f) / 2) |=> 800 -> "good attack odds"

            // Save power for ritual: end turn at 3+ gates when affordable
            // Gate diplomacy: NEVER abandon, prefer acolyte on gate
            case AbandonGateAction(_, _, _) =>
                true |=> -1000000 -> "FB: NEVER abandon gate"

            case ControlGateAction(_, r, u, _) =>
                val onGate = self.at(r).%(_.onGate)
                (onGate.none) |=> 10000 -> "FB: take control of uncontrolled gate"
                (onGate.exists(_.uclass == Acolyte) && u.uclass == Acolyte) |=> -1000000 -> "FB: acolyte already on gate"
                (onGate.exists(_.uclass == Acolyte)) |=> -500 -> "FB: don't switch off acolyte"
                (u.uclass == Acolyte && !onGate.exists(_.uclass == Acolyte)) |=> 5000 -> "FB: switch to acolyte"

            case EndTurnAction(_) =>
                val canRitual = self.gates.num >= 3 && gooOnMap > 0 && power >= game.ritualCost
                val saveForRitual = canRitual && power <= game.ritualCost + 1
                saveForRitual |=> 8000 -> "end turn: save power for doom-phase ritual"
                // Instant death: if ID is imminent, end turn to get to doom phase faster
                (instantDeathNow && canRitual) |=> 10000 -> "instant death: end turn to ritual NOW"
                // Ofinale: if enemy approaching finale, save power for ritual
                val anyFinale = others.%(ofinale).any
                (anyFinale && canRitual) |=> 9000 -> "enemy finale: end turn to ritual"
                // Balanced + pools empty + low power: stop acting, wait for doom phase
                val poolsEmpty = self.pool.cultists.none && self.pool(Desiccated).none &&
                    self.pool(RevenantOfKnaa).none && self.pool(Ghatanothoa).none
                val gatesBalanced = self.gates.num >= 3 && {
                    val gateCounts = self.gates./(gr => self.at(gr).num)
                    gateCounts.max - gateCounts.min <= 1
                }
                (poolsEmpty && gatesBalanced && power < 4) |=> 9000 -> "end turn: balanced, pools empty, low power"

            case PassAction(_) =>
                // HARD BLOCK: never pass with > 1 power
                (power > 1) |=> -15000 -> "NEVER pass with > 1 power"
                val saveForRitual = self.gates.num >= 3 && gooOnMap > 0 &&
                    power >= game.ritualCost && power <= game.ritualCost + 1
                saveForRitual |=> 7500 -> "pass: save power for ritual"
                // Balanced + pools empty: pass is fine
                val poolsEmptyP = self.pool.cultists.none && self.pool(Desiccated).none &&
                    self.pool(RevenantOfKnaa).none && self.pool(Ghatanothoa).none
                val gatesBalancedP = self.gates.num >= 3 && {
                    val gateCountsP = self.gates./(gr => self.at(gr).num)
                    gateCountsP.max - gateCountsP.min <= 1
                }
                (poolsEmptyP && gatesBalancedP && power < 4) |=> 8500 -> "pass: balanced, pools empty, low power"

            // ES REVEAL — HARD BLOCK with < 6 SBRs
            case RevealESAction(_, _, _, _) =>
                !allSB |=> -100000 -> "HARD BLOCK: never reveal ES with < 6 SBRs"

            // Play direction — copy TS logic
            case PlayDirectionAction(_, order) =>
                val fbIdx = order.indexOf(self)
                val weakest = others.sortBy(_.power).headOption
                val weakestIdx = weakest.map(order.indexOf(_)).getOrElse(-1)
                weakestIdx == order.num - 1 |=> 2000 -> "weakest player last"
                fbIdx == 0 && self.gates.num >= 3 |=> 1500 -> "go first with 3+ gates"

            // SL Demand Sacrifice — kills become pains when weaker
            case DemandSacrificeKillsArePainsAction(_) =>
                true |=> 500 -> "demand sacrifice: prefer pains over kills"

            // CC Thousand Forms — refuse when possible, competitive bidding otherwise
            case ThousandFormsAskAction(f, r, offers, _, _, _, p) =>
                r < p + offers./(_.n).sum |=> -6*6*6*6*6*6 -> "dont overpay"
                p == -1 && power >= f.power + r && !f.allSB |=> (4*4*4*4*4*4 * Math.random()).round.toInt -> "refuse pay"
                p == 0 |=> (3*3*3*3*3*3 * Math.random()).round.toInt -> "pay 0"
                p == 1 |=> (2*3*3*3*3*3 * Math.random()).round.toInt -> "pay 1"
                p == 2 |=> (2*2*3*3*3*3 * Math.random()).round.toInt -> "pay 2"
                p == 3 |=> (2*2*2*3*3*3 * Math.random()).round.toInt -> "pay 3"
                p == 4 |=> (2*2*2*2*3*3 * Math.random()).round.toInt -> "pay 4"
                p == 5 |=> (2*2*2*2*2*3 * Math.random()).round.toInt -> "pay 5"
                p == 6 |=> (2*2*2*2*2*2 * Math.random()).round.toInt -> "pay 6"

            // BG Ghroth — refuse or minimize payment
            case GhrothAskAction(_, _, _, _, _, _, n) =>
                n == -1 |=> 1000 -> "refuse"
                n == 0 |=> 1000 -> "wait"

            case GhrothTargetAction(_, c, f, _) =>
                true |=> 0 -> "ghroth target"

            // ──────────────────────────────────────────────────────────────────
            // BATTLE KILL ASSIGNMENT — based on summon cost, writhe-kill override
            // ──────────────────────────────────────────────────────────────────
            case AssignKillAction(_, _, _, u) if u.faction == FB =>
                val ghatoAwakenCost = math.max(1, 11 - game.ritualCost - infernalDiscount)
                val summonCost = u.uclass match {
                    case Acolyte => 0
                    case Desiccated => 1
                    case RevenantOfKnaa => 2
                    case HighPriest => 3
                    case Ghatanothoa => ghatoAwakenCost
                    case _ => u.uclass.cost
                }
                // Writhe-kill override: if conditions met, kill Ghato FIRST
                val ghatoKillReady = u.uclass == Ghatanothoa &&
                    game.fbGhatnothoaAwakenings >= 1 && game.fbGhatnothoaAwakenings < 3 &&
                    (game.ritualCost > 6 || idImminent)
                ghatoKillReady |=> 10000 -> "WRITHE-KILL: Ghato dies for re-awaken"
                // Lowest cost to resummon = highest priority to assign kill
                !ghatoKillReady |=> (5000 - summonCost * 500) -> ("kill by cost " + summonCost)
                (u.gateKeeper && !ghatoKillReady) |=> -3000 -> "avoid killing gate keeper"

            // BATTLE PAIN ASSIGNMENT — priority order with context
            case AssignPainAction(_, _, _, u) if u.faction == FB =>
                val battleRegion = game.battle.map(_.arena).getOrElse(u.region)
                val enemyMonsterRemains = others./~(_.at(battleRegion)).monsterly.any
                val enemyGOORemains = others./~(_.at(battleRegion)).goos.any
                val fbMonstersHere = self.at(battleRegion).%(m =>
                    m.uclass == Desiccated || m.uclass == RevenantOfKnaa).num
                val twoRevs = self.at(battleRegion, RevenantOfKnaa).num >= 2
                // Desc first if monsters remain, or rev first if 2 revs
                (u.uclass == Desiccated && fbMonstersHere >= 2) |=> 6000 -> "pain desc first (2+ monsters)"
                (u.uclass == RevenantOfKnaa && twoRevs) |=> 5500 -> "pain rev (2 revs in battle)"
                // Acolytes not on gate
                (u.uclass == Acolyte && !u.onGate) |=> 5000 -> "pain acolyte off-gate"
                // If enemy monster remains, pain on-gate acolyte before last monster
                (u.uclass == Acolyte && u.onGate && enemyMonsterRemains) |=> 4500 -> "pain on-gate aco (enemy monster remains)"
                (u.uclass == Acolyte && u.onGate) |=> 2000 -> "pain on-gate acolyte"
                (u.uclass == HighPriest) |=> 1500 -> "pain HP"
                (u.uclass == Desiccated) |=> 3000 -> "pain desiccated"
                (u.uclass == RevenantOfKnaa) |=> -1000 -> "avoid paining rev"
                // Pain Ghato last; if enemy GOO remains, Ghato should stay to fight
                (u.uclass == Ghatanothoa && enemyGOORemains) |=> -8000 -> "Ghato stays vs enemy GOO"
                (u.uclass == Ghatanothoa) |=> -5000 -> "pain Ghato last"

            // ──────────────────────────────────────────────────────────────────
            // BATTLE ELIMINATION — no retreat possible, must lose a unit
            // ──────────────────────────────────────────────────────────────────
            case EliminateNoWayAction(_, u) if u.faction == FB =>
                val ghatoAwakenCostE = math.max(1, 11 - game.ritualCost - infernalDiscount)
                val summonCostE = u.uclass match {
                    case Acolyte => 0; case Desiccated => 1; case RevenantOfKnaa => 2
                    case HighPriest => 3; case Ghatanothoa => ghatoAwakenCostE; case _ => u.uclass.cost
                }
                val ghatoKillReadyE = u.uclass == Ghatanothoa &&
                    game.fbGhatnothoaAwakenings >= 1 && game.fbGhatnothoaAwakenings < 3 &&
                    (game.ritualCost > 6 || idImminent)
                ghatoKillReadyE |=> 10000 -> "WRITHE-KILL: elim Ghato for re-awaken"
                !ghatoKillReadyE |=> (5000 - summonCostE * 500) -> ("elim by cost " + summonCostE)
                (u.gateKeeper && !ghatoKillReadyE) |=> -3000 -> "avoid elim gate keeper"

            // ──────────────────────────────────────────────────────────────────
            // BATTLE PAIN DESTINATIONS — land empty gate > water empty gate >
            // empty land > empty water > lowest enemy combat. Pain together.
            // ──────────────────────────────────────────────────────────────────
            case RetreatUnitAction(_, u, r) if u.faction == FB =>
                val isLand = r.glyph != Ocean
                val hasFBUnits = self.at(r).any
                val freeGateR = r.gate && !r.ownGate && !r.enemyGate && !r.chaosGate
                val ownGate = r.ownGate
                val isEmpty = r.foes.none
                val foeCombat = others./~(_.at(r)).%(f => f.uclass.utype == Monster || f.uclass.utype == Terror).num
                // Pain together: boost regions where FB units already retreated this battle
                hasFBUnits |=> 3000 -> "pain together with FB units"
                // v5.17 (2026-05-14): favor enemy-GOO regions (sets up future writhe-kill
                // on the GOO), land regions, and starting-glyph regions.
                val enemyGooHere = enemyGooAt(r)
                val isGlyphRegion = game.factions.exists(f => game.starting.get(f).contains(r))
                val landBoost = if (isLand) 600 else 0
                val glyphBoost = if (isGlyphRegion) 400 else 0
                val gooBoost = if (enemyGooHere) 2000 else 0
                enemyGooHere |=> gooBoost -> "pain destination has enemy GOO (set up next-AP writhe-kill)"
                isLand |=> landBoost -> "pain destination is land"
                isGlyphRegion |=> glyphBoost -> "pain destination is a starting-glyph region"
                // Destinations ranked
                (isLand && freeGateR && isEmpty) |=> 9000 -> "pain to land empty gate"
                (!isLand && freeGateR && isEmpty) |=> 8000 -> "pain to water empty gate"
                (isLand && isEmpty && !freeGateR) |=> 7000 -> "pain to empty land"
                (!isLand && isEmpty && !freeGateR) |=> 6000 -> "pain to empty water"
                (isLand && ownGate) |=> 5500 -> "pain to own land gate"
                (!isLand && ownGate) |=> 5000 -> "pain to own water gate"
                // Rank by combat: lower = better
                (foeCombat == 0) |=> 4500 -> "pain to 0 combat"
                (foeCombat <= 2) |=> 3500 -> "pain to low combat"
                (foeCombat <= 4) |=> 2500 -> "pain to moderate combat"
                (foeCombat > 4) |=> 1000 -> "pain to high combat"

            case _ =>
                evalLate()
        }

        // v5.7 (2026-05-13): eval split into two methods. JVM 64KB method-size
        // limit hits at ~1500 lines per the Faction Bot Builder Guide. The
        // trailing NeutralUnits..default cases live here as a nested def so
        // each gets its own bytecode budget while still sharing the same
        // `result` var and the `condToEval` implicit via closure.
        def evalLate() : Unit = unwrapped match {
            // ══════════════════════════════════════════════════════════════════
            // ── NEUTRAL UNIT SCORES ─────────────────────────────────────────
            // Low-range supplementary scores (500-2000). Core FB engine first.
            // Conditional on not already having another neutral unit or iGOO.
            // ══════════════════════════════════════════════════════════════════

            // ── NM: Loyalty card (2 doom cost) ──────────────────────────────
            case LoyaltyCardDoomAction(_) =>
                val hasNMCard = self.loyaltyCards.of[NeutralMonsterLoyaltyCard].any
                val hasIGOO = self.loyaltyCards.of[IGOOLoyaltyCard].any
                hasNMCard |=> -100000 -> "NU: already have NM card"
                (hasNMCard || hasIGOO) |=> -100000 -> "NU: already have neutral unit"
                self.doom < 5 |=> 4000 -> "NU: loyalty card early (low doom)"
                self.doom < 10 |=> 3000 -> "NU: loyalty card mid-game"
                true |=> 2000 -> "NU: loyalty card base"

            case NeutralMonstersAction(_, lc) =>
                val hasNMCard = self.loyaltyCards.of[NeutralMonsterLoyaltyCard].any
                val hasIGOO = self.loyaltyCards.of[IGOOLoyaltyCard].any
                hasNMCard |=> -100000 -> "NU: already have NM card"
                (hasNMCard || hasIGOO) |=> -100000 -> "NU: already have neutral unit"
                // 2026-05-11 v3: FB specializes in iGOOs (Ygolonac top). Keep NM scores
                // intentionally low so FB doesn't pick an NM card and then become unable
                // to awaken an iGOO. TS and DS cover NM diversity instead.
                true |=> 500 -> "NU: nm base (deprioritized for FB)"
                (lc.unit == Shantak)    |=> 800 -> "NU: shantak carry to gates"
                (lc.unit == Ghast)      |=> 700 -> "NU: ghast swarm"
                (lc.unit == Gnorri)     |=> 600 -> "NU: gnorri summons"
                (lc.unit == Voonith)    |=> 600 -> "NU: voonith combat"
                (lc.unit == Gug)        |=> 500 -> "NU: gug 3 combat"
                (lc.unit == StarVampire) |=> 500 -> "NU: star vampire"
                (lc.unit == DimensionalShamblerUnit) |=> 500 -> "NU: shambler"

            // ── NM: Placement of summoned NM units ──────────────────────────
            case LoyaltyCardSummonAction(_, uc, r) =>
                val ghatoHere = self.at(r, Ghatanothoa).any
                r.ownGate && ghatoHere |=> 2000 -> "NU: place NM at Ghato gate"
                r.ownGate && r.allies.cultists.any |=> 1500 -> "NU: place NM at own gate"
                r.ownGate |=> 1000 -> "NU: place NM at gate"
                true |=> 500 -> "NU: place NM anywhere"

            // ── NM: Ghast free summon ───────────────────────────────────────
            case FreeSummonAction(_, Ghast, r, _) =>
                self.at(r, Ghatanothoa).any |=> 1500 -> "NU: ghast at Ghato"
                r.ownGate |=> 1200 -> "NU: ghast at gate"
                true |=> 800 -> "NU: ghast free summon"

            // ── NM: Shantak carry cultist ───────────────────────────────────
            case ShantakCarryCultistAction(_, o, ur, r) =>
                r.ownGate && r.allies.cultists.none |=> 2000 -> "NU: shantak carry to empty gate"
                r.enemyGate && r.foes.goos.none |=> 1500 -> "NU: shantak carry to steal gate"
                true |=> 800 -> "NU: shantak carry base"

            // ── NM: Shambler deploy to map ──────────────────────────────────
            case ShamblerDeployMainAction(_, _) =>
                true |=> 1200 -> "NU: deploy shambler"

            case ShamblerDeployAction(_, r, _) =>
                r.foes.cultists.any && r.foes.goos.none && r.foes.monsterly.none |=> 2000 -> "NU: shambler to capture"
                r.ownGate && r.allies.monsterly.none |=> 1500 -> "NU: shambler defend gate"
                true |=> 800 -> "NU: shambler deploy"

            // ── NM: Shambler summon to faction card ─────────────────────────
            case ShamblerSummonMainAction(_) =>
                true |=> 1000 -> "NU: summon shambler to card"

            case ShamblerSummonAction(_) =>
                true |=> 1000 -> "NU: summon shambler"

            // ── iGOO: Awaken ────────────────────────────────────────────────
            case IndependentGOOMainAction(_, lc, _) =>
                val hasIGOO = self.loyaltyCards.of[IGOOLoyaltyCard].any
                val hasNMCard = self.loyaltyCards.of[NeutralMonsterLoyaltyCard].any
                val remaining = self.power - lc.power
                hasIGOO    |=> -100000 -> "NU: already have an iGOO"
                (hasIGOO || hasNMCard) |=> -100000 -> "NU: already have neutral unit"
                remaining < 0  |=> -99999 -> "NU: igoo: cannot afford"
                gooOnMap == 0  |=> -5000 -> "NU: igoo: awaken Ghato first"
                self.gates.num < 2 |=> -3000 -> "NU: igoo: need 2+ gates first"
                remaining >= 3 |=> 2500 -> "NU: igoo comfortable power"
                remaining >= 1 |=> 2000 -> "NU: igoo affordable"
                remaining == 0 |=> 1000 -> "NU: igoo spends all power"
                // 2026-05-11 v2: FB takes Ygolonac as primary iGOO (cheap at 2 power,
                // works with FB's swarm strategy). TS still picks Tulzscha as its top.
                // This diversifies iGOO picks across factions instead of all-Tulzscha.
                (lc == YgolonacCard) |=> 3500 -> "NU: ygolonac cheap+kills (FB primary)"
                (lc == TulzschaCard) |=> 3000 -> "NU: tulzscha undying"
                (lc == AbhothCard)   |=> 2800 -> "NU: abhoth filth"
                (lc == DaolothCard)  |=> 2800 -> "NU: daoloth gate builder"
                (lc == NyogthaCard)  |=> 2500 -> "NU: nyogtha fighter"
                (lc == ByatisCard)   |=> 2000 -> "NU: byatis anchor"
                true                 |=> 1800 -> "NU: igoo base"

            case IndependentGOOAction(_, lc, r, _) =>
                r.ownGate && self.at(r, Ghatanothoa).any |=> 900 -> "NU: igoo place: Ghato gate"
                r.ownGate && r.allies.cultists.any |=> 700 -> "NU: igoo place: own gate with cultists"
                r.ownGate                          |=> 500 -> "NU: igoo place: own gate"
                r.allies.monsterly.any             |=> 600 -> "NU: igoo place: with monsters"
                (lc.unit == Byatis) && r.ownGate  |=> 851 -> "NU: byatis at own gate (immobile)"
                (lc.unit == Byatis) && !r.ownGate |=> -500 -> "NU: byatis avoid non-gate"
                true |=> 400 -> "NU: igoo place: any"

            // ── iGOO abilities ──────────────────────────────────────────────
            case GodOfForgetfulnessMainAction(_, _, _) =>
                true |=> 1000 -> "NU: byatis pull cultists"

            case GodOfForgetfulnessAction(_, d, r) =>
                r.foes.cultists.num >= 2 |=> 1500 -> "NU: byatis pull many"
                r.foes.cultists.any |=> 1000 -> "NU: byatis pull"
                true |=> 500 -> "NU: byatis base"

            case FilthMainAction(_, _) =>
                true |=> 700 -> "NU: abhoth filth"

            case FilthAction(_, r) =>
                r.enemyGate |=> 1200 -> "NU: filth at enemy gate"
                true |=> 500 -> "NU: filth base"

            case NightmareWebMainAction(_, _) =>
                true |=> 800 -> "NU: nyogtha nightmare web"

            case NightmareWebAction(_, r) =>
                r.ownGate |=> 1500 -> "NU: nightmare web at gate"
                r.foes.any |=> 1000 -> "NU: nightmare web near enemies"
                true |=> 500 -> "NU: nightmare web base"

            case TulzschaGivePowerMainAction(_) =>
                !allSB |=> 1200 -> "NU: tulzscha give power for SB"
                true |=> 400 -> "NU: tulzscha give power"

            case TulzschaGivePowerAction(_) =>
                true |=> 700 -> "NU: tulzscha confirm"

            case CeremonyOfAnnihilationChoiceAction(_) =>
                power <= 2 |=> 1200 -> "NU: ceremony, need power"
                true |=> 600 -> "NU: ceremony base"

            // ── HP: Sacrifice out of turn (Unspeakable Oath) ────────────────
            case SacrificeHighPriestOutOfTurnMainAction(_) =>
                others.%(_.active).none |=> 2000 -> "NU: HP oath, enemies exhausted"
                power <= 1 |=> 1500 -> "NU: HP oath low power"
                true |=> 800 -> "NU: HP out-of-turn sacrifice"

            // ══════════════════════════════════════════════════════════════════
            // ── END NEUTRAL UNIT SCORES ──────────────────────────────────────
            // ══════════════════════════════════════════════════════════════════
            case _ =>
                true |=> 0 -> "default"
        }

        result.none |=> 0 -> "none"
        true |=> (math.random() * 4).round.toInt -> "random"

        result
    }
}
