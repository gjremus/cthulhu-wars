package cws

import hrf.colmat._

object BotTT extends BotX(implicit g => new GameEvaluationTT)

class GameEvaluationTT(implicit game : Game) extends GameEvaluation(TT)(game) {

    def eval(a : Action) : $[Evaluation] = {
        var result : $[Evaluation] = $

        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        val unwrapped = a.unwrap
        result ++= evalMain(unwrapped)
        result ++= fbPromptedEvals(a)

        result.none |=> 0 -> "#TT none"
        true |=> (math.random() * 4).round.toInt -> "#TT random"

        result.sortBy(v => -v.weight.abs)
    }

    def evalMain(a : Action) : $[Evaluation] = {
        var result : $[Evaluation] = $

        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        val firstAP  = game.turn == 1
        val secondAP = game.turn == 2

        val numGates  = self.gates.num
        val numHPs    = self.all(HighPriest).onMap.num
        val numSB     = self.spellbooks.num
        val ubboUp    = self.onMap(UbboSathla).any

        // Detect neutral monsters/terrors and 2/4-power iGOOs in game setup
        val hasNMorNT = game.loyaltyCards.exists(c =>
            c.isInstanceOf[NeutralMonsterLoyaltyCard] || c.isInstanceOf[NeutralTerrorLoyaltyCard]
        )
        val has24IGOO = game.loyaltyCards.exists(c =>
            c.isInstanceOf[IGOOLoyaltyCard] &&
            (c.asInstanceOf[IGOOLoyaltyCard].power == 2 || c.asInstanceOf[IGOOLoyaltyCard].power == 4)
        )

        a @@ {

            // ── STARTING REGION — distant start (FB pattern) ─────────────────
            case StartingRegionAction(_, r) =>
                val near1Occupied = r.near.%(nr => game.starting.values.$.has(nr)).num
                val near2Occupied = r.near2.%(nr => game.starting.values.$.has(nr)).num
                val startScore =
                    if (r.glyph == Ocean)       -3000
                    else if (near1Occupied >= 2) -2000
                    else if (near1Occupied >= 1) -1000
                    else if (near2Occupied == 0)  7000
                    else                          6500
                true |=> startScore -> ("start region tier " + startScore)

            // ── TRIBE SELECTION ──────────────────────────────────────────────
            // Sarkomand if both a 2/4-power iGOO AND a neutral monster/terror in game.
            // Otherwise randomly Leng or Tsang (equal scores with tiebreaker randomness).
            case TTChooseTribeAction(_, TribeSarkomand) =>
                (has24IGOO && hasNMorNT) |=> 30 -> "#TT tribe-sarkomand"
                (!has24IGOO || !hasNMorNT) |=> -100000 -> "#TT tribe-sarkomand-no-igoo"

            case TTChooseTribeAction(_, TribeLeng) =>
                (!has24IGOO || !hasNMorNT) |=> 20 -> "#TT tribe-leng"
                (has24IGOO && hasNMorNT)   |=> -100000 -> "#TT tribe-leng-sarkomand-game"

            case TTChooseTribeAction(_, TribeTsang) =>
                (!has24IGOO || !hasNMorNT) |=> 20 -> "#TT tribe-tsang"
                (has24IGOO && hasNMorNT)   |=> -100000 -> "#TT tribe-tsang-sarkomand-game"

            // ── SPELLBOOKS — priority order ──────────────────────────────────
            // Shared (all tribes): Hierophants > Soulless > TerrorSB
            // Then tribe-specific:
            //   Tsang: Idolatry > Martyrdom > Tablets
            //   Leng: Surprise! > DarkRituals > Fulmination
            //   Sarkomand: Doomsday > OtherworldAlliances > Inerrant
            case SpellbookAction(_, sb, _) => sb match {
                case Hierophants            => true |=> 1200 -> "#TT sb-hierophants"
                case Soulless               => true |=>  900 -> "#TT sb-soulless"
                case TerrorSB               => true |=>  700 -> "#TT sb-terror"
                case Idolatry               => true |=>  600 -> "#TT sb-idolatry"
                case Martyrdom              => true |=>  500 -> "#TT sb-martyrdom"
                case TabletsOfTheGods       => true |=>  400 -> "#TT sb-tablets"
                case SurpriseSB             => true |=>  600 -> "#TT sb-surprise"
                case DarkRituals            => true |=>  500 -> "#TT sb-dark-rituals"
                case Fulmination            => true |=>  400 -> "#TT sb-fulmination"
                case Doomsday               => true |=>  600 -> "#TT sb-doomsday"
                case OtherworldAlliances    => true |=>  500 -> "#TT sb-otherworld"
                case Inerrant               => true |=>  400 -> "#TT sb-inerrant"
                case _ => true |=> -1000 -> "#TT sb-unknown"
            }

            // ── RITUAL ───────────────────────────────────────────────────────
            case RitualAction(_, cost, _) =>
                instantDeathNow |=> 10000 -> "#TT instant death now"
                instantDeathNext && allSB && others.all(!_.allSB) |=> 10000 -> "#TT ritual if ID next and all SB"
                instantDeathNext && !allSB && others.%(_.allSB).any |=> -1000 -> "#TT dont ritual if ID next not all SB"
                instantDeathNext && !allSB && others.all(!_.allSB) && realDoom < others./(_.aprxDoom).max |=> 900 -> "#TT ritual so ID next nobody wins"
                allSB && realDoom + maxDoomGain >= 30 |=> 900 -> "#TT can break 30 all SB"
                !allSB && self.doom + self.gates.num >= 30 |=> -5000 -> "#TT will break 30 not all SB"
                !allSB && self.doom + self.gates.num < 30 && realDoom <= 29 && realDoom + maxDoomGain >= 29 |=> 700 -> "#TT wont break 30 come near"
                // Ritual if on 3 gates in doom phase
                (game.doomPhase && numGates >= 3) |=> 850 -> "#TT ritual 3-gate doom phase"
                // Ritual if aprxDoom > 20 and on 2+ gates
                (self.aprxDoom > 20 && numGates >= 2) |=> 800 -> "#TT ritual aprx-doom>20 2gates"
                firstAP |=> -100000 -> "#TT NEVER ritual first doom phase"
                numSB >= 2 && aprxDoomGain / cost > 1 |=> 600 -> "#TT very sweet deal"
                numSB >= 3 && aprxDoomGain / cost > 0.75 |=> 400 -> "#TT sweet deal"
                numSB >= 4 && aprxDoomGain / cost > 0.5 |=> 200 -> "#TT ok deal"
                true |=> -250 -> "#TT dont ritual unless reasons"

            // ── HELL'S BANQUET: always fire (ForcedAction, but score high to confirm) ──
            case TTHellsBanquetRollAction(_) =>
                result +:= Evaluation(50, "#TT hellsbanquet")

            // ── DOOM-PHASE AWAKEN: always take free Ubbo awaken ─────────────
            case TTAwakenUbboDoomMainAction(_) =>
                result +:= Evaluation(100000, "#TT awaken-ubbo-free")

            // ── AWAKEN UBBO (action phase, cost 6) ─────────────────────────
            case TTAwakenUbboSathlaChooseHPAction(_, r, cost) =>
                (cost == 0) |=> 100000 -> "#TT awaken-ubbo-free"
                // Choose gate with fewest units (prefer least-occupied gate)
                val unitsAtR = self.at(r).num + others./~(_.at(r)).num
                (cost > 0 && numGates >= 2) |=> (10000 - unitsAtR * 10) -> "#TT awaken-ubbo-gate-fewest"
                (cost > 0 && numGates == 1) |=> 8000 -> "#TT awaken-ubbo-1gate"

            case TTAwakenUbboSathlaEliminateHPAction(_, _, _, cost) =>
                (cost == 0) |=> 100000 -> "#TT awaken-ubbo-elim-hp-free"
                (cost > 0) |=> 5000 -> "#TT awaken-ubbo-elim-hp"

            case TTAwakenUbboSathlaAction(_, r, cost) =>
                (cost == 0) |=> 100000 -> "#TT awaken-ubbo-place-free"
                // Choose gate with fewest units
                val unitsAtR = self.at(r).num + others./~(_.at(r)).num
                (cost > 0) |=> (8000 - unitsAtR * 10) -> "#TT awaken-ubbo-place"

            // ── GATE-BUILDING — AP1: build 2nd gate, AP2+: build 3rd ────────
            case BuildGateAction(_, r) =>
                hasFBCrater(r) |=> -8000 -> "#TT no build on FB crater"
                (active.none && firstAP && numGates < 2)  |=> (10 * 100000 / 9) -> "#TT AP1 safe build gate"
                (active.none && secondAP && numGates < 3) |=> (10 * 100000 / 9) -> "#TT AP2 safe build 3rd gate"
                (active.none && !firstAP && !secondAP && numGates < 4) |=> (8 * 100000 / 9) -> "#TT build gate"
                firstAP && numGates < 2 |=> 5000 -> "#TT AP1 need 2nd gate"
                secondAP && numGates < 3 |=> 4000 -> "#TT AP2 need 3rd gate"

            // ── RECRUIT: acolyte (standard GC pattern) + HP if <2 on map ───
            case RecruitAction(_, Acolyte, r) =>
                hasFBCrater(r) |=> -5000 -> "#TT no recruit at FB crater"
                active.none && r.freeGate |=> (3 * 100000 / 1) -> "#TT safe recruit get gate"
                active.none && numGates < 3 && r.noGate && power > 3 && r.allies.cultists.none |=> (1 * 100000 / 1) -> "#TT safe recruit"
                active.none && power > 1 && r.near.%(_.freeGate).any |=> (3 * 100000 / 2) -> "#TT safe recruit near gate"
                active.none && r.ownGate |=> (8 * 100000 / 9) -> "#TT safe recruit own gate"
                r.freeGate |=> 330 -> "#TT free gate"
                r.ownGate |=> 200 -> "#TT own gate"

            case RecruitAction(_, HighPriest, r) =>
                hasFBCrater(r) |=> -5000 -> "#TT no recruit hp at FB crater"
                // Recruit HP at new gate (first gate with no HP yet, post-gate-build)
                (numHPs < 2 && r.ownGate && self.at(r, HighPriest).none) |=> 50000 -> "#TT recruit hp new gate <2 HPs"
                (numHPs < 2 && r.ownGate) |=> 20000 -> "#TT recruit hp own gate <2 HPs"
                (numHPs >= 2) |=> -100000 -> "#TT inactivated hp 2+ on map"

            // ── MOVE ─────────────────────────────────────────────────────────
            case MoveAction(_, u, o, d, _) if u.uclass == Acolyte =>
                fbMoveAvoidance(d).foreach(e => true |=> e)
                active.none && o.ownGate && o.allies.cultists.num == 1 |=> -200000 -> "#TT gatekeeper"
                // TT: penalise gate-to-gate shuffle (no-op gate control switching)
                o.ownGate && d.ownGate |=> -800 -> "#TT no gate-to-gate shuffle"
                // TT: do not move to a gate where control is blocked
                d.gate && gateControlBlocked(d) |=> -1000000 -> "#TT gate control blocked at dest"
                // AP1: move cultist to new gate after building it
                (firstAP && d.ownGate && o.ownGate.not) |=> (2 * 100000 / 1) -> "#TT AP1 move cultist to new gate"
                active.none && d.freeGate |=> (2 * 100000 / 1) -> "#TT safe move get gate"
                active.none && d.noGate && power > 3 |=> (2 * 100000 / 4) -> "#TT safe move build gate"
                d.allies.goos.any |=> 30 -> "#TT goo will protect"

            case MoveAction(_, u, o, d, _) if u.uclass == HighPriest =>
                fbMoveAvoidance(d).foreach(e => true |=> e)
                // TT: keep HP on its gate; penalise leaving or pointless gate-to-gate moves
                u.gateKeeper |=> -500 -> "#TT HP gatekeeper dont move"
                o.ownGate && d.ownGate |=> -800 -> "#TT HP no gate-to-gate shuffle"
                // TT: do not move HP to a gate where control is blocked
                d.gate && gateControlBlocked(d) |=> -1000000 -> "#TT HP gate control blocked at dest"
                d.freeGate |=> 200 -> "#TT HP claim free gate"
                d.ownGate && d.allies.%(_.canControlGate).none |=> 300 -> "#TT HP guard unguarded gate"

            case MoveAction(_, u, o, d, _) if u.uclass == UbboSathla =>
                fbMoveAvoidance(d).foreach(e => true |=> e)
                d.enemyGate && others.%(ofinale).%(f => f.at(d).goos.any).any |=> 550000 -> "#TT ubbo vs finale goo"
                d.enemyGate && others.%(ofinale).any |=> 500000 -> "#TT ubbo vs finale gate"
                d.ownGate && d.foes.goos.active.any |=> 11000 -> "#TT ubbo defend own gate"
                active.none && d.freeGate |=> (5 * 100000 / 3) -> "#TT ubbo free gate"

            // ── ATTACK ───────────────────────────────────────────────────────
            case AttackAction(_, r, f, _) =>
                val allies = self.at(r)
                val foes   = f.at(r)
                val ownStr = self.strength(allies, f)
                val eneStr = f.strength(foes, self)
                allies.goos.any && foes.goos.none && f.gates.contains(r) && ofinale(f) |=> 600000 -> "#TT finale gate attack"
                allies.goos.any && foes.goos.any && ofinale(f) |=> 700000 -> "#TT finale goo attack"
                allies.goos.any && ownStr >= 6 && (foes.goos.any || f.gates.contains(r)) |=> 250 -> "#TT pound"
                self.acted && foes.goos.none |=> -100000 -> "#TT unlimited battle only vs goos"

            // ── CAPTURE ──────────────────────────────────────────────────────
            case CaptureAction(_, r, f, _) =>
                val safe = active.none
                safe && !r.gateOf(f) |=> (1 * 100000 / 1) -> "#TT safe capture"
                safe && r.gateOf(f) && r.of(f).%(_.canControlGate).num == 1 |=> (2 * 100000 / 1) -> "#TT safe capture open gate"
                ofinale(f) && f.gates.contains(r) |=> 600000 -> "#TT finale capture"
                !f.has(Passion) |=> 1600 -> "#TT capture"

            // ── UNSPEAKABLE OATH ─────────────────────────────────────────────
            case TTUnspeakableOathMainAction(_) =>
                (self.power < 3 && self.all(HighPriest).onMap.any) |=> 20 -> "#TT oath-low-power"
                (self.power >= 3) |=> 5 -> "#TT oath-ok-power"

            case TTUnspeakableOathAction(_, _) =>
                result +:= Evaluation(15, "#TT oath-sacrifice")

            // ── DARK RITUALS (Leng) ───────────────────────────────────────────
            case TTDarkRitualsMainAction(_) =>
                (!TTExpansion.darkRitualsFlipped) |=> 30 -> "#TT dark-rituals"

            case TTDarkRitualsPayPowerAction(_, _) =>
                result +:= Evaluation(10, "#TT dark-rituals-pay-power")

            case TTDarkRitualsPay2DoomAction(_, _) =>
                result +:= Evaluation(8, "#TT dark-rituals-pay-doom")

            // ── SURPRISE! (Leng) ─────────────────────────────────────────────
            case TTSurpriseMainAction(_) =>
                (self.pool(ProtoShoggoth).any && self.power >= 2) |=> 25 -> "#TT surprise"

            case TTSurpriseTargetFactionAction(_, target) =>
                // Prefer factions near finale or with many cultists
                ofinale(target) |=> 500 -> "#TT surprise finale target"
                true |=> 10 -> "#TT surprise target"

            case TTSurpriseAcolyteChoiceAction(_, _, uref) =>
                val u = game.unit(uref)
                u.region.ownGate |=> -200 -> "#TT surprise spare gate acolyte"
                self.gates.has(u.region) |=> -500 -> "#TT surprise spare gatekeeper"
                !u.region.ownGate |=> 20 -> "#TT surprise off-gate acolyte"

            // ── IDOLATRY (Tsang) ─────────────────────────────────────────────
            case TTIdolatryMainAction(_) =>
                (self.pool(Acolyte).any && self.gates.any) |=> 25 -> "#TT idolatry"

            case TTIdolatryChooseDestAction(_, targets, _) =>
                targets.foreach { r =>
                    val hasEnemy = game.factions.but(self).exists(_.at(r).any)
                    hasEnemy |=> 10 -> "#TT idolatry-defend"
                    r.ownGate |=> 15 -> "#TT idolatry-own-gate"
                    true |=> 5 -> "#TT idolatry-place"
                }

            case TTIdolatryChooseSourceAction(_, dest, pool) =>
                true |=> 5 -> "#TT idolatry-source"

            case TTIdolatryChooseUnitAction(_, dest, src, pool) =>
                true |=> 10 -> "#TT idolatry-add-unit"

            case TTIdolatryAddUnitAction(_, _, _, _, _) =>
                result +:= Evaluation(10, "#TT idolatry-add-unit")

            case TTIdolatryDoneAction(_, _, pool) =>
                result +:= Evaluation(if (pool.any) 10 else 0, "#TT idolatry-done")

            case TTIdolatryCancelAction(_) =>
                result +:= Evaluation(0, "#TT idolatry-cancel")

            case TTIdolatryUndoLastAction(_, _, _, _) =>
                result +:= Evaluation(0, "#TT idolatry-undo")

            case TTIdolatryCancelSourceAction(_, _, _, _) =>
                result +:= Evaluation(0, "#TT idolatry-cancel-src")

            // ── DOOMSDAY (Sarkomand) ──────────────────────────────────────────
            case TTDoomsdayMainAction(_) =>
                val leadCount = game.factions.but(self).count(e => self.doom > e.doom)
                (leadCount >= 2) |=> 40 -> "#TT doomsday-strong"
                (leadCount >= 1) |=> 20 -> "#TT doomsday-ok"
                true |=> 15 -> "#TT doomsday"

            case TTDoomsdaySelectIGOOAction(_, card) =>
                // Prefer cost-4 iGOO (stronger combat)
                card.asInstanceOf[IGOOLoyaltyCard].power == 4 |=> 20 -> "#TT doomsday-4-igoo"
                card.asInstanceOf[IGOOLoyaltyCard].power == 2 |=> 10 -> "#TT doomsday-2-igoo"

            case TTDoomsdayPlaceAction(_, _, r) =>
                // Choose gate with fewest units
                val unitsAtR = self.at(r).num + others./~(_.at(r)).num
                true |=> (1000 - unitsAtR * 10) -> "#TT doomsday-gate-fewest"

            // ── REMOVE GATE (SBR trigger) ─────────────────────────────────────
            case TTRemoveGateMainAction(_) =>
                self.needs(TTRemoveControlledGate) |=> 35 -> "#TT remove-gate-sbr"

            case TTRemoveGateAction(_, _) =>
                result +:= Evaluation(25, "#TT remove-gate")

            // ── SYCOPHANCY (prompted choices) ─────────────────────────────────
            case TTSycophancyGiveDoomAction(_, _, _) =>
                // Give 1 doom to TT if ritualer is leading
                others.%(ofinale).any |=> 500 -> "#TT sycophancy-give-finale"
                true |=> 10 -> "#TT sycophancy-give"

            case TTSycophancyLoseDoomAction(_, _, _) =>
                true |=> 5 -> "#TT sycophancy-lose"

            // ── HIEROPHANTS HP PLACEMENT ────────────────────────────────────
            case TTHierophantsChooseGateAction(_, r, _) =>
                // Place at gate with fewest units (our own or other factions')
                val unitsAtR = self.at(r).num + others./~(_.at(r)).num
                true |=> (1000 - unitsAtR * 10) -> "#TT hierophants-gate-fewest"

            case TTHierophantsOtherFactionGateAction(_, _, r, _) =>
                // Other faction HP: place at their gate with fewest units
                val unitsAtR = self.at(r).num + others./~(_.at(r)).num
                true |=> (1000 - unitsAtR * 10) -> "#TT hierophants-other-gate"

            // ── TERROR ────────────────────────────────────────────────────────
            case TTTerrorReduceEnemyAction(_, n) =>
                result +:= Evaluation(n * 100, "#TT terror-reduce-enemy")

            case TTTerrorBoostOwnAction(_, n) =>
                result +:= Evaluation(n * 80, "#TT terror-boost-own")

            // ── FULMINATION (Leng) ────────────────────────────────────────────
            case TTFulminationTakeAction(_, kills) =>
                (kills >= 3) |=> 1000 -> "#TT fulmination-take-good"
                (kills >= 1) |=> 300 -> "#TT fulmination-take-ok"

            case TTFulminationDeclineAction(_) =>
                result +:= Evaluation(5, "#TT fulmination-decline")

            // ── STANDARD ACTION CHOICES ──────────────────────────────────────
            case DoomDoneAction(_) =>
                true |=> 10 -> "#TT doom done"

            case PassAction(_) =>
                true |=> -500 -> "#TT wasting power bad"

            case MoveDoneAction(_) =>
                true |=> 1000 -> "#TT move done"
                active.none |=> 5000000 -> "#TT move done safe"

            case EndTurnAction(_) =>
                self.battled.any |=> 20000 -> "#TT unlimited battle drains power"
                others.%(ofinale).any |=> 666000 -> "#TT extend finale"
                true |=> 500 -> "#TT main done"

            case ControlGateAction(_, r, u, _) =>
                // [2026-06-02 v2] Belt-and-braces with the candidate-level lockout
                // filter in BotX.askE (see BotGateLockout header comment). The
                // previous unconditional +1,000,000 baseline conflicted with
                // any negative penalty in sortByAbs/compareEL — both signs
                // had equal absolute value, +1M won the lex tiebreak, and the
                // bot kept swapping. v2 only awards the +1M when this is NOT
                // a switch (i.e. controlling a previously-empty gate). Real
                // swaps get a single negative score; the candidate filter
                // drops the action outright on the second event this turn.
                val currentlyOnGate = self.at(r).%(_.onGate)
                val isSwitch = currentlyOnGate.any && !currentlyOnGate.exists(_.ref == u)
                val swapHPforAco = isSwitch && currentlyOnGate.exists(_.uclass == HighPriest) && u.uclass == Acolyte
                !isSwitch |=> 1000000 -> "#TT control empty gate"
                // Allow ONE legit setup swap: HP off the gate, Acolyte on (mirrors BotTS Bug 69).
                swapHPforAco |=> 5000 -> "#TT swap HP off gate, acolyte controls"
                (isSwitch && !swapHPforAco) |=> -1000000 -> "#TT no-op swap (matches remain calm)"

            case AbandonGateAction(_, _, _) =>
                true |=> -1000000 -> "#TT never abandon gate"

            case RetreatUnitAction(_, u, r) =>
                u.cultist && r.allies.goos.any |=> 2000 -> "#TT retreat cultist to goo"
                u.cultist && r.foes.goos.any |=> -1500 -> "#TT dont retreat cultist to enemy goo"
                u.cultist && r.ownGate |=> 100 -> "#TT retreat cultist to own gate"
                u.cultist && r.freeGate |=> 4000 -> "#TT retreat cultist to free gate"
                u.cultist && r.foes.none && !r.gate |=> 200 -> "#TT retreat cultist to safety"
                u.goo && r.allies.%(_.capturable).any |=> 1000 -> "#TT retreat goo to protect"
                u.goo && r.ownGate |=> 400 -> "#TT retreat goo to own gate"
                true |=> r.connected.distinct.num -> "#TT reachable regions"

            case _ =>
        }

        // Battle scoring
        if (game.battle.any && game.battle./~(_.sides).has(self)) {
            implicit val battle = game.battle.get
            val allies  = self.forces
            val enemies = self.opponent.forces

            a match {
                case AssignKillAction(_, _, _, u) =>
                    u.is(Acolyte)      |=> 400 -> "#TT elim aco"
                    u.is(ProtoShoggoth)|=> 200 -> "#TT elim proto"
                    u.is(HighPriest)   |=> 100 -> "#TT elim hp"
                    u.is(UbboSathla)   |=> 50  -> "#TT elim ubbo"

                case AssignPainAction(_, _, _, u) =>
                    u.is(UbboSathla)   |=> 800 -> "#TT pain ubbo away"
                    u.is(ProtoShoggoth)|=> 300 -> "#TT pain proto"
                    u.is(HighPriest)   |=> 200 -> "#TT pain hp"
                    u.is(Acolyte)      |=> 100 -> "#TT pain aco"

                case _ =>
            }
        }

        result
    }
}
