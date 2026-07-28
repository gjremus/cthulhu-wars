package cws

import hrf.colmat._

object BotDC extends BotX(implicit g => new GameEvaluationDC)

class GameEvaluationDC(implicit game : Game) extends GameEvaluation(DC)(game) {
    def eval(a : Action) : $[Evaluation] = {
        var result : $[Evaluation] = $

        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        // ── Sin ceiling awareness (guide Task 3.6.3, 2026-07-27) ─────────────
        // Sin is capped at game.dcSinCap (= 2 × Ritual Marker, §1.5.2). Every
        // grant routes through game.grantDCSin, which silently drops the
        // overflow. The bot had no notion of the cap anywhere, so it valued Sin
        // it could not actually receive. headroom is the Sin still bankable.
        val sinHeadroom = math.max(0, game.dcSinCap - game.dcSin)
        val sinAtCap    = sinHeadroom == 0

        // ── Reachability note (guide Task 3.6.3, 2026-07-27) ─────────────────
        // Explode.explode drops every Soft action from the bot's candidate list
        // and keeps only what the Soft menus expand INTO. DC's five ability
        // buttons (Tenebrosum / Satiate / Lure / Pilgrimage / Dark Bargain) are
        // all Soft, so scoring them alone never reached the picker and the
        // reachable confirm/pick steps fell into the -1000 "unknown" catch-all
        // — a bot DC never used any of its abilities. The Soft scores below are
        // kept (harmless, and they document intent) and each reachable step is
        // now scored to match, mirroring how BotTB scores both halves of the
        // Thousand Writhing Maws chain.

        result ++= fbPromptedEvals(a)

        a.unwrap match {
            case MoveAction(_, u, from, to, _) =>
                fbMoveAvoidance(to).foreach(e => true |=> e)
            case BuildGateAction(_, r) =>
                hasFBCrater(r) |=> -8000 -> "cannot build gate on FB crater"
            case _ =>
        }

        if (game.battle.none) {
            a match {

                case FirstPlayerAction(_, f) =>
                    f == self && allSB                  |=> 100 -> "play first all SB"
                    f == self                           |=> -50 -> "stall"

                case PlayDirectionAction(_, order) =>
                    (order == game.factions)            |=> 20  -> "natural order"

                case DoomDoneAction(f) =>
                    f == self                           |=> 0   -> "done doom"

                case MoveAction(f, u, from, to, _) =>
                    u.cultist && to.freeGate            |=> 1600 -> "cultist to free gate"
                    u.cultist && to.ownGate             |=> 200  -> "cultist to own gate"
                    u.monster && to.ownGate             |=> 300  -> "monster to own gate"
                    u.goo && to.ownGate                 |=> 500  -> "goo to own gate"
                    to.foes.any && self.power > 3       |=> 800  -> "move toward enemies"
                    to.freeGate                         |=> 400  -> "move to free gate"

                case RecruitAction(f, uc, r) =>
                    uc == Acolyte && r.freeGate         |=> 600 -> "acolyte to free gate"

                case SummonAction(f, uc, r) =>
                    r.ownGate                           |=> 200 -> "summon at own gate"
                    r.foes.any                          |=> 100 -> "summon near foes"
                    uc == MindlessHusk                  |=> 50  -> "summon mindless husk"
                    uc == FallenProphet                 |=> 100 -> "summon fallen prophet"

                case BuildGateAction(f, r) =>
                    self.gates.num < 3                  |=> 800 -> "need more gates"
                    self.doom < 10                      |=> 600 -> "build gate for doom"

                case AttackAction(f, r, e, _) =>
                    self.strength(self.at(r), e) > e.strength(e.at(r), self) |=> 1200 -> "favorable battle"
                    e.at(r).goos.any                    |=> 800 -> "attack enemy goo"

                case AwakenAction(f, YgolonacDC, r, _) =>
                    self.needs(DarkBargainReq)          |=> 2000 -> "awaken Y'Golonac for SBR"
                    self.spellbooks.num <= 2            |=> 1500 -> "cheap awaken"
                    r.ownGate                           |=> 300  -> "awaken at own gate"

                case DCSatiateMainAction(f) =>
                    self.power >= 2                     |=> 1100 -> "use satiate"

                // Reachable step behind the Soft Satiate button. Satiate eats a
                // Cultist in Y'Golonac's area and pays Elder Signs, so it stays
                // worth doing even with Sin at the ceiling.
                case DCSatiateConfirmAction(f) =>
                    self.power >= 2                     |=> 1100 -> "commit satiate"

                case DCLureMainAction(f) =>
                    self.power >= 1                     |=> 900  -> "use lure"

                case DCLureConfirmAction(f) =>
                    self.power >= 1                     |=> 900  -> "commit lure"

                case DCPilgrimageMainAction(f) =>
                    self.power >= 1                     |=> 700  -> "use pilgrimage"

                // Prophet pick / destination pick behind the Soft Pilgrimage
                // button. Prefer a destination that gains ground.
                case DCPilgrimageProphetAction(_, _) =>
                    true                                |=> 700  -> "pick pilgrimage prophet"

                case DCPilgrimageDestAction(_, _, dest) =>
                    dest.freeGate                       |=> 900  -> "pilgrimage to free gate"
                    dest.ownGate                        |=> 500  -> "pilgrimage to own gate"
                    !dest.freeGate && !dest.ownGate     |=> 300  -> "pilgrimage destination"

                case DCPilgrimageUnitMoveAction(_, _, _, _, _) =>
                    true                               |=> 800  -> "move unit via pilgrimage"

                case DCPilgrimageDoneAction(_, _, _) =>
                    true                               |=> 100  -> "done pilgrimage"

                case DCDarkBargainMainAction(f) =>
                    true                                |=> 600  -> "use dark bargain"

                // Dark Bargain costs 0 Power and hands enemies Power equal to
                // the face DC takes, so with Sin at the ceiling it is pure
                // downside — DC gains nothing and every enemy gets paid.
                case DCDarkBargainConfirmAction(f) =>
                    !sinAtCap                           |=> 600  -> "commit dark bargain"
                    sinAtCap                            |=> -2000 -> "dark bargain would only feed enemies (Sin at cap)"

                // Sin above the ceiling is discarded, so a face larger than the
                // remaining headroom still hands out its full Power to enemies
                // while DC banks only the headroom. Value the Sin actually
                // received, and penalise the wasted overflow.
                case DCDarkBargainChooseSinAction(f, face) =>
                    val dbGain  = math.min(face, sinHeadroom)
                    val dbWaste = face - dbGain
                    (dbGain > 0)                        |=> dbGain * 200 -> "dark bargain sin value"
                    (dbWaste > 0)                       |=> dbWaste * -150 -> "dark bargain sin over cap is lost"

                // ── Tenebrosum (spend Sin to repeat the last action) ──────────
                // Was entirely unscored, so it landed in the -1000 "unknown"
                // catch-all and a bot DC never spent Sin on a repeat — its
                // signature ability went unused. Spending banked Sin is what Sin
                // is FOR, and spending it also frees headroom under the ceiling,
                // so this is scored generously when Sin is plentiful.
                case DCTenebrosumMainAction(f, cost, _) =>
                    game.dcSin >= cost                  |=> 1000 -> "repeat action with Tenebrosum"
                    sinAtCap                            |=> 400  -> "spend Sin sitting at the cap"

                case DCTenebrosumRepeatAction(f, cost, _) =>
                    game.dcSin >= cost                  |=> 1000 -> "commit Tenebrosum repeat"
                    sinAtCap                            |=> 400  -> "spend Sin sitting at the cap"

                case DCProselytizeReqOptInAction(f) =>
                    self.needs(ProselytizeReq)          |=> 1500 -> "take Proselytize for SBR"

                case DCProselytizeSelectFactionAction(_, _, _, _, _, _, target, _) =>
                    (target != self)                    |=> 1000 -> "drag enemy cultist"
                    (target == self)                    |=> -5000 -> "never drag own cultist"

                case DCProselytizeFactionSelectDoneAction(_, _, _, _, selected, _) =>
                    selected.any                       |=> 500 -> "done selecting factions"
                    selected.none                      |=> -500 -> "done with no factions"

                case DCProselytizeAssignToFactionAction(_, _, _, _, _, _, target, _) =>
                    (target != self)                    |=> 800 -> "assign extra drag to enemy"

                case DCProselytizeEnemyPickAction(_, _, _, cultistRef, _, _, _) =>
                    !game.unit(cultistRef).onGate       |=> 800 -> "sacrifice off-gate cultist"
                    game.unit(cultistRef).onGate        |=> 200 -> "sacrifice on-gate cultist"

                case DCSatiateReqOptInAction(f) =>
                    self.needs(SatiateReq)              |=> 1500 -> "take Satiate for SBR"

                case DCPlaceReservedAcolyteAction(f, r) =>
                    r.freeGate                          |=> 800 -> "place reserved acolyte at free gate"
                    r.ownGate                           |=> 200 -> "place reserved acolyte at own gate"

                case EndTurnAction(f) =>
                    f == self                           |=> 0 -> "end turn"

                case ControlGateAction(_, r, u, _) =>
                    val currentlyOnGate = self.at(r).%(_.onGate)
                    val isSwitch = currentlyOnGate.any && !currentlyOnGate.exists(_.ref == u)
                    !isSwitch                           |=> 1000 -> "control empty gate"
                    isSwitch                            |=> -1000000 -> "no-op swap (lockout in BotX.askE)"

                case AbandonGateAction(_, r, _) =>
                    true |=> -1000000 -> "never abandon gate"

                case _ =>
                    true |=> -1000 -> "unknown"
            }
        }
        else {
            implicit val battle = game.battle.get
            val allies  = self.forces
            val enemies = self.opponent.forces

            def elim(u : UnitFigure) {
                u.is(Acolyte)       |=> 800  -> "elim acolyte"
                u.is(MindlessHusk)  |=> 200  -> "elim husk"
                u.is(FallenProphet) |=> 400  -> "elim prophet"
                u.goo               |=> -500 -> "dont elim goo"
            }

            def retreat(u : UnitFigure) {
                u.gateKeeper        |=> -1000 -> "dont retreat gate keeper"
                u.is(Acolyte)       |=> 800   -> "retreat acolyte"
                u.is(MindlessHusk)  |=> 400   -> "retreat husk"
                u.is(FallenProphet) |=> 500   -> "retreat prophet"
                u.goo               |=> -5000 -> "dont retreat goo"
            }

            a match {
                case AssignKillAction(_, _, _, u) => elim(u)
                case AssignPainAction(_, _, _, u) => retreat(u)
                case RetreatUnitAction(_, u, r) =>
                    r.ownGate    |=> 1000 -> "retreat to own gate"
                    r.freeGate   |=> 800  -> "retreat to free gate"
                case _ =>
                    true |=> -1000 -> "unknown"
            }
        }

        result.none |=> 0 -> "none"
        true |=> -((1 + math.random() * 4).round.toInt) -> "random"

        result.sortBy(v => -v.weight.abs)
    }
}
