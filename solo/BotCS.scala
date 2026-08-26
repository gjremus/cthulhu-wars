package cws

import hrf.colmat._

object BotCS extends BotX(implicit g => new GameEvaluationCS)

class GameEvaluationCS(implicit game : Game) extends GameEvaluation(CS)(game) {
    def eval(a : Action) : $[Evaluation] = {
        var result : $[Evaluation] = $

        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        val power = max(self.power, 0)

        // Count Globules on board (critical for Tulzscha combat and Wells)
        def globulesOnBoard = self.onMap(LuminousGlobule).not(Zeroed).num

        // Is region a Prismatic Well?
        def isPrismaticWell(r : Region) = game.csPrismaticWellRegions.has(r)

        // Enemies near finale
        def ofinale(f : Faction) = f.allSB && f.aprxDoom >= 25

        // FB-awareness: avoid craters and CG regions
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
            // ---- FIRST PLAYER / ORDER ----
            case FirstPlayerAction(_, f) =>
                f == self && allSB |=> 100 -> "play first all SB"
                f == self && areas.%(_.capturers.any).%(_.allies.cultists.any).any |=> 100 -> "play first prevent capture"
                f == self |=> -50 -> "stall"

            case PlayDirectionAction(_, order) =>
                order(1) == CC |=> 100 -> "cc second"
                order(2) == CC |=> 50 -> "cc third"

            // ---- SPELLBOOK SELECTION ----
            case SpellbookAction(_, sb, _) => sb match {
                case CoreExposure            => true |=> 1000 -> "core exposure first converts meteorites to globules"
                case VermiculiteHypertrophy  => true |=> 900  -> "vermiculite hypertrophy summon without gates"
                case CosmicLandfall          => true |=> 800  -> "cosmic landfall free meteorites"
                case SpectralCollapse        => true |=> 700  -> "spectral collapse doom es engine"
                case EffulgentSacrifice      => true |=> 600  -> "effulgent sacrifice es conversion"
                case Insanity                => true |=> 500  -> "insanity redirect damage"
                case _                       => true |=> 100  -> "other"
            }

            // ---- RITUAL ----
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

            // ---- DOOM DONE ----
            case DoomDoneAction(_) =>
                true |=> 10 -> "doom done"

            // ---- PASS ----
            case PassAction(_) =>
                true |=> -500 -> "wasting power bad"

            // ---- MOVE DONE ----
            case MoveDoneAction(_) =>
                true |=> 1000 -> "move done"

            // ---- MOVE: Acolyte ----
            case MoveAction(_, u, o, d, cost) if u.uclass == Acolyte =>
                active.none && o.ownGate && o.allies.cultists.num == 1 |=> -200000 -> "gatekeeper"
                active.none && d.freeGate |=> 100000 -> "safe move and get gate"
                d.freeGate && d.allies.goos.any |=> 5000 -> "move to free gate with goo"
                o.ownGate && o.allies.cultists.num == 1 |=> -500 -> "gatekeeper"
                d.allies.goos.any |=> 30 -> "goo will protect"

            // ---- MOVE: Globule (high priority - enables Wells) ----
            case MoveAction(_, u, o, d, cost) if u.uclass == LuminousGlobule =>
                // Key strategy: move Globules to enemy gates to convert them to Prismatic Wells
                d.enemyGate && !isPrismaticWell(d) |=> 8000 -> "move globule to enemy gate convert to well"
                d.enemyGate && isPrismaticWell(d) |=> 200 -> "move globule to existing well maintain"
                d.ownGate |=> 100 -> "move globule to own gate"
                d.foes.goos.active.any |=> -5000 -> "dont move globule into active goo"
                d.allies.goos.any |=> 300 -> "move globule near tulzscha"

            // ---- MOVE: Meteorite (spread to enemy starts) ----
            case MoveAction(_, u, o, d, cost) if u.uclass == Meteorite =>
                // Spread Meteorites to enemy start areas for SBR1
                need(CSMeteoriteInEnemyStart) && others.exists(e => game.starting.get(e).contains(d)) |=> 3000 -> "meteorite to enemy start for sbr1"
                d.allies.cultists.any |=> 100 -> "meteorite near cultist for core exposure"

            // ---- MOVE: Excrescence (6 combat monsters) ----
            case MoveAction(_, u, o, d, cost) if u.uclass == EffervescentExcrescence =>
                d.allies.goos.any |=> 200 -> "excrescence with goo"
                d.ownGate && d.foes.any |=> 300 -> "excrescence defend gate"

            // ---- MOVE: Tulzscha ----
            case MoveAction(_, u, o, d, cost) if u.uclass == CSTulzscha =>
                power > 1 && d.enemyGate && d.foes.goos.none |=> 5000 -> "tulzscha capture gate"
                power > 1 && others.%(ofinale).%(f => f.gates.contains(d)).any |=> 500000 -> "others finale gate attack"
                d.foes.goos.any && self.strength(self.at(d), d.owner) > 5 |=> 300 -> "tulzscha fight goo"
                d.ownGate |=> 30 -> "tulzscha at own gate"

            // ---- BUILD GATE ----
            case BuildGateAction(_, r) =>
                active.none && self.gates.num < 6 |=> 100000 -> "safe build gate"
                self.gates.none |=> 3000 -> "desperately need first gate"
                r.allies.goos.any |=> 300 -> "build gate with goo"
                r.capturers.%(_.power > 0).any |=> -1000 -> "build gate risky"

            // ---- RECRUIT Acolyte ----
            case RecruitAction(_, Acolyte, r) =>
                active.none && r.freeGate |=> 100000 -> "safe recruit and get gate"
                r.freeGate |=> 5000 -> "recruit to free gate"
                r.ownGate && r.allies.goos.any |=> 200 -> "recruit at gate with goo"
                r.capturers.%(_.power > 0).any |=> -2000 -> "dont recruit to be captured"

            // ---- SUMMON: Meteorite (cheap 1-cost scout) ----
            case SummonAction(_, Meteorite, r) =>
                true |=> 100 -> "summon meteorite"
                need(CSMeteoriteInEnemyStart) && r.near.exists(n => others.exists(e => game.starting.get(e).contains(n))) |=> 1500 -> "meteorite near enemy start"
                r.ownGate |=> 200 -> "meteorite at gate"

            // ---- SUMMON: Excrescence (2-cost combat, becomes 6 with SB) ----
            case SummonAction(_, EffervescentExcrescence, r) =>
                true |=> 150 -> "summon excrescence"
                have(VermiculiteHypertrophy) |=> 500 -> "excrescence 6 combat with sb"
                r.ownGate && r.foes.any |=> 400 -> "excrescence defend gate"
                r.capturers.%(_.power > 0).any |=> 300 -> "excrescence prevent capture"

            // ---- SUMMON: Globule (4-cost, critical for Wells and Tulzscha) ----
            case SummonAction(_, LuminousGlobule, r) =>
                true |=> 50 -> "summon globule"
                // High priority: Globules enable Prismatic Wells and power Tulzscha
                r.enemyGate && !isPrismaticWell(r) |=> 3000 -> "globule at enemy gate convert to well"
                need(CSPrismaticWellExists) && r.enemyGate |=> 2000 -> "globule for sbr2"
                self.pool.goos.any |=> 800 -> "globule before tulzscha"
                globulesOnBoard < 2 |=> 600 -> "need more globules for tulzscha combat"
                r.allies.goos.any |=> 300 -> "globule with tulzscha"

            // ---- AWAKEN: Tulzscha (5 Power, requires Acolyte + Globule) ----
            case AwakenAction(_, CSTulzscha, r, _) =>
                need(CSAwakenTulzscha) |=> 10000 -> "need tulzscha for sbr6"
                globulesOnBoard >= 3 |=> 2000 -> "tulzscha strong with 3+ globules"
                globulesOnBoard >= 2 |=> 1000 -> "tulzscha ok with 2 globules"
                globulesOnBoard == 1 |=> -500 -> "tulzscha weak with only 1 globule"
                self.gates.num >= 2 |=> 500 -> "awaken tulzscha with 2+ gates"

            // ---- CORE EXPOSURE: Meteorite → Globule (0 cost!) ----
            case CSCoreExposureMainAction(_) =>
                true |=> 2000 -> "core exposure 0 cost globule"
                need(CSPrismaticWellExists) |=> 1000 -> "core exposure for sbr2"
                globulesOnBoard < 2 |=> 800 -> "core exposure need more globules"

            case CSCoreExposureAction(r) =>
                r.enemyGate && !isPrismaticWell(r) |=> 3000 -> "core exposure convert to well"
                r.near.%(_.enemyGate).any |=> 500 -> "core exposure near enemy gate"

            // ---- VERMICULITE HYPERTROPHY: summon Excrescence without gate ----
            case CSVermiculiteMainAction(_) =>
                have(VermiculiteHypertrophy) |=> 1500 -> "vermiculite 6 combat excrescences"
                true |=> 500 -> "vermiculite summon without gate"

            case CSVermiculiteAction(r) =>
                r.foes.any && !r.gate |=> 800 -> "vermiculite summon to contested no gate"
                r.allies.goos.any |=> 300 -> "vermiculite near goo"

            // ---- EFFULGENT SACRIFICE: Globule → ES ----
            case CSEffulgentMainAction(_) =>
                globulesOnBoard > 3 |=> 1500 -> "effulgent sacrifice spare globules"
                globulesOnBoard > 2 && numSB >= 4 |=> 800 -> "effulgent sacrifice good conversion"
                globulesOnBoard <= 2 |=> -500 -> "dont sacrifice needed globules"

            case CSEffulgentAction(r) =>
                isPrismaticWell(r) && others.exists(e => e.gates.contains(r)) |=> 200 -> "effulgent at well hurts enemy"

            // ---- CORRUPTED RENDING: force enemy battle ----
            case CSCorruptedRendingMainAction(_) =>
                power > 3 |=> 1200 -> "corrupted rending weaken enemies"
                others.%(ofinale).num >= 2 |=> 800 -> "corrupted rending finale enemies fight"

            case CSCorruptedRendingRegionAction(r) =>
                true |=> 500 -> "corrupted rending force battle"

            case CSRendingPickFirstAction(r, a) =>
                a.aprxDoom >= 25 |=> 1000 -> "force high doom faction"

            case CSRendingPickSecondAction(r, a, b) =>
                a.aprxDoom >= 25 && b.aprxDoom >= 25 |=> 2000 -> "force two finale factions fight"
                true |=> 500 -> "force battle"

            // ---- SBR4: Sacrifice Globule action ----
            case CSSacrificeGlobuleMainAction(_) =>
                globulesOnBoard > 2 && need(CSSacrificeGlobule) |=> 1000 -> "sbr4 sacrifice globule"

            case CSSacrificeGlobuleAction(r) =>
                isPrismaticWell(r) && others.exists(e => e.gates.contains(r)) |=> 500 -> "sbr4 enemy gets 2 power"

            // ---- ATTACK ----
            case AttackAction(_, r, f, _) if f.neutral =>
                true |=> -100000 -> "dont attack neutrals"

            case AttackAction(_, r, f, _) =>
                val allies = self.at(r)
                val foes = f.at(r)
                val ownStr = self.strength(allies, f)
                val enemyStr = f.strength(foes, self)

                allies.goos.any && foes.goos.none && f.gates.contains(r) |=> 5000 -> "tulzscha capture gate"
                allies.goos.any && ofinale(f) |=> 600000 -> "finale attack"
                ownStr >= 6 && foes.goos.any |=> 250 -> "pound goo"
                ownStr >= enemyStr * 2 |=> 400 -> "overwhelming force"

            // ---- CAPTURE ----
            case CaptureAction(_, r, f, _) =>
                val safe = active.none
                safe && r.enemyGate && r.controllers.num == 1 |=> 100000 -> "safe capture open gate"
                ofinale(f) && f.gates.contains(r) |=> 600000 -> "finale capture"
                r.enemyGate |=> 2000 -> "capture gate"

            // ---- GATE CONTROL ----
            case AbandonGateAction(_, _, _) =>
                true |=> -1000000 -> "never abandon"

            case ControlGateAction(_, r, u, _) =>
                r.allies.%(_.onGate).foreach { c =>
                    c.uclass == u.uclass |=> -1000000 -> "remain calm"
                }
                r.allies.%(_.onGate).none |=> 1000000 -> "claim gate"

            // ---- END TURN ----
            case EndTurnAction(_) =>
                self.battled.any |=> 20000 -> "unlimited battle drains power"
                others.%(ofinale).any |=> 666000 -> "extend finale"
                true |=> 500 -> "main done"

            // ---- REVEAL ES ----
            case RevealESAction(_, es, false, _) if self.es != es =>
                true |=> -10000 -> "better reveal all"

            case RevealESAction(_, _, _, _) =>
                allSB && realDoom >= 30 |=> 1100 -> "reveal and try to win"
                !allSB && realDoom >= 30 && others.all(!_.allSB) |=> 1100 -> "reveal break 30 nobody wins"
                true |=> -100 -> "dont reveal"

            // ---- THOUSAND FORMS ----
            case ThousandFormsAskAction(_, r, offers, _, _, _, power) =>
                r < power + offers./(_.n).sum |=> -6*6*6*6*6*6 -> "dont overpay"
                power == 0 |=> (5*5*5*5*5*5 * Math.random()).round.toInt -> "pay 0"
                power == 1 |=> (2*5*5*5*5*5 * Math.random()).round.toInt -> "pay 1"
                power == 2 |=> (2*2*5*5*5*5 * Math.random()).round.toInt -> "pay 2"
                power == 3 |=> (2*2*2*5*5*5 * Math.random()).round.toInt -> "pay 3"

            // ---- GHROTH ----
            case GhrothAskAction(_, _, _, _, _, _, n) =>
                n == -1 |=> 1000 -> "refuse"
                n == 0 |=> 1000 -> "wait"

            case GhrothTargetAction(_, c, f, _) =>
                c.gateKeeper |=> -900 -> "gate keeper"
                c.friends.cultists.none |=> 800 -> "will be captured anyway"

            case _ =>
        }

        // ---- BATTLE ----
        if (game.battle.any) {
            if (game.battle./~(_.sides).has(self).not) {
                a match {
                    case _ => true |=> 1000 -> "todo"
                }
            }
            else {
                implicit val battle = game.battle.get

                val allies = self.forces
                val enemies = self.opponent.forces

                def elim(u : UnitFigure) {
                    u.is(Acolyte)               |=> 800 -> "elim acolyte"
                    u.is(Meteorite)             |=> 600 -> "elim meteorite"
                    u.is(EffervescentExcrescence) |=> 400 -> "elim excrescence"
                    u.is(LuminousGlobule)       |=> 200 -> "elim globule valuable"
                    u.is(CSTulzscha)            |=> 100 -> "elim tulzscha"
                }

                def retreat(u : UnitFigure) {
                    u.gateKeeper |=> -1000 -> "dont retreat gatekeeper"
                    u.is(Acolyte)   |=> 600 -> "retreat acolyte"
                    u.is(Meteorite) |=> 400 -> "retreat meteorite"
                    u.is(EffervescentExcrescence) |=> 300 -> "retreat excrescence"
                    u.is(LuminousGlobule) |=> 800 -> "retreat globule preserve"
                    u.is(CSTulzscha) && allies.num > 1 |=> -500 -> "tulzscha stay"
                }

                a match {
                    case AssignKillAction(_, _, _, u) => elim(u)
                    case AssignPainAction(_, _, _, u) => retreat(u)
                    case EliminateNoWayAction(_, u) => elim(u)

                    case RetreatUnitAction(_, u, r) =>
                        u.cultist && r.allies.goos.any |=> 2000 -> "retreat cultist to goo"
                        u.cultist && r.freeGate |=> 4000 -> "retreat to free gate"
                        u.cultist && r.ownGate |=> 100 -> "retreat to own gate"
                        u.is(LuminousGlobule) && r.allies.goos.any |=> 1500 -> "retreat globule to safety"
                        u.goo && r.allies.num >= 2 |=> 500 -> "retreat tulzscha with support"
                        true |=> r.connected.distinct.num -> "reachable"

                    case _ =>
                        true |=> 1000000 -> "todo"
                }
            }
        }

        // FB-awareness: score CG/Eye Opens prompts
        result ++= fbPromptedEvals(a)

        result.none |=> 0 -> "none"
        true |=> (math.random() * 4).round.toInt -> "random"

        result.sortBy(v => -v.weight.abs)
    }
}
