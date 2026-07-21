package cws

import hrf.colmat._

object BotBB extends BotX(implicit g => new GameEvaluationBB)

class GameEvaluationBB(implicit game : Game) extends GameEvaluation(BB)(game) {
    def eval(a : Action) : $[Evaluation] = {
        var result : $[Evaluation] = $

        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        // Alt-variant awareness: when BBAlternateSpellbooks is on, BB earns Syzygy (doom-phase
        // ES if Moon is empty) instead of Catabolism, and Carnivore (post-battle Doom per enemy
        // Monster killed) instead of Ailurophobia. Bot must score those payoffs.
        val alt = game.options.has(BBAlternateSpellbooks)

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
                    f == self && allSB                          |=> 100  -> "play first all SB"
                    f == self && others.%(ofinale).any          |=> 200  -> "play first someone near win"
                    f == self                                   |=> -50  -> "stall"

                case PlayDirectionAction(_, order) =>
                    (order == game.factions)                    |=> 20   -> "natural order"

                case DoomDoneAction(f) =>
                    f == self                                   |=> 0    -> "done doom"

                case MoveAction(f, u, from, to, _) =>
                    u.cultist && to.freeGate                    |=> 1600 -> "cultist to free gate"
                    u.cultist && to.ownGate                     |=> 200  -> "cultist to own gate"
                    u.monster && to.ownGate                     |=> 300  -> "monster to own gate"
                    u.goo && to.ownGate                         |=> 500  -> "goo to own gate"
                    to.foes.any && self.power > 3               |=> 800  -> "move toward enemies"
                    to.freeGate                                 |=> 400  -> "move to free gate"
                    // Cats can move from Moon to anywhere — prefer areas with enemies or gates
                    from == BB.moon && to.freeGate              |=> 1800 -> "moon cat to free gate"
                    from == BB.moon && to.foes.any              |=> 900  -> "moon cat toward enemy"
                    // Syzygy (alt-variant): BB earns 1 ES at doom phase if no BB units are on
                    // the Moon. Reward emptying the Moon, mildly discourage occupying it.
                    // Penalty kept modest so Mobility regression risk stays bounded.
                    (alt && self.can(Syzygy) && from == BB.moon && self.at(BB.moon).num == 1) |=> 200  -> "vacating last moon unit (syzygy ES)"
                    (alt && self.can(Syzygy) && to == BB.moon && self.at(BB.moon).none)       |=> -150 -> "occupying empty moon kills next syzygy ES"

                case RecruitAction(f, uc, r) =>
                    uc == CatFromUranus && r.ownGate            |=> 500  -> "recruit uranus at own gate"
                    uc == CatFromSaturn && r.ownGate            |=> 400  -> "recruit saturn at own gate"
                    uc == CatFromMars && r.ownGate              |=> 300  -> "recruit mars at own gate"
                    uc == Acolyte && r.freeGate                 |=> 600  -> "acolyte to free gate"

                case SummonAction(f, uc, r) =>
                    r.ownGate                                   |=> 200  -> "summon at own gate"
                    r.foes.any                                  |=> 100  -> "summon near foes"
                    // Moon summon — worth doing early game to have Earth Cats available
                    r == BB.moon                                |=> 150  -> "summon to moon"

                case BuildGateAction(f, r) =>
                    self.gates.num < 3                          |=> 800  -> "need more gates"
                    self.doom < 10                              |=> 600  -> "build gate for doom"

                case AttackAction(f, r, e, _) =>
                    self.strength(self.at(r), e) > e.strength(e.at(r), self) |=> 1200 -> "favorable battle"
                    e.at(r).goos.any                            |=> 800  -> "attack enemy goo"
                    // Carnivore (alt-variant): +1 Doom per enemy Monster killed in this battle.
                    // Score expected yield using count of enemy Monsters present at the target.
                    val carnEnemyMonsters = e.at(r).%(_.uclass.utype == Monster).num
                    (alt && self.can(Carnivore) && carnEnemyMonsters > 0) |=> (300 * carnEnemyMonsters) -> "carnivore: doom per enemy monster"

                case AwakenAction(f, Bastet, r, _) =>
                    self.needs(AwakenBastet)                    |=> 2000 -> "awaken bastet for SBR"
                    r.ownGate                                   |=> 300  -> "awaken at own gate"
                    // Carnivore: awakening Bastet into an enemy region with Monsters likely
                    // triggers a battle that grants Carnivore Doom yield.
                    val awakenEnemyMonsters = game.factions.but(self)./~(_.at(r)).%(_.uclass.utype == Monster).num
                    (alt && self.can(Carnivore) && awakenEnemyMonsters > 0) |=> (250 * awakenEnemyMonsters) -> "carnivore: awaken into enemy monsters"

                // HIGH-8 revised: Requires Attention (Bastet doom-phase ritual). Strongly
                // prefer firing it when Bastet shares a region with an enemy GOO/ElderGod;
                // otherwise modestly avoid burning it.
                case RequiresAttentionMainAction(f) =>
                    val bastet = self.allInPlay.%(_.uclass == Bastet).headOption
                    val sharesWithEnemyGOO = bastet.exists(b =>
                        game.factions.but(self).exists(e => e.at(b.region).%(_.uclass.isGOO).any))
                    sharesWithEnemyGOO  |=> 2000  -> "requires attention: bastet vs enemy goo"
                    (!sharesWithEnemyGOO) |=> -1000 -> "requires attention: no enemy goo present"

                case RequiresAttentionTargetAction(f, r) =>
                    val bastet = self.allInPlay.%(_.uclass == Bastet).headOption
                    val isBastetRegion = bastet.exists(_.region == r)
                    val enemyGOOHere = game.factions.but(self).exists(e => e.at(r).%(_.uclass.isGOO).any)
                    (isBastetRegion && enemyGOOHere) |=> 2000 -> "requires attention target: bastet+enemy goo"
                    isBastetRegion                   |=> 500  -> "requires attention target: bastet here"

                case Pay2ForBBAction(f) =>
                    // Pay 2 satisfies a generic SBR slot: Catabolism in the standard variant,
                    // Syzygy in the alt-variant (BBAlternateSpellbooks). Wording is neutral
                    // so evaluation logs read correctly under either variant.
                    self.needs(Pay2ForBB)                       |=> 1500 -> "pay 2 for SBR slot (catabolism/syzygy)"

                case CatnappingMainAction(f) =>
                    true                                        |=> 700  -> "catnapping moon opponents"

                case CatnappingDoneAction(f, picked) =>
                    picked.any                                  |=> 0    -> "catnapping done"

                case CatnappingFactionPickAction(f, picked, remaining) =>
                    remaining.any                               |=> 200  -> "pick more for catnapping"
                    picked.any                                  |=> 50   -> "done picking"

                case EndTurnAction(f) =>
                    f == self                                   |=> 0    -> "end turn"

                // [2026-06-02 v2] Gate-occupation swap loop blockers — second
                // attempt. v1's eval-level penalty was overridden by other
                // bots' positive baselines; v2 moves the hard block to the
                // candidate filter in BotX.askE (BotGateLockout). This eval
                // logic is now the same single-negative pattern used by BotFB
                // and BotTS (canonical FB Bug 45 / TS Bug 69 fix).
                case ControlGateAction(_, r, u, _) =>
                    val currentlyOnGate = self.at(r).%(_.onGate)
                    val isSwitch = currentlyOnGate.any && !currentlyOnGate.exists(_.ref == u)
                    !isSwitch                                   |=> 1000 -> "control empty gate"
                    isSwitch                                    |=> -1000000 -> "no-op swap (lockout in BotX.askE)"

                case AbandonGateAction(_, r, _) =>
                    true |=> -1000000 -> "never abandon gate"

                // Audit V7: match the project-wide convention (-1000) — every other bot uses
                // a small negative weight for unknown/fallback so a stub never dominates scoring.
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
                u.is(EarthCat)      |=> 200  -> "elim earth cat"
                u.is(CatFromMars)   |=> 300  -> "elim mars cat"
                u.is(CatFromSaturn) |=> 400  -> "elim saturn cat"
                u.is(CatFromUranus) |=> 200  -> "elim uranus cat (sbr)"
                u.goo               |=> -500 -> "dont elim goo"
            }

            def retreat(u : UnitFigure) {
                u.gateKeeper        |=> -1000 -> "dont retreat gate keeper"
                u.is(Acolyte)       |=> 800   -> "retreat acolyte"
                u.is(EarthCat)      |=> 600   -> "retreat earth cat"
                u.is(CatFromMars)   |=> 400   -> "retreat mars cat"
                u.goo               |=> -5000 -> "dont retreat goo"
            }

            a match {
                case ZagazigUseAction(f) =>
                    allies./(_.uclass.cost).sum < enemies./(_.uclass.cost).sum |=> 2000 -> "zagazig when losing"

                case ZagazigSkipAction(f) =>
                    true |=> 0 -> "skip zagazig"

                // HIGH-8 revised: Predator chain — BB owns the Use/Skip + type-pick steps,
                // the affected enemy faction owns the final eliminate step.
                case PredatorUseAction(f) =>
                    true |=> 2000 -> "use predator (free enemy elim)"

                case PredatorSkipAction(f) =>
                    true |=> -500 -> "skip predator"

                case PredatorPickEnemyTypeAction(f, lostTypes) =>
                    true |=> 1500 -> "predator: pick enemy type"

                case PredatorTypeChoiceAction(f, uc) =>
                    // Prefer eliminating the most expensive class lost.
                    uc.cost >= 4 |=> 1800 -> "predator: pick costly class"
                    uc.cost >= 2 |=> 1200 -> "predator: pick mid class"
                    true         |=> 800  -> "predator: pick class"

                case SavageryUseAction(f) =>
                    allies.%(_.uclass == CatFromSaturn).any |=> 1000 -> "savagery with saturn cats"

                case SavagerySkipAction(f) =>
                    true |=> 0 -> "skip savagery"

                case AssignKillAction(_, _, _, u) => elim(u)

                case AssignPainAction(_, _, _, u) => retreat(u)

                case RetreatUnitAction(_, u, r) =>
                    r.ownGate           |=> 1000 -> "retreat to own gate"
                    r == BB.moon        |=> 600  -> "retreat to moon"
                    r.freeGate          |=> 800  -> "retreat to free gate"
                    u.cultist && r.freeGate |=> 1500 -> "cultist retreats to free gate"

                // Audit V7: match the project-wide convention (-1000) — every other bot uses
                // a small negative weight for unknown/fallback so a stub never dominates scoring.
                case _ =>
                    true |=> -1000 -> "unknown"
            }
        }

        result.none |=> 0 -> "none"
        true |=> -((1 + math.random() * 4).round.toInt) -> "random"

        result.sortBy(v => -v.weight.abs)
    }
}
