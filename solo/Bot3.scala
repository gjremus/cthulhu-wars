package cws

import hrf.colmat._


object Bot3 {
    var lastEval : $[ActionEval] = $
    // Round 9: centralized trace-faction state. SimRunner sets this to the
    // target faction when --trace is passed, and the weight-log pattern match
    // uses it to filter decisions. Replaces per-bot `traceWeights` flags so
    // new factions can opt in by setting this variable (no per-bot change).
    var traceFaction : Option[Faction] = None

    // Shared per-unit devour/elimination value ranking.
    // Higher score = better target to devour or eliminate.
    def devourRanking(u : UnitFigure) : Int = {
        val uc = u.uclass
        val utype = uc.utype
        val cost = uc.cost
        val isFactionUnit = uc.isInstanceOf[FactionUnitClass]
        val isNeutralMonster = !isFactionUnit && (utype == Monster || utype == Terror)
        val isMonsterOrTerror = utype == Monster || utype == Terror

        if (utype == Cultist) {
            if (u.onGate) 900 else 1000
        }
        else if (uc == Desiccated) 700
        else if (uc == RevenantOfKnaa) 400
        else if (isNeutralMonster && cost == 1) 800
        else if (isNeutralMonster && cost == 2) 600
        else if (isMonsterOrTerror && cost == 3) 500
        else if (isMonsterOrTerror && cost == 4) 300
        else if (isMonsterOrTerror && cost == 5) 200
        else if (isMonsterOrTerror && cost > 5) 100
        else 0
    }
}

case class Bot3(faction : Faction) {
    def cost(a : Action)(implicit game : Game) : Int = a match {
        case BuildGateAction(_, _) => 3
        case SummonAction(self, uc, r) => self.summonCost(uc, r) + self.taxIn(r)
        case AwakenAction(self, uc, r, cost) => cost + self.taxIn(r)
        case GhrothMainAction(_) => 2
        case DreamsAction(_, _, _) => 2
        case Pay4PowerMainAction(_) => 4
        case Pay6PowerMainAction(_) => 6
        case Pay10PowerMainAction(_) => 10
        case _ => 1
    }

    def ask(actions : $[Action], error : Double)(implicit game : Game) : Action =
        askE(Explode.explode(game, actions), error)

    def askE(actions : $[Action], error : Double)(implicit game : Game) : Action = {
        if (actions.num == 1)
            return actions.head

        val evaluated = eval(actions)
        Bot3.lastEval = evaluated
        evaluated.maxBy(_.evaluations.map(_.weight).sum * (1 + error * (random() * 2 - 1))).action
    }

    def eval(actions : $[Action])(implicit game : Game) : $[ActionEval] = {
        if (game.factions.none)
            return actions./{ a => ActionEval(a, $) }

        val self = faction
        val others = game.factions.%(_ != self)
        val power = self.power

        // [2026-05-22] Opt #1 — cache per-decision invariants (user-requested).
        // The implicit classes below close over these vals, so r.allies / r.foes /
        // r.gate / r.ownGate / r.enemyGate become O(1) instead of linearly scanning
        // self.units and others.units on every access. Behavior unchanged: caches
        // are read-only snapshots of game state at eval-time.
        val _selfAtRegion : scala.collection.Map[Region, $[UnitFigure]] = self.units.groupBy(_.region)
        val _factionAtRegion : scala.collection.Map[(Faction, Region), $[UnitFigure]] = {
            val m = scala.collection.mutable.Map[(Faction, Region), $[UnitFigure]]()
            game.factions.foreach { f =>
                f.units.groupBy(_.region).foreach { case (r, us) => m((f, r)) = us }
            }
            m
        }
        val _gatesSet : scala.collection.Set[Region] = game.gates.toSet
        val _factionGatesSet : scala.collection.Map[Faction, scala.collection.Set[Region]] =
            game.factions.map(f => f -> f.gates.toSet).toMap

        def _at(f : Faction, r : Region) : $[UnitFigure] = _factionAtRegion.getOrElse((f, r), Nil)

        implicit class FactionClassify(val f : Faction) {
            def realDoom = f.doom + f.es./(_.value).sum
            def aprxDoom = f.doom + (f.es.num * 1.67).round.toInt
            def count(uc : UnitClass) = f.all(uc).num
            def allSB = f.hasAllSB
            def numSB = f.spellbooks.num
        }

        implicit class RegionClassify(val r : Region) {
            def empty = allies.none && foes.none
            def allies = _selfAtRegion.getOrElse(r, Nil)
            def foes = others./~(f => _at(f, r))
            def gate = _gatesSet.contains(r)
            def ownGate = _factionGatesSet.getOrElse(self, Set.empty).contains(r)
            def enemyGate = others.exists(f => _factionGatesSet.getOrElse(f, Set.empty).contains(r))
            def chaosGate = DS.chaosGateRegions.has(r)
            def freeGate = gate && !ownGate && !enemyGate
            def controllers : $[UnitFigure] = (ownGate || enemyGate).??(owner.at(r).%(_.canControlGate))
            def owner = game.factions.%(_.gates.has(r)).single.get
            def capturers = allies.goos.none.??(others.%(f => _at(f, r).goos.any || (allies.monsterly.none && _at(f, r).monsterly.%(_.canCapture).any)))
        }

        implicit class UnitClassify(val u : UnitFigure) {
            def is(uc : UnitClass) = u.uclass == uc
            def ally = u.faction == self
            def foe = u.faction != self
            def friends = _at(u.faction, u.region).%(_ != u)
            def enemies = game.factions.%(_ != u.faction)./~(f => _at(f, u.region))
            def ownGate = u.region.ownGate
            def enemyGate = u.region.enemyGate
            def gateController = u.region.gate && u.region.controllers.has(u)
            def gateKeeper = gateController && friends.%(_.canControlGate).none
            def defender = ownGate && (u.monster || u.terror || u.goo) && friends.monsterly.none
            def protector = (u.monster || u.terror || u.goo) && friends.cultists.any && friends.monsterly.none
            def preventsCaptureM = u.monsterly && friends.cultists.any && friends.monsterly.none && friends.goos.none && enemies.monsterly.any
            def preventsCaptureG = u.goo && friends.cultists.any && friends.goos.none && enemies.goos.any
            def prevents = preventsCaptureM || preventsCaptureG
            def pretender = u.cultist && !capturable && enemyGate
            def shield = friends.goos.any
            def capturable = u.cultist && capturers.%(_.power > 0).any
            def capturers = game.factions.%(_ != u.faction).%(f => friends.goos.none && (_at(f, u.region).goos.any || (friends.monsterly.none && _at(f, u.region).monsterly.%(_.canCapture).any)))
            def vulnerable = u.cultist && friends.goos.none && friends.monsterly.none
        }

        implicit def unitRefToUnitClassify(r : UnitRef) : UnitClassify = UnitClassify(r)
        implicit def unitRefToUnitFigureGameEx(r : UnitRef) : UnitFigureGameEx = UnitFigureGameEx(r)

        val maxEnemyPower = others./(_.power).max

        val fbInGame3 = game.factions.has(FB) && self != FB
        val fbHasCG3 = fbInGame3 && FB.has(CyclopeanGaze) && !FB.oncePerGame.has(CyclopeanGaze)
        def fbAvoid3(r : Region) : Int = {
            if (!fbInGame3) 0
            else if (game.fbCraters.has(r)) -7000
            else if (fbHasCG3 && (FB.at(r, Ghatanothoa).any || FB.at(r, RevenantOfKnaa).any)) -7000
            else 0
        }

        def adjustedOwnStrengthForCosmicUnity(ownStr : Int, allies : $[UnitFigure], foes : $[UnitFigure], opponent : Faction)(implicit game : Game) : Int = {
            val hasDaoloth = foes.exists(_.uclass == Daoloth)
            if (!hasDaoloth) return ownStr

            val allyGOOs = allies.filter(_.uclass.utype == GOO)
            if (allyGOOs.none) return ownStr

            val nyogthas = allies.filter(_.uclass == Nyogtha)
            val nyogthaReduction : Int = if (nyogthas.any) nyogthas.head.faction.strength(nyogthas, opponent) else 0

            val perGOOStrengths : $[Int] = allyGOOs.map(u => u.faction.strength($(u), opponent))
            val strongestGOOStr = perGOOStrengths.foldLeft(0)(math.max)

            val reduction = math.max(nyogthaReduction, strongestGOOStr)
            math.max(0, ownStr - reduction)
        }

        def canRitual = self.acted.not && game.ritualCost <= power

        def canSummon(u : UnitClass) = self.pool(u).any

        val has1000f = faction.as[CC].?(f => actions.has(ThousandFormsMainAction(f)))

        val otherOceanGates = others./(_.gates.%(_.glyph == Ocean).any).any

        val instantDeathNow = game.ritualTrack(game.ritualMarker) == 999 || game.factions.%(_.doom >= 30).any
        val instantDeathNext = game.ritualTrack(game.ritualMarker) != 999 && game.ritualTrack(game.ritualMarker + 1) == 999

        def validGatesForRitual : $[Region] = {
            self.gates.filter { r =>
                val filthHere = game.factions.exists { other =>
                    other != self &&
                    other.has(TheBrood) &&
                    other.at(r).exists(_.uclass == Filth)
                }
                !filthHere
            }
        }

        def maxDoomGain = validGatesForRitual.num + self.factionGOOs.num * 3
        def aprxDoomGain = validGatesForRitual.num + self.factionGOOs.num * 1.666

        def evalA(a : Action) : $[Evaluation] = {
            var result : $[Evaluation] = $

            implicit class condToEval(val bool : Boolean) {
                def |=> (e : => (Int, String)) { if (bool) result :+= Evaluation(e._1, e._2) }
            }

            if (self == GC) a match {
                case SpellbookAction(_, sb, _) => sb match {
                    case Devolve =>
                        true |=> 500 -> "must have"
                    case Submerge =>
                        self.has(Cthulhu) |=> 900 -> "cthulhu in play"
                    case Dreams =>
                        true |=> 200 -> "dreaming is ok"
                        otherOceanGates |=> 400 -> "ocean gates"
                        self.has(Cthulhu) |=> -200 -> "cthulhu in play"
                    case YhaNthlei =>
                        true |=> -600 -> "late game"
                        self.has(Cthulhu) |=> 800 -> "cthulhu in play"
                        otherOceanGates |=> 800 -> "ocean gates"
                    case Regenerate =>
                        self.count(Starspawn) >= 2 |=> 200 -> "2 starspawn"
                        self.count(Starspawn) >= 1 |=> 300 -> "one starspawn"
                    case Absorb =>
                        self.count(Shoggoth) >= 2 |=> 200 -> "shoggoth"
                        self.count(Shoggoth) >= 1 |=> 200 -> "one shoggoth"
                        self.count(DeepOne) >= 4 |=> 200 -> "many deep ones"
                    case _ =>
                        true |=> -1000 -> "unknown"
                }

                case DevolveAction(_, r, then) =>
                    val c = self.at(r, Acolyte).head
                    true |=> -100 -> "not unless needed"
                    c.gateKeeper |=> -500 -> "don't devolve gatekeeper"
                    then != PreMainAction(self) && c.capturable |=> 1000 -> "devolve to avoid capture"
                    then == PreMainAction(self) && self.acted.not && self.has(Dreams) && self.pool.cultists.none && areas.%(r => r.enemyGate && r.controllers.num == 1 && others.%(_.power > 0).%(f => f.at(r).monsterly.any || f.at(r).goos.any).none).any |=> 300 -> "devolve to allow dreams"
                    then == PreMainAction(self) && !c.gateKeeper && self.pool.cultists.none && areas.%(r => r.freeGate && r.capturers.none).any |=> 800 -> "devolve to recruit at free gate"

                case DreamsAction(_, r, f) =>
                    val c = f.at(r)(Acolyte).head

                    true |=> -100 -> "dreams are expensive"
                    r.enemyGate |=> 300 -> "enemy gate"
                    c.gateKeeper |=> 200 -> "enemy gate controller"
                    c.friends.none |=> 200 -> "no friends"
                    c.faction.power == 0 && !c.faction.has(Passion) |=> 200 -> "enemy out of power"
                    r.allies.goos.any && r.foes.goos.none |=> -300 -> "have goo there already"
                    others.%(_.power > 0 || power == 2).%(f => f.at(r).goos.any || (r.allies.none && f.at(r).monsterly.any)).any |=> -200 -> "may end captured"

                case SubmergeMainAction(_, _) =>
                    val cthulhu = self.goo(Cthulhu)

                    cthulhu.enemies.any |=> -100 -> "can fight right now"
                    true |=> 400 -> "better than moving"
                    cthulhu.friends.num >= 5 |=> 100 -> "many friends"

                case SubmergeAction(_, r, uc) =>
                    val u = self.at(r).one(uc)
                    u.gateKeeper |=> -500 -> "don't submerge gate keeper"
                    u.defender |=> -400 -> "don't submerge defender"
                    u.uclass.cost == 3 |=> 300 -> "submerge 3"
                    u.uclass.cost == 2 |=> 300 -> "submerge 2"
                    u.uclass.cost == 1 |=> 300 -> "submerge 1"
                    u.cultist && u.faction.at(GC.deep).cultists.any |=> -400 -> "one cultist is enough"

                case UnsubmergeAction(_, r) =>
                    r.enemyGate |=> 200 -> "unsubmerge on gate"
                    r.glyph == Ocean |=> 200 -> "unsubmerge in ocean"
                    r.foes.goos.any |=> 200 -> "unsubmerge to goo"
                    r.foes.num > 5 |=> 200 -> "unsubmerge to many foes"

                case SummonAction(_, m, r) =>
                    m == DeepOne && r.allies.cultists.num >= 3 && self.has(Devolve) |=> -200 -> "don't summon, devolve"

                case _ =>
            }

            if (self == CC)
                if (has1000f)
                    power - cost(a) < 1 |=> -1000 -> "don't spend last power if 1000F unused"

            if (self == CC) a match {
                case SpellbookAction(_, sb, _) => sb match {
                    case ThousandForms =>
                        self.has(Nyarlathotep) |=> 1000 -> "must have if have nyarlathotep"
                    case Emissary =>
                        self.has(Nyarlathotep) |=> 900 -> "good if have nyarlathotep"
                    case SeekAndDestroy =>
                        self.count(HuntingHorror) == 2 |=> 800 -> "all hh"
                        self.count(HuntingHorror) == 1 |=> 500 -> "one hh"
                        self.count(HuntingHorror) == 0 |=> 200 -> "too good"
                    case Invisibility =>
                        self.count(FlyingPolyp) == 3 |=> 700 -> "all fp"
                        self.count(FlyingPolyp) == 2 |=> 600 -> "two fp"
                        self.count(FlyingPolyp) == 1 |=> 300 -> "one fp"
                    case Abduct =>
                        self.count(Nightgaunt) == 3 |=> 400 -> "all ng"
                        self.count(Nightgaunt) == 2 |=> 200 -> "two ng"
                        self.count(Nightgaunt) == 1 |=> 50 -> "one ng"
                    case Madness =>
                        true |=> 100 -> "madness"
                    case _ =>
                        true |=> -1000 -> "unknown"
                }

               case Pay4PowerMainAction(_) =>
                    self.numSB == 5 |=> 500 -> "last spellbook"
                    self.numSB == 4 |=> 400 -> "pre-last spellbook"
                    power < 6 |=> -200 -> "not much power"
                    power == 4 |=> -200 -> "last power"
                    self.realDoom >= 20 |=> 400 -> "the end is near"
                    self.realDoom >= 25 |=> 200 -> "the end is very near"
                    self.realDoom + self.gates.num >= 30 |=> 200 -> "the end is imminent"

               case Pay6PowerMainAction(_) =>
                    self.numSB == 5 |=> 500 -> "last spellbook"
                    self.numSB == 4 |=> 200 -> "pre-last spellbook"
                    power < 9 |=> -200 -> "not much power"
                    power == 6 |=> -200 -> "last power"
                    self.realDoom >= 20 |=> 400 -> "the end is near"
                    self.realDoom >= 25 |=> 200 -> "the end is very near"
                    self.realDoom + self.gates.num >= 30 |=> 200 -> "the end is imminent"

                case ThousandFormsMainAction(_) =>
                    power == 1 |=> 2000 -> "spend last power on 1000F"


                case _ =>
            }

            if (self == BG) a match {
                case SpellbookAction(_, sb, _) => sb match {
                    case BloodSacrifice =>
                        self.has(ShubNiggurath) |=> 1000 -> "must have if sn"
                    case ThousandYoung =>
                        self.has(ShubNiggurath) |=> 900 -> "very good if sn"
                        self.pool.monsterly.num > 5 |=> 200 -> "summoning to do"
                    case Frenzy =>
                        self.count(Acolyte) == 6  |=> 300 -> "frenzy 6"
                        self.count(Acolyte) == 5  |=> 200 -> "frenzy 5"
                        self.count(Acolyte) == 4  |=> 100 -> "frenzy 4"
                    case Necrophagy =>
                        self.count(Ghoul) == 2  |=> 550 -> "necrophagy 2"
                        self.count(Ghoul) == 1  |=> 400 -> "necrophagy 1"
                    case Ghroth =>
                        self.count(Fungi) == 4 |=> 450 -> "ghroth 4"
                        self.count(Fungi) == 3 |=> 350 -> "ghroth 3"
                        self.count(Fungi) == 2 |=> 250 -> "ghroth 2"
                        self.count(Fungi) == 1 |=> 150 -> "ghroth 1"
                    case RedSign =>
                        self.count(DarkYoung) == 3 |=> 800 -> "red sign 3"
                        self.count(DarkYoung) == 2 |=> 700 -> "red sign 2"
                        self.count(DarkYoung) == 1 |=> 600 -> "red sign 1"
                        self.count(DarkYoung) == 0 |=> 500 -> "red sign 1"
                    case _ =>
                        true |=> -1000 -> "unknown"
                }

                case EndTurnAction(_) =>
                    self.oncePerRound.has(Fertility) || self.acted |=> 8000 -> "don't oversummon"

                case NextPlayerAction(_) =>
                    self.oncePerRound.has(Fertility) || self.acted && self.cultists.%(_.capturable).none |=> 8000 -> "don't oversummon"

                case BloodSacrificeAction(_, r, u) =>
                    instantDeathNow |=> 10000 -> "instant death now"
                    self.realDoom >= others./(_.aprxDoom).max + 10 && !self.allSB |=> -500 -> "in the lead already, and not all spellbooks yet"
                    u.gateKeeper |=> -5000 -> "don't sacrifice gatekeeper"
                    u.capturable |=> 1000 -> "sacrifice capturable"
                    u.vulnerable |=> 500 -> "sacrifice vulnerable"
                    self.cultists.num > 4 |=> 200 -> "many cultists"
                    u.friends.%(_.canControlGate).num == 8 |=> 80 -> "many fcultists 8"
                    u.friends.%(_.canControlGate).num == 7 |=> 70 -> "many fcultists 7"
                    u.friends.%(_.canControlGate).num == 6 |=> 60 -> "many fcultists 6"
                    u.friends.%(_.canControlGate).num == 5 |=> 50 -> "many fcultists 5"
                    u.friends.%(_.canControlGate).num == 4 |=> 40 -> "many fcultists 4"
                    u.friends.%(_.canControlGate).num == 3 |=> 30 -> "many fcultists 3"
                    u.friends.%(_.canControlGate).num == 2 |=> 20 -> "many fcultists 2"
                    u.friends.%(_.canControlGate).num == 1 |=> 10 -> "many fcultists 1"
                    true |=> 200 -> "sacrifice is good"

                case GhrothMainAction(_) =>
                    self.all(Fungi)./(_.region).distinct.num match {
                        case 0 => true |=> -1000 -> "0 fungi"
                        case 1 => true |=> -500 -> "1 fungi"
                        case 2 => true |=> -200 -> "2 fungi"
                        case 3 => true |=> 100 -> "3 fungi"
                        case 4 => true |=> 450 -> "4 fungi"
                    }

                case AvatarAction(_, o, r, f) if f.neutral =>
                    true |=> -100000 -> "don't avatar uncontrolled filth (for now)"

                case AvatarAction(_, o, r, f) if self.all(ShubNiggurath).any =>
                    { val fb = fbAvoid3(r); fb != 0 |=> fb -> "avoid FB CG/crater" }
                    val shub = self.all(ShubNiggurath).head

                    r.enemyGate && f == r.owner && f.at(r).monsterly.num == 0 && (shub.friends.monsterly.any || !shub.region.ownGate) |=> 600 -> "get gate no monster"
                    r.enemyGate && f == r.owner && f.at(r).monsterly.num == 1 && (shub.friends.monsterly.any || !shub.region.ownGate) |=> 600 -> "get gate monster"

                    r.foes.goos.any |=> -500 -> "enemy gate and no goos"
                    shub.region.foes.goos.any |=> 500 -> "flee from goo"

                    game.cathedrals.has(shub.region) && AN.has(UnholyGround) && AN.strength(AN.at(shub.region), self) > 0 && (AN.power > 0 || power == 1) |=> 50000 -> "flee from unholy ground"
                    game.cathedrals.has(r) && AN.has(UnholyGround) && AN.strength(AN.at(r), self) > 0 && (AN.power > 0 || power < 3) |=> -50000 -> "beware unholy ground"

                case MoveAction(_, u, o, d, cost) =>
                    { val fb = fbAvoid3(d); fb != 0 |=> fb -> "avoid FB CG/crater" }
                    self.realDoom > 28 && d.allies.none && o.allies.any && self.needs(Spread8) && self.allInPlay.num >= 8 && areas.%(self.present).num < 8 |=> 3000 -> "final spread"
                    self.realDoom > 27 && self.needs(SpreadSocial) && d.allies.none && d.foes./(_.faction).%(f => areas.%(r => r.allies.any && f.at(r).any).none).any |=> 1000 -> "final social spread"

                    (u.is(Ghoul) || u.uclass == Fungi) && d.allies.none && u.friends.any && self.needs(Spread4) && areas.%(r => r.allies.any).num == 3 |=> 100 -> "get spread 4"
                    (u.is(Ghoul) || u.uclass == Fungi) && d.allies.none && u.friends.any && self.needs(Spread6) && areas.%(r => r.allies.any).num == 5 |=> 100 -> "get spread 6"
                    (u.is(Ghoul) || u.uclass == Fungi) && d.allies.none && u.friends.any && self.needs(Spread8) && areas.%(r => r.allies.any).num == 7 |=> 100 -> "get spread 8"
                    (u.is(Ghoul) || u.uclass == Fungi) && d.allies.none && u.friends.any && self.needs(SpreadSocial) && others.%(f => areas.%(r => r.allies.any && f.at(r).any).none).%(f => f.at(d).none).none |=> 100 -> "get social spread"

                case EliminateTwoCultistsAction(_, a, b) =>
                    a.gateKeeper |=> -1000 -> "don't eliminate gatekeeper a"
                    b.gateKeeper |=> -1000 -> "don't eliminate gatekeeper b"
                    (a.region == b.region) && a.ownGate && a.region.controllers.num == 2 |=> -1000 -> "don't eliminate two gatekeepers"
                    a.capturable |=> 200 -> "avoid capture a"
                    b.capturable |=> 200 -> "avoid capture b"
                    a.ownGate && a.region.controllers.num < 3 |=> -100 -> "gate a"
                    a.ownGate && b.region.controllers.num < 3 |=> -100 -> "gate b"
                    (a == b) |=> -50 -> "a == b"
                    power < self.pool.cultists.num + 2 |=> -200 -> "need power to re-recruit all cultists spellbook"
                    true |=> 400 -> "spell is good"
                    self.numSB == 5 |=> 500 -> "final spellbook"
                    self.numSB == 5 && self.realDoom >= 30 |=> 3500 -> "final spellbook"
                    self.cultists.num == 6 |=> 100 -> "too many cultists"

                case AwakenEliminateTwoCultistsAction(_, _, _, a, b) =>
                    a.gateKeeper |=> -1000 -> "don't eliminate gatekeeper a"
                    b.gateKeeper |=> -1000 -> "don't eliminate gatekeeper b"
                    (a.region == b.region) && a.ownGate && a.region.controllers.num == 2 |=> -1000 -> "don't eliminate two gatekeepers"
                    a.capturable |=> 200 -> "avoid capture a"
                    b.capturable |=> 200 -> "avoid capture b"
                    a.ownGate && a.region.controllers.num < 3 |=> -100 -> "gate a"
                    a.ownGate && b.region.controllers.num < 3 |=> -100 -> "gate b"
                    (a == b) |=> -50 -> "a == b"
                    power < 8 + self.pool.cultists.num + 2 |=> -200 -> "need power to re-recruit all cultists awaken"
                    true |=> 400 -> "awaken shub is good"
                    self.needs(AwakenShubNiggurath) && self.doom > 10 |=> 1100 -> "awaken shub is very good"
                    self.cultists.num == 6 |=> 100 -> "too many cultists"

                case _ =>
            }

            if (self == YS) a match {
                case SpellbookAction(_, sb, _) => sb match {
                    case ScreamingDead =>
                        self.has(KingInYellow) |=> 1000 -> "must have if kiy"
                    case ThirdEye =>
                        true |=> 100 -> "3rd eye good"
                        self.has(Hastur) |=> 800 -> "very good if hastur"
                    case HWINTBN =>
                        self.has(Hastur) |=> 800 -> "good if hastur"
                    case Passion =>
                        true |=> 1200 -> "passion super"
                    case Zingaya =>
                        self.pool(Undead).any && areas.%(r => self.at(r)(Undead).any && r.foes.cultists.any).any |=> 700 -> "make more undead"
                    case Shriek =>
                        self.count(Byakhee) > 1 |=> 500 -> "byakhee fly"
                    case _ =>
                        true |=> -1000 -> "unknown"
                }

                case DesecrateMainAction(_, _, te) =>
                    val kiy = self.all(KingInYellow).single.get

                    kiy.friends.num >= 5 |=> 900 -> "desecrate sure"
                    kiy.friends.num == 4 |=> 800 -> "desecrate 5"
                    kiy.friends.num == 3 |=> 700 -> "desecrate 4"
                    kiy.friends.num == 2 |=> 600 -> "desecrate 3"
                    kiy.friends.num == 1 |=> 500 -> "desecrate 2"
                    kiy.friends.num == 0 |=> 400 -> "desecrate 1"

                    kiy.region.freeGate && self.pool.cultists.any |=> 1000 -> "desecrate free gate with cultists in pool"

                    te |=> 300 -> "desecrate++"

                case DesecratePlaceAction(_, r, uc) =>
                    r.freeGate && uc == Acolyte |=> 1000 -> "place acolyte on a free gate"
                    r.allies(Byakhee).none && uc == Byakhee |=> 400 -> "new byakhee good"
                    uc == Undead |=> 200 -> "undead good default"
                    uc == Acolyte |=> 100 -> "cultist ok"

                case Provide3DoomAction(_, f) =>
                    self.power == 3 |=> 550 -> "time to give 3 doom"

                case HWINTBNAction(_, _, r) =>
                    r.enemyGate && others.%(f => f.strength(f.at(r), self) > 0 && !(r.owner == f && f.at(r).num == 1 && f.at(r).cultists.num == 1)).none |=> 1000 -> "get gate without retribution"

                case ScreamingDeadAction(_, o, r) =>
                    val kiy = self.all(KingInYellow).single.get

                    game.desecrated.has(r) |=> -500 -> "already desecrated dest"
                    game.desecrated.has(o).not |=> -500 -> "not desecrated origin"
                    self.all(KingInYellow).single.get.friends(Undead).num >= 5 |=> 800 -> "screaming sure"
                    self.all(KingInYellow).single.get.friends(Undead).num == 4 |=> 600 -> "screaming 5"
                    self.all(KingInYellow).single.get.friends(Undead).num == 3 |=> 500 -> "screaming 4"
                    self.all(KingInYellow).single.get.friends(Undead).num == 2 |=> 400 -> "screaming 3"
                    self.all(KingInYellow).single.get.friends(Undead).num == 1 |=> 300 -> "screaming 2"
                    self.all(KingInYellow).single.get.friends(Undead).num == 0 |=> 200 -> "screaming 1"

                    r.glyph == GlyphAA && self.needs(DesecrateAA) |=> 20 -> "king scream to aa"
                    r.glyph == GlyphWW && self.needs(DesecrateWW) |=> 20 -> "king scream to ww"
                    r.glyph == GlyphOO && self.needs(DesecrateOO) |=> 20 -> "king scream to oo"

                    self.needs(DesecrateAA) && r.connected.%(_.glyph == GlyphAA).any |=> 10 -> "king scream nearer to aa"
                    self.needs(DesecrateWW) && r.connected.%(_.glyph == GlyphWW).any |=> 10 -> "king scream nearer to ww"
                    self.needs(DesecrateOO) && r.connected.%(_.glyph == GlyphOO).any |=> 10 -> "king scream nearer to oo"

                    self.has(Hastur) && self.has(ThirdEye) |=> 200 -> "desecrate++"

                    kiy.enemies.goos.any |=> 500 -> "scream from goo"

                case ScreamingDeadFollowAction(_, o, r, uc) =>
                    val u = self.at(o, uc).head
                    true |=> 100 -> "scream along"
                    u.defender |=> -200 -> "don't scream defender"
                    game.desecrated.has(u.region) && u.friends.none |=> -200 -> "don't scream fiester"

                case _ =>
            }

            a match {
                case PlayDirectionAction(_, order) =>
                    order(1).power < order.last.power |=> 100 -> "low power first"

                case RitualAction(_, cost, _) =>
                    instantDeathNow |=> 10000 -> "instant death now"
                    instantDeathNext && self.allSB && others.all(!_.allSB) |=> 10000 -> "ritual if ID next and all SB"

                    instantDeathNext && !self.allSB && others.%(_.allSB).any |=> -1000 -> "don't ritual if ID next and not all SB"
                    instantDeathNext && !self.allSB && others.all(!_.allSB) && self.realDoom < others./(_.aprxDoom).max |=> 900 -> "ritual so ID next and nobody wins"
                    self.allSB && self.realDoom + maxDoomGain >= 30 |=> 900 -> "can break 30, and all SB"
                    !self.allSB && self.doom + self.gates.num >= 30 |=> -5000 -> "will break 30, but not all SB"
                    !self.allSB && self.doom + self.gates.num < 30 && self.realDoom <= 29 && self.realDoom + maxDoomGain >= 29 |=> 700 -> "won't break 30, but come near"
                    self.numSB >= 5 && cost * 2 <= power |=> 800 -> "5 SB and less than half available power"
                    self.numSB >= 2 && aprxDoomGain / cost > 1 |=> 200 -> "very sweet deal"
                    self.numSB >= 3 && aprxDoomGain / cost > 0.75 |=> 200 -> "sweet deal"
                    self.numSB >= 4 && aprxDoomGain / cost > 0.5 |=> 200 -> "ok deal"
                    cost == 5 |=> 100 -> "ritual first"
                    self.pool.goos.any |=> -200 -> "not all goos in play"
                    true |=> -250 -> "don't ritual unless have reasons"

                case NeutralMonstersAction(_, _) =>
                    true |=> -100000 -> "don't obtain loyalty cards (for now)"

                case ServitorAssignFactionAction(_, target) =>
                    // Prefer giving to enemy faction with most monsters (servitor hurts them)
                    val isEnemy = target != self
                    val monsterCount = target.allInPlay.%(u => u.uclass.utype == Monster).num
                    isEnemy |=> 1000 + monsterCount * 100 -> "give to enemy with monsters"
                    !isEnemy |=> -100000 -> "never give to self"

                case ServitorPlaceAction(_, _, _) =>
                    true |=> 0 -> "place servitor at gate"

                case PlaceSpinneretMainAction(_) =>
                    val webCount = game.webTokens.num
                    webCount >= 5 |=> 100000 -> "6th web token wins the game"
                    webCount >= 3 |=> 2000 -> "close to cosmic web victory"
                    webCount >= 0 |=> 800 -> "place web token towards cosmic web"

                case DoomDoneAction(_) =>
                    true |=> 0 -> "doom done"

                case TSUseTomeAction(_, n) =>
                    result ++= BotCursedTome.scoreUseTome(self, n)

                case TSRemoveTomeAction(_, _) =>
                    true |=> 300 -> "remove face-down tome to avoid end-game doom penalty"

                case TSSkipRemoveTomeAction(_) =>
                    true |=> -300 -> "keeping face-down tomes loses doom at game end"

                case PassAction(_) =>
                    true |=> -500 -> "wasting power bad"

                case MoveDoneAction(_) =>
                    true |=> 500 -> "move done"

                case MoveAction(_, u, o, d, cost) =>
                    { val fb = fbAvoid3(d); fb != 0 |=> fb -> "avoid FB CG/crater" }
                    true |=> 100 -> "moving is ok"
                    u.gateKeeper && (!u.capturable || u.enemies.goos.none) |=> -1000 -> "don't move gatekeeper"

                    u.monsterly && o.allies.none && d.foes.goos.any |=> 100 -> "alone vs goo"

                    u.canControlGate && !u.gateKeeper && d.freeGate && !d.chaosGate && self.gates.num < self.allInPlay.%(_.canControlGate).num && d.capturers.none |=> 500 -> "ic free gate"
                    u.monsterly && !d.foes.goos.any && d.freeGate && !d.chaosGate && self.gates.num < self.allInPlay.%(_.canControlGate).num && d.capturers.any && power > 1 |=> 500 -> "ic free gate"

                    d.ownGate && canSummon(u.uclass) && self.summonCost(u.uclass, d) == 1 |=> -1000 -> "why move if can summon for same"
                    d.ownGate && canSummon(u.uclass) && self.summonCost(u.uclass, d) == 2 |=> -500 -> "why move if can summon for almost same"

                    u.cultist && self.pool.cultists.any && d.allies.any && !self.allInPlay.tag(Moved).any |=> -1000 -> "why move if can recruit for same"

                    power < 3 && u.protector |=> -400 -> "don't move protector if low power"
                    power < 3 && u.defender |=> -400 -> "don't move defender if low power"

                    u.protector |=> -100 -> "don't move protector"
                    u.defender |=> -100 -> "don't move defender"

                    u.goo && d.ownGate && d.allies.%(_.capturable).any && d.allies.cultists.num == 1 |=> 800 -> "super-afraid lone cultist looking for goo visit"
                    u.goo && d.ownGate && d.allies.%(_.capturable).any && d.allies.cultists.num >= 2 |=> 790 -> "super-afraid cultists looking for goo visit"

                    u.monsterly && d.ownGate && d.allies.%(_.capturable).any && d.allies.cultists.num == 1 && d.foes.goos.none |=> 800 -> "afraid lone cultist looking for monster visit"
                    u.monsterly && d.ownGate && d.allies.%(_.capturable).any && d.allies.cultists.num >= 2 && d.foes.goos.none |=> 790 -> "afraid cultists looking for monster visit"

                    !u.cultist && d.enemyGate && d.owner.power <= 1 |=> 100 -> "enemy gate and low power owner"
                    !u.cultist && d.enemyGate && power - d.owner.power >= 3 |=> 100 -> "enemy gate and power diff"
                    !u.cultist && d.enemyGate && u.goo |=> 100 -> "enemy gate and move goo"

                    !u.cultist && o.enemyGate && o.owner.power <= 1 |=> -100 -> "enemy gate and low power owner origin"
                    !u.cultist && o.enemyGate && power - o.owner.power >= 3 |=> -100 -> "enemy gate and power diff origin"
                    !u.cultist && o.enemyGate && u.goo |=> -100 -> "enemy gate and move goo"

                    u.monsterly && d.allies.none && d.foes.cultists.%(_.vulnerable).any |=> 450 -> "vulnerable cultists"

                    u.monsterly && d.allies.none && d.enemyGate && d.owner == YS |=> -20 -> "damn passion"

                    (u.cultist || u.goo) && u.faction == GC && d.glyph == Ocean |=> 10 -> "cthulhu n cultist move to ocean"

                    u.uclass == KingInYellow && u.faction.has(ScreamingDead) |=> -1000 -> "king always scream"

                    o.allies.cultists.num == 6 && d.empty |=> 300 -> "crowded cultists 6 explore"

                    u.shield |=> -100 -> "don't move shield"
                    u.pretender |=> -100 -> "don't move pretender"
                    u.capturable |=> 150 -> "move capturable"

                    d.allies.goos.any && (d.foes.goos.any || !self.has(Emissary)) |=> 100 -> "move to shield"

                    power > 1 && u.is(Nyarlathotep) && d.foes(KingInYellow).any && d.foes(Hastur).none && YS.power == 0 |=> 1000 -> "nya likes kiy"
                    power > 1 && u.is(Nyarlathotep) && d.foes(ShubNiggurath).any && BG.power == 0 |=> 800 -> "nya likes shub"
                    power > 1 && u.is(Nyarlathotep) && d.foes(Cthulhu).any && GC.power == 0 |=> 600 -> "nya likes cthulhu"

                    u.goo |=> 30 -> "move goo"
                    u.goo && !o.gate && d.gate |=> 20 -> "move goo to gate"
                    u.monsterly && !o.gate && d.gate |=> 10 -> "move monster to gate"

                    val fbCGActive = game.factions.has(FB) && FB.has(CyclopeanGaze) && !FB.oncePerGame.has(CyclopeanGaze)
                    val fbDefendersAtDest = FB.at(d, Ghatanothoa).num + FB.at(d, RevenantOfKnaa).num
                    val alliesAtDest = self.at(d).num + 1
                    (fbCGActive && fbDefendersAtDest > 0 && alliesAtDest <= fbDefendersAtDest) |=> -3000 -> "CG gaze region: not enough units to overwhelm FB defenders"

                    u.goo && game.cathedrals.has(o) && AN.has(UnholyGround) && AN.strength(AN.at(o), self) > 0 && (AN.power > 0 || power == 1) |=> 50000 -> "flee from unholy ground"
                    u.goo && game.cathedrals.has(d) && AN.has(UnholyGround) && AN.strength(AN.at(d), self) > 0 && (AN.power > 0 || power < 3) |=> -50000 -> "beware unholy ground"

                case AttackAction(_, r, f, _) if f.neutral =>
                    true |=> -100000 -> "don't attack uncontrolled filth (for now)"

                case AttackAction(_, r, f, _) =>
                    val attackers = self.at(r)
                    val defenders = f.at(r)

                    val baseAttack = self.strength(attackers, f) +
                        (attackers(Cthulhu).any && defenders.monsterly.any).?(5).|(0) +
                        self.has(Abduct).?(3 * min(attackers(Nightgaunt).num, defenders.monsterly.num + defenders.cultists.num)).|(0)

                    val baseDefense = f.strength(defenders, self) +
                        (defenders(Cthulhu).any && attackers.monsterly.any).?(5).|(0) +
                        f.has(Abduct).?(3 * min(defenders(Nightgaunt).num, attackers.monsterly.num + attackers.cultists.num)).|(0)

                    val attack = adjustedOwnStrengthForCosmicUnity(baseAttack, attackers, defenders, opponent = f)
                    val defense = adjustedOwnStrengthForCosmicUnity(baseDefense, defenders, attackers, opponent = f)

                    self.acted || self.battled.any |=> -300 -> "unlimited battle drains power"
                    true |=> -200 -> "battle costs power"

                    attack <= defense |=> -300 -> "less attack"
                    attack > defense |=> 100 -> "more attack"
                    attack > defense * 3 + 3 |=> 300 -> "much more attack"

                    r.enemyGate && f == r.owner && others.%(_ != f).%(_.at(r).any).none |=> 200 -> "attack at enemy gate alone"

                    r.enemyGate && f == r.owner && attack > defenders.num * 2 && attackers.cultists.any |=> 600 -> "ready to take over gate"

                    r.allies.goos.none && f.at(r).goos.any && f.at(r).num <= self.strength(attackers, f) |=> 450 -> "assault goo"

                    self.needs(KillDevour1) && attack > 1 |=> 400 -> "need kill spellbook"
                    self.needs(KillDevour2) && defenders.num >= 2 && attack > 5 |=> 400 -> "need kill 2 spellbook"

                    defenders(FlyingPolyp).num == defenders.num && f.has(Invisibility) |=> -10000 -> "invisible polyps"
                    defenders(Nightgaunt).any && f.has(Abduct) && attackers.goos.none && attackers./(_.uclass.cost).sorted.take(defenders(Nightgaunt).num).sum > defenders(Nightgaunt).num |=> -10000 -> "suicide nightgaunts"

                    attackers.got(Nyarlathotep) && !defenders.got(Hastur) && defenders.goos.any && defense < attackers.num * 5 |=> 2000 -> "nya likes battle goos"

                    f.has(Abhoth) && defense == 0 && attack >= r.foes(Filth).num * 2 |=> 200 -> "get rid of filth"
                    f.has(Abhoth) && f.has(TheBrood) && defense == 0 && attack >= r.foes(Filth).num * 2 |=> 400 -> "get rid of brood filth"

                    f == AN && r.allies.goos.any && game.cathedrals.has(r) && AN.has(UnholyGround) |=> -50000 -> "unholy ground with goo"
                    f == AN && AN.has(Extinction) && defenders.num == 1 && defenders(Yothan).any && ((r.allies.goos.any && r.allies.num >= 3 && attack >= 6) || (r.allies.goos.none && attack >= 6)) |=> 1000 -> "attack lone extinct yothan"

                case CaptureAction(_, r, f, _) =>
                    true |=> 600 -> "capture"
                    r.enemyGate |=> 100 -> "enemy gate"
                    r.enemyGate && f == r.owner && r.controllers.num == 1 |=> 450 -> "capture and open gate"
                    r.enemyGate && f == r.owner && r.controllers.num == 2 |=> 400 -> "capture and nearly open gate"
                    r.enemyGate && f == r.owner && r.controllers.num == 1 && r.foes.%(_.canControlGate).num > 1 |=> -300 -> "give gate away"
                    self.needs(CaptureCultist) |=> 200 -> "spellbook good"

                case BuildGateAction(_, r) =>
                    true |=> 500 -> "building gates is good"
                    maxEnemyPower <= 1 && r.foes.none |=> 300 -> "enemies max 1p and no foes"
                    r.foes.goos.any |=> -400 -> "enemy goo present"
                    r.foes.monsterly.any |=> -100 -> "enemy monster present"
                    power == 3 && r.allies.num == 1 |=> -200 -> "lone cultist and last power"
                    self == GC && r.glyph == Ocean |=> 250 -> "cthulhu likes ocean gates"

                case RecruitAction(_, Acolyte, r) =>
                    // Bloated Woman Velvet Fan: slightly lower score for recruiting from fan
                    val fromVelvetFan = self.units.%(u => u.uclass == Acolyte && u.region.is[VelvetFanHold]).any && self.pool.cultists.none
                    fromVelvetFan |=> -50 -> "recruit from velvet fan slightly worse"
                    r.freeGate && !r.chaosGate && r.foes.goos.none && (r.foes.monsterly.none || r.allies.goos.any || r.allies.monsterly.any) |=> 800 -> "free gate"
                    r.foes.goos.any && r.allies.goos.none |=> -500 -> "recruit to capture"
                    r.allies.cultists.num == 1 |=> 200 -> "a cultist needs a friend"
                    r.allies.cultists.num == 2 |=> 100 -> "two cultists needs a friend"
                    self.pool.cultists.num >= power |=> 300 -> "recover lost cultists"
                    r.capturers.any |=> -1000 -> "don't recruit if can be captured"
                    true |=> 100 -> "cultist is good"

                case RecruitAction(_, HighPriest, r) =>
                    true |=> -100000 -> "inactivated"

                case SummonAction(_, uc, r) =>
                    val p = self.summonCost(uc, r)
                    // Bloated Woman Velvet Fan: slightly lower score for summoning from fan
                    val fromVelvetFan = self.units.%(u => u.uclass == uc && u.region.is[VelvetFanHold]).any && self.pool(uc).none
                    fromVelvetFan |=> -50 -> "summon from velvet fan slightly worse"
                    true |=> 300 -> "summoning is good"

                    p == 0 && !self.oncePerRound.has(Fertility) |=> 300 -> "stalling good"
                    r.allies.got(Fungi) && self.gates.%(!_.allies.got(Fungi)).any |=> -400 -> "fungi go another gate"
                    uc == DarkYoung && self.has(RedSign) |=> 100 -> "dark young are good with red sign"
                    uc == HuntingHorror && self.has(Nyarlathotep) |=> 200 -> "hunting horrors to shield nya"

                    r.foes.goos.any && r.allies.goos.none |=> -500 -> "enemy goo"
                    r.foes.goos.any && r.allies.goos.any |=> 500 -> "enemy goo and own goo"
                    r.ownGate && r.controllers.num == 1 && r.controllers.%(_.capturable).any  |=> 250 -> "prevent losing gate"
                    r.allies.%(_.capturable).any |=> 300 -> "allies capturable"
                    r.allies.%(_.vulnerable).any |=> 100 -> "allies vulnerable"
                    r.allies.monsterly.none |=> 100 -> "no monsters"
                    r.allies.cultists.num == 1 |=> 100 -> "lone cultist"
                    r.allies.cultists.num >= 3 |=> -100 -> "many cultist"
                    p == 2 |=> -50 -> "save power 2"
                    p == 3 |=> -100 -> "save power 3"

                    r.allies.goos.any |=> 100 -> "summon to shield"
                    r.foes.cultists.%(_.vulnerable).any |=> 200 -> "summon to capture"

                    // Servitor: -1 combat, summon to clear pool and unblock other monster summons
                    uc == ServitorUnit && self.pool(ServitorUnit).num == 1 |=> 800 -> "last servitor, unblocks monsters"
                    uc == ServitorUnit && r.foes.any |=> -400 -> "servitor -1 combat hurts in battle"
                    uc == ServitorUnit |=> 200 -> "free summon, clears pool"

                case AwakenAction(_, uc, r, _) =>
                    true |=> 400 -> "yes awaken"
                    r.allies.%(_.capturable).any |=> 250 -> "prevent capture"
                    r.foes.got(Hastur) |=> -200 -> "hastur is scary"
                    r.allies.%(_.vulnerable).any |=> 150 -> "allies vulnerable"

                    uc == Cthulhu |=> 500 -> "cthulhu for es"
                    uc == Nyarlathotep && !self.oncePerTurn.has(ThousandForms) |=> 200 -> "nyarlathotep for 1000f"
                    uc == ShubNiggurath && self.pool.monsterly.num > 4 |=> 300 -> "shub niggurath for summon"
                    uc == Hastur |=> 500 -> "hastur for 3rd eye"

                    self.numSB >= 5 && uc == Cthulhu && self.needs(AwakenCthulhu) |=> 500 -> "need cthulhu"
                    self.numSB >= 5 && uc == Nyarlathotep && self.needs(AwakenNyarlathotep) |=> 500 -> "need nyarlathotep"
                    self.numSB >= 5 && uc == ShubNiggurath && self.needs(AwakenShubNiggurath) |=> 500 -> "need shub niggurath"
                    self.numSB >= 5 && uc == Hastur && self.needs(AwakenHastur) |=> 500 -> "need hastur"

                case AvatarReplacementAction(_, _, r, o, u) =>
                    u.cultist && o.capturers.%(_.power > 0).any |=> -100 -> "don't send cultist to be captured"
                    u.cultist && o.capturers.%(_.power > 0).none |=> 100 -> "no capturers with power"
                    u.monsterly && o.foes.%(_.capturable).any |=> 200 -> "send to capture"

                case RevealESAction(_, es, false, _) if self.es != es =>
                    true |=> -10000 -> "better reveal all"

                case RevealESAction(_, _, _, _) =>
                    self.allSB && self.realDoom >= 30 |=> 1000 -> "reveal and try to win"
                    self.allSB && self.realDoom < 30 && self.realDoom < self.aprxDoom && self.realDoom < others./(_.aprxDoom).max |=> 900 -> "reveal bad ESs to take off heat"
                    !self.allSB && self.realDoom >= 30 && others.all(!_.allSB) && others./(_.aprxDoom).max >= 27 |=> 800 -> "reveal so 30 broken and nobody wins"
                    true |=> -100 -> "don't reveal"
                    self.acted.not && power >= game.ritualCost |=> -1000 -> "ritual first"

                case ThousandFormsAskAction(_, _, _, _, _, _, power) =>
                    power == 0 |=> 3*3*3*3*3*3 -> "pay 0"
                    power == 1 |=> 2*3*3*3*3*3 -> "pay 1"
                    power == 2 |=> 2*2*3*3*3*3 -> "pay 2"
                    power == 3 |=> 2*2*2*3*3*3 -> "pay 3"
                    power == 4 |=> 2*2*2*2*3*3 -> "pay 4"
                    power == 5 |=> 2*2*2*2*2*3 -> "pay 5"
                    power == 6 |=> 2*2*2*2*2*2 -> "pay 6"
                    result = result.map(e => Evaluation((e.weight * Math.random()).round.toInt, e.desc))

                case PowerDoomChoicePowerAction(_, _, _, _, _) =>
                    power <= 2 |=> 2000 -> "DS offer: take power (low power)"
                    !self.allSB |=> 1500 -> "DS offer: take power (need SBs)"
                    self.allSB && self.doom >= 25 |=> -500 -> "DS offer: power less useful late"
                    true |=> 1000 -> "DS offer: power default"

                case PowerDoomChoiceDoomAction(_, _, _, _, _) =>
                    self.allSB && self.doom >= 20 |=> 2000 -> "DS offer: take doom (endgame)"
                    self.allSB |=> 1500 -> "DS offer: take doom (all SBs)"
                    !self.allSB |=> 500 -> "DS offer: doom early is weak"
                    true |=> 800 -> "DS offer: doom default"

                case GiveWorstMonsterAskAction(_, _, uc, r, _)  =>
                    result = evalA(SummonAction(self, uc, r))

                case GiveBestMonsterAskAction(_, _, uc, r, _)  =>
                    result = evalA(SummonAction(self, uc, r))

                case ControlGateAction(_, r, u, _) =>
                    r.allies.%(_.onGate).foreach { c =>
                        c.uclass == u.uclass |=> -1000000 -> "remain calm"
                        // Prefer acolyte on gate over other cultist types
                        (c.uclass == HighPriest && u.uclass == Acolyte) |=> 500 -> "acolyte preferred on gate"
                        c.uclass == Acolyte && u.uclass == DarkYoung |=> 1000 -> "dark young on gate"
                        u.uclass == Acolyte && c.uclass == DarkYoung |=> -1000 -> "dark young on gate"
                        c.uclass == HighPriest && u.uclass == Acolyte |=> 1000 -> "high priest not on gate"
                        u.uclass == HighPriest && c.uclass == Acolyte |=> -1000 -> "high priest not on gate"
                    }

                case AbandonGateAction(_, _, _) =>
                    true |=> -1000000 -> "never"

                case ControlGateAction(_, r, u, _) =>
                    // Prefer taking control when no unit is on gate yet
                    val currentOnGate = r.allies.%(_.onGate)
                    (currentOnGate.none) |=> 1000000 -> "take control (no unit on gate)"
                    // If acolyte already on gate, don't switch — prefer Done
                    (currentOnGate.exists(_.uclass == Acolyte)) |=> -500 -> "acolyte already on gate, prefer done"
                    // If non-acolyte on gate and we're an acolyte, switch
                    (currentOnGate.any && !currentOnGate.exists(_.uclass == Acolyte) && u.uclass == Acolyte) |=> 500 -> "switch to acolyte on gate"

                // ────────────────────────────────────────────────────────────
                // Round 8 (FB): score the three FB-prompted choices that get
                // asked of *non-FB* factions. Bot3 can't share BotX's
                // GameEvaluation.fbPromptedEvals helper because it doesn't
                // extend GameEvaluation, so the same logic is inlined here.
                // Strategy mirrors BotX: protect valuable units, sacrifice
                // cheap off-gate units. See `BotX.fbPromptedEvals` for the
                // canonical version.
                // ────────────────────────────────────────────────────────────
                case FBCyclopeanGazePainUnitAction(_, _, uRef, _, _, _, _) =>
                    val u = game.unit(uRef)
                    u.goo                                  |=> -5000 -> "don't pain own GOO"
                    (u.uclass == HighPriest)               |=> -3000 -> "don't pain own HP"
                    u.gateKeeper                           |=> -2000 -> "don't pain own gate keeper"
                    u.monster                              |=> -1000 -> "avoid painting own monster"
                    (u.is(Acolyte) && !u.region.ownGate)   |=>  1000 -> "pain off-gate acolyte"
                    (u.cultist && !u.region.ownGate)       |=>   500 -> "pain off-gate cultist"
                    true                                   |=>     0 -> "default CG pain target"

                case FBCyclopeanGazeKillChoiceAction(_, _, killRef, _, _, _, _, _) =>
                    val k = game.unit(killRef)
                    k.goo                                  |=> -5000 -> "don't sacrifice own GOO to CG"
                    (k.uclass == HighPriest)               |=> -3000 -> "don't sacrifice own HP to CG"
                    k.gateKeeper                           |=> -2000 -> "don't sacrifice own gate keeper to CG"
                    k.monster                              |=> -1500 -> "avoid sacrificing own monster to CG"
                    (k.is(Acolyte) && !k.region.ownGate)   |=>  1000 -> "sacrifice off-gate acolyte to CG"
                    (k.cultist && !k.region.ownGate)       |=>   500 -> "sacrifice off-gate cultist to CG"
                    true                                   |=>     0 -> "default CG kill choice"

                case FBTheEyeOpensChooseCultistAction(_, _, _, uRef, _) =>
                    val u = game.unit(uRef)
                    (u.uclass == HighPriest)               |=> -2000 -> "don't sacrifice HP to eye opens"
                    u.region.ownGate                       |=> -1500 -> "keep gate keeper from eye opens"
                    (u.is(Acolyte) && !u.region.ownGate)   |=>  1000 -> "lose off-gate acolyte to eye opens"
                    true                                   |=>     0 -> "default eye opens cultist choice"

                // ── Library at Celaeno ──
                case SpendOnCustodianAction(_) =>
                    true |=> 800 -> "activate custodian"

                case SpendOnLibrarianAction(_) =>
                    true |=> 1200 -> "activate librarian vs overdue"

                case CustodianMoveAction(_, r) =>
                    r.foes.any                             |=> 1000 -> "custodian to region with enemies"
                    r.enemyGate                            |=> 500 -> "custodian to enemy gate"
                    r.allies.any                           |=> -500 -> "avoid custodian on own units"

                case CustodianStayAction(_, _) =>
                    true |=> 200 -> "custodian stay +1 bonus"

                case CustodianAssignToFactionAction(_, _, _, _, target) =>
                    (target != self)                       |=> 1000 -> "assign agony to enemy"
                    (target == self)                       |=> -5000 -> "avoid agony to self"

                case CustodianMoveToOublietteAction(_, _, target, uRef, _) =>
                    val u = game.unit(uRef)
                    u.goo                                  |=> -5000 -> "don't oubliette own GOO"
                    (u.uclass == HighPriest)               |=> -3000 -> "don't oubliette own HP"
                    u.gateKeeper                           |=> -2000 -> "don't oubliette own gate keeper"
                    (u.is(Acolyte) && !u.region.ownGate)   |=>  1000 -> "oubliette off-gate acolyte"
                    u.cultist                              |=>   500 -> "oubliette cultist"
                    true                                   |=>     0 -> "default oubliette"

                case LibrarianMoveAction(_, r) =>
                    r.foes.any                             |=> 1000 -> "librarian to region with enemies"
                    r.enemyGate                            |=> 500 -> "librarian to enemy gate"

                case LibrarianStayAction(_, _) =>
                    true |=> 200 -> "librarian stay +1 bonus"

                case LibrarianAssignToFactionAction(_, _, _, _, _, _, target) =>
                    (target != self)                       |=> 1000 -> "assign librarian agony to enemy"
                    (target == self)                       |=> -5000 -> "avoid librarian agony to self"

                case LibrarianAssignAmountAction(_, _, _, _, _, _, target, _) =>
                    (target != self)                       |=> 1000 -> "assign librarian agony to enemy"
                    (target == self)                       |=> -5000 -> "avoid librarian agony to self"

                case LibrarianAssignCancelAction(_, _, _, _, _, _) =>
                    true |=> -500 -> "avoid cancel during librarian agony assignment"

                case LibrarianResetAgonyAction(_, _, _) =>
                    true |=> -2000 -> "avoid reset during librarian agony assignment"

                case LibrarianEliminateUnitMainAction(_, _, _, _, _) =>
                    true |=> -500 -> "eliminate unit is costly"

                case LibrarianEliminateRegionAction(_, r, _, _, _, _, _) =>
                    val units = self.at(r).%(u => u.uclass.utype != MapUnit)
                    val hasGoo = units.exists(_.goo)
                    hasGoo                                  |=> -5000 -> "region has GOO"
                    (units.num == 1 && units.head.is(Acolyte)) |=> 1000 -> "region with lone acolyte"
                    true                                   |=>     0 -> "default region"

                case LibrarianEliminateUnitAction(_, uRef, _, _, _, _, _, _) =>
                    val u = game.unit(uRef)
                    u.goo                                  |=> -5000 -> "don't eliminate own GOO"
                    (u.uclass == HighPriest)               |=> -3000 -> "don't eliminate own HP"
                    u.gateKeeper                           |=> -2000 -> "don't eliminate own gate keeper"
                    (u.is(Acolyte) && !u.region.ownGate)   |=>  1000 -> "eliminate off-gate acolyte"
                    true                                   |=>     0 -> "default eliminate"

                case LibrarianEliminateDoneAction(_, _, _, _, _, eliminated) =>
                    (eliminated.any)                        |=>  2000 -> "done eliminating"
                    // 2026-05-11: -100 was tied/beat by HP (-3000) and gateKeeper
                    // (-2000) unit picks but was the *least* negative, so the bot
                    // picked Done-with-nothing and looped. Make Done-with-empty
                    // worse than the worst-but-still-eligible unit so the bot will
                    // bite the bullet and eliminate something. Handler-side safety
                    // in MapExpansion.scala also breaks the loop if this still loses.
                    eliminated.isEmpty                     |=> -10000 -> "done with nothing (avoid loop)"


                case LibrarianReturnTomeMainAction(_, _, _, _, _) =>
                    true |=> -1000 -> "returning a tome is bad"

                case LibrarianReturnTomeAction(_, tome, _, _, _, _) =>
                    true |=> 0 -> "return tome"

                case LibrarianLoseDoomAction(_, _, _, _, _) =>
                    true |=> -800 -> "losing doom is bad"

                case SpendToFlipTomeAction(_, _) =>
                    true |=> 600 -> "flip tome face-up"

                case FlipTomeReleaseCultistMainAction(_, _) =>
                    true |=> 400 -> "release cultist to flip tome"

                case FlipTomeDiscardESAction(_, _) =>
                    true |=> -200 -> "discard ES to flip tome"

                case FlipTomeDiscardTokenAction(_, _) =>
                    true |=> 300 -> "discard token to flip tome"

                case UseTomeGuardianMainAction(_) =>
                    true |=> 2500 -> "use guardian tome"

                case UseTomeGuardianRelocateAction(_, r, target) =>
                    r.ownGate                              |=>  1000 -> "relocate enemies from own gate"
                    r.enemyGate                            |=>  -500 -> "don't help enemy leave their gate"
                    true                                   |=>   200 -> "relocate enemy"

                case UseTomeGuardianDestAction(_, _, _, dest) =>
                    dest.ownGate                           |=> -2000 -> "don't move enemies to own gate"
                    dest.foes.none                         |=>  500 -> "move enemies to empty region"
                    true                                   |=>     0 -> "default guardian dest"

                case UseTomeLarvaeAction(_) =>
                    true |=> 3000 -> "gain ES from larvae"

                case UseTomeYrMainAction(_) =>
                    true |=> 2500 -> "use yr tome"

                case UseTomeYrMonsterAction(_) =>
                    // Score based on power: prefer monster path when faction has expensive units
                    val poolMonsters = self.pool.%(_.uclass.utype == Monster)
                    val maxCost = if (poolMonsters.any) poolMonsters./(_.uclass.cost).max else 0
                    (maxCost > 2)  |=> 2500 -> "place free monster (expensive units in pool)"
                    (maxCost == 2) |=> 1400 -> "place free monster (2-cost units)"
                    (maxCost <= 1) |=> 1000 -> "place free monster (cheap units only)"

                case UseTomeYrMonsterChooseAction(_, uc, r) =>
                    val gateUndefended = r.ownGate && r.allies.%(m => m.uclass.utype == Monster || m.uclass.utype == Terror).none
                    val gateUnitCount = r.ownGate.?(r.allies.num).|(99)
                    val costBoost = uc.cost * 300
                    (gateUndefended)                        |=> 3000 + costBoost -> "place at unprotected gate"
                    (r.ownGate && gateUnitCount <= 2)       |=> 2000 + costBoost -> "place at gate with few units"
                    (r.ownGate)                             |=> 1000 + costBoost -> "place at gate"
                    true                                   |=> costBoost -> "default monster placement"

                case UseTomeYrPowerAction(_) =>
                    // Power path: prefer when only cheap units in pool
                    val poolMonsters = self.pool.%(_.uclass.utype == Monster)
                    val maxCost = if (poolMonsters.any) poolMonsters./(_.uclass.cost).max else 0
                    (maxCost <= 1 || poolMonsters.none) |=> 2000 -> "gain net +1 power (no good monsters)"
                    (maxCost == 2) |=> 1600 -> "gain net +1 power (only 2-cost monsters)"
                    true |=> 1200 -> "gain net +1 power (expensive monsters available)"

                case BarrierReleaseCultistFactionAction(_, captiveFaction, _) =>
                    (captiveFaction == self)               |=> -1000 -> "don't release own cultist"
                    true                                   |=>   500 -> "release enemy cultist to battle"

                case BarrierDiscardESAction(_, _) =>
                    true |=> -200 -> "discard ES to battle"

                case BarrierDiscardTokenAction(_, _) =>
                    true |=> 500 -> "discard token to battle"

                case BarrierBlockedAction(_) =>
                    true |=> -800 -> "battle blocked is bad"

                case _ =>
            }

            // BATTLE
            if (game.battle.any) {
                implicit val battle = game.battle.get

                def elim(u : UnitFigure) {
                    u.uclass == Acolyte |=> 600 -> "elim acolyte"
                    u.gateKeeper |=> -1000 -> "elim gate keeper"
                    u.uclass == Acolyte && self.has(Passion) && !self.oncePerAction.has(Passion) |=> 200 -> "elim for passion"
                    u.uclass == Acolyte && self.has(Frenzy) |=> -100 -> "elim for frenzy"

                    u.uclass == DeepOne |=> 300 -> "elim do"
                    u.uclass == Shoggoth |=> 200 -> "elim sg"
                    u.uclass == Starspawn |=> 100 -> "elim ss"
                    u.uclass == Cthulhu |=> -400 -> "elim sn"

                    u.uclass == Nightgaunt |=> 300 -> "elim ng"
                    u.uclass == FlyingPolyp |=> 200 -> "elim fp"
                    u.uclass == HuntingHorror |=> 100 -> "elim hh"
                    u.uclass == Nyarlathotep |=> -1000 -> "elim sn"

                    u.is(Ghoul) |=> 1000 -> "elim ghoul"
                    u.uclass == Fungi && u.faction.has(ShubNiggurath) && u.faction.has(ThousandYoung) |=> 500 -> "elim fungi cheap"
                    u.uclass == Fungi |=> 400 -> "elim fungi"
                    u.uclass == DarkYoung |=> 100 -> "elim dy"
                    u.uclass == ShubNiggurath |=> -1000 -> "elim sn"

                    u.uclass == Undead |=> 700 -> "elim undead"
                    u.uclass == Byakhee |=> 200 -> "elim byakhee"
                    u.uclass == KingInYellow |=> -400 -> "elim kiy"
                    u.uclass == Hastur |=> -1000 -> "elim hastur"

                    u.uclass == Desiccated |=> 300 -> "elim desc"
                    u.uclass == RevenantOfKnaa |=> -400 -> "elim rev"
                    u.uclass == Ghatanothoa |=> -1000 -> "elim ghato"

                    u.uclass == TombHerd |=> 300 -> "elim tomb-herd"
                    u.uclass == DeepTendril |=> -400 -> "elim deep tendril"
                    u.uclass == Glaaki |=> -1000 -> "elim glaaki"

                    // DS
                    u.uclass == LarvaThesis |=> 500 -> "elim larva thesis"
                    u.uclass == LarvaAntithesis |=> 500 -> "elim larva antithesis"
                    u.uclass == LarvaSynthesis |=> 500 -> "elim larva synthesis"
                    u.uclass == AvatarThesis |=> -400 -> "elim avatar thesis"
                    u.uclass == AvatarAntithesis |=> -1000 -> "elim avatar antithesis"
                    u.uclass == AvatarSynthesis |=> -1000 -> "elim avatar synthesis"

                    // WW
                    u.uclass == Wendigo |=> 500 -> "elim wendigo"
                    u.uclass == GnophKeh |=> -200 -> "elim gnoph-keh"
                    u.uclass == RhanTegoth |=> -400 -> "elim rhan-tegoth"
                    u.uclass == Ithaqua |=> -1000 -> "elim ithaqua"

                    // SL
                    u.uclass == Wizard |=> 500 -> "elim wizard"
                    u.uclass == SerpentMan |=> 300 -> "elim serpent man"
                    u.uclass == FormlessSpawn |=> 100 -> "elim formless spawn"
                    u.uclass == Tsathoggua |=> -1000 -> "elim tsathoggua"

                    // AN
                    u.uclass == Reanimated |=> 200 -> "elim reanimated"
                    u.uclass == UnMan |=> 100 -> "elim un-man"
                    u.uclass == Yothan |=> -400 -> "elim yothan"

                    // OW
                    u.uclass == Mutant |=> 400 -> "elim mutant"
                    u.uclass == Abomination |=> 100 -> "elim abomination"
                    u.uclass == SpawnOW |=> -200 -> "elim spawn of yog-sothoth"
                    u.uclass == YogSothoth |=> -1000 -> "elim yog-sothoth"

                    // Neutral monsters
                    u.uclass.isInstanceOf[NeutralMonster] |=> 200 -> "elim neutral monster"

                    // iGOOs
                    u.uclass.isInstanceOf[IGOO] |=> -600 -> "elim igoo"

                    if (u.faction != self)
                        result = result./(e => Evaluation(-e.weight, "neg " + e.desc))
                }

                def retreat(u : UnitFigure) {
                    u.uclass == Acolyte |=> 600 -> "retr acolyte"
                    u.gateKeeper |=> -1000 -> "retr gate keeper"

                    u.uclass == DeepOne |=> 300 -> "retr do"
                    u.uclass == Shoggoth |=> 200 -> "retr sg"
                    u.uclass == Starspawn |=> 100 -> "retr ss"
                    u.uclass == Cthulhu |=> -400 -> "retr sn"

                    u.uclass == Nightgaunt |=> 100 -> "retr ng"
                    u.uclass == FlyingPolyp |=> 200 -> "retr fp"
                    u.uclass == HuntingHorror |=> 300 -> "retr hh"
                    u.uclass == Nyarlathotep |=> -1000 -> "retr sn"

                    u.is(Ghoul) |=> 1000 -> "retr ghoul"
                    u.uclass == Fungi |=> 800 -> "retr fungi"
                    u.uclass == DarkYoung |=> 100 -> "retr dy"
                    u.uclass == ShubNiggurath |=> -1000 -> "retr sn"

                    u.uclass == Undead |=> 700 -> "retr undead"
                    u.uclass == Byakhee |=> 800 -> "retr byakhee"
                    u.uclass == KingInYellow |=> 400 -> "retr kiy"
                    u.uclass == Hastur |=> -1000 -> "retr hastur"

                    // FB
                    u.uclass == Desiccated |=> 300 -> "retr desc"
                    u.uclass == RevenantOfKnaa |=> -400 -> "retr rev"
                    u.uclass == Ghatanothoa |=> -1000 -> "retr ghato"

                    // TS
                    u.uclass == TombHerd |=> 300 -> "retr tomb-herd"
                    u.uclass == DeepTendril |=> -400 -> "retr deep tendril"
                    u.uclass == Glaaki |=> -1000 -> "retr glaaki"

                    // DS
                    u.uclass == LarvaThesis |=> 500 -> "retr larva thesis"
                    u.uclass == LarvaAntithesis |=> 500 -> "retr larva antithesis"
                    u.uclass == LarvaSynthesis |=> 500 -> "retr larva synthesis"
                    u.uclass == AvatarThesis |=> -400 -> "retr avatar thesis"
                    u.uclass == AvatarAntithesis |=> -1000 -> "retr avatar antithesis"
                    u.uclass == AvatarSynthesis |=> -1000 -> "retr avatar synthesis"

                    // WW
                    u.uclass == Wendigo |=> 500 -> "retr wendigo"
                    u.uclass == GnophKeh |=> -200 -> "retr gnoph-keh"
                    u.uclass == RhanTegoth |=> -400 -> "retr rhan-tegoth"
                    u.uclass == Ithaqua |=> -1000 -> "retr ithaqua"

                    // SL
                    u.uclass == Wizard |=> 500 -> "retr wizard"
                    u.uclass == SerpentMan |=> 300 -> "retr serpent man"
                    u.uclass == FormlessSpawn |=> 100 -> "retr formless spawn"
                    u.uclass == Tsathoggua |=> -1000 -> "retr tsathoggua"

                    // AN
                    u.uclass == Reanimated |=> 200 -> "retr reanimated"
                    u.uclass == UnMan |=> 100 -> "retr un-man"
                    u.uclass == Yothan |=> -400 -> "retr yothan"

                    // OW
                    u.uclass == Mutant |=> 400 -> "retr mutant"
                    u.uclass == Abomination |=> 100 -> "retr abomination"
                    u.uclass == SpawnOW |=> -200 -> "retr spawn of yog-sothoth"
                    u.uclass == YogSothoth |=> -1000 -> "retr yog-sothoth"

                    if (u.faction != self)
                        result = result./(e => Evaluation(-e.weight, "neg " + e.desc))
                }


                a match {
                    case DevourPreBattleAction(f) =>
                        true |=> 1000 -> "always devour"

                    case DevourAction(_, u) =>
                        elim(u)

                    case AbsorbeeAction(_, _, t) =>
                        t.uclass == DeepOne |=> 300 -> "absorb deep one"
                        true |=> -100 -> "don't absorb"

                    case AbductPreBattleAction(f) =>
                        true |=> 100 -> "abduct"
                        f.opponent.strength(f.opponent.units, f) == 0 |=> -200 -> "no abduct if attack zero"

                    case AbductAction(_, _, u) =>
                        elim(u)

                    case InvisibilityAction(_, _, u) =>
                        elim(u)

                    case DemandSacrificeKillsArePainsAction(_) =>
                        self.str < self.opponent.str |=> 1000 -> "less str"
                        self.str > self.opponent.str |=> -1000 -> "more str"

                    case HarbingerPowerAction(_, _, n) =>
                        n == 2 |=> 200 -> "harb 2 power"
                        n == 3 |=> 300 -> "harb 3 power"
                        n == 4 |=> 400 -> "harb 4 power"
                        n == 5 |=> 500 -> "harb 5 power"

                    case HarbingerESAction(_, _, _) =>
                        self.allSB |=> 600 -> "es all spellbooks"
                        self.realDoom > 5 + others./(_.aprxDoom).max |=> -300 -> "doom lead"

                    case NecrophagyAction(_, u, r) =>
                        battle.attacker != self |=> 100 -> "necrophagy is good"
                        battle.attacker == self |=> -100 -> "necrophagy is bad"
                        u.prevents |=> -1000 -> "prevents capture"
                        u.region.freeGate && u.friends.none |=> -1000 -> "free gate"

                    case AssignKillAction(_, _, _, u) =>
                        elim(u)

                    case AssignPainAction(_, _, _, u) =>
                        retreat(u)

                    case EliminateNoWayAction(_, u) =>
                        elim(u)

                    case RetreatOrderAction(_, a, b) =>
                        a == self |=> 1000 -> "retreat self first"
                        a.aprxDoom < b.aprxDoom |=> 100 -> "retreat less doom first"

                    case OleaginousRetreatAction(_, u, r) =>
                        r.foes.none |=> 200 -> "oleaginous retreat to safety"
                        r.allies.goos.any |=> 100 -> "retreat to friendly goo"
                        r.ownGate |=> 100 -> "retreat to own gate"
                        r.enemyGate |=> -200 -> "avoid enemy gate"
                        r.foes.any |=> -500 -> "avoid foes"

                    case RetreatUnitAction(_, u, r) =>
                        u.foe && u.cultist && r.allies.monsterly.any |=> 1000 -> "send cultist to be captured by monsters"
                        u.foe && u.cultist && r.allies.goos.any |=> 1000 -> "send cultist to be captured by goos"
                        u.foe && u.cultist && r.foes.none |=> 200 -> "send cultist where no foes"
                        u.foe && u.cultist && u.faction.at(r).%(!_.cultist).none |=> 200 -> "send where no friends"
                        u.foe && u.cultist && u.faction.at(r).%(_.monsterly).any |=> -1000 -> "dont send where friends"
                        u.foe && u.cultist && u.faction.at(r).%(_.goo).any |=> -2000 -> "dont send to goo"
                        u.foe && u.cultist && r.freeGate |=> -1000 -> "dont send cultist to empty gate"
                        u.foe && u.cultist && r.ownGate |=> -100 -> "dont send cultist to own gate"
                        u.foe && u.cultist && r.gate |=> -100 -> "dont send cultist to gate"
                        u.foe && u.cultist && r.empty |=> 50 -> "send cultist to empty area"

                        u.ally && u.cultist && r.allies.monsterly.any |=> 1000 -> "send cultist to be protected by monsters"
                        u.ally && u.cultist && r.allies.goos.any |=> 2000 -> "send cultist to be protectd by goos"
                        u.ally && u.cultist && r.foes.none && !r.gate |=> 200 -> "send cultist where no foes"
                        u.ally && u.cultist && r.foes.none && r.freeGate |=> 4000 -> "send cultist to free gate"
                        u.ally && u.cultist && r.ownGate |=> 100 -> "sent cultist to own gate"
                        u.ally && u.cultist && r.enemyGate |=> -100 -> "dont send cultist to enemy gate"
                        u.ally && u.cultist && r.freeGate |=> -300 -> "dont send cultist to free gate"

                        u.foe && u.uclass == Fungi && u.faction.at(r)(Fungi).any |=> 1000 -> "retreat fungi to fungi"
                        u.ally && u.uclass == Fungi && u.faction.at(r)(Fungi).any |=> -800 -> "dont retreat fungi to fungi"

                        u.foe && !u.cultist && r.allies.any |=> -2000 -> "dont send non cultists to self"
                        u.foe && !u.cultist && r.ownGate |=> -2000 -> "dont send non cultists to own gate"
                        u.foe && !u.cultist && r.enemyGate && r.owner == u.faction |=> -2000 -> "dont send non cultists to their gate"
                        u.foe && !u.cultist && r.enemyGate && r.owner != u.faction && r.owner.gates.num == 6 |=> 600 -> "send non cultists to enemy gate 6"
                        u.foe && !u.cultist && r.enemyGate && r.owner != u.faction && r.owner.gates.num == 5 |=> 500 -> "send non cultists to enemy gate 5"
                        u.foe && !u.cultist && r.enemyGate && r.owner != u.faction && r.owner.gates.num == 4 |=> 400 -> "send non cultists to enemy gate 4"
                        u.foe && !u.cultist && r.enemyGate && r.owner != u.faction && r.owner.gates.num == 3 |=> 300 -> "send non cultists to enemy gate 3"
                        u.foe && !u.cultist && r.enemyGate && r.owner != u.faction && r.owner.gates.num == 2 |=> 200 -> "send non cultists to enemy gate 2"
                        u.foe && !u.cultist && r.enemyGate && r.owner != u.faction && r.owner.gates.num == 1 |=> 100 -> "send non cultists to enemy gate 1"
                        u.foe && !u.cultist && r.freeGate |=> -100 -> "dont send non cultists to free gate"
                        u.foe && !u.cultist && r.empty |=> 200 -> "send non cultists to empty"

                        u.ally && u.monsterly && r.allies.%(_.capturable).any && !r.foes.goos.any |=> 1000 -> "send monster to prevent capture"
                        u.ally && u.goo && r.allies.%(_.capturable).any |=> 1000 -> "send goo to prevent capture"

                        u.ally && u.monsterly && r.foes.%(_.vulnerable).any && !r.foes.goos.any |=> 1000 -> "send monster to capture"
                        u.ally && u.goo && r.foes.%(_.vulnerable).any |=> 1000 -> "send goo to capture"

                        u.ally && u.monsterly && r.allies.goos.any |=> 500 -> "send monster to friendly goo"
                        u.ally && u.goo && r.allies.goos.any |=> 500 -> "send goo to friendly goo"

                        u.ally && u.monsterly && r.ownGate |=> 400 -> "send monster to own gate"
                        u.ally && u.goo && r.ownGate |=> 400 -> "send goo to own gate"

                        u.ally && u.monsterly && r.freeGate |=> 300 -> "send monster to free gate"
                        u.ally && u.goo && r.freeGate |=> 300 -> "send goo to free gate"

                        u.ally && u.monsterly && r.enemyGate |=> 300 -> "send monster to enemy gate"
                        u.ally && u.goo && r.enemyGate |=> 300 -> "send goo to enemy gate"

                    // Mind Parasite: capture flow — score equivalent to normal capture
                    case MindParasiteCaptureMainAction(_) =>
                        true |=> 600 -> "attempt to capture parasitized cultist"

                    case MindParasiteCaptureTargetAction(_, r, _) =>
                        true |=> 600 -> "capture parasitized cultist"
                        r.enemyGate |=> 100 -> "enemy gate"
                        self.needs(CaptureCultist) |=> 200 -> "spellbook good"

                    // Mind Parasite: prefer blocking capture of your parasitized cultist
                    case MindParasiteBlockCaptureAction(_, _, _, _) =>
                        true |=> 800 -> "block capture of parasitized cultist"

                    case MindParasiteAllowCaptureAction(_, _, _, _) =>
                        true |=> -800 -> "dont allow capture of parasitized cultist"

                    // Moonbeast: premature return (pay 1 doom to unblock spellbook)
                    case MoonbeastPrematureReturnAction(_) =>
                        // Moderate priority — worth paying 1 doom to unblock a key spellbook
                        true |=> 400 -> "remove moonbeast from spellbook"

                    // Quachil Uttaus: Dust to Dust — victim chooses remove or give ES
                    // Leng Spider: Bloodthirst — prefer converting opponent's pains
                    case BloodthirstChooseFactionAction(_, target) =>
                        (target != self) |=> 800 -> "convert opponent pains to kill"
                        (target == self) |=> 200 -> "convert own pains to kill"

                    case BloodthirstDoneAction(_) =>
                        true |=> -100 -> "skip bloodthirst"

                    // Bokrug: Give Bokrug to another player
                    case GiveBokrugMainAction(_) =>
                        // Generally don't want to give away Bokrug unless forced — low priority
                        true |=> -500 -> "giving bokrug away costs ownership"

                    case GiveBokrugAction(_, target) =>
                        // Prefer giving to strongest enemy (they get the drawback SB)
                        val maxDoom = game.factions./(_.doom).max
                        (target.doom == maxDoom) |=> 200 -> "give bokrug to strongest enemy"
                        true |=> 0 -> "give bokrug"

                    // Bokrug: Ghosts of Ib placement
                    case GhostsOfIbChooseAction(_, r, _) =>
                        // Prefer regions with own gates
                        self.gates.has(r) |=> 500 -> "place bokrug at own gate"
                        self.at(r).any |=> 300 -> "place bokrug near own units"
                        true |=> 100 -> "place bokrug"

                    // Bokrug: Doom that Came to Sarnath choices
                    case DoomSarnathChooseOption(_, option, _) =>
                        // Prefer eliminating a unit (option 1) over discarding ES (option 2)
                        (option == 1) |=> 200 -> "have enemy eliminate a unit"
                        (option == 2) |=> 400 -> "have enemy discard ES (costs doom)"

                    case DoomSarnathChooseFactionAction(_, _, target, _) =>
                        // Pick weakest enemy to minimize impact
                        val minDoom = game.factions.but(self)./(_.doom).min
                        (target.doom == minDoom) |=> 200 -> "pick weakest enemy for sarnath"
                        true |=> 0 -> "sarnath faction choice"

                    case DoomSarnathEliminateUnit(_, owner, u, _) =>
                        // Prefer eliminating cheap/replaceable units
                        val uc = u.uclass
                        (uc.utype == Cultist) |=> 300 -> "eliminate cultist"
                        (uc.utype == Monster && uc.cost <= 2) |=> 200 -> "eliminate cheap monster"
                        (uc.utype == Monster) |=> 100 -> "eliminate monster"
                        true |=> 0 -> "eliminate unit"

                    case DoomSarnathDiscardES(_, _, index, _) =>
                        // Pick the first (oldest) ES
                        (index == 0) |=> 200 -> "discard oldest ES"
                        true |=> 100 -> "discard ES"

                    // Azathoth awakening: enemy die face selection
                    // Optimal pick = 8 / (players - 1), then alternating +1/-1/+2/-2/etc.
                    case AzathothEnemyChooseAction(_, face, _) =>
                        val players = game.factions.num
                        val optimal = 8 / (players - 1)
                        // Build preference order: optimal, optimal+1, optimal-1, optimal+2, optimal-2, ...
                        val prefs = $(0) ++ (1 to 5)./~(d => $(d, -d))
                        val ranked = prefs./(optimal + _).%(n => n >= 1 && n <= 6).distinct.take(6)
                        val idx = ranked.indexOf(face)
                        val score = if (idx >= 0) (6 - idx) * 500 else 0
                        true |=> score -> ("azathoth die face " + face + " (rank " + idx + ")")

                    case QuachilDustToDustRemoveAction(_, uRef, _) =>
                        val u = game.unit(uRef)
                        val uc = u.uclass
                        val utype = uc.utype
                        val isGOO = utype == GOO
                        val isCultist = utype == Cultist
                        val isFactionUnit = uc.isInstanceOf[FactionUnitClass]
                        val isNeutral = !isFactionUnit && (utype == Monster || utype == Terror)
                        // NEVER eliminate a GOO
                        isGOO |=> -100000 -> "never eliminate GOO"
                        // First 2 acolytes: prefer removing over giving ES
                        isCultist && uc == Acolyte && self.pool(Acolyte).num >= 4 |=> 500 -> "remove cheap acolyte (have plenty)"
                        isCultist && uc == Acolyte && self.pool(Acolyte).num >= 2 |=> 300 -> "remove acolyte (have some)"
                        // First 1-cost and 2-cost faction monsters: prefer removing
                        isFactionUnit && (utype == Monster || utype == Terror) && uc.cost <= 2 |=> 400 -> "remove cheap faction monster"
                        // Neutral 1-cost and 2-cost monsters: prefer removing
                        isNeutral && uc.cost <= 2 |=> 400 -> "remove cheap neutral monster"
                        // Everything else: don't remove (give ES instead)
                        isCultist && uc != Acolyte |=> -200 -> "dont remove non-acolyte cultist"
                        isFactionUnit && (utype == Monster || utype == Terror) && uc.cost >= 3 |=> -300 -> "dont remove expensive faction monster"
                        isNeutral && uc.cost >= 3 |=> -300 -> "dont remove expensive neutral monster"

                    case QuachilDustToDustESAction(_, uRef, _) =>
                        val u = game.unit(uRef)
                        val uc = u.uclass
                        val utype = uc.utype
                        val isGOO = utype == GOO
                        val isCultist = utype == Cultist
                        val isFactionUnit = uc.isInstanceOf[FactionUnitClass]
                        val isNeutral = !isFactionUnit && (utype == Monster || utype == Terror)
                        // GOO: always give ES rather than eliminate
                        isGOO |=> 100000 -> "always give ES for GOO"
                        // For cheap acolytes: prefer removing, so ES is worse
                        isCultist && uc == Acolyte && self.pool(Acolyte).num >= 4 |=> -500 -> "dont give ES for cheap acolyte"
                        isCultist && uc == Acolyte && self.pool(Acolyte).num >= 2 |=> -300 -> "dont give ES for acolyte"
                        // For cheap faction monsters: prefer removing
                        isFactionUnit && (utype == Monster || utype == Terror) && uc.cost <= 2 |=> -400 -> "dont give ES for cheap faction monster"
                        // For cheap neutral monsters: prefer removing
                        isNeutral && uc.cost <= 2 |=> -400 -> "dont give ES for cheap neutral monster"
                        // Everything else: prefer giving ES
                        isCultist && uc != Acolyte |=> 200 -> "give ES for non-acolyte cultist"
                        isFactionUnit && (utype == Monster || utype == Terror) && uc.cost >= 3 |=> 300 -> "give ES for expensive faction monster"
                        isNeutral && uc.cost >= 3 |=> 300 -> "give ES for expensive neutral monster"

                    // ── [2026-05-24] FULL MNU SCORING AUDIT ──────────────────
                    // Per direction: every ability/SB/SBR/response gets a score.
                    // Magnitudes mirror existing MNU entries (-1000..+1000 range
                    // for preferences; -100000 for hard blocks; +100000 for
                    // forced choices). When in doubt, neutral positive (~200)
                    // so the action surfaces in bot reasoning without dominating
                    // strategic moves.

                    // ── Loyalty card pickup (general entry point) ───────────
                    case LoyaltyCardDoomAction(_) =>
                        // Take a loyalty card at doom phase if we don't already have one
                        val hasNMCard = self.loyaltyCards.of[NeutralMonsterLoyaltyCard].any
                        val hasTerrorCard = self.loyaltyCards.of[NeutralTerrorLoyaltyCard].any
                        val hasIGOO = self.loyaltyCards.of[IGOOLoyaltyCard].any
                        (!hasNMCard && !hasTerrorCard && !hasIGOO) |=> 2000 -> "obtain LC (none yet)"
                        (hasNMCard || hasTerrorCard) |=> -500 -> "already have a neutral card"
                        hasIGOO |=> -500 -> "already have IGOO"

                    // ── LC unit placement (initial place from new LC) ────────
                    case LoyaltyCardSummonAction(_, uc, r) =>
                        // Generally place at own gate if possible, else any sane area
                        self.gates.has(r) |=> 800 -> "place LC unit at own gate"
                        self.at(r).any |=> 400 -> "place LC unit near own units"
                        true |=> 200 -> "place LC unit"

                    // ── Ghast Frenzy chain (free summon) ────────────────────
                    case FreeSummonAction(_, _, r, _) =>
                        // Always good — free unit
                        self.gates.has(r) |=> 1000 -> "frenzy at own gate"
                        true |=> 500 -> "frenzy summon"

                    // ── Brown Jenkin Familiar respawn ───────────────────────
                    case BrownJenkinFamiliarPlaceAction(_, r, _) =>
                        // Prefer enemy gate (Loathsome Titter triggers there) over own gate
                        val enemyGate = self.enemies.exists(_.gates.has(r))
                        enemyGate |=> 600 -> "familiar at enemy gate (loathsome titter)"
                        self.gates.has(r) |=> 300 -> "familiar at own gate"
                        true |=> 100 -> "familiar respawn"

                    // ── Moonbeast: 2-power summon onto enemy SB (Action) ────
                    case MoonbeastSummonMainAction(_) =>
                        // Worth 2 power to block a key enemy SB
                        self.pool(MoonbeastUnit).any |=> 800 -> "use moonbeast block"

                    case MoonbeastChooseFactionAction(_, target) =>
                        // Target the enemy with most spellbooks (most to lose)
                        val maxSB = self.enemies./(_.spellbooks.num).max
                        (target.spellbooks.num == maxSB) |=> 600 -> "block enemy with most SBs"
                        // Or the leader
                        val maxDoom = self.enemies./(_.doom).max
                        (target.doom == maxDoom) |=> 400 -> "block leader"
                        true |=> 100 -> "moonbeast block faction"

                    case MoonbeastChooseSpellbookAction(_, target, sb) =>
                        // Prefer blocking already-earned SBs (immediate impact)
                        val earned = target.spellbooks.has(sb)
                        earned |=> 800 -> "block earned SB"
                        // BattleSpellbooks have more in-game impact
                        sb.isInstanceOf[BattleSpellbook] |=> 400 -> "block battle SB"
                        true |=> 200 -> "block any SB"

                    case MoonbeastInitialPlaceAction(_, target, sb) =>
                        // Same logic as ChooseSpellbookAction — pick highest-impact target
                        target.spellbooks.has(sb) |=> 800 -> "initial block earned SB"
                        sb.isInstanceOf[BattleSpellbook] |=> 400 -> "initial block battle SB"
                        true |=> 200 -> "initial moonbeast block"

                    case MoonbeastReturnChooseGateAction(_, _, r, _) =>
                        // Return MB to own gate if possible
                        self.gates.has(r) |=> 500 -> "return moonbeast to own gate"
                        true |=> 200 -> "return moonbeast"

                    // ── Dimensional Shambler (deploy + summon) ──────────────
                    case ShamblerSummonMainAction(_) =>
                        // Park on Faction Card for later pre-battle deploy
                        self.pool(DimensionalShamblerUnit).any |=> 500 -> "park shambler on card"

                    case ShamblerSummonAction(_) =>
                        true |=> 500 -> "summon shambler to faction card"

                    case ShamblerDeployMainAction(_, _) =>
                        // Always deploy when offered pre-battle (free unit on side)
                        true |=> 1000 -> "deploy shambler"

                    case ShamblerDeployAction(_, r, _) =>
                        // Deploy to battle area
                        true |=> 800 -> "deploy shambler to battle"

                    // ── IGOO awakening ──────────────────────────────────────
                    case AwakenIGOOMainAction(_) =>
                        // Always want to awaken if affordable
                        val hasIGOO = self.loyaltyCards.of[IGOOLoyaltyCard].any
                        !hasIGOO |=> 1500 -> "awaken IGOO"

                    case IndependentGOOMainAction(_, lc, _) =>
                        // Per-IGOO choice from the sub-menu
                        true |=> 1000 -> ("awaken IGOO " + lc.unit.name)

                    case IndependentGOOAction(_, lc, r, cost) =>
                        // Place at own gate where possible
                        self.gates.has(r) |=> 1000 -> "awaken IGOO at own gate"
                        self.at(r).any |=> 500 -> "awaken IGOO near units"
                        cost <= self.power / 2 |=> 200 -> "cheap awaken"
                        true |=> 300 -> "awaken IGOO"

                    // ── Cthugha-specific awaken (replace own GOO) ──────────
                    case CthughaAwakenMainAction(_) =>
                        self.allInPlay.%(_.uclass.utype == GOO).%(_.uclass != Cthugha).any |=> 1500 -> "use cthugha replace"

                    case CthughaAwakenAction(_, _, replacedGOO, cost) =>
                        // Replace cheapest GOO first
                        val isFactionGOO = replacedGOO.isInstanceOf[FactionUnitClass]
                        isFactionGOO |=> 800 -> "replace own faction GOO"
                        !isFactionGOO |=> 200 -> "replace own IGOO (loses LC + SB)"
                        cost <= 2 |=> 400 -> "cheap cthugha replace"

                    // ── Azathoth awaken (forum + dice) ──────────────────────
                    case AzathothAwakenMainAction(_) =>
                        true |=> 1500 -> "awaken azathoth"

                    // ── Ghatanothoa Mummify (Action 1 power) ────────────────
                    case GhatanotoaMummifyAction(_) =>
                        // Always want to mummify enemy cultists in area
                        true |=> 800 -> "use mummify action"

                    // ── Place Spinneret (target region, when offered) ───────
                    // PlaceSpinneretMainAction already scored above (line 553)

                    // ── Father Dagon Tsunami (Action) ───────────────────────
                    case FatherDagonTsunamiMainAction(_) =>
                        true |=> 800 -> "use father dagon tsunami"

                    case FatherDagonTsunamiTargetAction(_, r) =>
                        // Prefer area with most enemy cultists
                        val enemyCults = self.enemies./~(_.at(r, Cultist)).num
                        enemyCults >= 3 |=> 1000 -> "tsunami high-density"
                        enemyCults >= 1 |=> 500 -> "tsunami enemy cultists"
                        true |=> 100 -> "tsunami land"

                    // ── Mother Hydra Agony Sting (Action) ───────────────────
                    case MotherHydraAgonyStingMainAction(_) =>
                        true |=> 800 -> "use agony sting"

                    case MotherHydraAgonyStingTargetAction(_, r) =>
                        val enemyCults = self.enemies./~(_.at(r, Cultist)).num
                        enemyCults >= 3 |=> 1000 -> "agony sting high-density"
                        enemyCults >= 1 |=> 500 -> "agony sting enemy cultists"
                        true |=> 100 -> "agony sting ocean"

                    // ── Tsunami / Agony Sting cultist move (response) ───────
                    case TsunamiMoveCultistAction(_, u, dest, _, _) =>
                        // Cultist owner picks where forced cultist goes
                        self.gates.has(dest) |=> 500 -> "tsunami: move to own gate"
                        self.at(dest).any |=> 300 -> "tsunami: move near own units"
                        self.enemies.exists(_.gates.has(dest)) |=> -200 -> "tsunami: avoid enemy gate"
                        true |=> 100 -> "tsunami move"

                    // ── Innsmouth Look (Doom phase SB) ──────────────────────
                    case InnsmouthLookChooseAction(_, u, _) =>
                        // Remove cheapest acolyte; +6 power makes it worth it
                        !u.onGate |=> 400 -> "innsmouth: remove off-gate acolyte"
                        u.onGate |=> -200 -> "innsmouth: dont remove gate-controlling acolyte"

                    // ── The Zygote (MH SB Action) ───────────────────────────
                    case TheZygoteMainAction(_) =>
                        self.pool(Acolyte).num >= 3 |=> 1000 -> "zygote with pool acolytes"
                        true |=> 200 -> "use zygote"

                    case TheZygoteTargetAction(_, r, remaining) =>
                        self.gates.has(r) |=> 600 -> "zygote at own gate"
                        self.at(r).any |=> 300 -> "zygote near own units"
                        true |=> 100 -> "zygote target"

                    // ── Nuclear Chaos (Az SB Doom phase) ────────────────────
                    case NuclearChaosMainAction(_) =>
                        // Always trigger — random benefit + doom penalty for lowest
                        true |=> 800 -> "trigger nuclear chaos"

                    case NuclearChaosAdjustAction(_, rolls, adjust) =>
                        // Owner adjusts own roll +/- 1; prefer adjusting up
                        val myRoll = rolls(self)
                        (adjust == 1 && myRoll <= 5) |=> 500 -> "nuclear chaos: bump roll up"
                        (adjust == -1 && myRoll == 6) |=> 300 -> "nuclear chaos: bump 6→5"
                        true |=> 0 -> "nuclear chaos adjust"

                    case NuclearChaosKeepAction(_, rolls) =>
                        // Keep if my roll is already 5+
                        val myRoll = rolls(self)
                        myRoll >= 5 |=> 400 -> "nuclear chaos: keep high roll"
                        myRoll <= 2 |=> -200 -> "nuclear chaos: dont keep low roll"
                        true |=> 100 -> "nuclear chaos keep"

                    // ── Byatis: God of Forgetfulness (SB) ───────────────────
                    case GodOfForgetfulnessMainAction(_, _, _) =>
                        true |=> 400 -> "use god of forgetfulness"

                    case GodOfForgetfulnessAction(_, _, r) =>
                        // Pick region with most enemy cultists to suck in
                        val enemyCults = self.enemies./~(_.at(r, Cultist)).num
                        (enemyCults > 0) |=> enemyCults * 100 -> "GoF region density"
                        true |=> 100 -> "GoF region"

                    // ── Abhoth: Filth placement (SB) ────────────────────────
                    case FilthMainAction(_, _) =>
                        true |=> 500 -> "use filth ability"

                    case FilthAction(_, r) =>
                        // Place filth at enemy controlled gate to disable their SBs
                        val enemyGate = self.enemies.exists(_.gates.has(r))
                        enemyGate |=> 700 -> "filth at enemy gate"
                        true |=> 100 -> "filth"

                    // ── Nyogtha: Nightmare Web (SB) ─────────────────────────
                    case NightmareWebMainAction(_, _) =>
                        true |=> 800 -> "use nightmare web"

                    case NightmareWebAction(_, r) =>
                        self.gates.has(r) |=> 500 -> "nightmare web at own gate"
                        true |=> 200 -> "nightmare web"

                    // ── Tulzscha: Give Power SBR ────────────────────────────
                    case TulzschaGivePowerMainAction(_) =>
                        true |=> 800 -> "satisfy tulzscha SBR"

                    case TulzschaGivePowerAction(_) =>
                        true |=> 800 -> "give each enemy 2 power for SBR"

                    // ── Tulzscha: Ceremony of Annihilation (SB doom action) ─
                    case CeremonyOfAnnihilationChoiceAction(_) =>
                        // Replaces ritual with power gain = ritual cost; usually positive
                        true |=> 600 -> "use ceremony of annihilation"

                    // ── Yig: Remove Gate (SBR) ──────────────────────────────
                    case YigRemoveGateMainAction(_) =>
                        true |=> 500 -> "satisfy yig SBR (remove gate)"

                    case YigRemoveGateAction(_, r) =>
                        // Prefer removing a less valuable gate
                        val isOwn = self.gates.has(r)
                        isOwn |=> 200 -> "remove own gate for SBR"

                    // ── Ghatanothoa IGOO: Pay 3 Power SBR (Execration of Mu) ─
                    case GhatanotoaSBRPayAction(_) =>
                        // Pay 3 power for Execration of Mu SB
                        self.power >= 5 |=> 600 -> "pay for execration of mu"
                        self.power >= 3 |=> 200 -> "barely afford execration"

                    // ── Messenger of Yig (donate / refuse) ──────────────────
                    case MessengerOfYigDonateAction(_, yigOwner) =>
                        // Donate 1 power; alternative is yig owner gets 1 doom
                        // Prefer donate if we have extra power
                        self.power > 5 |=> 500 -> "donate power (have plenty)"
                        self.power <= 2 |=> -300 -> "dont donate (low power)"
                        true |=> 100 -> "donate power to yig"

                    case MessengerOfYigRefuseAction(_, yigOwner) =>
                        // Refuse — yig owner gets 1 doom; bad if yig owner is leader
                        val yigDoom = yigOwner.doom
                        val maxDoom = self.enemies./(_.doom).max
                        yigDoom >= maxDoom - 2 |=> -500 -> "refuse: yig owner near doom lead"
                        true |=> 200 -> "refuse: dont feed yig owner"

                    // ── Velvet Fan capture (battle, BW owner picks) ─────────
                    case VelvetFanCaptureAction(_, uRef) =>
                        val u = game.unit(uRef)
                        val uc = u.uclass
                        // Prefer capturing high-value units
                        (uc.utype == GOO) |=> 1500 -> "velvet fan: capture GOO"
                        (uc.utype == Terror) |=> 1000 -> "velvet fan: capture Terror"
                        (uc.utype == Monster && uc.cost >= 2) |=> 500 -> "velvet fan: capture good Monster"
                        true |=> 200 -> "velvet fan: capture"

                    case VelvetFanSkipAction(_) =>
                        true |=> -300 -> "velvet fan: skip (prefer to capture something)"

                    // ── Fire Vampires (battle, CC/Ct owner) ─────────────────
                    case FireVampiresSpareAction(_, uRef) =>
                        val u = game.unit(uRef)
                        val uc = u.uclass
                        // Spare for 1 power; prefer sparing low-value units
                        (uc.utype == Cultist) |=> 600 -> "spare cultist for 1 power"
                        (uc.utype == Monster && uc.cost <= 2) |=> 400 -> "spare cheap monster"
                        // Don't spare high-value — they should die
                        (uc.utype == GOO) |=> -500 -> "dont spare enemy GOO"
                        (uc.utype == Terror) |=> -300 -> "dont spare enemy Terror"

                    case FireVampiresSkipAction(_) =>
                        true |=> -200 -> "fire vampires: skip (prefer to spare for power)"

                    // ── Prime Cause (battle, ES owner replaces own unit) ────
                    case PrimeCauseChooseUnitAction(_, uRef) =>
                        val u = game.unit(uRef)
                        // Replace own killed/spared units; not full-health alive
                        u.health match {
                            case Killed => true |=> 800 -> "prime cause: replace killed unit"
                            case Pained => true |=> 200 -> "prime cause: replace pained unit"
                            case _ =>
                        }
                        true |=> 100 -> "prime cause: choose unit"

                    case PrimeCauseChooseReplacementAction(_, _, newUC) =>
                        // Pick highest-value replacement
                        (newUC.utype == GOO) |=> 1000 -> "prime cause: bring GOO"
                        (newUC.utype == Terror) |=> 600 -> "prime cause: bring Terror"
                        (newUC.utype == Monster && newUC.cost >= 2) |=> 300 -> "prime cause: bring Monster"
                        true |=> 100 -> "prime cause: replacement"

                    case PrimeCauseSkipAction(_) =>
                        true |=> -200 -> "prime cause: skip"

                    // ── Dhole Planetary Destruction (opponent's choice) ─────
                    case DholePlanetaryDestructionDoomAction(_, _) =>
                        // Opponent picks own doom vs own power. Doom is worse for us.
                        // We're the opponent of dhole owner here. Pick LESS bad option.
                        self.power > 5 |=> 200 -> "take power penalty (have plenty)"
                        self.power <= 2 |=> -500 -> "dont take power loss when low"
                        true |=> 100 -> "planetary destruction: doom choice"

                    case DholePlanetaryDestructionPowerAction(_, _) =>
                        self.power > 5 |=> -300 -> "dont take power loss when high (prefer doom)"
                        self.power <= 2 |=> 500 -> "take doom when low power"
                        true |=> 200 -> "planetary destruction: power choice"

                    // ── Laughingstock (Penguin owner) ───────────────────────
                    case LaughingstockMoveAction(_, uRef) =>
                        // Move Penguin into battle area
                        true |=> 600 -> "laughingstock: move penguins"

                    case LaughingstockDoneAction(_) =>
                        // Skip — keep penguins where they are
                        true |=> 100 -> "laughingstock: keep penguins still"

                    case LaughingstockSideAction(_, side) =>
                        // Pick which side Penguins fight for; prefer the weaker / our ally
                        val isUs = side == self
                        isUs |=> 800 -> "laughingstock: penguins on our side"
                        // Else weakest enemy
                        val weakest = self.enemies./(_.str).min
                        (side.str == weakest) |=> 400 -> "laughingstock: penguins on weakest enemy"

                    // ── Yig Snakebite (battle, Yig owner picks Kill target) ─
                    case YigSnakebiteAssignAction(_, _, uc) =>
                        // Assign extra Kill to highest-value enemy unit
                        (uc.utype == GOO) |=> 1500 -> "snakebite: kill GOO"
                        (uc.utype == Terror) |=> 1000 -> "snakebite: kill Terror"
                        (uc.utype == Monster && uc.cost >= 2) |=> 500 -> "snakebite: kill good Monster"
                        true |=> 200 -> "snakebite: kill"

                    // ── Cthugha combat match (battle, Ct owner picks enemy GOO) ─
                    case CthughaCombatChooseGOOAction(_, _, goo, combat) =>
                        // Match the HIGHEST-combat enemy GOO
                        (combat >= 6) |=> 1000 -> "cthugha: match highest GOO combat"
                        (combat >= 4) |=> 500 -> "cthugha: match good GOO combat"
                        true |=> 200 -> "cthugha: match combat"

                    // ── Cronophage teleport (Hound battle-time) ─────────────
                    case CronophageTeleportAction(_, _, dest) =>
                        // Teleport to a gate where Hound contributes most
                        val enemyGate = self.enemies.exists(_.gates.has(dest))
                        enemyGate |=> 800 -> "cronophage: teleport to enemy gate"
                        self.gates.has(dest) |=> 400 -> "cronophage: teleport to own gate"
                        true |=> 200 -> "cronophage: teleport"

                    // ── Shantak Riding (carry cultist) ──────────────────────
                    case ShantakCarryCultistAction(_, _, _, r) =>
                        self.gates.has(r) |=> 600 -> "shantak ride: carry to own gate"
                        self.enemies.exists(_.gates.has(r)) |=> 400 -> "shantak ride: carry to enemy gate"
                        true |=> 200 -> "shantak ride: carry"

                    case _ =>

                }
            }

            result
        }

        // Add Library map-specific scores
        val mapScores = BotMaps.eval(actions, self).groupBy(_._1).view.mapValues(_./(t => Evaluation(t._2, t._3))).toMap

        actions./{ a => ActionEval(a, evalA(a) ++ mapScores.getOrElse(a, $)) }
    }
}
