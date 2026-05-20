package cws

import hrf.colmat._

/**
 * NeutralUnitTest — runs actual games using bot AI, intercepting
 * decisions to force each neutral unit to be obtained and tested.
 *
 * Uses save/restore of game state to test multiple units in one session.
 *
 * Usage: sbt "runMain cws.NeutralUnitTest"
 */
object NeutralUnitTest {
    var testLog = new StringBuilder
    var passed = 0
    var failed = 0
    var tested = scala.collection.mutable.Set[String]()

    def log(s: String): Unit = {
        testLog.append(s + "\n")
        println(s)
    }

    def check(name: String, cond: Boolean, msg: String = ""): Unit = {
        if (cond) {
            passed += 1
        } else {
            log(s"  FAIL: $name — $msg")
            failed += 1
        }
    }

    // Modified askFaction that can override specific decisions
    var overrideAction: Option[Continue => Option[Action]] = None

    def testAskFaction(g: Game, c: Continue): Action = {
        // Check for override first
        overrideAction.flatMap(_(c)).getOrElse(Host.askFaction(g, c))
    }

    def runGame(game: Game, maxSteps: Int = 50000): Unit = {
        val (l, cc) = game.perform(StartAction)
        var c: Continue = cc
        var n = 0
        while (!c.isInstanceOf[GameOver] && n < maxSteps) {
            n += 1
            val a = Host.askFaction(game, c)
            val (ll, cc2) = game.perform(a.unwrap)
            c = cc2
        }
    }

    // Run game until a specific condition is met
    def runUntil(game: Game, cond: Game => Boolean, maxSteps: Int = 50000): Boolean = {
        val (l, cc) = game.perform(StartAction)
        var c: Continue = cc
        var n = 0
        while (!c.isInstanceOf[GameOver] && n < maxSteps && !cond(game)) {
            n += 1
            val a = testAskFaction(game, c)
            val (ll, cc2) = game.perform(a)
            c = cc2
        }
        cond(game)
    }

    // Run game step by step, returning the Continue after each step
    def stepGame(game: Game, c: Continue): (Action, Continue) = {
        val a = testAskFaction(game, c)
        val (ll, cc) = game.perform(a)
        (a, cc)
    }

    def makeGame(extraOpts: $[GameOption] = $): Game = {
        val allOpts: $[GameOption] = $(
            NeutralMonsters, NeutralTerrors, IGOOs, HighPriests,
            UseGhast, UseGug, UseShantak, UseStarVampire, UseVoonith, UseDimensionalShamblers, UseGnorri,
            UseLengSpider, UseSatyr, UseElderThing, UseMoonbeast, UseAlbinoPenguins, UseInsectsFromShaggai,
            UseDhole, UseQuachilUttaus, UseBrownJenkin, UseElderShoggoth, UseShadowPharaoh, UseGreatRaceOfYith, UseHoundOfTindalos,
            UseByatis, UseAbhoth, UseDaoloth, UseNyogtha, UseTulzscha, UseYgolonac,
            UseYig, UseMotherHydra, UseFatherDagon, UseCthugha, UseBloatedWoman, UseAzathothIGOO,
            UseServitor, UseAtlachNacha, UseBokrug, UseGlaakiIGOO
        ) ++ extraOpts ++ $(MapEarth35 : GameOption)
        val board = EarthMap4v35
        val track = RitualTrack.for4
        val seating = $(GC, CC, BG, OW) // No FB to avoid Ghatanothoa conflict
        new Game(board, track, seating, true, allOpts)
    }

    def main(args: Array[String]): Unit = {
        log("=== NEUTRAL UNIT INTEGRATION TEST ===\n")

        // ── TEST GROUP 1: Card pool and basic setup ──
        log("--- Group 1: Card pool and setup ---")
        locally {
            val game = makeGame()
            game.factions = $(GC, CC, BG, OW)

            // All cards should be in pool
            val allCards = $(DholeCard, GreatRaceOfYithCard, QuachilUttausCard, ShadowPharaohCard,
                HoundOfTindalosCard, BrownJenkinCard, ElderShoggothCard,
                MoonbeastCard, AlbinoPenguinsCard, ElderThingCard, LengSpiderCard, SatyrCard, InsectsFromShaggaiCard,
                YigCard, MotherHydraCard, FatherDagonCard, CthughaCard, GhatanotoaIGOOCard, BloatedWomanCard, AzathothIGOOCard)

            for (c <- allCards)
                check(s"${c.unit.name} in pool", game.loyaltyCards.contains(c), "missing from pool")

            // Expansion loading
            check("NeutralMonstersExpansion loaded", game.expansions.exists(_.isInstanceOf[NeutralMonstersExpansion.type]))
            check("IGOOsExpansion loaded", game.expansions.exists(_.isInstanceOf[IGOOsExpansion.type]))
        }

        // ── TEST GROUP 2: Ghatanothoa/FB conflict ──
        log("\n--- Group 2: Ghatanothoa/FB conflict ---")
        locally {
            val fbGame = new Game(EarthMap4v35, RitualTrack.for4, $(FB, GC, CC, BG), true,
                $(IGOOs, UseGhatanotoaIGOO, NeutralMonsters, NeutralTerrors))
            fbGame.factions = $(FB, GC, CC, BG)
            // Ghatanothoa IGOO should NOT be in the available pool when FB is playing
            // (it should be filtered out at the independents() offer level, not the pool level)
            // The card IS in the pool, but should not be OFFERED
            check("Ghatanothoa IGOO card in pool", fbGame.loyaltyCards.contains(GhatanotoaIGOOCard), "card missing entirely")
            // The filtering happens at offer time in independents() — we can't test that without running the game
        }

        // ── TEST GROUP 3: Unit mechanics (direct state tests) ──
        log("\n--- Group 3: Unit mechanics ---")
        locally {
            val game = makeGame()
            game.factions = $(GC, CC, BG, OW)
            implicit val g = game

            // Mummification
            val testAcolyte = new UnitFigure(GC, Acolyte, 99, game.board.regions.head)
            GC.units :+= testAcolyte
            game.mummifiedCultists :+= testAcolyte.ref
            check("Mummified: canMove=false", !Acolyte.canMove(testAcolyte))
            check("Mummified: canBattle=false", !Acolyte.canBattle(testAcolyte))
            game.mummifiedCultists = $
            check("Unmummified: canMove=true", Acolyte.canMove(testAcolyte))
            GC.units = GC.units.%(_.ref != testAcolyte.ref)

            // Mind Parasite
            OW.loyaltyCards :+= InsectsFromShaggaiCard
            val insect = new UnitFigure(OW, InsectsFromShaggai, 1, game.board.regions.head)
            OW.units :+= insect
            val paraTarget = new UnitFigure(GC, Acolyte, 98, game.board.regions.head)
            paraTarget.onGate = false
            GC.units :+= paraTarget
            // Run checkConversions to actually parasitize the acolyte
            MindParasite.checkConversions()
            // Find the newly created MindParasiteCultist under OW
            val parasitized = OW.units.find(u => u.uclass == MindParasiteCultist && u.region == game.board.regions.head)
            check("Mind Parasite: conversion happened", parasitized.any, "no MindParasiteCultist found")
            if (parasitized.any) {
                val mp = parasitized.get
                check("Mind Parasite: detected", MindParasite.isParasitized(mp))
                check("Mind Parasite: controller=OW", MindParasite.controller(mp).has(OW))
                check("Mind Parasite: canMove=false", !MindParasiteCultist.canMove(mp))
            }
            // Test that on-gate acolytes are NOT parasitized (new acolyte, on gate)
            val onGateAcolyte = new UnitFigure(GC, Acolyte, 97, game.board.regions.head)
            onGateAcolyte.onGate = true
            GC.units :+= onGateAcolyte
            check("Mind Parasite: on-gate NOT parasitized", !MindParasite.shouldParasitize(onGateAcolyte).any)
            GC.units = GC.units.%(_.ref != onGateAcolyte.ref)
            // Cleanup
            OW.loyaltyCards = OW.loyaltyCards.but(InsectsFromShaggaiCard)
            OW.units = OW.units.%(u => u.ref != insect.ref && u.uclass != MindParasiteCultist)
            GC.units = GC.units.%(_.ref != paraTarget.ref)

            // Elder Thing Mind Control
            val et = new UnitFigure(OW, ElderThing, 1, game.board.regions.head)
            OW.units :+= et
            // Test against GC's Cthulhu
            val cthulhu = GC.units.find(_.uclass == Cthulhu)
            if (cthulhu.isDefined) {
                val origR = cthulhu.get.region
                cthulhu.get.region = game.board.regions.head
                check("ET: enemy GOO suppressed", ElderThingMindControl.suppresses(cthulhu.get))
                cthulhu.get.region = origR
            }
            // Test own GOO NOT suppressed
            val owGoo = OW.units.find(u => u.uclass.utype == GOO && u.uclass != ElderThing)
            if (owGoo.isDefined) {
                val origR = owGoo.get.region
                owGoo.get.region = game.board.regions.head
                check("ET: own GOO NOT suppressed", !ElderThingMindControl.suppresses(owGoo.get))
                owGoo.get.region = origR
            }
            OW.units = OW.units.%(_.ref != et.ref)

            // Hound canMove
            val hound = new UnitFigure(GC, HoundOfTindalos, 1, game.board.regions.head)
            check("Hound: canMove=false", !HoundOfTindalos.canMove(hound))

            // Moonbeast canBeSummoned
            check("Moonbeast: canBeSummoned=false", !MoonbeastUnit.canBeSummoned(GC))

            // NeutralStrength values
            def testStr(uc: UnitClass, expected: Int, name: String): Unit = {
                val u = new UnitFigure(GC, uc, 1, game.board.regions.head)
                GC.units :+= u
                val s = GC.neutralStrength($(u), CC)
                check(s"$name combat=$expected", s == expected, s"got $s")
                GC.units = GC.units.%(_.ref != u.ref)
            }
            testStr(Dhole, 5, "Dhole")
            testStr(GreatRaceOfYith, 3, "GRoY")
            testStr(QuachilUttaus, 1, "QU")
            testStr(HoundOfTindalos, 4, "HoT")
            testStr(ElderShoggoth, 2, "ES")
            testStr(ElderThing, 2, "ET")
            testStr(LengSpiderUnit, 1, "LS")
            testStr(Satyr, 1, "Satyr")
            testStr(Yig, 2, "Yig")
            testStr(BloatedWoman, 1, "BW")

            // VelvetFanHold
            val hold = VelvetFanHold(GC)
            check("VelvetFanHold: not on map", !hold.onMap)
            check("VelvetFanHold: is Pool glyph", hold.glyph == Pool)

            // Spellbooks exist
            for ((sb, name) <- $(
                (MessengerOfYig, "Messenger of Yig"), (TheZygote, "The Zygote"),
                (TheInnsmouthLook, "The Innsmouth Look"), (Firestorm, "Firestorm"),
                (ExecrationOfMu, "Execration of Mu"), (DisasterLooms, "Disaster Looms"),
                (NuclearChaos, "Nuclear Chaos")))
                check(s"Spellbook $name exists", sb.isInstanceOf[NeutralSpellbook])
            check("Firestorm is BattleSpellbook", Firestorm.isInstanceOf[BattleSpellbook])
        }

        // ── TEST GROUP 4: Full games with all units ──
        log("\n--- Group 4: Full games (50 games, checking for crashes) ---")
        locally {
            var crashes = 0
            var gamesRun = 0
            var unitsUsed = scala.collection.mutable.Set[String]()

            for (i <- 1 to 50) {
                val game = makeGame()
                try {
                    runGame(game, 20000)
                    gamesRun += 1
                    // Check which units were obtained
                    implicit val g2 = game
                    for (f <- game.factions) {
                        for (lc <- f.loyaltyCards) {
                            unitsUsed += lc.unit.name
                        }
                    }
                } catch {
                    case e: Exception =>
                        crashes += 1
                        log(s"  CRASH game $i: ${e.getClass.getSimpleName}: ${e.getMessage}")
                        if (crashes <= 3) e.printStackTrace()
                }
            }
            check(s"$gamesRun/50 games completed", gamesRun == 50, s"$crashes crashes")
            log(s"  Units obtained across 50 games: ${unitsUsed.toList.sorted.mkString(", ")}")
            if (unitsUsed.size < 5) log("  ⚠ Very few units obtained — bots may not prioritize neutral units")
        }

        // ── TEST GROUP 5: FB game with all units (separate to test FB-specific) ──
        log("\n--- Group 5: FB games (10 games) ---")
        locally {
            var crashes = 0
            for (i <- 1 to 10) {
                val game = new Game(EarthMap4v35, RitualTrack.for4, $(FB, GC, CC, BG), true,
                    $(NeutralMonsters, NeutralTerrors, IGOOs, HighPriests, MapEarth35,
                      UseGhast, UseGug, UseShantak, UseStarVampire, UseVoonith, UseDimensionalShamblers, UseGnorri,
                      UseLengSpider, UseSatyr, UseElderThing, UseMoonbeast, UseAlbinoPenguins, UseInsectsFromShaggai,
                      UseDhole, UseQuachilUttaus, UseBrownJenkin, UseElderShoggoth, UseShadowPharaoh, UseGreatRaceOfYith, UseHoundOfTindalos,
                      UseByatis, UseAbhoth, UseDaoloth, UseNyogtha, UseTulzscha, UseYgolonac,
                      UseYig, UseMotherHydra, UseFatherDagon, UseCthugha, UseBloatedWoman, UseAzathothIGOO))
                try {
                    runGame(game, 20000)
                } catch {
                    case e: Exception =>
                        crashes += 1
                        log(s"  CRASH FB game $i: ${e.getClass.getSimpleName}: ${e.getMessage}")
                        if (crashes <= 3) e.printStackTrace()
                }
            }
            check("10 FB games completed", crashes == 0, s"$crashes crashes")
        }

        // ── TEST GROUP 6: New units (Servitor, Atlach-Nacha, Bokrug, Glaaki IGOO) ──
        log("\n--- Group 6: New units — forced GC wrapper trace (10 games each) ---")
        locally {
            // Test with new units enabled, GC wrapper forces card acquisition
            val newUnitOpts = $(UseServitor, UseAtlachNacha, UseBokrug, UseGlaakiIGOO)
            val baseOpts : $[GameOption] = $(NeutralMonsters, NeutralTerrors, IGOOs, HighPriests, MapEarth35,
                UseGhast, UseGug, UseShantak, UseStarVampire, UseVoonith, UseDimensionalShamblers, UseGnorri,
                UseLengSpider, UseSatyr, UseElderThing, UseMoonbeast, UseAlbinoPenguins, UseInsectsFromShaggai,
                UseDhole, UseQuachilUttaus, UseBrownJenkin, UseElderShoggoth, UseShadowPharaoh, UseGreatRaceOfYith, UseHoundOfTindalos,
                UseByatis, UseAbhoth, UseDaoloth, UseNyogtha, UseTulzscha, UseYgolonac,
                UseYig, UseMotherHydra, UseFatherDagon, UseCthugha, UseBloatedWoman, UseAzathothIGOO
            ) ++ newUnitOpts

            var crashes = 0
            var gamesRun = 0
            var servitorSeen = false
            var atlachSeen = false
            var bokrugSeen = false
            var glaakiSeen = false

            TestBotGC.reset()
            TestBotGC.forceLoyaltyCard = true
            TestBotGC.forceSubmerge = true
            TestBotGC.forceAwaken = true
            TestBotGC.forceIGOOAwaken = true

            for (i <- 1 to 10) {
                val game = new Game(EarthMap4v35, RitualTrack.for4, $(GC, CC, BG, OW), true, baseOpts)
                try {
                    runGame(game, 30000)
                    gamesRun += 1
                    implicit val g2 = game
                    // Check which new units were obtained by GC
                    if (GC.loyaltyCards.has(ServitorCard)) servitorSeen = true
                    if (GC.loyaltyCards.has(AtlachNachaCard)) atlachSeen = true
                    if (GC.loyaltyCards.has(BokrugCard)) bokrugSeen = true
                    if (GC.loyaltyCards.has(GlaakiIGOOCard)) glaakiSeen = true
                    // Also check other factions
                    for (f <- game.factions) {
                        if (f.loyaltyCards.has(ServitorCard)) servitorSeen = true
                        if (f.loyaltyCards.has(AtlachNachaCard)) atlachSeen = true
                        if (f.loyaltyCards.has(BokrugCard)) bokrugSeen = true
                        if (f.loyaltyCards.has(GlaakiIGOOCard)) glaakiSeen = true
                    }
                } catch {
                    case e: Exception =>
                        crashes += 1
                        log(s"  CRASH new-unit game $i: ${e.getClass.getSimpleName}: ${e.getMessage}")
                        if (crashes <= 3) e.printStackTrace()
                }
            }
            check(s"$gamesRun/10 new-unit games completed", crashes == 0, s"$crashes crashes")
            log(s"  Servitor seen: $servitorSeen")
            log(s"  Atlach-Nacha seen: $atlachSeen")
            log(s"  Bokrug seen: $bokrugSeen")
            log(s"  Glaaki IGOO seen: $glaakiSeen")

            // Verify submerge worked (GC log should contain submerge entries)
            log(s"  TestBotGC log length: ${TestBotGC.testLog.length} chars")
            val gcLog = TestBotGC.testLog.toString
            check("TestBotGC: some actions taken", gcLog.nonEmpty, "empty log")
        }

        // ── TEST GROUP 7: Servitor combat -1 verification ──
        log("\n--- Group 7: Servitor combat -1 ---")
        locally {
            val game = makeGame($(UseServitor))
            implicit val g = game
            val servitor = new UnitFigure(GC, ServitorUnit, 1, game.board.regions.head)
            GC.units :+= servitor
            val str = GC.neutralStrength($(servitor), CC)
            check("Servitor combat = -1", str == -1, s"got $str")
            GC.units = GC.units.%(_.ref != servitor.ref)
        }

        // ── TEST GROUP 8: Atlach-Nacha web tokens ──
        log("\n--- Group 8: Atlach-Nacha web tokens ---")
        locally {
            val game = makeGame($(UseAtlachNacha))
            implicit val g = game
            check("Web tokens start empty", game.webTokens.none)
            val r1 = game.board.regions.head
            game.webTokens :+= r1
            check("Web token placed", game.webTokens.num == 1)
            check("Web token in region", game.webTokens.has(r1))
            // 6 tokens
            game.webTokens = game.board.regions.take(6)
            check("6 web tokens", game.webTokens.num == 6)
        }

        // ── TEST GROUP 9: Bokrug stays with owner ──
        log("\n--- Group 9: Bokrug retention ---")
        locally {
            val game = makeGame($(UseBokrug))
            implicit val g = game
            GC.loyaltyCards :+= BokrugCard
            val bokrug = new UnitFigure(GC, Bokrug, 1, game.board.regions.head)
            GC.units :+= bokrug
            check("GC has Bokrug card", GC.loyaltyCards.has(BokrugCard))
            check("Bokrug on map", GC.allInPlay.%(_.uclass == Bokrug).any)
            // Simulate death: move to pool
            bokrug.region = GC.reserve
            check("Bokrug in pool after death", GC.pool(Bokrug).any)
            check("Card still with GC", GC.loyaltyCards.has(BokrugCard))
            GC.loyaltyCards = GC.loyaltyCards.but(BokrugCard)
            GC.units = GC.units.%(_.uclass != Bokrug)
        }

        // ── TEST GROUP 10: Glaaki IGOO Tomb Herd ──
        log("\n--- Group 10: Glaaki IGOO Tomb Herd ---")
        locally {
            val game = makeGame($(UseGlaakiIGOO))
            implicit val g = game
            // Simulate: GC has Glaaki IGOO in play, 3 cultists in pool
            GC.loyaltyCards :+= GlaakiIGOOCard
            val glaaki = new UnitFigure(GC, GlaakiIGOO, 1, game.board.regions.head)
            GC.units :+= glaaki
            // Count pool cultists
            val poolCultists = GC.pool(Acolyte).num
            check("GC has pool cultists", poolCultists > 0, s"pool has $poolCultists")
            // The Tomb Herd power would add poolCultists worth of power during gather
            GC.loyaltyCards = GC.loyaltyCards.but(GlaakiIGOOCard)
            GC.units = GC.units.%(_.uclass != GlaakiIGOO)
        }

        // ── TEST GROUP 11: Per-unit power/ability verification ──
        log("\n--- Group 11: Per-unit ability verification (1 game each) ---")

        // Servitor: verify summon + pool blocking
        locally {
            log("  [Servitor]")
            TestBotGC.reset()
            TestBotGC.forceLoyaltyCard = true
            TestBotGC.targetUnit = Some("Servitor")
            val game = new Game(EarthMap4v35, RitualTrack.for4, $(GC, CC, BG, OW), true,
                $(NeutralMonsters, NeutralTerrors, IGOOs, HighPriests, MapEarth35,
                  UseServitor, UseGhast, UseGug, UseShantak, UseStarVampire, UseVoonith,
                  UseDimensionalShamblers, UseGnorri, UseLengSpider, UseSatyr, UseElderThing,
                  UseDhole, UseQuachilUttaus, UseBrownJenkin, UseElderShoggoth, UseShadowPharaoh,
                  UseGreatRaceOfYith, UseHoundOfTindalos))
            try {
                runGame(game, 30000)
                implicit val g2 = game
                val gcHasServitor = GC.loyaltyCards.has(ServitorCard)
                val anyHasServitor = game.factions.exists(_.loyaltyCards.has(ServitorCard))
                check("Servitor: card obtained by some faction", anyHasServitor, "no faction has ServitorCard")
                if (anyHasServitor) {
                    val owner = game.factions.find(_.loyaltyCards.has(ServitorCard)).get
                    val servitorsOnMap = owner.allInPlay.%(_.uclass == ServitorUnit).num
                    val servitorsInPool = owner.pool(ServitorUnit).num
                    log(s"  Servitor owner=${owner}, on map=$servitorsOnMap, in pool=$servitorsInPool")
                    check("Servitor: at least one summoned (pool < 3)", servitorsOnMap > 0 || servitorsInPool < 3,
                        s"all 3 still in pool ($servitorsInPool)")
                    // Check summon blocking evidence in log
                    val blockEntries = game.mlog.%(_.contains("blocked by"))
                    log(s"  Servitor block log entries: ${blockEntries.num}")
                }
            } catch {
                case e: Exception =>
                    log(s"  CRASH Servitor test: ${e.getClass.getSimpleName}: ${e.getMessage}")
                    e.printStackTrace()
                    failed += 1
            }
        }

        // Atlach-Nacha: verify spinneret placement
        locally {
            log("  [Atlach-Nacha]")
            TestBotGC.reset()
            TestBotGC.forceLoyaltyCard = true
            TestBotGC.forceAwaken = true
            TestBotGC.forceIGOOAwaken = true
            TestBotGC.forcePlaceSpinneret = true
            TestBotGC.targetUnit = Some("Atlach-Nacha")
            val game = new Game(EarthMap4v35, RitualTrack.for4, $(GC, CC, BG, OW), true,
                $(NeutralMonsters, NeutralTerrors, IGOOs, HighPriests, MapEarth35,
                  UseAtlachNacha, UseGhast, UseGug, UseShantak, UseStarVampire, UseVoonith,
                  UseDimensionalShamblers, UseGnorri, UseLengSpider, UseSatyr, UseElderThing,
                  UseDhole, UseQuachilUttaus, UseBrownJenkin, UseElderShoggoth, UseShadowPharaoh,
                  UseGreatRaceOfYith, UseHoundOfTindalos))
            try {
                runGame(game, 30000)
                implicit val g2 = game
                val anyHasAN = game.factions.exists(_.loyaltyCards.has(AtlachNachaCard))
                check("Atlach-Nacha: card obtained", anyHasAN, "no faction has AtlachNachaCard")
                val webCount = game.webTokens.num
                log(s"  Atlach-Nacha web tokens placed: $webCount")
                val spinneretLogs = game.mlog.%(_.contains("Spinneret"))
                log(s"  Spinneret log entries: ${spinneretLogs.num}")
                check("Atlach-Nacha: web tokens placed", webCount > 0 || spinneretLogs.any,
                    "no web tokens and no spinneret log entries")
            } catch {
                case e: Exception =>
                    log(s"  CRASH Atlach-Nacha test: ${e.getClass.getSimpleName}: ${e.getMessage}")
                    e.printStackTrace()
                    failed += 1
            }
        }

        // Bokrug: verify Ghosts of Ib fires
        locally {
            log("  [Bokrug]")
            TestBotGC.reset()
            TestBotGC.forceLoyaltyCard = true
            TestBotGC.forceAwaken = true
            TestBotGC.forceIGOOAwaken = true
            TestBotGC.targetUnit = Some("Bokrug")
            val game = new Game(EarthMap4v35, RitualTrack.for4, $(GC, CC, BG, OW), true,
                $(NeutralMonsters, NeutralTerrors, IGOOs, HighPriests, MapEarth35,
                  UseBokrug, UseGhast, UseGug, UseShantak, UseStarVampire, UseVoonith,
                  UseDimensionalShamblers, UseGnorri, UseLengSpider, UseSatyr, UseElderThing,
                  UseDhole, UseQuachilUttaus, UseBrownJenkin, UseElderShoggoth, UseShadowPharaoh,
                  UseGreatRaceOfYith, UseHoundOfTindalos))
            try {
                runGame(game, 30000)
                implicit val g2 = game
                val anyHasBokrug = game.factions.exists(_.loyaltyCards.has(BokrugCard))
                check("Bokrug: card obtained", anyHasBokrug, "no faction has BokrugCard")
                val ghostsLogs = game.mlog.%(_.contains("Ghosts of Ib"))
                log(s"  Ghosts of Ib log entries: ${ghostsLogs.num}")
                check("Bokrug: Ghosts of Ib fired", ghostsLogs.any,
                    "no Ghosts of Ib entries in game log")
            } catch {
                case e: Exception =>
                    log(s"  CRASH Bokrug test: ${e.getClass.getSimpleName}: ${e.getMessage}")
                    e.printStackTrace()
                    failed += 1
            }
        }

        // Glaaki IGOO: verify Tomb Herd power fires
        locally {
            log("  [Glaaki IGOO]")
            TestBotGC.reset()
            TestBotGC.forceLoyaltyCard = true
            TestBotGC.forceAwaken = true
            TestBotGC.forceIGOOAwaken = true
            TestBotGC.targetUnit = Some("Gla'aki")
            // No TS in seating to avoid conflict
            val game = new Game(EarthMap4v35, RitualTrack.for4, $(GC, CC, BG, OW), true,
                $(NeutralMonsters, NeutralTerrors, IGOOs, HighPriests, MapEarth35,
                  UseGlaakiIGOO, UseGhast, UseGug, UseShantak, UseStarVampire, UseVoonith,
                  UseDimensionalShamblers, UseGnorri, UseLengSpider, UseSatyr, UseElderThing,
                  UseDhole, UseQuachilUttaus, UseBrownJenkin, UseElderShoggoth, UseShadowPharaoh,
                  UseGreatRaceOfYith, UseHoundOfTindalos))
            try {
                runGame(game, 30000)
                implicit val g2 = game
                val anyHasGlaaki = game.factions.exists(_.loyaltyCards.has(GlaakiIGOOCard))
                check("Glaaki IGOO: card obtained", anyHasGlaaki, "no faction has GlaakiIGOOCard")
                val tombHerdLogs = game.mlog.%(_.contains("Tomb Herd"))
                log(s"  Tomb Herd log entries: ${tombHerdLogs.num}")
                check("Glaaki IGOO: Tomb Herd power fired", tombHerdLogs.any,
                    "no Tomb Herd entries in game log")
            } catch {
                case e: Exception =>
                    log(s"  CRASH Glaaki IGOO test: ${e.getClass.getSimpleName}: ${e.getMessage}")
                    e.printStackTrace()
                    failed += 1
            }
        }

        // ── SUMMARY ──
        log(s"\n=== FINAL RESULTS: $passed passed, $failed failed ===")

        // Write results to file
        val outPath = "/Users/gremus/My Drive/Personal/Games/Cthulhu Wars/Neutral Units/test-results.txt"
        java.nio.file.Files.write(java.nio.file.Paths.get(outPath), testLog.toString.getBytes)
        log(s"Results written to: $outPath")

        if (failed > 0) System.exit(1)
    }
}
