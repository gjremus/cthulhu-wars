package cws

import hrf.colmat._
import scala.collection.parallel.CollectionConverters._
import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets

/**
 * SimRunner — sim-specific entry point.
 *
 * Args: <players> <games> [faction1 faction2 ...] [flags...]
 *   players  : 3, 4, or 5 (ignored if --rand)
 *   games    : number of games to run
 *   factions : optional list of required factions (gc cc bg ys sl ww ow an ts fb ds)
 *   --hp     : include High Priests (each faction gets 1 HP at start)
 *   --nm     : include all Neutral Monsters (Ghast, Gug, Shantak, StarVampire, Voonith, DS, Gnorri)
 *   --igoo   : include all iGOOs (Byatis, Abhoth, Daoloth, Nyogtha, Tulzscha, Ygolonac)
 *   --rand   : randomize player count per game (3, 4, or 5)
 *   --rand-flags : randomly toggle HP/NM/iGOO (50%) and other flags (30%) per game
 *   --opener10 : Opener4P10Gates flag
 *   --demand-tsat : DemandTsathoggua flag (SL needs Tsathoggua)
 *   --neutral-sb : NeutralSpellbooks
 *   --ice-lethargy : IceAgeAffectsLethargy
 *   --gate-dip : GateDiplomacy
 *
 * Structured output lines (parsed by tune_ts.py):
 *   GAME|factions=...|players=...|winner=...|turns=...[|ts_*=...]
 *   GAME_ERROR|factions=...|players=...|error=...
 *
 * All other output is human-readable progress info.
 *
 * Example commands (run from sim/ directory):
 *   sbt "compile; runMain cws.SimRunner 4 200 ts"
 *   sbt "compile; runMain cws.SimRunner 4 200 ts --hp --nm --igoo"
 *   sbt "compile; runMain cws.SimRunner rand 200 ts --hp --nm --igoo"
 *
 * Or from the project root with absolute path:
 *   cd "/Users/gremus/cthulhu-wars TS and RoA/sim" && sbt "compile; runMain cws.SimRunner 4 200 ts --hp"
 */
object SimRunner {
    def main(args : Array[String]) {
        // ── REPLAY MODE: replay an action file and output log ──────────────
        if (args.length >= 2 && args(0) == "--replay") {
            val inputPath = args(1)
            val outputPath = if (args.length > 2) args(2) else inputPath.replace(".txt", "-log.txt")
            val lines = scala.io.Source.fromFile(inputPath).getLines().toList.map(_.trim).filter(_.nonEmpty)

            // Parse header: "Cthulhu Wars 1.18 Replay" then "BG-YS-SL-FB MapEarth35 PlayerCount(4)"
            val headerLine = lines.find(_.contains("Map")).getOrElse(lines(1))
            val factionCodes = headerLine.split(" ")(0).split("-").toList
            val mapName = headerLine.split(" ").find(_.startsWith("Map")).getOrElse("MapEarth35").replace("Map", "")
            val pcMatch = "PlayerCount\\((\\d+)\\)".r.findFirstMatchIn(headerLine)
            val pc = pcMatch.map(_.group(1).toInt).getOrElse(factionCodes.size)

            val board = mapName match {
                case "Earth33" => EarthMap3
                case "Earth35" | "Earth4v35" => EarthMap4v35
                case "Earth53" | "Earth4v53" => EarthMap4v53
                case "Earth55" => EarthMap5
                case _ => EarthMap4v35
            }
            val ritualTrack = pc match {
                case 3 => RitualTrack.for3
                case 5 => RitualTrack.for5
                case _ => RitualTrack.for4
            }

            // Parse options from header
            var opts : $[GameOption] = $
            if (headerLine.contains("HighPriests")) opts :+= HighPriests
            if (headerLine.contains("UseGhast")) opts :+= UseGhast
            if (headerLine.contains("UseGug")) opts :+= UseGug
            if (headerLine.contains("UseShantak")) opts :+= UseShantak
            if (headerLine.contains("UseStarVampire")) opts :+= UseStarVampire
            if (headerLine.contains("UseVoonith")) opts :+= UseVoonith
            if (headerLine.contains("UseDimensionalShamblers")) opts :+= UseDimensionalShamblers
            if (headerLine.contains("UseGnorri")) opts :+= UseGnorri
            if (headerLine.contains("UseByatis")) opts :+= UseByatis
            if (headerLine.contains("UseAbhoth")) opts :+= UseAbhoth
            if (headerLine.contains("UseDaoloth")) opts :+= UseDaoloth
            if (headerLine.contains("UseNyogtha")) opts :+= UseNyogtha
            if (headerLine.contains("UseTulzscha")) opts :+= UseTulzscha
            if (headerLine.contains("UseYgolonac")) opts :+= UseYgolonac
            if (headerLine.contains("Opener4P10Gates")) opts :+= Opener4P10Gates

            def parseFactionReplay(s : String) : Faction = s match {
                case "GC" => GC; case "CC" => CC; case "BG" => BG; case "YS" => YS
                case "SL" => SL; case "WW" => WW; case "OW" => OW; case "AN" => AN
                case "TS" => TS; case "FB" => FB; case "DS" => DS
            }
            val seating = factionCodes.map(parseFactionReplay)
            val game = new Game(board, ritualTrack, seating, true, opts)
            val serializer = new Serialize(game)

            // Filter to action lines only (skip header, skip log divs)
            val actionLines = lines.filter(l =>
                !l.startsWith("Cthulhu Wars") && !l.contains("Map") && !l.startsWith("<div") &&
                !l.startsWith("Options") && l.nonEmpty)

            var log : $[String] = $
            def writeLog(s : String) { log :+= s }

            println("Replaying " + actionLines.size + " actions for " + seating.map(_.short).mkString("-") + " on " + mapName + " (" + pc + "p)")

            val (startLog, startCont) = game.perform(StartAction)
            startLog.foreach(writeLog)
            var c = startCont
            var actionIdx = 0

            try {
                while (!c.isInstanceOf[GameOver] && actionIdx < actionLines.size) {
                    val actionStr = actionLines(actionIdx).replace("&gt;", ">")
                    try {
                        val action = serializer.parseAction(actionStr)
                        val (ll, cc) = game.perform(action)
                        ll.foreach(writeLog)
                        c = cc
                    } catch {
                        case e : Throwable =>
                            writeLog("<div class='p'>REPLAY ERROR at action " + actionIdx + ": " + actionStr.take(80) + "</div>")
                    }
                    actionIdx += 1
                }
            } catch {
                case _ : Throwable =>
                    writeLog("<div class='p'>REPLAY ENDED at action " + actionIdx + "</div>")
            }

            // Write output: action lines + log lines (same format as SimRunner save)
            val writer = new java.io.PrintWriter(new java.io.File(outputPath))
            actionLines.foreach(writer.println)
            writer.println()
            writer.println()
            log.foreach(s => writer.println("<div class='p'>" + s + "</div>"))
            writer.close()

            println("Written " + actionLines.size + " actions + " + log.size + " log lines to " + outputPath)
            return
        }

        val allFactions = $(GC, CC, BG, YS, SL, WW, OW, AN, TS, FB, DS)

        // ── Flag parsing ───────────────────────────────────────────────────
        val withHP    = args.contains("--hp")
        val withNM    = args.contains("--nm")
        val withIGOO  = args.contains("--igoo")
        val withRand  = args.contains("--rand")
        val withTrace = args.contains("--trace")
        val saveAll   = args.contains("--save-all")
        val withOpener10 = args.contains("--opener10")
        val withDemandTsat = args.contains("--demand-tsat")
        val withNeutralSB = args.contains("--neutral-sb")
        val withIceLethargy = args.contains("--ice-lethargy")
        val withGateDip = args.contains("--gate-dip")
        val withRandFlags = args.contains("--rand-flags")
        val cleanArgs = args.filterNot(Set("--hp", "--nm", "--igoo", "--rand", "--trace", "--save-all",
            "--opener10", "--demand-tsat", "--neutral-sb", "--ice-lethargy", "--gate-dip", "--rand-flags").contains)

        val defaultPlayers = if (cleanArgs.length > 0 && cleanArgs(0) != "rand") cleanArgs(0).toInt else 4
        val numberOfGames  = if (cleanArgs.length > 1) cleanArgs(1).toInt else 100

        def parseFaction(s : String) : Option[Faction] = s.toLowerCase match {
            case "gc" => Some(GC)
            case "cc" => Some(CC)
            case "bg" => Some(BG)
            case "ys" => Some(YS)
            case "sl" => Some(SL)
            case "ww" => Some(WW)
            case "ow" => Some(OW)
            case "an" => Some(AN)
            case "ts" => Some(TS)
            case "fb" => Some(FB)
            case "ds" => Some(DS)
            case _    => None
        }

        val requiredFactions : List[Faction] = cleanArgs.drop(2).flatMap(parseFaction).toList

        // ── Game options ───────────────────────────────────────────────────
        val allNMOptions : $[GameOption] =
            $(UseGhast, UseGug, UseShantak, UseStarVampire, UseVoonith, UseDimensionalShamblers, UseGnorri)
        val allIGOOOptions : $[GameOption] =
            $(UseByatis, UseAbhoth, UseDaoloth, UseNyogtha, UseTulzscha, UseYgolonac)

        val allToggleOptions : $[GameOption] =
            $(Opener4P10Gates, DemandTsathoggua, NeutralSpellbooks, IceAgeAffectsLethargy, GateDiplomacy)

        def buildOptions(pc : Int) : $[GameOption] = {
            if (withRandFlags) {
                val nm   : $[GameOption] = if (random() < 0.5) allNMOptions   else $
                val hp   : $[GameOption] = if (random() < 0.5) $(HighPriests) else $
                val igoo : $[GameOption] = if (random() < 0.5) allIGOOOptions else $
                val toggles : $[GameOption] = allToggleOptions.filter(_ => random() < 0.3)
                nm ++ hp ++ igoo ++ toggles
            } else {
                val nm   : $[GameOption]   = if (withNM)   allNMOptions   else $
                val hp   : $[GameOption]   = if (withHP)   $(HighPriests) else $
                val igoo : $[GameOption]   = if (withIGOO) allIGOOOptions else $
                val opener : $[GameOption] = if (withOpener10) $(Opener4P10Gates) else $
                val dTsat  : $[GameOption] = if (withDemandTsat) $(DemandTsathoggua) else $
                val nSB    : $[GameOption] = if (withNeutralSB) $(NeutralSpellbooks) else $
                val iceL   : $[GameOption] = if (withIceLethargy) $(IceAgeAffectsLethargy) else $
                val gDip   : $[GameOption] = if (withGateDip) $(GateDiplomacy) else $
                nm ++ hp ++ igoo ++ opener ++ dTsat ++ nSB ++ iceL ++ gDip
            }
        }

        // ── Faction combination pools ──────────────────────────────────────
        def makePool(n : Int) : $[$[Faction]] =
            allFactions.combinations(n).$.filter(ff => requiredFactions.forall(ff.contains(_)))

        def factionCode(f : Faction) : String = f match {
            case GC => "gc" ; case CC => "cc" ; case BG => "bg"
            case YS => "ys" ; case SL => "sl" ; case WW => "ww"
            case OW => "ow" ; case AN => "an" ; case TS => "ts"
            case FB => "fb" ; case DS => "ds"
            case _  => "xx"
        }

        def allSeatings(factions : $[Faction]) =
            factions.permutations.$.%(s => s.contains(GC).?(s(0) == GC).|(s(0) != WW))
        def randomSeating(factions : $[Faction]) =
            allSeatings(factions).shuffle.first

        // ── Build game list ────────────────────────────────────────────────
        // Each entry is (playerCount, factions)
        val games : $[(Int, $[Faction])] = if (withRand || cleanArgs.headOption.contains("rand")) {
            val validCounts = $(3, 4, 5).filter(makePool(_).nonEmpty)
            if (validCounts.isEmpty) {
                System.err.println("ERROR: No valid 3/4/5-player combinations for: " +
                    requiredFactions./(_.name).mkString(", "))
                System.exit(1)
                Nil
            } else {
                val randPools = validCounts.map(n => n -> makePool(n)).toMap
                (1 to numberOfGames).map { _ =>
                    val pc = validCounts.shuffle.head
                    pc -> randPools(pc).shuffle.head
                }.toList
            }
        } else {
            val pool = makePool(defaultPlayers)
            if (pool.isEmpty) {
                System.err.println("ERROR: No valid " + defaultPlayers + "-player combinations for: " +
                    requiredFactions./(_.name).mkString(", "))
                System.exit(1)
                Nil
            } else {
                var gameList : $[$[Faction]] = $
                while (gameList.num < numberOfGames)
                    gameList = gameList ++ pool.shuffle
                gameList.take(numberOfGames).map(defaultPlayers -> _)
            }
        }

        val optLabel = (if (withHP) " +HP" else "") + (if (withNM) " +NM" else "") + (if (withIGOO) " +iGOO" else "")
        val pcLabel  = if (withRand || cleanArgs.headOption.contains("rand")) "rand(3-5)" else defaultPlayers.toString
        println("Running " + numberOfGames + " " + pcLabel + "-player games" +
            requiredFactions.any.?(", required: " + requiredFactions./(_.name).mkString(", ")).|("") +
            optLabel.nonEmpty.?(optLabel).|(("")) + " ...")

        var resultList : $[$[Faction]] = $
        resultList ++= games.par.map { case (pc, ff) =>
            var log : $[String] = $
            def writeLog(s : String) { log = s :: log }

            val opts = buildOptions(pc)
            val game : Game = pc match {
                case 3 => new Game(EarthMap3,    RitualTrack.for3, randomSeating(ff), true, opts)
                case 5 => new Game(EarthMap5,    RitualTrack.for5, randomSeating(ff), true, opts)
                case _ => new Game(EarthMap4v35, RitualTrack.for4, randomSeating(ff), true, opts)
            }

            var aa : $[Action] = $

            // ── Firstborn (FB) telemetry ─────────────────────────────────────
            // Round 8: track basic FB activity per game so we can verify the
            // bot exercises every faction-defining ability at least once.
            // Each counter is incremented when the corresponding log line is
            // matched. The "max" counters snapshot the highest seen value of
            // a game-state field. The boolean flags become true if the action
            // ever happened in this game.
            var fbWritheCount     = 0   // # of Writhe activations (logged "used Writhe")
            var fbEyeOpensCount   = 0   // # of Eye Opens activations
            var fbCarnageCount    = 0   // # of Carnage triggers (paid power OR flipped SB)
            var fbDevilsMarkCount = 0   // # of Devil's Mark crater placements
            var fbCallFaithfulCount = 0 // # of Call of the Faithful summons
            var fbCyclopeanCount  = 0   // # of Cyclopean Gaze fires
            var fbInfernalPactCount = 0 // # of Infernal Pact activations (any flip)
            var fbRevenantsMax    = 0   // peak # of Revenants on map
            var fbDesiccatedMax   = 0   // peak # of Desiccated on map
            var fbAuguryKillsMax  = 0   // peak fbAuguryKills value
            var fbRituals         = 0   // # of FB rituals performed
            var fbAwakenings      = 0   // # of Ghatanothoa awakenings (1 = mandatory, 2-3 = bonus)

            var tsPeakGates = 0
            // AP-by-AP milestone tracking for TS
            var tsAPCount = 0       // counts APs completed (increments at POWER GATHER)
            var tsGatesAP1 = 0      // gates at end of AP1 (at POWER GATHER)
            var tsGatesAP2 = 0      // gates at end of AP2
            var tsGatesAP3 = 0      // [2026-04-01 10:35] gates at end of AP3
            var tsSBAP3 = 0         // spellbooks at end of AP3
            var tsGlaakiTurn = 0    // AP when Glaaki awakened (0 = never)
            var tsBoardWiped = false // ever had 0 gates during an AP
            // [2026-04-01 10:35] New metrics for comprehensive tactic comparison
            var tsPowerAP2 = 0      // power at start of AP2
            var tsPowerAP3 = 0      // power at start of AP3
            var tsTotalPower = 0    // cumulative power gathered across all APs
            var tsPowerForfeited = 0 // power remaining when ending turn (wasted)
            var tsRitualCount = 0   // number of rituals performed
            var tsRitualDoom = 0    // total doom gained from rituals
            var tsCaptureCount = 0  // total cultists captured
            var tsESCount = 0       // total Elder Signs gained (all sources)
            var tsESFromCapture = 0 // [2026-04-01 13:22] ES from Green Decay captures specifically
            var tsESFromRitual = 0  // [2026-04-01 13:22] ES from ritual performance
            var tsESDoom = 0        // doom from revealing ES
            // [2026-04-01 14:42] Cursed tome metrics
            var tsTomesUsed = 0     // times any faction used a TS-given tome
            var tsTomeDoom = 0      // doom TS gained from tome usage
            var tsTomeES = 0        // ES TS gained from tome usage
            var tsTomePower = 0     // power other factions gained from using TS tomes
            var tsCumGates = 0      // [2026-04-01 12:20] cumulative gates at each AP end (for avg gates held)

            // Round 9: central trace-faction state. When --trace is passed with
            // a single required faction, that faction becomes the trace target.
            // If no required faction, TS is the default for backward compatibility.
            val traceTarget : Option[Faction] =
                if ((withTrace && numberOfGames == 1) || saveAll)
                    requiredFactions.headOption.orElse(Some(TS))
                else None
            Bot3.traceFaction = traceTarget
            // Emit a marker line so save-replay.py can read the trace faction.
            traceTarget.foreach(f => writeLog("TRACE_FACTION=" + f.short))

            try {
                val (l, cc) = game.perform(StartAction)
                var c = cc
                l.foreach(writeLog)
                var n = 0
                while (!c.isInstanceOf[GameOver]) {
                    n += 1
                    val a = Host.askFaction(game, c)
                    // Log decision weights for the trace target faction only
                    if (traceTarget.isDefined) {
                        c match {
                            case ask : Ask if ask.actions.num > 1 && (
                                traceTarget.contains(ask.faction) ||
                                // Also log battle decisions involving trace target's units (during battle only)
                                (game.battle.any && traceTarget.isDefined && !traceTarget.contains(ask.faction) &&
                                    ask.actions.exists(act => {
                                        val name = act.unwrap.toString
                                        name.contains(traceTarget.get.short + "/")
                                    }))
                                ) &&
                                // Only log meaningful decisions — skip menus, end turn, move done, etc.
                                !a.unwrap.isInstanceOf[EndTurnAction] &&
                                !a.unwrap.getClass.getSimpleName.contains("MoveDone") &&
                                !a.unwrap.isInstanceOf[MainAction] &&
                                !a.unwrap.isInstanceOf[MainGatesAction] &&
                                !a.unwrap.isInstanceOf[PreMainAction] &&
                                !a.unwrap.getClass.getSimpleName.contains("PassAction") &&
                                !a.unwrap.getClass.getSimpleName.contains("DoomDone") &&
                                !a.unwrap.getClass.getSimpleName.contains("NextPlayer") =>

                                // Use the ACTUAL evaluation results from the bot's decision
                                // Sort using the SAME compareEL logic as BotX (sorted-by-abs element-wise comparison, NOT sum)
                                def sortByAbs(a : $[Int]) : $[Int] = a.sortBy(v => -v.abs)
                                def compareEL(aaa : $[Int], bbb : $[Int]) : Int =
                                    (aaa, bbb) match {
                                        case (a :: aa, b :: bb) => if (a == b) compareEL(aa, bb) else if (a > b) 1 else -1
                                        case (0 :: _, Nil) => 0
                                        case (Nil, 0 :: _) => 0
                                        case (a :: _, Nil) => if (a > 0) 1 else -1
                                        case (Nil, b :: _) => if (0 > b) 1 else -1
                                        case (Nil, Nil) => 0
                                    }
                                def compareAE(a : ActionEval, b : ActionEval) : Boolean =
                                    compareEL(sortByAbs(a.evaluations.map(_.weight)), sortByAbs(b.evaluations.map(_.weight))) > 0
                                val ranked = Bot3.lastEval.sortWith(compareAE)
                                // Display: top weight (what compareEL compares) for each action
                                val scored = ranked.map { ae =>
                                    val topWeight = ae.evaluations.filter(_.weight != 0).sortBy(e => -e.weight.abs).headOption.map(_.weight).getOrElse(0)
                                    (ae.action, topWeight)
                                }
                                // Console output (uses cleanName too)
                                def cleanConsole(act : Action) : String = {
                                    act.unwrap.toString.replaceAll("<[^>]+>", "")
                                        .replaceAll("WrappedQForcedAction\\(", "").replaceAll("WrappedForcedAction\\(", "")
                                        .replaceAll("UnitRef\\([^,]+,([^,]+),\\d+\\)", "$1")
                                        .take(80)
                                }
                                val consoleTag = traceTarget.map(_.short).getOrElse("BOT")
                                println("  [" + consoleTag + "] Chose: " + cleanConsole(a))
                                scored.take(4).foreach { case (act, score) =>
                                    val marker = if (act == a.unwrap) ">>>" else "   "
                                    println("  [TS] " + marker + " " + score + " " + cleanConsole(act))
                                }
                                Bot3.lastEval.find(_.action.unwrap == a.unwrap).foreach { ae =>
                                    ae.evaluations.filter(_.weight != 0).sortBy(e => -e.weight.abs).take(5).foreach { e =>
                                        println("  [TS]       " + e.weight + " <- " + e.desc)
                                    }
                                }
                                // Game log output (visible in replay weight panel)
                                def readable(act : Action) : String = {
                                    val s = act.unwrap.toString.replaceAll("<[^>]+>", "")
                                        .replaceAll("Wrapped[QForced]*Action\\(", "").replaceAll("UnitRef\\([^,]+,([^,]+),\\d+\\)", "$1")
                                        .replaceAll("List\\(([^)]+)\\)", "$1")
                                    val t = act.unwrap.getClass.getSimpleName.replace("$", "")
                                    t match {
                                        case n if n.contains("MoveAction") && !n.contains("Main") && !n.contains("Done") =>
                                            val parts = s.replaceAll(".*Action\\([^,]+,", "").replaceAll("[()]", "").split(",").map(_.trim)
                                            if (parts.length >= 3) "Move " + parts(0) + " from " + parts(1) + " to " + parts(2) else "Move " + parts.mkString(" ")
                                        case n if n.contains("SummonAction") && !n.contains("Main") =>
                                            val parts = s.replaceAll(".*Action\\([^,]+,", "").replaceAll("[()]", "").split(",").map(_.trim)
                                            "Summon " + parts.mkString(" at ")
                                        case n if n.contains("AttackAction") && !n.contains("Main") =>
                                            val parts = s.replaceAll(".*Action\\([^,]+,", "").replaceAll("[()]", "").split(",").map(_.trim)
                                            if (parts.length >= 2) "Attack " + parts(1) + " in " + parts(0) else "Attack " + parts.mkString(" ")
                                        case n if n.contains("CaptureAction") && !n.contains("Main") =>
                                            val parts = s.replaceAll(".*Action\\([^,]+,", "").replaceAll("[()]", "").split(",").map(_.trim)
                                            if (parts.length >= 2) "Capture " + parts(1) + " in " + parts(0) else "Capture " + parts.mkString(" ")
                                        case n if n.contains("BuildGate") => "Build gate " + s.replaceAll(".*,", "").replaceAll("[()]", "").trim
                                        case n if n.contains("Recruit") && !n.contains("Main") => "Recruit " + s.replaceAll(".*Action\\([^,]+,", "").replaceAll("[()]", "").trim
                                        case n if n.contains("EndTurn") => "End Turn"
                                        case n if n.contains("Pass") => "Pass"
                                        case n if n.contains("MoveDone") => "(move done)"
                                        case n if n.contains("MoveMain") => "Move (choose dest)..."
                                        case n if n.contains("AttackMain") =>
                                            val rgn = s.replaceAll(".*List\\(", "").replaceAll("[()]", "").split(",").head.trim
                                            "Attack in " + rgn + "..."
                                        case n if n.contains("SummonMain") =>
                                            val parts = s.replaceAll(".*Action\\([^,]+,", "").replaceAll("[()]", "").split(",").map(_.trim)
                                            if (parts.length >= 1) "Summon " + parts(0) else "Summon..."
                                        case n if n.contains("CaptureMain") => "Capture..."
                                        case n if n.contains("StartingRegion") =>
                                            "Place in " + s.replaceAll(".*,", "").replaceAll("[()]", "").trim
                                        case n if n.contains("SpellbookAction") =>
                                            "Take spellbook: " + s.replaceAll(".*Action\\([^,]+,", "").split(",").head.trim
                                        case n if n.contains("DeathMarch") && !n.contains("Doom") =>
                                            "Death March to " + s.replaceAll(".*,", "").replaceAll("[()]", "").trim
                                        case n if n.contains("UndulateCarry") =>
                                            val parts = s.replaceAll(".*Action\\([^,]+,", "").replaceAll("[()]", "").split(",").map(_.trim)
                                            if (parts.length >= 2) "Carry " + parts(0) + " to " + parts.last else "Carry " + parts.mkString(" ")
                                        case n if n.contains("UndulateSkip") => "Skip Undulate carry"
                                        case n if n.contains("TSAwakenGlaakiPay") =>
                                            val parts = s.replaceAll(".*Action\\([^,]+,", "").replaceAll("[()]", "").split(",").map(_.trim)
                                            if (parts.length >= 2) "Awaken Glaaki at " + parts(0) + " (cost " + parts(1) + " power)" else "Awaken Glaaki"
                                        case n if n.contains("TSAwakenGlaakiChooseCost") => "Choose Glaaki awakening cost"
                                        case n if n.contains("AwakenMain") => "Awaken Glaaki"
                                        case n if n.contains("TSHecatombRitualCost") =>
                                            val parts = s.replaceAll(".*Action\\([^,]+,", "").replaceAll("[()]", "").split(",").map(_.trim)
                                            if (parts.length >= 2) "Hecatomb ritual (power " + parts(0) + ", DH " + parts(1) + ")" else "Hecatomb ritual"
                                        case n if n.contains("Ritual") && !n.contains("Hecatomb") =>
                                            val parts = s.replaceAll(".*Action\\([^,]+,", "").replaceAll("[()]", "").split(",").map(_.trim)
                                            if (parts.length >= 1) "Ritual (cost " + parts(0) + ")" else "Ritual"
                                        case n if n.contains("TSDeathMarchDoom") => "Use Death March"
                                        case n if n.contains("TSShepherdGather") =>
                                            val parts = s.replaceAll(".*Action\\([^,]+,", "").replaceAll("[()]", "").split(",").map(_.trim)
                                            if (parts.length >= 1) "Shepherd gather to " + parts(0) else "Shepherd gather"
                                        case n if n.contains("GraspingDeadMain") => "Use Grasping Dead"
                                        case n if n.contains("GraspingDeadBattle") =>
                                            val parts = s.replaceAll(".*Action\\([^,]+,", "").replaceAll("[()]", "").split(",").map(_.trim)
                                            if (parts.length >= 2) "Grasping Dead battle " + parts(1) + " in " + parts(0) else "Grasping Dead battle"
                                        case n if n.contains("GraspingDeadPay") => "Pay for Grasping Dead"
                                        case n if n.contains("ElevenRevelationsMain") => "Use Eleven Revelations"
                                        case n if n.contains("ElevenRevelations") && !n.contains("Main") =>
                                            val parts = s.replaceAll(".*Action\\([^,]+,", "").replaceAll("[()]", "").split(",").map(_.trim)
                                            if (parts.length >= 1) "Give tome to " + parts(0) else "Give tome"
                                        case n if n.contains("RevealES") => "Reveal Elder Signs"
                                        case n if n.contains("PlayDirection") => "Choose play order"
                                        case n if n.contains("FirstPlayer") =>
                                            val parts = s.replaceAll(".*Action\\([^,]+,", "").replaceAll("[()]", "").split(",").map(_.trim)
                                            if (parts.length >= 1) "Choose first player: " + parts(0) else "Choose first player"
                                        case n if n.contains("ControlGate") => "Control gate"
                                        case n if n.contains("OleaginousRetreat") =>
                                            val parts = s.replaceAll(".*Action\\([^,]+,", "").replaceAll("[()]", "").split(",").map(_.trim)
                                            "Oleaginous retreat to " + parts.lastOption.getOrElse("?")
                                        case n if n.contains("RetreatUnit") =>
                                            val parts = s.replaceAll(".*Action\\([^,]+,", "").replaceAll("[()]", "").split(",").map(_.trim)
                                            "Retreat " + parts.headOption.getOrElse("unit") + " to " + parts.lastOption.getOrElse("?")
                                        case n if n.contains("AssignKill") => "Assign kill"
                                        case n if n.contains("AssignPain") => "Assign pain"
                                        case n if n.contains("BattleRoll") => "Battle roll"
                                        case n if n.contains("DoomDone") => "End doom phase"
                                        case n if n.contains("DoomNextPlayer") => "Next doom player"
                                        case n if n.contains("TSRemoveTome") => "Remove face-down tome"
                                        case n if n.contains("TSSkipRemoveTome") => "Keep face-down tome"
                                        case _ =>
                                            // Faction-independent fallback: strip faction name, UnitRefs, brackets
                                            val cleanType = t.replaceAll("Action.*", "").replaceAll("([a-z])([A-Z])", "$1 $2")
                                            // Strip any faction name prefix from args
                                            val allFactionNames = List("Firstborn", "Tombstalker", "Great Cthulhu", "Crawling Chaos",
                                                "Black Goat", "Yellow Sign", "Sleeper", "Windwalker", "Opener of the Way", "The Ancients")
                                            var cleanArgs = s
                                            allFactionNames.foreach(fn => cleanArgs = cleanArgs.replaceAll(".*" + fn + ",?\\s*", ""))
                                            cleanArgs = cleanArgs.replaceAll("UnitRef\\([^,]+,([^,]+),\\d+\\)", "$1")
                                                .replaceAll("List\\(([^)]+)\\)", "$1").replaceAll("[()]", "").take(60)
                                            (cleanType + " " + cleanArgs).trim
                                    }
                                }
                                // Filter out pure system actions — keep menu items that show useful alternatives
                                val realOpts = scored.filter { case (act, _) =>
                                    val nm = act.unwrap.getClass.getSimpleName
                                    !nm.contains("SacrificeHighPriest") && !nm.contains("OutOfTurnRefresh") &&
                                    !nm.contains("NeedOk") && !nm.contains("MoveDone") && !nm.contains("PreMain") &&
                                    !nm.contains("EndTurn") && !nm.contains("MainGates")
                                }
                                // Use the top weight from the bot's evaluation (what compareEL uses)
                                val chosenScore = scored.find(_._1.unwrap == a.unwrap).map(_._2).getOrElse(0)
                                // Header — include action counter + faction for replay sync, show top weight
                                val facStr = traceTarget.map(_.short).getOrElse("??")
                                writeLog("[BOT " + facStr + " @" + n + "] " + readable(a) + " (" + chosenScore + ")")
                                // All real alternatives with REAL eval scores
                                val chosenReadable = readable(a)
                                // Deduplicate: show best option per category, but keep all unique destinations
                                // For moves: category = unit type + destination (so "Move Acolyte to Arabia" differs from "Move Acolyte to N America")
                                // For placement/starting: each region is its own category
                                // For summons: each unit type is its own category
                                val seen = scala.collection.mutable.Set[String]()
                                val dedupedOpts = realOpts.filter { case (act, _) =>
                                    val name = readable(act)
                                    val actType = act.unwrap.getClass.getSimpleName
                                    val cat = if (actType.contains("StartingRegion") || actType.contains("DeathMarch") || actType.contains("BuildGate"))
                                        name // each is unique
                                    else if (actType.contains("MoveAction") && !actType.contains("Main"))
                                        name.split(" to ").lastOption.getOrElse(name) // dedupe by destination
                                    else
                                        name.split("\\s+").take(2).mkString(" ") // dedupe by action+target type
                                    if (seen.contains(cat)) false
                                    else { seen += cat; true }
                                }
                                // Display: rank (compareEL order). Already sorted by rank.
                                val realScoredOpts = dedupedOpts.take(5).map(t => (t._1, readable(t._1), t._2))
                                val topScore = if (realScoredOpts.nonEmpty) realScoredOpts.map(_._3.abs).max else 0
                                realScoredOpts.foreach { case (act, actName, score) =>
                                    val isChosen = act.unwrap == a.unwrap
                                    val close = !isChosen && score.abs >= topScore - 500 && score > 0
                                    val color = if (isChosen) "#4a4" else if (close) "#aa4" else "#a44"
                                    writeLog("[OPT " + color + "] " + actName + " = " + score)
                                }
                                // Weight breakdown: sorted by ABSOLUTE VALUE (matches compareEL comparison order)
                                def showBreakdown(evals : $[Evaluation], prefix : String) {
                                    val nonZero = evals.filter(_.weight != 0)
                                    // Show top 7 by absolute value — this is the order compareEL uses to decide
                                    nonZero.sortBy(e => -e.weight.abs).take(7).foreach { e =>
                                        writeLog(prefix + e.weight + " = " + e.desc)
                                    }
                                }
                                val chosenEvals : $[Evaluation] = Bot3.lastEval.find(_.action.unwrap == a.unwrap).map(_.evaluations).getOrElse(Nil)
                                showBreakdown(chosenEvals, "[WHY] ")
                                // Also show breakdown for the top non-chosen alternative (by rank)
                                val topAlt = realScoredOpts.find(t => t._1.unwrap != a.unwrap)
                                topAlt.foreach { case (altAct, altName, altScore) =>
                                    val altEvals : $[Evaluation] = Bot3.lastEval.find(_.action.unwrap == altAct.unwrap).map(_.evaluations).getOrElse(Nil)
                                    if (altEvals.nonEmpty) {
                                        writeLog("[ALT] " + altName + " breakdown:")
                                        showBreakdown(altEvals, "[ALT] ")
                                    }
                                }
                            case _ =>
                        }
                    }
                    aa +:= a
                    val (ll, cc2) = game.perform(a.unwrap)
                    c = cc2
                    ll.foreach(writeLog)
                    if (withTrace && numberOfGames == 1)
                        ll.foreach(s => println("  " + s.replaceAll("<[^>]+>", "")))
                    if (ff.contains(TS)) {
                        val ts = factionToState(TS)(game)
                        val tsg = ts.gates.num
                        if (tsg > tsPeakGates) tsPeakGates = tsg
                        if (tsg == 0 && tsAPCount > 0) tsBoardWiped = true
                        // Track Glaaki awakening
                        if (tsGlaakiTurn == 0 && ts.goos.any) tsGlaakiTurn = tsAPCount + 1
                        // Snapshot at POWER GATHER (end of action phase, before doom phase additions)
                        if (ll.exists(_.contains("POWER GATHER"))) {
                            tsAPCount += 1
                            if (tsAPCount == 1) tsGatesAP1 = tsg
                            if (tsAPCount == 2) { tsGatesAP2 = tsg; tsPowerAP2 = ts.power }
                            if (tsAPCount == 3) { tsSBAP3 = ts.spellbooks.num; tsGatesAP3 = tsg; tsPowerAP3 = ts.power }
                            tsTotalPower += ts.power  // accumulate power at each AP start
                            tsCumGates += tsg         // [2026-04-01 12:20] accumulate gates for avg gates held
                        }
                        // [2026-04-01 10:40] Track actions by log text (strip HTML tags)
                        val llText = ll.mkString(" ").replaceAll("<[^>]+>", "")
                        // [2026-04-01 14:10] Power forfeited: "Tombstalker passed and forfeited X Power"
                        val forfeitMatch = "Tombstalker passed and forfeited (\\d+) Power".r.findFirstMatchIn(llText)
                        forfeitMatch.foreach(m => tsPowerForfeited += m.group(1).toInt)
                        // [2026-04-01 13:25] Fixed ritual doom tracking + separated ES sources
                        // Ritual: "performed the ritual for X Power and gained Y Doom"
                        if (llText.contains("Tombstalker performed the ritual")) {
                            tsRitualCount += 1
                            val ritDoomMatch = "gained (\\d+) Doom".r.findFirstMatchIn(llText)
                            ritDoomMatch.foreach(m => tsRitualDoom += m.group(1).toInt)
                            // ES from ritual: "and an Elder Sign" or "and N Elder Signs"
                            if (llText.contains("Elder Sign")) tsESFromRitual += 1
                        }
                        // Hecatomb ritual: same pattern
                        if (llText.contains("Hecatomb") && llText.contains("performed the ritual")) {
                            // Already counted above via "performed the ritual"
                        }
                        // Capture count
                        if (llText.contains("Tombstalker captured"))
                            tsCaptureCount += 1
                        // ES from GDCY captures: "Green Decay: N captured cultist → an Elder Sign"
                        if (llText.contains("Green Decay") && llText.contains("Elder Sign"))
                            tsESFromCapture += 1
                        // Total ES (all sources)
                        if (llText.contains("Tombstalker gained") && llText.contains("Elder Sign"))
                            tsESCount += 1
                        // [2026-04-01 14:42] Cursed tome tracking
                        if (llText.contains("used Cursed Tome") && !llText.contains("Tombstalker used")) {
                            tsTomesUsed += 1
                            val tomePowerMatch = "gained (\\d+) Power".r.findFirstMatchIn(llText)
                            tomePowerMatch.foreach(m => tsTomePower += m.group(1).toInt)
                        }
                        // TS tome benefits: "Tombstalker gained X Doom" or "Tombstalker gained an Elder Sign" from tome
                        if (llText.contains("Cursed Tome") && llText.contains("Tombstalker")) {
                            val tomeDoomMatch = "Tombstalker gained (\\d+) Doom".r.findFirstMatchIn(llText)
                            tomeDoomMatch.foreach(m => tsTomeDoom += m.group(1).toInt)
                            if (llText.contains("Tombstalker") && llText.contains("Elder Sign") && llText.contains("Tome"))
                                tsTomeES += 1
                        }
                        // [2026-04-01 14:38] ES reveal doom: match TS-specific "Tombstalker revealed [...] for N Doom"
                        val tsRevealMatch = "Tombstalker revealed.*?for (\\d+) Doom".r.findFirstMatchIn(llText)
                        tsRevealMatch.foreach(m => tsESDoom += m.group(1).toInt)
                    }

                    // ── Firstborn (FB) per-turn telemetry ────────────────────
                    // Round 8: count basic FB activities so we can verify the
                    // bot exercises every faction ability. Uses the same
                    // log-text-matching pattern as TS above. Patterns are
                    // tuned to match the actual ABILITY USE log line, not the
                    // receive / flip-state-change lines that share the same
                    // spellbook name.
                    if (ff.contains(FB)) {
                        val fb = factionToState(FB)(game)
                        val fbText = ll.mkString(" ").replaceAll("<[^>]+>", "")
                        // Snapshot peak counts
                        val revs = fb.onMap(RevenantOfKnaa).num
                        val descs = fb.onMap(Desiccated).num
                        if (revs > fbRevenantsMax) fbRevenantsMax = revs
                        if (descs > fbDesiccatedMax) fbDesiccatedMax = descs
                        if (game.fbAuguryKills > fbAuguryKillsMax) fbAuguryKillsMax = game.fbAuguryKills
                        // Activity counters via log matching
                        if (fbText.contains("Firstborn used Writhe"))                       fbWritheCount += 1
                        // Eye Opens use: "<unit> eliminated in <region> by The Eye Opens"
                        if (fbText.contains("by The Eye Opens"))                            fbEyeOpensCount += 1
                        // Carnage use: "Carnage: paid 1 Power" or "Carnage: flipped <SB> facedown for 1 Elder Sign"
                        if (fbText.contains("Carnage: paid") ||
                            fbText.contains("Carnage: flipped"))                            fbCarnageCount += 1
                        // Devil's Mark use: "placed Crater in <region> with Devil's Mark"
                        if (fbText.contains("with Devil's Mark"))                           fbDevilsMarkCount += 1
                        // Call of the Faithful use: "Call of the Faithful: placed Acolyte in <region>"
                        if (fbText.contains("Call of the Faithful: placed"))                fbCallFaithfulCount += 1
                        // Cyclopean Gaze fire: "Cyclopean Gaze - <source>: pained <unit> from <r> to <r>"
                        if (fbText.contains("Cyclopean Gaze - "))                           fbCyclopeanCount += 1
                        // Infernal Pact use: "Infernal Pact: flipped <SB> facedown, discount now ..."
                        if (fbText.contains("Infernal Pact: flipped"))                      fbInfernalPactCount += 1
                        if (fbText.contains("Firstborn awakened Ghatanothoa"))              fbAwakenings += 1
                        if (fbText.contains("Firstborn performed the ritual"))              fbRituals += 1
                    }

                    if (n > 7000)
                        throw null
                }

                val w = c.asInstanceOf[GameOver].winners
                val winner = w.any.?(w./(_.name).mkString(",")).|("Humanity")

                println(winner.replaceAll(",", ", ") + " won (" + n + ")")

                var line = "GAME" +
                    "|factions=" + ff./(_.name).mkString(",") +
                    "|players="  + pc +
                    "|winner="   + winner +
                    "|turns="    + n

                ff.foreach { f =>
                    val fs   = factionToState(f)(game)
                    val code = factionCode(f)
                    val gooUp = fs.goos.any
                    println("  " + f.name + ": doom=" + fs.doom +
                        " sb=" + fs.spellbooks.num + " gates=" + fs.gates.num +
                        " goo=" + gooUp + " won=" + w.contains(f))
                    line +=
                        "|" + code + "_doom="  + fs.doom +
                        "|" + code + "_sb="    + fs.spellbooks.num +
                        "|" + code + "_gates=" + fs.gates.num +
                        "|" + code + "_goo="   + gooUp +
                        "|" + code + "_won="   + w.contains(f)
                    if (f == FB) {
                        // Round 8: per-game FB activity telemetry. These let
                        // us check whether the bot is exercising every basic
                        // faction ability or only a subset. A healthy bot
                        // should show non-zero counts for every column over a
                        // batch of games (some — like Carnage — may be 0 in
                        // games without battles).
                        line +=
                            "|fb_revenants_max="   + fbRevenantsMax +
                            "|fb_desiccated_max="  + fbDesiccatedMax +
                            "|fb_writhe="          + fbWritheCount +
                            "|fb_eye_opens="       + fbEyeOpensCount +
                            "|fb_carnage="         + fbCarnageCount +
                            "|fb_devils_mark="     + fbDevilsMarkCount +
                            "|fb_call_faithful="   + fbCallFaithfulCount +
                            "|fb_cyclopean_gaze="  + fbCyclopeanCount +
                            "|fb_infernal_pact="   + fbInfernalPactCount +
                            "|fb_augury_max="      + fbAuguryKillsMax +
                            "|fb_awakenings="      + fbAwakenings +
                            "|fb_rituals="         + fbRituals +
                            "|fb_craters="         + game.fbCraters.num
                        println("  FB activity: writhe=" + fbWritheCount +
                            " eye=" + fbEyeOpensCount + " carn=" + fbCarnageCount +
                            " mark=" + fbDevilsMarkCount + " cof=" + fbCallFaithfulCount +
                            " cg=" + fbCyclopeanCount + " ip=" + fbInfernalPactCount +
                            " awake=" + fbAwakenings + " rituals=" + fbRituals +
                            " revs(max)=" + fbRevenantsMax + " desc(max)=" + fbDesiccatedMax +
                            " craters=" + game.fbCraters.num + " augury(max)=" + fbAuguryKillsMax)
                    }
                    if (f == TS) {
                        // Total face-down tomes held by non-TS factions at game end
                        val totalFaceDown = game.cursedTomesOwned.filter(_._1 != TS).values
                            .flatMap(_.filter { case (_, fd) => fd }).size
                        line +=
                            "|ts_tombherds="    + fs.onMap(TombHerd).num +
                            "|ts_deeptendrils=" + fs.onMap(DeepTendril).num +
                            "|ts_dh="           + game.deathsHead +
                            "|ts_tomes_given="  + (11 - game.tsTomesOnCard) +
                            "|ts_facedown="     + totalFaceDown +
                            "|ts_peak_gates="   + tsPeakGates +
                            "|ts_gates_ap1="    + tsGatesAP1 +
                            "|ts_gates_ap2="    + tsGatesAP2 +
                            "|ts_glaaki_turn="  + tsGlaakiTurn +
                            "|ts_sb_ap3="       + tsSBAP3 +
                            "|ts_board_wiped="  + tsBoardWiped +
                            "|ts_gates_ap3="    + tsGatesAP3 +
                            "|ts_power_ap2="    + tsPowerAP2 +
                            "|ts_power_ap3="    + tsPowerAP3 +
                            "|ts_total_power="  + tsTotalPower +
                            "|ts_power_forfeit=" + tsPowerForfeited +
                            "|ts_rituals="      + tsRitualCount +
                            "|ts_ritual_doom="  + tsRitualDoom +
                            "|ts_captures="     + tsCaptureCount +
                            "|ts_es="           + fs.es.num +  // [2026-04-01 14:55] Use game state, not log parsing
                            "|ts_es_doom="      + tsESDoom +
                            "|ts_es_capture="   + tsESFromCapture +
                            "|ts_es_ritual="    + tsESFromRitual +
                            "|ts_cum_gates="    + tsCumGates +
                            "|ts_ap_count="     + tsAPCount +
                            "|ts_tomes_used="   + tsTomesUsed +
                            "|ts_tome_doom="    + tsTomeDoom +
                            "|ts_tome_es="      + tsTomeES +
                            "|ts_tome_power="   + tsTomePower
                    }
                }

                println(line)



                // Save replay when target faction wins or --save-all.
                // Target faction = first required faction (or TS for backward compat).
                val targetFaction : Faction = requiredFactions.headOption.getOrElse(TS)
                val tshort = targetFaction.short.toLowerCase
                if (w.contains(targetFaction) || saveAll) {
                    val path = "win-logs"
                    new java.io.File(path).mkdirs()
                    val serializer = new Serialize(game)
                    val label = if (w.contains(targetFaction)) tshort + "-win" else tshort + "-game"
                    val fname = path + "/" + label + "-" + java.lang.System.currentTimeMillis + ".txt"
                    Files.write(
                        Paths.get(fname),
                        (aa.reverse./(_.unwrap)./(serializer.write).mkString("\n") + "\n\n" +
                        log.reverse.map("<div class='p'>" + _ + "</div>").mkString("\n")
                        ).getBytes(StandardCharsets.UTF_8)
                    )
                    if (w.contains(targetFaction)) println("  *** " + targetFaction.short + " WIN SAVED: " + fname)
                    else println("  Game saved: " + fname)
                }

                w
            }
            catch {
                case e : Throwable if !false =>
                    val errMsg = if (e != null && e.getMessage != null)
                        e.getMessage.replaceAll("[|\n\r]", " ").take(200)
                    else
                        "null"

                    println("GAME_ERROR" +
                        "|factions=" + ff./(_.name).mkString(",") +
                        "|players="  + pc +
                        "|error="    + errMsg)

                    if (e != null)
                        System.err.println(e.toString + "\n" + e.getStackTrace.take(5).mkString("\n"))

                    val path = "."
                    val serializer = new Serialize(game)
                    Files.write(
                        Paths.get(path + "/game-error-" + java.lang.System.currentTimeMillis + ".txt"),
                        (aa.reverse./(_.unwrap)./(serializer.write).mkString("\n") + "\n\n" +
                        (if (e != null) e.getMessage + "\n" + e.getStackTrace.mkString("\n") else "null error") + "\n\n" +
                        log.reverse.map("<div class='p'>" + _ + "</div>").mkString("\n")
                        ).getBytes(StandardCharsets.UTF_8)
                    )
                    Nil
            }
        }

        // ── Summary stats ────────────────────────────────────────────────────

        println()
        val wins = resultList.groupBy(w => w).view.mapValues(_.size).toMap

        wins.keys.toList.sortBy(k => wins(k)).reverse.foreach { k =>
            println(k.any.?(k./(_.name).mkString(", ")).|("Humanity") + ": " + wins(k) + " " +
                "%6.0f".format(wins(k) * 100.0 / wins.values.sum) + "%")
        }

        println()
        allFactions.map { f =>
            val ww   = wins.view.filterKeys(_.contains(f))
            val solo = ww.view.filterKeys(_.size == 1).values.sum
            val tie  = ww.view.filterKeys(_.size > 1).values.sum
            (solo + tie) -> (f.name + ": " + solo + "+" + tie + " " +
                "%6.0f".format((solo + tie) * 100.0 / wins.values.sum) + "%")
        }.sortBy(_._1).map(_._2).reverse.foreach(println)

        println("Humanity: " + wins.view.filterKeys(_.size == 0).values.sum + " " +
            "%6.0f".format(wins.view.filterKeys(_.size == 0).values.sum * 100.0 / wins.values.sum) + "%")
        println("Total: " + resultList.num)

        // Score tracking/regression output removed — see Backup/sim-codebase for original

    }
}
