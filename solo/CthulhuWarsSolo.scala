package cws

import scala.scalajs._
import scala.scalajs.js.timers._

import org.scalajs.dom
import org.scalajs.dom.html

import hrf.colmat._

import util.canvas._

import hrf.BuildInfo

import cws.html._


sealed trait UIAction
case object UIStop extends UIAction
case class UILog(s : String) extends UIAction
case class UIPerform(game : Game, action : Action) extends UIAction
case class UIQuestion(faction : Faction, game : Game, actions : $[Action], waiting : $[Faction] = $) extends UIAction
case class UIQuestionDebug(faction : Faction, game : Game, actions : $[Action]) extends UIAction

case class UIRead(game : Game) extends UIAction
case class UIProcess(game : Game, recorded : $[Action]) extends UIAction

case class UIRollD6(game : Game, question : Game => String, roll : Int => Action) extends UIAction
case class UIRollAgony(game : Game, question : Game => String, roll : Int => Action) extends UIAction
case class UIRollBattle(game : Game, question : Game => String, n : Int, o : $[BattleRoll] => Action) extends UIAction
case class UIDrawES(game : Game, question : Game => String, es1 : Int, es2 : Int, es3 : Int, draw : (Int, Boolean) => Action) extends UIAction

sealed trait Difficulty extends Record {
    def elem : String
}
case object Off extends Difficulty { def elem = "Off".hl }
case object Recorded extends Difficulty { def elem = "Recorded".hl }
case object Human extends Difficulty { def elem = "Human".hl }
case object Debug extends Difficulty { def elem = "Debug".hl }
case object Easy extends Difficulty { def elem = "Easy".styled("miss") }
case object Normal extends Difficulty { def elem = "Normal".styled("pain") }
case object AllVsHuman extends Difficulty { def elem = "AllVsHuman".styled("pain") }

class Setup(factions : $[Faction], diff : Difficulty) {
    var seating : $[Faction] = factions
    var options : $[GameOption] = $(factions.num match {
        case 3 => MapEarth33
        case 4 => MapEarth35
        case 5 => MapEarth55
    })

    def toggle(go : GameOption) {
        options = if (options.has(go))
            options.but(go)
        else
            go +: options
    }

    def get(go : GameOption) = options.has(go)

    var difficulty : Map[Faction, Difficulty] = seating.map(_ -> diff).toMap

    var dice = true
    var es = true
    var confirm = false
}

class CachedBitmap(val node : dom.Element) {
    private var bitmap : Bitmap = null
    def canvas : html.Canvas = if (bitmap != null) bitmap.canvas else null

    def get(w : Int, h : Int) = {
        if (bitmap == null || bitmap.width != w || bitmap.height != h) {
            if (bitmap != null)
                if (bitmap.canvas.parentNode != null)
                    bitmap.canvas.parentNode.removeChild(bitmap.canvas)

            bitmap = new Bitmap(w, h)
        }

        node.appendChild(bitmap.canvas)

        bitmap
    }
}

case class GameOverAction(winners : $[Faction], msg : String) extends Action with NoClear with Soft {
    def question(implicit game : Game) = winners.none.?("Winner is Humanity").|((winners.num == 1).?("Winner is " + winners.head).|("Winners are " + winners.mkString(", ")))
    def option(implicit game : Game) = msg
}

object CthulhuWarsSolo {
    val original = dom.document.documentElement.outerHTML

    // [2026-05-31] Random Neutrals (alt faction picker). Picker arms this
    // closure; startSetup and startOnlineSetup invoke + clear it immediately
    // after creating their local Setup so the picked Use* options are added
    // before the Variants menu renders.
    var pendingRandomNeutrals : Option[Setup => Unit] = None

    def main(args : Array[String]) {
        if (dom.document.readyState == dom.DocumentReadyState.complete)
            setupUI()
        else
            dom.window.onload = (e) => setupUI()
    }

    def getElem(k : String) = dom.document.getElementById(k).asInstanceOf[html.Element]

    def getAsset(k : String) : html.Image = dom.document.getElementById(k).asInstanceOf[html.Image]

    case class Processing(tint : |[String], screen : |[String], overlay : |[String]) extends GoodMatch

    def getTintedAsset(k : String, processing : Processing) : html.Canvas = {
        val source = dom.document.getElementById(k).asInstanceOf[html.Image]

        val result = new Bitmap(source.width, source.height)
        result.context.drawImage(source, 0, 0)

        processing.tint.foreach { tint =>
            result.context.fillStyle = tint
            result.context.globalCompositeOperation = "color"
            result.context.fillRect(0, 0, source.width, source.height)
        }

        processing.screen.foreach { screen =>
            result.context.fillStyle = screen
            result.context.globalCompositeOperation = "screen"
            result.context.fillRect(0, 0, source.width, source.height)
        }

        processing.overlay.foreach { overlay =>
            result.context.fillStyle = overlay
            result.context.globalCompositeOperation = "overlay"
            result.context.fillRect(0, 0, source.width, source.height)
        }

        result.context.globalCompositeOperation = "destination-in"
        result.context.drawImage(source, 0, 0)
        result.canvas
    }

    def newDiv(cl : String, content : String, click : () => Unit = null) = {
        val p = dom.document.createElement("div").asInstanceOf[html.Div]
        p.className = cl
        p.innerHTML = content
        if (click != null)
            p.onclick = (e) => click()
        p
    }

    def clear(e : dom.Element) {
        while (e.hasChildNodes())
            e.removeChild(e.lastChild)
    }

    def hide(e : html.Element) {
        e.style.display = "none"
    }

    def show(e : html.Element) {
        e.style.display = ""
    }

    def fail(url : String) {
        println("fail " + url)
    }

    def post(url : String, data : String)(then : String => Unit) {
        var xhr = new dom.XMLHttpRequest()
        xhr.onerror = (e : dom.ProgressEvent) => fail(url)
        xhr.onload = (e : dom.Event) => then(xhr.response.asInstanceOf[String])
        xhr.open("POST", url, true)
        xhr.responseType = "text"
        xhr.send(data)
    }

    def postF(url : String, data : String)(then : => Unit) {
        var xhr = new dom.XMLHttpRequest()
        xhr.onerror = (e : dom.ProgressEvent) => then
        xhr.onload = (e : dom.Event) => then
        xhr.open("POST", url, true)
        xhr.responseType = "text"
        xhr.send(data)
    }

    def get(url : String)(then : String => Unit) {
        var xhr = new dom.XMLHttpRequest()
        xhr.onerror = (e : dom.ProgressEvent) => fail(url)
        xhr.onload = (e : dom.Event) => then(xhr.response.asInstanceOf[String])
        xhr.open("GET", url, true)
        xhr.responseType = "text"
        xhr.send(null)
    }

    def getF(url : String)(then : String => Unit) {
        var xhr = new dom.XMLHttpRequest()
        xhr.onerror = (e : dom.ProgressEvent) => then("")
        xhr.onload = (e : dom.Event) => then(xhr.response.asInstanceOf[String])
        xhr.open("GET", url, true)
        xhr.responseType = "text"
        xhr.send(null)
    }

    def clipboard(text : String) : Boolean = {
        println(text)

        val cb = getElem("clipboard").asInstanceOf[html.TextArea]
        cb.value = text
        cb.focus()
        cb.select()
        try {
            dom.document.execCommand("copy")
        } catch {
            case e : Throwable => false
        }
    }

    val DottedLine = "............................................................................................................................................................................................................................................"
    val DoubleLine = "======================================================================================================================="

    def setupUI() {
        val (hash, quick) = dom.window.location.hash.drop(1) @@ {
            case "quick" => ("", true)
            case h if h != "" => (h, false)
            case _ =>
                val path = dom.window.location.pathname

                if (path.startsWith("/play/quick") || path.startsWith("/mnu/play/quick"))
                    ("", true)
                else
                if (path.startsWith("/play/"))
                    (path.drop("/play/".length), false)
                else
                if (path.startsWith("/mnu/play/"))
                    (path.drop("/mnu/play/".length), false)
                else
                    ("", false)
        }

        val origin = dom.window.location.origin + "/"
        val cwsOptions = Option(getElem("cws-options"))
        val delay = cwsOptions./~(_.getAttribute("data-delay").?)./~(_.toIntOption).|(30)
        val menu = cwsOptions./~(_.getAttribute("data-menu").?)./~(_.toIntOption).|(5)
        // val menu = cwsOptions./~(_.getAttribute("data-menu").?)./~(_.toIntOption).|(6) // Temp for test (comment out before deploying)
        val scroll = cwsOptions./~(_.getAttribute("data-scroll").?)./(_ == "true").|(false)
        // Round 8: if data-server isn't set (local dev), fall back to origin (the URL the
        // page is loaded from) instead of the literal "###SERVER-URL###" placeholder. This
        // makes online game API calls go to the local server during local testing.
        val rawServer = cwsOptions./~(_.getAttribute("data-server").?).|("###SERVER-URL###")
        val server = if (rawServer == "###SERVER-URL###") origin else rawServer
        // Round 8: forced to false for local online testing — when redirect=false the
        // "Online game" button runs the local logic (post to `server + "create"` etc.)
        // instead of opening a hyperlink to the production server.
        // Original line: val redirect = origin != server
        val redirect = false
        val localReplay = false

        val logDiv = getElem("log")

        def log(s : String, onClick : () => Unit = () => {}) = {
            val nd = logDiv
            val isScrolledToBottom = nd.scrollHeight - nd.clientHeight <= nd.scrollTop + 1

            val p = newDiv("p", s, onClick)

            logDiv.appendChild(p)

            if (isScrolledToBottom || scroll)
                nd.scrollTop = nd.scrollHeight - nd.clientHeight
        }

        val msday = 24 * 60 * 60 * 1000
        val diff = (System.currentTimeMillis - BuildInfo.time) % msday / 1000
        val secs = diff % 60
        val mins = diff / 60

        val version = "Cthulhu Wars HRF " + BuildInfo.version

        log(version)

        val actionDiv = getElem("action")
        val undoDiv = getElem("undo")

        def ask(question : String, options : $[String], onResult : Int => Unit, style : Option[String] = None) : None.type = {
            clear(actionDiv)

            actionDiv.appendChild(newDiv("", question))

            options.zipWithIndex.foreach { case(o, n) =>
                actionDiv.appendChild(newDiv("option" + style./(" " + _).|(""), o, () => if (o.contains("<a ").not) { clear(actionDiv); onResult(n) }))
            }

            None
        }

        var scrollTop = 0.0

        def askTop() {
            actionDiv.scrollTop = 0
        }

        def askM(headers : $[String], qos : $[(String, String)], onResult : Int => Unit, style : |[String] = None, extra : Int => |[String] = _ => None) : None.type = {
            clear(actionDiv)

            headers.foreach(h => actionDiv.appendChild(newDiv("", h)))

            var prev : |[String] = None

            qos.zipWithIndex.foreach { case((qq, oo), n) =>
                val q = |(qq).but("")
                val o = |(oo).but("")

                if (q.any && q != prev) {
                    if (prev.any)
                        actionDiv.appendChild(newDiv("", "&nbsp;"))

                    actionDiv.appendChild(newDiv("", q.get))

                    prev = q
                }

                if (o.any)
                    actionDiv.appendChild(newDiv("option" + style./(" " + _).|("") + extra(n)./(" " + _).|(""), o.get, () => {
                        if (extra(n).none) {
                            scrollTop = actionDiv.scrollTop
                            clear(actionDiv)
                            onResult(n)
                        }
                    }))
            }

            actionDiv.scrollTop = scrollTop

            None
        }

        var lastScrollTop : |[Double] = None

        case class AskLine(group : String, option : String, styles : $[String], clear : Boolean, onClick : () => Unit = () => ())

        def askN(headers : $[String], lines : $[AskLine]) : None.type = {
            clear(actionDiv)

            headers.foreach(h => actionDiv.appendChild(newDiv("", h)))

            var prev : |[String] = None

            lines.foreach { line =>
                val q = |(line.group).but("")
                val o = |(line.option).but("")

                if (q.any && q != prev) {
                    if (prev.any)
                        actionDiv.appendChild(newDiv("", "&nbsp;"))

                    actionDiv.appendChild(newDiv("", q.get))

                    prev = q
                }

                if (o.any)
                    actionDiv.appendChild(newDiv("option" + line.styles./(" " + _).join(""), o.get, () => {
                        if (line.onClick != null) {
                            scrollTop = actionDiv.scrollTop

                            if (line.clear) {
                                clear(actionDiv)
                                lastScrollTop = None
                            }
                            else {
                                lastScrollTop = |(actionDiv.scrollTop)
                            }

                            line.onClick()
                        }
                    }))
            }

            actionDiv.scrollTop = lastScrollTop.|(0.0)

            None
        }

        var statuses = $(getElem("status-1"), getElem("status-2"), getElem("status-3"), getElem("status-4"), getElem("status-5"))

        val mapWest = getElem("map-west")
        val mapEast = getElem("map-east")

        val cw = getElem("to-cw")
        val ccw = getElem("to-ccw")

        val mapSmall = getElem("map-small")
        val mapBig = getElem("map-big")

        hide(mapBig.parentElement.parentElement)
        hide(cw)
        hide(ccw)

        def processStatus(strings : $[String], ps : String) = strings
            ./(_.replace("------", "<br/>"))
            ./(s =>
                (if (s.startsWith("    "))
                    ("<div class='indent1'>" + s.drop(4))
                else
                    ("<div class='" + ps + "'>" + s))
                + "</div>")
           .mkString("\n")

        def onlineGameName = {
            val n = $(
                "Power",
                "Doom",
                "Glory",
                "Destiny",
                "Might",
                "Fight",
                "Betrayal",
                "Fate",
                "Eternity",
                "Existence",
                "Time",
                "Space",
                "Agony",
                "Pain",
                "Torment",
                "Anything",
                "Sacrifice",
                "Death",
                "Despair",
                "Rage",
                "Curse",
                "Fear",
                "Undefined",
                "Shift",
                "Colour",
                "Gate",
                "Break",
                "Desperation",
                "Ritual",
                "Dread",
                "Discord",
                "Slaughter",
                "Horror",
                "Omen",
                "Insanity",
                "Rupture",
                "Decay",
                "Blindness",
                "Continuum",
                "Catastrophe",
                "Disaster",
                "Hazard",
                "Devastation",
                "Failure",
                "Tentacles",
                "Maw",
                "Void",
                "Blood",
                "Oblivion",
                "Nothingness",
                "Lunacy",
            ).sortBy(_ => math.random())
            val c = $("for", "against", "versus", "through", "and", "of", "in", "as").sortBy(_ => math.random())
            n.head + " " + c.head + " " + n.last
        }

        def startOnlineGame(setup : Setup, recorded : $[String] = $) {
            val roles = setup.seating.%(f => setup.difficulty(f) == Human).map(_.short).mkString(" ")
            val stp = (setup.seating.%(f => setup.difficulty(f) != Off)./(f => f.short + ":" + setup.difficulty(f)).mkString("/") + " " + setup.options./(_.toString).mkString(" "))
            val name = onlineGameName

            val urlV = (dom.window.location.search + "&version=").splt("version=")(1).splt("&")(0)
            val v = (urlV != "").??("?version=" + urlV)

            post(server + "create", $(roles, version, name, stp).mkString("\n")) { master =>
                get(server + "roles/" + master) { ras =>
                    val rs = ras.split("\n").toList.map(_.split(" ")).map(s => s(0) -> s(1)).filter(_._1 != "$")
                    var ca = "Copy all"
                    def linkMenu() {
                        val op = ca +: rs.map {
                            case ("#", s) => "<a target=\"_blank\" rel=\"noopener\" href=\"" + server + "play/" + s + v + "\"><div>" + "Spectator".hl + "</div></a>"
                            case (f, s) => "<a target=\"_blank\" rel=\"noopener\" href=\"" + server + "play/" + s + v + "\"><div>" + "Play as".hl + " " + Serialize.parseFaction(f).get + "</div></a>"
                        }
                        ask(name.hl, op, { n =>
                            if (n == 0)
                                ca = clipboard(name + "\n" + rs.map {
                                    case ("#", s) => "Spectate " + server + "play/" + s + v
                                    case (f, s) => Serialize.parseFaction(f).get.short + " " + server + "play/" + s + v
                                }.mkString("\n")).?("Copied links to clipboard").|("Error copying to clipboard").hl

                            linkMenu()
                        })
                    }

                    linkMenu()
                }
            }
        }

        def startGame(setup : Setup, recorded : $[String] = $, self : |[Faction] = None) {
            // Clear the map preview from setup menu
            getElem("map-small").innerHTML = ""

            setup.options = GameOptions.all.intersect(setup.options)
            val seating = setup.seating.%(f => setup.difficulty(f) != Off)

            if (seating.num == 5)
                statuses = statuses.take(3) ++ statuses.drop(4).take(1) ++ statuses.drop(3).take(1)

            statuses.take(seating.num)./(_.parentElement.parentElement).foreach(show)
            statuses.drop(seating.num)./(_.parentElement.parentElement).foreach(hide)

            if (seating.num <= 4) {
                hide(getElem("to-cw6"))
                hide(getElem("to-ccw6"))
            }
            else {
                hide(getElem("to-cw4"))
                hide(getElem("to-ccw4"))
            }

            statuses.lazyZip(setup.seating).foreach { (s, f) =>
                // s.as[html.Element].get.style.backgroundImage = "url(info/" + f.style + "-header.png)"
                s.as[html.Element].get.style.backgroundImage = "url(" + Overlays.imageSource("info:" + f.style + "-background") + ")"
                s.as[html.Element].get.style.backgroundSize = "cover"
            }

            val board = setup.options.of[MapOption].starting match {
                case Some(MapEarth33) => EarthMap3
                case Some(MapEarth35) | None => EarthMap4v35
                case Some(MapEarth53) => EarthMap4v53
                case Some(MapEarth55) => EarthMap5
                case Some(MapLibrary55) => LibraryCelaeno55
                case Some(MapLibrary33) => LibraryCelaeno33
                case Some(MapLibrary35) => LibraryCelaeno35
                case Some(MapLibrary53) => LibraryCelaeno53
            }

            val track = seating.num @@ {
                case 3 => RitualTrack.for3
                case 4 => RitualTrack.for4
                case 5 => RitualTrack.for5
            }

            var game = new Game(board, track, seating, true, setup.options)
            var overrideGame : |[Game] = None
            def displayGame = overrideGame.|(game)

            var actions : $[Action] = $
            var queue : $[UIAction] = $
            var paused = recorded.any && hash == ""

            val serializer = new Serialize(game)

            def askFaction(c : Continue)(implicit game : Game) : UIAction = {
                def dontAttack(factions : $[Faction])(a : Action) = factions.map(f => Explode.isOffense(f)(a)(game).not).reduce(_ && _)

                def filterAttack(actions : $[Action], factions : $[Faction]) = actions.%(dontAttack(factions)).some.|(actions)

                val gameActions = actions
                c match {
                    case Force(action) =>
                        throw new Error("force escaped " + action)

                    case Then(action) =>
                        UIPerform(game, action)

                    case DelayedContinue(_, continue) =>
                        askFaction(continue)

                    case RollD6(question, roll) if setup.dice && recorded.any && recorded.num > actions.num && localReplay.not =>
                        try { UIPerform(game, serializer.parseAction(recorded(actions.num).replace("&gt;", ">"))) }
                        catch { case _ : Throwable => UIPerform(game, roll((1::2::3::4::5::6).maxBy(_ => random()))) }
                    case RollD6(question, roll) if setup.dice =>
                        UIPerform(game, roll((1::2::3::4::5::6).maxBy(_ => random())))

                    case RollD6(question, roll) =>
                        UIRollD6(game, question, roll)

                    case RollAgony(question, roll) if setup.dice && recorded.any && recorded.num > actions.num && localReplay.not =>
                        try { UIPerform(game, serializer.parseAction(recorded(actions.num).replace("&gt;", ">"))) }
                        catch { case _ : Throwable => UIPerform(game, roll(AgonyDie.roll())) }
                    case RollAgony(question, roll) if setup.dice =>
                        UIPerform(game, roll(AgonyDie.roll()))

                    case RollAgony(question, roll) =>
                        UIRollAgony(game, question, roll)

                    case RollBattle(_, 0, roll) =>
                        UIPerform(game, roll($))

                    case RollBattle(_, n, roll) if setup.dice && recorded.any && recorded.num > actions.num && localReplay.not =>
                        try { UIPerform(game, serializer.parseAction(recorded(actions.num).replace("&gt;", ">"))) }
                        catch { case _ : Throwable => UIPerform(game, roll(List.fill(n)(BattleRoll.roll()))) }
                    case RollBattle(_, n, roll) if setup.dice =>
                        UIPerform(game, roll(List.fill(n)(BattleRoll.roll())))

                    case RollBattle(question, n, roll) =>
                        UIRollBattle(game, question, n, roll)

                    case DrawES(_, 0, 0, 0, draw) =>
                        UIPerform(game, draw(0, true))

                    case DrawES(_, es1, es2, es3, draw) if setup.es && recorded.any && recorded.num > actions.num && localReplay.not =>
                        try { UIPerform(game, serializer.parseAction(recorded(actions.num).replace("&gt;", ">"))) }
                        catch { case _ : Throwable => UIPerform(game, draw((List.fill(es1)(1) ++ List.fill(es2)(2) ++ List.fill(es3)(3)).maxBy(_ => random()), false)) }
                    case DrawES(_, es1, es2, es3, draw) if setup.es =>
                        UIPerform(game, draw((List.fill(es1)(1) ++ List.fill(es2)(2) ++ List.fill(es3)(3)).maxBy(_ => random()), false))

                    case DrawES(question, es1, es2, es3, draw) =>
                        UIDrawES(game, question, es1, es2, es3, draw)

                    case GameOver(winners) =>
                        UIQuestion(self.||(winners.starting).|(game.setup.first), game, GameOverAction(winners, "Hooray!") :: GameOverAction(winners, "Meh...") :: GameOverAction(winners, "Save replay"))

                    case MultiAsk(asks) =>
                        val a = asks.sortBy(ask =>
                            if (self.has(ask.faction))
                                0
                            else
                                setup.difficulty(ask.faction) match {
                                    case Debug => 100 + random(10)
                                    case Human | Recorded => 200 + random(10)
                                    case Easy => 300 + random(10)
                                    case Normal => 400 + random(10)
                                    case AllVsHuman => 500 + random(10)
                                }
                        ).first

                        askFaction(a) match {
                            case q : UIQuestion => q.copy(waiting = asks./(_.faction))
                            case ui => ui
                        }

                    case Ask(faction, actions) =>
                        if (actions(0).isInstanceOf[PlayDirectionAction] || actions(0).isInstanceOf[StartingRegionAction]) {
                            hide(cw)
                            hide(ccw)
                        }
                        else {
                            if ((game.factions ++ game.factions).containsSlice(game.setup)) {
                                show(cw)
                                hide(ccw)
                            }
                            else {
                                hide(cw)
                                show(ccw)
                            }
                        }

                        val confirm = setup.confirm && setup.difficulty(faction) == Human

                        if (recorded.any && recorded.num > gameActions.num && localReplay.not && setup.difficulty(faction) != Human && setup.difficulty(faction) != Recorded) {
                            try { UIPerform(game, serializer.parseAction(recorded(gameActions.num).replace("&gt;", ">"))) }
                            catch { case _ : Throwable =>
                                if (confirm.not && actions.num == 1)
                                    UIPerform(game, actions(0))
                                else
                                if (confirm.not && actions.%!(_.isInfo).num == 1 && actions.has(NeedOk).not)
                                    UIPerform(game, actions.%!(_.isInfo).only)
                                else
                                    UIPerform(game, actions(0))
                            }
                        }
                        else
                        if (confirm.not && actions.num == 1)
                            UIPerform(game, actions(0))
                        else
                        if (confirm.not && actions.%!(_.isInfo).num == 1 && actions.has(NeedOk).not)
                            UIPerform(game, actions.%!(_.isInfo).only)
                        else
                        if (confirm.not && actions.%!(_.isInfo).any && actions.%!(_.isInfo)(0).isInstanceOf[SpellbookAction] && actions.%!(_.isInfo).num == faction.unclaimedSB)
                            UIPerform(game, actions.%!(_.isInfo)(0))
                        else {
                            setup.difficulty(faction) match {
                                case Human | Recorded =>
                                    UIQuestion(faction, game, actions)
                                case Debug =>
                                    UIQuestionDebug(faction, game, actions)
                                case Easy =>
                                    UIPerform(game, faction match {
                                        case GC => Bot3(GC).ask(actions, 0.5)(game)
                                        case CC => Bot3(CC).ask(actions, 0.2)(game)
                                        case BG => Bot3(BG).ask(actions, 0.6)(game)
                                        case YS => Bot3(YS).ask(actions, 0.3)(game)
                                        case SL => BotSL   .ask(actions, 0.2)(game)
                                        case WW => BotWW   .ask(actions, 0.2)(game)
                                        case OW => BotOW   .ask(actions, 0.2)(game)
                                        case AN => BotAN   .ask(actions, 0.2)(game)
                                        // Tombstalker (TS): AI bot decision-making at Easy difficulty
                                        case TS => BotTS   .ask(actions, 0.2)(game)
                                        // Firstborn (FB): AI bot decision-making at Easy difficulty.
                                        // Lowered error 0.2→0.0 because randomization was causing
                                        // bot to pick lower-scored alternatives (e.g. CoF over CG)
                                        // ~14% of the time, which broke the SB priority order.
                                        case FB => BotFB   .ask(actions, 0.0)(game)
                                        // Daemon Sultan (DS): AI bot decision-making at Easy difficulty
                                        case DS => BotDS   .ask(actions, 0.3)(game)
                                    })
                                case Normal =>
                                    UIPerform(game, faction match {
                                        case GC => BotGC   .ask(actions, 0.03)(game)
                                        case CC => BotCC   .ask(actions, 0.03)(game)
                                        case BG => Bot3(BG).ask(actions, 0.03)(game)
                                        case YS => BotYS   .ask(actions, 0.03)(game)
                                        case SL => BotSL   .ask(actions, 0.03)(game)
                                        case WW => BotWW   .ask(actions, 0.03)(game)
                                        case OW => BotOW   .ask(actions, 0.03)(game)
                                        case AN => BotAN   .ask(actions, 0.03)(game)
                                        // Tombstalker (TS): AI bot decision-making at Normal difficulty
                                        case TS => BotTS   .ask(actions, 0.03)(game)
                                        // Firstborn (FB): AI bot decision-making at Normal difficulty
                                        case FB => BotFB   .ask(actions, 0.03)(game)
                                        // Daemon Sultan (DS): AI bot decision-making at Normal difficulty
                                        case DS => BotDS   .ask(actions, 0.03)(game)
                                    })
                                case AllVsHuman =>
                                    val aa = Explode.explode(game, actions)
                                    val fr = setup.seating.but(faction).filter(f => setup.difficulty(f) == AllVsHuman)
                                    val as = filterAttack(aa, fr)
                                    UIPerform(game, faction match {
                                        case GC => BotGC   .ask(as, 0.03)(game)
                                        case CC => BotCC   .ask(as, 0.03)(game)
                                        case BG => Bot3(BG).ask(as, 0.03)(game)
                                        case YS => BotYS   .ask(as, 0.03)(game)
                                        case SL => BotSL   .ask(as, 0.03)(game)
                                        case WW => BotWW   .ask(as, 0.03)(game)
                                        case OW => BotOW   .ask(as, 0.03)(game)
                                        case AN => BotAN   .ask(as, 0.03)(game)
                                        // Tombstalker (TS): AI bot decision-making at AllVsHuman difficulty
                                        case TS => BotTS   .ask(as, 0.03)(game)
                                        // Firstborn (FB): AI bot decision-making at AllVsHuman difficulty
                                        case FB => BotFB   .ask(as, 0.03)(game)
                                        // Daemon Sultan (DS): AI bot decision-making at AllVsHuman difficulty
                                        case DS => BotDS   .ask(as, 0.03)(game)
                                    })


                                case d => throw new Error("Unknown difficulty " + d)
                            }

                        }
                }
            }

            val mapBitmapSmall = new CachedBitmap(mapSmall)
            val mapBitmapBig = new CachedBitmap(mapBig)
            var map = mapBitmapSmall

            // Round 8 Bug 53: shared GlyphPlacement engine. Used by findAnother (random
            // valid placement for unit layout) and findStaticGlyphPos (deterministic
            // position for dynamic-start faction glyphs — FB, TS, etc.).
            // Library maps: separate placement bitmaps for vertical and horizontal orientations.
            // Create both up front; activeGlyphPlacer switches based on orientation in drawMap.
            val glyphPlacerV = new GlyphPlacement(board.id)
            val glyphPlacerH = if (board.isLibraryMap) new GlyphPlacement(board.id + "-h") else glyphPlacerV
            var activeGlyphPlacer = glyphPlacerV
            def place = activeGlyphPlacer.place
            val findAnother = (x : Int, y : Int) => activeGlyphPlacer.findAnother(x, y)
            // Cache glyph positions: glyphs are static (same position every render).
            // Without caching, findStaticGlyphPos scans the entire placement bitmap
            // on every map redraw — consuming 85% of all CPU time.
            val glyphPosCache = scala.collection.mutable.Map[(Int, Int), (Int, Int)]()
            var glyphPosCacheHorizontal : Boolean = false
            def findStaticGlyphPos(gx : Int, gy : Int, explicitRegionColor : Int = -1) : (Int, Int) =
                glyphPosCache.getOrElseUpdate((gx, gy), activeGlyphPlacer.findStaticGlyphPos(gx, gy, halfGlyph = (33 * board.unitScale).toInt, halfGate = (38 * board.unitScale).toInt, maxRadius = (200 * board.unitScale).toInt, explicitRegionColor = explicitRegionColor))

            case object DesecrationToken extends FactionUnitClass(YS, "Desecration", Token, 0)
            case object WebToken extends UnitClass("Web Token", Token, 0)
            case object IceAgeToken extends FactionUnitClass(WW, "Ice Age", Token, 0)
            case object Cathedral extends FactionUnitClass(AN, "Cathedral", Token, 0)
            // Firstborn (FB): Crater rendered as Token (same pattern as Cathedral) so rank() doesn't crash
            case object Crater extends FactionUnitClass(FB, "Crater", Token, 0)
            // Daemon Sultan (DS): Chaos Gate rendered as Token
            case object ChaosGate extends FactionUnitClass(DS, "Chaos Gate", Token, 0)
            case object Gate extends UnitClass("Gate", Token, 3)
            case object FactionGlyph extends UnitClass("Faction Glyph", Token, 0)
            // Round 8 Bug 53: separate UnitClass for the on-map dynamic-start glyph render.
            // Used by FB and TS to draw their starting glyph on the map at 66x66 (smaller than
            // the 100x100 used in faction status panels). The DrawRect for StartingGlyph
            // dispatches to fb-glyph or ts-glyph based on the faction passed to DrawItem.
            case object StartingGlyph extends UnitClass("Starting Glyph", Token, 0)

            // Per-render cache: per-unit-class shrink ratio applied during placement
            // and rendering on library maps. Populated at the start of drawMap. Sprites
            // without an entry (or with entry == 1.0) render at full size.
            // Class-global: any region forcing shrink shrinks every instance of that
            // class on the map for visual consistency.
            val classShrink = scala.collection.mutable.Map[UnitClass, Double]()
            // Per-game cache: bounding box geometry for each region in placement-bitmap
            // coords (regionWidth, availableHeightAboveGate). Built from one full scan
            // of the placement bitmap on first library-map render.
            val regionGeom = scala.collection.mutable.Map[Region, (Int, Int)]()
            val regionArea = scala.collection.mutable.Map[Region, Int]()
            // Pixel centroid (mean of all region pixels) per region. Used to auto-center
            // the gate icon for non-tome library regions where the hand-coded XY is in
            // Map.scala (and may be off-kilter for some bitmaps — e.g. Lake of Hali
            // Overlook in library3-place.webp where the region shape differs from
            // library5-place.webp despite the same hand-coded XY).
            val regionCentroid = scala.collection.mutable.Map[Region, (Int, Int)]()
            // Uniform shrink applied to EVERY object (units, gates, tokens, etc.) in
            // regions that meet the "small region" criterion (library maps only,
            // bbox area ≤ Gloomloft 5U). Factor 0.852 is the midpoint of the average
            // shrinkage needed to bring large units down to 1.30× cultist height —
            // computed one-off across all unit protos (see Region Geometry analysis).
            val regionUniformShrink = scala.collection.mutable.Map[Region, Double]()
            var regionGeomComputed = false
            // Constants tied to the resize redesign (computed one-off from bitmaps).
            //   SmallRegionAreaCutoff = Gloomloft 5U bbox area in placement-bitmap pixels.
            //   SmallRegionUniformShrink = 1 − (avg(% needed to bring proto.maxDim > 1.3×cultist
            //                                       down to 1.3× cultist) / 2)  =  0.852.
            //   LargeUnitTriggerMaxDim = 1.60 × cultist height = 96 (only applies to non-small regions).
            val SmallRegionAreaCutoff = 154128
            val SmallRegionUniformShrink = 0.852
            val LargeUnitTriggerMaxDim = 96

            // L/T-region balanced placement table. Same data as the Library at Celaeno
            // mirror; see that file (and Region Geometry/lt-partitions.json) for derivation.
            // v3 (2026-05-21) — partition cuts revised per user feedback. Mirror of
            // Library at Celaeno table; see that file for derivation.
            val lt5U : List[(String, List[(Int, Int, Int, Int, Double)])] = List(
                "Yr and the Nhhngr" -> List(
                    (120,  627,  165,  913,   6.4),
                    (166,  627,  429,  927,  57.6),
                    (430,  627,  503,  920,  13.4),
                    (187,  928,  429, 1067,  22.5)
                ),
                "Guardian under the Lake" -> List(
                    (1115, 627, 1273,  925, 19.7),
                    (1274, 627, 1533,  925, 36.0),
                    (1534, 627, 1676,  925, 19.7),
                    (1274, 926, 1533, 1139, 24.6)
                ),
                "Gloomloft" -> List(
                    (580, 1106,  768, 1230, 19.6),
                    (769,  897, 1036, 1235, 80.4)
                ),
                "Larvae of the Outer Gods" -> List(
                    (1102, 1169, 1335, 1462, 24.3),
                    (1336, 1178, 1694, 1768, 75.7)
                )
            )
            val lt5L : List[(String, List[(Int, Int, Int, Int, Double)])] = List(
                "Red Hall" -> List(
                    (1504, 2242, 1653, 2374, 11.2),
                    (1384, 2375, 1651, 2589, 88.8)
                ),
                // v5: Black Hall top cut at Red Hall's top line (y=2242).
                "Black Hall" -> List(
                    (1183, 2031, 1290, 2241,  9.0),
                    (1175, 2242, 1349, 2629, 46.2),
                    (1181, 2630, 1442, 2862, 44.8)
                ),
                // v4: Charnel vertical moved 1380 → 1350.
                "Charnel Hall" -> List(
                    (1215, 2905, 1349, 3425, 62.4),
                    (1350, 2905, 1520, 3391, 37.6)
                ),
                "The Crawling Ones" -> List(
                    (1384, 2989, 1553, 3442, 39.3),
                    (1554, 2913, 1653, 3442, 60.7)
                )
            )
            val LtPartitions : Map[(String, String), List[(Int, Int, Int, Int, Double)]] = (
                lt5U.map { case (n, parts) => ("library5",  n) -> parts } ++
                lt5L.map { case (n, parts) => ("library5",  n) -> parts } ++
                lt5U.map { case (n, parts) => ("library35", n) -> parts } ++
                List(("library3", "Fountain") -> List(
                    // v5: Fountain lower cut 905 → 920.
                    ( 560,  649, 1086,  770, 55.6),
                    ( 828,  771, 1037,  919, 33.6),
                    ( 832,  920, 1037, 1015, 10.8)
                ))
            ).toMap

            def findWeighted(ox : Int, oy : Int, r : Region) : (Int, Int) = {
                LtPartitions.get((board.id, r.name)) match {
                    case Some(parts) =>
                        val totalPct = parts.map(_._5).sum
                        var pick = scala.math.random() * totalPct
                        var chosen = parts.head
                        var i = 0
                        while (i < parts.size) {
                            pick -= parts(i)._5
                            if (pick <= 0) { chosen = parts(i); i = parts.size }
                            else i += 1
                        }
                        val (x1, y1, x2, y2, _) = chosen
                        activeGlyphPlacer.findAnotherInBox(ox, oy, x1, y1, x2, y2)
                    case None => activeGlyphPlacer.findAnother(ox, oy)
                }
            }

            case class DrawRect(key : String, tint : |[Processing], x : Int, y : Int, width : Int, height : Int, cx : Int = 0, cy : Int = 0, alpha : Double = 1.0, rotation : Double = 0.0, splitTint : |[Processing] = None)

            case class DrawItem(region : Region, faction : Faction, unit : UnitClass, health : UnitHealth, tags : $[UnitState], x : Int, y : Int) {
                val defaultProcessing = Processing(None, None, None)

                val tint = faction @@ {
                    case GC => Processing(|("#77a055"), |("#222222"), None)
                    case CC => Processing(|("#4977b3"), |("#111111"), None)
                    case BG => Processing(|("#cd3233"), None, |("#555555"))
                    case YS => Processing(|("#ffd000"), |("#663344"), None)
                    case WW => Processing(|("#88a9be"), |("#5577aa"), None)
                    case SL => Processing(|("#db6a33"), |("#4a1a1a"), None)
                    case OW => Processing(|("#6c4296"), None, |("#4c4c4c"))
                    case AN => Processing(|("#47a5bc"), |("#333333"), None)
                    // Tombstalker (TS): faction color (pale green #BDE0BC) for unit rendering
                    case TS => Processing(|("#BDE0BC"), |("#333333"), None)
                    // Firstborn (FB): faction color (magenta/pink #CB307E) for unit rendering
                    case FB => Processing(|("#CB307E"), |("#333333"), None)
                    // Daemon Sultan (DS): faction color
                    case DS => Processing(|("#3A2825"), None, |("#120E0C"))
                    // Library map units: no tint (use original icon images)
                    case LibraryFaction => defaultProcessing
                    case _  => defaultProcessing
                }

                def proto : DrawRect = unit match {
                    case Gate => DrawRect("gate", None, x - 38, y - 38, 76, 76)

                    case Acolyte => faction match {
                        case BG => DrawRect("bg-acolyte", None, x - 17, y - 54, 39, 60)
                        case CC => DrawRect("cc-acolyte", None, x - 17, y - 54, 38, 60)
                        case GC => DrawRect("gc-acolyte", None, x - 17, y - 54, 40, 59)
                        case YS => DrawRect("ys-acolyte", None, x - 17, y - 54, 39, 61)
                        case SL => DrawRect("sl-acolyte", None, x - 17, y - 54, 38, 60)
                        case WW => DrawRect("ww-acolyte", None, x - 17, y - 52, 40, 58)
                        case OW => DrawRect("ow-acolyte", None, x - 17, y - 54, 38, 60)
                        case AN => DrawRect("an-acolyte", None, x - 17, y - 54, 39, 60)
                        // Tombstalker (TS): acolyte unit sprite
                        case TS => DrawRect("ts-acolyte", |(tint), x - 17, y - 54, 39, 60)
                        // Firstborn (FB): acolyte unit sprite
                        case FB => DrawRect("fb-acolyte", |(tint), x - 17, y - 54, 39, 60)
                        // Daemon Sultan (DS): acolyte unit sprite
                        case DS => DrawRect("ds-acolyte", None, x - 17, y - 54, 38, 60)
                        case _ => null
                    }

                    // Mind Parasite: split color — left half original faction, right half insect owner
                    case MindParasiteCultist =>
                        val origFac = if (region != null) {
                            implicit val g : Game = displayGame
                            factionToState(faction)(g).at(region).%(_.uclass == MindParasiteCultist).headOption./~(u => displayGame.mindParasiteOriginalFaction.get(u.ref))
                        } else None
                        val origTint = origFac./(of => DrawItem(null, of, Acolyte, Alive, $, 0, 0).tint).|(tint)
                        DrawRect("ts-acolyte", |(origTint), x - 17, y - 54, 39, 60, splitTint = |(tint))

                    case HighPriest => faction match {
                        case BG => DrawRect("bg-high-priest", None, x - 35, y - 60, 70, 68)
                        case CC => DrawRect("cc-high-priest", None, x - 35, y - 60, 70, 69)
                        case GC => DrawRect("gc-high-priest", None, x - 35, y - 60, 70, 67)
                        case YS => DrawRect("ys-high-priest", None, x - 35, y - 60, 70, 66)
                        case SL => DrawRect("sl-high-priest", None, x - 35, y - 60, 70, 69)
                        case WW => DrawRect("ww-high-priest", None, x - 35, y - 60, 70, 67)
                        case OW => DrawRect("ow-high-priest", None, x - 35, y - 60, 70, 66) // Left to do
                        case AN => DrawRect("an-high-priest", None, x - 35, y - 60, 70, 66) // Left to do
                        // Firstborn (FB): high priest unit sprite (FB cannot recruit HP, but sprite is defined)
                        case FB => DrawRect("fb-high-priest", |(tint), x - 35, y - 60, 70, 66)
                        // Daemon Sultan (DS): high priest unit sprite
                        case DS => DrawRect("ds-high-priest", None, x - 35, y - 60, 70, 66)
                        // Tombstalker (TS): high priest unit sprite
                        case TS => DrawRect("ts-high-priest", |(tint), x - 35, y - 60, 70, 66)
                        case _ => DrawRect("gc-high-priest", |(tint), x - 35, y - 60, 70, 66)
                    }

                    case FactionGlyph => faction match {
                        case BG => DrawRect("bg-glyph", None, x - 50, y - 50, 100, 100)
                        case CC => DrawRect("cc-glyph", None, x - 50, y - 50, 100, 100)
                        case GC => DrawRect("gc-glyph", None, x - 50, y - 50, 100, 100)
                        case YS => DrawRect("ys-glyph", None, x - 51, y - 50, 102, 100)
                        case SL => DrawRect("sl-glyph", None, x - 50, y - 50, 100, 102)
                        case WW => DrawRect("ww-glyph", None, x - 50, y - 50, 100, 100)
                        case OW => DrawRect("ow-glyph", None, x - 50, y - 50, 100, 100)
                        case AN => DrawRect("an-glyph", None, x - 50, y - 50, 100, 101)
                        // Tombstalker (TS): faction glyph sprite (100x100, matches all other factions).
                        // Used by the faction status panel. On-map dynamic-start render uses StartingGlyph instead.
                        case TS => DrawRect("ts-glyph", None, x - 50, y - 50, 100, 100)
                        // Firstborn (FB): faction glyph sprite (100x100, matches all other factions).
                        // Used by the faction status panel. On-map dynamic-start render uses StartingGlyph instead.
                        case FB => DrawRect("fb-glyph", None, x - 50, y - 50, 100, 100)
                        // Daemon Sultan (DS): faction glyph sprite
                        case DS => DrawRect("ds-glyph", None, x - 50, y - 50, 100, 100)
                        case _ => null
                    }

                        case Ghoul         => DrawRect("bg-ghoul", None, x - 20, y - 40, 39, 47)
                        case Fungi         => DrawRect("bg-fungi", None, x - 40, y - 73, 72, 80)
                        case DarkYoung     => DrawRect("bg-dark-young", None, x - 53, y - 122, 83, 131)
                        case ShubNiggurath => DrawRect("bg-shub", None, x - 69, y - 173, 132, 185, 0, 10)

                        case Nightgaunt    => DrawRect("cc-nightgaunt", None, x - 36, y - 82, 69, 90, -1, 0)
                        case FlyingPolyp   => DrawRect("cc-flying-polyp", None, x - 36, y - 81, 73, 90, 10, 0)
                        case HuntingHorror => DrawRect("cc-hunting-horror", None, x - 86, y - 70, 166, 77)
                        case Nyarlathotep  => DrawRect("cc-nyarly", None, x - 50, y - 155, 106, 163)

                        case DeepOne       => DrawRect("gc-deep-one", None, x - 16, y - 25, 36, 31, 0, -5)
                        case Shoggoth      => DrawRect("gc-shoggoth", None, x - 31, y - 62, 63, 69)
                        case Starspawn     => DrawRect("gc-starspawn", None, x - 35, y - 63, 69, 70)
                        case Cthulhu       => DrawRect("gc-cthulhu", None, x - 65, y - 209, 117, 225, 0, 50)

                        case Undead        => DrawRect("ys-undead", None, x - 27, y - 49, 44, 54, -5, 0)
                        case Byakhee       => DrawRect("ys-byakhee", None, x - 32, y - 64, 57, 70)
                        case KingInYellow  => DrawRect("ys-king-in-yellow", None, x - 44, y - 111, 85, 116)
                        case Hastur        => DrawRect("ys-hastur", None, x - 87, y - 163, 150, 170)

                        case Wizard        => DrawRect("sl-wizard", None, x - 23, y - 33, 45, 41)
                        case SerpentMan    => DrawRect("sl-serpent-man", None, x - 34, y - 76, 70, 85, 3, 0)
                        case FormlessSpawn => DrawRect("sl-formless-spawn", None, x - 38, y - 85, 78, 94)
                        case Tsathoggua    => DrawRect("sl-tsathoggua", None, x - 75, y - 133, 152, 146)

                        case Wendigo       => DrawRect("ww-wendigo", None, x - 26, y - 62, 56, 68)
                        case GnophKeh      => DrawRect("ww-gnoph-keh", None, x - 30, y - 88, 61, 95)
                        case RhanTegoth    => DrawRect("ww-rhan-tegoth", None, x - 74, y - 128, 153, 135)
                        case Ithaqua       => DrawRect("ww-ithaqua", None, x - 112, y - 192, 164, 202)

                        case Mutant        => DrawRect("ow-mutant", None, x - 20, y - 52, 40, 58)
                        case Abomination   => DrawRect("ow-abomination", None, x - 30, y - 76, 62, 82)
                        case SpawnOW       => DrawRect("ow-spawn-of-yog-sothoth", None, x - 49, y - 94, 91, 100, 3, 3)
                        case YogSothoth    => DrawRect("ow-yog-sothoth", None, x - 82, y - 162, 132, 174)

                        case UnMan         => DrawRect("an-un-man", None, x - 24, y - 60, 48, 65)
                        case Reanimated    => DrawRect("an-reanimated", None, x - 28, y - 62, 57, 65)
                        case Yothan        => DrawRect("an-yothan", None, x - 61, y - 85, 122, 90)

                    // Tombstalker (TS): monster and GOO unit sprites (TombHerd, DeepTendril, Gla'aki)
                    case TombHerd      => DrawRect("ts-tomb-herd", |(tint), x - 30, y - 70, 60, 80)
                    case DeepTendril   => DrawRect("ts-tendril",    |(tint), x - 30, y - 75, 60, 85)
                    case Glaaki        => DrawRect("ts-glaaki",     |(tint), x - 45, y - 120, 90, 130)

                    // Firstborn (FB): monster, GOO, and building unit sprites
                    case Desiccated      => DrawRect("fb-desiccated",   |(tint), x - 30, y - 72, 60, 78)
                    case RevenantOfKnaa  => DrawRect("fb-revenant",     |(tint), x - 45, y - 98, 90, 105)
                    // Firstborn (FB): Ghatanothoa GOO — large unit
                    case Ghatanothoa     => DrawRect("fb-ghatanothoa",  |(tint), x - 48, y - 160, 96, 176)
                    case Crater          => DrawRect("fb-crater",       |(tint), x - 36, y - 36, 72, 72)

                    // Daemon Sultan (DS): larva, avatar, and chaos gate unit sprites
                    case LarvaThesis      => DrawRect("ds-larva-thesis", None, x - 37, y - 46, 73, 52)
                    case LarvaAntithesis  => DrawRect("ds-larva-antithesis", None, x - 46, y - 61, 92, 68)
                    case LarvaSynthesis   => DrawRect("ds-larva-synthesis", None, x - 39, y - 87, 78, 95, rotation = 5.0)
                    case AvatarThesis     => DrawRect("ds-avatar-thesis", None, x - 67, y - 74, 133, 87)
                    case AvatarAntithesis => DrawRect("ds-avatar-antithesis", None, x - 70, y - 89, 139, 104)
                    case AvatarSynthesis  => DrawRect("ds-avatar-synthesis", None, x - 70, y - 170, 141, 187, rotation = 10.0)

                    case DesecrationToken => DrawRect("ys-desecration", None, x - 20, y - 20, 41, 40)
                    case WebToken         => DrawRect("web-token", |(tint), x - 31, y - 30, 62, 60)
                    case IceAgeToken      => DrawRect("ww-ice-age", None, x - 44, y - 67, 91, 75)
                    case Cathedral        => DrawRect("an-cathedral", None, x - 39, y - 90, 78, 110)
                    case ChaosGate        => DrawRect("gate", |(Processing(|("#3C2E18"), None, |("#130E08"))), x - 38, y - 38, 76, 76)

                    case Ghast         => DrawRect("n-ghast", |(tint), x - 17, y - 53, 35, 59)
                    case Gug           => DrawRect("n-gug", |(tint), x - 36, y - 78, 73, 90)
                    case Shantak       => DrawRect("n-shantak", |(tint), x - 39, y - 89, 79, 100)
                    case StarVampire   => DrawRect("n-star-vampire", |(tint), x - 35, y - 75, 70, 85)
                    case Voonith       => if (region != null) DrawRect("n-voonith", |(tint), x - 35, y - 75, 70, 85)
                                          else DrawRect("n-voonith", |(tint), x - 22, y - 52, 45, 57)
                    case DimensionalShamblerUnit => if (region != null) DrawRect("n-dimensional-shambler", |(tint), x - 35, y - 75, 70, 85)
                                                    else DrawRect("n-dimensional-shambler", |(tint), x - 29, y - 60, 58, 70)
                    case Gnorri            => if (region != null) DrawRect("n-gnorri", |(tint), x - 50, y - 90, 100, 100)
                                              else DrawRect("n-gnorri", |(tint), x - 30, y - 54, 60, 60)
                    case Ygolonac      => DrawRect("n-ygolonac", |(tint), x - 37, y - 90, 75, 100)
                    case Byatis        => DrawRect("n-byatis", |(tint), x - 47, y - 90, 95, 90)
                    case Abhoth        => DrawRect("n-abhoth", |(tint), x - 47, y - 110, 95, 120)
                    case Filth         => DrawRect("n-filth", |(tint), x - 20, y - 20, 40, 40)
                    case Daoloth       => DrawRect("n-daoloth", |(tint), x - 59, y - 91, 118, 99)
                    case Nyogtha       => DrawRect("n-nyogtha", |(tint), x - 40, y - 69, 81, 80)
                    case Tulzscha         => if (region != null) DrawRect("n-tulzscha", |(tint), x - 69, y - 127, 137, 137)
                                             else DrawRect("n-tulzscha", |(tint), x - 50, y - 90, 100, 100)

                    // New Terrors
                    // Terrors (sized relative to cultist from docx thumbnails)
                    case Dhole             => DrawRect("n-dhole", |(tint), x - 50, y - 112, 100, 122)
                    case GreatRaceOfYith   => DrawRect("n-great-race-of-yith", |(tint), x - 64, y - 146, 128, 156)
                    case QuachilUttaus     => DrawRect("n-quachil-uttaus", |(tint), x - 65, y - 150, 131, 160)
                    case ShadowPharaoh     => DrawRect("n-the-shadow-pharaoh", |(tint), x - 63, y - 175, 126, 185)
                    case HoundOfTindalos   => DrawRect("n-hound-of-tindalos", |(tint), x - 48, y - 109, 95, 116)
                    case BrownJenkin       => DrawRect("n-brown-jenkin", |(tint), x - 43, y - 117, 86, 127)
                    case ElderShoggoth     => DrawRect("n-elder-shoggoth", |(tint), x - 50, y - 112, 100, 122)
                    // Monsters (sized relative to cultist from docx thumbnails)
                    case MoonbeastUnit     => DrawRect("n-moonbeast", |(tint), x - 38, y - 76, 77, 86)
                    case AlbinoPenguins    => DrawRect("n-giant-blind-albino-penguins", |(tint), x - 49, y - 130, 99, 140)
                    case ElderThing        => DrawRect("n-elder-thing", |(tint), x - 50, y - 103, 101, 113)
                    case LengSpiderUnit    => DrawRect("n-leng-spider", |(tint), x - 47, y - 95, 94, 105)
                    case Satyr             => DrawRect("n-satyr", |(tint), x - 44, y - 88, 88, 98)
                    case InsectsFromShaggai => DrawRect("n-insects-from-shaggai", |(tint), x - 38, y - 97, 76, 107)
                    case ServitorUnit      => DrawRect("n-servitor-of-the-outer-gods", |(tint), x - 35, y - 75, 70, 85)
                    // IGOOs (sized relative to cultist from docx thumbnails)
                    case AzathothIGOO      => DrawRect("n-azathoth", |(tint), x - 59, y - 128, 119, 138)
                    case Cthugha           => DrawRect("n-cthugha", |(tint), x - 52, y - 112, 105, 122)
                    case MotherHydra       => DrawRect("n-mother-hydra", |(tint), x - 62, y - 97, 124, 107)
                    case Yig               => DrawRect("n-yig", |(tint), x - 52, y - 112, 105, 122)
                    case FatherDagon       => DrawRect("n-father-dagon", |(tint), x - 60, y - 95, 121, 105)
                    case GhatanotoaIGOO    => DrawRect("n-ghatanothoa", |(tint), x - 62, y - 135, 125, 145)
                    case BloatedWoman      => DrawRect("n-the-bloated-woman", |(tint), x - 68, y - 147, 136, 157)
                    case AtlachNacha       => DrawRect("n-atlach-nacha", |(tint), x - 62, y - 110, 125, 110)
                    case Bokrug            => DrawRect("n-bokrug", |(tint), x - 55, y - 115, 110, 125)
                    case GlaakiIGOO        => DrawRect("n-glaaki-igoo", |(tint), x - 55, y - 115, 110, 125)

                    case GhastIcon        => DrawRect("ghast-icon", None, x - 17, y - 55, 50, 50)
                    case GugIcon          => DrawRect("gug-icon", None, x - 17, y - 55, 50, 50)
                    case ShantakIcon      => DrawRect("shantak-icon", None, x - 17, y - 55, 50, 50)
                    case StarVampireIcon  => DrawRect("star-vampire-icon", None, x - 17, y - 55, 50, 50)
                    case VoonithIcon      => DrawRect("voonith-icon", None, x - 17, y - 55, 50, 50)
                    case DimensionalShamblerIcon => DrawRect("dimensional-shambler-icon", None, x - 17, y - 55, 50, 50)
                    case GnorriIcon        => DrawRect("gnorri-icon", None, x - 17, y - 55, 50, 50)
                    case YgolonacIcon      => DrawRect("ygolonac-icon", None, x - 17, y - 55, 50, 50)
                    case ByatisIcon       => DrawRect("byatis-icon", None, x - 17, y - 55, 50, 50)
                    case AbhothIcon       => DrawRect("abhoth-icon", None, x - 17, y - 55, 50, 50)
                    case DaolothIcon      => DrawRect("daoloth-icon", None, x - 17, y - 55, 50, 50)
                    case NyogthaIcon      => DrawRect("nyogtha-icon", None, x - 17, y - 55, 50, 50)
                    case TulzschaIcon     => DrawRect("tulzscha-icon", None, x - 17, y - 55, 50, 50)
                    case HighPriestIcon   => DrawRect("high-priest-icon", None, x - 17, y - 55, 50, 50)
                    // New Terror icons
                    case DholeIcon              => DrawRect("dhole-icon", None, x - 17, y - 55, 50, 50)
                    case GreatRaceOfYithIcon     => DrawRect("great-race-of-yith-icon", None, x - 17, y - 55, 50, 50)
                    case QuachilUttausIcon       => DrawRect("quachil-uttaus-icon", None, x - 17, y - 55, 50, 50)
                    case ShadowPharaohIcon       => DrawRect("the-shadow-pharaoh-icon", None, x - 17, y - 55, 50, 50)
                    case HoundOfTindalosIcon     => DrawRect("hound-of-tindalos-icon", None, x - 17, y - 55, 50, 50)
                    case BrownJenkinIcon         => DrawRect("brown-jenkin-icon", None, x - 17, y - 55, 50, 50)
                    case ElderShoggothIcon       => DrawRect("elder-shoggoth-icon", None, x - 17, y - 55, 50, 50)
                    // New Monster icons
                    case MoonbeastIcon           => DrawRect("moonbeast-icon", None, x - 17, y - 55, 50, 50)
                    case AlbinoPenguinsIcon      => DrawRect("giant-blind-albino-penguins-icon", None, x - 17, y - 55, 50, 50)
                    case ElderThingIcon          => DrawRect("elder-thing-icon", None, x - 17, y - 55, 50, 50)
                    case LengSpiderIcon          => DrawRect("leng-spider-icon", None, x - 17, y - 55, 50, 50)
                    case SatyrIcon               => DrawRect("satyr-icon", None, x - 17, y - 55, 50, 50)
                    case InsectsFromShaggaiIcon  => DrawRect("insects-from-shaggai-icon", None, x - 17, y - 55, 50, 50)
                    case ServitorIcon            => DrawRect("servitor-of-the-outer-gods-icon", None, x - 17, y - 55, 50, 50)
                    // New IGOO icons
                    case AzathothIGOOIcon        => DrawRect("azathoth-icon", None, x - 17, y - 55, 50, 50)
                    case CthughaIcon             => DrawRect("cthugha-icon", None, x - 17, y - 55, 50, 50)
                    case MotherHydraIcon         => DrawRect("mother-hydra-icon", None, x - 17, y - 55, 50, 50)
                    case YigIcon                 => DrawRect("yig-icon", None, x - 17, y - 55, 50, 50)
                    case FatherDagonIcon         => DrawRect("father-dagon-icon", None, x - 17, y - 55, 50, 50)
                    case GhatanotoaIGOOIcon      => DrawRect("ghatanothoa-igoo-icon", None, x - 17, y - 55, 50, 50)
                    case BloatedWomanIcon        => DrawRect("the-bloated-woman-icon", None, x - 17, y - 55, 50, 50)
                    case AtlachNachaIcon         => DrawRect("atlach-nacha-icon", None, x - 17, y - 55, 50, 50)
                    case BokrugIcon              => DrawRect("bokrug-icon", None, x - 17, y - 55, 50, 50)
                    case GlaakiIGOOIcon          => DrawRect("glaaki-igoo-icon", None, x - 17, y - 55, 50, 50)

                    // Round 8 Bug 53: dedicated 66x66 render for the on-map starting glyph of
                    // dynamic-start factions (FB, TS). Dispatches by faction. Smaller than the
                    // 100x100 FactionGlyph used in faction status panels.
                    // Round 9: AN and OW also use dynamic glyph placement via findStaticGlyphPos.
                    case StartingGlyph if faction == FB => DrawRect("fb-glyph", None, x - 33, y - 33, 66, 66)
                    case StartingGlyph if faction == TS => DrawRect("ts-glyph", None, x - 33, y - 33, 66, 66)
                    case StartingGlyph if faction == AN => DrawRect("an-glyph", None, x - 33, y - 33, 66, 66)
                    case StartingGlyph if faction == OW => DrawRect("ow-glyph", None, x - 33, y - 33, 66, 66, alpha = 0.55)

                    // Library map units — larger than monsters, smaller than GOOs
                    case TheCustodian => DrawRect("custodian-icon", |(Processing(None, |("rgba(255,255,255,0.2)"), None)), x - 52, y - 104, 104, 104)
                    case TheLibrarian => DrawRect("librarian-icon", |(Processing(None, |("rgba(255,255,255,0.2)"), None)), x - 47, y - 146, 94, 146)

                    case _ => null
                }

                // Library maps: scale up unit sprites to match Earth-map visual size.
                // Library maps are 2-floor joined images (~2x larger per dimension).
                // Only scale on-map units (region != null), not faction card glyphs.
                def scaledProto : DrawRect = {
                    val p = proto
                    if (p == null) return null
                    val baseScale = if (region != null) board.unitScale else 1.0
                    // Two shrink factors for on-map units on library maps:
                    //   classK   — class-global per-region shrink for "large" units (proto maxDim > 1.6×cultist)
                    //              in non-small regions where they'd otherwise pack too tight.
                    //   uniformK — flat per-region shrink that applies to EVERY object (units, gates, tokens)
                    //              in regions ≤ Gloomloft 5U bbox area. classK is suppressed there.
                    val classK   = if (region != null && board.isLibraryMap) classShrink.getOrElse(unit, 1.0) else 1.0
                    val uniformK = if (region != null && board.isLibraryMap) regionUniformShrink.getOrElse(region, 1.0) else 1.0
                    val shrinkK  = classK * uniformK
                    val s = baseScale * shrinkK
                    if (math.abs(s - 1.0) > 0.01) p.copy(
                        x = (x + (p.x - x) * s).toInt,
                        y = (y + (p.y - y) * s).toInt,
                        width = (p.width * s).toInt,
                        height = (p.height * s).toInt,
                        cx = (p.cx * s).toInt,
                        cy = (p.cy * s).toInt
                    ) else p
                }

                def rect = scaledProto
                    .useIf(tags.has(Hidden))(_.copy(alpha = 0.5))
                    .useIf(tags.has(Absorbed)) { r =>
                        val k = math.sqrt(1 + tags.count(Absorbed))
                        r.copy(x = r.x + (r.width * 0.5 + r.cx) * (1 - k) ~, y = r.y + (r.height * 0.5 + r.cy) * (1 - k) ~, width = r.width * k ~, height = r.height * k ~, cx = r.cx * k ~, cy = r.cy * k ~)
                    }
                    .useIf(tags.has(Mummified))(_.copy(rotation = 90.0))

                def icon = {
                    val hs = (30 * board.unitScale).toInt
                    val is = (60 * board.unitScale).toInt
                    if (health == Killed)
                        |(DrawRect("kill", None, x + rect.cx - hs, (rect.y + y) / 2 + rect.cy - hs, is, is))
                    else
                    if (health == Pained)
                        |(DrawRect("pain", None, x - hs + 1 + rect.cx, (rect.y + y) / 2 + rect.cy - hs, is, is))
                    else
                        None
                }
            }

            var oldPositions : $[DrawItem] = $
            var oldGates : $[Region] = $
            // Tracks per-tome "on map" state; re-placement triggers when a tome is picked
            // up so units can flow into the formerly-occupied space.
            var oldTomeOnMap : Map[LibraryTome, Boolean] = Map.empty
            var horizontal = true

            var cachedMapWidth : Double = 0
            var cachedMapHeight : Double = 0
            var cachedDPR : Double = 0

            def invalidateMapSize() {
                cachedMapWidth = 0
                cachedMapHeight = 0
            }

            def drawMap(implicit game : Game) {
                val upscale = 2

                val dpr = dom.window.devicePixelRatio
                if (cachedMapWidth == 0 || cachedDPR != dpr) {
                    cachedMapWidth = map.node.clientWidth * dpr
                    cachedMapHeight = map.node.clientHeight * dpr
                    cachedDPR = dpr
                }
                val width = cachedMapWidth
                val height = cachedMapHeight

                val bitmap = map.get(width.~ * upscale, height.~ * upscale)

                bitmap.canvas.style.width = "100%"
                bitmap.canvas.style.height = "100%"

                if (bitmap.height <= bitmap.width != horizontal) {
                    horizontal = !horizontal
                    oldPositions = $
                    oldGates = $
                    // Earth maps: gateXY rotates coords on H/V flip, so cached
                    // glyph positions are stale. Library maps clear this cache
                    // in their own placement-bitmap-swap block below; Earth maps
                    // had no clear path until now.
                    glyphPosCache.clear()
                }

                // Library maps: switch placement bitmap based on orientation
                if (board.isLibraryMap) {
                    val target = if (horizontal) glyphPlacerH else glyphPlacerV
                    if (activeGlyphPlacer ne target) {
                        activeGlyphPlacer = target
                        glyphPosCache.clear()
                    }
                }

                var mp = if (board.isLibraryMap && horizontal)
                    getAsset(board.id + "-h")
                else
                    getAsset(board.id)

                val gateXYO : Region => (Int, Int) =
                    if (board.isLibraryMap && horizontal) board.gateXYOHorizontal
                    else board.gateXYO

                def gateXY(r : Region) =
                    if (horizontal && !board.isLibraryMap)
                        gateXYO(r)
                    else if (board.isLibraryMap) {
                        // Library maps: both orientations use pre-joined images, no coord rotation
                        val (x, y) = gateXYO(r)
                        (x, y)
                    }
                    else {
                        val (x, y) = gateXYO(r)
                        (mp.height - y, x)
                    }

                def find(ox : Int, oy : Int) =
                    if (horizontal)
                        findAnother(ox, oy)
                    else if (board.isLibraryMap)
                        findAnother(ox, oy)
                    else {
                        val (x, y) = findAnother(oy, mp.height - ox)
                        (mp.height - y, x)
                    }

                val g = bitmap.context

                g.setTransform(1, 0, 0, 1, 0, 0)

                g.clearRect(0, 0, bitmap.width, bitmap.height)

                if (horizontal)
                {
                    val dw = 12
                    val dh = 12
                    if ((dw + mp.width + dw) * bitmap.height < bitmap.width * (dh + mp.height + dh)) {
                        g.translate((bitmap.width - (dw + mp.width + dw) * bitmap.height / (dh + mp.height + dh)) / 2, 0)
                        g.scale(1.0 * bitmap.height / (dh + mp.height + dh), 1.0 * bitmap.height / (dh + mp.height + dh))
                    }
                    else {
                        g.translate(0, (bitmap.height - (dh + mp.height + dh) * bitmap.width / (dw + mp.width + dw)) / 2)
                        g.scale(1.0 * bitmap.width / (dw + mp.width + dw), 1.0 * bitmap.width / (dw + mp.width + dw))
                    }
                    g.translate(dw, dh)
                    g.drawImage(mp, 0, 0)
                }
                else if (board.isLibraryMap) {
                    // Library maps: vertical image is pre-joined (Upper on top, Lower on bottom)
                    // No rotation needed — draw directly, fitting to canvas
                    val dw = 12
                    val dh = 12
                    if ((dw + mp.width + dw) * bitmap.height < bitmap.width * (dh + mp.height + dh)) {
                        g.translate((bitmap.width - (dw + mp.width + dw) * bitmap.height / (dh + mp.height + dh)) / 2, 0)
                        g.scale(1.0 * bitmap.height / (dh + mp.height + dh), 1.0 * bitmap.height / (dh + mp.height + dh))
                    }
                    else {
                        g.translate(0, (bitmap.height - (dh + mp.height + dh) * bitmap.width / (dw + mp.width + dw)) / 2)
                        g.scale(1.0 * bitmap.width / (dw + mp.width + dw), 1.0 * bitmap.width / (dw + mp.width + dw))
                    }
                    g.translate(dw, dh)
                    g.drawImage(mp, 0, 0)
                }
                else {
                    g.translate(bitmap.width, 0)
                    g.rotate(math.Pi / 2)

                    val dw = 12
                    val dh = 12
                    if ((dw + mp.width + dw) * bitmap.width < bitmap.height * (dh + mp.height + dh)) {
                        g.translate((bitmap.height - (dw + mp.width + dw) * bitmap.width / (dh + mp.height + dh)) / 2, 0)
                        g.scale(1.0 * bitmap.width / (dh + mp.height + dh), 1.0 * bitmap.width / (dh + mp.height + dh))
                    }
                    else {
                        g.translate(0, (bitmap.width - (dh + mp.height + dh) * bitmap.height / (dw + mp.width + dw)) / 2)
                        g.scale(1.0 * bitmap.height / (dw + mp.width + dw), 1.0 * bitmap.height / (dw + mp.width + dw))
                    }
                    g.translate(dw, dh)
                    g.drawImage(mp, 0, 0)
                    g.rotate(-math.Pi / 2)
                    g.translate(-mp.width/2, 0)
                }

                var saved = oldPositions
                oldPositions = $

                var draws : $[DrawItem] = $

                //
                // §10 Library-map per-class shrink pre-pass
                //
                // Shrink target:
                //   the smallest k such that the shrunk sprite satisfies the PRIMARY
                //   caps (bottom 0%, top ≤30%, sides ≤10%), with a floor at 0.455
                //   (bumped from 0.35 — minimum was visually too small for large units).
                //
                // Class-global: classShrink(uc) = min(k_per_region) across all regions
                // where the class has an instance. So a single tight region forces
                // every instance of the class on the map to render at the same size.
                classShrink.clear()
                if (board.isLibraryMap && place.nonEmpty) {
                    if (!regionGeomComputed) {
                        val pw = place.length
                        val ph = if (pw > 0) place(0).length else 0
                        val bounds = scala.collection.mutable.Map[Int, (Int, Int, Int, Int)]()
                        // sum_x, sum_y, count per region color for centroid computation.
                        val centroidSums = scala.collection.mutable.Map[Int, (Long, Long, Int)]()
                        var py0 = 0
                        while (py0 < ph) {
                            var px0 = 0
                            while (px0 < pw) {
                                val c = place(px0)(py0)
                                val r0 = (c >> 16) & 0xFF
                                val g0 = (c >> 8) & 0xFF
                                val b0 = c & 0xFF
                                val isWhite = r0 > 240 && g0 > 240 && b0 > 240
                                if (c != 0 && !isWhite) {
                                    bounds.get(c) match {
                                        case Some((mnX, mxX, mnY, mxY)) =>
                                            bounds(c) = (math.min(mnX, px0), math.max(mxX, px0), math.min(mnY, py0), math.max(mxY, py0))
                                        case None =>
                                            bounds(c) = (px0, px0, py0, py0)
                                    }
                                    centroidSums.get(c) match {
                                        case Some((sx, sy, n)) =>
                                            centroidSums(c) = (sx + px0, sy + py0, n + 1)
                                        case None =>
                                            centroidSums(c) = (px0.toLong, py0.toLong, 1)
                                    }
                                }
                                px0 += 1
                            }
                            py0 += 1
                        }
                        board.regions.foreach { rg =>
                            // 2026-05-22: identify region pixels by PALETTE color, not by
                            // sampling the hand-coded gateXY (which mis-identified regions on
                            // 3U/3L bitmaps). See Library mirror for full context.
                            val (gx0, gy0) = gateXY(rg)
                            val color = RegionPalette.getOrElse(rg, place(gx0.min(pw - 1).max(0))(gy0.min(ph - 1).max(0)))
                            bounds.get(color).foreach { case (mnX, mxX, mnY, mxY) =>
                                val regionWidth = mxX - mnX
                                val gateTopY = gy0 - (38 * board.unitScale).toInt
                                val availAbove = (gateTopY - mnY).max(0)
                                regionGeom(rg) = (regionWidth, availAbove)
                                regionArea(rg) = (mxX - mnX) * (mxY - mnY)
                            }
                            centroidSums.get(color).foreach { case (sx, sy, n) =>
                                if (n > 0) regionCentroid(rg) = ((sx / n).toInt, (sy / n).toInt)
                            }
                        }
                        regionGeomComputed = true
                    }

                    // Identify "small" library regions (bbox area ≤ Gloomloft 5U).
                    // Every object in these regions gets a flat 0.852 shrink applied
                    // up front. Class-global shrink (below) is suppressed for these,
                    // since the uniform handles them.
                    regionUniformShrink.clear()
                    regionArea.foreach { case (rg, area) =>
                        if (area <= SmallRegionAreaCutoff) regionUniformShrink(rg) = SmallRegionUniformShrink
                    }

                    // 2026-05-21: large-unit-trigger regions also get the uniform shrink
                    // applied to every object in the region — per user spec "If a large
                    // unit triggers the shrinking rule in an area, then apply the all
                    // unit resizing to that area."
                    factions.foreach { f =>
                        f.allInPlay.foreach { u =>
                            if (u.region != null && regionGeom.contains(u.region) && !regionUniformShrink.contains(u.region)) {
                                val sample = DrawItem(u.region, f, u.uclass, u.health, u.state, 0, 0)
                                val protoR = sample.proto
                                if (protoR != null) {
                                    val maxDim = math.max(protoR.width, protoR.height)
                                    if (maxDim > LargeUnitTriggerMaxDim)
                                        regionUniformShrink(u.region) = SmallRegionUniformShrink
                                }
                            }
                        }
                    }

                    // classRegions stays effectively empty after the loop above marks
                    // every large-unit region for uniform shrink. Kept as a defensive
                    // no-op so the downstream classShrink loop iterates nothing.
                    val classRegions = scala.collection.mutable.Map[UnitClass, scala.collection.mutable.Set[Region]]()
                    factions.foreach { f =>
                        f.allInPlay.foreach { u =>
                            if (u.region != null && regionGeom.contains(u.region) && !regionUniformShrink.contains(u.region)) {
                                val sample = DrawItem(u.region, f, u.uclass, u.health, u.state, 0, 0)
                                val protoR = sample.proto
                                if (protoR != null) {
                                    val maxDim = math.max(protoR.width, protoR.height)
                                    if (maxDim > LargeUnitTriggerMaxDim) {
                                        val set = classRegions.getOrElseUpdate(u.uclass, scala.collection.mutable.Set.empty)
                                        set += u.region
                                    }
                                }
                            }
                        }
                    }
                    // [2026-05-23] Custodian + Librarian belong to LibraryFaction, which
                    // isn't in `factions` (real players only). Without this they bypass
                    // classShrink entirely and render full-size everywhere. Treat them
                    // as a synthetic "library faction" iteration so they're eligible
                    // for the same per-region shrink as other large units. The
                    // uniformK in small regions already applied (it's region-based,
                    // not faction-based); this is the missing classK path.
                    $((TheCustodian, game.custodianRegion), (TheLibrarian, game.librarianRegion)).foreach { case (uc, regionOpt) =>
                        regionOpt.foreach { r =>
                            if (regionGeom.contains(r) && !regionUniformShrink.contains(r)) {
                                val sample = DrawItem(r, LibraryFaction, uc, Alive, $, 0, 0)
                                val protoR = sample.proto
                                if (protoR != null) {
                                    val maxDim = math.max(protoR.width, protoR.height)
                                    if (maxDim > LargeUnitTriggerMaxDim) {
                                        val set = classRegions.getOrElseUpdate(uc, scala.collection.mutable.Set.empty)
                                        set += r
                                    }
                                }
                            }
                        }
                    }

                    classRegions.foreach { case (uc, regs) =>
                        val sampleAny = DrawItem(regs.head, null, uc, Alive, $, 0, 0)
                        val protoR = sampleAny.proto
                        if (protoR != null) {
                            val swScaled = (protoR.width * board.unitScale).toDouble
                            val shScaled = (protoR.height * board.unitScale).toDouble
                            val ks = regs.toList.flatMap { rg =>
                                regionGeom.get(rg).map { case (regionW, availAbove) =>
                                    // F2 fit check at full size: top ≤60% (40% must fit
                                    // above gate), sides ≤30% (40% must fit width-wise).
                                    val vertK_F2 = if (shScaled > 0) (availAbove / 0.4) / shScaled else 1.0
                                    val horK_F2  = if (swScaled > 0) (regionW / 0.4)  / swScaled else 1.0
                                    val f2Fits = vertK_F2 >= 1.0 && horK_F2 >= 1.0
                                    if (f2Fits) 1.0
                                    else {
                                        // F2 fails — shrink to fit primary caps.
                                        // Primary: top ≤30% (70% above gate), sides ≤10% (80% in region).
                                        val vertK = if (shScaled > 0) (availAbove / 0.7) / shScaled else 1.0
                                        val horK  = if (swScaled > 0) (regionW / 0.8)  / swScaled else 1.0
                                        math.min(math.min(vertK, horK), 1.0).max(0.455)
                                    }
                                }
                            }
                            val effectiveK = if (ks.isEmpty) 1.0 else ks.min
                            if (effectiveK < 0.999) classShrink(uc) = effectiveK
                        }
                    }

                    // §23: enforce a per-GOO floor so GOOs always render
                    // larger than the largest cultist sprite. Without this, the
                    // 0.455 hard floor + library-map tight regions could shrink
                    // a GOO below an un-shrunk Acolyte/HP, which is wrong.
                    // Cultists never shrink via classShrink (maxDim < 1.6×cultist eligibility check).
                    val cultistDims = factions.flatMap { f =>
                        f.allInPlay.%(_.uclass.utype == Cultist).flatMap { u =>
                            val sample = DrawItem(u.region, f, u.uclass, u.health, u.state, 0, 0)
                            Option(sample.proto).map(p => math.max(p.width, p.height))
                        }
                    }
                    val maxCultistDim = if (cultistDims.nonEmpty) cultistDims.max else 0
                    if (maxCultistDim > 0) {
                        // Per-non-cultist floor: any unit whose proto is naturally larger than
                        // the largest cultist must render at least 1.30× cultist max dim.
                        // Units whose proto is <= cultist size (e.g. Ghouls) are intentionally
                        // small and are skipped — they shrink as before.
                        val nonCultistMinDim = maxCultistDim * 1.30
                        classShrink.toList.foreach { case (uc, k) =>
                            if (uc.utype != Cultist) {
                                val instance = factions.iterator.flatMap(_.allInPlay.iterator)
                                    .find(u => u.uclass == uc && u.region != null)
                                instance.foreach { u =>
                                    val sample = DrawItem(u.region, u.faction, u.uclass, u.health, u.state, 0, 0)
                                    val protoR = sample.proto
                                    if (protoR != null) {
                                        val unitMaxDim = math.max(protoR.width, protoR.height).toDouble
                                        if (unitMaxDim > nonCultistMinDim) {  // eligibility: naturally bigger than the floor target
                                            val effective = unitMaxDim * k
                                            if (effective < nonCultistMinDim) {
                                                val raisedK = math.min(1.0, nonCultistMinDim / unitMaxDim)
                                                classShrink(uc) = raisedK
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                areas.foreach { r =>
                    val (rawPx, rawPy) = gateXY(r)
                    val gated = game.gates.has(r)

                    // Auto-center the gate at the region's pixel centroid for library
                    // regions that DON'T host a tome on the map. See Library at Celaeno
                    // mirror for rationale.
                    val isTomeRegion = board.isLibraryMap && TomePlacement.positions.exists(_._1.region == r)
                    val (centeredPx, centeredPy) =
                        if (board.isLibraryMap && !isTomeRegion && regionCentroid.contains(r))
                            regionCentroid(r)
                        else
                            (rawPx, rawPy)

                    // Library: check if an unclaimed tome is in this region
                    val tomeInRegion = if (board.isLibraryMap) TomePlacement.positions.find { case (tome, _, _, _, _) =>
                        tome.region == r && !game.tomeHolders.getOrElse(tome, None).isDefined
                    } else None

                    // Gate position: offset away from tome when tome is present.
                    val (gatePx, gatePy) = tomeInRegion match {
                        case Some((_, _, _, _, th)) if gated => (centeredPx, centeredPy + (th * 0.7).toInt)
                        case _ => (centeredPx, centeredPy)
                    }
                    // Unit anchor: when tome present WITHOUT gate, offset units around tome.
                    // Tome-present branch keeps the hand-coded XY base because that base
                    // was chosen to position units relative to the tome graphic.
                    val (pxRaw, pyRaw) = tomeInRegion match {
                        case Some((_, _, _, _, th)) if !gated => (rawPx, rawPy + (th * 0.7).toInt)
                        case _ => (gatePx, gatePy)
                    }

                    // 2026-05-21: clamp the tome-offset anchor back into the region if the
                    // fixed 0.7×tome-height shift overshoots the bottom edge (happens when
                    // a region is shorter in 3U than in 5U — e.g. Guardian Under the Lake
                    // in library3-place.webp). See Library at Celaeno mirror for details.
                    val (px, py) = if (board.isLibraryMap && place.nonEmpty) {
                        val ax = pxRaw.min(place.length - 1).max(0)
                        val ay0 = pyRaw.min(place(0).length - 1).max(0)
                        val targetY = gatePy.min(place(0).length - 1).max(0)
                        def isWhite(c : Int) =
                            (c >> 16 & 0xFF) > 240 && (c >> 8 & 0xFF) > 240 && (c & 0xFF) > 240
                        if (isWhite(place(ax)(ay0))) {
                            var ny = ay0
                            val step = if (ay0 > targetY) -1 else 1
                            var tries = 0
                            while (ny != targetY && isWhite(place(ax)(ny)) && tries < 400) {
                                ny += step; tries += 1
                            }
                            if (isWhite(place(ax)(ny))) (gatePx, gatePy) else (ax, ny)
                        } else (ax, ay0)
                    } else (pxRaw, pyRaw)

                    // Library maps: validate gate fits inside region, nudge if needed.
                    // Sample regionColor at the (possibly auto-centered) gatePx/gatePy
                    // rather than the hand-coded rawPx/rawPy, so the nudge loop is
                    // anchored to the centroid for non-tome regions.
                    val (adjGatePx, adjGatePy) = if (board.isLibraryMap && place.nonEmpty) {
                        val halfGate = (38 * board.unitScale).toInt
                        val gpx = gatePx.min(place.length - 1).max(0)
                        val gpy = gatePy.min(place(0).length - 1).max(0)
                        var regionColor = place(gpx)(gpy)
                        // If center lands on white boundary, search nearby for the real region color
                        val isWhite = (regionColor >> 16 & 0xFF) > 240 && (regionColor >> 8 & 0xFF) > 240 && (regionColor & 0xFF) > 240
                        if (isWhite) {
                            val searchOffsets = $(10, 20, 30, 40, 50)
                            val dirs = $(0 -> 1, 1 -> 0, 0 -> -1, -1 -> 0, 1 -> 1, -1 -> -1)
                            var found = false
                            searchOffsets.foreach { d => if (!found) dirs.foreach { case (dx, dy) => if (!found) {
                                val sx = (gpx + dx * d).min(place.length - 1).max(0)
                                val sy = (gpy + dy * d).min(place(0).length - 1).max(0)
                                val c = place(sx)(sy)
                                val cw = (c >> 16 & 0xFF) > 240 && (c >> 8 & 0xFF) > 240 && (c & 0xFF) > 240
                                if (!cw && c != 0) { regionColor = c; found = true }
                            }}}
                        }
                        var gx = gatePx; var gy = gatePy
                        // Check corners of gate box; if any are outside region, nudge inward
                        var nudged = false
                        for (attempt <- 0 until 15 if !nudged) {
                            val offsets = $(-halfGate, halfGate)
                            var outsideCount = 0
                            var nudgeDx = 0; var nudgeDy = 0
                            offsets.foreach { ox => offsets.foreach { oy =>
                                val cx = (gx + ox).min(place.length - 1).max(0)
                                val cy = (gy + oy).min(place(0).length - 1).max(0)
                                if (place(cx)(cy) != regionColor) {
                                    outsideCount += 1
                                    // Push away from the failing corner
                                    nudgeDx -= ox.sign * 8
                                    nudgeDy -= oy.sign * 8
                                }
                            }}
                            if (outsideCount == 0) nudged = true
                            else {
                                gx += nudgeDx
                                gy += nudgeDy
                            }
                        }
                        (gx, gy)
                    } else (gatePx, gatePy)

                    val controler = game.setup.%(_.gates.has(r)).single
                    val keeper = controler./~(_.at(r).%(_.onGate).%(_.health == Alive).starting)

                    var fixed : $[DrawItem] = $
                    var all : $[DrawItem] = $
                    var sticking : $[DrawItem] = $
                    var free : $[DrawItem] = $

                    if (gated && DS.chaosGateRegions.has(r).not)
                        fixed +:= DrawItem(r, null, Gate, Alive, $, adjGatePx, adjGatePy)

                    if (DS.chaosGateRegions.has(r))
                        fixed +:= DrawItem(r, DS, ChaosGate, Alive, $, adjGatePx, adjGatePy)

                    keeper match {
                        case Some(u) =>
                            val tags = u.state ++ game.mummifiedCultists.has(u.ref).$(Mummified)
                            fixed +:= DrawItem(r, u.faction, u.uclass, u.health, tags, adjGatePx, adjGatePy)
                        case _ =>
                    }

                    factionlike.foreach { f =>
                        f.at(r).diff(keeper.$).foreach { u =>
                            val tags = u.state ++ game.mummifiedCultists.has(u.ref).$(Mummified)
                            all +:= DrawItem(r, f, u.uclass, u.health, tags, 0, 0)
                        }
                    }

                    // Library at Celaeno: Custodian and Librarian on map as proper units
                    if (board.isLibraryMap) {
                        if (game.custodianRegion.has(r))
                            all +:= DrawItem(r, LibraryFaction, TheCustodian, Alive, $, 0, 0)
                        if (game.librarianRegion.has(r))
                            all +:= DrawItem(r, LibraryFaction, TheLibrarian, Alive, $, 0, 0)
                    }

                    if (game.desecrated.has(r))
                        all +:= DrawItem(r, null, DesecrationToken, Alive, $, 0, 0)

                    if (game.webTokens.has(r)) {
                        // Atlach-Nacha: tint web token with owner's faction color
                        val anOwner = game.factions.find(f => f.units.exists(_.uclass == AtlachNacha))
                        all +:= DrawItem(r, anOwner.|(null), WebToken, Alive, $, 0, 0)
                    }

                    if (game.cathedrals.has(r))
                        all +:= DrawItem(r, null, Cathedral, Alive, $, 0, 0)

                    // Firstborn (FB): draw Crater token on map exactly where the gate was
                    // (placed as fixed so the layout engine doesn't offset it like other tokens)
                    if (game.factions.has(FB) && game.fbCraters.has(r))
                        fixed +:= DrawItem(r, FB, Crater, Alive, $, px, py)

                    // Firstborn (FB): draw FB faction glyph at starting region if FB is in the game
                    // (dynamic-start factions don't have permanent board glyphs — render explicitly).
                    // Round 8 Bug 44: use findStaticGlyphPos which scans the placement map to find
                    // the position farthest from the gate where the 60x60 glyph box fits fully
                    // within the region. Guarantees the glyph never overlaps another region
                    // (highest priority) and minimizes gate overlap (placed in the largest empty
                    // space). Glyph is static — same position every render.
                    // Round 8 Bug 52: use `game.setup` (the full faction list set at game creation)
                    // instead of `game.factions` (the play order, set only after PlayDirectionAction).
                    // Otherwise the glyph wouldn't render until after play order was decided.
                    // Faction starting glyphs — placed offset from gate to avoid overlap
                    // Library maps: use fixed offset (findStaticGlyphPos fails on large scaled regions)
                    // Earth maps: use findStaticGlyphPos for smart placement
                    def placeGlyph(f : Faction) {
                        if (game.setup.has(f) && game.starting.get(f).contains(r)) {
                            val (glyphX, glyphY) = if (board.isLibraryMap) {
                                // Search for a position where 80%+ of the glyph is inside the region
                                // Use validated gate position (adjGatePx/Py) as search center
                                var regionColor = place(adjGatePx.min(place.length - 1).max(0))(adjGatePy.min(place(0).length - 1).max(0))
                                // If center is white boundary, search nearby for real region color
                                val rcWhite = (regionColor >> 16 & 0xFF) > 240 && (regionColor >> 8 & 0xFF) > 240 && (regionColor & 0xFF) > 240
                                if (rcWhite) {
                                    val sOffsets = $(10, 20, 30, 40, 50)
                                    val sDirs = $(0 -> 1, 1 -> 0, 0 -> -1, -1 -> 0, 1 -> 1, -1 -> -1)
                                    var rcFound = false
                                    sOffsets.foreach { d => if (!rcFound) sDirs.foreach { case (ddx, ddy) => if (!rcFound) {
                                        val sx2 = (adjGatePx + ddx * d).min(place.length - 1).max(0)
                                        val sy2 = (adjGatePy + ddy * d).min(place(0).length - 1).max(0)
                                        val c2 = place(sx2)(sy2)
                                        val c2w = (c2 >> 16 & 0xFF) > 240 && (c2 >> 8 & 0xFF) > 240 && (c2 & 0xFF) > 240
                                        if (!c2w && c2 != 0) { regionColor = c2; rcFound = true }
                                    }}}
                                }
                                val halfG = (33 * board.unitScale).toInt
                                val halfGate = (38 * board.unitScale).toInt
                                val searchRadius = (150 * board.unitScale).toInt

                                // Try offsets in a spiral pattern around the validated gate position
                                val searchCx = adjGatePx
                                val searchCy = adjGatePy
                                var bestX = searchCx
                                var bestY = searchCy
                                var bestScore = -1
                                val step = 10
                                var dy = -searchRadius
                                while (dy <= searchRadius) {
                                    var dx = -searchRadius
                                    while (dx <= searchRadius) {
                                        val cx = searchCx + dx
                                        val cy = searchCy + dy
                                        if (cx >= halfG && cx < place.length - halfG && cy >= halfG && cy < place(0).length - halfG) {
                                            // Count region pixels in glyph box
                                            var inRegion = 0
                                            var total = 0
                                            var sy = -halfG
                                            while (sy <= halfG) {
                                                var sx = -halfG
                                                while (sx <= halfG) {
                                                    total += 1
                                                    if (place(cx + sx)(cy + sy) == regionColor) inRegion += 1
                                                    sx += 4
                                                }
                                                sy += 4
                                            }
                                                            // Must have 80%+ inside region AND not overlap gate
                                            val pct = inRegion * 100 / total
                                            val gateOverlap = scala.math.abs(cx - searchCx) < halfGate + halfG && scala.math.abs(cy - searchCy) < halfGate + halfG
                                            // Prefer below-gate placement (higher cy = lower on screen)
                                            val belowBonus = if (cy > searchCy) 500 else 0
                                            val score = if (pct >= 80 && !gateOverlap) pct * 1000 + belowBonus + (searchRadius - scala.math.abs(dx) - scala.math.abs(dy))
                                                else if (pct >= 80) pct
                                                else -1
                                            if (score > bestScore) {
                                                bestScore = score
                                                bestX = cx
                                                bestY = cy
                                            }
                                        }
                                        dx += step
                                    }
                                    dy += step
                                }
                                (bestX, bestY)
                            } else if (!horizontal && !board.isLibraryMap) {
                                // Portrait/mobile: rotate coords to original space, find glyph, rotate back.
                                // [2026-05-23] Pass the canonical EarthRegionPalette color so
                                // findStaticGlyphPos doesn't sample the bitmap at the rotated
                                // gate XY (which can land on a GAP pixel and cause the
                                // algorithm to operate on the wrong region — the Australia bug).
                                val (ox, oy) = (rawPy, mp.height - rawPx)
                                val rc = EarthRegionPalette.get(board.id, r).getOrElse(-1)
                                val (fx, fy) = findStaticGlyphPos(ox, oy, rc)
                                (mp.height - fy, fx)
                            } else {
                                // Landscape Earth path. Same palette hardening — the bitmap
                                // sample CAN be wrong even in landscape on certain mobile
                                // viewers / cached asset states. Belt-and-braces.
                                val rc = if (!board.isLibraryMap) EarthRegionPalette.get(board.id, r).getOrElse(-1) else -1
                                findStaticGlyphPos(rawPx, rawPy, rc)
                            }
                            fixed +:= DrawItem(r, f, StartingGlyph, Alive, $, glyphX, glyphY)
                        }
                    }
                    placeGlyph(FB)
                    placeGlyph(TS)
                    placeGlyph(AN)
                    placeGlyph(OW)

                    if (game.setup.%(_.iceAge.?(_ == r)).any)
                        all +:= DrawItem(r, null, IceAgeToken, Alive, $, 0, 0)

                    all.foreach { d =>
                        saved.find(o => d.region == o.region && d.faction == o.faction && d.unit == o.unit && d.tags == o.tags && d.health == o.health) match {
                            case Some(o) =>
                                sticking +:= o
                                saved :-= o
                            case None =>
                                saved.find(o => d.region == o.region && d.faction == o.faction && d.unit == o.unit && d.tags == o.tags) match {
                                    case Some(o) =>
                                        sticking +:= o.copy(health = d.health)
                                        saved :-= o
                                    case None =>
                                        saved.find(o => d.region == o.region && d.faction == o.faction && d.unit == o.unit) match {
                                            case Some(o) =>
                                                sticking +:= o.copy(tags = d.tags, health = d.health)
                                                saved :-= o
                                            case None =>
                                                free +:= d
                                        }
                                }
                        }
                    }

                    // Tome state change in this region (picked up or replaced) → force
                    // re-placement of sticky units so they fill / vacate the tome's space.
                    val tomeStateChangedHere = board.isLibraryMap && TomePlacement.positions.exists { case (tome, _, _, _, _) =>
                        tome.region == r && {
                            val isOnMapNow = !game.tomeHolders.getOrElse(tome, None).isDefined
                            val wasOnMap   = oldTomeOnMap.getOrElse(tome, isOnMapNow)
                            wasOnMap != isOnMapNow
                        }
                    }

                    if (free.num > sticking.num * 0 + 3 || free.%(_.unit.utype == GOO).any || (oldGates.has(r).not && game.gates.has(r)) || tomeStateChangedHere) {
                        free = free ++ sticking
                        sticking = $
                    }

                    def rank(d : DrawItem) = d.unit.utype match {
                        case Token => 0
                        case MapUnit => 0
                        case Building => 0
                        case Cultist => 1
                        case Monster => 2
                        case Terror => 3
                        case GOO => 4
                    }

                    // Library maps: perspective-aware placement. Sprites are bottom-
                    // anchored (the bottom row at y is the unit's "footprint" on the
                    // board, like a real game piece). Apply directional spill caps
                    // matching a perspective view: bottom must be in region, but the
                    // top of the sprite (further from the viewer) may extend into a
                    // neighboring region, and left/right may extend slightly.
                    //
                    // Primary caps (D-perspective):
                    //   bottom 0%, top ≤ 30%, left ≤ 10%, right ≤ 10%
                    // Fallback caps when nothing qualifies (F2):
                    //   bottom 0%, top ≤ 60%, left ≤ 30%, right ≤ 30%
                    //
                    // Within qualifying candidates: minimize unit-on-unit overlap
                    // (the gate sits in `fixed`, so its overlap penalty avoids
                    // placing units on the gate). Earth maps fall through to base
                    // overlap-only behavior unchanged.
                    val unitRegionColor = if (board.isLibraryMap && place.nonEmpty)
                        place(px.min(place.length - 1).max(0))(py.min(place(0).length - 1).max(0))
                    else 0

                    // For a given rect, compute (bottomOut, topOut, leftOut, rightOut)
                    // as fractions in [0, 1] of the sprite's height/width respectively
                    // that fall outside the region color.
                    def directionalSpill(rect : DrawRect) : (Double, Double, Double, Double) = {
                        if (rect == null || !board.isLibraryMap || place.isEmpty) return (0.0, 0.0, 0.0, 0.0)
                        val pw = place.length
                        val ph = place(0).length
                        // Sample the bottom row (1px tall): count out-of-region samples horizontally.
                        var bottomBad = 0; var bottomTotal = 0
                        var sx = rect.x
                        while (sx < rect.x + rect.width) {
                            val cx = sx.min(pw - 1).max(0)
                            val cy = (rect.y + rect.height - 1).min(ph - 1).max(0)
                            if (place(cx)(cy) != unitRegionColor) bottomBad += 1
                            bottomTotal += 1
                            sx += 4
                        }
                        // Sample top row (just the very top of the sprite).
                        // Per-DIRECTION top spill: how many TOP rows are entirely out of region,
                        // measured as fraction of sprite height.
                        var topRowsOut = 0
                        var sy = rect.y
                        var topRowsScanned = 0
                        var topDone = false
                        while (sy < rect.y + rect.height && !topDone) {
                            var allOut = true
                            var rx = rect.x
                            while (rx < rect.x + rect.width && allOut) {
                                val cx = rx.min(pw - 1).max(0)
                                val cy = sy.min(ph - 1).max(0)
                                if (place(cx)(cy) == unitRegionColor) allOut = false
                                rx += 4
                            }
                            if (allOut) topRowsOut += 1 else topDone = true
                            sy += 1
                            topRowsScanned += 1
                            if (topRowsScanned > rect.height) topDone = true
                        }
                        // Per-direction left/right: leftmost contiguous columns where the entire
                        // column is out of region, as fraction of width. (Likewise on the right.)
                        var leftColsOut = 0
                        var lx = rect.x
                        var leftDone = false
                        while (lx < rect.x + rect.width && !leftDone) {
                            var allOut = true
                            var ly = rect.y
                            while (ly < rect.y + rect.height && allOut) {
                                val cx = lx.min(pw - 1).max(0)
                                val cy = ly.min(ph - 1).max(0)
                                if (place(cx)(cy) == unitRegionColor) allOut = false
                                ly += 4
                            }
                            if (allOut) leftColsOut += 1 else leftDone = true
                            lx += 1
                        }
                        var rightColsOut = 0
                        var rx2 = rect.x + rect.width - 1
                        var rightDone = false
                        while (rx2 >= rect.x && !rightDone) {
                            var allOut = true
                            var ry2 = rect.y
                            while (ry2 < rect.y + rect.height && allOut) {
                                val cx = rx2.min(pw - 1).max(0)
                                val cy = ry2.min(ph - 1).max(0)
                                if (place(cx)(cy) == unitRegionColor) allOut = false
                                ry2 += 4
                            }
                            if (allOut) rightColsOut += 1 else rightDone = true
                            rx2 -= 1
                        }
                        val bottomFrac = if (bottomTotal > 0) bottomBad.toDouble / bottomTotal else 0.0
                        val topFrac = topRowsOut.toDouble / rect.height.max(1)
                        val leftFrac = leftColsOut.toDouble / rect.width.max(1)
                        val rightFrac = rightColsOut.toDouble / rect.width.max(1)
                        (bottomFrac, topFrac, leftFrac, rightFrac)
                    }

                    def passesCaps(spill : (Double, Double, Double, Double),
                                   bottomCap : Double, topCap : Double,
                                   leftCap : Double, rightCap : Double) : Boolean = {
                        val (b, t, l, r) = spill
                        b <= bottomCap && t <= topCap && l <= leftCap && r <= rightCap
                    }

                    // [2026-05-23] Library tomes that are still on the map (not held
                    // by a faction) are visual fixtures the same way gates are. Without
                    // adding them to the overlap calculation, units (faction or neutral
                    // including iGOOs like Father Dagon) would happily place over the
                    // tome image. Treat tome overlap with the same 25× weight + 500
                    // flat-penalty cliff as gates so candidates that touch a tome are
                    // strongly avoided.
                    val onMapTomes : $[(Int, Int, Int, Int)] = if (board.isLibraryMap)
                        TomePlacement.positions.collect {
                            case (tome, tcx, tcy, tw, th)
                                if tome.region == r && !game.tomeHolders.getOrElse(tome, None).isDefined =>
                                (tcx - tw/2, tcy - th/2, tw, th)
                        }
                    else $

                    // Soft glyph-avoidance: discourage placing units directly atop the
                    // starting-glyph rendered in this same region (FB/TS/AN/OW dynamic
                    // glyphs are already in `fixed` as StartingGlyph items by now). Soft
                    // penalty (~5 max) loses to the 500-cliff gate/tome constraints, so
                    // when a region is full units still land on the glyph; only breaks
                    // ties between otherwise-equal candidates.
                    val glyphAvoidRadius = 60.0 * board.unitScale
                    val regionGlyphCenters = fixed.collect {
                        case gi if gi.region == r && gi.unit == StartingGlyph =>
                            (gi.x.toDouble, gi.y.toDouble)
                    }
                    def overlapOf(dd : DrawItem) : Double = (draws ++ fixed ++ sticking).map { oo =>
                        val d = dd.rect
                        val o = oo.rect
                        if (d == null || o == null) 0.0
                        else {
                            val w = min(o.x + o.width, d.x + d.width) - max(o.x, d.x)
                            val h = min(o.y + o.height, d.y + d.height) - max(o.y, d.y)
                            val s = (w > 0 && h > 0).?(w * h).|(0)
                            val base = s * (1.0 / (o.width * o.height) + 1.0 / (d.width * d.height))
                            val isGate = oo.unit == Gate || oo.unit == ChaosGate
                            val gateWeight = if (isGate) 25.0 else 1.0
                            // Hard cliff: ANY non-zero overlap with the gate adds a 500.0
                            // flat penalty. Typical unit-on-unit overlap scores are < 5, so any
                            // candidate that touches the gate is dwarfed by any candidate that
                            // doesn't. The 25× weight remains as a tie-breaker WITHIN the
                            // gate-overlapping set when no clear position exists.
                            val gateFlatPenalty = if (isGate && s > 0) 500.0 else 0.0
                            base * gateWeight + gateFlatPenalty
                        }
                    }.sum + onMapTomes.map { case (ox, oy, ow, oh) =>
                        val d = dd.rect
                        if (d == null) 0.0
                        else {
                            val w = min(ox + ow, d.x + d.width) - max(ox, d.x)
                            val h = min(oy + oh, d.y + d.height) - max(oy, d.y)
                            val s = (w > 0 && h > 0).?(w * h).|(0)
                            val base = s * (1.0 / (ow * oh) + 1.0 / (d.width * d.height))
                            val tomeFlatPenalty = if (s > 0) 500.0 else 0.0
                            base * 25.0 + tomeFlatPenalty
                        }
                    }.sum + regionGlyphCenters.map { case (gxC, gyC) =>
                        val d = dd.rect
                        if (d == null) 0.0
                        else {
                            val dcx = d.x + d.width / 2.0
                            val dcy = d.y + d.height / 2.0
                            val dist = math.hypot(dcx - gxC, dcy - gyC)
                            if (dist < glyphAvoidRadius) 5.0 * (1.0 - dist / glyphAvoidRadius)
                            else 0.0
                        }
                    }.sum

                    // Conditional enhanced candidate generation: 200 random samples instead of 40,
                    // applied only when a large unit (>= 80% of Ghatanothoa's proto max-dim) is
                    // being placed in a region whose bbox area is no more than 10% larger than
                    // Black Hall's. Black Hall is the canonical "cramped" region where the gate
                    // pile-on happens; any region that size or smaller gets the extra search budget.
                    // Outside this case, 40 candidates is plenty and faster.
                    val ghatoLikeThreshold = (176 * 0.80).toInt   // 80% of Ghatanothoa proto maxDim (96x176)
                    val blackHallAreaOpt = board.regions.find(_.name == "Black Hall").flatMap(regionArea.get)
                    val tightRegionThreshold = blackHallAreaOpt.map(a => (a * 1.10).toInt)

                    free.sortBy(d => -rank(d)).foreach { d =>
                        val protoR = d.proto
                        val isLargeUnit = d.unit.utype != Cultist && protoR != null &&
                                          math.max(protoR.width, protoR.height) >= ghatoLikeThreshold
                        val isTightRegion = tightRegionThreshold.exists(t => regionArea.get(r).exists(_ <= t))
                        val candidateCount = if (board.isLibraryMap && isLargeUnit && isTightRegion) 200 else 40

                        // L/T-partitioned library regions get balanced placement (see
                        // Library at Celaeno mirror); other regions use the standard find().
                        val partitioned = board.isLibraryMap && LtPartitions.contains((board.id, r.name))
                        val candidates = Array.tabulate(candidateCount)(n =>
                            if (partitioned) findWeighted(px, py, r) else find(px, py)
                        ).sortBy { case (x, y) => ((x - px).abs * 5 + (y - py).abs) }
                            .map { case (x, y) => DrawItem(d.region, d.faction, d.unit, d.health, d.tags, x, y) }

                        // Library map: filter via perspective caps, primary then fallback.
                        // Earth map: skip filter, base overlap-only behavior.
                        val chosen : DrawItem = if (board.isLibraryMap && place.nonEmpty) {
                            val withSpill = candidates.toSeq.map(c => (c, directionalSpill(c.rect)))
                            val primary = withSpill.filter { case (_, s) => passesCaps(s, 0.0, 0.30, 0.10, 0.10) }
                            val pool = if (primary.nonEmpty) primary
                                       else withSpill.filter { case (_, s) => passesCaps(s, 0.0, 0.60, 0.30, 0.30) }
                            if (pool.nonEmpty) pool.minBy { case (c, _) => overlapOf(c) }._1
                            else candidates.minBy(overlapOf)
                        } else candidates.minBy(overlapOf)

                        sticking +:= chosen
                    }

                    draws ++= fixed
                    draws ++= sticking
                    oldPositions ++= sticking
                }

                oldGates = game.gates
                if (board.isLibraryMap)
                    oldTomeOnMap = TomePlacement.positions.map { case (tome, _, _, _, _) =>
                        tome -> !game.tomeHolders.getOrElse(tome, None).isDefined
                    }.toMap

                // DS starting region glyph (DS isn't in the DrawItem loop above because
                // it only shows when cultists are present, unlike FB/TS/AN/OW which show always).
                // Trigger per OG haunt-roll-fail PR #10 (b8617a8): game.starting.get(DS) +
                // DS.cultists.any filter. Location stays on the current findStaticGlyphPos engine.
                game.starting.get(DS).filter(_ => DS.cultists.any).foreach { r =>
                    val (gx, gy) = gateXY(r)
                    val size = (60 * board.unitScale).toInt
                    val (sx, sy) = if (!horizontal && !board.isLibraryMap) {
                        // Portrait/mobile Earth: gateXY returns V-rotated coords
                        // (mp.height - y, x). The placement bitmap is H-oriented,
                        // so un-rotate to original landscape coords, search there,
                        // then rotate result back. Pass canonical EarthRegionPalette
                        // color so findStaticGlyphPos doesn't sample at the wrong
                        // pixel (same Australia-bug fix as placeGlyph above).
                        val (ox, oy) = (gy, mp.height - gx)
                        val rc = EarthRegionPalette.get(board.id, r).getOrElse(-1)
                        val (fx, fy) = findStaticGlyphPos(ox, oy, rc)
                        (mp.height - fy, fx)
                    } else {
                        val rc = if (!board.isLibraryMap) EarthRegionPalette.get(board.id, r).getOrElse(-1) else -1
                        findStaticGlyphPos(gx, gy, rc)
                    }
                    g.drawImage(getAsset("ds-glyph"), sx - size / 2, sy - size / 2, size, size)
                }

                // Library at Celaeno: draw tomes on the map for unclaimed tomes
                if (board.isLibraryMap) {
                    val tomeImg = getAsset("library-tome")
                    TomePlacement.positions.foreach { case (tome, tcx, tcy, tw, th) =>
                        val held = game.tomeHolders.getOrElse(tome, None).isDefined
                        if (!held) {
                            val (sx, sy) = if (horizontal) {
                                if (tcy < 1792) (tcx + 1791, tcy) else (tcx, tcy - 1792)
                            } else (tcx, tcy)
                            g.drawImage(tomeImg, sx - tw/2, sy - th/2, tw, th)
                        }
                    }

                    // Draw Custodian and Librarian in starting circles when off-map
                    val has5L = board.regions.exists(_.name == "Scorched Chamber")
                    val (libX, libY) = if (has5L) LibraryUnitPlacement.librarianPos5L else LibraryUnitPlacement.librarianPos3L
                    val (cusX, cusY) = if (has5L) LibraryUnitPlacement.custodianPos5L else LibraryUnitPlacement.custodianPos3L

                    def toScreen(vx : Int, vy : Int) = if (horizontal) {
                        if (vy < 1792) (vx + 1791, vy) else (vx, vy - 1792)
                    } else (vx, vy)

                    val us = LibraryUnitPlacement.unitSize
                    if (game.librarianRegion.isEmpty) {
                        val (sx, sy) = toScreen(libX, libY)
                        g.drawImage(getAsset("librarian-icon"), sx - us/2, sy - us/2, us, us)
                    }
                    if (game.custodianRegion.isEmpty) {
                        val (sx, sy) = toScreen(cusX, cusY)
                        g.drawImage(getAsset("custodian-icon"), sx - us/2, sy - us/2, us, us)
                    }
                }

                draws.sortBy(d => d.y + (d.unit == Gate || d.unit == ChaosGate).?(-2000).|(0) + (d.unit == DesecrationToken || d.unit == WebToken).?(-1000).|(0))./(_.rect).foreach { d =>
                    g.globalAlpha = d.alpha
                    if (d.splitTint.any) {
                        val leftImg = d.tint./(t => getTintedAsset(d.key, t)).|(getAsset(d.key))
                        val rightImg = getTintedAsset(d.key, d.splitTint.get)
                        val halfW = d.width / 2
                        g.save()
                        g.beginPath()
                        g.rect(d.x, d.y, halfW, d.height)
                        g.clip()
                        g.drawImage(leftImg, d.x, d.y, d.width, d.height)
                        g.restore()
                        g.save()
                        g.beginPath()
                        g.rect(d.x + halfW, d.y, d.width - halfW, d.height)
                        g.clip()
                        g.drawImage(rightImg, d.x, d.y, d.width, d.height)
                        g.restore()
                    } else if (d.rotation != 0.0) {
                        g.save()
                        g.translate(d.x + d.width / 2.0, d.y + d.height / 2.0)
                        g.rotate(d.rotation * math.Pi / 180.0)
                        g.drawImage(d.tint./(t => getTintedAsset(d.key, t)).|(getAsset(d.key)), -d.width / 2, -d.height / 2, d.width, d.height)
                        g.restore()
                    } else
                        g.drawImage(d.tint./(t => getTintedAsset(d.key, t)).|(getAsset(d.key)), d.x, d.y, d.width, d.height)
                    g.globalAlpha = 1.0
                }


                draws.sortBy(d => d.y + (d.unit == Gate || d.unit == ChaosGate).?(-2000).|(0) + (d.unit == DesecrationToken || d.unit == WebToken).?(-1000).|(0)).foreach { d =>
                    if (d.icon.any)
                        g.drawImage(getAsset(d.icon.get.key), d.icon.get.x, d.icon.get.y)
                }

                // Library: draw hint card, tome info, and white Custodian/Librarian silhouettes in top-right corner
                if (board.isLibraryMap) {
                    val silSize = 100
                    val silX = mp.width - 120
                    val tomeInfoW = 80
                    val tomeInfoH = 112
                    val tomeInfoX = silX - tomeInfoW - 10
                    val hintW = 100
                    val hintH = 140
                    val hintX = tomeInfoX - hintW - 10
                    g.globalAlpha = 0.85
                    g.drawImage(getAsset("library-hint-card"), hintX, 10, hintW, hintH)
                    g.drawImage(getAsset("library-tome-card"), tomeInfoX, 10, tomeInfoW, tomeInfoH)
                    g.globalAlpha = 0.8
                    g.drawImage(getAsset("custodian-silhouette"), silX, 10, silSize, silSize)
                    g.drawImage(getAsset("librarian-silhouette"), silX, 120, silSize, silSize)
                    g.globalAlpha = 1.0
                }
            }

            // Library: check if click hits a tome or silhouette on the map
            def checkLibraryClick(e : dom.MouseEvent, mapElem : html.Element) : Boolean = {
                if (!board.isLibraryMap) return false
                implicit val g = displayGame
                val canvas = map.canvas
                val rect = canvas.getBoundingClientRect()
                val clickX = e.clientX - rect.left
                val clickY = e.clientY - rect.top
                val scaleX = canvas.width.toDouble / rect.width
                val scaleY = canvas.height.toDouble / rect.height
                val cx = (clickX * scaleX).toInt
                val cy = (clickY * scaleY).toInt

                val mp = if (horizontal) getAsset(board.id + "-h") else getAsset(board.id)
                val dw = 12
                val dh = 12
                val fitW = (dw + mp.width + dw) * canvas.height < canvas.width * (dh + mp.height + dh)
                val imgScale = if (fitW) 1.0 * canvas.height / (dh + mp.height + dh) else 1.0 * canvas.width / (dw + mp.width + dw)
                val offsetX = if (fitW) (canvas.width - (dw + mp.width + dw) * imgScale) / 2.0 else 0.0
                val offsetY = if (fitW) 0.0 else (canvas.height - (dh + mp.height + dh) * imgScale) / 2.0

                def imgToCanvas(ix : Int, iy : Int) = ((offsetX + (dw + ix) * imgScale).toInt, (offsetY + (dh + iy) * imgScale).toInt)

                // Check tomes
                TomePlacement.positions.foreach { case (tome, tcx, tcy, tw, th) =>
                    val held = g.tomeHolders.getOrElse(tome, None).isDefined
                    if (!held) {
                        val (sx, sy) = if (horizontal) {
                            if (tcy < 1792) (tcx + 1791, tcy) else (tcx, tcy - 1792)
                        } else (tcx, tcy)
                        val (ccx, ccy) = imgToCanvas(sx, sy)
                        val hw = (tw * imgScale / 2).toInt
                        val hh = (th * imgScale / 2).toInt
                        if (cx >= ccx - hw && cx <= ccx + hw && cy >= ccy - hh && cy <= ccy + hh) {
                            Overlays.onExternalClick(tome.name, true)
                            return true
                        }
                    }
                }

                // Check tome info card and hint card (to the left of silhouettes)
                val silSize = (100 * imgScale).toInt
                val tomeInfoW = (80 * imgScale).toInt
                val tomeInfoH = (112 * imgScale).toInt
                val hintW = (100 * imgScale).toInt
                val hintH = (140 * imgScale).toInt
                val (silX, silY1) = imgToCanvas(mp.width - 120, 10)
                val (tomeInfoCX, tomeInfoCY) = imgToCanvas(mp.width - 120 - 80 - 10, 10)
                val (hintCX, hintCY) = imgToCanvas(mp.width - 120 - 80 - 10 - 100 - 10, 10)
                if (cx >= tomeInfoCX && cx <= tomeInfoCX + tomeInfoW && cy >= tomeInfoCY && cy <= tomeInfoCY + tomeInfoH) {
                    Overlays.onExternalClick("LibraryTomeInfo", true)
                    return true
                }
                if (cx >= hintCX && cx <= hintCX + hintW && cy >= hintCY && cy <= hintCY + hintH) {
                    Overlays.onExternalClick("LibraryHintCard", true)
                    return true
                }

                // Check custodian/librarian silhouettes (top-right corner of map)
                val (_, silY2) = imgToCanvas(mp.width - 120, 120)
                if (cx >= silX && cx <= silX + silSize && cy >= silY1 && cy <= silY1 + silSize) {
                    Overlays.onExternalClick("Custodian", true)
                    return true
                }
                if (cx >= silX && cx <= silX + silSize && cy >= silY2 && cy <= silY2 + silSize) {
                    Overlays.onExternalClick("Librarian", true)
                    return true
                }

                false
            }

            mapSmall.onclick = (e) => {
                if (!checkLibraryClick(e, mapSmall)) {
                    hide(mapSmall.parentElement.parentElement)
                    show(mapBig.parentElement.parentElement)

                    map = mapBitmapBig
                    invalidateMapSize()
                    drawMap(displayGame)
                }
            }

            mapBig.onclick = (e) => {
                if (!checkLibraryClick(e, mapBig)) {
                    hide(mapBig.parentElement.parentElement)
                    show(mapSmall.parentElement.parentElement)

                    map = mapBitmapSmall
                    invalidateMapSize()
                    drawMap(displayGame)
                }
            }

            drawMap(displayGame)

            val statusBitmaps = statuses.take(seating.num)./(s => new CachedBitmap(s))

            def factionStatus(f : Faction, b : CachedBitmap)(implicit game : Game) {
                if (!game.setup.has(f))
                    return
                // Debug logging removed — was firing on every UI cycle for every faction, causing severe slowdown

                def div(styles : String*)(content : String) = if (styles.none) "<div>" + content + "</div>" else "<div class=\"" + styles.mkString(" ") + "\">" + content + "</div>"
                def r(content : String) = div("right")(content)

                val current = game.factions.starting.has(f)

                // Library at Celaeno: silence token icon next to faction name
                val stCount = if (board.isLibraryMap) game.silenceTokens.getOrElse(f, 0) else 0
                val silenceTokenIcon = if (stCount > 0)
                    s""" <span onclick='event.stopPropagation(); onExternalClick("SilenceToken", true)' onpointerover='event.stopPropagation(); onExternalOver("SilenceToken", true)' onpointerout='event.stopPropagation(); onExternalOut("SilenceToken", true)' style='cursor:pointer'><img src='${Overlays.imageSource("silence-token")}' style='height:1.2em; vertical-align:middle;'/></span>"""
                else ""
                val name = div("name")("" + f + silenceTokenIcon)
                val nameS = div("name")(f.short.styled(f) + silenceTokenIcon)
                // Tombstalker (TS): append Death's Head count to faction status panel
                val dhStr = (f == TS).?(" | " + (game.deathsHead.toString + " Death's Head").styled(TS)).|("")
                // Firstborn (FB): Round 8 Bug 75 — append Infernal Pact discount count
                // to faction status panel, mirroring the TS Death's Head display. Shows
                // a grey pipe "|" separator followed by "N IP Disc" in FB color. Only
                // renders when discount > 0 (i.e. during an active IP session).
                val fbIPDiscStr = (f == FB && game.fbInfernalPactDiscount > 0).?(" | " + (game.fbInfernalPactDiscount.toString + " IP Disc").styled(FB)).|("")
                val fbIPDiscSStr = (f == FB && game.fbInfernalPactDiscount > 0).?(" " + (game.fbInfernalPactDiscount.toString + "IP").styled(FB)).|("")
                val power = div()(f.hibernating.?(("" + f.power + " Power").styled("hibernate")).|((f.power > 0).?(f.power.power).|("0 Power")) + dhStr + fbIPDiscStr)
                val powerS = div()(f.hibernating.?(("" + f.power + "P").styled("hibernate")).|((f.power > 0).?(("" + f.power + "P").styled("power")).|("0P")) + (f == TS).?(" " + (game.deathsHead.toString + " DH").styled(TS)).|("") + fbIPDiscSStr)
                // Firstborn (FB): read Infernal Pact discount and stored Augury kills for the faction panel display
                val fbIPDiscount = if (f == FB) game.fbInfernalPactDiscount else 0
                val fbAugury = if (f == FB) game.fbAuguryKills else 0
                // Tombstalker (TS) Cursed Tomes: display face-down tome count badge and clickable overlay for each faction
                val faceDownTomes = game.cursedTomesOwned.get(f).|(Nil).count { case (_, fd) => fd }
                val totalTomes = game.cursedTomesOwned.get(f).|(Nil).num
                val fStyle = f.style
                val tomeImgSrc = Overlays.imageSource("ts-cursed-tome")
                // [2026-05-23] Per user: face-down tome count is centered ON the
                // tome image (was: count text to the LEFT of the image, with a
                // small total-count badge in the top-right corner). Both old
                // numbers are replaced by a single centered face-down count.
                val tomeBadge = s"""<span style="position:absolute;top:50%;left:50%;transform:translate(-50%,-50%);font-size:90%;font-weight:bold;line-height:1;text-shadow:0 0 3px black,0 0 3px black,0 0 3px black;" class="ts">$faceDownTomes</span>"""
                val tomeImgSpan = s"""<span style="position:relative;display:inline-block;vertical-align:middle;"><img src="$tomeImgSrc" style="height:1.4em;display:block;"/>$tomeBadge</span>"""
                val tomeStr = (totalTomes > 0).?(" " + "- ".styled(TS) + s"""<span onclick='event.stopPropagation(); onExternalClick("cursed-tomes", "$fStyle")' onpointerover='event.stopPropagation(); onExternalOver("cursed-tomes", "$fStyle")' onpointerout='event.stopPropagation(); onExternalOut("cursed-tomes", "$fStyle")' style='cursor:pointer'>""" + tomeImgSpan + "</span>").|("")
                val tomeSStr = (totalTomes > 0).?(" " + "-".styled(TS) + faceDownTomes.toString.styled(TS) + "T".styled(TS)).|("")
                // Tombstalker (TS): show next available tome from the stack (tsTomesOnCard = # already given away)
                val tsNextTome = game.tsTomesOnCard + 1
                val tsStack = (f == TS && game.tsTomesOnCard < 11).?(
                    s"""<span onclick='event.stopPropagation(); onExternalClick("cursed-tomes", "$fStyle")' onpointerover='event.stopPropagation(); onExternalOver("cursed-tomes", "$fStyle")' onpointerout='event.stopPropagation(); onExternalOut("cursed-tomes", "$fStyle")' style='cursor:pointer; float:right; margin-left:0.5em;'>${tomeNumToRoman(tsNextTome).styled(TS)}<img src='${Overlays.imageSource("ts-cursed-tome")}' style='height:1.2em; vertical-align:middle;'/></span>"""
                ).|("")

                // Library at Celaeno: show held Library Tomes on faction card
                // Face-up: use specific tome image. Face-down (flipped after use): use generic back.
                val tomeBackSrc = if (board.isLibraryMap) Overlays.imageSource("library-tome-card") else ""
                def tomeAssetId(t : LibraryTome) = t match {
                    case TomeBarrier => "tome-barrier"
                    case TomeGuardian => "tome-guardian"
                    case TomeLarvae => "tome-larvae"
                    case TomeYr => "tome-yr"
                }
                val libraryTomeStr = if (board.isLibraryMap) {
                    val heldTomes = $(TomeBarrier, TomeGuardian, TomeLarvae, TomeYr).%(t =>
                        game.tomeHolders.getOrElse(t, None).has(f))
                    heldTomes./{ t =>
                        val overdue = game.tomeOverdue.getOrElse(t, false)
                        val faceUp = game.tomeFaceUp.getOrElse(t, true)
                        val tn = t.name.replace("\\", "\\\\").replace("'", "&#39")
                        val overdueMarker = overdue.??("<span style='position:absolute;top:50%;left:50%;transform:translate(-50%,-50%);font-weight:bold;font-size:1em;color:red;text-shadow:0 0 2px black;'>O</span>")
                        // Barrier never flips, always face-up. Others: face-up image when up, generic back when down.
                        val imgSrc = if (faceUp || t == TomeBarrier) Overlays.imageSource(tomeAssetId(t)) else tomeBackSrc
                        s""" <span onclick='event.stopPropagation(); onExternalClick("${tn}", ${faceUp})' onpointerover='event.stopPropagation(); onExternalOver("${tn}", ${faceUp})' onpointerout='event.stopPropagation(); onExternalOut("${tn}", ${faceUp})' style='cursor:pointer;position:relative;display:inline-block;vertical-align:middle;'><img src='${imgSrc}' style='height:1.4em;display:block;'/>${overdueMarker}</span>"""
                    }.mkString("")
                } else ""

                val doom = div()(tsStack + ("" + f.doom + " Doom").styled("doom") + f.es.any.?(" + " + (f.es.num == 1).?("ES").|("" + f.es.num + " ES").styled("es")).|("") + libraryTomeStr + tomeStr)
                val doomL = div()(tsStack + ("" + f.doom + " Doom").styled("doom") + f.es.any.?(" + " + (f.es.num == 1).?("Elder Sign").|("" + f.es.num + " Elder Signs").styled("es")).|("") + libraryTomeStr + tomeStr)
                val doomS = div()(tsStack + ("" + f.doom + "D").styled("doom") + f.es.any.?("+" + ("" + f.es.num + "ES").styled("es")).|("") + libraryTomeStr + tomeStr)

                val sb = f.spellbooks./{ sb =>
                    // Firstborn (FB): append augury skull image and stored kill count next to the Augury spellbook entry
                    // Bug fix Round 4: drop the `fbAugury > 0` guard so the skull (and "0") shows
                    // as soon as FB acquires the Augury spellbook, not only after the first kill is stored.
                    // Counter displays should appear at the moment the source ability is acquired.
                    // Firstborn (FB): augury skull and kill count shown to the LEFT of the spellbook text
                    val auguryPrefix =
                        if (f == FB && sb == Augury)
                            "<img src='" + getAsset("fb-augury-skull").src + "' style='height: 1em; vertical-align: middle;'/> " + fbAugury.toString.styled(FB) + " "
                        else ""
                    // Moonbeast: check if this spellbook is blocked by a Moonbeast
                    val moonbeastOnThis = displayGame.moonbeastOnSpellbook.values.find(t => t._1 == f && t._2 == sb)
                    val moonbeastImg = moonbeastOnThis./(t => {
                        val mbOwner = displayGame.moonbeastOnSpellbook.find(_._2 == t)./(e => displayGame.unit(e._1).faction).|(f)
                        // Use faction-tinted image (same tint as map units)
                        val tint = DrawItem(null, mbOwner, MoonbeastUnit, Alive, $, 0, 0).tint
                        val tintedSrc = getTintedAsset("n-moonbeast", tint).toDataURL("image/png")
                        s"<img src='${tintedSrc}' style='height:1.4em;vertical-align:middle;opacity:0.9;margin-right:0.3em;' />"
                    }).|("")
                    val full = auguryPrefix + moonbeastImg + sb.elem
                    val s = sb.name.replace("\\", "\\\\").replace("'", "&#39") // "
                    // Pass option state for conditional overlay text
                    val sbExtra = (f, sb) match {
                        case (SL, EnergyNexus) if game.options.has(SleeperEnergyNexusPreBattle) => ", true"
                        case (OW, DreadCurse) if game.options.has(OpenerYogCurseDie) => ", true"
                        case _ => ""
                    }
                    val d = s"""<div class='spellbook'
                        onclick='event.stopPropagation(); onExternalClick("${f.short}", "${s}"${sbExtra})'
                        onpointerover='event.stopPropagation(); onExternalOver("${f.short}", "${s}")'
                        onpointerout='event.stopPropagation(); onExternalOut("${f.short}", "${s}")'
                        >${full}</div>"""
                    f.can(sb).?(d).|(d.styled("used"))
                }.mkString("") +
                (1.to(6 - f.spellbooks.num - f.unfulfilled.num)./(x => "?".styled(f)))./(div("spellbook", f.style + "-background")).mkString("") +
                f.unfulfilled./{ r =>
                    // Check if any Moonbeast is on an SBR that maps to this requirement's spellbook
                    val reqSpellbooks = f.library.%(sb => !f.spellbooks.has(sb))
                    val moonbeastOnReq = reqSpellbooks.exists(sb => displayGame.moonbeastOnSpellbook.values.exists(t => t._1 == f && t._2 == sb))
                    val mbImg = if (moonbeastOnReq) {
                        // Find the moonbeast owner for tinting
                        val mbEntry = displayGame.moonbeastOnSpellbook.find { case (_, (target, sb)) => target == f && reqSpellbooks.contains(sb) }
                        val mbOwner = mbEntry./(e => displayGame.unit(e._1).faction).|(f)
                        val tint = DrawItem(null, mbOwner, MoonbeastUnit, Alive, $, 0, 0).tint
                        val tintedSrc = getTintedAsset("n-moonbeast", tint).toDataURL("image/png")
                        s"<img src='${tintedSrc}' style='height:1.4em;vertical-align:middle;opacity:0.9;margin-right:0.3em;' />"
                    } else ""
                    // Sleeper easier SBR: show alternate requirement text on faction card
                    val displayText = if (f == SL && game.options.has(SleeperEasierSBR)) r match {
                        case Pay3SomeoneGains3 => "Pay your last power"
                        case Pay3EverybodyGains1 => "Pay your last 2 power"
                        case Pay3EverybodyLoses1 => "Pay your last 3 power"
                        case _ => r.text
                    }
                    // OW variant (OpenerCheapMutants, MNU+): UnitsAtEnemyGates SBR threshold
                    // raises from 2 to 3. Defer to Requirement.displayText which subclasses
                    // override (see FactionOW.scala UnitsAtEnemyGates.displayText). Send the
                    // dynamic text through the overlay click handler too so the popup matches.
                    else if (f == OW) r.displayText(game)
                    else r.text
                    val s = displayText.replace("\\", "\\\\").replace("'", "&#39;") // "
                    val d = s"""<div class='spellbook'
                        onclick='event.stopPropagation(); onExternalClick("${f.short}", "${s}")'
                        onpointerover='event.stopPropagation(); onExternalOver("${f.short}", "${s}")'
                        onpointerout='event.stopPropagation(); onExternalOut("${f.short}", "${s}")'
                        >${mbImg}${displayText}</div>"""
                    d
                }.mkString("")

                val iconSpacing = 30
                val baseRightOffset = 3

                val lcis = f.loyaltyCards.reverse.indexed./ { (lc, i) =>
                    val spellbook = lc match {
                        case ByatisCard => |(f.upgrades.has(GodOfForgetfulness))
                        case AbhothCard => |(f.upgrades.has(TheBrood))
                        case DaolothCard => |(f.upgrades.has(Interdimensional))
                        case NyogthaCard => |(f.upgrades.has(NightmareWeb))
                        case TulzschaCard => |(f.upgrades.has(CeremonyOfAnnihilation))
                        case YgolonacCard => |(f.upgrades.has(TheRevelations))
                        case YigCard => |(f.upgrades.has(MessengerOfYig))
                        case MotherHydraCard => |(f.upgrades.has(TheZygote))
                        case FatherDagonCard => |(f.upgrades.has(TheInnsmouthLook))
                        case CthughaCard => |(f.upgrades.has(Firestorm))
                        case GhatanotoaIGOOCard => |(f.upgrades.has(ExecrationOfMu))
                        case BloatedWomanCard => |(f.upgrades.has(DisasterLooms))
                        case AtlachNachaCard => |(f.upgrades.has(CosmicWeb))
                        case AzathothIGOOCard => |(f.upgrades.has(NuclearChaos))
                        case BokrugCard => |(f.upgrades.has(DoomThatCameToSarnath))
                        case GlaakiIGOOCard => |(f.upgrades.has(GlaakiGreenDecay))
                        case _ => None
                    }

                    // Round 8 Bug 40: check if the IGOO spellbook is facedown via Infernal Pact.
                    // Facedown spellbooks are in f.oncePerGame. Only relevant for FB.
                    val isFacedown = spellbook.has(true) && (lc match {
                        case ByatisCard => f.oncePerGame.has(GodOfForgetfulness)
                        case AbhothCard => f.oncePerGame.has(TheBrood)
                        case DaolothCard => f.oncePerGame.has(Interdimensional)
                        case NyogthaCard => f.oncePerGame.has(NightmareWeb)
                        case TulzschaCard => f.oncePerGame.has(CeremonyOfAnnihilation)
                        case YgolonacCard => f.oncePerGame.has(TheRevelations)
                        case YigCard => f.oncePerGame.has(MessengerOfYig)
                        case MotherHydraCard => f.oncePerGame.has(TheZygote)
                        case FatherDagonCard => f.oncePerGame.has(TheInnsmouthLook)
                        case CthughaCard => f.oncePerGame.has(Firestorm)
                        case GhatanotoaIGOOCard => f.oncePerGame.has(ExecrationOfMu)
                        case BloatedWomanCard => f.oncePerGame.has(DisasterLooms)
                        case AtlachNachaCard => f.oncePerGame.has(CosmicWeb)
                        case AzathothIGOOCard => f.oncePerGame.has(NuclearChaos)
                        case BokrugCard => f.oncePerGame.has(DoomThatCameToSarnath)
                        case GlaakiIGOOCard => f.oncePerGame.has(GlaakiGreenDecay)
                        case _ => false
                    })

                    // Extra overlay params for specific iGOOs
                    val extraParams = if (lc == AzathothIGOOCard) ", " + game.azathothGlyphPosition
                        else if (lc == BloatedWomanCard) {
                            val captured = factions./~(ff => ff.at(VelvetFanHold(f)))./(u => u.uclass.name + " (" + u.faction.short + ")")
                            ", " + captured.num + ", \"" + captured.mkString(", ").replace("\"", "") + "\""
                        }
                        else ""
                    val sb = spellbook @@ {
                        case Some(true) => ", true" + (if (isFacedown) ", true" else ", false") + extraParams
                        case Some(false) => ", false, false" + extraParams
                        case None => ""
                    }

                    val d = DrawItem(null, f, lc.icon, Alive, $, 0, 0)
                    val unitName = lc.name.replace("\\", "\\\\").replace("\"", "&quot;").replace("'", "&#39;")
                    val factionShort = f.short.replace("\"", "&quot;")

                    val right = baseRightOffset + i * iconSpacing

                    val img = s"""<img class='loyalty-card-icon'
                        src='${Overlays.imageSource("info:" + "n-" + lc.name.toLowerCase.replace("'", "").replace(" ", "-"))}'
                        style='right:${right/12.0}vh; right:${right/12.0}dvh;'
                        onclick='event.stopPropagation(); onExternalClick("${unitName}"${sb})'
                        onpointerover='event.stopPropagation(); onExternalOver("${unitName}"${sb})'
                        onpointerout='event.stopPropagation(); onExternalOut("${unitName}"${sb})' />"""

                    // Azathoth iGOO: overlay glyph position number on the silhouette icon
                    if (lc == AzathothIGOOCard && game.azathothGlyphPosition > 0)
                        img + s"""<span style='position:absolute;bottom:0.25vh;bottom:0.25dvh;right:${right/12.0}vh;right:${right/12.0}dvh;height:5vh;height:5dvh;width:5vh;width:5dvh;display:flex;align-items:center;justify-content:center;transform:scale(0.8);transform-origin:bottom right;font:bold 3.6vh "Bohemian Typewriter",monospace;color:#ff3333;text-shadow:-1px -1px 0 #000,1px -1px 0 #000,-1px 1px 0 #000,1px 1px 0 #000;pointer-events:none;'>${game.azathothGlyphPosition}</span>"""
                    // Atlach-Nacha iGOO: overlay web token count on the silhouette icon
                    else if (lc == AtlachNachaCard && game.webTokens.num > 0)
                        img + s"""<span style='position:absolute;bottom:0.25vh;bottom:0.25dvh;right:${right/12.0}vh;right:${right/12.0}dvh;height:5vh;height:5dvh;width:5vh;width:5dvh;display:flex;align-items:center;justify-content:center;transform:scale(0.8);transform-origin:bottom right;font:bold 3.6vh "Bohemian Typewriter",monospace;color:#ff3333;text-shadow:-1px -1px 0 #000,1px -1px 0 #000,-1px 1px 0 #000,1px 1px 0 #000;pointer-events:none;'>${game.webTokens.num}</span>"""
                    else
                        img
                }.mkString("")

                val h = 450
                val scale = b.node.clientHeight / h.toDouble
                val w = (b.node.clientWidth / scale).round.toInt

                val s =
                if (w > 580)
                    name + power + doomL
                else
                if (w > 420)
                    name + power + doom
                else
                if (w > 300)
                    nameS + powerS + doomS
                else
                    r(nameS) + r(powerS) + r(doomS)

                val fClickArgs = f match {
                    case OW => s""""${f.short}", ${game.options.has(OpenerCheapMutants)}, ${game.options.has(OpenerYogCurseDie)}"""
                    case SL => s""""${f.short}", ${game.options.has(SleeperEasierSBR)}, ${game.options.has(SleeperEnergyNexusPreBattle)}"""
                    case DS => s""""${f.short}", ${game.options.has(DSAlternateSpellbooks)}"""
                    case _ => s""""${f.short}""""
                }
                b.node.innerHTML = s"""<div class='full-height'
                    onclick='event.stopPropagation(); onExternalClick($fClickArgs)'
                    onpointerover='event.stopPropagation(); onExternalOver("${f.short}")'
                    onpointerout='event.stopPropagation(); onExternalOut("${f.short}")'
                    >${div("top")(s) + sb + lcis}</div>"""

                val bitmap = b.get(w, h)

                bitmap.canvas.style.pointerEvents = "none"
                bitmap.canvas.style.width = "" + b.node.clientWidth + "px"
                bitmap.canvas.style.height = "" + b.node.clientHeight + "px"

                val g = bitmap.context
                g.setTransform(1, 0, 0, 1, 0, 0)
                g.clearRect(0, 0, bitmap.width, bitmap.height)

                def dd(d : DrawRect) = {
                    if (d.splitTint.any) {
                        // Split color: left half = tint, right half = splitTint
                        val leftImg = d.tint./(t => getTintedAsset(d.key, t)).|(getAsset(d.key))
                        val rightImg = getTintedAsset(d.key, d.splitTint.get)
                        val halfW = d.width / 2
                        g.save()
                        g.beginPath()
                        g.rect(d.x, d.y, halfW, d.height)
                        g.clip()
                        g.drawImage(leftImg, d.x, d.y, d.width, d.height)
                        g.restore()
                        g.save()
                        g.beginPath()
                        g.rect(d.x + halfW, d.y, d.width - halfW, d.height)
                        g.clip()
                        g.drawImage(rightImg, d.x, d.y, d.width, d.height)
                        g.restore()
                    } else {
                        val img = d.tint./(t => getTintedAsset(d.key, t)).|(getAsset(d.key))
                        g.drawImage(img, d.x, d.y, d.width, d.height)
                    }
                }

                dd(DrawItem(null, f, FactionGlyph, Alive, $, 55, 55).rect.copy(tint = |(Processing(None, game.factions.starting.has(f).?("#444444"), None))))

                if (f == SL && game.gates.has(SL.slumber)) {
                    dd(DrawItem(null, f, Gate, Alive, $, w - 46, 56).rect)
                    val cultistOrHP = f.at(SL.slumber, Cultist) ++ f.at(SL.slumber, HighPriest)
                    if (cultistOrHP.any) {
                        val unit = cultistOrHP.head
                        dd(DrawItem(null, unit.faction, unit.uclass, Alive, $, w - 46, 56).rect)
                    }
                }

                var smx = 0
                game.setup.but(f).foreach { e =>
                    if (e.borrowed.has(f.abilities.head)) {
                        dd(DrawItem(null, e, SerpentMan, Alive, $, w - 46 + smx, 86).rect)
                        smx -= 20
                    }
                }

                if (f == DS && DS.unfulfilled.has(AwakenAvatarThesis).not) {
                    val track = DS.azathothTrack.toString
                    g.font = "bold 31px \"Bohemian Typewriter\", monospace"
                    g.textAlign = "center"
                    g.textBaseline = "middle"
                    g.lineWidth = 5.0
                    g.strokeStyle = "rgba(0,0,0,0.85)"
                    g.strokeText(track, 55, 28)
                    g.fillStyle = "white"
                    g.fillText(track, 55, 28)
                }


                val deep = f.at(GC.deep).any.?? {
                    var draws = $(DrawItem(null, f, Cthulhu, Alive, $, 64, h - 12 - 6))

                    val sortedDeep = f.at(GC.deep).filterNot(_.uclass == Cthulhu).sortBy(_.uclass @@ {
                        case Cthulhu =>      0
                        case Abhoth =>       1
                        case Daoloth =>      2
                        case Nyogtha =>      3
                        case Tulzscha =>     4
                        case Starspawn =>    4
                        case Ygolonac =>     5
                        case Shoggoth =>     6
                        case DeepOne =>      7
                        case Acolyte =>      8
                        case HighPriest =>   9
                        case Ghast =>       10
                        case Gug =>         11
                        case Shantak =>     12
                        case StarVampire => 13
                        case Voonith =>     14
                        case DimensionalShamblerUnit => 15
                        case Gnorri =>      16
                        case ServitorUnit => 17
                        case Filth =>       18
                        case AtlachNacha => 19
                        case Bokrug =>      20
                        case GlaakiIGOO =>  21
                        case _ =>           22
                    })

                    while (draws.num - 1 < sortedDeep.num) {
                        val last = draws.last
                        val next = sortedDeep(draws.num - 1).uclass
                        draws :+= ((last.unit, next) match {
                            case (Cthulhu, Abhoth) => DrawItem(null, f, Abhoth, Alive, $, 90 + last.x, 6 + last.y)

                            case (Cthulhu, Daoloth) => DrawItem(null, f, Daoloth, Alive, $, 92 + last.x, 6 + last.y)
                            case (Abhoth, Daoloth) => DrawItem(null, f, Daoloth, Alive, $, 88 + last.x, last.y)

                            case (Cthulhu, Nyogtha) => DrawItem(null, f, Nyogtha, Alive, $, 86 + last.x, 6 + last.y)
                            case (Abhoth, Nyogtha) => DrawItem(null, f, Nyogtha, Alive, $, 81 + last.x, last.y)
                            case (Daoloth, Nyogtha) => DrawItem(null, f, Nyogtha, Alive, $, 86 + last.x, last.y)
                            case (Nyogtha, Nyogtha) => DrawItem(null, f, Nyogtha, Alive, $, 80 + last.x, last.y)
                            case (Tulzscha, Nyogtha) => DrawItem(null, f, Nyogtha, Alive, $, 47 + last.x, last.y)
                            case (Cthulhu, Tulzscha) => DrawItem(null, f, Tulzscha, Alive, $, 63 + last.x, 6 + last.y)
                            case (Abhoth, Tulzscha) => DrawItem(null, f, Tulzscha, Alive, $, 58 + last.x, last.y)
                            case (Daoloth, Tulzscha) => DrawItem(null, f, Tulzscha, Alive, $, 63 + last.x, last.y)
                            case (Nyogtha, Tulzscha) => DrawItem(null, f, Tulzscha, Alive, $, 57 + last.x, last.y)
                            case (Starspawn, Tulzscha) => DrawItem(null, f, Tulzscha, Alive, $, 62 + last.x, last.y)
                            case (Tulzscha, Tulzscha) => DrawItem(null, f, Tulzscha, Alive, $, 32 + last.x, last.y)
                            case (Tulzscha, Starspawn) => DrawItem(null, f, Starspawn, Alive, $, 52 + last.x, last.y)
                            case (Tulzscha, Shoggoth) => DrawItem(null, f, Shoggoth, Alive, $, 48 + last.x, last.y)
                            case (Tulzscha, DeepOne) => DrawItem(null, f, DeepOne, Alive, $, 35 + last.x, last.y)
                            case (Tulzscha, Acolyte) => DrawItem(null, f, Acolyte, Alive, $, 37 + last.x, last.y)
                            case (Tulzscha, HighPriest) => DrawItem(null, f, HighPriest, Alive, $, 52 + last.x, last.y)
                            case (Tulzscha, Ghast) => DrawItem(null, f, Ghast, Alive, $, 34 + last.x, last.y)
                            case (Tulzscha, Gug) => DrawItem(null, f, Gug, Alive, $, 53 + last.x, last.y)
                            case (Tulzscha, Shantak) => DrawItem(null, f, Shantak, Alive, $, 56 + last.x, last.y)
                            case (Tulzscha, StarVampire) => DrawItem(null, f, StarVampire, Alive, $, 52 + last.x, last.y)
                            case (Tulzscha, Voonith) => DrawItem(null, f, Voonith, Alive, $, 33 + last.x, last.y)
                            case (Tulzscha, DimensionalShamblerUnit) => DrawItem(null, f, DimensionalShamblerUnit, Alive, $, 52 + last.x, last.y)
                            case (Tulzscha, Ygolonac) => DrawItem(null, f, Ygolonac, Alive, $, 55 + last.x, last.y)
                            case (Tulzscha, Gnorri) => DrawItem(null, f, Gnorri, Alive, $, 43 + last.x, last.y)
                            case (Tulzscha, Filth) => DrawItem(null, f, Filth, Alive, $, 37 + last.x, last.y - 15)

                            case (Cthulhu, Starspawn) => DrawItem(null, f, Starspawn, Alive, $, 75 + last.x, 6 + last.y)
                            case (Abhoth, Starspawn) => DrawItem(null, f, Starspawn, Alive, $, 70 + last.x, last.y)
                            case (Daoloth, Starspawn) => DrawItem(null, f, Starspawn, Alive, $, 82 + last.x, last.y)
                            case (Nyogtha, Starspawn) => DrawItem(null, f, Starspawn, Alive, $, 77 + last.x, last.y)
                            case (Starspawn, Starspawn) => DrawItem(null, f, Starspawn, Alive, $, 70 + last.x, last.y)
                            case (Starspawn, Ygolonac) => DrawItem(null, f, Ygolonac, Alive, $, 62 + last.x, last.y)

                            case (Cthulhu, Ygolonac) => DrawItem(null, f, Ygolonac, Alive, $, 75 + last.x, 6 + last.y)
                            case (Abhoth, Ygolonac) => DrawItem(null, f, Ygolonac, Alive, $, 63 + last.x, last.y)
                            case (Daoloth, Ygolonac) => DrawItem(null, f, Ygolonac, Alive, $, 75 + last.x, last.y)
                            case (Nyogtha, Ygolonac) => DrawItem(null, f, Ygolonac, Alive, $, 67 + last.x, last.y)
                            case (Ygolonac, Ygolonac) => DrawItem(null, f, Ygolonac, Alive, $, 65 + last.x, last.y)
                            case (Ygolonac, Shoggoth) => DrawItem(null, f, Shoggoth, Alive, $, 63 + last.x, last.y)
                            case (Ygolonac, DeepOne) => DrawItem(null, f, DeepOne, Alive, $, 52 + last.x, last.y)
                            case (Ygolonac, Acolyte) => DrawItem(null, f, Acolyte, Alive, $, 50 + last.x, last.y)
                            case (Ygolonac, HighPriest) => DrawItem(null, f, HighPriest, Alive, $, 62 + last.x, last.y)
                            case (Ygolonac, Ghast) => DrawItem(null, f, Ghast, Alive, $, 52 + last.x, last.y)
                            case (Ygolonac, Gug) => DrawItem(null, f, Gug, Alive, $, 66 + last.x, last.y)
                            case (Ygolonac, Shantak) => DrawItem(null, f, Shantak, Alive, $, 62 + last.x, last.y)
                            case (Ygolonac, StarVampire) => DrawItem(null, f, StarVampire, Alive, $, 62 + last.x, last.y)
                            case (Ygolonac, Voonith) => DrawItem(null, f, Voonith, Alive, $, 43 + last.x, last.y)
                            case (Ygolonac, DimensionalShamblerUnit) => DrawItem(null, f, DimensionalShamblerUnit, Alive, $, 62 + last.x, last.y)
                            case (Ygolonac, Gnorri) => DrawItem(null, f, Gnorri, Alive, $, 55 + last.x, last.y)
                            case (Ygolonac, Filth) => DrawItem(null, f, Filth, Alive, $, 42 + last.x, last.y - 15)

                            case (Cthulhu, Shoggoth) => DrawItem(null, f, Shoggoth, Alive, $, 76 + last.x, 6 + last.y)
                            case (Abhoth, Shoggoth) => DrawItem(null, f, Shoggoth, Alive, $, 65 + last.x, last.y)
                            case (Daoloth, Shoggoth) => DrawItem(null, f, Shoggoth, Alive, $, 79 + last.x, last.y)
                            case (Nyogtha, Shoggoth) => DrawItem(null, f, Shoggoth, Alive, $, 73 + last.x, last.y)
                            case (Starspawn, Shoggoth) => DrawItem(null, f, Shoggoth, Alive, $, 66 + last.x, last.y)
                            case (Shoggoth, Shoggoth) => DrawItem(null, f, Shoggoth, Alive, $, 62 + last.x, last.y)

                            case (Cthulhu, DeepOne) => DrawItem(null, f, DeepOne, Alive, $, 64 + last.x, 6 + last.y)
                            case (Abhoth, DeepOne) => DrawItem(null, f, DeepOne, Alive, $, 60 + last.x, last.y)
                            case (Daoloth, DeepOne) => DrawItem(null, f, DeepOne, Alive, $, 74 + last.x, last.y)
                            case (Nyogtha, DeepOne) => DrawItem(null, f, DeepOne, Alive, $, 62 + last.x, last.y)
                            case (Starspawn, DeepOne) => DrawItem(null, f, DeepOne, Alive, $, 51 + last.x, last.y)
                            case (Shoggoth, DeepOne) => DrawItem(null, f, DeepOne, Alive, $, 48 + last.x, last.y)
                            case (DeepOne, DeepOne) if last.health == Alive => DrawItem(null, f, DeepOne, Pained, $, last.x, last.y - 31)
                            case (DeepOne, DeepOne) if last.health == Pained => DrawItem(null, f, DeepOne, Alive, $, 35 + last.x, last.y + 31)

                            case (Cthulhu, Acolyte) => DrawItem(null, f, Acolyte, Alive, $, 57 + last.x, 6 + last.y)
                            case (Abhoth, Acolyte) => DrawItem(null, f, Acolyte, Alive, $, 54 + last.x, last.y)
                            case (Daoloth, Acolyte) => DrawItem(null, f, Acolyte, Alive, $, 68 + last.x, last.y)
                            case (Nyogtha, Acolyte) => DrawItem(null, f, Acolyte, Alive, $, 60 + last.x, last.y)
                            case (Starspawn, Acolyte) => DrawItem(null, f, Acolyte, Alive, $, 52 + last.x, last.y)
                            case (Shoggoth, Acolyte) => DrawItem(null, f, Acolyte, Alive, $, 48 + last.x, last.y)
                            case (DeepOne, Acolyte) if last.health == Alive => DrawItem(null, f, Acolyte, Alive, $, 36 + last.x, last.y)
                            case (DeepOne, Acolyte) if last.health == Pained => DrawItem(null, f, Acolyte, Alive, $, 36 + last.x, last.y + 31)
                            case (Acolyte, Acolyte) => DrawItem(null, f, Acolyte, Alive, $, 35 + last.x, last.y)

                            case (Cthulhu, HighPriest) => DrawItem(null, f, HighPriest, Alive, $, 75 + last.x, 6 + last.y)
                            case (Abhoth, HighPriest) => DrawItem(null, f, HighPriest, Alive, $, 68 + last.x, last.y)
                            case (Daoloth, HighPriest) => DrawItem(null, f, HighPriest, Alive, $, 82 + last.x, last.y)
                            case (Nyogtha, HighPriest) => DrawItem(null, f, HighPriest, Alive, $, 77 + last.x, last.y)
                            case (Starspawn, HighPriest) => DrawItem(null, f, HighPriest, Alive, $, 70 + last.x, last.y)
                            case (Shoggoth, HighPriest) => DrawItem(null, f, HighPriest, Alive, $, 66 + last.x, last.y)
                            case (DeepOne, HighPriest) if last.health == Alive => DrawItem(null, f, HighPriest, Alive, $, 54 + last.x, last.y)
                            case (DeepOne, HighPriest) if last.health == Pained => DrawItem(null, f, HighPriest, Alive, $, 54 + last.x, last.y + 31)
                            case (Acolyte, HighPriest) => DrawItem(null, f, HighPriest, Alive, $, 53 + last.x, last.y)

                            case (Cthulhu, Ghast) => DrawItem(null, f, Ghast, Alive, $, 62 + last.x, 6 + last.y)
                            case (Abhoth, Ghast) => DrawItem(null, f, Ghast, Alive, $, 53 + last.x, last.y)
                            case (Daoloth, Ghast) => DrawItem(null, f, Ghast, Alive, $, 67 + last.x, last.y)
                            case (Nyogtha, Ghast) => DrawItem(null, f, Ghast, Alive, $, 61 + last.x, last.y)
                            case (Starspawn, Ghast) => DrawItem(null, f, Ghast, Alive, $, 54 + last.x, last.y)
                            case (Shoggoth, Ghast) => DrawItem(null, f, Ghast, Alive, $, 52 + last.x, last.y)
                            case (DeepOne, Ghast) if last.health == Alive => DrawItem(null, f, Ghast, Alive, $, 39 + last.x, last.y)
                            case (DeepOne, Ghast) if last.health == Pained => DrawItem(null, f, Ghast, Alive, $, 39 + last.x, last.y + 31)
                            case (Acolyte, Ghast) => DrawItem(null, f, Ghast, Alive, $, 37 + last.x, last.y)
                            case (HighPriest, Ghast) => DrawItem(null, f, Ghast, Alive, $, 52 + last.x, last.y)
                            case (Ghast, Ghast) => DrawItem(null, f, Ghast, Alive, $, 35 + last.x, last.y)

                            case (Cthulhu, Gug) => DrawItem(null, f, Gug, Alive, $, 78 + last.x, 6 + last.y)
                            case (Abhoth, Gug) => DrawItem(null, f, Gug, Alive, $, 70 + last.x, last.y)
                            case (Daoloth, Gug) => DrawItem(null, f, Gug, Alive, $, 87 + last.x, last.y)
                            case (Nyogtha, Gug) => DrawItem(null, f, Gug, Alive, $, 77 + last.x, last.y)
                            case (Starspawn, Gug) => DrawItem(null, f, Gug, Alive, $, 70 + last.x, last.y)
                            case (Shoggoth, Gug) => DrawItem(null, f, Gug, Alive, $, 66 + last.x, last.y)
                            case (DeepOne, Gug) if last.health == Alive => DrawItem(null, f, Gug, Alive, $, 56 + last.x, last.y)
                            case (DeepOne, Gug) if last.health == Pained => DrawItem(null, f, Gug, Alive, $, 56 + last.x, last.y + 31)
                            case (Acolyte, Gug) => DrawItem(null, f, Gug, Alive, $, 54 + last.x, last.y)
                            case (HighPriest, Gug) => DrawItem(null, f, Gug, Alive, $, 68 + last.x, last.y)
                            case (Ghast, Gug) => DrawItem(null, f, Gug, Alive, $, 55 + last.x, last.y)
                            case (Gug, Gug) => DrawItem(null, f, Gug, Alive, $, 72 + last.x, last.y)

                            case (Cthulhu, Shantak) => DrawItem(null, f, Shantak, Alive, $, 83 + last.x, 6 + last.y)
                            case (Abhoth, Shantak) => DrawItem(null, f, Shantak, Alive, $, 64 + last.x, last.y)
                            case (Daoloth, Shantak) => DrawItem(null, f, Shantak, Alive, $, 90 + last.x, last.y)
                            case (Nyogtha, Shantak) => DrawItem(null, f, Shantak, Alive, $, 70 + last.x, last.y)
                            case (Starspawn, Shantak) => DrawItem(null, f, Shantak, Alive, $, 66 + last.x, last.y)
                            case (Shoggoth, Shantak) => DrawItem(null, f, Shantak, Alive, $, 66 + last.x, last.y)
                            case (DeepOne, Shantak) if last.health == Alive => DrawItem(null, f, Shantak, Alive, $, 49 + last.x, last.y)
                            case (DeepOne, Shantak) if last.health == Pained => DrawItem(null, f, Shantak, Alive, $, 49 + last.x, last.y + 31)
                            case (Acolyte, Shantak) => DrawItem(null, f, Shantak, Alive, $, 50 + last.x, last.y)
                            case (HighPriest, Shantak) => DrawItem(null, f, Shantak, Alive, $, 61 + last.x, last.y)
                            case (Ghast, Shantak) => DrawItem(null, f, Shantak, Alive, $, 48 + last.x, last.y)
                            case (Gug, Shantak) => DrawItem(null, f, Shantak, Alive, $, 63 + last.x, last.y)
                            case (Shantak, Shantak) => DrawItem(null, f, Shantak, Alive, $, 74 + last.x, last.y)

                            case (Cthulhu, StarVampire) => DrawItem(null, f, StarVampire, Alive, $, 79 + last.x, 6 + last.y)
                            case (Abhoth, StarVampire) => DrawItem(null, f, StarVampire, Alive, $, 63 + last.x, last.y)
                            case (Daoloth, StarVampire) => DrawItem(null, f, StarVampire, Alive, $, 77 + last.x, last.y)
                            case (Nyogtha, StarVampire) => DrawItem(null, f, StarVampire, Alive, $, 69 + last.x, last.y)
                            case (Starspawn, StarVampire) => DrawItem(null, f, StarVampire, Alive, $, 61 + last.x, last.y)
                            case (Shoggoth, StarVampire) => DrawItem(null, f, StarVampire, Alive, $, 60 + last.x, last.y)
                            case (DeepOne, StarVampire) if last.health == Alive => DrawItem(null, f, StarVampire, Alive, $, 50 + last.x, last.y)
                            case (DeepOne, StarVampire) if last.health == Pained => DrawItem(null, f, StarVampire, Alive, $, 50 + last.x, last.y + 31)
                            case (Acolyte, StarVampire) => DrawItem(null, f, StarVampire, Alive, $, 52 + last.x, last.y)
                            case (HighPriest, StarVampire) => DrawItem(null, f, StarVampire, Alive, $, 59 + last.x, last.y)
                            case (Ghast, StarVampire) => DrawItem(null, f, StarVampire, Alive, $, 53 + last.x, last.y)
                            case (Gug, StarVampire) => DrawItem(null, f, StarVampire, Alive, $, 64 + last.x, last.y)
                            case (Shantak, StarVampire) => DrawItem(null, f, StarVampire, Alive, $, 70 + last.x, last.y)
                            case (StarVampire, StarVampire) => DrawItem(null, f, StarVampire, Alive, $, 65 + last.x, last.y)

                            case (Cthulhu, Voonith) => DrawItem(null, f, Voonith, Alive, $, 50 + last.x, 6 + last.y)
                            case (Abhoth, Voonith) => DrawItem(null, f, Voonith, Alive, $, 40 + last.x, last.y)
                            case (Daoloth, Voonith) => DrawItem(null, f, Voonith, Alive, $, 49 + last.x, last.y)
                            case (Nyogtha, Voonith) => DrawItem(null, f, Voonith, Alive, $, 44 + last.x, last.y)
                            case (Starspawn, Voonith) => DrawItem(null, f, Voonith, Alive, $, 39 + last.x, last.y)
                            case (Shoggoth, Voonith) => DrawItem(null, f, Voonith, Alive, $, 38 + last.x, last.y)
                            case (DeepOne, Voonith) if last.health == Alive => DrawItem(null, f, Voonith, Alive, $, 32 + last.x, last.y)
                            case (DeepOne, Voonith) if last.health == Pained => DrawItem(null, f, Voonith, Alive, $, 32 + last.x, last.y + 31)
                            case (Acolyte, Voonith) => DrawItem(null, f, Voonith, Alive, $, 33 + last.x, last.y)
                            case (HighPriest, Voonith) => DrawItem(null, f, Voonith, Alive, $, 38 + last.x, last.y)
                            case (Ghast, Voonith) => DrawItem(null, f, Voonith, Alive, $, 34 + last.x, last.y)
                            case (Gug, Voonith) => DrawItem(null, f, Voonith, Alive, $, 41 + last.x, last.y)
                            case (Shantak, Voonith) => DrawItem(null, f, Voonith, Alive, $, 45 + last.x, last.y)
                            case (StarVampire, Voonith) => DrawItem(null, f, Voonith, Alive, $, 42 + last.x, last.y)
                            case (Voonith, Voonith) => DrawItem(null, f, Voonith, Alive, $, 42 + last.x, last.y)
                            case (Voonith, Gnorri) => DrawItem(null, f, Gnorri, Alive, $, 46 + last.x, last.y)

case (Cthulhu, DimensionalShamblerUnit) => DrawItem(null, f, DimensionalShamblerUnit, Alive, $, 79 + last.x, 6 + last.y)
case (Abhoth, DimensionalShamblerUnit) => DrawItem(null, f, DimensionalShamblerUnit, Alive, $, 63 + last.x, last.y)
case (Daoloth, DimensionalShamblerUnit) => DrawItem(null, f, DimensionalShamblerUnit, Alive, $, 77 + last.x, last.y)
case (Nyogtha, DimensionalShamblerUnit) => DrawItem(null, f, DimensionalShamblerUnit, Alive, $, 69 + last.x, last.y)
case (Starspawn, DimensionalShamblerUnit) => DrawItem(null, f, DimensionalShamblerUnit, Alive, $, 61 + last.x, last.y)
case (Shoggoth, DimensionalShamblerUnit) => DrawItem(null, f, DimensionalShamblerUnit, Alive, $, 60 + last.x, last.y)
case (DeepOne, DimensionalShamblerUnit) if last.health == Alive => DrawItem(null, f, DimensionalShamblerUnit, Alive, $, 50 + last.x, last.y)
case (DeepOne, DimensionalShamblerUnit) if last.health == Pained => DrawItem(null, f, DimensionalShamblerUnit, Alive, $, 50 + last.x, last.y + 31)
case (Acolyte, DimensionalShamblerUnit) => DrawItem(null, f, DimensionalShamblerUnit, Alive, $, 52 + last.x, last.y)
case (HighPriest, DimensionalShamblerUnit) => DrawItem(null, f, DimensionalShamblerUnit, Alive, $, 59 + last.x, last.y)
case (Ghast, DimensionalShamblerUnit) => DrawItem(null, f, DimensionalShamblerUnit, Alive, $, 53 + last.x, last.y)
case (Gug, DimensionalShamblerUnit) => DrawItem(null, f, DimensionalShamblerUnit, Alive, $, 64 + last.x, last.y)
case (Shantak, DimensionalShamblerUnit) => DrawItem(null, f, DimensionalShamblerUnit, Alive, $, 70 + last.x, last.y)
case (StarVampire, DimensionalShamblerUnit) => DrawItem(null, f, DimensionalShamblerUnit, Alive, $, 65 + last.x, last.y)
case (Voonith, DimensionalShamblerUnit) => DrawItem(null, f, DimensionalShamblerUnit, Alive, $, 42 + last.x, last.y)
case (DimensionalShamblerUnit, DimensionalShamblerUnit) => DrawItem(null, f, DimensionalShamblerUnit, Alive, $, 65 + last.x, last.y)
                            case (Cthulhu, Gnorri) => DrawItem(null, f, Gnorri, Alive, $, 64 + last.x, 6 + last.y)
                            case (Abhoth, Gnorri) => DrawItem(null, f, Gnorri, Alive, $, 52 + last.x, last.y)
                            case (Daoloth, Gnorri) => DrawItem(null, f, Gnorri, Alive, $, 63 + last.x, last.y)
                            case (Nyogtha, Gnorri) => DrawItem(null, f, Gnorri, Alive, $, 56 + last.x, last.y)
                            case (Starspawn, Gnorri) => DrawItem(null, f, Gnorri, Alive, $, 50 + last.x, last.y)
                            case (Shoggoth, Gnorri) => DrawItem(null, f, Gnorri, Alive, $, 48 + last.x, last.y)
                            case (DeepOne, Gnorri) if last.health == Alive => DrawItem(null, f, Gnorri, Alive, $, 41 + last.x, last.y)
                            case (DeepOne, Gnorri) if last.health == Pained => DrawItem(null, f, Gnorri, Alive, $, 41 + last.x, last.y + 31)
                            case (Acolyte, Gnorri) => DrawItem(null, f, Gnorri, Alive, $, 42 + last.x, last.y)
                            case (HighPriest, Gnorri) => DrawItem(null, f, Gnorri, Alive, $, 48 + last.x, last.y)
                            case (Ghast, Gnorri) => DrawItem(null, f, Gnorri, Alive, $, 43 + last.x, last.y)
                            case (Gug, Gnorri) => DrawItem(null, f, Gnorri, Alive, $, 52 + last.x, last.y)
                            case (Shantak, Gnorri) => DrawItem(null, f, Gnorri, Alive, $, 57 + last.x, last.y)
                            case (StarVampire, Gnorri) => DrawItem(null, f, Gnorri, Alive, $, 53 + last.x, last.y)
                            case (DimensionalShamblerUnit, Gnorri) => DrawItem(null, f, Gnorri, Alive, $, 53 + last.x, last.y)
                            case (Gnorri, Gnorri) => DrawItem(null, f, Gnorri, Alive, $, 58 + last.x, last.y)
                            case (Gnorri, Filth) => DrawItem(null, f, Filth, Alive, $, 37 + last.x, last.y - 15)
                            case (Cthulhu, Filth) => DrawItem(null, f, Filth, Alive, $, 62 + last.x, last.y - 10)
                            case (Abhoth, Filth) => DrawItem(null, f, Filth, Alive, $, 50 + last.x, last.y - 15)
                            case (Daoloth, Filth) => DrawItem(null, f, Filth, Alive, $, 70 + last.x, last.y - 15)
                            case (Nyogtha, Filth) => DrawItem(null, f, Filth, Alive, $, 65 + last.x, last.y - 15)
                            case (Starspawn, Filth) => DrawItem(null, f, Filth, Alive, $, 53 + last.x, last.y - 15)
                            case (Shoggoth, Filth) => DrawItem(null, f, Filth, Alive, $, 52 + last.x, last.y - 15)
                            case (DeepOne, Filth) if last.health == Alive => DrawItem(null, f, Filth, Alive, $, 42 + last.x, last.y - 15)
                            case (DeepOne, Filth) if last.health == Pained => DrawItem(null, f, Filth, Alive, $, 42 + last.x, last.y + 16)
                            case (Acolyte, Filth) => DrawItem(null, f, Filth, Alive, $, 38 + last.x, last.y - 15)
                            case (HighPriest, Filth) => DrawItem(null, f, Filth, Alive, $, 53 + last.x, last.y - 15)
                            case (Ghast, Filth) => DrawItem(null, f, Filth, Alive, $, 40 + last.x, last.y - 15)
                            case (Gug, Filth) => DrawItem(null, f, Filth, Alive, $, 57 + last.x, last.y - 15)
                            case (Shantak, Filth) => DrawItem(null, f, Filth, Alive, $, 53 + last.x, last.y - 15)
                            case (StarVampire, Filth) => DrawItem(null, f, Filth, Alive, $, 53 + last.x, last.y - 15)
                            case (Voonith, Filth) => DrawItem(null, f, Filth, Alive, $, 34 + last.x, last.y - 15)
case (DimensionalShamblerUnit, Filth) => DrawItem(null, f, Filth, Alive, $, 53 + last.x, last.y - 15)
                            case (Filth, Filth) => DrawItem(null, f, Filth, Alive, $, 40 + last.x, last.y)

                            // Generic fallback for neutral monsters/iGOOs/new units in deep
                            case (_, next) => DrawItem(null, f, next, Alive, $, 60 + last.x, last.y)
                        })
                    }

                    draws./(_.rect)
                }

                val shamblers : $[DrawRect] = $

                val captured = {
                    var draws : $[DrawItem] = $

                    game.setup.but(f)./~(_.at(f.prison)).foreach { u =>
                        val (prisonXOffset, prisonYOffset) = u.uclass match {
                            case Filth        => (8, -15)
                            case _            => (0, 0)
                        }

                        val x = draws./(_.rect.width).sum - 0 + prisonXOffset
                        val y = h - 12 + prisonYOffset

                        draws :+= DrawItem(null, u.faction, u.uclass, Alive, $, x, y)
                    }

                    draws./(_.rect)
                }

                val dw = deep.any.?(deep./(r => r.x + r.width).max - deep./(_.x).min).|(0)
                val cw = captured.any.?(captured./(r => r.x + r.width).max - captured./(_.x).min).|(0)
                val sw = 0
                val draws =
                    if (dw + cw + sw > w - 20)
                        (deep ++ shamblers./(r => r.copy(x = r.x + dw)) ++ captured./(r => r.copy(x = r.x + dw + sw)))./(r => r.copy(x = r.x * (w - 20) / (dw + cw + sw)))
                    else
                    if (dw > 0 && sw > 0)
                        deep ++ shamblers./(r => r.copy(x = r.x + dw)) ++ captured./(r => r.copy(x = r.x + w - cw - 20))
                    else
                    if (dw > 0)
                        deep ++ captured./(r => r.copy(x = r.x + w - cw - 20))
                    else if (sw > 0 && cw > 0)
                        shamblers./(r => r.copy(x = r.x + 35)) ++ captured./(r => r.copy(x = r.x + w - cw - 20))
                    else if (sw > 0)
                        shamblers./(r => r.copy(x = r.x + 35))
                    else
                        captured./(r => r.copy(x = r.x + (w - cw) / 2))

                draws.reverse.foreach(dd)

                val iconsWidth = f.loyaltyCards.num * 30 + 60
                f.at(ShamblerHold(f)).reverse.zipWithIndex.foreach { case (u, i) =>
                    val x = w - iconsWidth - 40 - i * 40
                    val y = h - 85 - 12
                        dd(DrawItem(null, f, DimensionalShamblerUnit, Alive, $, x + 35, y + 75).rect)
                }

                // Bloated Woman Velvet Fan: render captured units on BW owner's faction card
                val velvetFanUnits = game.setup./~(e => e.at(VelvetFanHold(f)))
                velvetFanUnits.reverse.zipWithIndex.foreach { case (u, i) =>
                    val shamblerCount = f.at(ShamblerHold(f)).num
                    val x = w - iconsWidth - 40 - (shamblerCount + i) * 40
                    val y = h - 85 - 12
                    dd(DrawItem(null, u.faction, u.uclass, Alive, $, x + 35, y + 75).rect)
                }
            }

            def updateStatus() {
                0.until(seating.num).foreach { n =>
                    factionStatus(displayGame.setup(n), statusBitmaps(n))(displayGame)
                }

                val prevHistLen = RitualTrackOverlay.ritualHistory.length
                val prevMarker = RitualTrackOverlay.markerIndex
                val prevPureDH = RitualTrackOverlay.tsPureDHMarkerIndices.length // Tombstalker (TS): track pure DH marker changes for overlay refresh
                RitualTrackOverlay.numPlayers = seating.num
                RitualTrackOverlay.markerIndex = displayGame.ritualMarker
                RitualTrackOverlay.trackLength = displayGame.ritualTrack.length
                RitualTrackOverlay.ritualHistory = displayGame.ritualHistory./(_.style)
                RitualTrackOverlay.ritualHistoryCeremony = displayGame.ritualHistoryCeremony
                // Tombstalker (TS): sync pure Death's Head hecatomb ritual markers for RoA track overlay
                RitualTrackOverlay.tsPureDHMarkerIndices = TSExpansion.pureDHMarkerIndices
                // [2026-04-03] Auto-refresh overlay when ritual data changes
                if (RitualTrackOverlay.ritualHistory.length != prevHistLen ||
                    RitualTrackOverlay.markerIndex != prevMarker ||
                    RitualTrackOverlay.tsPureDHMarkerIndices.length != prevPureDH)
                    Overlays.refreshIfShowing()

                // Tombstalker (TS): populate Cursed Tomes overlay with per-faction tome ownership and stack state
                TSCursedTomesOverlay.factionTomes = displayGame.cursedTomesOwned.map { case (f, tomes) => f.style -> tomes }
                TSCursedTomesOverlay.tomesOnCard = displayGame.tsTomesOnCard

                dom.document.getElementById("roa-cost-num").?.foreach(_.innerHTML = displayGame.ritualCost.toString)

                mapWest.innerHTML = (board.west :+ GC.deep)./(r => processStatus(displayGame.regionStatus(r), "p8")).mkString("")
                mapEast.innerHTML = board.east./(r => processStatus(displayGame.regionStatus(r), "p8")).mkString("")

                drawMap(displayGame)
            }

            dom.window.onresize = e => { invalidateMapSize(); updateStatus() }

            if (hash == "") {
                val localTitle = dom.document.createElement("div")
                localTitle.innerHTML = s"""
                    <div style="
                        position: absolute;
                        left: 0%;
                        top: 0%;
                        width: 100%;
                        height: 100%;
                        z-index: 10;
                        pointer-events: none;
                    ">
                        <div style="
                            color: rgb(255, 255, 255);
                            font-size: 100%;
                            font-weight: bold;
                            filter: drop-shadow(rgb(0, 0, 0) 0px 0px 6px) drop-shadow(rgb(0, 0, 0) 0px 0px 6px) drop-shadow(rgb(0, 0, 0) 0px 0px 6px);
                            text-align: left;
                            display: flex;
                            align-items: center;
                            flex-wrap: nowrap;
                        ">
                            <span
                                style="
                                    pointer-events: auto;
                                    cursor: pointer;
                                    display: inline-flex;
                                    align-items: center;
                                    position: relative;
                                    vertical-align: middle;
                                    flex-shrink: 0;
                                "
                                onclick="event.stopPropagation(); onExternalClick('RoA')"
                                onpointerover="event.stopPropagation(); onExternalOver('RoA')"
                                onpointerout="event.stopPropagation(); onExternalOut('RoA')">
                                <img src="${Overlays.imageSource("roa-icon")}"
                                     style="height: max(2.5em, 7vmin); width: auto; display: block;" />
                                <span id="roa-cost-num"
                                      style="
                                          position: absolute;
                                          top: 50%;
                                          left: 50%;
                                          transform: translate(-50%, -50%);
                                          color: white;
                                          font-size: max(1.1em, 3vmin);
                                          font-weight: bold;
                                          line-height: 1;
                                          text-shadow: 0 0 3px black, 0 0 3px black, 0 0 3px black;
                                          pointer-events: none;
                                      ">5</span>
                            </span>
                        </div>
                    </div>"""
                mapSmall.appendChild(localTitle)
            }

            var token : Double = 0.0

            var savedUIAction : |[(Faction, $[Action])] = None
            var savedContinue : |[Continue] = None

            class BackgroundCheckToken(random : Double)

            var backgroundCheckThread : |[BackgroundCheckToken] = None

            def startBackgroundCheck() {
                if (backgroundCheckThread.none) {
                    val t = new BackgroundCheckToken(math.random())
                    backgroundCheckThread = |(t)
                    executeBackgroundCheck(t)
                }
            }

            def executeBackgroundCheck(token : BackgroundCheckToken) {
                if (backgroundCheckThread.has(token)) {
                    setTimeout(500) {
                        if (backgroundCheckThread.has(token)) {
                            getF(server + "read/" + hash + "/" + (actions.num + 3)) { ll =>
                                if (backgroundCheckThread.has(token)) {
                                    if (ll.splt("\n").but("").any) {
                                        backgroundCheckThread = None

                                        // ask("Waiting for update >" + ll + "<", $, n => {})
                                        ask("Waiting for update", $, n => {})

                                        perform(UpdateAction)
                                    }
                                    else {
                                        executeBackgroundCheck(token)
                                    }
                                }
                            }
                        }
                    }
                }
            }

            def stopBackgroundCheck() {
                backgroundCheckThread = None
            }

            def perform(action : Action) {
                queue :+= UIPerform(game, action)

                if (!paused)
                    startUI()
            }

            def finishUI() {
                updateUI() match {
                    case Some(_) => finishUI()
                    case None =>
                }
            }

            def startUI() {
                token = math.random()

                processUI(token)
            }

            def processUI(t : Double) {
                if (t == token) {
                    updateUI() match {
                        case Some((s, n)) =>
                            if (s) {
                                updateStatus()
                                if (recorded.any && hash == "")
                                    replayMenu()
                            }

                            setTimeout(n * delay) { processUI(t) }
                        case None =>
                    }
                }
            }

            def cancelUndo() {
                clear(undoDiv)
                hide(undoDiv.parentElement.parentElement)

                if (overrideGame.any) {
                    overrideGame = None
                    updateStatus()
                }
            }

            def showUndo(n : Int) : () => Unit = () => {
                show(undoDiv.parentElement.parentElement)

                clear(undoDiv)

                undoDiv.appendChild(newDiv("", "Game state after " + n + " actions"))

                val style = None

                if (hash != "")
                    undoDiv.appendChild(newDiv("option" + style./(" " + _).|(""), "Undo to here".hl, () => { clear(undoDiv); performUndoOnline(n) }))
                else
                    undoDiv.appendChild(newDiv("option" + style./(" " + _).|(""), "Undo to here".hl, () => { clear(undoDiv); performUndoLocal(n) }))

                undoDiv.appendChild(newDiv("option" + style./(" " + _).|(""), "Cancel", () => { cancelUndo() }))

                val g = new Game(board, track, seating, true, setup.options)

                actions.reverse.take(n).foreach { a =>
                    if (a.isVoid.not)
                        g.perform(a.unwrap)
                }

                overrideGame = |(g)

                updateStatus()

                ()
            }

            def performUndoLocal(n : Int) : Unit = {
                actions = actions.takeLast(n)

                clear(logDiv)

                log(version)

                var cc : Continue = StartContinue

                val g = new Game(board, track, seating, true, setup.options)

                actions.reverse.indexed./ { (a, i) =>
                    if (a.isVoid.not) {
                        val (l, c) = g.perform(a.unwrap)

                        l.foreach(s => log(s, showUndo(i + 1)))

                        if (a.isOutOfTurn.not)
                            cc = c
                    }
                }

                game = g
                overrideGame = None

                queue = $(askFaction(cc)(game))

                startUI()
            }

            def performUndoOnline(n : Int) : Unit = {
                hrf.web.postF(server + "rollback-v2/" + hash + "/" + (n + 3), "") { _ =>
                    dom.document.location.assign(dom.document.location.href)
                } {
                    log("Failed to undo, reloading...")

                    setTimeout(3000) {
                        dom.document.location.assign(dom.document.location.href)
                    }
                }
            }


            def updateUI() : Option[(Boolean, Int)] = {
                queue.@@ {
                    case head :: rest =>
                        queue = rest
                        // println(head)
                        head match {
                            case UILog(l) => {
                                log(l, showUndo(actions.num))

                                Some((false, (l == DottedLine).?(16).|(5)))
                            }
                            case UIPerform(g, a : GameOverAction) if a.msg == "Save replay" => {
                                val ir = hrf.html.ImageResources(Map(), Map(), hrf.HRF.imageCache)
                                val resources = hrf.html.Resources(ir, () => Map())
                                val title = "Cthulhu Wars " + BuildInfo.version + " Replay"
                                val filename = "cthulhu-wars-" + BuildInfo.version + "-replay-" + hrf.HRF.startAt.toISOString().take(16).replace("T", "-")

                                hrf.quine.Quine.save(title, g.setup, g.options, resources, actions.reverse, new Serialize(g), filename, true, "", {
                                    val winners = a.winners
                                    queue :+= UIQuestion(null, game, GameOverAction(winners, "Hooray!") :: GameOverAction(winners, "Meh...") :: GameOverAction(winners, "Save replay"))
                                })

                                Some((false, 10))
                            }
                            case UIPerform(g, UpdateAction) => {
                                queue :+= UIRead(g)

                                Some((false, 0))
                            }
                            case UIPerform(g, OutOfTurnReturn) => {
                                savedUIAction.$./{ (f, l) =>
                                    if (l.of[OutOfTurnRefresh].any)
                                        queue = UIPerform(g, l.of[OutOfTurnRefresh].only.action) +: queue
                                    else
                                        queue = UIQuestion(f, g, l) +: queue
                                }
                                savedUIAction = None
                                Some((false, 0))
                            }
                            case UIPerform(g, OutOfTurnRepeat(f, action)) => {
                                val (l, c) = g.perform(action.unwrap)

                                val cc = c match {
                                    case Ask(f, l) => Ask(f, l./(a => a.useIf(_.unwrap == CancelAction)(_ => OutOfTurnDone.as("Done")(" "))))
                                    case c => c
                                 }

                                queue :+= askFaction(cc)(game)

                                Some((false, 0))
                            }
                            case UIPerform(g, a) if hash != "" && self == None && localReplay.not => {
                                queue :+= UIRead(g)

                                Some((false, 30))
                            }
                            case UIPerform(g, a) if hash != "" && a.isRecorded && localReplay.not => {
                                val position = actions.num
                                var n = 0

                                def retry() {
                                    n += 1

                                    if (n < 12) {
                                        if (n > 1)
                                            askN($(n.times(".").mkString("")), $)

                                        setTimeout(n * 100) { post() }
                                    }
                                    else {
                                        hrf.web.getF(server + "alive") { s =>
                                            if (s == "1")
                                                askN($("Synchronization Error"), $(AskLine("", "Reload", $, false, () => dom.document.location.assign(dom.document.location.href))))
                                            else
                                                askN($("Server Error"), $(AskLine("", "Reload", $, false, () => dom.document.location.assign(dom.document.location.href))))
                                        } {
                                            askN($("Server Down"), $(AskLine("", "Please Reload Later", $("thin"), false)))
                                        }
                                    }
                                }

                                def post() {
                                    if (position == actions.num) {
                                        hrf.web.postF(server + "write/" + hash + "/" + (position + 3), serializer.write(a.unwrap)) { _ =>
                                            queue :+= UIRead(g)

                                            startUI()
                                        } {
                                            if (position == actions.num) {
                                                hrf.web.getF(server + "read/" + hash + "/" + (position + 3)) { ll =>
                                                    if (ll.splt("\n").but("").any) {
                                                        queue :+= UIRead(g)

                                                        startUI()
                                                    }
                                                    else
                                                        retry()
                                                } {
                                                    retry()
                                                }
                                            }
                                        }
                                    }
                                }

                                post()

                                None
                            }
                            case UIRead(g) => {
                                getF(server + "read/" + hash + "/" + (actions.num + 3)) { ll =>
                                    queue :+= UIProcess(g, ll.splt("\n").but("")./(serializer.parseAction))

                                    startUI()
                                }

                                None
                            }
                            case UIProcess(g, recorded) if recorded.none => {
                                queue :+= UIRead(g)

                                Some((false, 30))
                            }
                            case UIProcess(g, recorded) => {
                                savedUIAction = None

                                var cc : |[Continue] = savedContinue

                                val initial = actions.none

                                recorded.indexed./ { (a, n) =>
                                    actions +:= a

                                    if (a.is[ReloadAction.type] && initial.not) {
                                        println("reloading...")
                                        dom.document.location.assign(dom.document.location.href)
                                        return None
                                    }

                                    if (a.isVoid.not) {
                                        game.nextReplayActionHint = if (n + 1 < recorded.num) Some(serializer.write(recorded(n + 1))) else None
                                        val (l, c) = game.perform(a.unwrap)
                                        game.nextReplayActionHint = None

                                        l.foreach(s => log(s, showUndo(actions.num)))

                                        if (a.isOutOfTurn.not)
                                            cc = |(c)
                                        else
                                            c @@ {
                                                case Then(OutOfTurnRepeat(f, action)) if self.has(f) => cc = |(c)
                                                case Then(OutOfTurnRepeat(f, action)) if self.has(f).not =>
                                                case c => cc = |(c)
                                            }
                                    }
                                }

                                savedContinue = cc

                                queue :+= askFaction(cc.get)(game)

                                Some((true, 0))
                            }

                            case UIPerform(g, action) if action.isVoid =>
                                if (action.isRecorded || recorded.any)
                                    actions +:= action

                                if (recorded.any && hash == "" && localReplay.not) {
                                    if (recorded.num > actions.num && paused.not)
                                        queue :+= UIPerform(game, serializer.parseAction(recorded(actions.num).replace("&gt;", ">")))
                                }

                                Some((true, 0))

                            case UIPerform(g, action) => {
                                val a = action match {
                                    case esa : ElderSignAction if recorded.any => esa.copy(public = true)
                                    case a => a
                                }

                                if (a.isRecorded)
                                    actions +:= a

                                game.nextReplayActionHint = if (recorded.any && hash == "" && localReplay.not && recorded.num > actions.num) Some(recorded(actions.num).replace("&gt;", ">")) else None
                                val (l, c) = g.perform(a.unwrap)
                                game.nextReplayActionHint = None

                                l.foreach { s =>
                                    queue :+= UILog(s)
                                }

                                val t = c match {
                                    case Ask(f, _) if setup.difficulty(f) == Human => 2
                                    case Ask(_, actions) if actions.distinct.num <= 2 => 4
                                    case DelayedContinue(n, _) => n
                                    case Then(_) => 0
                                    case _ => 30
                                 }

                                if (recorded.any && hash == "" && localReplay.not) {
                                    if (recorded.num > actions.num && !paused)
                                        queue :+= UIPerform(game, serializer.parseAction(recorded(actions.num).replace("&gt;", ">")))
                                }
                                else
                                    queue :+= askFaction(c)(game)

                                Some((true, t))
                            }
                            case UIRollD6(g, q, roll) => {
                                ask(q(g), (1::2::3::4::5::6)./("[" + _.styled("power") + "]"), x => perform(roll(x)))
                            }
                            case UIRollAgony(g, q, roll) => {
                                ask(q(g), AgonyDie.faces.distinct.sorted./("[" + _.styled("power") + "]"), x => perform(roll(x)))
                            }
                            case UIRollBattle(g, q, n, roll) if n <= 3 => {
                                def apr(br : BattleRoll) = 0.to(n)./(_.times(br))
                                val results = apr(Kill)./~(k => apr(Pain)./~(p => apr(Miss)./(m => k ++ p ++ m))).%(_.num == n)
                                val os = results./(roll)
                                ask(q(g), results./(_.mkString(" ")), v => perform(roll(results(v))))
                            }
                            case UIRollBattle(g, q, n, roll) => {
                                val osK = 0.to(n)./(_.times(Kill))./(x => x.any.?(x.mkString(" ")).|("None"))
                                ask(q(g) + "<br/>" + "Number of " + "Kills".styled("kill"), osK, kills => {
                                    if (kills == n) {
                                        perform(roll(kills.times(Kill)))
                                    }
                                    else {
                                        val osP = 0.to(n - kills)./(_.times(Pain))./(x => x.any.?(x.mkString(" ")).|("None"))
                                        ask(q(g) + "<br/>" + "Number of " + "Pains".styled("pain"), osP, pains => {
                                            perform(roll(kills.times(Kill) ++ pains.times(Pain) ++ (n - kills - pains).times(Miss)))
                                        })
                                    }
                                })
                            }
                            case UIDrawES(g, q, es1, es2, es3, draw) =>
                                val options = ((1 -> es1) :: (2 -> es2) :: (3 -> es3)).%>(_ > 0)
                                ask(q(g), options./((e, q) => "[" + e.styled("es") + "]" + " of " + q), n => perform(draw(options(n)._1, true)))

                            case UIQuestion(e, game, actions, waiting) if hash != "" && e != null && self.none && localReplay.not => {
                                startBackgroundCheck()

                                scrollTop = 0

                                askN($("Waiting for " + waiting.some.|($(e)).mkString(", ") + "<br/>“" + actions.first.safeQ(game) + "”<br/><br/>"), $)
                            }
                            case UIQuestion(e, game, actions, waiting) if hash != "" && e != null && self.any && self.has(e).not && localReplay.not => {
                                val extra = self./~(f => game.extraActions(f, true, actions.has(SacrificeHighPriestAllowedAction)))
                                val f = self.get

                                startBackgroundCheck()

                                scrollTop = 0

                                askN($("Waiting for " + waiting.some.|($(e)).mkString(", ") + "<br/>“" + actions.first.safeQ(game) + "”<br/><br/>"),
                                    extra./(a => AskLine(a.question(game), a.option(game), false.$(f.style + "-border") ++ a.isInfo.$("thin"), a.isNoClear.not, a.isInfo.not.??(() => {
                                        stopBackgroundCheck()

                                        savedUIAction = |((e, actions))

                                        perform(a)
                                    })))
                                )
                            }
                            case UIQuestion(f, game, actions, waiting) => {
                                cancelUndo()

                                if (hash != "")
                                    startBackgroundCheck()

                                val extra = actions.unwrap.use(l => game.extraActions(f, false, actions.has(SacrificeHighPriestAllowedAction)).%{
                                    case _ if l.has(CancelAction) => false
                                    case _ if l.has(OutOfTurnDone) => false
                                    case _ : DragonAscendingOutOfTurnAction if l.of[DragonAscendingPromptAction].any => false
                                    case _ : SacrificeHighPriestOutOfTurnMainAction if l.of[SacrificeHighPriestPromptAction].any => false
                                    case _ => true
                                })

                                scrollTop = 0

                                val aa = actions.useIf(game.options.has(AsyncActions))(_
                                    .%!(_.unwrap.is[RevealESMainAction])
                                    .%!(_.unwrap.is[SacrificeHighPriestDoomAction])
                                    .%!(_.unwrap.is[SacrificeHighPriestMainAction])
                                    .%!(_.unwrap.is[DragonAscendingMainAction])
                                    .%!(_.unwrap.is[DragonAscendingDoomAction])
                                )

                                actionDiv.className = "inner action unselectable"

                                askN($,
                                    aa./(a => AskLine(a.question(game), a.option(game), $(f.style + "-border") ++ a.isInfo.$("thin"), a.isNoClear.not, a.isInfo.not.??(() => {
                                        stopBackgroundCheck()

                                        perform(a)
                                    }))) ++
                                    extra./(a => AskLine(a.question(game), a.option(game), false.$(f.style + "-border") ++ a.isInfo.$("thin"), a.isNoClear.not, a.isInfo.not.??(() => {
                                        stopBackgroundCheck()

                                        savedUIAction = |((f, actions))

                                        perform(a)
                                    })))
                                )

                                None
                            }
                            case UIQuestionDebug(f, g, actions) => {
                                cancelUndo()

                                val aa = Explode.explode(g, actions)

                                // Tombstalker (TS) and BG: use Bot3 evaluation for debug action sorting
                                // NOTE: only fires in Debug difficulty mode, not during normal bot play
                                val sorted = if (f == BG || f == TS)
                                    Bot3(f).eval(aa)(g).sortBy(-_.evaluations.map(_.weight).sum)
                                else
                                if (f == null) {
                                    (BotYS.eval(g, aa).sortWith(BotYS.compare).take(1) ++ BotYSOld.eval(g, aa).sortWith(BotYSOld.compare).take(1))./(ae => ae.copy(evaluations = ae.evaluations.%(_.desc != "random"))).distinct
                                }
                                else {
                                    val bot = (f match {
                                        case GC => BotGC
                                        case CC => BotCC
                                        case YS => BotYS
                                        case SL => BotSL
                                        case WW => BotWW
                                        case OW => BotOW
                                        case AN => BotAN
                                        // Round 8 (FB): added FB case so the debug action-sort menu works for
                                        // FB games. Without this, opening the debug menu for an FB action
                                        // would throw a MatchError at runtime.
                                        case FB => BotFB
                                    })
                                    bot.eval(g, aa).sortWith(bot.compare)
                                }

                                askM($, sorted./(wa => wa.action.question(g).some.|(" ") -> (wa.action.option(g) + " " + wa.action.toString + " (" + wa.evaluations.starting./(_.weight)./(v => v.styled((v > 0).?("power").|("doom"))).|("0") + ")" + "<br/>" +
                                    wa.evaluations./(e =>
                                        ("(" + e.weight.styled((e.weight > 0).?("power").|("doom")) + " -> " + e.desc + ")").styled("expl")
                                    ).mkString("<br/>"))),
                                    n => {
                                        println((sorted(n).action.question(g) + " -> " + sorted(n).action.option(g)).replaceAll("<[^>]*>", ""))
                                        sorted(n).evaluations.foreach { e =>
                                            println("  (" + e.weight + " -> " + e.desc + ")")
                                        }
                                        perform(sorted(n).action)
                                    }
                                )
                            }
                        }
                    case Nil =>
                        None
                }
            }

            def replayMenu() {
                def action = recorded.lift(actions.num).map(_.replace("&gt;", ">")).map(serializer.parseAction)
                ask("Replay (" + actions.num + " / " + recorded.num + ")", $(paused.?("Play").|("Pause"), "Start", "End", "Next"), {
                    case 0 =>
                        if (paused) {
                            paused = false
                            if (queue.none)
                                action.map(perform)
                        }
                        else {
                            paused = true
                        }
                        replayMenu()
                    case 1 =>
                        paused = true
                        game = new Game(game.board, game.ritualTrack, game.factions, game.logging, $)
                        actions = $
                        clear(logDiv)
                        updateStatus()
                        replayMenu()
                    case 2 =>
                        if (paused) {
                            paused = false
                            action.map(perform)
                        }
                        finishUI()
                        updateStatus()
                        paused = true
                        replayMenu()
                    case 3 =>
                        paused = true
                        if (queue.none) {
                            action.map(perform)
                            startUI()
                        }
                        replayMenu()
                })
            }

            if (hash != "") {
                if (recorded.any || self == None) {
                    queue :+= UIProcess(game, recorded./(serializer.parseAction))

                    startUI()
                }
                else
                    perform(StartAction)
            }
            else {
                if (recorded.any)
                    replayMenu()
                else
                    perform(StartAction)
            }
        }

        def smaller(s : String) = "<span class=\"smaller\">" + s + "</span>"

        def allSeatings(factions : $[Faction]) = factions.permutations.toList.%(s => s.has(GC).?(s(0) == GC).|(s(0) != WW))
        def randomSeating(factions : $[Faction]) = allSeatings(factions).sortBy(s => random()).head

        def useWith(setup : Setup, lc : LoyaltyCard, opt : GameOption, cond : Boolean) : $[(String, String)] = {
            if (!cond) $ else {
                val qm = Overlays.imageSource("question-mark")
                val nm = lc.name.replace("\\", "\\\\").replace("'", "&#39;")
                val p = s""""${nm}"""".replace('"'.toString, "&quot;")
                val yesNo = setup.get(opt).?("yes").|("no").hl
                val text = "<div class=sbdiv>Use " + lc.short + " (" + yesNo + ")" +
                    s"""<img class=explain src="${qm}" onclick="event.stopPropagation(); onExternalClick(${p})" onpointerover="onExternalOver(${p})" onpointerout="onExternalOut(${p})" />""" +
                    "</div>"
                $("Variants" -> text)
            }
        }

        def startOnlineSetup(factions : $[Faction]) {
            val all = allSeatings(factions)

            val seatings = all.%(s => all.indexOf(s) <= all.indexOf(s.take(1) ++ s.drop(1).reverse))

            val setup = new Setup(seatings(0), Human)

            // 2026-05-21: online-only option defaults — GateDiplomacy + AsyncActions
            // always on; Opener4P10Gates on when OW is in a 4-player game;
            // DemandTsathoggua on when SL is in the game. User can toggle off below.
            setup.options ++= $(GateDiplomacy, AsyncActions)
            if (factions.has(OW) && factions.num == 4)
                setup.options :+= Opener4P10Gates
            if (factions.has(SL))
                setup.options :+= DemandTsathoggua

            // [2026-05-31] Apply any pending Random Neutrals selection from the
            // alt faction picker, then clear so it doesn't leak to a later setup.
            pendingRandomNeutrals.foreach(_(setup))
            pendingRandomNeutrals = None

            def showMapPreview() {
                val mapDiv = getElem("map-small")
                setup.options.of[MapOption].lastOption.foreach { opt =>
                    val imgId = opt match {
                        case MapEarth33 => "earth33"
                        case MapEarth35 => "earth35"
                        case MapEarth53 => "earth53"
                        case MapEarth55 => "earth55"
                        case MapLibrary33 => "library3"
                        case MapLibrary35 => "library35"
                        case MapLibrary53 => "library53"
                        case MapLibrary55 => "library5"
                    }
                    val isHorizontal = dom.window.innerWidth > dom.window.innerHeight
                    val isLibrary = opt == MapLibrary33 || opt == MapLibrary35 || opt == MapLibrary53 || opt == MapLibrary55
                    val assetId = if (isLibrary && isHorizontal) imgId + "-h" else imgId
                    val img = getAsset(assetId)
                    mapDiv.innerHTML = ""
                    val canvas = dom.document.createElement("canvas").asInstanceOf[html.Canvas]
                    canvas.style.width = "100%"
                    canvas.style.height = "100%"
                    mapDiv.appendChild(canvas)
                    val w = mapDiv.clientWidth
                    val h = mapDiv.clientHeight
                    if (w > 0 && h > 0) {
                        canvas.width = w
                        canvas.height = h
                        val g = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
                        val scaleX = w.toDouble / img.width
                        val scaleY = h.toDouble / img.height
                        val scale = math.min(scaleX, scaleY)
                        val dw = (img.width * scale).toInt
                        val dh = (img.height * scale).toInt
                        val dx = (w - dw) / 2
                        val dy = (h - dh) / 2
                        g.drawImage(img, dx, dy, dw, dh)
                    }
                }
            }

            def setupQuestions() {
                showMapPreview()

                askM($,
                    factions.map(f => "Factions" -> ("" + f + " (" + setup.difficulty(f).elem + ")")) ++
                    seatings.map(ff => ("Seating" + factions.has(GC).not.??(" and first player")) -> ((ff == setup.seating).?(ff.map(_.ss)).|(ff.map(_.short)).mkString(" -> "))) ++
                    $("Gameplay" -> ("Gate Diplomacy (" + setup.get(GateDiplomacy).?("yes").|("no").hl + ")")) ++
                    $("Gameplay" -> ("Async Options (" + setup.get(AsyncActions).?("yes").|("no").hl + ")")) ++
                    $("Variants" -> ("High Priests (" + setup.get(HighPriests).?("yes").|("no").hl + ")")) ++
                    $("Variants" -> ("Neutral".styled("neutral") + " spellbooks (" + setup.get(NeutralSpellbooks).?("yes").|("no").hl + ")")) ++
                    $("Variants" -> ("Neutral".styled("neutral") + " monsters (" + setup.get(NeutralMonsters).?("yes").|("no").hl + ")")) ++
                    // Monsters (alphabetical)
                    useWith(setup, DimensionalShamblerCard, UseDimensionalShamblers, setup.options.has(NeutralMonsters)) ++
                    useWith(setup, ElderThingCard, UseElderThing, setup.options.has(NeutralMonsters)) ++
                    useWith(setup, GhastCard, UseGhast, setup.options.has(NeutralMonsters)) ++
                    useWith(setup, AlbinoPenguinsCard, UseAlbinoPenguins, setup.options.has(NeutralMonsters)) ++
                    useWith(setup, GnorriCard, UseGnorri, setup.options.has(NeutralMonsters)) ++
                    useWith(setup, GugCard, UseGug, setup.options.has(NeutralMonsters)) ++
                    useWith(setup, InsectsFromShaggaiCard, UseInsectsFromShaggai, setup.options.has(NeutralMonsters)) ++
                    useWith(setup, LengSpiderCard, UseLengSpider, setup.options.has(NeutralMonsters)) ++
                    useWith(setup, MoonbeastCard, UseMoonbeast, setup.options.has(NeutralMonsters)) ++
                    useWith(setup, SatyrCard, UseSatyr, setup.options.has(NeutralMonsters)) ++
                    useWith(setup, ServitorCard, UseServitor, setup.options.has(NeutralMonsters)) ++
                    useWith(setup, ShantakCard, UseShantak, setup.options.has(NeutralMonsters)) ++
                    useWith(setup, StarVampireCard, UseStarVampire, setup.options.has(NeutralMonsters)) ++
                    useWith(setup, VoonithCard, UseVoonith, setup.options.has(NeutralMonsters)) ++
                    // Terrors (alphabetical)
                    $("Variants" -> ("Neutral".styled("neutral") + " terrors (" + setup.get(NeutralTerrors).?("yes").|("no").hl + ")")) ++
                    useWith(setup, BrownJenkinCard, UseBrownJenkin, setup.options.has(NeutralTerrors)) ++
                    useWith(setup, DholeCard, UseDhole, setup.options.has(NeutralTerrors)) ++
                    useWith(setup, ElderShoggothCard, UseElderShoggoth, setup.options.has(NeutralTerrors)) ++
                    useWith(setup, GreatRaceOfYithCard, UseGreatRaceOfYith, setup.options.has(NeutralTerrors)) ++
                    useWith(setup, HoundOfTindalosCard, UseHoundOfTindalos, setup.options.has(NeutralTerrors)) ++
                    useWith(setup, QuachilUttausCard, UseQuachilUttaus, setup.options.has(NeutralTerrors)) ++
                    useWith(setup, ShadowPharaohCard, UseShadowPharaoh, setup.options.has(NeutralTerrors)) ++
                    // iGOOs (alphabetical)
                    $("Variants" -> ("Independent Great Old Ones (" + setup.get(IGOOs).?("yes").|("no").hl + ")")) ++
                    useWith(setup, AbhothCard, UseAbhoth, setup.options.has(IGOOs)) ++
                    useWith(setup, AtlachNachaCard, UseAtlachNacha, setup.options.has(IGOOs)) ++
                    useWith(setup, AzathothIGOOCard, UseAzathothIGOO, setup.options.has(IGOOs)) ++
                    useWith(setup, BokrugCard, UseBokrug, setup.options.has(IGOOs)) ++
                    useWith(setup, ByatisCard, UseByatis, setup.options.has(IGOOs)) ++
                    useWith(setup, CthughaCard, UseCthugha, setup.options.has(IGOOs)) ++
                    useWith(setup, DaolothCard, UseDaoloth, setup.options.has(IGOOs)) ++
                    useWith(setup, FatherDagonCard, UseFatherDagon, setup.options.has(IGOOs)) ++
                    useWith(setup, GhatanotoaIGOOCard, UseGhatanotoaIGOO, setup.options.has(IGOOs)) ++
                    useWith(setup, GlaakiIGOOCard, UseGlaakiIGOO, setup.options.has(IGOOs)) ++
                    useWith(setup, MotherHydraCard, UseMotherHydra, setup.options.has(IGOOs)) ++
                    useWith(setup, NyogthaCard, UseNyogtha, setup.options.has(IGOOs)) ++
                    useWith(setup, BloatedWomanCard, UseBloatedWoman, setup.options.has(IGOOs)) ++
                    useWith(setup, TulzschaCard, UseTulzscha, setup.options.has(IGOOs)) ++
                    useWith(setup, YgolonacCard, UseYgolonac, setup.options.has(IGOOs)) ++
                    useWith(setup, YigCard, UseYig, setup.options.has(IGOOs)) ++
                    (factions.has(SL) && factions.has(WW))
                        .$("Variants" -> ("" + IceAge + " affects " + Lethargy + " (" + setup.get(IceAgeAffectsLethargy).?("yes").|("no").hl + ")")) ++
                    (factions.has(OW) && factions.num == 4)
                        .$("Variants" -> ("" + OW + " needs 10 Gates in 4-Player (" + setup.get(Opener4P10Gates).?("yes").|("no").hl + ")")) ++
                    (factions.has(OW))
                        .$("Variants" -> ("" + OW + " Cheap Mutants + Harder SBR (" + setup.get(OpenerCheapMutants).?("yes").|("no").hl + ") <span class='pointer' onclick='event.stopPropagation(); onExternalClick(\"OpenerCheapMutants\")'>?</span>")) ++
                    (factions.has(OW))
                        .$("Variants" -> ("" + OW + " Yog Curse Die + DC GOO ES (" + setup.get(OpenerYogCurseDie).?("yes").|("no").hl + ") <span class='pointer' onclick='event.stopPropagation(); onExternalClick(\"OpenerYogCurseDie\")'>?</span>")) ++
                    (factions.has(SL))
                        .$("Variants" -> ("" + DemandSacrifice + " requires " + Tsathoggua + " (" + setup.get(DemandTsathoggua).?("yes").|("no").hl + ")")) ++
                    (factions.has(SL))
                        .$("Variants" -> ("" + SL + " Different Spellbook Requirements (" + setup.get(SleeperEasierSBR).?("yes").|("no").hl + ") <span class='pointer' onclick='event.stopPropagation(); onExternalClick(\"SleeperEasierSBR\")'>?</span>")) ++
                    (factions.has(SL))
                        .$("Variants" -> ("" + SL + " Energy Nexus - Pre Battle (" + setup.get(SleeperEnergyNexusPreBattle).?("yes").|("no").hl + ") <span class='pointer' onclick='event.stopPropagation(); onExternalClick(\"SleeperEnergyNexusPreBattle\")'>?</span>")) ++
                    (factions.has(DS))
                        .$("Variants" -> ("" + DS + " Alternate Spellbooks (" + setup.get(DSAlternateSpellbooks).?("yes").|("no").hl + ") <span class='pointer' onclick='event.stopPropagation(); onExternalClick(\"DSAlternateSpellbooks\")'>?</span>")) ++
                    $("Map" -> ("Map Configuration (" + setup.options.of[MapOption].lastOption.?(_.toString.hl) + ")")) ++
                    $("Done" -> "Start game".styled("power")),
                    nn => {
                        var n = nn
                        if (n >= 0 && n < factions.num) {
                            setup.difficulty += factions(n) -> $(Human, Easy, Normal, Human).dropWhile(_ != setup.difficulty(factions(n))).drop(1).head
                            setupQuestions()
                        }
                        n -= factions.num
                        if (n >= 0 && n < seatings.num) {
                            setup.seating = seatings(n)
                            setupQuestions()
                        }
                        n -= seatings.num
                        if (n == 0) {
                            setup.toggle(GateDiplomacy)
                            setupQuestions()
                        }
                        n -= 1
                        if (n == 0) {
                            setup.toggle(AsyncActions)
                            setupQuestions()
                        }
                        n -= 1
                        if (n == 0) {
                            setup.toggle(HighPriests)
                            setupQuestions()
                        }
                        n -= 1
                        if (n == 0) {
                            setup.toggle(NeutralSpellbooks)
                            setupQuestions()
                        }
                        n -= 1
                        if (n == 0) {
                            setup.toggle(NeutralMonsters)

                            if (setup.options.has(NeutralMonsters))
                                setup.options ++= $(UseDimensionalShamblers, UseElderThing, UseGhast, UseAlbinoPenguins, UseGnorri, UseGug, UseInsectsFromShaggai, UseLengSpider, UseMoonbeast, UseSatyr, UseServitor, UseShantak, UseStarVampire, UseVoonith)
                            else
                                setup.options = setup.options.notOf[NeutralMonsterOption]

                            setupQuestions()
                        }
                        if (setup.options.has(NeutralMonsters)) {
                            // Monsters (alphabetical): DimensionalShambler, ElderThing, Ghast, AlbinoPenguins, Gnorri, Gug, InsectsFromShaggai, LengSpider, Moonbeast, Satyr, Servitor, Shantak, StarVampire, Voonith
                            n -= 1; if (n == 0) { setup.toggle(UseDimensionalShamblers); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseElderThing); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseGhast); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseAlbinoPenguins); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseGnorri); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseGug); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseInsectsFromShaggai); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseLengSpider); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseMoonbeast); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseSatyr); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseServitor); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseShantak); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseStarVampire); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseVoonith); setupQuestions() }
                        }
                        n -= 1
                        if (n == 0) {
                            setup.toggle(NeutralTerrors)
                            if (setup.options.has(NeutralTerrors))
                                setup.options ++= $(UseBrownJenkin, UseDhole, UseElderShoggoth, UseGreatRaceOfYith, UseHoundOfTindalos, UseQuachilUttaus, UseShadowPharaoh)
                            else
                                setup.options = setup.options.notOf[NeutralTerrorOption]
                            setupQuestions()
                        }
                        if (setup.options.has(NeutralTerrors)) {
                            // Terrors (alphabetical): BrownJenkin, Dhole, ElderShoggoth, GreatRaceOfYith, HoundOfTindalos, QuachilUttaus, ShadowPharaoh
                            n -= 1; if (n == 0) { setup.toggle(UseBrownJenkin); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseDhole); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseElderShoggoth); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseGreatRaceOfYith); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseHoundOfTindalos); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseQuachilUttaus); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseShadowPharaoh); setupQuestions() }
                        }
                        n -= 1
                        if (n == 0) {
                            setup.toggle(IGOOs)

                            if (setup.options.has(IGOOs))
                                setup.options ++= $(UseAbhoth, UseAtlachNacha, UseAzathothIGOO, UseBokrug, UseByatis, UseCthugha, UseDaoloth, UseFatherDagon, UseGhatanotoaIGOO, UseGlaakiIGOO, UseMotherHydra, UseNyogtha, UseBloatedWoman, UseTulzscha, UseYgolonac, UseYig)
                            else
                                setup.options = setup.options.notOf[IGOOOption]

                            setupQuestions()
                        }
                        if (setup.options.has(IGOOs)) {
                            // iGOOs (alphabetical): Abhoth, Atlach-Nacha, Azathoth, Bokrug, Byatis, Cthugha, Daoloth, Father Dagon, Ghatanothoa, Gla'aki, Mother Hydra, Nyogtha, The Bloated Woman, Tulzscha, Y'Golonac, Yig
                            n -= 1; if (n == 0) { setup.toggle(UseAbhoth); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseAtlachNacha); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseAzathothIGOO); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseBokrug); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseByatis); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseCthugha); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseDaoloth); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseFatherDagon); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseGhatanotoaIGOO); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseGlaakiIGOO); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseMotherHydra); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseNyogtha); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseBloatedWoman); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseTulzscha); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseYgolonac); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseYig); setupQuestions() }
                        }
                        if (factions.has(SL) && factions.has(WW)) {
                            n -= 1
                            if (n == 0) {
                                setup.toggle(IceAgeAffectsLethargy)
                                setupQuestions()
                            }
                        }
                        if (factions.has(OW) && factions.num == 4) {
                            n -= 1
                            if (n == 0) {
                                setup.toggle(Opener4P10Gates)
                                setupQuestions()
                            }
                        }
                        if (factions.has(OW)) {
                            n -= 1
                            if (n == 0) {
                                setup.toggle(OpenerCheapMutants)
                                setupQuestions()
                            }
                        }
                        if (factions.has(OW)) {
                            n -= 1
                            if (n == 0) {
                                setup.toggle(OpenerYogCurseDie)
                                setupQuestions()
                            }
                        }
                        if (factions.has(SL)) {
                            n -= 1
                            if (n == 0) {
                                setup.toggle(DemandTsathoggua)
                                setupQuestions()
                            }
                        }
                        if (factions.has(SL)) {
                            n -= 1
                            if (n == 0) {
                                setup.toggle(SleeperEasierSBR)
                                setupQuestions()
                            }
                        }
                        if (factions.has(SL)) {
                            n -= 1
                            if (n == 0) {
                                setup.toggle(SleeperEnergyNexusPreBattle)
                                setupQuestions()
                            }
                        }
                        if (factions.has(DS)) {
                            n -= 1
                            if (n == 0) {
                                setup.toggle(DSAlternateSpellbooks)
                                setupQuestions()
                            }
                        }
                        n -= 1
                        if (n == 0) {
                            val all = $(MapEarth33, MapEarth35, MapEarth53, MapEarth55, MapLibrary33, MapLibrary35, MapLibrary53, MapLibrary55)
                            setup.options = setup.options.notOf[MapOption] :+ (all.dropWhile(setup.options.of[MapOption].lastOption.has(_).not).drop(1) ++ all).first
                            setupQuestions()
                        }
                        n -= 1
                        if (n == 0) {
                            val hasHuman = setup.seating.exists(f => setup.difficulty(f) == Human)
                            if (hasHuman)
                                startOnlineGame(setup)
                            else {
                                log("Online games must have at least one human player")
                                setupQuestions()
                            }
                        }
                    }
                )
            }

            setupQuestions()
            askTop()
        }

        def startSetup(factions : $[Faction]) {
            val all = allSeatings(factions)

            val seatings = all.%(s => all.indexOf(s) <= all.indexOf(s.take(1) ++ s.drop(1).reverse))

            val setup = new Setup(seatings(0), Human)

            // [2026-05-31] Apply any pending Random Neutrals selection from the
            // alt faction picker, then clear so it doesn't leak to a later setup.
            pendingRandomNeutrals.foreach(_(setup))
            pendingRandomNeutrals = None

            def showMapPreview() {
                val mapDiv = getElem("map-small")
                setup.options.of[MapOption].lastOption.foreach { opt =>
                    val imgId = opt match {
                        case MapEarth33 => "earth33"
                        case MapEarth35 => "earth35"
                        case MapEarth53 => "earth53"
                        case MapEarth55 => "earth55"
                        case MapLibrary33 => "library3"
                        case MapLibrary35 => "library35"
                        case MapLibrary53 => "library53"
                        case MapLibrary55 => "library5"
                    }
                    val isHorizontal = dom.window.innerWidth > dom.window.innerHeight
                    val isLibrary = opt == MapLibrary33 || opt == MapLibrary35 || opt == MapLibrary53 || opt == MapLibrary55
                    val assetId = if (isLibrary && isHorizontal) imgId + "-h" else imgId
                    val img = getAsset(assetId)
                    mapDiv.innerHTML = ""
                    val canvas = dom.document.createElement("canvas").asInstanceOf[html.Canvas]
                    canvas.style.width = "100%"
                    canvas.style.height = "100%"
                    mapDiv.appendChild(canvas)
                    val w = mapDiv.clientWidth
                    val h = mapDiv.clientHeight
                    if (w > 0 && h > 0) {
                        canvas.width = w
                        canvas.height = h
                        val g = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
                        val scaleX = w.toDouble / img.width
                        val scaleY = h.toDouble / img.height
                        val scale = math.min(scaleX, scaleY)
                        val dw = (img.width * scale).toInt
                        val dh = (img.height * scale).toInt
                        val dx = (w - dw) / 2
                        val dy = (h - dh) / 2
                        g.drawImage(img, dx, dy, dw, dh)
                    }
                }
            }

            def setupQuestions() {
                showMapPreview()

                askM($,
                    factions.map(f => "Factions" -> ("" + f + " (" + setup.difficulty(f).elem + ")")) ++
                    seatings.map(ff => ("Seating" + factions.has(GC).not.??(" and first player")) -> ((ff == setup.seating).?(ff.map(_.ss)).|(ff.map(_.short)).mkString(" -> "))) ++
                    $("Gameplay" -> ("Gate Diplomacy (" + setup.get(GateDiplomacy).?("yes").|("no").hl + ")")) ++
                    $("Gameplay" -> ("Async Options (" + setup.get(AsyncActions).?("yes").|("no").hl + ")")) ++
                    $("Variants" -> ("High Priests (" + setup.get(HighPriests).?("yes").|("no").hl + ")")) ++
                    $("Variants" -> ("Neutral".styled("neutral") + " spellbooks (" + setup.get(NeutralSpellbooks).?("yes").|("no").hl + ")")) ++
                    $("Variants" -> ("Neutral".styled("neutral") + " monsters (" + setup.get(NeutralMonsters).?("yes").|("no").hl + ")")) ++
                    // Monsters (alphabetical)
                    useWith(setup, DimensionalShamblerCard, UseDimensionalShamblers, setup.options.has(NeutralMonsters)) ++
                    useWith(setup, ElderThingCard, UseElderThing, setup.options.has(NeutralMonsters)) ++
                    useWith(setup, GhastCard, UseGhast, setup.options.has(NeutralMonsters)) ++
                    useWith(setup, AlbinoPenguinsCard, UseAlbinoPenguins, setup.options.has(NeutralMonsters)) ++
                    useWith(setup, GnorriCard, UseGnorri, setup.options.has(NeutralMonsters)) ++
                    useWith(setup, GugCard, UseGug, setup.options.has(NeutralMonsters)) ++
                    useWith(setup, InsectsFromShaggaiCard, UseInsectsFromShaggai, setup.options.has(NeutralMonsters)) ++
                    useWith(setup, LengSpiderCard, UseLengSpider, setup.options.has(NeutralMonsters)) ++
                    useWith(setup, MoonbeastCard, UseMoonbeast, setup.options.has(NeutralMonsters)) ++
                    useWith(setup, SatyrCard, UseSatyr, setup.options.has(NeutralMonsters)) ++
                    useWith(setup, ServitorCard, UseServitor, setup.options.has(NeutralMonsters)) ++
                    useWith(setup, ShantakCard, UseShantak, setup.options.has(NeutralMonsters)) ++
                    useWith(setup, StarVampireCard, UseStarVampire, setup.options.has(NeutralMonsters)) ++
                    useWith(setup, VoonithCard, UseVoonith, setup.options.has(NeutralMonsters)) ++
                    // Terrors (alphabetical)
                    $("Variants" -> ("Neutral".styled("neutral") + " terrors (" + setup.get(NeutralTerrors).?("yes").|("no").hl + ")")) ++
                    useWith(setup, BrownJenkinCard, UseBrownJenkin, setup.options.has(NeutralTerrors)) ++
                    useWith(setup, DholeCard, UseDhole, setup.options.has(NeutralTerrors)) ++
                    useWith(setup, ElderShoggothCard, UseElderShoggoth, setup.options.has(NeutralTerrors)) ++
                    useWith(setup, GreatRaceOfYithCard, UseGreatRaceOfYith, setup.options.has(NeutralTerrors)) ++
                    useWith(setup, HoundOfTindalosCard, UseHoundOfTindalos, setup.options.has(NeutralTerrors)) ++
                    useWith(setup, QuachilUttausCard, UseQuachilUttaus, setup.options.has(NeutralTerrors)) ++
                    useWith(setup, ShadowPharaohCard, UseShadowPharaoh, setup.options.has(NeutralTerrors)) ++
                    // iGOOs (alphabetical)
                    $("Variants" -> ("Independent Great Old Ones (" + setup.get(IGOOs).?("yes").|("no").hl + ")")) ++
                    useWith(setup, AbhothCard, UseAbhoth, setup.options.has(IGOOs)) ++
                    useWith(setup, AtlachNachaCard, UseAtlachNacha, setup.options.has(IGOOs)) ++
                    useWith(setup, AzathothIGOOCard, UseAzathothIGOO, setup.options.has(IGOOs)) ++
                    useWith(setup, BokrugCard, UseBokrug, setup.options.has(IGOOs)) ++
                    useWith(setup, ByatisCard, UseByatis, setup.options.has(IGOOs)) ++
                    useWith(setup, CthughaCard, UseCthugha, setup.options.has(IGOOs)) ++
                    useWith(setup, DaolothCard, UseDaoloth, setup.options.has(IGOOs)) ++
                    useWith(setup, FatherDagonCard, UseFatherDagon, setup.options.has(IGOOs)) ++
                    useWith(setup, GhatanotoaIGOOCard, UseGhatanotoaIGOO, setup.options.has(IGOOs)) ++
                    useWith(setup, GlaakiIGOOCard, UseGlaakiIGOO, setup.options.has(IGOOs)) ++
                    useWith(setup, MotherHydraCard, UseMotherHydra, setup.options.has(IGOOs)) ++
                    useWith(setup, NyogthaCard, UseNyogtha, setup.options.has(IGOOs)) ++
                    useWith(setup, BloatedWomanCard, UseBloatedWoman, setup.options.has(IGOOs)) ++
                    useWith(setup, TulzschaCard, UseTulzscha, setup.options.has(IGOOs)) ++
                    useWith(setup, YgolonacCard, UseYgolonac, setup.options.has(IGOOs)) ++
                    useWith(setup, YigCard, UseYig, setup.options.has(IGOOs)) ++
                    (factions.has(SL) && factions.has(WW))
                        .$("Variants" -> ("" + IceAge + " affects " + Lethargy + " (" + setup.get(IceAgeAffectsLethargy).?("yes").|("no").hl + ")")) ++
                    (factions.has(OW) && factions.num == 4)
                        .$("Variants" -> ("" + OW + " needs 10 Gates in 4-Player (" + setup.get(Opener4P10Gates).?("yes").|("no").hl + ")")) ++
                    (factions.has(OW))
                        .$("Variants" -> ("" + OW + " Cheap Mutants + Harder SBR (" + setup.get(OpenerCheapMutants).?("yes").|("no").hl + ") <span class='pointer' onclick='event.stopPropagation(); onExternalClick(\"OpenerCheapMutants\")'>?</span>")) ++
                    (factions.has(OW))
                        .$("Variants" -> ("" + OW + " Yog Curse Die + DC GOO ES (" + setup.get(OpenerYogCurseDie).?("yes").|("no").hl + ") <span class='pointer' onclick='event.stopPropagation(); onExternalClick(\"OpenerYogCurseDie\")'>?</span>")) ++
                    (factions.has(SL))
                        .$("Variants" -> ("" + DemandSacrifice + " requires " + Tsathoggua + " (" + setup.get(DemandTsathoggua).?("yes").|("no").hl + ")")) ++
                    (factions.has(SL))
                        .$("Variants" -> ("" + SL + " Different Spellbook Requirements (" + setup.get(SleeperEasierSBR).?("yes").|("no").hl + ") <span class='pointer' onclick='event.stopPropagation(); onExternalClick(\"SleeperEasierSBR\")'>?</span>")) ++
                    (factions.has(SL))
                        .$("Variants" -> ("" + SL + " Energy Nexus - Pre Battle (" + setup.get(SleeperEnergyNexusPreBattle).?("yes").|("no").hl + ") <span class='pointer' onclick='event.stopPropagation(); onExternalClick(\"SleeperEnergyNexusPreBattle\")'>?</span>")) ++
                    (factions.has(DS))
                        .$("Variants" -> ("" + DS + " Alternate Spellbooks (" + setup.get(DSAlternateSpellbooks).?("yes").|("no").hl + ") <span class='pointer' onclick='event.stopPropagation(); onExternalClick(\"DSAlternateSpellbooks\")'>?</span>")) ++
                    $("Map" -> ("Map Configuration (" + setup.options.of[MapOption].lastOption.?(_.toString.hl) + ")")) ++
                    $("Options" -> ("Dice rolls (" + setup.dice.?("auto").|("manual").hl + ")")) ++
                    $("Options" -> ("Elder Signs (" + setup.es.?("auto").|("manual").hl + ")")) ++
                    $("Options" -> ("Forced moves (" + setup.confirm.?("confirm").|("perform").hl + ")")) ++
                    $("Done" -> "Start game".styled("power")),
                    nn => {
                        var n = nn
                        if (n >= 0 && n < factions.num) {
                            setup.difficulty += factions(n) -> $(Human, Easy, Normal, Human).dropWhile(_ != setup.difficulty(factions(n))).drop(1).head
                            setupQuestions()
                        }
                        n -= factions.num
                        if (n >= 0 && n < seatings.num) {
                            setup.seating = seatings(n)
                            setupQuestions()
                        }
                        n -= seatings.num
                        if (n == 0) {
                            setup.toggle(GateDiplomacy)
                            setupQuestions()
                        }
                        n -= 1
                        if (n == 0) {
                            setup.toggle(AsyncActions)
                            setupQuestions()
                        }
                        n -= 1
                        if (n == 0) {
                            setup.toggle(HighPriests)
                            setupQuestions()
                        }
                        n -= 1
                        if (n == 0) {
                            setup.toggle(NeutralSpellbooks)
                            setupQuestions()
                        }
                        n -= 1
                        if (n == 0) {
                            setup.toggle(NeutralMonsters)

                            if (setup.options.has(NeutralMonsters))
                                setup.options ++= $(UseDimensionalShamblers, UseElderThing, UseGhast, UseAlbinoPenguins, UseGnorri, UseGug, UseInsectsFromShaggai, UseLengSpider, UseMoonbeast, UseSatyr, UseServitor, UseShantak, UseStarVampire, UseVoonith)
                            else
                                setup.options = setup.options.notOf[NeutralMonsterOption]

                            setupQuestions()
                        }
                        if (setup.options.has(NeutralMonsters)) {
                            // Monsters (alphabetical): DimensionalShambler, ElderThing, Ghast, AlbinoPenguins, Gnorri, Gug, InsectsFromShaggai, LengSpider, Moonbeast, Satyr, Servitor, Shantak, StarVampire, Voonith
                            n -= 1; if (n == 0) { setup.toggle(UseDimensionalShamblers); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseElderThing); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseGhast); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseAlbinoPenguins); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseGnorri); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseGug); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseInsectsFromShaggai); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseLengSpider); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseMoonbeast); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseSatyr); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseServitor); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseShantak); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseStarVampire); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseVoonith); setupQuestions() }
                        }
                        n -= 1
                        if (n == 0) {
                            setup.toggle(NeutralTerrors)
                            if (setup.options.has(NeutralTerrors))
                                setup.options ++= $(UseBrownJenkin, UseDhole, UseElderShoggoth, UseGreatRaceOfYith, UseHoundOfTindalos, UseQuachilUttaus, UseShadowPharaoh)
                            else
                                setup.options = setup.options.notOf[NeutralTerrorOption]
                            setupQuestions()
                        }
                        if (setup.options.has(NeutralTerrors)) {
                            // Terrors (alphabetical): BrownJenkin, Dhole, ElderShoggoth, GreatRaceOfYith, HoundOfTindalos, QuachilUttaus, ShadowPharaoh
                            n -= 1; if (n == 0) { setup.toggle(UseBrownJenkin); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseDhole); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseElderShoggoth); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseGreatRaceOfYith); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseHoundOfTindalos); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseQuachilUttaus); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseShadowPharaoh); setupQuestions() }
                        }
                        n -= 1
                        if (n == 0) {
                            setup.toggle(IGOOs)

                            if (setup.options.has(IGOOs))
                                setup.options ++= $(UseAbhoth, UseAtlachNacha, UseAzathothIGOO, UseBokrug, UseByatis, UseCthugha, UseDaoloth, UseFatherDagon, UseGhatanotoaIGOO, UseGlaakiIGOO, UseMotherHydra, UseNyogtha, UseBloatedWoman, UseTulzscha, UseYgolonac, UseYig)
                            else
                                setup.options = setup.options.notOf[IGOOOption]

                            setupQuestions()
                        }
                        if (setup.options.has(IGOOs)) {
                            // iGOOs (alphabetical): Abhoth, Atlach-Nacha, Azathoth, Bokrug, Byatis, Cthugha, Daoloth, Father Dagon, Ghatanothoa, Gla'aki, Mother Hydra, Nyogtha, The Bloated Woman, Tulzscha, Y'Golonac, Yig
                            n -= 1; if (n == 0) { setup.toggle(UseAbhoth); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseAtlachNacha); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseAzathothIGOO); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseBokrug); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseByatis); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseCthugha); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseDaoloth); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseFatherDagon); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseGhatanotoaIGOO); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseGlaakiIGOO); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseMotherHydra); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseNyogtha); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseBloatedWoman); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseTulzscha); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseYgolonac); setupQuestions() }
                            n -= 1; if (n == 0) { setup.toggle(UseYig); setupQuestions() }
                        }
                        if (factions.has(SL) && factions.has(WW)) {
                            n -= 1
                            if (n == 0) {
                                setup.toggle(IceAgeAffectsLethargy)
                                setupQuestions()
                            }
                        }
                        if (factions.has(OW) && factions.num == 4) {
                            n -= 1
                            if (n == 0) {
                                setup.toggle(Opener4P10Gates)
                                setupQuestions()
                            }
                        }
                        if (factions.has(OW)) {
                            n -= 1
                            if (n == 0) {
                                setup.toggle(OpenerCheapMutants)
                                setupQuestions()
                            }
                        }
                        if (factions.has(OW)) {
                            n -= 1
                            if (n == 0) {
                                setup.toggle(OpenerYogCurseDie)
                                setupQuestions()
                            }
                        }
                        if (factions.has(SL)) {
                            n -= 1
                            if (n == 0) {
                                setup.toggle(DemandTsathoggua)
                                setupQuestions()
                            }
                        }
                        if (factions.has(SL)) {
                            n -= 1
                            if (n == 0) {
                                setup.toggle(SleeperEasierSBR)
                                setupQuestions()
                            }
                        }
                        if (factions.has(SL)) {
                            n -= 1
                            if (n == 0) {
                                setup.toggle(SleeperEnergyNexusPreBattle)
                                setupQuestions()
                            }
                        }
                        if (factions.has(DS)) {
                            n -= 1
                            if (n == 0) {
                                setup.toggle(DSAlternateSpellbooks)
                                setupQuestions()
                            }
                        }
                        n -= 1
                        if (n == 0) {
                            val all = $(MapEarth33, MapEarth35, MapEarth53, MapEarth55, MapLibrary33, MapLibrary35, MapLibrary53, MapLibrary55)
                            setup.options = setup.options.notOf[MapOption] :+ (all.dropWhile(setup.options.of[MapOption].lastOption.has(_).not).drop(1) ++ all).first
                            setupQuestions()
                        }
                        n -= 1
                        if (n == 0) {
                            setup.dice = !setup.dice
                            setupQuestions()
                        }
                        n -= 1
                        if (n == 0) {
                            setup.es = !setup.es
                            setupQuestions()
                        }
                        n -= 1
                        if (n == 0) {
                            setup.confirm = !setup.confirm
                            setupQuestions()
                        }
                        n -= 1
                        if (n == 0)
                            startGame(setup)
                    }
                )
            }

            setupQuestions()
            askTop()
        }

        // Tombstalker (TS), Firstborn (FB), and Daemon Sultan (DS): included in the master faction list for game setup and replay parsing
        val allFactions = $(GC, CC, BG, YS, SL, WW, OW, AN, TS, FB, DS)

        // [2026-05-23] MNU alt faction picker. Replaces the combinations dropdown
        // for users who want to: (a) lock specific factions onto specific player
        // slots and/or limit which factions are eligible per slot before
        // randomizing, (b) name the players, (c) randomize seating order.
        // After Generate → Continue, the resulting `factions` list (already in
        // the desired seating order) is fed to startSetup, which builds the
        // standard Setup object and proceeds to the normal player-order +
        // game-flags + start flow.
        def altFactionPicker(pn : Int, onContinue : $[Faction] => Unit = startSetup) {
            clear(actionDiv)
            // Inject the alt-picker stylesheet once per page load — keyed by id
            // so subsequent picker entries don't duplicate it.
            if (dom.document.getElementById("alt-picker-style") == null) {
                val style = dom.document.createElement("style").asInstanceOf[html.Style]
                style.id = "alt-picker-style"
                style.innerHTML = """
                    @media (max-width: 720px) {
                        .alt-picker-cb-wrap { flex-basis: 100% !important; }
                    }
                """
                dom.document.head.appendChild(style)
            }
            val playerNames = scala.collection.mutable.ArrayBuffer.fill(pn)("")
            for (i <- 0 until pn) playerNames(i) = "Player " + (i + 1)
            val enabledFactions = scala.collection.mutable.ArrayBuffer.fill(pn)(scala.collection.mutable.Set[Faction]())
            for (i <- 0 until pn) allFactions.foreach(f => enabledFactions(i) += f)
            var randomizeOrder = false

            // [2026-05-31] Random Neutrals: optional random subset of Monsters /
            // Terrors / iGOOs to include in the game. Each category has an
            // enable checkbox and a count; on Continue we shuffle that
            // category's Use* option list, take the first N, add the umbrella
            // option (NeutralMonsters / NeutralTerrors / IGOOs) + the picked
            // Use* options to setup.options.
            val randMonsterPool : $[GameOption] = $(
                UseDimensionalShamblers, UseElderThing, UseGhast, UseAlbinoPenguins,
                UseGnorri, UseGug, UseInsectsFromShaggai, UseLengSpider, UseMoonbeast,
                UseSatyr, UseServitor, UseShantak, UseStarVampire, UseVoonith
            )
            val randTerrorPool : $[GameOption] = $(
                UseBrownJenkin, UseDhole, UseElderShoggoth, UseGreatRaceOfYith,
                UseHoundOfTindalos, UseQuachilUttaus, UseShadowPharaoh
            )
            val randIGOOPool : $[GameOption] = $(
                UseAbhoth, UseAtlachNacha, UseAzathothIGOO, UseBokrug, UseByatis,
                UseCthugha, UseDaoloth, UseFatherDagon, UseGhatanotoaIGOO,
                UseGlaakiIGOO, UseMotherHydra, UseNyogtha, UseBloatedWoman,
                UseTulzscha, UseYgolonac, UseYig
            )
            var randMonsters = false
            var randTerrors = false
            var randIGOOs = false
            // [2026-06-03] Auto-populate count fields based on player count
            // (pn). Previously hard-coded as 4/2/4 regardless of pn, which the
            // user noticed for pn=3 (got 4/4/2 — first two categories at 4,
            // third at 2 from the iGOO default the user remembered backwards).
            // [2026-06-07] Terrors default updated from max(1, pn-1) to pn so
            // all three categories match the player count (per user spec).
            // New formula:
            //   Monsters: pn       (one fresh monster per player)
            //   Terrors:  pn       (one fresh terror per player)
            //   iGOOs:    pn       (one fresh iGOO per player)
            // Floor at 1 so a 1-player setup still has a runnable default;
            // pool-cap clamping happens at Generate time, not here.
            var randMonsterCount = math.max(1, pn)
            var randTerrorCount = math.max(1, pn)
            var randIGOOCount = math.max(1, pn)

            // [2026-05-23] Per user: HIDE the other screen sections (map halves,
            // faction status panels, game log) for the duration of the alt
            // picker flow. Save their display values + the action panel's
            // outer container so they can be restored when the picker exits
            // (cancel / continue / back).
            val sectionsToHide = $("map-west", "map-east", "log", "status-1", "status-2", "status-3", "status-4", "status-5")
            val savedDisplays = scala.collection.mutable.Map[String, String]()
            val actionOuter = actionDiv.parentNode.parentNode.asInstanceOf[html.Element]
            val savedActionOuter = actionOuter.style.cssText
            def hideOtherSections() {
                sectionsToHide.foreach { id =>
                    val el = dom.document.getElementById(id)
                    if (el != null) {
                        val outer = el.parentNode.parentNode.asInstanceOf[html.Element]
                        savedDisplays(id) = outer.style.display
                        outer.style.display = "none"
                    }
                }
                // Expand action panel to fill the screen for the picker UI.
                actionOuter.style.cssText =
                    "position:absolute; left:0; top:0; width:100vw; height:100vh; " +
                    "left:0dvw; top:0dvh; width:100dvw; height:100dvh; background:#0d1117; z-index:100;"
            }
            def restoreOtherSections() {
                sectionsToHide.foreach { id =>
                    val el = dom.document.getElementById(id)
                    if (el != null) {
                        val outer = el.parentNode.parentNode.asInstanceOf[html.Element]
                        outer.style.display = savedDisplays.getOrElse(id, "")
                    }
                }
                actionOuter.style.cssText = savedActionOuter
            }
            hideOtherSections()

            val root = dom.document.createElement("div").asInstanceOf[html.Div]
            root.style.cssText =
                "padding:14px 16px; max-width:100%; box-sizing:border-box; height:100%; overflow-y:auto;" +
                "font-family:-apple-system,BlinkMacSystemFont,'Helvetica Neue',Arial,sans-serif;" +
                "color:#e6edf3;"

            // ── Lock logic + feasibility ─────────────────────────────────────
            // A faction is "locked-to" a player iff that player has exactly one
            // checked faction. Locked factions are forcibly disabled in every
            // OTHER player's row (greyed-out, can't be checked even if the user
            // tries). Cascade: when greying-out reduces another player's set
            // to size 1, THAT becomes a lock too. We recompute the lock map on
            // every render so the UI always reflects the current cascade.
            def computeLocks() : Map[Faction, Int] = {
                // Repeat until stable.
                val effective = scala.collection.mutable.ArrayBuffer[scala.collection.mutable.Set[Faction]]()
                for (i <- 0 until pn) effective += enabledFactions(i).clone()
                var locked : Map[Faction, Int] = Map()
                var changed = true
                while (changed) {
                    changed = false
                    for (i <- 0 until pn) {
                        val pool = effective(i) -- locked.keySet
                        if (pool.size == 1 && !locked.values.toSet.contains(i)) {
                            val f = pool.head
                            if (!locked.contains(f)) {
                                locked = locked + (f -> i)
                                changed = true
                            }
                        }
                    }
                }
                locked
            }

            // Hall's theorem via brute-force bipartite matching: does any
            // complete assignment of distinct factions to all players exist
            // under the current enabled sets? Players × factions are small.
            def isFeasible() : Boolean = {
                def search(remaining : List[Int], used : Set[Faction]) : Boolean = remaining match {
                    case Nil => true
                    case _ =>
                        // Pick the most-constrained player first for efficiency.
                        val p = remaining.minBy(i => (enabledFactions(i) -- used).size)
                        val pool = (enabledFactions(p) -- used).toList
                        if (pool.isEmpty) false
                        else pool.exists(f => search(remaining.filterNot(_ == p), used + f))
                }
                search((0 until pn).toList, Set.empty)
            }

            // For Generate: assign factions, processing players with FEWEST
            // eligible factions first (per user direction).
            def generateAssignment() : |[$[Faction]] = {
                val locked = computeLocks()
                // Players whose faction is forced by lock get it immediately.
                val assignment = scala.collection.mutable.Map[Int, Faction]()
                locked.foreach { case (f, i) => assignment(i) = f }

                val remaining = (0 until pn).toList.filterNot(assignment.contains)
                val usedFactions = scala.collection.mutable.Set[Faction]() ++ assignment.values

                // Backtrack to find ANY valid completion; randomize at each
                // branch so different runs produce different lineups.
                def backtrack(rem : List[Int]) : Boolean = rem match {
                    case Nil => true
                    case _ =>
                        val p = rem.minBy(i => (enabledFactions(i) -- usedFactions -- locked.keySet).size)
                        val pool = (enabledFactions(p).toList.diff(locked.keySet.toList).diff(usedFactions.toList))
                        if (pool.isEmpty) false
                        else {
                            val shuffled = scala.util.Random.shuffle(pool)
                            shuffled.find { f =>
                                usedFactions += f
                                assignment(p) = f
                                val ok = backtrack(rem.filterNot(_ == p))
                                if (!ok) { usedFactions -= f; assignment -= p }
                                ok
                            }.isDefined
                        }
                }
                if (!backtrack(remaining)) None
                else |((0 until pn).toList.map(assignment))
            }

            def glyphSrc(f : Faction) : String =
                "webp/images/" + f.short.toLowerCase + "-glyph.webp"

            def render() {
                clear(root)
                val title = dom.document.createElement("div").asInstanceOf[html.Div]
                title.style.cssText = "font-size:14pt;font-weight:600;margin-bottom:8px;color:#fff;"
                title.innerHTML = "Choose factions included for random selection"
                root.appendChild(title)

                val help = dom.document.createElement("div").asInstanceOf[html.Div]
                help.style.cssText = "font-size:9.5pt;color:#8c95a0;margin-bottom:16px;"
                help.innerHTML = "Type each player's name. Uncheck factions you don't want that player to be eligible for. When a player has only ONE faction checked, that faction is reserved for them — it's auto-greyed-out in every other player's row."
                root.appendChild(help)

                val locked = computeLocks()
                val lockedFactions = locked.keySet

                for (i <- 0 until pn) {
                    val row = dom.document.createElement("div").asInstanceOf[html.Div]
                    row.style.cssText =
                        "display:flex;flex-wrap:wrap;align-items:center;gap:6px 10px;" +
                        "padding:8px 6px;border-bottom:1px solid #2a2e36;margin-bottom:6px;"

                    val nameIn = dom.document.createElement("input").asInstanceOf[html.Input]
                    nameIn.`type` = "text"
                    nameIn.value = playerNames(i)
                    nameIn.style.cssText = "flex:0 0 140px;padding:5px 8px;background:#0d1117;border:1px solid #30363d;border-radius:5px;color:#e6edf3;font-family:inherit;font-size:10.5pt;"
                    nameIn.oninput = (_) => { playerNames(i) = nameIn.value }
                    row.appendChild(nameIn)

                    // [2026-05-23] Layout: the checkbox container has internal
                    // flex-wrap so individual checkboxes flow into multiple
                    // sub-rows when the container isn't wide enough. A media
                    // query on the .alt-picker-cb-wrap class (injected once via
                    // <style> below) forces flex-basis:100% at viewport widths
                    // < 720px, which drops the whole container beneath the name
                    // on narrow screens. On wider screens the container flows
                    // inline with the name and partial-wraps as needed.
                    val cbWrap = dom.document.createElement("div").asInstanceOf[html.Div]
                    cbWrap.className = "alt-picker-cb-wrap"
                    cbWrap.style.cssText = "display:flex;flex-wrap:wrap;align-items:center;gap:6px 10px;flex:1 1 auto;"

                    // "All" toggle for this row
                    val allLbl = dom.document.createElement("label").asInstanceOf[html.Label]
                    allLbl.style.cssText = "display:inline-flex;flex-direction:column;align-items:center;gap:2px;font-size:8.5pt;color:#8c95a0;"
                    val allCb = dom.document.createElement("input").asInstanceOf[html.Input]
                    allCb.`type` = "checkbox"
                    val unlockableForThisPlayer = allFactions.filter(f => !lockedFactions.contains(f) || locked(f) == i)
                    allCb.checked = unlockableForThisPlayer.forall(enabledFactions(i).contains)
                    allCb.style.cssText = "margin:0;cursor:pointer;"
                    allLbl.appendChild(newDiv("", "All"))
                    allLbl.appendChild(allCb)
                    cbWrap.appendChild(allLbl)

                    val factionCbs = scala.collection.mutable.ArrayBuffer[(Faction, html.Input)]()
                    allFactions.foreach { f =>
                        val lockedElsewhere = lockedFactions.contains(f) && locked(f) != i
                        val lbl = dom.document.createElement("label").asInstanceOf[html.Label]
                        lbl.style.cssText =
                            "display:inline-flex;flex-direction:column;align-items:center;gap:2px;font-size:8.5pt;color:#8c95a0;" +
                            (if (lockedElsewhere) "opacity:0.35;cursor:not-allowed;" else "")

                        // Glyph image instead of acronym text.
                        val glyph = dom.document.createElement("img").asInstanceOf[html.Image]
                        glyph.src = glyphSrc(f)
                        glyph.alt = f.short
                        glyph.style.cssText = "width:32px;height:32px;object-fit:contain;"
                        glyph.title = f.full

                        val cb = dom.document.createElement("input").asInstanceOf[html.Input]
                        cb.`type` = "checkbox"
                        cb.checked = enabledFactions(i).contains(f) && !lockedElsewhere
                        cb.disabled = lockedElsewhere
                        cb.style.cssText = "margin:0;cursor:" + (if (lockedElsewhere) "not-allowed" else "pointer") + ";"
                        cb.onchange = (_) => {
                            if (cb.checked) enabledFactions(i) += f else enabledFactions(i) -= f
                            render()  // re-cascade
                        }
                        lbl.appendChild(glyph)
                        lbl.appendChild(cb)
                        cbWrap.appendChild(lbl)
                        factionCbs += ((f, cb))
                    }
                    allCb.onchange = (_) => {
                        // [2026-05-23] All-toggle has clean semantics now:
                        //   ON  → enabledFactions(i) = allFactions minus those
                        //         locked to OTHER players (their lock wins).
                        //   OFF → enabledFactions(i) = empty.
                        // Never touches any other player's set. The locked-
                        // elsewhere check guarantees that flipping All on player
                        // B cannot dislodge player A's single-faction lock.
                        enabledFactions(i).clear()
                        if (allCb.checked) {
                            allFactions.foreach { f =>
                                if (!lockedFactions.contains(f) || locked(f) == i)
                                    enabledFactions(i) += f
                            }
                        }
                        render()
                    }

                    row.appendChild(cbWrap)
                    root.appendChild(row)
                }

                // [2026-05-31] Random Neutrals section. Three checkbox+count
                // rows under a small heading, one each for Monsters / Terrors /
                // iGOOs. Validation per row:
                //   count <= 0        → forcibly un-check that category
                //   count >  pool     → red "all units picked, will not be random"
                //   count >= pool     → red "Only X units available, choose less"
                //                         + disable Continue (rnHardBlock)
                var rnHardBlock = false
                val rnHeading = dom.document.createElement("div").asInstanceOf[html.Div]
                rnHeading.style.cssText = "margin-top:14px;font-size:12pt;font-weight:600;color:#fff;border-top:1px solid #2a2e36;padding-top:10px;"
                rnHeading.innerHTML = "Random Neutrals"
                root.appendChild(rnHeading)

                def renderRandRow(
                    label : String,
                    poolSize : Int,
                    getEnabled : () => Boolean,
                    setEnabled : Boolean => Unit,
                    getCount : () => Int,
                    setCount : Int => Unit
                ) {
                    val r = dom.document.createElement("div").asInstanceOf[html.Div]
                    r.style.cssText = "padding:6px 6px;display:flex;align-items:center;gap:8px;flex-wrap:wrap;"
                    val cb = dom.document.createElement("input").asInstanceOf[html.Input]
                    cb.`type` = "checkbox"
                    cb.checked = getEnabled()
                    cb.style.cssText = "margin:0;cursor:pointer;"
                    val lbl = dom.document.createElement("label").asInstanceOf[html.Label]
                    lbl.style.cssText = "color:#e6edf3;font-size:10.5pt;cursor:pointer;"
                    lbl.innerHTML = label + " (from " + poolSize + " total)"
                    lbl.onclick = (_) => { cb.checked = !cb.checked; setEnabled(cb.checked); render() }
                    cb.onchange = (_) => { setEnabled(cb.checked); render() }

                    // [2026-06-03] Stepper control replaces the previous
                    // free-text number input. Why: a free-text <input
                    // type="number"> here triggered render() on every
                    // keystroke (oninput), which clears `root` and rebuilds
                    // it — so the input lost focus after every digit. The
                    // user perceived this as "the page steals focus back to
                    // the canvas." Switching to explicit ▼ / ▲ buttons
                    // eliminates the typing path entirely: clicks don't
                    // need the input to retain focus, and there is no
                    // canvas-vs-input focus race. The value readout in the
                    // middle is a non-interactive <span>, not an editable
                    // input, so there's nothing left to steal focus from.
                    val stepWrap = dom.document.createElement("div").asInstanceOf[html.Div]
                    stepWrap.style.cssText = "display:inline-flex;align-items:center;gap:0;border:1px solid #30363d;border-radius:5px;background:#0d1117;overflow:hidden;"

                    val btnStyle =
                        "padding:4px 10px;background:#161b22;color:#e6edf3;border:none;" +
                        "cursor:pointer;font-family:inherit;font-size:11pt;font-weight:600;" +
                        "user-select:none;line-height:1;"

                    val downBtn = dom.document.createElement("button").asInstanceOf[html.Button]
                    downBtn.`type` = "button"
                    downBtn.innerHTML = "&#9660;"  // ▼
                    downBtn.style.cssText = btnStyle
                    downBtn.title = "Decrease"
                    downBtn.onclick = (_) => {
                        val n = math.max(0, getCount() - 1)
                        setCount(n)
                        if (n <= 0) setEnabled(false)
                        render()
                    }

                    val valSpan = dom.document.createElement("span").asInstanceOf[html.Span]
                    valSpan.style.cssText = "min-width:32px;padding:4px 8px;text-align:center;color:#e6edf3;font-family:inherit;font-size:10.5pt;background:#0d1117;border-left:1px solid #30363d;border-right:1px solid #30363d;"
                    valSpan.innerHTML = getCount().toString

                    val upBtn = dom.document.createElement("button").asInstanceOf[html.Button]
                    upBtn.`type` = "button"
                    upBtn.innerHTML = "&#9650;"  // ▲
                    upBtn.style.cssText = btnStyle
                    upBtn.title = "Increase"
                    upBtn.onclick = (_) => {
                        // Cap at poolSize — going over only produces a red
                        // warning, never useful values, so block at the
                        // ceiling for cleaner UX. Validation messages below
                        // still cover the legacy out-of-range cases (in case
                        // pn-derived defaults exceed the pool for very small
                        // pools).
                        val n = math.min(poolSize, getCount() + 1)
                        setCount(n)
                        render()
                    }

                    stepWrap.appendChild(downBtn)
                    stepWrap.appendChild(valSpan)
                    stepWrap.appendChild(upBtn)

                    r.appendChild(cb)
                    r.appendChild(lbl)
                    r.appendChild(stepWrap)
                    if (getEnabled()) {
                        val n = getCount()
                        if (n >= poolSize) {
                            val warn = dom.document.createElement("span").asInstanceOf[html.Span]
                            warn.style.cssText = "color:#f85149;font-size:10pt;font-weight:600;"
                            warn.innerHTML = "Only " + poolSize + " units available, choose a number less than " + poolSize
                            r.appendChild(warn)
                            rnHardBlock = true
                        }
                        else if (n > poolSize) {
                            val warn = dom.document.createElement("span").asInstanceOf[html.Span]
                            warn.style.cssText = "color:#f85149;font-size:10pt;font-weight:600;"
                            warn.innerHTML = "all units picked, will not be random"
                            r.appendChild(warn)
                        }
                    }
                    root.appendChild(r)
                }

                renderRandRow("Monsters", randMonsterPool.num,
                    () => randMonsters, v => randMonsters = v,
                    () => randMonsterCount, v => randMonsterCount = v)
                renderRandRow("Terrors", randTerrorPool.num,
                    () => randTerrors, v => randTerrors = v,
                    () => randTerrorCount, v => randTerrorCount = v)
                renderRandRow("iGOOs", randIGOOPool.num,
                    () => randIGOOs, v => randIGOOs = v,
                    () => randIGOOCount, v => randIGOOCount = v)

                // Randomize-order row
                val rndRow = dom.document.createElement("div").asInstanceOf[html.Div]
                rndRow.style.cssText = "padding:10px 6px;display:flex;align-items:center;gap:8px;margin-top:6px;"
                val rndCb = dom.document.createElement("input").asInstanceOf[html.Input]
                rndCb.`type` = "checkbox"
                rndCb.checked = randomizeOrder
                rndCb.style.cssText = "margin:0;cursor:pointer;"
                rndCb.onchange = (_) => { randomizeOrder = rndCb.checked }
                val rndLbl = dom.document.createElement("label").asInstanceOf[html.Label]
                rndLbl.style.cssText = "color:#e6edf3;font-size:10.5pt;cursor:pointer;"
                rndLbl.innerHTML = "Randomize player order"
                rndLbl.onclick = (_) => { rndCb.checked = !rndCb.checked; randomizeOrder = rndCb.checked }
                rndRow.appendChild(rndCb)
                rndRow.appendChild(rndLbl)
                root.appendChild(rndRow)

                // Feasibility warning + button row
                val feasible = isFeasible() && !rnHardBlock
                if (!feasible) {
                    val warn = dom.document.createElement("div").asInstanceOf[html.Div]
                    warn.style.cssText = "color:#f85149;font-weight:600;font-size:11pt;margin-top:10px;"
                    warn.innerHTML = "Not enough factions selected — some players will have no eligible faction. Widen the checkboxes."
                    root.appendChild(warn)
                }

                val btnRow = dom.document.createElement("div").asInstanceOf[html.Div]
                btnRow.style.cssText = "display:flex;gap:10px;margin-top:14px;flex-wrap:wrap;"
                val genBtn = dom.document.createElement("button").asInstanceOf[html.Button]
                genBtn.innerHTML = "Generate"
                genBtn.disabled = !feasible
                genBtn.style.cssText =
                    "padding:8px 18px;border:none;border-radius:6px;font-weight:600;font-size:11pt;" +
                    (if (feasible) "background:#2f81f7;color:#fff;cursor:pointer;"
                     else "background:#30363d;color:#7d8590;cursor:not-allowed;")
                genBtn.onclick = (_) => if (feasible) attemptGenerate()
                val backBtn = dom.document.createElement("button").asInstanceOf[html.Button]
                backBtn.innerHTML = "Back"
                backBtn.style.cssText = "padding:8px 18px;background:#21262d;color:#e6edf3;border:1px solid #30363d;border-radius:6px;cursor:pointer;font-size:11pt;"
                backBtn.onclick = (_) => { restoreOtherSections(); clear(actionDiv); dom.window.location.reload() }
                btnRow.appendChild(genBtn)
                btnRow.appendChild(backBtn)
                root.appendChild(btnRow)
            }

            def attemptGenerate() {
                generateAssignment() match {
                    case None =>
                        // Should be unreachable now that the button is disabled
                        // when infeasible, but keep the alert as belt-and-braces.
                        dom.window.alert("No valid assignment exists. Widen at least one player's faction set.")
                    case Some(picked) =>
                        // [2026-05-23] Keep (name, faction) paired through the
                        // shuffle. Previously the faction list was reshuffled
                        // independently while playerNames stayed at fixed
                        // indices — so the confirmation screen mixed up which
                        // name belonged to which faction. Build paired tuples
                        // first, then if randomizeOrder is on pick a valid
                        // seating and reorder the PAIRS so the factions match
                        // it (carrying each player name with its faction).
                        val pairs : $[(String, Faction)] =
                            picked.zipWithIndex.map { case (f, idx) => (playerNames(idx), f) }
                        val orderedPairs : $[(String, Faction)] =
                            if (randomizeOrder) {
                                val seatings = allSeatings(picked.toList)
                                if (seatings.nonEmpty) {
                                    val chosen = seatings(scala.util.Random.nextInt(seatings.size))
                                    // Reorder pairs to match the chosen seating's faction order.
                                    chosen.map(f => pairs.find(_._2 == f).get)
                                } else pairs
                            } else pairs
                        showConfirmation(orderedPairs)
                }
            }

            def showConfirmation(ordered : $[(String, Faction)]) {
                clear(actionDiv)
                val box = dom.document.createElement("div").asInstanceOf[html.Div]
                // [2026-05-23] Per user: each player is centered, name above
                // glyph, tight enough that the whole lineup + buttons fit on
                // one mobile screen. No indent, no border rows.
                box.style.cssText = "padding:10px 12px;color:#e6edf3;font-family:-apple-system,sans-serif;height:100%;display:flex;flex-direction:column;align-items:center;justify-content:flex-start;text-align:center;box-sizing:border-box;overflow-y:auto;"
                val ttl = dom.document.createElement("div").asInstanceOf[html.Div]
                ttl.style.cssText = "font-size:13pt;font-weight:600;margin-bottom:8px;color:#fff;text-align:center;"
                ttl.innerHTML = "Player seating order"
                box.appendChild(ttl)
                ordered.foreach { case (name, f) =>
                    val block = dom.document.createElement("div").asInstanceOf[html.Div]
                    block.style.cssText = "display:flex;flex-direction:column;align-items:center;justify-content:center;gap:4px;margin:6px 0;text-align:center;"
                    val nm = dom.document.createElement("div").asInstanceOf[html.Div]
                    nm.style.cssText = "font-size:15pt;font-weight:700;color:#fff;line-height:1.1;"
                    nm.innerHTML = name
                    block.appendChild(nm)
                    val gImg = dom.document.createElement("img").asInstanceOf[html.Image]
                    gImg.src = glyphSrc(f)
                    gImg.alt = f.full
                    gImg.title = f.full
                    gImg.style.cssText = "width:64px;height:64px;object-fit:contain;"
                    block.appendChild(gImg)
                    box.appendChild(block)
                }

                val br = dom.document.createElement("div").asInstanceOf[html.Div]
                br.style.cssText = "display:flex;gap:10px;margin-top:10px;flex-wrap:wrap;justify-content:center;"
                val cont = dom.document.createElement("button").asInstanceOf[html.Button]
                cont.innerHTML = "Continue"
                cont.style.cssText = "padding:8px 18px;background:#3fb950;color:#0d1117;border:none;border-radius:6px;font-weight:600;cursor:pointer;font-size:11pt;"
                cont.onclick = (_) => {
                    restoreOtherSections()
                    clear(actionDiv)
                    // [2026-05-31] Arm Random Neutrals closure (consumed by
                    // startSetup / startOnlineSetup right after they create
                    // their local Setup). For each enabled category, shuffle
                    // the pool, take the requested count, add umbrella +
                    // picks to setup.options.
                    pendingRandomNeutrals = Some({ setup =>
                        if (randMonsters && randMonsterCount > 0) {
                            val picks = scala.util.Random.shuffle(randMonsterPool.toList).take(randMonsterCount)
                            setup.options = (setup.options.notOf[NeutralMonsterOption]).but(NeutralMonsters)
                            setup.options ++= (NeutralMonsters +: picks)
                        }
                        if (randTerrors && randTerrorCount > 0) {
                            val picks = scala.util.Random.shuffle(randTerrorPool.toList).take(randTerrorCount)
                            setup.options = (setup.options.notOf[NeutralTerrorOption]).but(NeutralTerrors)
                            setup.options ++= (NeutralTerrors +: picks)
                        }
                        if (randIGOOs && randIGOOCount > 0) {
                            val picks = scala.util.Random.shuffle(randIGOOPool.toList).take(randIGOOCount)
                            setup.options = (setup.options.notOf[IGOOOption]).but(IGOOs)
                            setup.options ++= (IGOOs +: picks)
                        }
                    })
                    onContinue(ordered.map(_._2))
                }
                val backBtn = dom.document.createElement("button").asInstanceOf[html.Button]
                backBtn.innerHTML = "Back to picker"
                backBtn.style.cssText = "padding:8px 18px;background:#21262d;color:#e6edf3;border:1px solid #30363d;border-radius:6px;cursor:pointer;font-size:11pt;"
                backBtn.onclick = (_) => { clear(actionDiv); render(); actionDiv.appendChild(root) }
                br.appendChild(cont)
                br.appendChild(backBtn)
                box.appendChild(br)

                actionDiv.appendChild(box)
            }

            render()
            actionDiv.appendChild(root)
        }

        val replay = getElem("replay").?./(_.innerHTML).|("")

        if (replay.trim != "") {
            val entries = replay.split("\n").toList./(_.trim).%(_ != "")
            val nf = entries(0)
            val of = entries(1)
            val factions = of.split(" ")(0).split("-").toList./(Serialize.parseFaction)./(_.get)
            val options = of.split(" ").toList.drop(1)./(Serialize.parseGameOption)./(_.get)
            val setup = new Setup(factions, Recorded)
            setup.options = options
            log(nf)
            startGame(setup, entries.drop(2))
        }
        else
        if (hash != "") {
            get(server + "role/" + hash) { role =>
                val self = Serialize.parseFaction(role)

                self match {
                    case Some(f) => getElem("icon").asInstanceOf[html.Link].href = getAsset(f.style + "-glyph").src
                    case None =>
                }

                // [2026-05-24] Preserve the /mnu/ prefix when rewriting the URL.
                // Previously this always pushed /play/<hash>, which on refresh would
                // serve the root (Library) build — the user would then crash on any
                // MNU-specific action because Library's main.js doesn't know MNU units.
                {
                    val curPath = dom.window.location.pathname
                    val targetPath =
                        if (curPath.startsWith("/mnu/play/")) "/mnu/play/" + hash
                        else if (curPath.startsWith("/mnu/")) "/mnu/play/" + hash
                        else "/play/" + hash
                    val needsRewrite =
                        !curPath.startsWith("/play/") &&
                        !curPath.startsWith("/mnu/play/")
                    if (needsRewrite)
                        dom.window.history.pushState("initilaize", "", targetPath)
                }

                // Master role "$" loads the game as a spectator (self = None).
                // The downstream startGame path already supports a None self
                // ("Spectating" log line); the previous early-return for "$"
                // left the screen blank.
                {
                    get(server + "read/" + hash + "/0") { read =>
                        val logs = read.split("\n").toList

                        val oldVersion = logs(0)
                        if (oldVersion != version)
                            // 2026-05-30: this is purely informational — the game's recorded
                            // build version doesn't match the current build because we shipped a
                            // new version while the game was in flight. NOT an error. The
                            // `oldVersion @@ {...}` block below handles retro-version migration.
                            log("Game version updated mid-game (was " + oldVersion.hl + ")")

                        oldVersion @@ {
                            case "Cthulhu Wars Solo HRF 1.8"
                               | "Cthulhu Wars Solo HRF 1.9"
                               | "Cthulhu Wars Solo HRF 1.10"
                               | "Cthulhu Wars Solo HRF 1.11"
                               | "Cthulhu Wars Solo HRF 1.12"
                               | "Cthulhu Wars Solo HRF 1.13"
                               | "Cthulhu Wars Solo HRF 1.14"
                               | "Cthulhu Wars Solo HRF 1.15"
                               | "Cthulhu Wars Solo HRF 1.16"
                               | "Cthulhu Wars Solo HRF 1.17"
                            =>
                                val l = dom.document.location
                                val search = ("version=" + "retro") +: l.search.drop(1).split('&').$.%(_.startsWith("version").not).but("")
                                val url = l.origin + l.pathname + "?" + search.join("&") + l.hash
                                log("Reload: " + url)
                                dom.document.location.assign(url)
                                return

                            case _ =>
                        }

                        // Defensive guard: a normal game log has at least 3 header lines
                        // (version, name, factions/options). If a /create call lost the
                        // body mid-write the persisted log can have only 2 lines, and the
                        // logs(2) read further down will throw IndexOutOfBoundsException.
                        // Show a clear page-level error instead of an uncaught throw so the
                        // owner can act (recreate the game).
                        if (logs.size < 3) {
                            val err = dom.document.createElement("div")
                            err.asInstanceOf[html.Element].setAttribute("style",
                                "position:fixed; top:20px; left:20px; right:20px; padding:20px; " +
                                "background:#2a1f1f; border:2px solid #cc5555; color:#ffd6d6; " +
                                "font-family:monospace; font-size:11pt; z-index:9999; max-width:800px;")
                            err.innerHTML =
                                "<b>Game log header is incomplete.</b><br/>" +
                                "Only " + logs.size + " of the required 3 header lines were written. " +
                                "This game was created but lost data mid-write and cannot be loaded.<br/><br/>" +
                                "This game is unrecoverable and will need to be recreated."
                            dom.document.body.appendChild(err)
                            return
                        }

                        val title = dom.document.createElement("div")
                        title.innerHTML = s"""
                            <div style="
                                position: fixed;
                                left: 0;
                                top: 0;
                                z-index: 9999;
                                pointer-events: none;
                                padding: 0.4vmin 0.8vmin;
                                color: rgb(255, 255, 255);
                                font-size: 100%;
                                font-weight: bold;
                                filter: drop-shadow(0 0 6px black) drop-shadow(0 0 6px black) drop-shadow(0 0 6px black);
                                display: flex;
                                align-items: center;
                                flex-wrap: nowrap;
                                max-width: 95vw;
                            ">
                                    <span
                                        style="
                                            pointer-events: auto;
                                            cursor: pointer;
                                            display: inline-flex;
                                            align-items: center;
                                            position: relative;
                                            vertical-align: middle;
                                            flex-shrink: 0;
                                        "
                                        onclick="event.stopPropagation(); onExternalClick('RoA')"
                                        onpointerover="event.stopPropagation(); onExternalOver('RoA')"
                                        onpointerout="event.stopPropagation(); onExternalOut('RoA')">
                                        <img src="${Overlays.imageSource("roa-icon")}"
                                             style="height: max(2em, 5.3vmin); width: auto; display: block;"
                                             alt="RoA" />
                                        <span id="roa-cost-num"
                                              style="
                                                  position: absolute;
                                                  top: 50%;
                                                  left: 50%;
                                                  transform: translate(-50%, -50%);
                                                  color: white;
                                                  font-size: max(0.9em, 2.4vmin);
                                                  font-weight: bold;
                                                  line-height: 1;
                                                  text-shadow: 0 0 3px black, 0 0 3px black, 0 0 3px black;
                                                  pointer-events: none;
                                              ">5</span>
                                    </span>
                                    <span data-elem="text" style="white-space: nowrap; margin-left: 0.5em; overflow: hidden; text-overflow: ellipsis; min-width: 0;">
                                        ${logs(1)}
                                    </span>
                            </div>"""

                        dom.document.body.appendChild(title)

                        dom.document.title = logs(1) + " - Cthulhu Wars HRF"

                        def parseDifficulty(s : String) : |[Difficulty] = Serialize.parseSymbol(s)./~(_.as[Difficulty])

                        val factions = logs(2).split(" ")(0).split("/").toList./(_.split(":"))./(s => Serialize.parseFaction(s(0)).get -> parseDifficulty(s(1)).get)
                        val options = logs(2).split(" ").toList.drop(1)./(Serialize.parseGameOption)./(_.get)

                        log(logs(1).styled("nt"))
                        log(self./("Playing as " + _).|("Spectating"))

                        val setup = new Setup(factions.lefts, Recorded)
                        factions.foreach { case (f, d) => setup.difficulty += f -> d }
                        setup.options = options

                        startGame(setup, logs.drop(3), self)
                    }
                }
            }
        }
        else {
            def topMenu() {
                // Round 8: replaced hardcoded cwo.im URL with the page's own origin so the
                // "Online game" link goes to localhost when running locally. For production
                // (data-server set to a real backend URL), the link still goes to that URL.
                ask("Cthulhu Wars", $("Quick Game".hl, "Local Game".hl, redirect.?("<a href='" + origin + "' target='_blank'><div>" + "Online game".hl + "</div></a>").|("Online Game".hl), "Extra", "About", "Test", "Betas".hl).take(menu), {
                    case 998_0 =>
                        val setup = new Setup(randomSeating($(GC, BG, WW, OW)), Normal)
                        setup.difficulty += OW -> Debug
                        setup.options = $(MapEarth35, GateDiplomacy)
                        startGame(setup)
                    case 999_0 =>
                        val n = 1
                        val pn = n + 3
                        ask("Play as", allFactions./(_.full) :+ "Back", nf => {
                            if (nf < allFactions.num) {
                                val faction = allFactions(nf)
                                val combinations = allFactions.but(faction).combinations(pn - 1).toList
                                ask("Choose opponents", combinations./(_.mkString(", ")) :+ "Back", no => {
                                    if (no < combinations.num) {
                                        val opponents = combinations(no)
                                        val setup = new Setup(randomSeating(faction +: opponents), Normal)
                                        setup.difficulty += faction -> Human
                                        setup.options ++= $(QuickGame, MapEarth35)
                                        startGame(setup)
                                    }
                                    else
                                        topMenu()
                                })
                            }
                            else
                                topMenu()
                        })
                    case 0 =>
                        val faction = allFactions.shuffle.first
                        val combinations = allFactions.but(faction).combinations(3).$
                        val opponents = combinations.shuffle.first
                        val setup = new Setup(randomSeating(faction +: opponents), Normal)
                        setup.difficulty += faction -> Human
                        setup.options ++= $(QuickGame) ++ $(MapEarth35, MapEarth53).shuffle.take(1)
                        startGame(setup)
                    case 1 =>
                        ask("Players", ("3 Players".hl :: "4 Players".hl :: "5 Players".hl) :+ "Back", n => {
                            if (n < 3) {
                                val pn = n + 3
                                val combinations = allFactions.combinations(pn).toList
                                // [2026-05-23] Alt faction picker entry at top of the
                                // faction-selection list. Picking it opens the per-player
                                // glyph-checkbox UI; the rest of the list is the existing
                                // combinatorial dropdown for users who want the literal
                                // faction set.
                                val opts = (("Alt faction picker".hl) +: combinations./(_.mkString(", "))./(smaller).toList) :+ "Back"
                                ask("Choose factions", opts, n2 => {
                                    if (n2 == 0)
                                        altFactionPicker(pn)
                                    else if (n2 - 1 < combinations.num)
                                        startSetup(combinations(n2 - 1))
                                    else
                                        topMenu()
                                })
                            }
                            else
                                topMenu()
                        })
                    case 2 =>
                        ask("Players", ("3 Players".hl :: "4 Players".hl :: "5 Players".hl) :+ "Back", n => {
                            if (n < 3) {
                                val pn = n + 3
                                val combinations = allFactions.combinations(pn).toList
                                // [2026-05-23] Alt faction picker at top of online flow too.
                                val opts = (("Alt faction picker".hl) +: combinations./(_.mkString(", "))./(smaller).toList) :+ "Back"
                                ask("Choose factions", opts, n2 => {
                                    if (n2 == 0)
                                        altFactionPicker(pn, startOnlineSetup)
                                    else if (n2 - 1 < combinations.num)
                                        startOnlineSetup(combinations(n2 - 1))
                                    else
                                        topMenu()
                                })
                            }
                            else
                                topMenu()
                        })
                    case 3 => ask("Cthulhu Wars Extra", $("Survival mode".styled("kill"), "Download Offline Version".hl, "<a href='https://necronomicon.app/' target='_blank'><div>Necronomicon</div></a>", "<a href='https://cthulhuwars.fandom.com/' target='_blank'><div>Cthulhu Wars Strategy Wiki</div></a>", "Back"), {
                        case 0 =>
                            val base = allFactions.take(4)
                            ask("Choose faction", base./(f => f.full) :+ "Back", nf => {
                                if (nf < base.num) {
                                    val setup = new Setup(randomSeating(base), AllVsHuman)
                                    setup.difficulty += base(nf) -> Human
                                    startGame(setup)
                                }
                                else
                                    topMenu()
                            })
                        case 1 =>
                            val ir = hrf.html.ImageResources(Map(), Map(), hrf.HRF.imageCache)
                            val resources = hrf.html.Resources(ir, () => Map())
                            var game = new Game(EarthMap3, RitualTrack.for3, $(GC, CC, BG), true, $)

                            hrf.quine.Quine.save("cthulhu-wars-solo-" + BuildInfo.version, $, $, resources, $, new Serialize(game), "cthulhu-wars-solo-" + BuildInfo.version, false, "", topMenu())
                        case 2 | 3 | 4 =>
                            topMenu()
                    })
                    case 4 =>
                        ask("Cthulhu Wars Solo", $(
                            "<a href='https://boardgamegeek.com/filepage/152635/cthulhu-wars-solo-hrf-19' target='_blank'><div>Project Homepage</div></a>",
                            "Developed by " + "Haunt Roll Fail".hl,
                            "Additional AI programming by " + "ricedwlit".hl,
                            "Ancients, High Priests, Neutral Monsters, Independent Great Old Ones developed by " + "Legrasse81".hl,
                            "Board game by " + "Peterson Games".hl,
                            "All graphics in the app belong to Petersen Games.<br>Used with permission.",
                            "Back"
                        ), { _ => topMenu() })
                    case 6 =>
                        // "Beta builds" — links to all other builds.
                        // Colors: TT=faction pink, BB=faction gold, MNU=light grey, HB=rainbow per-letter
                        ask("Beta builds", $(
                            "<a href='/' target='_blank'><div>Library at Celaeno (main)</div></a>",
                            "<a href='/TchoTcho/' target='_blank'><div><span style='color:#fc9ca0'>TchoTcho</span></div></a>",
                            "<a href='/BB/' target='_blank'><div><span style='color:#c8a84b'>Bubastis</span></div></a>",
                            "<a href='/HB/' target='_blank'><div><span style='color:#ff4444'>H</span><span style='color:#ff8c00'>o</span><span style='color:#ffd700'>m</span><span style='color:#44cc44'>e</span><span style='color:#4488ff'>b</span><span style='color:#8844ff'>r</span><span style='color:#cc44cc'>e</span><span style='color:#ff4444'>w</span></div></a>",
                            "Cancel"
                        ), { _ => topMenu() })
                    case 5 =>
                        val setup = new Setup(randomSeating($(GC, BG, WW)), Normal)
                        setup.difficulty += GC -> Human
                        setup.difficulty += BG -> Normal
                        setup.difficulty += WW -> Debug
                        // setup.options = $(MapEarth53)
                        // setup.options = $(NeutralMonsters, UseGhast, UseGug, UseShantak, UseStarVampire)
                        setup.options = $(IGOOs, UseAbhoth, UseDaoloth, UseNyogtha)
                        //setup.options = $(HighPriests)
                        startGame(setup)
                    case 666 =>
                        val base = allFactions.take(4)
                        ask("Choose faction", base./(f => f.full) :+ "Back", nf => {
                            if (nf < base.num) {
                                val setup = new Setup(randomSeating(base), AllVsHuman)
                                setup.difficulty += base(nf) -> Human
                                startGame(setup)
                            }
                            else
                                topMenu()
                        })
                    case 777 =>
                        val setup = new Setup(randomSeating($(YS, GC, BG, AN)), Normal)
                        startGame(setup)
                    case 888 =>
                        val setup = new Setup(randomSeating($(GC, CC, YS, OW)), Normal)
                        setup.difficulty += OW -> Debug
                        startGame(setup)
                })
            }

            topMenu()

            if (quick) {
                val faction = allFactions.shuffle.first
                val combinations = allFactions.but(faction).combinations(3).$
                val opponents = combinations.shuffle.first
                val setup = new Setup(randomSeating(faction +: opponents), Normal)
                setup.difficulty += faction -> Human
                startGame(setup)
            }

        }
    }
}
