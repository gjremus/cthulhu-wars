package cws.online

import slick.jdbc.HsqldbProfile.api._
import slick.jdbc.HsqldbProfile.api.DBIO.seq

import akka.actor.ActorSystem
import akka.http.scaladsl.{Http, HttpsConnectionContext}
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer

import ch.megard.akka.http.cors.scaladsl.CorsDirectives._

object CthulhuWarsOnline {
    def main(args : Array[String]) : Unit = {
        if (args.size != 4 && args.size != 5) {
            println("cwo <create|drop|run|drop-create-run> <database-name> <server-url> <port> [owner-admin-token]")
            return
        }

        val mode = args(0).split("-")
        val database = args(1)
        val url = args(2)
        val port = args(3).toInt
        // Optional 5th argv: owner admin token. Required for /admin/<token>/... endpoints
        // (lists games, reads any game log, owner-override rollback). Empty string disables.
        val ownerToken = if (args.size >= 5) args(4) else ""

        def readFile(path : String) = {
            import java.nio.charset.StandardCharsets._
            import java.nio.file.{Files, Paths}

            new String(Files.readAllBytes(Paths.get(path)), UTF_8)
        }

        def full = readFile("../solo/index.html").replace("###SERVER-URL###", url)

        implicit class Ascii(val s : String) {
            def ascii = s.filter(c => c >= 32 && c < 128)
        }

        case class Game(name : String, id : Option[Int] = None)

        class Games(tag : Tag) extends Table[Game](tag, "Games") {
            def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
            def name = column[String]("name")
            def * = (name, id.?).mapTo[Game]
        }

        val games = TableQuery[Games]
        val gamesId = games.returning(games.map(_.id))

        case class Role(gameId : Int, name : String, secret : String)

        class Roles(tag : Tag) extends Table[Role](tag, "Roles") {
            def gameId = column[Int]("gameId")
            def name = column[String]("name")
            def secret = column[String]("secret")
            def * = (gameId, name, secret).mapTo[Role]
            def pk = primaryKey("Roles" + "Key", (gameId, name))
            def game = foreignKey("Roles" + "Games", gameId, games)(_.id)
        }

        val roles = TableQuery[Roles]

        case class Log(gameId : Int, index : Int, role : String, value : String)

        class Logs(tag : Tag) extends Table[Log](tag, "Logs") {
            def gameId = column[Int]("gameId")
            def index = column[Int]("index")
            def role = column[String]("role")
            def value = column[String]("value")
            def * = (gameId, index, role, value).mapTo[Log]
            def pk = primaryKey("Logs" + "Key", (gameId, index))
            def game = foreignKey("Logs" + "Games", gameId, games)(_.id)
        }

        val logs = TableQuery[Logs]

        // Per-game last-write timestamp. Populated lazily on every write/rollback/edit.
        // Older games (no Meta row yet) report 0 → admin UI shows "—" for inactivity.
        case class Meta(gameId : Int, lastWriteMs : Long)

        class Metas(tag : Tag) extends Table[Meta](tag, "Meta") {
            def gameId = column[Int]("gameId", O.PrimaryKey)
            def lastWriteMs = column[Long]("lastWriteMs")
            def * = (gameId, lastWriteMs).mapTo[Meta]
        }

        val meta = TableQuery[Metas]

        // Per-game admin annotations: a "broken" flag and a free-text "notes" field.
        // These are NOT part of the game state / action log — they're purely owner-side
        // bookkeeping in the admin console (mark games that crashed; jot notes about
        // what went wrong). Stored in a separate table so a broken/notes update never
        // touches Logs and can't be mistaken for game state by the engine.
        case class AdminAnnotation(gameId : Int, broken : Boolean, notes : String)

        class AdminAnnotations(tag : Tag) extends Table[AdminAnnotation](tag, "AdminAnnotation") {
            def gameId = column[Int]("gameId", O.PrimaryKey)
            def broken = column[Boolean]("broken")
            def notes = column[String]("notes")
            def * = (gameId, broken, notes).mapTo[AdminAnnotation]
        }

        val adminAnnotations = TableQuery[AdminAnnotations]

        // Bot-game registry. Presence of a gameId in this table means the game
        // was created by the OnlineSimRunner orchestrator (not a real human
        // session). Admin UI hides these by default; the orchestrator
        // auto-deletes them on clean GameOver and retains them on crash.
        case class BotGame(gameId : Int)

        class BotGames(tag : Tag) extends Table[BotGame](tag, "BotGame") {
            def gameId = column[Int]("gameId", O.PrimaryKey)
            def * = (gameId).mapTo[BotGame]
        }

        val botGames = TableQuery[BotGames]

        val db = Database.forURL("jdbc:hsqldb:file:" + database, driver="org.hsqldb.jdbcDriver")

        object q {
            import scala.concurrent.Await
            import scala.concurrent.duration.Duration

            def apply[E <: Effect](actions : DBIOAction[_, NoStream, E]*) = Await.result(db.run(DBIO.seq(actions : _*).withPinnedSession), Duration.Inf)
            def apply[R](action : DBIOAction[R, NoStream, Effect.Read]) : R = Await.result(db.run(action.withPinnedSession), Duration.Inf)
        }

        if (mode.contains("drop")) {
            q(roles.schema.dropIfExists, logs.schema.dropIfExists, games.schema.dropIfExists)
        }

        if (mode.contains("create")) {
            q(games.schema.create, logs.schema.create, roles.schema.create)
        }

        // Always ensure Meta exists (lightweight, idempotent — survives old DBs
        // that were `create`d before this column landed).
        try {
            import slick.jdbc.HsqldbProfile.api.actionBasedSQLInterpolation
            q(sqlu"""CREATE TABLE IF NOT EXISTS "Meta" ("gameId" INTEGER PRIMARY KEY, "lastWriteMs" BIGINT NOT NULL)""")
        }
        catch {
            case e : Exception => println("Meta table init: " + e.getMessage)
        }

        // Same pattern for AdminAnnotation — idempotent, runs on every boot so
        // existing DBs get the new table without a drop/create cycle.
        try {
            import slick.jdbc.HsqldbProfile.api.actionBasedSQLInterpolation
            q(sqlu"""CREATE TABLE IF NOT EXISTS "AdminAnnotation" ("gameId" INTEGER PRIMARY KEY, "broken" BOOLEAN NOT NULL, "notes" LONGVARCHAR NOT NULL)""")
        }
        catch {
            case e : Exception => println("AdminAnnotation table init: " + e.getMessage)
        }

        // BotGame registry — same idempotent CREATE IF NOT EXISTS pattern.
        try {
            import slick.jdbc.HsqldbProfile.api.actionBasedSQLInterpolation
            q(sqlu"""CREATE TABLE IF NOT EXISTS "BotGame" ("gameId" INTEGER PRIMARY KEY)""")
        }
        catch {
            case e : Exception => println("BotGame table init: " + e.getMessage)
        }

        if (!mode.contains("run")) {
            return
        }

        def secret = {
            val random = new scala.util.Random()

            0.until(16).map(_ => "abcdefghijklmnopqrstuvwxyz".charAt(random.nextInt(26))).mkString("")
        }

        // Stamp this game's last-write timestamp. Called from every mutation endpoint.
        // Old rows that pre-date this column simply have no Meta entry → admin shows "—".
        def touchMeta(gameId : Int) : Unit = {
            val now = System.currentTimeMillis
            q(meta.filter(_.gameId === gameId).delete, meta += Meta(gameId, now))
        }

        implicit val system = ActorSystem()
        implicit val executionContext = system.dispatcher

        def htm(s : String) = complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, s))
        def jsx(s : String) = complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, s))
        def txt(s : String) = complete(HttpEntity(ContentTypes.`text/plain(UTF-8)`, s))
        def rdr(s : String) = redirect(s, StatusCodes.TemporaryRedirect)

        // encodeResponse wraps every response in gzip / deflate / brotli where the
        // client's Accept-Encoding allows. Static assets like main.js (now ~2.9 MB
        // after the fullLinkJS pass) compress ~3× more on the wire, dropping the
        // visible cold-load time by roughly the same factor. The directive is a
        // no-op for clients that don't advertise compression.
        val route = encodeResponse { cors() {
            (get & path("")) {
                htm(full)
            } ~
            pathPrefix("hrf") {
                getFromDirectory("../solo")
            } ~
            // FB Round 8: serve solo assets at the same relative paths the
            // index.html uses (webp/, fonts/, target/) so the unbuilt solo/index.html
            // works in online mode without needing assets baked into a single HTML.
            pathPrefix("webp") {
                getFromDirectory("../solo/webp")
            } ~
            pathPrefix("fonts") {
                getFromDirectory("../solo/fonts")
            } ~
            pathPrefix("target") {
                getFromDirectory("../solo/target")
            } ~
            pathPrefix("play") {
                // Assets resolved relative to /play/<id> end up at /play/<asset>/...,
                // not /play/<id>/<asset>/... — because <id> is treated as a filename
                // by the browser's relative URL resolution. Handle both layouts.
                pathPrefix("webp")   { getFromDirectory("../solo/webp") } ~
                pathPrefix("fonts")  { getFromDirectory("../solo/fonts") } ~
                pathPrefix("target") { getFromDirectory("../solo/target") } ~
                pathPrefix(Segment) { _ =>
                    pathPrefix("webp")   { getFromDirectory("../solo/webp") } ~
                    pathPrefix("fonts")  { getFromDirectory("../solo/fonts") } ~
                    pathPrefix("target") { getFromDirectory("../solo/target") } ~
                    pathEnd { htm(full) }
                } ~
                pathEnd { htm(full) }
            } ~
            (post & path("create")) {
                parameter("bot".as[Boolean].?(false)) { botFlag =>
                    decodeRequest {
                        entity(as[String]) { body =>
                            val ss = body.split("\n").toList.map(_.ascii)
                            val rls = List("$", "#") ++ ss(0).split(" ").toList
                            val name = ss(2)
                            val lgs = ss.drop(1)
                            val srs = rls.map(r => r -> secret).toMap
                            q((gamesId += Game(name)).flatMap(id => seq(
                                roles ++= rls.map(r => Role(id, r, srs(r))),
                                logs ++= lgs.zipWithIndex.map { case (l, n) => Log(id, n, "", l) }
                            )))
                            val newGameId = q(roles.filter(_.secret === srs("$")).map(_.gameId).result.head)
                            touchMeta(newGameId)
                            // bot=true: insert into BotGame in the same call, saving the orchestrator
                            // a separate /admin/mark-bot HTTP roundtrip per game.
                            if (botFlag)
                                try { q(botGames += BotGame(newGameId)) }
                                catch { case _ : Throwable => () }
                            txt(srs("$"))
                        }
                    }
                }
            } ~
            (get & path("roles" / Segment)) { role =>
                val list = q(roles.filter(_.secret === role).filter(_.name === "$").map(_.gameId).result.head.flatMap { id =>
                    roles.filter(_.gameId === id).result
                })
                txt(list.map(r => r.name + " " + r.secret).mkString("\n"))
            } ~
            (get & path("role" / Segment)) { role =>
                val name = q(roles.filter(_.secret === role).map(_.name).result.head)
                txt(name)
            } ~
            (get & path("read" / Segment / IntNumber)) { (role, from) =>
                val log = q(roles.filter(_.secret === role).map(_.gameId).result.head.flatMap { id =>
                        logs.filter(_.gameId === id).filter(_.index >= from).map(_.value).result
                })
                txt(log.mkString("\n"))
            } ~
            (post & path("write" / Segment / IntNumber)) { (role, index) =>
                decodeRequest {
                    entity(as[String]) { body =>
                        val ss = body.split("\n").toList.map(_.ascii)

                        try {
                            // Original atomic shape — single composed DBIO with pinned session
                            // (the prior multi-query refactor + touchMeta broke /write atomicity
                            // for online games and is the suspected cause of the Thousand Forms
                            // online-only regression). Meta last-write tracking still happens on
                            // /create and admin-side endpoints; for /write we now just append the
                            // log rows and let touchMeta run after, OUTSIDE the atomic block, so
                            // any meta failure does not abort the log write.
                            q(roles.filter(_.secret === role).filter(_.name =!= "#").map(r => (r.name, r.gameId)).result.head.flatMap { case (name, id) =>
                                logs ++= 0.until(ss.size).map(n => Log(id, index + n, name, ss(n)))
                            })
                            try { touchMeta(q(roles.filter(_.secret === role).filter(_.name =!= "#").map(_.gameId).result.head)) }
                            catch { case _ : Throwable => () }
                            complete(StatusCodes.Accepted)
                        }
                        catch {
                            case e : java.sql.SQLIntegrityConstraintViolationException => complete(StatusCodes.Conflict)
                        }
                    }
                }
            } ~
            (post & path("rollback-v2" / Segment / IntNumber)) { (role, index) =>
                // Atomic: composed action, then optional meta touch (best-effort).
                q(roles.filter(_.secret === role).map(_.gameId).result.head.flatMap { id =>
                    logs.filter(_.gameId === id).filter(_.index >= index).delete
                })
                try { touchMeta(q(roles.filter(_.secret === role).map(_.gameId).result.head)) }
                catch { case _ : Throwable => () }
                complete(StatusCodes.Accepted)
            } ~
            // ── ADMIN ENDPOINTS (owner-only) ──
            // Gated by ownerToken matching the 5th argv. If ownerToken is empty (not
            // configured), every admin endpoint returns 404 so it's invisible.
            // Serve the admin UI as a static file at /admin.html. Reads from
            // ../solo/admin.html on the VM (which we upload separately).
            (get & path("admin.html")) {
                getFromFile("../solo/admin.html")
            } ~
            // Serve the MNU beta build under /mnu/. Assets in ../mnu/ on the VM.
            pathPrefix("mnu") {
                pathPrefix("webp")   { getFromDirectory("../mnu/webp") } ~
                pathPrefix("fonts")  { getFromDirectory("../mnu/fonts") } ~
                pathPrefix("target") { getFromDirectory("../mnu/target") } ~
                pathEnd { getFromFile("../mnu/index.html") } ~
                path("") { getFromFile("../mnu/index.html") } ~
                pathPrefix(Segment) { _ =>
                    pathPrefix("webp")   { getFromDirectory("../mnu/webp") } ~
                    pathPrefix("fonts")  { getFromDirectory("../mnu/fonts") } ~
                    pathPrefix("target") { getFromDirectory("../mnu/target") } ~
                    pathEnd { getFromFile("../mnu/index.html") }
                }
            } ~
            (get & path("admin" / Segment / "games")) { token =>
                if (ownerToken.isEmpty || token != ownerToken) complete(StatusCodes.NotFound)
                else {
                    // Tab-separated columns: id, name, masterSecret, lastWriteMs, broken (0|1), completed (0|1).
                    // Notes are NOT included here — fetched on row select via /admin/<t>/annotation
                    // so the list endpoint stays compact for long game lists.
                    val rows = q(games.join(roles).on(_.id === _.gameId)
                        .filter(_._2.name === "$")
                        .joinLeft(meta).on(_._1.id === _.gameId)
                        .joinLeft(adminAnnotations).on(_._1._1.id === _.gameId)
                        .map { case (((g, r), m), a) => (g.id, g.name, r.secret, m.map(_.lastWriteMs), a.map(_.broken)) }
                        .result)
                    // Detect completion per game: any log row whose value contains
                    // "GameOverPhaseAction" means the engine reached the end-of-game phase.
                    // One query per game keeps the per-row computation simple; for the
                    // current game counts (< 200) this is fast.
                    val completed = rows.map(_._1).map { id =>
                        val n = q(logs.filter(_.gameId === id).filter(_.value.like("%GameOverPhaseAction%")).length.result)
                        id -> (n > 0)
                    }.toMap
                    val botFlagged = q(botGames.map(_.gameId).result).toSet
                    txt(rows.map { case (id, name, secret, lastMs, broken) =>
                        val b = if (broken.getOrElse(false)) "1" else "0"
                        val c = if (completed.getOrElse(id, false)) "1" else "0"
                        val ib = if (botFlagged.contains(id)) "1" else "0"
                        s"$id\t$name\t$secret\t${lastMs.getOrElse(0L)}\t$b\t$c\t$ib"
                    }.mkString("\n"))
                }
            } ~
            // Mark a game as bot-created. The OnlineSimRunner orchestrator calls this
            // immediately after /create so the admin UI can filter the game out by
            // default and the orchestrator can later POST /admin/<t>/delete on clean
            // game-over.
            (post & path("admin" / Segment / "mark-bot" / IntNumber)) { (token, gameId) =>
                if (ownerToken.isEmpty || token != ownerToken) complete(StatusCodes.NotFound)
                else {
                    q(botGames.filter(_.gameId === gameId).delete, botGames += BotGame(gameId))
                    complete(StatusCodes.Accepted)
                }
            } ~
            (get & path("admin" / Segment / "annotation" / IntNumber)) { (token, gameId) =>
                if (ownerToken.isEmpty || token != ownerToken) complete(StatusCodes.NotFound)
                else {
                    val row = q(adminAnnotations.filter(_.gameId === gameId).result.headOption)
                    val (broken, notes) = row.map(r => (r.broken, r.notes)).getOrElse((false, ""))
                    txt((if (broken) "1" else "0") + "\t" + notes)
                }
            } ~
            (post & path("admin" / Segment / "annotation" / IntNumber)) { (token, gameId) =>
                if (ownerToken.isEmpty || token != ownerToken) complete(StatusCodes.NotFound)
                else {
                    decodeRequest {
                        entity(as[String]) { body =>
                            // Body: "broken\tnotes" where broken is "0" or "1" and notes is
                            // the remaining string (newlines and tabs preserved verbatim).
                            val tab = body.indexOf('\t')
                            val (brokenStr, notes) =
                                if (tab >= 0) (body.substring(0, tab), body.substring(tab + 1))
                                else (body, "")
                            val broken = brokenStr.trim == "1"
                            q(
                                adminAnnotations.filter(_.gameId === gameId).delete,
                                adminAnnotations += AdminAnnotation(gameId, broken, notes)
                            )
                            complete(StatusCodes.Accepted)
                        }
                    }
                }
            } ~
            (get & path("admin" / Segment / "roles" / IntNumber)) { (token, gameId) =>
                if (ownerToken.isEmpty || token != ownerToken) complete(StatusCodes.NotFound)
                else {
                    val rs = q(roles.filter(_.gameId === gameId).result)
                    txt(rs.map(r => s"${r.name}\t${r.secret}").mkString("\n"))
                }
            } ~
            (get & path("admin" / Segment / "log" / IntNumber)) { (token, gameId) =>
                if (ownerToken.isEmpty || token != ownerToken) complete(StatusCodes.NotFound)
                else {
                    val entries = q(logs.filter(_.gameId === gameId).sortBy(_.index).result)
                    txt(entries.map(l => s"${l.index}\t${l.role}\t${l.value}").mkString("\n"))
                }
            } ~
            (post & path("admin" / Segment / "rollback" / IntNumber / IntNumber)) { (token, gameId, index) =>
                if (ownerToken.isEmpty || token != ownerToken) complete(StatusCodes.NotFound)
                else {
                    q(logs.filter(_.gameId === gameId).filter(_.index >= index).delete)
                    touchMeta(gameId)
                    complete(StatusCodes.Accepted)
                }
            } ~
            (post & path("admin" / Segment / "write" / IntNumber / IntNumber)) { (token, gameId, index) =>
                if (ownerToken.isEmpty || token != ownerToken) complete(StatusCodes.NotFound)
                else {
                    decodeRequest {
                        entity(as[String]) { body =>
                            val ss = body.split("\n").toList.map(_.ascii)
                            try {
                                q(logs ++= 0.until(ss.size).map(n => Log(gameId, index + n, "$", ss(n))))
                                touchMeta(gameId)
                                complete(StatusCodes.Accepted)
                            }
                            catch {
                                case e : java.sql.SQLIntegrityConstraintViolationException => complete(StatusCodes.Conflict)
                            }
                        }
                    }
                }
            } ~
            // Admin: hard-delete a game (logs + roles + meta + annotation + game row). Requires owner token.
            (post & path("admin" / Segment / "delete" / IntNumber)) { (token, gameId) =>
                if (ownerToken.isEmpty || token != ownerToken) complete(StatusCodes.NotFound)
                else {
                    // FK order: logs and roles reference games(id), so delete them first.
                    q(
                        logs.filter(_.gameId === gameId).delete,
                        roles.filter(_.gameId === gameId).delete,
                        meta.filter(_.gameId === gameId).delete,
                        adminAnnotations.filter(_.gameId === gameId).delete,
                        botGames.filter(_.gameId === gameId).delete,
                        games.filter(_.id === gameId).delete
                    )
                    complete(StatusCodes.Accepted)
                }
            } ~
            // Admin: edit a single existing log entry's value (used by the admin UI's
            // "force same outcome" flow — owner rolls back past a dice roll, then
            // edits the original line back to the same value before continuing).
            (post & path("admin" / Segment / "edit" / IntNumber / IntNumber)) { (token, gameId, index) =>
                if (ownerToken.isEmpty || token != ownerToken) complete(StatusCodes.NotFound)
                else {
                    decodeRequest {
                        entity(as[String]) { body =>
                            val newValue = body.split("\n").headOption.map(_.ascii).getOrElse("")
                            q(logs.filter(_.gameId === gameId).filter(_.index === index).map(_.value).update(newValue))
                            touchMeta(gameId)
                            complete(StatusCodes.Accepted)
                        }
                    }
                }
            } ~
            // Engine-replay validation of a game's full log. Used by the admin UI's
            // Edit-ES flow to confirm an edited value doesn't break the game.
            // Shells to OnlineSimRunner --validate-game which runs the full engine
            // replay. Synchronous; returns 200 + "VALID actions=N" on clean replay,
            // 422 + "INVALID idx error" on first failure. May take 5–30s depending
            // on game length.
            (post & path("admin" / Segment / "validate-game" / IntNumber)) { (token, gameId) =>
                if (ownerToken.isEmpty || token != ownerToken) complete(StatusCodes.NotFound)
                else {
                    import scala.sys.process._
                    val cmd = Seq("java", "-cp", "/opt/cwo/server/online-sim.jar",
                        "cws.OnlineSimRunner", "--validate-game",
                        "http://localhost:" + port, ownerToken, gameId.toString)
                    val out = new StringBuilder
                    val exitCode = Process(cmd).!(ProcessLogger(line => out.append(line + "\n")))
                    val msg = out.toString.trim
                    if (exitCode == 0) complete(StatusCodes.OK -> msg)
                    else complete(StatusCodes.UnprocessableEntity -> msg)
                }
            }
        } }  // close encodeResponse + cors

        val bindingFuture = Http().newServerAt("0.0.0.0", port).bind(route)

        while (true) Thread.sleep(1000)

        bindingFuture.flatMap(_.unbind()).onComplete(_ => system.terminate())
    }
}
