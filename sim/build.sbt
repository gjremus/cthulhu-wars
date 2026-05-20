name := "Cthulhu Wars FB Sim"

version := "1.0"

scalaVersion := "2.13.16"

scalacOptions := Seq(
    "-unchecked",
    "-deprecation",
    "-feature",
    "-language:postfixOps",
    "-language:implicitConversions",
    "-Xlint:infer-any",
    "-Wconf:" + List(
        "will become a keyword",
        "procedure syntax",
        "match may not be exhaustive",
        "unreachable code",
    ).map("msg=" + _ + ":s").mkString(",")
)

val soloDir   = file("/Users/gremus/cthulhu-wars More Neutral Units/solo")
val baseDir   = soloDir / "base"
val simSrcDir = file("/Users/gremus/cthulhu-wars More Neutral Units/sim/src")

Compile / unmanagedSourceDirectories := Seq(soloDir, baseDir, simSrcDir)

val jsSpecific = Set(
    "ReflectJS.scala",
    "StatsStub.scala",
    "web.scala",
    "loader.scala",
    "utils.canvas.scala",
    "CthulhuWarsSolo.scala",
    "overlay.scala",
    "hrf.scala",
    "quine.scala",
    "resources.scala",
    "GlyphPlacement.scala",
)

Compile / unmanagedSources / excludeFilter := new SimpleFileFilter(f =>
    jsSpecific(f.getName) || f.getAbsolutePath.contains("/target/")
)

libraryDependencies += "com.lihaoyi" %% "fastparse" % "3.0.2"
libraryDependencies += "com.lihaoyi" %% "pprint" % "0.7.0"
libraryDependencies += "com.lihaoyi" %% "fansi" % "0.4.0"
libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"

Compile / sourceGenerators += Def.task {
    val file = (Compile / sourceManaged).value / "info.scala"
    IO.write(file, """package hrf { object BuildInfo { val name = "%s" ; val version = "%s" ; val time = %d ; val seed = "%s" } }""".stripMargin.format(name.value, version.value, System.currentTimeMillis % (24 * 60 * 60 * 1000), scala.util.Random.alphanumeric.take(16)))
    Seq(file)
}.taskValue

Compile / mainClass := Some("cws.SimRunner")

bspEnabled := false

maxErrors := 5
