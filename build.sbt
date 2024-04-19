val scala3Version = "3.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "pps-code-scala",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,
    javacOptions ++= Seq("-source", "17.0", "-target", "17.0"),

    libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.3" % Test
  )
