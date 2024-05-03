ThisBuild / organization := "ch.epfl"
ThisBuild / version      := "2024"

ThisBuild / scalaVersion := "3.3.1"

val javaMemOptions = Seq("-Xss32M", "-Xms128M")

lazy val root = (project in file("."))
// Enable packaging of the L3 compiler so that it can be run without SBT.
// See documentation at https://www.scala-sbt.org/sbt-native-packager/
// Among the tasks added by this plugin, the most useful are:
// - "stage" to create the scripts locally in target/universal/stage/bin,
// - "dist" to create a Zip archive in target/universal.
  .enablePlugins(JavaAppPackaging)
  .settings(
    name := "l3c",

    scalacOptions ++= Seq("-feature",
                          "-deprecation",
                          "-unchecked",
                          "-encoding", "utf-8",
                          "-Wunused:all",
                          "-language:implicitConversions",
    ),

    // Main configuration
    Compile / scalaSource := baseDirectory.value / "src",
    libraryDependencies ++= Seq(
      "com.lihaoyi"   %% "fastparse"   % "3.0.2",
      "org.typelevel" %% "paiges-core" % "0.4.3"),

    fork := true,
    javaOptions ++= javaMemOptions,

    run / connectInput := true,
    run / outputStrategy := Some(StdoutOutput),

    // Test configuration
    Test / scalaSource := baseDirectory.value / "test",
    libraryDependencies += "com.lihaoyi" %% "utest" % "0.8.2" % "test",
    testFrameworks += new TestFramework("utest.runner.Framework"),

    // Packaging configuration (sbt-native-packager)
    Compile / packageDoc / mappings := Seq(),
    Universal / javaOptions ++= javaMemOptions.map("-J" + _))
