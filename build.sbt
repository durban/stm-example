
ThisBuild / scalaVersion := "2.13.7"
ThisBuild / semanticdbEnabled := true

val core = project.in(file("."))
  .settings(libraryDependencies ++= Seq(
    "dev.zio" %% "zio" % "2.0.0-RC1+53-9395d005-SNAPSHOT",
    "org.scalameta" %% "munit" % "0.7.29" % Test,
  ))
