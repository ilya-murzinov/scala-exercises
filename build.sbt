import Dependencies._

lazy val commonSettings: Seq[Setting[_]] = Seq(
  organization := "com.github.ilyamurzinov",
  scalaVersion := "2.12.4",
  version := "0.1.0-SNAPSHOT",
  scalacOptions ++= Seq("-Ypartial-unification", "-language:higherKinds"),
  libraryDependencies ++= allDependencies,
)

lazy val allDependencies =
  Seq(kindProjector, cats, catsLaws, catsTestkit, scalaTest, scalaCheck)

lazy val root = (project in file("."))
  .settings(commonSettings)
  .settings(name := "scala-exercises")
  .aggregate(`scala-with-cats`, `junk-yard`)
  .dependsOn(`scala-with-cats`, `junk-yard`)

lazy val common = (project in file("modules/common"))
  .settings(commonSettings)
  .settings(name := "common")

lazy val `scala-with-cats` = (project in file("modules/scala-with-cats"))
  .settings(commonSettings)
  .settings(name := "scala-with-cats")
  .dependsOn(common % "compile->compile;test->test")

lazy val `junk-yard` = (project in file("modules/junk-yard"))
  .settings(commonSettings)
  .settings(name := "junk-yard")
  .dependsOn(common % "compile->compile;test->test")
