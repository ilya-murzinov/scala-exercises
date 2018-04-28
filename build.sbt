import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.4",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "scala-with-cats-exercises",
    libraryDependencies ++= Seq(
      cats,
      catsLaws,
      catsTestkit,
      scalaTest,
      scalaCheck,
      scalaCheckShapeless)
  )
