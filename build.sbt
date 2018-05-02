import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization  := "com.example",
      scalaVersion  := "2.12.4",
      version       := "0.1.0-SNAPSHOT",
      scalacOptions ++= Seq(
        "-Ypartial-unification",
        "-language:higherKinds")
    )),
    name := "scala-with-cats-exercises",
    libraryDependencies ++= Seq(
      kindProjector,
      cats,
      catsLaws,
      catsTestkit,
      scalaTest,
      scalaCheck)
  )
