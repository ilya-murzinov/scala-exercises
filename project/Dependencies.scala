import sbt._

object Dependencies {
  lazy val cats = "org.typelevel" %% "cats-core" % "1.1.0"
  lazy val catsLaws = "org.typelevel" %% "cats-laws" % "1.0.1" % Test
  lazy val catsTestkit = "org.typelevel" %% "cats-testkit" % "1.0.1"% Test
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.5" % Test
  lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.14.0" % Test
  lazy val scalaCheckShapeless = "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.6" % Test
}
