package common

import cats.tests._
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import org.typelevel.discipline.scalatest.Discipline

package object test {

  trait Spec extends PropSpec with PropertyChecks with Matchers

  trait Test extends FlatSpec with Matchers

  trait CatsLawsSpec
      extends FunSuite
      with Matchers
      with Discipline
      with TestSettings
      with StrictCatsEquality
}
