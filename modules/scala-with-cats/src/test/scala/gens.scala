import org.scalacheck._

object GenCat {
  implicit val genCat: Arbitrary[Cat] = Arbitrary(for {
    name <- Gen.alphaStr
    age <- Gen.chooseNum(1, 30)
    color <- Gen.alphaStr
  } yield Cat(name, age, color))
}

object GenBox {
  implicit def genBox[A: Arbitrary]: Arbitrary[Box[A]] =
    Arbitrary(Arbitrary.arbitrary[A].map(Box(_)))
}

object GenTree {
  def genLeaf[A: Arbitrary]: Gen[Tree[A]] =
    for {
      e <- Arbitrary.arbitrary[A]
    } yield Leaf(e)

  def genBranch[A: Arbitrary]: Gen[Tree[A]] =
    for {
      l <- Gen.sized(h => Gen.resize(h / 2, genTree[A]))
      r <- Gen.sized(h => Gen.resize(h / 2, genTree[A]))
    } yield Branch(l, r)

  def genTree[A: Arbitrary]: Gen[Tree[A]] = Gen.sized { height =>
    if (height <= 0)
      genLeaf[A]
    else
      Gen.oneOf(genBranch[A], genLeaf[A])
  }

  implicit def arbTree[A: Arbitrary]: Arbitrary[Tree[A]] =
    Arbitrary(genTree[A])
}

object GenMap {
  val genMap: Gen[Map[String, Int]] =
    for {
      keys <- Gen.containerOf[List, String](Gen.alphaStr.filterNot(_.isEmpty))
      values <- Gen.containerOfN[List, Int](keys.size, Gen.posNum[Int])
    } yield keys.zip(values).toMap

  val genMaps: Gen[(Map[String, Int], Map[String, Int])] =
    for {
      m1 <- genMap
      m2 <- genMap
    } yield (m1, m2)
}
