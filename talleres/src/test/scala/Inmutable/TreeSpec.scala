package Inmutable

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

object TreeSpec extends AnyFlatSpec with Matchers{
  "La funcion size enviándole el árbol Leaf(10)" should "deberia devolver 1" in {
    Tree.size(Leaf(10)) shouldBe(1)
  }
  "La funcion size enviándole el árbol Branch(Leaf(10),Leaf(20))" should "deberia devolver 1" in {
    Tree.size(Branch(Leaf(10), Leaf(20))) shouldBe(3)
  }
}