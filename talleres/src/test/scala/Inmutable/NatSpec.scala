package Inmutable

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

object NatSpec extends AnyFlatSpec with Matchers{
  "La funcion addNat enviando los naturales Suc(Cero) y Suc(Suc(Cero))" should "deberia devolver el resultado " +
    "Suc(Suc(Suc(Cero)))" in {
    val nat1 = Suc(Cero)
    val nat2 = Suc(Suc(Cero))
    Nat.addNat(nat1,nat2) shouldBe(Suc(Suc(Suc(Cero))))
  }

  "La funcion prodNat enviando los naturales Suc(Suc(Suc(Cero))) y Suc(Suc(Cero))" should "deberia devolver el resultado " +
    "Suc(Suc(Suc(Suc(Suc(Suc(Cero))))))" in {
    val nat1 = Suc(Suc(Suc(Cero)))
    val nat2 = Suc(Suc(Cero))
    Nat.prodNat(nat1,nat2) shouldBe(Suc(Suc(Suc(Suc(Suc(Suc(Cero)))))))
  }
}