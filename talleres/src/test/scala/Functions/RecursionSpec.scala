package Functions

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RecursionSpec extends AnyFlatSpec with Matchers{
  // Ejercicio 16
  "La funcion factorial al recibir el entero 5" should "debería devolver su factorial 120" in {
    Recursion.factorial(5) shouldBe(120)
  }

  // Ejercicio 15-17
  "La función fibonacci enviando la posición 7" should "debería devolver el número 13" in {
    Recursion.fibonacci(7) shouldBe(13)
  }
}
