package Functions

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.math.Pi

class Taller1Spec extends AnyFlatSpec with Matchers{
  "El area de un triangulo rectangulo de base y altura 2" should "es 2" in {
    val areaTrianguloRectangulo = (2) shouldEqual 2
  }

  "El area de un circulo con radio uno" should "es Pi" in {
    val areaCirculo = (1) shouldEqual Pi
  }
}
