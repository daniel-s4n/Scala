package Functions

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.math.Pi

class FuncionesSpec extends AnyFlatSpec with Matchers{
  // Ejercicio 1
  "La funcion areaTrianguloRectangulo enviando el lado de valor 4" should "es 8" in {
    Funciones.areaTrianguloRectangulo(4) shouldBe(8)
  }

  // Ejercicio 2
  "La función areaCirculo enviando el radio de valor 1" should "es Pi" in {
    Funciones.areaCirculo(1) shouldBe(Pi)
  }

  //Ejercicio 3
  "La función calSalario al enviarle devengado de 500000 y deducciones de 250000" should "debería devolver 250000 de " in{
    Funciones.calSalario(500000, 250000) shouldBe(250000)
  }

  // Ejercicio 4
  "La función calSalarioBono al enviarle devengado de 1000000 y deducciones de 370000" should "debería devolver 730000 " +
    "de salario" in {
    val devengado = 1000000
    val deducciones = 370000
    Funciones.calSalarioBono(devengado,deducciones) shouldBe(730000)
  }

  // Ejercicio 5
  "La función compSalario enviando la función calSalario, devengado de 700000 y deducciones de 150000" should "debería " +
    "devolver un salario de 550000" in {
    val devengado = 700000
    val deducciones = 150000
    Funciones.compSalario(Funciones.calSalario,devengado,deducciones) shouldBe(550000)
  }

  // Ejercicio 6
  "La función compSalario enviando la función calSalarioBono, devengado de 800000 y deducciones de 170000" should "debería " +
    "devolver salario de 710000" in {
    val devengado = 900000
    val deducciones = 150000
    Funciones.compSalario(Funciones.calSalarioBono,devengado,deducciones) shouldBe(840000)
  }

  // Ejercicio 7
  "La función genCalSalarioBono con el valor 1.05" should "debería devolver una función que cuando reciba de devengado " +
    "2000000 y de deducciones 300000 devuelva 1800000 de salario" in {
    val bono = 1.05
    val devengado = 2000000
    val deducciones = 300000
    val funcion = Funciones.genCalSalarioBono(bono)
    funcion(devengado, deducciones) shouldBe(1800000)
  }

  // Ejercicio 8
  "La función calSalario5 enviando devengado de 2000000 y deducciones de 300000" should "debería devolver salario de " +
    "180000" in {
    val devengado = 2000000
    val deducciones = 300000
    Funciones.calSalario5(devengado, deducciones) shouldBe(1800000)
  }

  // Ejercicio 8
  "La función calSalario20 enviando devengado de 2000000 y deducciones de 300000" should "debería devolver salario de " +
    "2100000" in {
    val devengado = 2000000
    val deducciones = 300000
    Funciones.calSalario20(devengado, deducciones) shouldBe(2100000)
  }

  // Ejercicio 9
  "La función calSalarioBonoClausura enviando devengado de 5000000 y deducciones de 635000 usando el valor de bono 1.15 " +
    "declarado fuera de la función" should "debería devolver 5115000 de salario" in {
    val devengado = 5000000
    val deducciones = 635000
    Funciones.calSalarioBonoClausura(devengado, deducciones) shouldBe(5115000)
  }

  // Ejercicio 11
  "La función calSalario15" should "debería devolver una función que al enviarle 4000000 de devengado y 500000 de " +
    "deducciones devuelva 4100000 de salario" in {
    val devengado = 4000000
    val deducciones = 500000
    val funcion = Funciones.calCalario15(devengado, deducciones)
    funcion shouldBe(4100000)
  }
}
