package Functions
import scala.math.Pi

object Funciones extends App{
  // Ejercicio 1
  val areaTrianguloRectangulo = ( a:Int ) => (a*a) / 2

  // Ejercicio 2
  val areaCirculo = new Function1 [Double, Double] {
    def apply(r:Double) = Pi * (r*r)
  }

  //Ejercicio 3
  val calSalario = (devengado:Double, deducciones:Double) => devengado - deducciones
  //Ejercicio 4
  val calSalarioBono = (devengado:Double, deducciones:Double) => devengado * 1.10 - deducciones

  //Ejercicio 5
  def compSalario( salario:(Double, Double) => Double, devengado:Double, deducciones:Double ) {
    salario(devengado, deducciones)
  }

  // Ejercicio 6
  def genCalSalarioBono(bono:Double): (Double, Double)=>Double = {
    (devengado: Double, deduccion: Double) => devengado * bono - deduccion
  }

  // Ejercicio 7
  def calSalario5(devengado:Double, deduccion:Double):Double = {
    val salario: (Double, Double) => Double = genCalSalarioBono(1.05)
    salario(devengado, deduccion)
  }

  // Ejercicio 8
  def calSalario20(devengado:Double, deduccion:Double):Double = {
    val salario: (Double, Double) => Double = genCalSalarioBono(1.2)
    salario(devengado, deduccion)
  }

  // Ejercicio 9
  val bono = 1
  def calSalarioBonoClausura(devengados:Double, deducciones:Double): Double = {
    devengados * bono - deducciones
  }

  //Ejercicio 11
  val calCalario15: (Double, Double) => Double = genCalSalarioBono(1.15)

  //Ejercicio 12
  val calCalario100 = genCalSalarioBono(1.00)

}
