package Taller1

object Taller1 extends App{
  val areaTrianguloRectangulo = (a:Int) => (a*a) / 2
  val areaCirculo = new Function1 [Double, Double] {
    def apply(r:Double) = math.Pi * (r*r)
  }

  val calSalario = (devengado:Double, deducciones:Double) => devengado - deducciones
  val calSalarioBono = (devengado:Double, deducciones:Double) => devengado * 1.10 - deducciones

  def compSalario( salario:(Double, Double) => Double, devengado:Double, deducciones:Double ) {
    salario(devengado, deducciones)
  }

  def genCalSalarioBono(bono:Double): (Double, Double)=>Double = {
    (devengado: Double, deduccion: Double) => devengado * bono - deduccion
  }

  def calSalario5(devengado:Double, deduccion:Double):Double = {
    val salario: (Double, Double) => Double = genCalSalarioBono(1.05)
    salario(devengado, deduccion)
  }

  def calSalario20(devengado:Double, deduccion:Double):Double = {
    val salario: (Double, Double) => Double = genCalSalarioBono(1.2)
    salario(devengado, deduccion)
  }

  val bono = 1
  def calSalarioBonoClausura(devengados:Double, deducciones:Double): Double = {
    devengados * bono - deducciones
  }

  val triangulo = areaTrianguloRectangulo(2)
  val circulo = areaCirculo(3.3)

  //compSalario(calSalarioBono, 1300, 100)
  println( calSalario5(1000, 200) )
}
