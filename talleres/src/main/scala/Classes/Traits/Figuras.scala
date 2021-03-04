package Classes.Traits

import scala.math.Pi

sealed trait Forma {
  def getTamanio:Double
  def getPerimetro:Double
  def getArea:Double
}

case class Circulo(val rad:Double) extends Forma{
  def getPerimetro:Double =  2 * Pi * rad
  def getArea:Double      = Pi * rad * rad
  def getTamanio:Double   = 0
}

case class Rectangulo(val lado1:Double, val lado2:Double){
  def getTamanio:Int      = 4
  def getPerimetro:Double = (lado1*2) + (lado2*2)
  def getArea:Double      = lado1 * lado2
}

case class Cuadrado (val lado:Double) extends Forma {
  val rect = new Rectangulo(lado, lado)

  def getPerimetro(): Double  = rect.getPerimetro
  def getArea(): Double       = rect.getArea
  def getTamanio:Double       = rect.getTamanio
}

object Draw {
  def apply(form:Forma):String = form match {
    case Circulo(rad)               =>  s"Circulo de radio $rad"
    case Rectangulo(lado1, lado2)   =>  s"Rectangulo de ancho $lado1 y largo $lado2"
    case Cuadrado(lado)             =>  s"Cuadrado de lados $lado"
  }
}

class Color(r:Int, g:Int, b:Int)
object Color{
  val red = new Color(255, 0, 0)
  val pink = new Color(255, 0, 255)
  val yellow = new Color(255, 255, 0)
}