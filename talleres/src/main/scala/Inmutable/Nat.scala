package Inmutable

import scala.annotation.tailrec

sealed trait Nat
case object Cero extends Nat
case class Suc(nat: Nat) extends Nat

object Nat extends App{
  // -------------------- Taller 2 --------------------
  // Ejercicio 10
  // Implemente la función fromNatToInt que toma un número natural Nat y lo transforma a su valor Int.
  def fromNatToInt(num:Nat):Int = num match {
    case Cero     => 0
    case Suc(num) => 1 + fromNatToInt(num)
  }

  // Ejercicio 11
  // Implemente la función fromIntToNat que tomar valores enteros
  // positivo (inclusive le cero) y produce el correspondiente número natural.
  def fromIntToNat(int:Int):Nat = int match {
    case 0    => Suc(Cero)
    case int  => Suc(fromIntToNat(int - 1))
  }

  // -------------------- Taller 3 v1 --------------------
  //  Ejercicio 9
  //  Implemente la función addNat. Esta función recibe dos naturales y
  //  se encarga de sumarlos produciendo un valor correcto.
  def addNat(nat1:Nat, nat2:Nat): Nat = {
    @tailrec
    def adder(nat1:Nat, nat2:Nat, sum:Nat):Nat = (nat1, nat2) match {
      case (Cero, Cero)       => sum
      case (Suc(n), Cero)     => adder(n, Cero, Suc(sum))
      case (Cero, Suc(n))     => adder(n, Cero, Suc(sum))
      case (Suc(n1), Suc(n2)) => adder(n1, n2, Suc(Suc(sum)))
    }
    adder(nat1, nat2, Cero)
  }

  // Ejercicio 10
  //  Implemente la función prodNat. Esta función realiza la
  //  multiplicación de dos valores naturales.
  def prodNat(nat1:Nat, nat2: Nat):Nat = {
    @tailrec
    def producer(nat1:Nat, nat2:Nat, prd:Nat):Nat = (nat1, nat2) match {
      case (_, Cero)            => Cero
      case (Cero, _)            => Cero
      case (Suc(Cero), n1)      => addNat(n1, prd)
      case (n1, Suc(Cero))      => addNat(prd, n1)
      case (Suc(n1), Suc(n2))   => producer( n1, nat2, addNat(prd, nat2) )
    }
    producer(nat1, nat2, Cero)
  }
}
