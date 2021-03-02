package dataStructures

sealed trait Nat
case object Cero extends Nat
case class Suc(nat: Nat) extends Nat

object Nat {
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

//TODO: Terminar los ejercicios de los naturales

  // -------------------- Taller 3 v1 --------------------
  //  Ejercicio 9
  //  Implemente la función addNat. Esta función recibe dos naturales y
  //  se encarga de sumarlos produciendo un valor correcto.
  def addNat(nat1:Nat, nat2:Nat): Nat = (nat1, nat2) match {
    case (Cero, Cero)           => Cero
    case (Suc(nat1), Cero)      => nat1
    case (Cero, Suc(nat2))      => nat2
    case (Suc(nat1), Suc(nat2)) =>
  }

}
