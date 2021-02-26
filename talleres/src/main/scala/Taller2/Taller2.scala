package Taller2

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Const [+A](h: A, t: List[A]) extends List[A]

object Taller2 extends App {
  // Notas de clase

  def length[A](list:List[A]):Int = list match {
    case Nil => 0
    case Const(h, t) => 1 + length(t)
  }
  def sum(ints: List[Int]):Int  = ints match {
    case Nil => 0
    case Const(h,t) => h + sum(t)
  }
  def product(ds: List[Double]):Double = ds match {
    case Nil => 1
    case Const(h,t) => h * product(t)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Const(as.head, apply(as.tail: _*))
  }
}
