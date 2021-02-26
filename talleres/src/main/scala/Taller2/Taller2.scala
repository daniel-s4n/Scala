package Taller2

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Const[+A](h: A, t: List[A]) extends List[A]

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

  // Ejercicio 2
  def tail[A](ls: List[A]):List[A] = ls match{
    case Nil => Nil
    case Const(h, t) => t
  }

  // Ejercicio 3
  def head[A](ls: List[A]):A = ls match{
    case Nil => null
    case Const(h, t) => h
  }

  // Ejercicio 4
  def and(lst: List[Boolean]): Boolean = lst match{
    case Nil => false
    case Const(h, t) => h && and(t)
  }

  // Ejercicio 5
  def or (lst: List[Boolean]): Boolean  = lst match{
    case Nil => false
    case Const(h, t) => h || or(t)
  }

  // Ejercicio 6
  def max(lst: List[Int]): Int = lst match{
    case Nil => 0
    case Const(h, Nil) => h

  }


}
