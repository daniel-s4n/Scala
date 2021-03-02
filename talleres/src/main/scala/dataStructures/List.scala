package dataStructures

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Const[+A](h: A, t: List[A]) extends List[A]

object List extends App {
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Const(as.head, apply(as.tail: _*))
  }

  def const[A](h:A, t:List[A]):List[A] = Const(h, t)

  def length[A](list:List[A]):Int = list match {
    case Nil          => 0
    case Const(h, t)  => 1 + length(t)
  }

  def sum(ints: List[Int]):Int  = ints match {
    case Nil        => 0
    case Const(h,t) => h + sum(t)
  }

  def product(ds: List[Double]):Double = ds match {
    case Nil        => 1
    case Const(h,t) => h * product(t)
  }

  def addEnd[A](lst:List[A], elem:A):List[A] = lst match{
    case Nil          => Const(elem, Nil)
    case Const(h, t)  => Const(h, addEnd(t, elem))
  }

  def append[A](lst1:List[A], lst2:List[A]):List[A] = (lst1, lst2) match{
    case (Nil, Nil)           => Nil
    case (lst1, Nil)          => lst1
    case (Nil, lst2)          => lst2
    case (Const(h, t), lst2)  => Const(h, append(t, lst2))
  }

  def drop[A](n:Int, lst:List[A]): List[A] = (n, lst) match {
    case (0, lst)         => lst
    case (n, Nil)         => Nil
    case (n, Const(h, t)) => drop(n-1, t)
  }

  // -------------------- Taller 2 --------------------
  // COINCIDENCIA DE PATRONES

  // Ejercicio 2
  // Implementa la función tail que remueva el primer elemento de una lista
  def tail[A](ls: List[A]):List[A] = ls match{
    case Nil          => Nil
    case Const(h, t)  => t
  }

  // Ejercicio 3
  // Implementa la función head que devuelva el primer elemento de la lista.
  def head[A](ls: List[A]):A = ls match{
    case Const(h,_) => h
  }

  // Ejercicio 4
  // Esta función recibe un arreglo de valores Boolean y devuelve true si todos los
  // valores son verdaderos, en caso contrario devuelve false.
  @tailrec
  def and(lst:List[Boolean]):Boolean = lst match {
    case Nil              => true
    case Const(false, _)  => false
    case Const(true,  t)  => and(t)
  }

  // Ejercicio 5
  // Esta función recibe un arreglo de valores Boolean y devuelve false si todos los
  // valores son falsos, en caso contrario devuelve true.
  @tailrec
  def or(lst:List[Boolean]):Boolean = lst match {
    case Const(true,  Nil) => true
    case Const(false, Nil) => false
    case Const(true,  _)   => true
    case Const(false, t)   => or(t)
  }

  // Ejercicio 6
  //  Esta función recibe un arreglo de valores Int y devuelve valor máximo de todos
  //  los valores en la lista.
  def max(lst: List[Int]): Int = {
    @tailrec
    def maxis(lst: List[Int], gr:Int):Int = lst match {
      case Nil          => gr
      case Const(h, t)  => if(h > gr) maxis(t, h) else maxis(t, gr)
    }
    maxis(lst, 0)
  }

  // Ejercicio 7
  //  Esta función recibe un arreglo de valores Long y devuelve valor mı́nimo de todos
  //  los valores en la lista.
  def min(lst: List[Long]): Long = {
    @tailrec
    def mini(lst: List[Long], gr:Long):Long = lst match {
      case Nil          => gr
      case Const(h, t)  => if(h < gr) mini(t, h) else mini(t, gr)
    }
    mini(lst, head(lst))
  }

  // -------------------- Taller 3 v1--------------------
  // CONSTRUCCION DE LISTAS

  //  Ejercicio 1
  //  Implemente la función take que se encarga de tomar dos parámetros.
  //  El primero un valor entero positivo n y el segundo una lista de valores de
  //  cualquier tiempo. Y esta función se encarga de tomar los n primeros valores, si
  //  existen de la lista.
  def take[A](n:Int, lst:List[A]):List[A] = {
    @tailrec
    def taker(n:Int, lst:List[A], lstTmp:List[A]):List[A] = (n, lst) match {
      case (0, lst)         => lst
      case (_, Nil)         => lstTmp
      case (n, Const(h, t)) => taker(n-1, t, addEnd(lstTmp, h))
    }
    taker(n, lst, Nil)
  }

  // Ejercicio 2
  // Esta función toma una lista y toma los valores iniciales excepto el último.
  def init[A](lst:List[A]):List[A] = {
    @tailrec
    def initer(lst:List[A], lstTmp:List[A]): List[A] = lst match {
      case Nil            => Nil
      case Const(_, Nil)  => lstTmp
      case Const(h, t)    => initer(t, addEnd(lstTmp, h))
    }
    initer(lst, Nil)
  }

  // Ejercicio 3
  // Implemente la función split, recibe dos parámetros n y una lista;
  // divide la primera lista en n elementos y los restantes quedan en la segunda lista.
  def split[A](n:Int, lst:List[A]):(List[A], List[A]) = {
    @tailrec
    def spliter[A](n:Int, lst:List[A], lstTmp:List[A]):(List[A], List[A]) = (n, lst) match {
      case (0, _)           => (lstTmp, lst)
      case (n, Const(h, t)) => spliter(n-1, t, addEnd(lstTmp, h))
    }
    spliter(n, lst, Nil)
  }

  // Ejercicio 4
  // Implemente la función zip esta función fusiona dos listas de tipos
  // diferentes en una lista de pares del mismo tamaño.
  def zip[A,B](lst1:List[A], lst2:List[B]):List[(A,B)] = {
    @tailrec
    def zipper(lst1:List[A], lst2:List[B], lstTmp:List[(A,B)]): List[(A,B)] = (lst1, lst2) match{
      case(Nil, _)                       => lstTmp
      case(_, Nil)                       => lstTmp
      case(Const(h1, t1), Const(h2, t2)) => zipper(t1, t2, addEnd( lstTmp, (h1,h2) ))
    }
    zipper(lst1, lst2, Nil)
  }

  // Ejercicio 5
  // Implemente la función unzip esta lista separa una lista de tuplas
  // en dos listas distintas.
  def unzip[A,B](lst:List[(A,B)]):(List[A], List[B]) = {
    @tailrec
    def unzipper(lst:List[(A,B)], lstTmp1:List[A], lstTmp2:List[B]):(List[A], List[B]) = lst match {
      case Nil                => (lstTmp1, lstTmp2)
      case Const((h1,h2), t)  => unzipper(t, addEnd(lstTmp1, h1), addEnd(lstTmp2, h2))
    }
    unzipper(lst, Nil, Nil)
  }

  // Ejercicio 6
  //  Implemente la función reverse. Toma una lista y devuelve una
  //  versión invertida de la misma.
  def reverse[A](lst:List[A]):List[A] = {
    @tailrec
    def reverser[A](lst:List[A], lstTmp:List[A]):List[A] = lst match {
      case Nil          => lstTmp
      case Const(h, t)  => reverser(t, Const(h, lstTmp))
    }
    reverser(lst, Nil)
  }

  // Ejercicio 7
  //  Implemente la función intersperse. Esta se encarga de entremezclar
  //  un valor entre los elementos originales de la lista.
  def intersperse[A](elem:A, lst:List[A]):List[A] = {
    @tailrec
    def intersperser[A](elem:A, lst:List[A], lstTmp:List[A]):List[A] = lst match {
      case Nil          => lstTmp
      case Const(h, t)  => intersperser(elem, t, addEnd(addEnd(lstTmp, h), elem))
    }
    intersperser(elem, lst, Nil)
  }

  // Ejercicio 8
  //  Implemente la función concat. Es función recibe una lista de lista
  //  valores de un tipo A y la transforma en una lista de valores de tipo A.
  def concat[A](lst: List[List[A]]):List[A] = {
    @tailrec
    def concater(lst:List[List[A]], lstTmp:List[A]):List[A]= lst match {
      case Nil        => lstTmp
      case Const(h,t) => concater(t, append(lstTmp, h))
    }
    concater(lst, Nil)
  }

  // -------------------- Taller 3 v2 --------------------
  // FUNCIONES DE ALTO ORDEN

  // Ejercicio 13

}
