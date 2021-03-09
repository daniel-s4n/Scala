package Inmutable

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

  def foldRight[A,B](as:List[A], z:B)(f:(A,B) => B): B = as match {
    case Nil          => z
    case Const(x, xs) => f( x, foldRight(xs, z)(f) )
  }

  def foldLeft[A,B](list:List[A], z:B)(f:(B,A)=>B):B = list match {
    case Nil         => z
    case Const(h, t) => foldLeft(t, f(z,h) )(f)
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
  val e13 = foldRight( List(9L, 6L, 7L), Nil:List[Long] )( Const(_,_) )

  // Ejercicio 14
  //  Compute la función length de una lista utilizando foldRight.
  def lengthR[A](lst:List[A]):Int = foldRight(lst, 0)((_,y) => y + 1)

  // Ejercicio 15
  //  Compute la función and utilizando foldRight.
  def andR(lst:List[Boolean]):Boolean = foldRight(lst, true)( (x,y) => x && y )

  //Ejercicio 16
  //  La función takeWhile aplicada a un predicado p y a una lista
  //  lst, retorna el prefijo más largo (posiblemente vacı́o) que satisface p
  def takeWhile[A](lst:List[A])(p:A => Boolean):List[A] = {
    def takeWhiler[A](list:List[A],aux:List[A])(p:A=>Boolean):List[A] = list match {
      case _                  => aux
      case Const(h,t) if p(h) => takeWhiler(t, addEnd(aux, h))(p)
    }
    takeWhiler(lst,Nil)(p)
  }

  // Ejercicio 17
  // Implementar la funcion filter
  def filter[A](lst:List[A])(p:A => Boolean):List[A] = foldRight(lst, Nil:List[A])((x, y) => if(p(x)) Const(x,y) else y)

  // Ejercicio 18
  //  Implemente la función unzip esta lista separa una lista de tuplas
  //  en dos listas distintas.
  def unzip[A,B](lst:List[(A,B)]):(List[A], List[B]) = {
    foldRight(lst, (Nil:List[A], Nil:List[B]) )( (x,y) => (Const(x._1, y._1), Const(x._2, y._2)))
  }

  // Ejericio 19
  // Devuelve la cantidad de elementos de la lista recibida
  def lengthL[A](list:List[A]):Int =  foldLeft(list, 0)( (x, _) => x + 1)

  // Ejercicio 20
  //Devuelve un booleano true si todos los elementos de la lista recibida son true
  def andL(list:List[Boolean]):Boolean = foldLeft(list, true)(_ && _)

  //Ejercicio 21
  // Devuelve una lista con los elementos de la lista recibida hasta el primer elemento que deje de
  // cumplir con el predicado recibido
  def takeWhileL[A](lst:List[A])(p:A => Boolean):List[A] = {
    def f(b:(Boolean,List[A]),a:A):(Boolean,List[A]) = b match {
      case (true,lst)  => if ( p(a) ) (true, addEnd(lst, a)) else (false, lst)
      case (false,lst) => b
    }
    foldLeft(lst, (true, Nil:List[A]) )(f)._2
  }

  //Ejercicio 22
  // Devuelve una lista con todos los elementos de la lista recibida que cumplan con el predicado recibido
  def filterL[A](list:List[A])(p:A => Boolean):List[A] = foldLeft(list,Nil:List[A])((y,x) => if( p(x) ) addEnd(y, x) else y)

  //Ejercicio 23.
  // Devuelve dos listas separando los elementos en las tuplas de la lista recibida, la primer lista con
  // el primer elemento de cada tupla y la segunda con el segundos elemento de cada tupla
  def unzipL[A,B](list:List[(A,B)]):(List[A],List[B]) =
    foldLeft(list, (Nil:List[A], Nil:List[B]) )( (y, x) => (addEnd(y._1, x._1), addEnd(y._2, x._2)) )
}
