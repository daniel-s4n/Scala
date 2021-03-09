import scala.+:
import scala.annotation.tailrec

object Listas extends App {
  // Notas de clase
  def subs[A](list: List[A]): List[List[A]] = {
    @tailrec
    def subsAux[A](list: List[A], iterator: Int, aux: List[List[A]]): List[List[A]] = iterator match {
      case -1 => aux.reverse
      case n  => subsAux(list, iterator - 1, aux ::: list.combinations(iterator).toList)
    }
    subsAux(list, list.length, Nil)
  }

  def barajar[A](a:A, lst:List[A]):List[List[A]] = lst match {
    case Nil      => List(List(a))
    case x :: xs  =>(a :: ( x :: xs)) :: (barajar(a, xs)).map(x::_)
  }

  def permutar[A](lst:List[A]):List[List[A]] = lst match {
    case Nil      => List(Nil)
    case x :: xs  => (permutar(xs)).flatMap(barajar(x, _))
  }

  // Ejercicio 1
  def myLast[A](lst:List[A]):A = lst match {
    case h :: Nil   => h
    case _ :: tail  => myLast(tail)
    case _          => throw new NoSuchElementException
  }

  // Ejercicio 2
  def myButLast[A](lst:List[A]):List[A] = lst match {
    case x :: y :: Nil => x :: y :: Nil
    case _ :: y        => myButLast(y)
    case x :: Nil      => throw new NoSuchElementException
  }

  // Ejercicio 3
  def elementAt[A](lst:List[A], ind:Int):A = (lst, ind) match{
    case (x :: xs, 0) => x
    case (x :: xs, _) => elementAt(xs, ind-1)
    case (Nil, _)     => throw new NoSuchElementException
  }

  // Ejercicio 4
  def myLength[A](lst:List[A]):Int = {
    @tailrec
    def sizer[A](lst:List[A], size:Int):Int = lst match {
      case Nil      => size
      case x :: xs  => sizer(xs, size+1)
    }
    sizer(lst, 0)
  }

  def myLengthFR[A](lst:List[A]):Int = lst.foldRight(0)((_,x) => x+1)

  // Ejercicio 5
  def myReverse[A](lst:List[A]):List[A] = {
    @tailrec
    def reverser[A](lst:List[A], lstTmp:List[A]):List[A] = lst match {
      case Nil      => lstTmp
      case x :: xs  => reverser(xs, x :: lstTmp)
    }
    reverser(lst, Nil)
  }

  // Ejercicio 6
  def isPalindrome[A](lst:List[A]):Boolean = lst == myReverse(lst)

  // Ejercicio 7
  def flatten[A](lst:List[List[A]]):List[A] = ???

  // Ejercicio 8
  def compress[A](lst:List[A]):List[A] = {
    def compresser[A](lst:List[A], lstTmp:List[A], aux:A):List[A] = {
      if(lst == Nil) lstTmp
      else if(aux == lst.head) compresser(lst.tail, lstTmp, aux)
      else compresser(lst.tail, lstTmp :+ lst.head, lst.head)
    }
    compresser(lst, List(lst.head), lst.head)
  }

  // Ejercicio 9 --> Preguntar
  def pack[A](lst:List[A]):List[List[A]] = {
    def packer[A](lst:List[A], res:List[List[A]], lstTmp:List[A], aux:A ):List[List[A]]  = (lst, aux) match {
      case (Nil, _)                         => res
      case (x :: xs, a) if(xs.tail == Nil)  => res ::: List(lst)
      case (x :: xs, a) if(xs.head == a)    => packer(xs, res,           lstTmp :+ a,     aux)
      case (x :: xs, a) if(xs.head != a)    => packer(xs, res :+ lstTmp, xs.head :: Nil,  xs.head)
    }

    if(lst != Nil) packer(lst, Nil, List(lst.head), lst.head)
    else throw new NoSuchElementException
  }

  // Ejercicio 10
  def encode[A](lst:List[A]):List[(Int,A)] = {
    val pk = pack(lst)
    def cont(lsts:List[List[A]], tmp:List[(Int,A)]):List[(Int,A)] = lsts match {
      case Nil => tmp
      case x :: xs => cont(xs, tmp :+ (myLength(x), x.head))
    }
    cont(pk, Nil)
  }

  // Ejercicio 14
//  def dupli[A](lst:List[A]):List[A] = {
//    def dup[A](lst:List[A], lstTmp:List[A], aux:A) = lst match {
//      case Nil => lstTmp
//    }
//  }

  val l = List(1, 1, 2, 2, 2, 3, 3, 2, 1, 2, 2)
  val l2 = List(5)

  println( encode(l) )
}
