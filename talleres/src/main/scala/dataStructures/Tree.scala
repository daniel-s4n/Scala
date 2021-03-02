package dataStructures
import scala.annotation.tailrec

sealed trait Tree[+A]
case class Leaf[A](value:A) extends Tree[A]
case class Branch[A](left:Tree[A], right:Tree[A]) extends Tree[A]

object Tree extends App{
  // -------------------- Taller 3 v2 --------------------
  // Ejercicio 11
  // Implemente la función size que cuente el número de nodos Leaf
  // y Branches en un árbol.
  def size[A](tree:Tree[A]):Int = {
    def sizer[A](tree:Tree[A], n:Int):Int = tree match {
      case Leaf(_)                              => n+1
      case Branch(Branch(br1,br2), Leaf(value)) => sizer(Branch(br1,br2), n+2)
      case Branch(Leaf(value), Branch(br1,br2)) => sizer(Branch(br1,br2), n+2)
      case Branch(br1, br2)                     => sizer(br1, n) + sizer(br2, n)
      case Branch(Leaf(value), Leaf(value2))    => n+2
    }
    sizer(tree, 1)
  }

  // Ejercicio 12
  // Implemente la función depth que retorna la longitud máxima
  // desde profundidad desde la raı́z a cualquier hoja.
  def depth[A](tree:Tree[A]):Int = {
    def dpt[A](tree:Tree[A], n:Int):Int = tree match {
      case Leaf(_)                                  => n
      case Branch(Branch(br1, br2), Leaf(value))    => dpt(Branch(br1, br2), n+1)
      case Branch(Leaf(value), Branch(br1, br2))    => dpt(Branch(br1, br2), n+1)
      case Branch(br1, br2)                         => dpt(br1, n) + dpt(br2, n)
      case Branch(Leaf(value), Leaf(value2))        => n+1
    }
    dpt(tree, 1)
  }

  println(depth(Branch(Branch(Leaf(10),Leaf(20)),Branch(Leaf(30),Leaf(40)))))
}
