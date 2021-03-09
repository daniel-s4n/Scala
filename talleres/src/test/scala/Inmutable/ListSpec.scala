package Inmutable

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

object ListSpec extends AnyFlatSpec with Matchers{
  "La funcion tail con la Lista (1,2,3,4)" should " deberia devolver (2,3,4)" in {
    val list = List(1,2,3,4)
    List.tail(list) shouldBe Const(2,Const(3,Const(4,Nil)))
  }

  "La funcion head con la Lista (1,2,3,4)" should " deberia devolver (1)" in {
    val list = List(1,2,3,4)
    List.head(list) shouldBe 1
  }

  "La funcion and con la Lista (true,false,true,true)" should " deberia devolver (false)" in {
    val list = List(true,false,true,true)
    List.and(list) shouldBe false
  }

  "La funcion and con la Lista (true,true,true,true)" should " deberia devolver (true)" in {
    val list = List(true,true,true,true)
    List.and(list) shouldBe true
  }

  "La funcion or con la lista (false,true,false,false)" should "deberia devolver (true)" in {
    val list = List(false,true,false,false)
    List.or(list) shouldBe true
  }
  "La funcion or con la lista (false,false,false)" should "deberia devolver (false)" in {
    val list = List(false,false,false)
    List.or(list) shouldBe false
  }

  "La funcion max enviando la lista (15,6,8,32,10)" should "deberia devolver (32)" in {
    val list = List(15,6,8,32,10)
    List.max(list) shouldBe 32
  }

  "La funcion min enviando la lista (15,6,8,32,10)" should "deberia devolver (6)" in {
    val list = List(15L,6L,8L,32L,10L)
    List.min(list) shouldBe 6L
  }

  "La funcion const enviando el elemento (true) y la lista (false,true,true)" should "deberia devolver la lista " +
    "(true,false,true,true)" in {
    val list = List(false,true,true)
    val head = true
    val listExit = List(true,false,true,true)
    List.const(head,list) shouldBe listExit
  }

  "La funcion addEnd enviando el elemento (false) y la lista (false,true,true)" should "deberia devolver la lista " +
    "(false,true,true,false)" in {
    val list = List(false,true,true)
    val element = false
    val listExit = List(false,true,true,false)
    List.addEnd(list,element) shouldBe listExit
  }

  "La funcion take con la Lista (1,2,3,4) y el n 3" should " deberia devolver (1,2,3)" in {
    val list = List(1,2,3,4)
    List.take(3,list) shouldBe(Const(1,Const(2,Const(3,Nil))))
  }

  "La funcion take con la Lista (1,2,3,4) y el n 0" should " deberia devolver Nil" in {
    val list = List(1,2,3,4)
    List.take(0,list) shouldBe Nil
  }

  "La funcion take con la Lista (1,2,3,4) y el n 6" should " deberia devolver (1,2,3,4)" in {
    val list = List(1,2,3,4)
    List.take(6,list) shouldBe(Const(1,Const(2,Const(3,Const(4,Nil)))))
  }

  "La funcion init con la Lista (1,2,3,4) " should " deberia devolver (1,2,3)" in {
    val list = List(1,2,3,4)
    List.init(list) shouldBe(Const(1,Const(2,Const(3,Nil))))
  }

  "La funcion Split con la Lista (1,2,3,4) y n 2" should " deberia devolver ((1,2),(3,4))" in {
    val list = List(1,2,3,4)
    List.split(2,list) shouldBe(((Const(1,Const(2,Nil))),(Const(3,Const(4,Nil)))))
  }

  "La funcion zip al enviarle la listas (1,2,3) y (true, false, true)" should "deberia devolver la lista de tipo " +
    "(A,B) ((1,true),(2,false),(3,true))" in {
    val list1 = List(1,2,3)
    val list2 = List(true,false,true)
    List.zip(list1,list2) shouldBe(Const((1,true),Const((2,false),Const((3,true),Nil))))
  }

  "La funcion zip al enviarle la listas (1,2,3,4) y (true, true, false)" should "deberia devolver la lista de tipo " +
    "(A,B) ((1,true),(2,true),(3,false))" in {
    val list1 = List(1,2,3,4)
    val list2 = List(true,true,false)
    List.zip(list1,list2) shouldBe(Const((1,true),Const((2,true),Const((3,false),Nil))))
  }

  "La funcion unzip al enviarle la lista ((1,\"a\"),(2,\"b\"),(3,\"c\"))" should "deberia devolver las listas (1,2,3)" +
    " y (\"a\",\"b\",\"c\")" in {
    val list = List((1,"a"),(2,"b"),(3,"c"))
    List.unzip(list) shouldBe((Const(1,Const(2,Const(3,Nil))),Const("a",Const("b",Const("c",Nil)))))
  }

  "La funcion reverse al enviarle la lista (6,7,8,9)" should "deberia devolver la lista (9,8,7,6)" in {
    val list = List(6,7,8,9)
    val listExit = List(9,8,7,6)
    List.reverse(list) shouldBe(listExit)
  }

  "La funcion intersperse al enviarle la lista (1,2,3) y enviarle el elemento 6" should "deberia devolver la lista " +
    "(1,6,2,6,3)" in {
    val list = List(1,2,3)
    val listExit = List(1,6,2,6,3)
    List.intersperse(6,list) shouldBe(listExit)
  }

  "La funcion concat al enviarle la lista (List(1,2),List(3,4),List(5,6))" should "deberia devolver la lista " +
    "(1,2,3,4,5,6)" in {
    val list1 = List(1,2)
    val list2 = List(3,4)
    val list3 = List(5,6)
    val list = List(list1,list2,list3)
    val listExit = List(1,2,3,4,5,6)
    List.concat(list) shouldBe(listExit)
  }

  "La funcion foldRight enviando la lista ( 9 L , 6 L , 7 L ) y Nil" should "deberia devolver la lista " +
    "( 9 L , 6 L , 7 L )" in {
    val list = List(9L,6L,7L)
    List.foldRight(list,Nil:List[Long])(Const(_,_)) shouldBe(list)
  }
  "La funcion lengthF enviando la lista (1,2,3,4,5,6) y 0" should "deberia devolver (6)" in {
    val list = List(1,2,3,4,5,6)
    List.lengthL(list) shouldBe(6)
  }
  "La funcion andF con la Lista (true,false,true,true)" should " deberia devolver (false)" in {
    val list = List(true,false,true,true)
    List.andL(list) shouldBe(false)
  }

  "La funcion andF con la Lista (true,true,true,true)" should " deberia devolver (true)" in {
    val list = List(true,true,true,true)
    List.andL(list) shouldBe(true)
  }

  "La funcion takeWhile al enviarle la lista (1,2,3,4,5,6,0) y la fucion anonima (< 4)" should "deberia devolver la lista " +
    "(1,2,3)" in {
    val list = List(1,2,3,4,5,6,0)
    val listExit = List(1,2,3)
    List.takeWhile(list)(x => x < 4) shouldBe(listExit)
  }

  "La funcion filter al enviarle la lista (1,10,4,8,3,7,2) y la funcion anonima (< 5)" should "deberia devolver la " +
    "lista (1,4,3,2)" in {
    val list = List(1,10,4,8,3,7,2)
    val listExit = List(1,4,3,2)
    List.filter(list)(x => x < 5) shouldBe(listExit)
  }

  "La funcion unzipF al enviarle la lista ((true,5),(false,6),(false,7))" should "deberia devolver las listas " +
    "(true,false,false) y (5,6,7) " in {
    val list = List((true,5),(false,6),(false,7))
    val listExit1 = List(true,false,false)
    val listExit2 = List(5,6,7)
    List.unzipL(list) shouldBe((listExit1,listExit2))
  }

  "La funcion lengthL al enviarle la lista (1,10,4,8,3,7,2)" should "deberia devolver (7)" in {
    val list = List(1,10,4,8,3,7,2)
    List.lengthL(list) shouldBe(7)
  }

  "La funcion andL con la Lista (true,false,true,true)" should " deberia devolver (false)" in {
    val list = List(true,false,true,true)
    List.andL(list) shouldBe(false)
  }

  "La funcion andL con la Lista (true,true,true,true)" should " deberia devolver (true)" in {
    val list = List(true,true,true,true)
    List.andL(list) shouldBe(true)
  }

  "La funcion takeWhileL con la lista (1,2,3,4,5,6) y la funcion anonima (< 4)" should "deberia devolver la " +
    "lista (1,2,3)" in {
    val list = List(1,2,3,4,5,6)
    val listExit = List(1,2,3)
    List.takeWhileL(list)(_ < 4) shouldBe(listExit)
  }

  "La funcion filterL al enviarle la lista (1,10,4,8,3,7,2) y la funcion anonima (< 5)" should "deberia devolver la " +
    "lista (1,4,3,2)" in {
    val list = List(1,10,4,8,3,7,2)
    val listExit = List(1,4,3,2)
    List.filterL(list)(_ < 5) shouldBe(listExit)
  }

  "La funcion unzipL al enviarle la lista ((true,5),(false,6),(false,7))" should "deberia devolver las listas " +
    "(true,false,false) y (5,6,7) " in {
    val list = List((true,5),(false,6),(false,7))
    val listExit1 = List(true,false,false)
    val listExit2 = List(5,6,7)
    List.unzipL(list) shouldBe((listExit1,listExit2))
  }
}
