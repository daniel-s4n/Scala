object ForListas extends App{
  def myLength[A](lst:List[A]):Int = ( for{ _ <- lst  } yield (a:Int) => a+1 ).foldLeft(0)( (e,f) => f(e) )

  def myLast[A](lst:List[A]):A     = ( for{ xi <- lst } yield (a:A) => xi ).foldLeft(lst.head)( (e,f) => f(e) )
//
//
//  def myKthElem[A](k:Int, lst:List[A]):Option[A] = (for {
//    xi <- lst
//  } yield ).foldLeft((0, None))((e, f) => f(e))

  val lst = List(1, 2, 3, 4, 5)
  println(myLength(lst))
  println(myLast(lst))
}
