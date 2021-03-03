package Classes

class Contador(contador:Int){
  def incr():Contador = new Contador(contador + 1)
  def incr(n:Int):Contador = new Contador(contador + n)

  def decr():Contador = new Contador(contador - 1)
  def decr(n:Int):Contador = new Contador(contador - n)

  def getContador = contador

  def ajuste(sumador: Sumador):Contador = {
    new Contador(sumador.adicionar(10))
  }
}

class Sumador(monto:Int){
  def adicionar(valor:Int) = valor + monto
}

object Contador{
  def apply(contador:Int):Contador = new Contador(contador)

  def main(args:Array[String]):Unit = {
    val cont:Contador = Contador(2)
    val cont2 = cont.incr(4)

    println(Contador(10).incr.decr.incr.incr.getContador)
  }
}