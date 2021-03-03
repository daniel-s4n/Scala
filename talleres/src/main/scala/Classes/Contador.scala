package Classes

class Sumador(monto:Int){
  def adicionar(valor:Int) = valor + monto
}

class Contador(contador:Int){
  def incr():Contador = new Contador(contador + 1)
  def incr(n:Int):Contador = new Contador(contador + n)

  def decr():Contador = new Contador(contador - 1)
  def decr(n:Int):Contador = new Contador(contador - n)

  // Preguntar
  def ajuste(sumador: Sumador):Contador = {

  }
}
