package Functions
import scala.annotation.tailrec

object Recursion {
  //Ejercicio 16
  def fibonacci(n:Int): Int = {
    if (n == 0) 0
    else if (n == 1) 1
    else fibonacci(n - 1) + fibonacci(n - 2)
  }

  // Ejercicio 15-17
  def factorial(n:Int): Int = {
    @tailrec
    def tail(n:Int, res:Int):Int = {
      if (n == 1) res
      else tail(n - 1, res * n)
    }
    tail(n, 1)
  }
}
