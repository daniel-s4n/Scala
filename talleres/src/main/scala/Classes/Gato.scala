package Classes

class Gato(val nombre:String, val color:String, val comida:String)

object gatos {
  val io      = new Gato("IO", "Fawn", "Churrus")
  val make    = new Gato("Make", "Red", "Leche")
  val docker  = new Gato("Docker", "Blue", "Cuido")
}

object VentaDeChurrus {
  def despachar(cat:Gato):Boolean = cat.comida == "Churrus"
}