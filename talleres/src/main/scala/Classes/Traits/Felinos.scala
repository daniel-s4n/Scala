package Classes.Traits

trait Felino{
  val color:String
  val sonido:String

  def getColor:String
  def getSonido:String
}

class Felinos {
  class Leon(val color:String, val sonido:String, val melena:Double) extends Felino{
    def getColor:String = color
    def getSonido:String = sonido
    def getMelena:Double = melena
  }

  class Tigre(val color:String, val sonido:String) extends Felino{
    def getColor:String = color
    def getSonido:String = sonido
  }

  class Jaguar(val color:String, val sonido:String) extends Felino{
    def getColor:String = color
    def getSonido:String = sonido
  }

  class Gato(val color:String, val sonido:String, val comida:String) extends Felino{
    def getColor:String = color
    def getSonido:String = sonido
    def getComida:String = comida
  }
}
