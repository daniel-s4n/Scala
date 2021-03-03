package Classes

class Persona(val nombre:String, val apellido:String){
  def getNombre = s"$nombre $apellido"
}

object Persona{
  def apply(fullName:String) = {
    val name = fullName.split(" ")
    new Persona(name(0), name(1))
  }

  def main(args:Array[String]):Unit = {
    val daniel = Persona("Daniel Caita")
    println(daniel.getNombre)
  }
}
