package Classes.Cars

class Conductor ( val nombre:String,
                  val apellido:String,
                  val totalCarreras:Int,
                  val carrerasTerminadas:Int )
{
  def getNombre():String = nombre
  def getApellido():String = apellido
  def getTotalCarreras():Int = totalCarreras
  def getCarrerasTerminadas():Int = carrerasTerminadas
  def getCarrerasNoTerminadas():Int = totalCarreras - carrerasTerminadas
}


