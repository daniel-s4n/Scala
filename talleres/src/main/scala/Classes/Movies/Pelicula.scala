package Classes.Movies

class Pelicula (
               val nombre:String,
               val presentacion:Int,
               val rangoIMDB:Double,
               val director: Director
               ){

  def directorEdad = presentacion - director.nacimiento
  def esDirigidaPor(director: Director) = this.director == director

  def copy( nombre:String       = this.nombre,
            presentacion:Int    = this.presentacion,
            rangoIMDB:Double    = this.rangoIMDB,
            director: Director  = this.director): Pelicula = {
    new Pelicula(nombre, presentacion, rangoIMDB, director)
  }
}

object Pelicula {
  def apply(nombre:String, presentacion:Int, rangoIMDB:Double, director:Director):Pelicula = {
    new Pelicula(nombre, presentacion, rangoIMDB, director)
  }

  def mejorCalificada(pelicula1:Pelicula, pelicula2:Pelicula):Pelicula = {
    if (pelicula1.rangoIMDB > pelicula2.rangoIMDB) pelicula1
    else pelicula2
  }

  def mayorDirectorEnElTiempo(pelicula1:Pelicula, pelicula2:Pelicula):Director = {
    if(pelicula1.director.nacimiento > pelicula2.director.nacimiento) pelicula1.director
    else pelicula2.director
  }
}

