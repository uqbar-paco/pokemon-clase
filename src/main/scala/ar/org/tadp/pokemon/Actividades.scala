package ar.org.tadp.pokemon
import scala.util.Try

object Actividades {
  type Actividad = Pokemon => Try[Pokemon]
  val actividades : List[Actividad] = List(_.descansar, _.levantarPesas(10))
}


	class Rutina(val nombre: String)(actividades: Pokemon => Try[Pokemon]*) {
		def apply(pokemon: Pokemon) = 
		  (Try(pokemon) /: actividades){ _ flatMap _ }
	}






