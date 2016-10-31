package ar.org.tadp.pokemon
import scala.util.{ Success, Failure, Try }
import org.scalatest.FreeSpec
import org.scalatest.Matchers
import org.scalatest.BeforeAndAfter
import org.scalatest.Matchers
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.MatchResult
import ar.org.tadp.pokemon.Actividades._


class IntegradorTest extends FreeSpec with Matchers with BeforeAndAfter {
		
  // Parte 1 
  
  val caracteristicas = Caracteristicas(1, 100, 200, 20, 10)
	val gastly = Especie((Fantasma, Some(Veneno)), new Incrementos)
	val pikachu = Especie((Electrico, None), new Incrementos) 
	val machop = Especie((Pelea, None), new Incrementos) 
	val charmander = Especie((Fuego, None), new Incrementos)
	
	
	
	val unGastly = Pokemon(gastly, Macho, caracteristicas)
	val unMachop = Pokemon(machop, Macho, caracteristicas.copy(fuerza = 99))
	val unCharmander = Pokemon(charmander, Macho, caracteristicas)

	
	// Parte 5 

  val actividadesConPesas : List[Actividad] = List(_.descansar, _.levantarPesas(1))

  "Integrador" - {

    "Parte 1" - {

      "Al hacer que un machop levante pesas, gana el doble de kg en experiencia" in {
        
        val caracteristicasLuegoDePesas = caracteristicas.copy(fuerza = 99, experiencia = 3)

        unMachop.hacerActividad(_.levantarPesas(1)).get should be(unMachop.copy(caracteristicas = caracteristicasLuegoDePesas)) 
      }
      
      "Si es un pokemon de tipo fantasma, al intentar levantar pesas deberia fallar" in {
       unGastly.hacerActividad(_.levantarPesas(1)).isFailure should be(true)
        
      }
      
      }

    
    "Parte 3 : Estados" - {
      
      "Al levantar pesas, si son mas de 10 kg por punto de fuerza" in {
        unMachop.hacerActividad(_.levantarPesas(10000)).get.estado should be(Paralizado)
  
      }
      
      "Al estar KO y hacer una actividad" in {
         val charmanderKO = unCharmander.copy(estado = KO)
         charmanderKO.hacerActividad(_.descansar).isFailure should be(true)
         
      }
    }
      
      "Parte 4: Rutinas" - {
        "Al hacer que un fantasma haga una rutina que levanta pesas" in {
        val rutinaNoAptaParaFantasmas = new Rutina("SuperFuerte")(_.levantarPesas(10), _.descansar)
        rutinaNoAptaParaFantasmas(unGastly).isFailure should be(true)
        }
      }
      
    

    }
  
}