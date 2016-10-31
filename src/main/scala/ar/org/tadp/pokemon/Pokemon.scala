package ar.org.tadp.pokemon
import scala.util.Try
//import ar.org.tadp.pokemon.Actividades._

case class Pokemon(especie: Especie,
                   genero: Genero,
                   caracteristicas: Caracteristicas,
                   estado: Estado = Sano) {

  def energia = caracteristicas.energia
  def experiencia = caracteristicas.experiencia
  def energiaMaxima = caracteristicas.energiaMaxima + nivel * especie.incrementos.energiaMaxima
  def fuerza = caracteristicas.fuerza + nivel * especie.incrementos.fuerza
  def velocidad = caracteristicas.velocidad + nivel * especie.incrementos.velocidad

  def energia(delta: Int) = copy(caracteristicas = caracteristicas.copy(energia = energia + delta min energiaMaxima))
  def fuerza(delta: Int) = copy(caracteristicas = caracteristicas.copy(fuerza = fuerza + delta))
  def velocidad(delta: Int) = copy(caracteristicas = caracteristicas.copy(velocidad = velocidad + delta))
  def nivel: Int = {
			def nivelR(expNivelAnterior: Int, nivelAnterior: Int): Int = {
				val expNivelSiguiente = 2 * expNivelAnterior + especie.resistenciaEvolutiva
				if (experiencia > expNivelSiguiente) nivelAnterior else nivelR(expNivelSiguiente, nivelAnterior + 1)
			}
			nivelR(0, 1)
		}

  def estado(estado: Estado) = copy(estado = estado)

  def esDeTipo(tipo: Tipo): Boolean = {
    especie.tipos match {
      case (`tipo`, _)     => true
      case (_, secundario) => secundario.fold(false) { _ == tipo }
      case _               => false
    }
  }

  def ganarExp(exp: Int) = {
    copy(caracteristicas = caracteristicas.copy(experiencia = experiencia + exp))
  }

  def descansar = Try(energia(energiaMaxima - energia))

  def levantarPesas(kilos: Int) = Try {
    require(!esDeTipo(Fantasma), "Los fantasmas no levantan pesas")

    this match {
      case pk if pk.fuerza * 10 < kilos => this.energia(-10).estado(Paralizado)
      case _ if this.esDeTipo(Pelea)    => this.ganarExp(kilos * 2)
      case _                            => this.ganarExp(kilos)
    }
  }
  

  def hacerActividad(actividad: Pokemon => Try[Pokemon]): Try[Pokemon] = {
    val validado = Try {
      require(estado != KO, "no puede hacer actividades estando KO")
      this
    }

    validado.flatMap { pk =>
      pk.estado match {
        case Dormido(0) => actividad(pk.estado(Sano))
        case Dormido(n) => Try(pk.estado(Dormido(n - 1)))
        case _          => actividad(pk)
      }
    }
  }
  
  def mejorRutina(criterio: Pokemon => Int)(rutinas: Rutina*) = {
		val resultados = for {
			rutina <- rutinas
			pokemonEntrenado <- rutina(this).toOption
			valor = criterio(pokemonEntrenado)
		} yield (rutina.nombre, valor)

		resultados.sortBy(_._2).map(_._1).reverse.headOption
	}
  
	//Parte 5
	def fingirIntercambio = {
	  val pokemonTriste = this.energia(-10)
		especie.condicionEvolutiva.fold(pokemonTriste){ (_, condicion) =>
		      condicion match {
		        case Intercambiar(evolucion) => copy(especie = evolucion)
		        case _ => pokemonTriste
		      }
		}
	}
	
	def usarPiedra(piedra: Piedra) = {
	  
	  especie.condicionEvolutiva.foldLeft(){ (poke, condicion) => 
	    condicion match {
	      case UsarPiedra(piedraRequerida, evolucion) if piedra == piedraRequerida =>
	        copy(especie = evolucion)
	      case _ => poke
	    }
	  }
	  
	}
}

case class Caracteristicas(
  experiencia: Int,
  energia: Int,
  energiaMaxima: Int,
  fuerza: Int,
  velocidad: Int)

sealed trait Genero
case object Macho extends Genero
case object Hembra extends Genero

sealed trait Estado
case class Dormido(turnosPendientes: Int = 3) extends Estado
case object KO extends Estado
case object Sano extends Estado
case object Paralizado extends Estado