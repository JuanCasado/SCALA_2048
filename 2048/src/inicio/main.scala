package inicio

import java.util
import java.util.Random
import java.util.Scanner
import java.io.BufferedReader
import java.io.InputStreamReader
object Juego {
  
        def main(args:Array[String]) = 
        {
            print("\n>>>>>>           16384           <<<<<<\n")
            println("--------------------------------------------")
            val vidas= 3
            inicioJuego(vidas)
		        
        }
        def inicioJuego(vidas:Int): Unit= {
            
            val reader = new Scanner(System.in)
            println("Introduce el nivel seleccionado (1/2/3/4):")
            val nivel = reader.nextInt()
		        seleccionNivel(nivel, vidas)
		        reiniciarJuego(vidas)
		        
        }
       def seleccionNivel(nivel:Int, vidas:Int):Unit = {
             val posicion = 1         
     /*El nivel 1 muestra un tablero de 4x4 con dos semillas de 2*/
          if(nivel==1){
            val fil = 2
            val col = 2
            val ale = 2 //dos semillas 
            val tablero = generarAle(generarTablero(col*fil), ale, posicion, 2*2)
            imprimirTablero(tablero, col-1, 0,1, fil)
         
        }else if(nivel == 2){
            val fil = 9
            val col = 9
            val ale = 4 //semillas 
            val tablero = generarAle(generarTablero(col*fil), ale, posicion, 9*9)
            imprimirTablero(tablero, col-1, 0,1, fil)
        }
          else if(nivel == 3){
            val fil = 14
            val col = 14
            val ale = 6 //semillas 
            val tablero = generarAle(generarTablero(col*fil), ale, posicion, 14*14)
            imprimirTablero(tablero, col-1, 0,1, fil)
        }else if(nivel == 4){
            val fil = 17
            val col = 17
            val ale = 6 //semillas 
            val tablero = generarAle(generarTablero(col*fil), ale, posicion, 17*17)
            imprimirTablero(tablero, col-1, 0,1, fil)
        }else{
            System.err.println("Error, por favor, escoja un nivel v�lido")
            inicioJuego(vidas)
        }
    }
       def generarTablero(col:Int): List[Int] = col match{
		        case 0 => Nil
		        case _ => (0)::generarTablero(col-1)
	  }
       
         
      
       def generarAle(l:List[Int], semillas:Int, posicion:Int, tamano:Int): List[Int] = {
          val aleatorio = ((Math.random()*9) + 1)
   			  if (l.head == null || semillas==0) l
   			  else{
   				    if(l.head==0 && aleatorio.toInt== posicion) {
  					    val a = 2
  					    val semillas2 = semillas -1
  					    List(a):::generarAle(l.tail, semillas2, posicion+1, tamano)
  					      
   				    }else{
   					    List(l.head):::generarAle(l.tail, semillas, posicion, tamano)
   				    }
   			
   			}
   }
def imprimirTablero(l:List[Int],salto:Int,n:Int,fil:Int,fil_final:Int):Unit = {
  	if(l.isEmpty) print("\n")
  	else if(n==salto){
  	
  				print(l.head)
  				val f = fil+1
  				if(f>fil_final){
  				  print("\t\n")
  				  
  				}else{
  				  print("\t\n" )
  				}
  				
  				imprimirTablero(l.tail, salto, 0,f,fil_final)
  				
  				
  		} else {
  			print(l.head)
  			print("\t")
  			imprimirTablero(l.tail, salto, n+1,fil,fil_final)
  		}
  }

def reiniciarJuego(vidas:Int):Unit = {
  
  if(vidas > 0){
     println("�Desea comenzar otra partida? 1 = s� 0 = No");
     val respuesta = new Scanner(System.in)
     if (respuesta.nextInt() == 1){
        inicioJuego(vidas)
     }else{
       print("Gracias por jugar, adios!!")
     }
       
  }
 
}

}