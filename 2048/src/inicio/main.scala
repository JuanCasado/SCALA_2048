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
        def imprimirCabecera(col:Int,num:Int):Unit = {
            if(col==num-1) println(num)
            else{
              print(num + "\t")
              val n = num + 1
              imprimirCabecera(col,n)
        }
    
    
        }
       def seleccionNivel(nivel:Int, vidas:Int):Unit = {
             val posicion = 1         
     /*El nivel 1 muestra un tablero de 4x4 con dos semillas de 2*/
          if(nivel==1){
            val fil = 2
            val col = 2
            val ale = 1 //dos semillas 
            val tablero = generarTablero(col*fil)
            val posicion = 0
            val casillasLibres = comprobarCasillasLibres(tablero, tablero.length)
            val vectorAleatorios= generarVector(casillasLibres, ale) //creamos un vector con 2 posiciones libres del tablero
            val tableroJuego = colocarTablero(tablero, vectorAleatorios, ale)
            imprimirTablero(tableroJuego, col-1, fil)
            imprimirTablero(vectorAleatorios, ale, 1)
          
         
        }else if(nivel == 2){
            val fil = 9
            val col = 9
            val ale = 3 //semillas 
            val tablero = generarTablero(col*fil)
            val posicion = 0
            val casillasLibres = comprobarCasillasLibres(tablero, tablero.length)
            val vectorAleatorios= generarVector(casillasLibres, ale) //creamos un vector con 4 posiciones libres del tablero
            val tableroJuego = colocarTablero(tablero, vectorAleatorios, ale)
            imprimirTablero(tableroJuego, col-1, fil)
            imprimirTablero(vectorAleatorios, ale, 1)
        }
          else if(nivel == 3){
            val fil = 14
            val col = 14
            val ale = 5 //semillas 
            val tablero = generarTablero(col*fil)
            val posicion = 0
            val casillasLibres = comprobarCasillasLibres(tablero, tablero.length)
            val vectorAleatorios= generarVector(casillasLibres, ale) //creamos un vector con 4 posiciones libres del tablero
            val tableroJuego = colocarTablero(tablero, vectorAleatorios, ale)
            imprimirTablero(tableroJuego, col-1, fil)
            imprimirTablero(vectorAleatorios, ale, 1)
        }else if(nivel == 4){
            val fil = 17
            val col = 17
            val ale = 5 //semillas 
            val tablero = generarTablero(col*fil)
            val posicion = 0
            val casillasLibres = comprobarCasillasLibres(tablero, tablero.length)
            val vectorAleatorios= generarVector(casillasLibres, ale) //creamos un vector con 4 posiciones libres del tablero
            val tableroJuego = colocarTablero(tablero, vectorAleatorios, ale)
            imprimirTablero(tableroJuego, col-1, fil)
            imprimirTablero(vectorAleatorios, ale, 1)
        }else{
            System.err.println("Error, por favor, escoja un nivel v�lido")
            inicioJuego(vidas)
        }
    }
       def generarTablero(col:Int): List[Int] = col match{
		        case 0 => Nil
		        case _ => (0)::generarTablero(col-1)
	  }
       
       def buscarIndice(l:List[Int], posicion: Int) : Int = {
         if (l.length == 0) 0
         else{
           if(l.length == posicion){
             return l.head
             }
             else{
               buscarIndice(l.tail, posicion)
           }
         }
       }
      /*Funci�n que recibe un vector de todas las posiciones libres del tablero, el n�mero de semillas que hay que crear
       * y devulve un vector con posiciones aleatorias fijas*/
       
       def generarVector(posicionesLibres: List[Int], semillas: Int): List[Int] ={
           
		        if (semillas < 0) Nil
		        else{
		          val aleatorio = (Math.random()*posicionesLibres.length)
              val indiceCasillasLibres = buscarIndice(posicionesLibres, aleatorio.toInt)
              val semillasaux = semillas -1
		          (indiceCasillasLibres)::generarVector(posicionesLibres, semillasaux)
		          }
		        
       }
       
      def colocarTablero(tablero: List[Int], posiciones: List[Int], semillas:Int) :List[Int]={
           val posicionActual = 1
           val semillasaux = semillas -1
           val tableroaux = tablero
           if(semillas<0) tablero
           else{
              val semillaaux = semillas-1
              colocarTablero(colocarSemillas(tableroaux, posiciones, posicionActual), posiciones.tail, semillaaux)
           }
      
       }
       def colocarSemillas(l:List[Int], semillas:List[Int], posicionActual:Int): List[Int] = {
   				 if(l.length == 0) l
   				 else{
           if(posicionActual == semillas.head) {
  					    val a = 2
  					    val posicionNueva = posicionActual + 1
  					    List(a):::colocarSemillas(l.tail, semillas, posicionNueva)
  					      
   				    }else {
   				      val posicionNueva = posicionActual + 1
   				      List(l.head):::colocarSemillas(l.tail, semillas, posicionNueva)
   				    }
   				 }
   			}
   
    def comprobarCasillasLibres(l:List[Int], posicion:Int) : List[Int] ={
      val nuevaposicion = posicion-1
      if (l.length == 0) l
      else {
        if (l.head == 0) {
          List(posicion):::comprobarCasillasLibres(l.tail, nuevaposicion)
        }else{
          List():::comprobarCasillasLibres(l.tail, nuevaposicion)
        }
      }
    }
    def imprimir_aux(l:List[Int],salto:Int,n:Int,fil:Int,fil_final:Int):Unit = {
  	if(l.length ==  0) print("\n")
  	else if(n==salto+1){
  	
  				print(l.head)
  				val f = fil+1
  				if(f>fil_final){
  				  print("\t\n")
  				  
  				}else{
  				  print("\t\n"+ f +"\t|\t" )
  				}
  				
  				imprimir_aux(l.tail, salto, 0,f,fil_final)
  				
  				
  		} else {
  			print(l.head)
  			print("\t")
  			imprimir_aux(l.tail, salto, n+1,fil,fil_final)
  		}
  }
 def imprimirTablero(l:List[Int],col:Int,fil:Int) ={
    print("\t\t")
    imprimirCabecera(col,1)
    println("-----------" * col)
    print("1\t| \t")
  		imprimir_aux(l,col-1,0,1,fil)
  }

def reiniciarJuego(vidas:Int):Unit = {
  
  if(vidas > 0){
     println("�Desea comenzar otra partida? 1 = s� 0 = No");
     val v= vidas -1 
     val respuesta = new Scanner(System.in)
     if (respuesta.nextInt() == 1){
        inicioJuego(v)
     }else{
       print("Gracias por jugar, adios!!")
     }
       
  }
 
}

}