package main

import scala.util.Random
import scala.swing._
import scala.swing.event._
import java.util.concurrent.ArrayBlockingQueue
import java.io.File
import java.io.PrintWriter
import scala.io.Source

object main extends App {
  /* Se piden los datos iniciales al usuario para poder comenzar el juego*/
  def setupJuego() = {
    def gameStrater (nivel : Int, vidas : Int, puntos_totales : Int, window : Interfaz, ia : Boolean) : Int = {
      val (_puntos_totales, _vidas, continue) = gameLoop (nivel, vidas, 0, puntos_totales, crearTablero (nivel), window, ia, false)
      if (continue) gameStrater (nivel, _vidas, _puntos_totales, window, ia)
      else 0
    }
    def createInterface (tablero : List[Int], nivel : Int) : Interfaz = {
      if (getNumber("Interfaz grafica?", 0 , 1)==1) {
        val window = frame(tablero, getCols(nivel));window.visible=true;window}
      else console(getCols(nivel))
    }
    print("\n>>>>>>           2048           <<<<<<\n")
    println("--------------------------------------------")
    val vidas= 2
    val nivel = getNumber("Seleccione nivel", 1 , 4)
    val ia = (getNumber("Movientos automáticos?", 0 , 1)==1)
    val window = createInterface(crearTablero (nivel), nivel)
    gameStrater (nivel, vidas, 0, window, ia)
    }
  /*Bucle principal del juego*/
  def gameLoop (nivel : Int, vidas : Int, puntos : Int, puntos_totales : Int, tablero : List[Int], window : Interfaz, ia : Boolean, skyp_update : Boolean) : (Int, Int, Boolean) = {
    /*Fin de juego*/
    def finJuego (nivel:Int, vidas : Int, puntos : Int, window : Interfaz, ia : Boolean) : (Int, Int, Boolean) = {
      window.setAccPoints(puntos)
      window.setLives(vidas)
      vidas match {
      case 0 => print("Te quedaste sin vidas!!!"); (puntos_totales+puntos, vidas -1, false)
      case _ => if(window.getOption("Desea jugar de nuevo?") == 1) (puntos_totales+puntos, vidas -1, true)
                else {print("Gracias por jugar, adios!!"); (puntos_totales+puntos, vidas -1, false)}
    }}
    if (!skyp_update){//Se muestra el tablero
      window.setPoints(puntos)
      window.updateContent(tablero)}
    val cols = getCols(nivel)
    if (isFull(tablero, cols)){finJuego(nivel, vidas, puntos + puntos_totales, window, ia)}//Se comprueba si el juego ha terminado
    else {try{
      val mejor_movimiento = mejorMoviento(tablero, cols)
      window.setRecomendation(mejor_movimiento)
      val movimiento = if (ia) {mejor_movimiento} else {window.getMovement()}                                                                 //PARA MOVIMIENTOS AUTOMÁTICOS
      val (nuevo_tablero, nuevo_puntos) = mover (tablero, cols, movimiento)
      val skyp_update = mismoTablero(tablero,nuevo_tablero)
      if (skyp_update) {window.setRecomendation("DIRECCION IMPOSIBLE")}
      else {window.setRecomendation("")}
      val tablero_final = if (skyp_update) nuevo_tablero else colocarSemillas(nuevo_tablero, getEmptyPositions(nuevo_tablero), nivel)
      gameLoop (nivel, vidas, nuevo_puntos+puntos, puntos_totales, tablero_final, window, ia, skyp_update)
    }catch {case _:IllegalArgumentException => finJuego (nivel, vidas, puntos, window, ia)}}//Si nos introducen una q para salir antes
  }
  /*Realiza un moviento devolviendo la tupla de el nuevo tablero con los puntos que se han hagando*/
  def mover (tablero : List [Int], cols : Int, movement : Int) : (List[Int], Int) = {
    /*Rota la matriz hasta la posición nesecaria para realizar el movimento en el sentido adecuado*/
    def preMover (tablero : List[Int], cols : Int, movimiento : Int) : List [Int] = movimiento match {
      case 1 => {tablero}                    //LEFT
      case 2 => {flip(tablero, cols)}        //RIGHT
      case 3 => {rotate90(tablero,cols)}     //UP
      case 4 => {rotate90(tablero,cols, 3)}  //DOWN
    }
    /*Rota la matriz hasta dejarla en su posición original*/
    def postMover (tablero : List[Int], cols : Int, movimiento : Int) : List [Int] = movimiento match {
      case 1 => {tablero}                    //LEFT
      case 2 => {flip(tablero, cols)}        //RIGHT
      case 3 => {rotate90(tablero,cols, 3)}  //UP
      case 4 => {rotate90(tablero,cols)}     //DOWN
    }
    /*Suma todas las piezas de un tablero, se usa para contar los puntos*/
    def sumaTablero (tablero : List[Int]) : Int = tablero match {
      case Nil => 0
      case head::tail => head+sumaTablero(tail)
    }
    val preTablero = preMover (tablero, cols, movement)              //Se rota el tablero de ser necesario según el movimiento elegido
    val t_sin_ceros = quitarCeros (preTablero, cols)                 //Se eliminan los ceros del tablero
    val piezas_nuevas = crearPiezasNuevas (t_sin_ceros, cols)        //Se crea una lista con las piezas nuevas que aparecerán en el tablero
    val nuevo_puntos = sumaTablero(piezas_nuevas)                    //Se suman las piezas nuevas para obetener los puntos
    val nuevo_tablero = ensamblarTablero (t_sin_ceros, piezas_nuevas)//Se colocan las piezas sobre le tablero y se borran las que no hagan falta
    val nuevo_t_sin_ceros = quitarCeros (nuevo_tablero, cols)        //Se vuelven a quitar los ceros
    val postTablero = postMover (nuevo_t_sin_ceros, cols, movement)  //Se deja el tablero en su rotación original
    (postTablero, nuevo_puntos)
  }
  /*El lablero es girado en espejo respecto a su eje vertical*/
  def flip (tablero : List[Int], cols : Int) : List [Int] ={
    /*Entendiendo la lista como una matriz da la vuelta a una de sus filas*/
    def reverse (tablero : List[Int], cols : Int) : List [Int] = cols match {
      case 0 => Nil
      case _ => reverse(tablero.tail, cols-1):::List(tablero.head)
    }
    tablero match {
    case Nil => Nil
    case  _  => _inicioFila (tablero.length, cols) match {
      case true => reverse(tablero, cols):::flip(tablero.tail, cols)
      case false  => flip(tablero.tail, cols)
  }}}
  /*Rota la matriz rep veces*/
  def rotate90 (tablero : List[Int], cols : Int, rep : Int) : List[Int] = rep match {
    case 0 => tablero
    case _ => rotate90 (rotate90(tablero, cols), cols, rep - 1)
  }
  /*Se rota el tablero 90grados a la izquierda*/
  def rotate90 (tablero : List[Int], cols : Int) : List [Int] = {
    /*Logaritmos en base dos para saber el tamaño final del tablero rotado*/
    def log2 (number : Int) : Int = {
      scala.math.ceil(scala.math.log10(number)/scala.math.log10(2.0)).toInt
    }
    /*Añade tantos ceros como sean necesarios al tablero para hacer que su tamaño se al cuadrado de una potencia de dos*/
    def _fillZeros (tablero : List[Int], cols : Int, fillFactor : Int) : List[Int] = tablero match {
      case Nil => generarLista(fillFactor*(cols+fillFactor))
      case  _  => _finFila(tablero.length, cols) match {
        case true  => tablero.head::generarLista(fillFactor):::_fillZeros (tablero.tail, cols, fillFactor)
        case false => tablero.head::_fillZeros (tablero.tail, cols, fillFactor)
    }}
    /*Extrae de una lista la sección de ella indicada*/
    def seccion (tablero : List[Int], cols : Int, iniciox : Int, finx : Int, inicioy : Int, finy : Int) : List[Int] = tablero match{
      case Nil => Nil
      case  _  => (isBetween(((tablero.length-1)/cols),inicioy,finy) && isBetween(((tablero.length-1)%cols),iniciox,finx)) match {
        case true  => tablero.head::seccion (tablero.tail, cols, iniciox, finx, inicioy, finy)
        case false => seccion (tablero.tail, cols, iniciox, finx, inicioy, finy)
    }}
    /*Rota el tablero meadiante divide y vencerás*/
    def rotate (tablero : List[Int], cols : Int) : List [Int] = cols match {
      case 1 => List(tablero.head)
      case _ =>
        val q1 = rotate(seccion (tablero, cols, cols/2, cols, cols/2, cols), cols/2)
        val q2 = rotate(seccion (tablero, cols, 0, (cols/2)-1, cols/2, cols), cols/2)
        val q3 = rotate(seccion (tablero, cols, cols/2, cols, 0, (cols/2)-1), cols/2)
        val q4 = rotate(seccion (tablero, cols, 0, (cols/2)-1, 0, (cols/2)-1), cols/2)
        join(q2,q4,cols/2):::join(q1,q3,cols/2)
    }
    /*De una lista toma la parte que se correspondería con una fila de una matriz según el número de columnas dado*/
    def tomarFila (tablero : List[Int], cols : Int) :  List [Int] = cols match{
      case 1 => List(tablero.head)
      case _ => tablero.head::tomarFila (tablero.tail, cols-1)
    }
    /*Une dos listas una a continuación de la otra como si fueran dos matrices consecutivas (fila1 de una con fila1 de la otra etc)*/
    def join (l1 : List[Int], l2 : List[Int], cols : Int) : List [Int] = {
      if ((l1 == Nil) || (l2 == Nil)) Nil
      else _inicioFila(l1.length, cols) match{
        case true  => tomarFila (l1, cols):::tomarFila (l2, cols):::join (l1.tail, l2.tail, cols)
        case false => join (l1.tail, l2.tail, cols)
    }}
    /*A la matriz se le quitan los ceros que se le añadieron con anterioridad*/
    def quitaZeros(tablero : List[Int], cols : Int, fake_len : Int, facke_cols : Int) : List[Int] = {
      def _quitaZeros(tablero : List[Int], cols : Int, fake_len : Int, facke_cols : Int, to_skyp : Int) : List[Int] = {
        if (tablero == Nil) Nil
        else if (((fake_len - facke_cols*cols) > 0)) _quitaZeros(tablero.tail, cols, fake_len-1, facke_cols, 1)
        else to_skyp  match {
          case 1 =>  tomarFila (tablero, cols):::_quitaZeros(tablero.tail, cols, fake_len-1, facke_cols, facke_cols)
          case _ =>  _quitaZeros(tablero.tail, cols, fake_len-1, facke_cols, to_skyp-1)
        }}
        _quitaZeros(tablero, cols, fake_len, facke_cols, 1)
    }
    val facke_cols = scala.math.pow(2,log2(cols)).toInt
    quitaZeros(rotate(_fillZeros (tablero, cols, facke_cols-cols), facke_cols), cols, facke_cols*facke_cols, facke_cols)
  }
  /*Crea una lista con las piezas nuevas que aparecerán al realizar el movimento en su posición correspondiente*/
  def crearPiezasNuevas (tablero : List[Int], cols : Int) : List[Int] = {
    def _crearPiezasNuevas (tablero : List[Int], cols : Int, last_seen : Int, amount_seen : Int) : List[Int] = {
      /*Proporciona las piezas nuevas*/
      def _nuevaPieza (pieza_actual : Int, last_seen : Int, amount_seen : Int) : Int = {
          if ((pieza_actual == last_seen) && ((amount_seen % 2) == 0)) last_seen*2
          else 0
      }
      /*Indicica que mieza mantener como vista para la siguiente recursión*/
      def _nextSeen (pieza_actual : Int, last_seen : Int) : Int = {
        if (pieza_actual == 0) last_seen
        else pieza_actual
      }
      /*Cuenta las piezas seguidas vista en cada fila*/
      def piezasSeguidas (amount_seen : Int, pieza_actual : Int, last_seen : Int) : Int = {
        def _mismaPieza (pieza_actual : Int, last_seen : Int) : Int = {
          if (pieza_actual == last_seen) 1
          else 0}
        def _clear (pieza_actual : Int, last_seen : Int) : Int = {
          if ((pieza_actual == last_seen) || (pieza_actual == 0)) 1
          else 0}
        val inc = _mismaPieza (pieza_actual, last_seen)
        val clear = _clear (pieza_actual, last_seen)
        (amount_seen+inc)*clear
      }
      if (tablero == Nil) return Nil
      val pieza = _nuevaPieza (tablero.head, last_seen, amount_seen)
      if (_finFila(tablero.length, cols)) pieza::_crearPiezasNuevas (tablero.tail, cols, 0, 0)
      else pieza::_crearPiezasNuevas (tablero.tail, cols,  _nextSeen(tablero.head, last_seen), piezasSeguidas(amount_seen,tablero.head, last_seen))
    }
    _crearPiezasNuevas (tablero, cols, 0, 0)
  }
  /*Dice si una pieza está al final de una fila de la matriz representada por la lista*/
  def _finFila (length : Int, cols : Int) : Boolean = ((length-1) % cols) == 0
  /*Dice si una pieza está al inicio de una fila de la matriz representada por la lista*/
  def _inicioFila (length : Int, cols : Int) : Boolean = ((length) % cols) == 0
  /*Comprueba si un número está dentro del rango*/
  def isBetween (input : Int, min : Int, max : Int) : Boolean = (input >= min) && (input <= max)
  /*Añade las piezas nuevas al tablero sin añadir aquellas que deban desaparecer, de este modo se contrulle el movimento
    Quedan huecos que deberán eliminarse con otra pasada de quitarCeros*/
  def ensamblarTablero (tablero : List[Int], nuevas_piezas : List[Int]) : List[Int] = {
    def _ensamblarTablero (tablero : List[Int], nuevas_piezas : List[Int], omitir : Int) : List[Int] = nuevas_piezas match {
      case Nil => tablero
      case head::tail => head match {
        case 0 => (tablero.head*omitir)::_ensamblarTablero (tablero.tail, tail, 1)
        case _ => head::_ensamblarTablero (tablero.tail, tail, 0)}}
    _ensamblarTablero (tablero, nuevas_piezas.tail, 1)
  }
  /*Elimina los cero que hay entre las piezas de un tablero dejando estas pegadas al lado izquierdo*/
  def quitarCeros (tablero : List[Int], cols : Int) : List[Int] = {
    def _quitarCeros (tablero : List[Int], cols : Int, ceros : Int) : List[Int] = {
      if (tablero == Nil) generarLista(ceros)
      else if (_finFila(tablero.length, cols)) tablero.head::generarLista(ceros):::_quitarCeros (tablero.tail, cols, 0)
      else if (tablero.head == 0) _quitarCeros (tablero.tail, cols, ceros+1)
      else tablero.head::_quitarCeros (tablero.tail, cols, ceros)}
    val tablero_sin_ceros = _quitarCeros (tablero, cols,  0)
    if (mismoTablero( tablero, tablero_sin_ceros)) tablero
    else _quitarCeros (tablero_sin_ceros, cols,  0)
  }
  /*Dice el mejor movimento ccon una predicción de un nivel de profundidad*/
  def mejorMoviento (tablero : List[Int], cols : Int) : Int = {
    def _mejorMoviento (tablero : List[Int], cols : Int, movement : Int, max : Int, indice : Int) : Int = {
      if (movement > 4) indice
      else {
        val (_,puntos) = mover (tablero, cols, movement)
        if (puntos > max) _mejorMoviento (tablero, cols, movement+1, puntos, movement)
        else _mejorMoviento (tablero, cols, movement+1, max, indice)
    }}//EL RUIDO GENERADO POR MOVIMIENTOS ALEATORIOS PUEDE AUMENTAR LA PUNTUACION CONSEGUIDA ENORMEMENTE YA QUE EL LOOKUP ES SOLO DE 1 DE PROFUNDIDAD
    if ((cols > 5)&&((Math.random()*30).toInt == 15)) {(Math.random()*4).toInt + 1}
    else {
      val mejor_moviento = _mejorMoviento (tablero, cols, 1, 0, 0)
      if(mejor_moviento<1) (Math.random()*4).toInt + 1
      else mejor_moviento 
  }}
  /*Dice si dos tablero son el mismo*/
  def mismoTablero (tablero : List[Int], tablero_anterior : List[Int]) : Boolean = {
    if ((tablero == Nil) || (tablero_anterior == Nil)) true
    else if (tablero.head == tablero_anterior.head) mismoTablero(tablero.tail, tablero_anterior.tail)
    else false
  }
  /*Indica si en un tablero no se pueden hacer mas movietos validos*/
  def isFull (tablero : List[Int], cols : Int) : Boolean = {
    def _isFull (tablero : List[Int], movement : Int, cols : Int) : Boolean = {
      if (movement > 4) true
      else  {val (tablero_nuevo,_) = mover (tablero, cols, movement)
          if (mismoTablero(tablero,tablero_nuevo)) _isFull (tablero, movement+1, cols)
          else false
    }}
    _isFull (tablero, 1, cols)
  }
  /*Crea una lista de ceros con el tamaño indicado*/
  def generarLista(col:Int): List[Int] = {
      List.fill(col)(0)
  }
  /*Crea una tablero inicial nuevo con las caracetrísticas que le correspondan según el nivel elegido*/
  def crearTablero (nivel:Int) : List[Int] = {
    val cols = getCols (nivel)
    val tablero = generarLista(cols*cols)
    colocarSemillas(tablero, getEmptyPositions(tablero), nivel)
  }
  /*Se obtienen las pisiciones libres del tablero, en ellas podremos colocar piezas aleatorias nuevas*/
  def getEmptyPositions (tablero:List[Int]) : List[Int] = {
   def _getEmptyPositions (tablero:List[Int], position:Int) : List[Int] =  tablero match {
     case Nil => Nil
     case _ => tablero.head match {
       case 0 => _getEmptyPositions(tablero.tail, position+1):::List(position)
       case _ => _getEmptyPositions(tablero.tail, position+1)
     }
    }
   _getEmptyPositions (tablero, 1)
  }
  /*Pone tantas semillas aleatorias como se indique sienpre que quede hueco en el tablero*/
  def colocarSemillas(tablero:List[Int], semillas:List[Int], nivel: Int): List[Int] = {
    /*Selecciona un índice vacío donde colocar la semilla*/
    def buscarIndice(l:List[Int], posicion: Int) : Int = {
      def _buscarIndice(l:List[Int], posicion: Int) : Int = l match{
        case Nil => 0
        case _ => 
          if (l.length == posicion) l.head
          else _buscarIndice(l.tail, posicion)}
      _buscarIndice(l, l.length - posicion)
    }
    /*El índice la la posición vacía se elimina para no tenerla más en cuenta*/
    def eliminarIndice(l:List[Int], posicion: Int) : List[Int] = {
      def _eliminarIndice(l:List[Int], posicion: Int) : List[Int] = l match{
        case Nil => Nil
        case _ => 
          if (l.length == posicion) l.tail
          else l.head::_eliminarIndice(l.tail, posicion)}
      _eliminarIndice(l, l.length - posicion)
    }
    /*Pone una semilla en una posición del tablero*/
    def ponerSemilla (l:List[Int], posicion: Int, valor: Int) : List[Int] = {
      def _ponerIndice (l:List[Int], posicion: Int, valor: Int) : List[Int] = l match{
        case Nil => Nil
        case _ =>  
          if (l.length == posicion) valor::l.tail
          else l.head::_ponerIndice(l.tail, posicion, valor)}
      _ponerIndice(l, l.length - posicion + 1, valor)
    }
    def _colocarSemillas(tablero:List[Int], semillas:List[Int], nivel: Int, restantes : Int): List[Int] = {
  		if (semillas == Nil) tablero
  		if (restantes == 0) tablero
  		else {
  		  val valores = getListaSemillas(nivel)
  		  val posicion_semilla = (Math.random()*semillas.length).toInt //Posición aleatoria de una posición libre para colocar una semilla
    		val posicion_valor = (Math.random()*valores.length).toInt    //Posición aleatoria para encontrar valor de la semilla de la semilla
    		val semilla = buscarIndice(semillas, posicion_semilla)       //Posición libre se una semilla
    		val valor = buscarIndice(valores, posicion_valor)            //Valor se una semilla
  		  val siguientes_semillas = eliminarIndice (semillas, posicion_semilla)
  		  _colocarSemillas(ponerSemilla(tablero, semilla, valor), siguientes_semillas, nivel, restantes-1)}
    }	
    _colocarSemillas(tablero, semillas, nivel, getCantidadSemillas(nivel))
  }
  /*Muestra el tablero por pantallla*/
  def printTablero (tablero:List[Int], cols : Int) = {
    /*Da color a los caracteres mostrados por consola*/
    def color (value : Int) : String = {
      def _getColor (value : Int) : String = value match {
        case 0 => scala.Console.WHITE
        case 2 => scala.Console.BLACK
        case 4 => scala.Console.BLACK_B
        case 8 => scala.Console.GREEN
        case 16 => scala.Console.BLUE
        case 32 => scala.Console.RED
        case 64 => scala.Console.YELLOW
        case 128 => scala.Console.MAGENTA
        case 256 => scala.Console.CYAN
        case 512 => scala.Console.MAGENTA_B
        case 1024 => scala.Console.BLUE_B
        case 2048 => scala.Console.YELLOW_B
        case 4096 => scala.Console.GREEN_B
        case 8192 => scala.Console.YELLOW_B
        case _ => scala.Console.WHITE
      }
      //_getColor(value) + " "+value+ " " + scala.Console.BLACK //FUNCIONA SI SE EJECUTA EN LINUX/MAC FUERA DE UN IDE!!!
      value+""
    }
    /*Da formato a cada elemento de la matriz*/
    def _format (element : Int, x : Int, y : Int, cols : Int) : String = x match {
      case  0   => "\n" +_buildString("-",20*cols) + "|\t" + y + "\t|\t" + color(element) + "\t|"
      case  _   =>  "\t" + color(element) + "\t|"
    }
    /*Crea una cadena con el contenido del tablero*/
    def _printTablero (tablero:List[Int], cols : Int) : String = tablero match {
      case Nil => "\n" + _buildString("-",21*cols)
      case  _  => _format (tablero.head, (tablero.length%cols), (cols + 1 - tablero.length/cols).toInt, cols) +
                  _printTablero(tablero.tail,cols)
    }
    /*Crea una lista de número de 1 a n*/
    def _listNumbers (cols : Int) : String = cols match {
      case 0 => "|\t"
      case _ => _listNumbers(cols - 1) + "\t|\t" + cols.toString
    }
    /*Crea una cadena nueva a partir de la cadena pasada repetida n veces*/
    def _buildString (text : String, ammount : Int) : String = ammount match{
      case 0 => text + "\n"
      case _ => text+_buildString (text, ammount-1)
    }
    print(_buildString("-",21*cols) +  _listNumbers(cols) + "\t|" + _printTablero(tablero, cols))
  }
  /*Guarda un número en un archivo de texto*/
  def saveNumber (value : Int, file : String = "./.score") = {
    val writer = new PrintWriter(new File(file))
    writer.write(value.toString)
    writer.close()
  }
  /*Guarda un número en un archivo de texto*/
  def retrieveNumber (file : String = "./.score") : Int = {
    try{Source.fromFile(file).mkString.toInt}
    catch {case e: Throwable => print(e);0}
  }
  /*Imprime los puntos que se han sonseguido*/
  def printNumero (text : String ,puntos : Int) = {
    print("-----------------------\n")
    print(text +": " + puntos + "\n")
    print("-----------------------\n")
  }
  /*Pide al usuario un número dentro de un rango Con una q se puede terminar la partida con antelación*/
  def getNumber (text : String, min : Int, max : Int) : Int = {
    print (text + " [ " + min + ", " + max +" ] : ")
    try{val input = scala.io.StdIn.readInt();
      if (isBetween(input, min,max)) input
      else {print("ERROR ENTRADA FUERA DE RANGO\n"); getNumber (text, min, max)}
    }catch  {case _: Throwable => { if (scala.io.StdIn.readLine()=="q") throw new IllegalArgumentException("EXIT RECIVED");
                                    else print("ERROR ENTRADA INVALIDA\n");getNumber (text, min, max)}}}
  /*Define la clase que nos permite controlar la interfar, la clase solo actúa como interfaz para la función frame que
  Realmente es el objeto que tiene la implementacion, (En Scala Funciones == Objetos)*/
  abstract trait Interfaz extends MainFrame{
    def updateContent (tablero : List[Int]);  //MUESTRA EL TABLERO
    def setPoints (points : Int);             //MUESTRA LOS PUNTOS
    def setAccPoints (acc_points : Int);      //MUESTRA LOS PUNTOS ACUMULADOS
    def setRecomendation (movement : Int);    //DA RECOMENDACIONES DE MOVIENTO AL USUARIO
    def setRecomendation (text : String);     //CAMPO GENERAL
    def setLives (lives : Int);               //MUESTRA LAS VIDAS
    def getOption (option : String) : Int;    //Retorna una opcion
    def getMovement () : Int;                 //Retorna un moviento realizado
  }
  def console(cols : Int) : Interfaz = new Interfaz {
    def updateContent (tablero : List[Int]) = printTablero (tablero, cols)           //MUESTRA EL TABLERO
    def setPoints (points : Int) = printNumero ("Puntos",points)                     //MUESTRA LOS PUNTOS
    def setAccPoints (acc_points : Int) = printNumero("Puntos acc", acc_points)      //MUESTRA LOS PUNTOS ACUMULADOS
    def setRecomendation (movement : Int) = println(movementTxt(movement))           //DA RECOMENDACIONES DE MOVIENTO AL USUARIO
    def setRecomendation (text : String) = println(text)                             //CAMPO GENERAL
    def setLives (lives : Int) = printNumero("Vidas", lives)                         //MUESTRA LAS VIDAS
    def getOption (option : String) : Int= getNumber(option, 0, 1)                   //Retorna una opcion
    def getMovement () : Int= getNumber("Moviento", 1, 4)                            //Retorna un moviento realizado
  }
  def frame (tablero : List[Int], cols:Int) : Interfaz = new Interfaz {
    //Se utiliza para leer la entrada de la interfaz evitando polling pero sin cambiar el código como en caso de utilizar listeners
    val queue = new ArrayBlockingQueue[Int](1)
    def getMovement () : Int = queue.take
    def getOption (option : String) : Int = getNumber (option, 0, 1)
    /*Colorea los label se la interfaz según el valor que contengan*/
    def getColor (value : Int) : java.awt.Color = value match {
      case 0 => java.awt.Color.white
      case 2 => java.awt.Color.black
      case 4 => java.awt.Color.gray
      case 8 => java.awt.Color.green
      case 16 => java.awt.Color.blue
      case 32 => java.awt.Color.red
      case 64 => java.awt.Color.orange
      case 128 => java.awt.Color.magenta
      case 256 => java.awt.Color.cyan
      case 512 => java.awt.Color.pink
      case 1024 => java.awt.Color.lightGray
      case 2048 => java.awt.Color.yellow
      case 4096 => java.awt.Color.darkGray
      case 8192 => java.awt.Color.yellow
      case _ => java.awt.Color.black
    }
    /*CREA EL TABLERO DEL TAMAÑO QUE SEA NECESARIO*/
    def createContent (tablero : List[Int]) : List[Label] = tablero match {
      case Nil => Nil
      case  _  => val label = new Label(""+tablero.head)
                  label.foreground = getColor (tablero.head)
                  label::createContent (tablero.tail)
    }
    //MUESTRA EL TABLERO
    def updateContent (tablero : List[Int]) = {
      def _updateContent (tablero : List[Int], content :List[Label]) : Int = tablero match {
        case Nil => 0
        case  _  => content.head.text=""+tablero.head
                    content.head.foreground = getColor (tablero.head)
                    _updateContent(tablero.tail, content.tail)
      }
      _updateContent (tablero, content)
    }
    def setPoints (value : Int) = points.text="Points: "+value                                               //MUESTRA LOS PUNTOS
    def setAccPoints (value : Int) = acc_points.text="AccPoints: "+value                                     //MUESTRA LOS PUNTOS ACUMULADOS
    def setRecomendation (movement : Int) = recomendations.text = movementTxt(movement)                      //MUESTRA EL MOVIMEINTO RECOMENDADO
    def setRecomendation (value : String) = direccion_imposible.text = value                                 //CAMPO GENERAL
    def setLives (value : Int) = lives.text = "Vidas: "+value                                                //MUESTRA LAS VIDAS
    val header = new Label(">>>>>>           2048           <<<<<<") 
    val points = new Label ("Points: 0")
    val sep = new Label ("             ")
    val recomendations = new Label ("Seleccione nivel")
    val table = new Table (cols,cols)
    val acc_points = new Label ("AccPoints: 0")
    val content :List[Label] = createContent(tablero)
    val lives = new Label("Vidas: 3")
    val direccion_imposible = new Label("")
    val up = new Button("UP")
    val down = new Button("DOWN")
    val left = new Button("LEFT")
    val right = new Button("RIGHT")
    /*Coloca el contenido para crear la interfaz y define los listeners de los botones*/
    def panel(cols : Int)  = new BoxPanel (Orientation.Horizontal){
      contents += new BoxPanel (Orientation.Vertical) {
        contents += new BoxPanel(Orientation.Horizontal){
          contents += header}
        contents += new GridPanel(cols,cols){
          def applyContent (con : List[Label]) : Int = con match {
            case Nil => 0
            case  _  => contents+=con.head; applyContent (con.tail)
          }
          applyContent (content)}
        contents += new BoxPanel (Orientation.Horizontal) {
          contents += points
          contents+= sep
          contents += acc_points}}
      contents += new BoxPanel (Orientation.Vertical){
        contents += new BoxPanel (Orientation.Horizontal){
          contents += lives}
        contents += new BoxPanel (Orientation.Horizontal){
          listenTo(up)
          contents += up
          reactions+={case ButtonClicked(up) => {if (queue.size == 0) queue.put(3)}}}
        contents += new BoxPanel (Orientation.Horizontal){
          contents += new BoxPanel (Orientation.Horizontal){
            contents += left
            listenTo(left)
            reactions+={case ButtonClicked(left) => {if (queue.size == 0) queue.put(1)}}}
          contents += new BoxPanel (Orientation.Horizontal){
            contents += right
            listenTo(right)
            reactions+={case ButtonClicked(right) => {if (queue.size == 0) queue.put(2)}}}}
        contents += new BoxPanel (Orientation.Horizontal){
          contents += down
          listenTo(down)
          reactions+={case ButtonClicked(down) => {if (queue.size == 0) queue.put(4)}}}
        contents += new BoxPanel (Orientation.Horizontal){
          contents += recomendations}
        contents += new BoxPanel (Orientation.Horizontal){
          contents += direccion_imposible
    }}}
    title = "2048"
    contents = panel(cols)
    centerOnScreen
    size = new Dimension(cols * 27 + 340, cols * 22 + 90)
    minimumSize = size
    maximumSize = size
  }
  /*Columnas de tablero para cada nivel*/
  def getCols (nivel : Int) : Int = nivel match {
    case 1 => 4
    case 2 => 9
    case 3 => 14  
    case 4 => 17
  }
  /*Cantidad de semillas de tablero para cada nivel*/
  def getCantidadSemillas (nivel : Int) : Int = nivel match {
    case 1 => 1
    case 2 => 2
    case 3 => 5  
    case 4 => 5
  }
  /*Valor de las semillas para cada nivel*/
  def getListaSemillas (nivel : Int) : List[Int] = nivel match {
    case 1 => List(2)
    case 2 => List(2, 4)
    case 3 => List(2, 4, 8)
    case 4 => List(2, 4, 8)
  }
  def movementTxt (movement : Int) : String = movement match {                                                 //DA RECOMENDACIONES DE MOVIENTO AL USUARIO
      case 1 => "Mejor moviento: "+"LEFT"
      case 2 => "Mejor moviento: "+"RIGHT"
      case 3 => "Mejor moviento: "+"UP"
      case 4 => "Mejor moviento: "+"DOWN"
    } 
  //Inicia el juego
  setupJuego()
}