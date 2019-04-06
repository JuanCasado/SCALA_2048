package main

import java.util.Random
import java.util.Scanner

object main extends App {
  val reader = new Scanner(System.in)
  setupJuego()
  /* Se piden los datos iniciales al usuario para poder comenzar el juego*/
  def setupJuego() = {
    print("\n>>>>>>           2048           <<<<<<\n")
    println("--------------------------------------------")
    val vidas= 3  
    val nivel = getNumber("Seleccione nivel", 1 , 4)
    gameLoop (nivel, vidas, 0, 0, crearTablero (nivel))
  }
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
  /*Muestra el tablero por pantallla*/
  def printTablero (tablero:List[Int], cols : Int) = {
    /*Da formato a cada elemento de la matriz*/
    def _format (element : Int, x : Int, y : Int, cols : Int) : String = x match {
      case  0   => "\n" +_buildString("-",20*cols) + "|\t" + y + "\t|\t" + element + "\t|"
      case  _   =>  "\t" + element + "\t|"
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
  
  /*Comienza una partida nueva*/
  def iniciarJuegoNuevo (nivel:Int, vidas : Int, puntos : Int) : Int = {
    printPuntos("Puntos Acumulados", puntos)
    vidas match {
    case 1 => print("Te quedaste sin vidas!!!"); 0
    case _ => if(getNumber("Desea jugar de nuevo?", 0, 1) == 1){gameLoop(nivel, vidas-1, 0, puntos, crearTablero(nivel))}
              else {print("Gracias por jugar, adios!!"); 0}
  }}
  /*Bucle principal del juego*/
  def gameLoop (nivel : Int, vidas : Int, puntos : Int, puntos_totales : Int, tablero : List[Int]) : Int = {
    def continue (tablero : List[Int], nivel : Int) : Int = {
      if (isFull(tablero)) iniciarJuegoNuevo(nivel, vidas, puntos + puntos_totales)
      else {
        val cols = getCols(nivel)
        val movimiento = getNumber("Realizar movimiento", 1, 4)
        val preTablero = preMover (tablero, cols, movimiento)
        val t_sin_ceros = quitarCeros (preTablero, cols)
        val piezas_nuevas = crearPiezasNuevas (t_sin_ceros, cols)
        val nuevo_puntos = sumaTablero(piezas_nuevas) + puntos
        val nuevo_tablero = ensamblarTablero (t_sin_ceros, piezas_nuevas)
        val postTablero = postMover (nuevo_tablero, cols, movimiento)
        gameLoop (nivel, vidas, nuevo_puntos, puntos_totales, colocarSemillas(postTablero, getEmptyPositions(postTablero), nivel))
      }
    }
    printPuntos("Puntos Totales", puntos)
    printTablero(tablero, getCols(nivel))
    continue(tablero, nivel)
  }
  
  def printPuntos (text : String ,puntos : Int) = {
    print("-----------------------\n")
    print(text +": " + puntos + "\n")
    print("-----------------------\n")
  }
  
  def flip (tablero : List[Int], cols : Int) : List [Int] ={
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
  
  def preMover (tablero : List[Int], cols : Int, movimiento : Int) : List [Int] = movimiento match {
    case 1 => {tablero}
    case 2 => {flip(tablero, cols)}
    case 3 => {tablero}
    case 4 => {tablero}
  }
  def postMover (tablero : List[Int], cols : Int, movimiento : Int) : List [Int] = movimiento match {
    case 1 => {tablero}
    case 2 => {flip(tablero, cols)}
    case 3 => {tablero}
    case 4 => {tablero}
  }
  
  def crearPiezasNuevas (tablero : List[Int], cols : Int) : List[Int] = {
    def _crearPiezasNuevas (tablero : List[Int], cols : Int, last_seen : Int, amount_seen : Int) : List[Int] = {
      def _nuevaPieza (pieza_actual : Int, last_seen : Int, amount_seen : Int) : Int = {
          if ((pieza_actual == last_seen) && ((amount_seen % 2) == 0)) last_seen*2
          else 0
      }
      def _nextSeen (pieza_actual : Int, last_seen : Int) : Int = {
        if (pieza_actual == 0) last_seen
        else pieza_actual
      }
      if (tablero == Nil) return List()
      val pieza = _nuevaPieza (tablero.head, last_seen, amount_seen)
      if (_finFila(tablero.length, cols)) pieza::_crearPiezasNuevas (tablero.tail, cols, 0, 0)
      else pieza::_crearPiezasNuevas (tablero.tail, cols,  _nextSeen(tablero.head, last_seen), ammountContinued(amount_seen,tablero.head, last_seen))
    }
    _crearPiezasNuevas (tablero, cols, 0, 0)
  }
  
  def ammountContinued (amount_seen : Int, pieza_actual : Int, last_seen : Int) : Int = {
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
  
  def _finFila (length : Int, cols : Int) : Boolean = ((length-1) % cols) == 0
  
  def _inicioFila (length : Int, cols : Int) : Boolean = ((length) % cols) == 0
  
  
  def ensamblarTablero (tablero : List[Int], nuevas_piezas : List[Int]) : List[Int] = {
    def _ensamblarTablero (tablero : List[Int], nuevas_piezas : List[Int], omitir : Int) : List[Int] = nuevas_piezas match {
      case Nil => tablero
      case head::tail => head match {
        case 0 => (tablero.head*omitir)::_ensamblarTablero (tablero.tail, tail, 1)
        case _ => head::_ensamblarTablero (tablero.tail, tail, 0)}}
    _ensamblarTablero (tablero, nuevas_piezas.tail, 1)
  }
  
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
  
  def mismoTablero (tablero : List[Int], tablero_anterior : List[Int]) : Boolean = {
    if ((tablero == Nil) || (tablero_anterior == Nil)) true
    else if (tablero.head == tablero_anterior.head) mismoTablero(tablero.tail, tablero_anterior.tail)
    else false
  }
  
  def isFull (tablero : List[Int]) : Boolean = tablero match{
    case Nil => true
    case head::tail => if (head == 0) false else isFull(tail)
  }
  
  def sumaTablero (tablero : List[Int]) : Int = tablero match {
    case Nil => 0
    case head::tail => head+sumaTablero(tail)
  }
  
  def generarLista(col:Int): List[Int] = col match{
      case 0 => Nil
      case _ => (0)::generarLista(col-1)
    }
  
  def crearTablero (nivel:Int) : List[Int] = {
    val cols = getCols (nivel)
    val cantidad_semilla = getCantidadSemillas (nivel)
    val lista_semillas = getListaSemillas (nivel)
    val tablero = generarLista(cols*cols)
    colocarSemillas(tablero, getEmptyPositions(tablero), cantidad_semilla, lista_semillas)
  }
  
  def buscarIndice(l:List[Int], posicion: Int) : Int = {
    def _buscarIndice(l:List[Int], posicion: Int) : Int = l match{
      case Nil => 0
      case _ => 
        if (l.length == posicion) l.head
        else _buscarIndice(l.tail, posicion)}
    _buscarIndice(l, l.length - posicion)
  }
  
  def eliminarIndice(l:List[Int], posicion: Int) : List[Int] = {
    def _eliminarIndice(l:List[Int], posicion: Int) : List[Int] = l match{
      case Nil => Nil
      case _ => 
        if (l.length == posicion) l.tail
        else l.head::_eliminarIndice(l.tail, posicion)}
    _eliminarIndice(l, l.length - posicion)
  }
  
  def ponerIndice (l:List[Int], posicion: Int, valor: Int) : List[Int] = {
    def _ponerIndice (l:List[Int], posicion: Int, valor: Int) : List[Int] = l match{
      case Nil => Nil
      case _ =>  
        if (l.length == posicion) valor::l.tail
        else l.head::_ponerIndice(l.tail, posicion, valor)}
    _ponerIndice(l, l.length - posicion + 1, valor)
  }
  
  
  def colocarSemillas(tablero:List[Int], semillas:List[Int], cantidad : Int, valores:List[Int] = List(2,4)): List[Int] = {
  		if (semillas.length == 0) tablero
  		if (cantidad == 0) tablero
  		else {
  		  val posicion_semilla = (Math.random()*semillas.length).toInt
    		val posicion_valor = (Math.random()*valores.length).toInt
    		val semilla = buscarIndice(semillas, posicion_semilla)
    		val valor = buscarIndice(valores, posicion_valor)
  		  val siguientes_semillas = eliminarIndice (semillas, posicion_semilla)
  		  colocarSemillas(ponerIndice(tablero, semilla, valor), siguientes_semillas, cantidad-1, valores)}
  }
   
  /*Pide al usuario un número dentro de un rango*/
  def getNumber (text : String, min : Int, max : Int) : Int = {
    print (text + " [ " + min + ", " + max +" ] : ")
    try{
      val input = reader.nextInt();
      if (isBetween(input, min,max)) input
      else {print("ERROR ENTRADA FUERA DE RANGO\n"); getNumber (text, min, max)}
    }catch{case _ => {print("ERROR ENTRADA INVALIDA\n"); getNumber (text, min, max)}}
  }
  /*Comprueba si un número está dentro del rango*/
  def isBetween (input : Int, min : Int, max : Int) : Boolean = (input >= min) && (input <= max)
  
  def getCols (nivel : Int) : Int = nivel match {
    case 1 => 4
    case 2 => 9
    case 3 => 14  
    case 4 => 17
  }
  
  def getCantidadSemillas (nivel : Int) : Int = nivel match {
    case 1 => 1
    case 2 => 2
    case 3 => 5  
    case 4 => 5
  }
  
  def getListaSemillas (nivel : Int) : List[Int] = nivel match {
    case 1 => List(2)
    case 2 => List(2, 4)
    case 3 => List(2, 4, 8)
    case 4 => List(2, 4, 8)
  }
}