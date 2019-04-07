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
      else {try{
        val cols = getCols(nivel)
        val movimiento = getNumber("Realizar movimiento", 1, 4)          //Acción del usuario
        val preTablero = preMover (tablero, cols, movimiento)            //Se rota el tablero de ser necesario según el movimiento elegido
        val t_sin_ceros = quitarCeros (preTablero, cols)                 //Se eliminan los ceros del tablero
        val piezas_nuevas = crearPiezasNuevas (t_sin_ceros, cols)        //Se crea una lista con las piezas nuevas que aparecerán en el tablero
        val nuevo_puntos = sumaTablero(piezas_nuevas) + puntos           //Se suman las piezas nuevas para obetener los puntos
        val nuevo_tablero = ensamblarTablero (t_sin_ceros, piezas_nuevas)//Se colocan las piezas sobre le tablero y se borran las que no hagan falta
        val nuevo_t_sin_ceros = quitarCeros (nuevo_tablero, cols)        //Se vuelven a quitar los ceros
        val postTablero = postMover (nuevo_t_sin_ceros, cols, movimiento)//Se deja el tablero en su rotación original
        gameLoop (nivel, vidas, nuevo_puntos, puntos_totales, colocarSemillas(postTablero, getEmptyPositions(postTablero), nivel))
      }catch {case _: Throwable => iniciarJuegoNuevo (nivel, vidas, puntos)}}}//Si nos introducen una q para salir antes
    //Se muestra el tablero
    printPuntos("Puntos Totales", puntos)
    printTablero(tablero, getCols(nivel))
    continue(tablero, nivel)
  }
  /*Imprime los puntos que se han sonseguido*/
  def printPuntos (text : String ,puntos : Int) = {
    print("-----------------------\n")
    print(text +": " + puntos + "\n")
    print("-----------------------\n")
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
    def tomarFila (l1 : List[Int], cols : Int) :  List [Int] = cols match{
      case 1 => List(l1.head)
      case _ => l1.head::tomarFila (l1.tail, cols-1)
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
    val zeros = _fillZeros (tablero, cols, facke_cols-cols)
    printTablero(zeros, facke_cols)
    printTablero(rotate(zeros, facke_cols), facke_cols)
    printTablero(quitaZeros(rotate(zeros, facke_cols), cols, zeros.length, facke_cols), cols)
    quitaZeros(rotate(zeros, facke_cols), cols, zeros.length, facke_cols)
  }
  /*Rota la matriz hasta la posición nesecaria para realizar el movimento en el sentido adecuado*/
  def preMover (tablero : List[Int], cols : Int, movimiento : Int) : List [Int] = movimiento match {
    case 1 => {tablero}
    case 2 => {flip(tablero, cols)}
    case 3 => {rotate90(tablero,cols)}
    case 4 => {rotate90(rotate90(rotate90(tablero,cols),cols),cols)}
  }
  /*Rota la matriz hasta dejarla en su posición original*/
  def postMover (tablero : List[Int], cols : Int, movimiento : Int) : List [Int] = movimiento match {
    case 1 => {tablero}
    case 2 => {flip(tablero, cols)}
    case 3 => {rotate90(rotate90(rotate90(tablero,cols),cols),cols)}
    case 4 => {rotate90(tablero,cols)}
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
  /*Dice si dos tablero son el mismo*/
  def mismoTablero (tablero : List[Int], tablero_anterior : List[Int]) : Boolean = {
    if ((tablero == Nil) || (tablero_anterior == Nil)) true
    else if (tablero.head == tablero_anterior.head) mismoTablero(tablero.tail, tablero_anterior.tail)
    else false
  }
  /*Indica si un tablero está lleno de piezas*/
  def isFull (tablero : List[Int]) : Boolean = tablero match{
    case Nil => true
    case head::tail => if (head == 0) false else isFull(tail)
  }
  /*Suma todas las piezas de un tablero, se usa para contar los puntos*/
  def sumaTablero (tablero : List[Int]) : Int = tablero match {
    case Nil => 0
    case head::tail => head+sumaTablero(tail)
  }
  /*Crea una lista de ceros con el tamaño indicado*/
  def generarLista(col:Int): List[Int] = col match{
      case 0 => Nil
      case _ => (0)::generarLista(col-1)
    }
  /*Crea una tablero inicial nuevo con las caracetrísticas que le correspondan según el nivel elegido*/
  def crearTablero (nivel:Int) : List[Int] = {
    val cols = getCols (nivel)
    val cantidad_semilla = getCantidadSemillas (nivel)
    val lista_semillas = getListaSemillas (nivel)
    val tablero = generarLista(cols*cols)
    colocarSemillas(tablero, getEmptyPositions(tablero), cantidad_semilla, lista_semillas)
  }
  /*Pone tantas semillas aleatorias como se indique sienpre que quede hueco en el tablero*/
  def colocarSemillas(tablero:List[Int], semillas:List[Int], cantidad : Int, valores:List[Int] = List(2,4)): List[Int] = {
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
		if (semillas.length == 0) tablero
		if (cantidad == 0) tablero
		else {
		  val posicion_semilla = (Math.random()*semillas.length).toInt //Posición aleatoria de una posición libre para colocar una semilla
  		val posicion_valor = (Math.random()*valores.length).toInt    //Posición aleatoria para encontrar valor de la semilla de la semilla
  		val semilla = buscarIndice(semillas, posicion_semilla)       //Posición libre se una semilla
  		val valor = buscarIndice(valores, posicion_valor)            //Valor se una semilla
		  val siguientes_semillas = eliminarIndice (semillas, posicion_semilla)
		  colocarSemillas(ponerSemilla(tablero, semilla, valor), siguientes_semillas, cantidad-1, valores)}
  }
   
  /*Pide al usuario un número dentro de un rango Con una q se puede terminar la partida con antelación*/
  def getNumber (text : String, min : Int, max : Int) : Int = {
    print (text + " [ " + min + ", " + max +" ] : ")
    try{
      val input = reader.nextInt();
      if (isBetween(input, min,max)) input
      else {print("ERROR ENTRADA FUERA DE RANGO\n"); getNumber (text, min, max)}
    }catch  {case _: Throwable => { if (reader.nextLine()=="q") throw new Exception 
                                    else print("ERROR ENTRADA INVALIDA\n");getNumber (text, min, max)}}
  }
  /*Comprueba si un número está dentro del rango*/
  def isBetween (input : Int, min : Int, max : Int) : Boolean = (input >= min) && (input <= max)
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
}