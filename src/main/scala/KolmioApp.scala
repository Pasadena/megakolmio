

/**
  * Created by spokos on 2/3/16.
  */
object KolmioApp {

  type MegaKolmio = List[List[ListKolmio]]

  object Direction extends Enumeration {
    type Direction = Value
    val UP, DOWN = Value
  }

  object Figure extends Enumeration {
    type Figure = Value
    val FOX_BOTTOM, FOX_TOP, RACCOON_BOTTOM, RACCOON_TOP, DEER_BOTTOM, DEER_TOP, EMPTY = Value

    def isMatch(first: Figure.Value, second: Figure.Value): Boolean = {
      first match {
        case FOX_BOTTOM => isSame(second, FOX_TOP)
        case FOX_TOP => isSame(second, FOX_BOTTOM)
        case RACCOON_BOTTOM => isSame(second, RACCOON_TOP)
        case RACCOON_TOP => isSame(second, RACCOON_BOTTOM)
        case DEER_BOTTOM => isSame(second, DEER_TOP)
        case DEER_TOP => isSame(second, DEER_BOTTOM)
        case EMPTY => false
      }
    }
    def isSame(first: Figure.Value, second: Figure.Value) = first == second

    def opposite(first: Figure.Value): Figure.Value = {
      first match {
        case FOX_BOTTOM => FOX_TOP
        case FOX_TOP => FOX_BOTTOM
        case RACCOON_BOTTOM => RACCOON_TOP
        case RACCOON_TOP => RACCOON_BOTTOM
        case DEER_BOTTOM => DEER_TOP
        case DEER_TOP => DEER_BOTTOM
        case EMPTY => EMPTY
      }
    }

    def toString(element: Figure.Value): String = {
      element match {
        case FOX_BOTTOM => "FB"
        case FOX_TOP => "FT"
        case RACCOON_BOTTOM => "RB"
        case RACCOON_TOP => "RT"
        case DEER_BOTTOM => "DB"
        case DEER_TOP => "DT"
        case EMPTY => "E"
      }
    }
  }

  object Side extends Enumeration {
    type Side = Value
    val LEFT, RIGHT, TOP, BOTTOM = Value

    def opposite(side: Side.Value) = side match {
      case LEFT => RIGHT
      case RIGHT => LEFT
      case TOP => BOTTOM
      case BOTTOM => TOP
    }
  }

  case class MiniKolmio(identifier: String, firstSide: Figure.Value, secondSide: Figure.Value, thirdSide: Figure.Value) {
    def fitsNextToNeighbours(neighbours: List[Figure.Value]): Boolean = {
      def fitsNextToKolmio(remainingNeighbours: List[Figure.Value], remainingSides: List[Figure.Value], result: Boolean): Boolean = {
        if(remainingNeighbours.isEmpty) return result
        val firstToMatch = remainingNeighbours.head
        val matches = remainingSides.exists(side => Figure.isMatch(side, firstToMatch))
        fitsNextToKolmio(remainingNeighbours.tail, remainingSides.filter(side => !Figure.isMatch(side, firstToMatch)), matches)
      }
      fitsNextToKolmio(neighbours, List(firstSide, secondSide, thirdSide), false)
    }

    def getFittingSides(neighbours: List[(Figure.Value, Side.Value)]): List[(Figure.Value, Side.Value)] = {
      def fitsNextToKolmio(remainingNeighbours: List[(Figure.Value, Side.Value)], remainingSides: List[Figure.Value], result: List[(Figure.Value, Side.Value)]): List[(Figure.Value, Side.Value)] = {
        if(remainingNeighbours.isEmpty) return result
        val firstToMatch = remainingNeighbours.head
        val matches = remainingSides.exists(side => Figure.isMatch(side, firstToMatch._1))
        val updatedResult = if(matches) (remainingSides.find(side => Figure.isMatch(side, firstToMatch._1)).get, Side.opposite(firstToMatch._2)) :: result else result
        fitsNextToKolmio(remainingNeighbours.tail, remainingSides.filter(side => !Figure.isMatch(side, firstToMatch._1)), updatedResult)
      }
      fitsNextToKolmio(neighbours, List(firstSide, secondSide, thirdSide), List.empty)
    }
  }

  case class ListKolmio(identifier: String, leftSide: Figure.Value, rightSide: Figure.Value, bottomSide: Figure.Value, direction: Direction.Value) {
    override def toString() = {
      s"${identifier} (${Figure.toString(leftSide)} | ${Figure.toString(bottomSide)} | ${Figure.toString(rightSide)} : ${direction.toString})"
    }
    override def equals(other: Any) = other match {
      case listKolmio: ListKolmio => listKolmio.identifier.equals(identifier)
      case _ => false
    }
  }

  case class WeightedKolmio(miniKolmio: MiniKolmio, weight: Int)

  def createMegaKolmio(minikolmios: List[MiniKolmio]): MegaKolmio = {
    if(Math.sqrt(minikolmios.size) % 1.0 != 0 ) throw new IllegalArgumentException("There are incorrect amount of kolmios")

    val rowCount = Math.sqrt(minikolmios.size)
    val longestRowLength = rowCount * 2 - 1

    lazy val triangleTips = List((0, 0), (0, rowCount -1), (rowCount -1, longestRowLength -1))
    lazy val megakolmioContainer: MegaKolmio = fillInitialContainer(List.empty[List[ListKolmio]], 0)

    lazy val foo: List[Figure.Value] = minikolmios.flatMap(ele => ele.firstSide :: ele.secondSide :: List(ele.thirdSide))
    lazy val countsMap: Map[Figure.Value, Int] = foo.groupBy(ele => ele).map(ele => (ele._1, ele._2.length))
    lazy val weightedKolmios = minikolmios.map(ele => WeightedKolmio(ele, countTotalWeight(ele)))

    def countTotalWeight(minikolmio: MiniKolmio): Int = {
      val firstWeight: Int = countsMap.getOrElse(Figure.opposite(minikolmio.firstSide), 0)
      val secondWeight: Int = countsMap.getOrElse(Figure.opposite(minikolmio.secondSide), 0)
      val thirdWeight: Int = countsMap.getOrElse(Figure.opposite(minikolmio.thirdSide), 0)
      firstWeight + secondWeight + thirdWeight
    }

    def fillInitialContainer(container: MegaKolmio, rowNumber: Int = 0): MegaKolmio = {
      if(rowNumber < rowCount) {
        val columnsInRow = rowNumber * 2 + 1
        fillInitialContainer(List.tabulate(columnsInRow)(column => ListKolmio(s"{$rowNumber + $column}", Figure.EMPTY, Figure.EMPTY, Figure.EMPTY, Direction.UP)) :: container, rowNumber +1)
      } else container.reverse
    }

    def fillMegaKolmio(minikolmios: List[MiniKolmio], container: List[List[ListKolmio]], column: Int): MegaKolmio = {
      val oddFillResult = fillOddColumns(minikolmios, container, 0)
      val evenNotTipsResult = fillEvenColumns(oddFillResult._2, oddFillResult._1, 0, !isTipOfTriangle(_,_))
      val pointFillResults = fillEvenColumns(evenNotTipsResult._2, evenNotTipsResult._1, 0, isTipOfTriangle(_,_))
      if(!validateMegaKolmio(pointFillResults._1)) println("Invalid! ")
      pointFillResults._1
    }

    def fillOddColumns(minikolmios: List[MiniKolmio], container: MegaKolmio, currentRow: Int): (MegaKolmio, List[MiniKolmio]) = {
      if(currentRow >= rowCount) return (container, minikolmios)
      val containerRow = container(currentRow)
      val oddColumns = containerRow.filter(column => isOdd(column, containerRow) && isColumnFree(column))
      oddColumns match {
        case Nil => fillOddColumns(minikolmios, container, currentRow +1)
        case _ => {
          val minikolmio = minikolmios.head
          val updatedRow = containerRow.updated(containerRow.indexOf(oddColumns.head),
            ListKolmio(minikolmio.identifier, minikolmio.thirdSide, minikolmio.secondSide, minikolmio.firstSide, Direction.DOWN))
          fillOddColumns(minikolmios.tail, container.updated(currentRow, updatedRow), currentRow)
        }
      }
    }

    def isOdd(column: ListKolmio, row: List[ListKolmio]) = {
      row.indexOf(column) % 2 != 0
    }

    def isEven(column: ListKolmio, row: List[ListKolmio]) = row.indexOf(column) % 2 == 0

    def isTipOfTriangle(spot: (Int, Int)) = triangleTips.contains(spot)

    def isColumnFree(listKolmio: ListKolmio) = listKolmio.leftSide == Figure.EMPTY

    def fillEvenColumns(minikolmios: List[MiniKolmio], container: MegaKolmio, currentRow: Int = 0, tipsOnly: (Int, Int) => Boolean): (MegaKolmio, List[MiniKolmio]) = {
      if(currentRow >= rowCount) return (container, minikolmios)
      val containerRow = container(currentRow)
      val evenColumnsNotTips = containerRow.filter(ele => isEven(ele, containerRow) && isColumnFree(ele) && tipsOnly(container.indexOf(containerRow), containerRow.indexOf(ele)))
      evenColumnsNotTips match {
        case Nil => fillEvenColumns(minikolmios, container, currentRow + 1, tipsOnly)
        case _ => {
          val firstElementToFill = evenColumnsNotTips.head
          val emptySpot = (container.indexOf(containerRow), containerRow.indexOf(firstElementToFill))
          val kolmioThatFits = getFittingKolmio(minikolmios, container, emptySpot)
          if (kolmioThatFits == null) fillEvenColumns(minikolmios, container, currentRow, tipsOnly)
          else {
            val updatedContainer = insertMinikolmio(container, kolmioThatFits, emptySpot)
            fillEvenColumns(minikolmios.filter(ele => ele.identifier != kolmioThatFits._1.identifier), updatedContainer, currentRow, tipsOnly)
          }
        }
      }
    }

    def insertMinikolmio(container: MegaKolmio, instrumentation: (MiniKolmio, List[(Figure.Value, Side.Value)]), spot: (Int, Int)): (MegaKolmio) = {
      val sidesAsList = List(instrumentation._1.firstSide, instrumentation._1.secondSide, instrumentation._1.thirdSide)
      val unusedSides = sidesAsList.filter(ele => !instrumentation._2.map(_._2).contains(ele))
      val newListaKolmio = ListKolmio(instrumentation._1.identifier, instrumentation._2.find(inst => inst._2 == Side.LEFT) match {
        case Some(x) => x._1
        case None => unusedSides.head
      },
        instrumentation._2.find(inst => inst._2 == Side.RIGHT) match {
          case Some(x) => x._1
          case None => unusedSides.head
        },
        instrumentation._2.find(inst => inst._2 == Side.BOTTOM) match {
          case Some(x) => x._1
          case None => unusedSides.head
        },
      Direction.UP
      )
      val updatedRow = container(spot._1).updated(spot._2, newListaKolmio)
      container.updated(spot._1, updatedRow)
    }

    def getFittingKolmio(remainingKolmios: List[MiniKolmio], container: MegaKolmio, spot: (Int, Int)): (MiniKolmio, List[(Figure.Value, Side.Value)]) = {
      if (remainingKolmios.isEmpty) return null
      val figuresNeedMatching = getFiguresAdjacentToSpot(container, spot)
      val fittingSidesInPosition = remainingKolmios.head.getFittingSides(figuresNeedMatching)
      if(fittingSidesInPosition.size == figuresNeedMatching.size) (remainingKolmios.head, fittingSidesInPosition)
      else getFittingKolmio(remainingKolmios.tail, container, spot)
    }

    def getFiguresAdjacentToSpot(container: MegaKolmio, spot: (Int, Int)): List[(Figure.Value, Side.Value)] = {
      var adjacentKolmios = List.empty[(Figure.Value, Side.Value)]
      if(spotExists(container, (spot._1 -1, spot._2 -1))) adjacentKolmios = (container(spot._1 -1)(spot._2 - 1).bottomSide, Side.BOTTOM) :: adjacentKolmios
      if(spotExists(container, (spot._1 +1, spot._2 +1))) adjacentKolmios = (container(spot._1 +1)(spot._2 + 1).bottomSide, Side.TOP) :: adjacentKolmios
      if(spotExists(container, (spot._1, spot._2 -1))) adjacentKolmios = (container(spot._1)(spot._2 -1).rightSide, Side.RIGHT) :: adjacentKolmios
      if(spotExists(container, (spot._1, spot._2 +1))) adjacentKolmios = (container(spot._1)(spot._2 +1).leftSide, Side.LEFT) :: adjacentKolmios
      adjacentKolmios
    }

    val sortedKolmios = weightedKolmios.sortWith((first, second) => first.weight < second.weight)
    val result = fillMegaKolmio(sortedKolmios.map(ele => ele.miniKolmio), megakolmioContainer, 0)
    result
  }

  def validateMegaKolmio(megakolmio: MegaKolmio): Boolean = {
    var invalidKolmios = List.empty[ListKolmio]
    megakolmio.foreach { row =>
      row foreach { column =>
        val columnIndex = row.indexOf(column)
        val rowIndex = megakolmio.indexOf(row)
        if(spotExists(megakolmio, (rowIndex, columnIndex -1))) {
          Figure.isMatch(column.leftSide, megakolmio(rowIndex)(columnIndex -1).rightSide) match {
            case false => invalidKolmios = column :: invalidKolmios
            case true =>
          }
        }
        if(spotExists(megakolmio, (rowIndex, columnIndex +1))) {
          Figure.isMatch(column.rightSide, megakolmio(rowIndex)(columnIndex +1).leftSide) match {
            case false => invalidKolmios = column :: invalidKolmios
            case true =>
          }
        }
        if(spotExists(megakolmio, (rowIndex -1, columnIndex -1))) {
          Figure.isMatch(column.bottomSide, megakolmio(rowIndex -1)(columnIndex  -1).bottomSide) match {
            case false => invalidKolmios = column :: invalidKolmios
            case true =>
          }
        }
        if(spotExists(megakolmio, (rowIndex +1, columnIndex +1))) {
          Figure.isMatch(column.bottomSide, megakolmio(rowIndex +1)(columnIndex  +1).bottomSide) match {
            case false => invalidKolmios = column :: invalidKolmios
            case true =>
          }
        }
      }
    }
    System.out.println(invalidKolmios)
    invalidKolmios.isEmpty
  }

  def spotExists(container: MegaKolmio, spot: (Int, Int)) = (spot._1 >= 0 && spot._1 < container.size) && (spot._2 >= 0 && spot._2 < container(spot._1).size)

  def printMegaKolmio(megaKolmio: MegaKolmio) = {
    megaKolmio.foreach { row =>
      System.out.println(row)
    }
  }

  def main(args: Array[String]): Unit = {
    val minikolmiosTwoRows = List(MiniKolmio("P1", Figure.FOX_TOP, Figure.FOX_BOTTOM, Figure.DEER_TOP),
      MiniKolmio("P9", Figure.FOX_BOTTOM, Figure.DEER_TOP, Figure.DEER_BOTTOM),
      MiniKolmio("P2", Figure.DEER_TOP, Figure.FOX_BOTTOM, Figure.RACCOON_BOTTOM),
      MiniKolmio("P6", Figure.RACCOON_BOTTOM, Figure.FOX_BOTTOM, Figure.RACCOON_TOP))
    val minikolmios = List(MiniKolmio("P1", Figure.FOX_TOP, Figure.FOX_BOTTOM, Figure.DEER_TOP))

    val minikolmiosThreeRows = List(MiniKolmio("P1", Figure.FOX_TOP, Figure.FOX_BOTTOM, Figure.DEER_TOP),
      MiniKolmio("P2", Figure.DEER_TOP, Figure.FOX_BOTTOM, Figure.RACCOON_BOTTOM),
      MiniKolmio("P3", Figure.DEER_TOP, Figure.FOX_BOTTOM, Figure.FOX_TOP),
      MiniKolmio("P4", Figure.DEER_TOP, Figure.DEER_BOTTOM, Figure.FOX_BOTTOM),
      MiniKolmio("P5", Figure.DEER_TOP, Figure.RACCOON_BOTTOM, Figure.DEER_BOTTOM),
      MiniKolmio("P6", Figure.RACCOON_BOTTOM, Figure.FOX_BOTTOM, Figure.RACCOON_TOP),
      MiniKolmio("P7", Figure.FOX_BOTTOM, Figure.RACCOON_TOP, Figure.FOX_TOP),
      MiniKolmio("P8", Figure.RACCOON_TOP, Figure.DEER_TOP, Figure.RACCOON_BOTTOM),
      MiniKolmio("P9", Figure.FOX_BOTTOM, Figure.DEER_TOP, Figure.DEER_BOTTOM)
      )

    val megaKolmio = this.createMegaKolmio(minikolmiosThreeRows)
    printMegaKolmio(megaKolmio)
  }
}
