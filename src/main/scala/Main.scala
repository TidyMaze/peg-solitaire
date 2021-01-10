import java.time.Instant
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

object Main {

  type Grid = Array[Array[Char]]

  case class Coord(x: Int, y: Int)

  case class Action(from: Coord, to: Coord, over: Coord) {
    override def toString: String = s"(${from.x}, ${from.y})->(${to.x}, ${to.y})"
  }

  val PurgeTrigger = 100000

  val offsets = Seq(Coord(0, -1), Coord(0, 1), Coord(-1, 0), Coord(1, 0))

  def main(args: Array[String]): Unit = {

    val englishMapRaw =
      """##ooo##
        |##ooo##
        |ooooooo
        |ooooooo
        |ooooooo
        |##ooo##
        |##ooo##""".stripMargin

    val map = englishMapRaw.split("\n").map(_.toCharArray)

    //    printGrid(map)

    map(3)(3) = '.'
    //    printGrid(map)

    val coordsInMap = allCoords(map)

    solveGrid(coordsInMap, map, ListBuffer())
  }

  // avoid avaluating an already-met state
  var seen: mutable.HashMap[Int, Int] = mutable.HashMap[Int, Int]()
  var countsWins = 0
  var countsSeen = 0
  var best = 35

  var countNodes = 0
  var start = Instant.now()

  def hashGrid(g: Grid) = showGrid(g).hashCode()

  def solveGrid(coordsInMap: Seq[Coord], map: Grid, hist: ListBuffer[Action]): Unit = {
    countNodes += 1
    if ((seen.size % PurgeTrigger) == 0) {
      val elapsedMillis = Instant.now().toEpochMilli - start.toEpochMilli
      println(s"Speed: ${countNodes / elapsedMillis}K op/s out of ${countNodes / 1000}K nodes. Count seen ${countsSeen / 1000}K out of ${seen.size / 1000}K nodes")

      if(seen.size > 0) {

        val keep = PurgeTrigger / 2

        val sorted = seen.toSeq.sortBy(_._2)(Ordering.Int.reverse)
        seen = mutable.HashMap(sorted.take(keep):_*)
        println(seen.size + " and best " + sorted.head._2)
      }
    }

    val hash = hashGrid(map)

    val current = seen.getOrElse(hash, 0)
    seen.update(hash, current + 1)
    
    if (current > 0) {
      countsSeen += 1
      return
    }

    if (won(map)) {
      countsWins += 1
      printGrid(map)
      println(s"SUCCESS ${countsWins}: ${hist}")
    } else {
      val playable = coordsInMap.view.flatMap {
        case originCoord if hasPeg(map, originCoord) =>
          offsets.flatMap(getEventualAction(map, originCoord, _))
        case _ => Nil
      }

      playable foreach { a =>
        playActionMut(map, a)
        hist.addOne(a)
        solveGrid(coordsInMap, map, hist)
        revertActionMut(map, a)
        hist.remove(hist.size-1)
        // solveGrid(coordsInMap, playAction(map, a))
      }
    }
  }

  def getEventualAction(map: Grid, originCoord: Coord, direction: Coord): Option[Action] = {
    val jumpedCoord = addOffset(originCoord, direction)
    if (hasPeg(map, jumpedCoord)){
      val landingCoord = addOffset(jumpedCoord, direction)
      if(isFree(map, landingCoord)) {
        Some(Action(originCoord, landingCoord, jumpedCoord))
      } else {
        None
      }
    } else {
      None
    }
  }

  val showGrid: Grid => String = _.map(_.mkString("")).mkString("\n")
  val printGrid = showGrid andThen println andThen (_ => println)

  val allCoords = (map: Grid) => for {
    i <- (0 until map.size)
    j <- (0 until map.size)
  } yield Coord(j, i)

  val won: Grid => Boolean = score(_) == 1

  val score: Grid => Int = _.flatten.count(_ == 'o')

  val playAction = (g: Grid, a: Action) => {
    val resG = cloneGrid(g)
    resG(a.from.y)(a.from.x) = '.'
    resG(a.to.y)(a.to.x) = 'o'
    resG(a.over.y)(a.over.x) = '.'
    resG
  }

  val playActionMut = (g: Grid, a: Action) => {
    g(a.from.y)(a.from.x) = '.'
    g(a.to.y)(a.to.x) = 'o'
    g(a.over.y)(a.over.x) = '.'
  }

  val revertActionMut = (g: Grid, a: Action) => {
    g(a.from.y)(a.from.x) = 'o'
    g(a.to.y)(a.to.x) = '.'
    g(a.over.y)(a.over.x) = 'o'
  }

  val cloneGrid: Grid => Grid = _.map(_.clone())

  val isFree = (map: Grid, c: Coord) => inMap(map, c) && map(c.y)(c.x) == '.'

  val hasPeg = (map: Grid, c: Coord) => inMap(map, c) && map(c.y)(c.x) == 'o'

  val addOffset = (coord: Coord, offset: Coord) => Coord(coord.x + offset.x, coord.y + offset.y)

  val inMap = (map: Grid, c: Coord) => c.x >= 0 && c.x < map.size && c.y >= 0 && c.y < map.size
}
