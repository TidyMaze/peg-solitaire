import java.time.Instant
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.{dynamics, postfixOps}
import scala.util.Random

object Main {

  type Grid = Array[Array[Char]]

  case class Coord(x: Int, y: Int)

  case class Action(from: Coord, to: Coord, over: Coord) {
    override def toString: String = s"${from.x}x${from.y}-${to.x}x${to.y}"
  }

  val PurgeTrigger = 1000000
  val MaxSolutions = 1000

  val offsets = Seq(Coord(0, -1), Coord(0, 1), Coord(-1, 0), Coord(1, 0))

  val englishMapRaw =
    """##ooo##
      |##ooo##
      |ooooooo
      |ooo.ooo
      |ooooooo
      |##ooo##
      |##ooo##""".stripMargin

  val englishMap: Seq[Seq[Char]] = englishMapRaw.split("\n").map(_.toIndexedSeq).toSeq
  
  def main(args: Array[String]): Unit = {
    val coordsInMap = allCoords(englishMap)
    solveGrid(coordsInMap, englishMap.map(_.toArray).toArray, ListBuffer())
  }

  // avoid avaluating an already-met state
  var seen: mutable.HashSet[Long] = mutable.HashSet[Long]()
  var countsWins = 0
  var countsSeen = 0
  var best = 35

  var countNodes = 0L
  var start = Instant.now()

  val zobristKeys = englishMap.map { l =>
    l.map(c =>
      Random.nextLong()
    )
  }
  
  println(s"zobrist keys: $zobristKeys")

  def zobrist(g: Grid) = {
    var res: Long = 0
    (0 until 7).foreach { i =>
      (0 until 7).foreach { j =>
        if (g(i)(j) == 'o') {
          res ^= zobristKeys(i)(j)
        }
      }
    }
    res
  }

  def hashGrid(g: Grid) = zobrist(g)

  def verticalMirror(g: Grid) = {
    var res = Array.ofDim[Char](g.size, g.size)
    (0 until g.size).foreach { i =>
      (0 until g.size).foreach { j =>
        res(i)(g.size - j - 1) = g(i)(j)
      }
    }
    res.toArray
  }

  def horizontalMirror(g: Grid) = {
    var res = Array.ofDim[Char](g.size, g.size)
    (0 until g.size).foreach { i =>
      (0 until g.size).foreach { j =>
        res(g.size - i - 1)(j) = g(i)(j)
      }
    }
    res.toArray
  }

  def rotate90(g: Grid) = {
    var res = Array.ofDim[Char](g.size, g.size)
    (0 until g.size).foreach { i =>
      (0 until g.size).foreach { j =>
        res(i)(j) = g(g.size - j - 1)(i)
      }
    }
    res.toArray
  }

  def rotate180(g: Grid) = {
    var res = Array.ofDim[Char](g.size, g.size)
    (0 until g.size).foreach { i =>
      (0 until g.size).foreach { j =>
        res(i)(j) = g(g.size - i - 1)(g.size - j - 1)
      }
    }
    res.toArray
  }

  def rotate270(g: Grid) = {
    var res = Array.ofDim[Char](g.size, g.size)
    (0 until g.size).foreach { i =>
      (0 until g.size).foreach { j =>
        res(i)(j) = g(j)(g.size - i - 1)
      }
    }
    res.toArray
  }

  def generateDisplayBoardSteps(map: Seq[Seq[Char]], hist: Seq[Action]): String = {
    val mutableMap = map.map(_.toArray).toArray
    val steps: Seq[Grid] = hist.scanLeft(mutableMap)(playAction)
    (0 until map.length).map(i => steps.map(_ (i).mkString("")).mkString(" ")).mkString("\n")
  }

  def solveGrid(coordsInMap: Seq[Coord], map: Grid, hist: ListBuffer[Action]): Unit = {
    countNodes += 1L
    if ((countNodes % PurgeTrigger) == 0) {
      val elapsedMillis = Instant.now().toEpochMilli - start.toEpochMilli
      println(s"Speed: ${countNodes / elapsedMillis}K op/s out of ${countNodes / 1000}K nodes. ${countsWins} solutions so far. Seen ${seen.size}. Count seen ${countsSeen}")
    }

    val hash = hashGrid(map)
    val alreadySeen = seen.contains(hash)

    if (alreadySeen) {
      countsSeen += 1
      return
    } else {
      val hm = horizontalMirror(map)
      val vm = verticalMirror(map)
      seen.addAll(Seq(
        hash,
        hashGrid(hm),
        hashGrid(vm),
        hashGrid(rotate90(map)),
        hashGrid(rotate90(hm)),
        hashGrid(rotate90(vm)),
        hashGrid(rotate180(map)),
        hashGrid(rotate180(hm)),
        hashGrid(rotate180(vm)),
        hashGrid(rotate270(map)),
        hashGrid(rotate270(hm)),
        hashGrid(rotate270(vm)),
      ))
      //      println(seen.knownSize)
    }

    if (won(map)) {
      countsWins += 1
      println(s"SOLUTION ${countsWins}")
    } else {
      coordsInMap.foreach { originCoord =>
        if (hasPeg(map, originCoord)) {
          offsets.foreach { o =>
            val jumpedCoord = addOffset(originCoord, o)
            if (inMap(map, jumpedCoord) && hasPeg(map, jumpedCoord)) {
              val landingCoord = addOffset(jumpedCoord, o)
              if (isFree(map, landingCoord)) {
                val a = Action(originCoord, landingCoord, jumpedCoord)
                // play action
                map(originCoord.y)(originCoord.x) = '.'
                map(landingCoord.y)(landingCoord.x) = 'o'
                map(jumpedCoord.y)(jumpedCoord.x) = '.'

                // add history
                hist.addOne(a)

                // solve recursively
                solveGrid(coordsInMap, map, hist)

                // revert action
                map(originCoord.y)(originCoord.x) = 'o'
                map(landingCoord.y)(landingCoord.x) = '.'
                map(jumpedCoord.y)(jumpedCoord.x) = 'o'

                // revert history
                hist.remove(hist.size - 1)
                // solveGrid(coordsInMap, playAction(map, a))
              }
            }
          }
        }
      }
    }
  }

  def printGrid(g: Grid) = {
    val sb = new StringBuilder()
    var i = 0
    var j = 0
    while (i < g.size) {
      j = 0
      while (j < g.head.size) {
        sb.append(g(i)(j))
        j += 1
      }
      sb.append("\n")
      i += 1
    }
    println(sb.toString())
  }

  val allCoords = (map: IterableOnce[IterableOnce[Char]]) => for {
    i <- (0 until map.size)
    j <- (0 until map.size)
  } yield Coord(j, i)

  // fast solution check
  def won(g: Grid): Boolean = {
    var found = false
    for (i <- (0 until g.size)) {
      for (j <- (0 until g.size)) {
        if (g(i)(j) == 'o') {
          if (!found && i == 3 && j == 3) {
            found = true
          } else {
            return false
          }
        }
      }
    }
    return found
  }

  val playAction = (g: Grid, a: Action) => {
    val resG = cloneGrid(g)
    resG(a.from.y)(a.from.x) = '.'
    resG(a.to.y)(a.to.x) = 'o'
    resG(a.over.y)(a.over.x) = '.'
    resG
  }

  val cloneGrid: Grid => Grid = _.map(_.clone())

  def isFree(map: Grid, c: Coord) = inMap(map, c) && map(c.y)(c.x) == '.'

  def hasPeg(map: Grid, c: Coord) = map(c.y)(c.x) == 'o'

  def addOffset(coord: Coord, offset: Coord) = Coord(coord.x + offset.x, coord.y + offset.y)

  def inMap(map: Grid, c: Coord) = c.x >= 0 && c.x < map.size && c.y >= 0 && c.y < map.size
}
