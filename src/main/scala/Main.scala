import scala.language.postfixOps

object Main {

  type Grid = Array[Array[Char]]
  case class Coord(x: Int, y: Int)
  case class Action(from: Coord, to: Coord, over: Coord)

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

    printGrid(map)

    map(3)(3) = '.'
    printGrid(map)

    solveGrid(map)
  }

  val seen: scala.collection.mutable.Set[Int] = scala.collection.mutable.Set.empty[Int]
  var countsWins = 0
  var best = 35

  def solveGrid(map: Grid): Unit = {
    if (seen.contains(map.hashCode())) {
      return
    }

    if (won(map)) {
      countsWins += 1
      printGrid(map)
      println(s"SUCCESS ${countsWins}!")
    } else {
      seen.add(map.hashCode())

      val sc = score(map)

      if (sc < best) {
        best = sc
        println(s"best ${best}")
      }

      val playable = allCoords(map).flatMap { case originCoord =>
        if (hasPeg(map, originCoord)) {
          offsets.flatMap { direction =>
            val jumpedCoord = addOffset(originCoord, direction)
            val landingCoord = addOffset(jumpedCoord, direction)
            if (hasPeg(map, jumpedCoord) && isFree(map, landingCoord)) {
              Some(Action(originCoord, landingCoord, jumpedCoord))
            } else {
              None
            }
          }
        } else {
          Nil
        }
      }

      playable foreach { a =>
        solveGrid(playAction(map, a))
      }
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

  val cloneGrid: Grid => Grid = _.map(_.clone())

  val isFree = (map: Grid, c: Coord) => inMap(map, c) && map(c.y)(c.x) == '.'

  val hasPeg = (map: Grid, c: Coord) => inMap(map, c) && map(c.y)(c.x) == 'o'

  val addOffset = (coord: Coord, offset: Coord) => Coord(coord.x + offset.x, coord.y + offset.y)

  val inMap = (map: Grid, c: Coord) => c.x >= 0 && c.x < map.size && c.y >= 0 && c.y < map.size
}
