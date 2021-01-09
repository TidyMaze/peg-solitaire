import scala.language.postfixOps

object Main {

  type Grid = Array[Array[Char]]
  case class Coord(x: Int, y: Int)

  case class Action(from: Coord, to: Coord, over: Coord)

  val showGrid: Grid => String = _.map(_.mkString("")).mkString("\n")
  val printGrid = {
    showGrid andThen println andThen (_ => println)
  }

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

  def won(g: Grid): Boolean = score(g) == 1

  def score(g: Grid): Int = g.flatten.count(_ == 'o')

  val seen: scala.collection.mutable.Set[Int] = scala.collection.mutable.Set.empty[Int]
  var countsWins = 0
  var best = 35

  def allCoords(map: Grid): Seq[Coord] = for {
    i <- (0 until map.size)
    j <- (0 until map.size)
  } yield Coord(j, i)

  private def solveGrid(map: Array[Array[Char]]): Unit = {
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

      val playable = allCoords(map).flatMap { case c =>
        if (hasPeg(map, c)) {
          offsets.flatMap { o =>
            val d = addOffset(c, o)
            val r = addOffset(d, o)
            if (hasPeg(map, d) && isFree(map, r)) {
              Some(Action(c, r, d))
            } else {
              None
            }
          }
        } else {
          Nil
        }
      }

      playable foreach { a =>
        val resGrid = playAction(map, a)
        solveGrid(resGrid)
      }
    }
  }

  def playAction(g: Grid, a: Action): Grid = {
    val resG = cloneGrid(g)
    resG(a.from.y)(a.from.x) = '.'
    resG(a.to.y)(a.to.x) = 'o'
    resG(a.over.y)(a.over.x) = '.'
    resG
  }

  def cloneGrid(g: Grid): Grid = g.map(_.clone())

  def isFree(map: Array[Array[Char]], c: Coord): Boolean = inMap(map, c) && map(c.y)(c.x) == '.'

  def hasPeg(map: Array[Array[Char]], c: Coord): Boolean = inMap(map, c) && map(c.y)(c.x) == 'o'

  def addOffset(coord: Coord, offset: Coord): Coord = Coord(coord.x + offset.x, coord.y + offset.y)

  val inMap = (map: Grid, c: Coord) => c.x >= 0 && c.x < map.size && c.y >= 0 && c.y < map.size
}
