import scala.language.postfixOps

object Main {

  type Grid = Array[Array[Char]]
  case class Coord(x: Int, y: Int)

  case class Action(from: Coord, to: Coord, over: Coord)

  val showGrid: Grid => String = _.map(_.mkString("")).mkString("\n")
  val printGrid = {
    showGrid andThen println andThen (_ => println)
  }

  val offsets = Seq((0, -1), (0, 1), (-1, 0), (1, 0))

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

      val playable = allCoords(map).flatMap { case Coord(j, i) =>
        if (hasPeg(map, j, i)) {
          offsets.flatMap { o =>
            val (dx, dy) = addOffset(j, i, o)
            val (rx, ry) = addOffset(dx, dy, o)
            if (hasPeg(map, dx, dy) && isFree(map, rx, ry)) {
              Some(Action(Coord(j, i), Coord(rx, ry), Coord(dx, dy)))
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

  def isFree(map: Array[Array[Char]], x: Int, y: Int): Boolean = inMap(map, x, y) && map(y)(x) == '.'

  def hasPeg(map: Array[Array[Char]], x: Int, y: Int): Boolean = inMap(map, x, y) && map(y)(x) == 'o'

  def addOffset(x: Int, y: Int, o: (Int, Int)): (Int, Int) = (x + o._1, y + o._2)

  val inMap = (map: Grid, x: Int, y: Int) => x >= 0 && x < map.size && y >= 0 && y < map.size
}
