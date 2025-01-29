package paiza.denno2088

object c_a_01 {

    case class Point(x: Int, y: Int)
    case class Cell(p: Point, value: Int, fixed: Boolean, isGoal: Boolean)
    case class Wall()
    case class Board(n: Int, matrix: Array[Array[Cell | Wall]])

    def createBoard(lines: Array[String]): Board = {
        val matrix = lines.zipWithIndex.map {
            case (line, y) => {
                line.split("").zipWithIndex.map {
                    case (c, x) => {
                        c match {
                            case "A" => Cell(Point(x, y), 0, false, false)
                            case "B" => Cell(Point(x, y), Int.MaxValue, false, true)
                            case "." => Cell(Point(x, y), Int.MaxValue, false, false)
                            case "#" => Wall()
                        }
                    }
                }
            }
        }
        Board(lines.length, matrix)
    }

    def getCell(b: Board, p: Point): Option[Cell] = {
        b.matrix(p.y)(p.x) match {
            case c: Cell => Some(c)
            case _ => null
        }
    }

    def getCells(b: Board): IndexedSeq[Cell] = {
        val cells = for (
            y <- 0 until b.matrix.length;
            x <- 0 until b.matrix(0).length
        ) yield b.matrix(y)(x)

        cells.filter(_.isInstanceOf[Cell]).asInstanceOf[IndexedSeq[Cell]]
    }

    def getSmallestCell(b: Board): Cell = {
        val cells = getCells(b)
        val sorted = cells.sortBy(c => c.value)
        sorted(0)
    }

    def display(b: Board): String = {
        b.matrix.map[String](cells => {
            cells.map[String](c => {
                c match {
                    case c: Cell => if (c.isGoal) "G" else if (c.value == Int.MaxValue) "*" else c.value.toString()
                    case _ => "#"
                }
            }).mkString("")
        }).mkString("\n")
    }

    def seek(b: Board): Board = {
        b
        // val minValuePoint = 
        // val minValuePoint = for (
        //     x <- 0 until b.matrix(0).length;
        //     y <- 0 until b.matrix.length) yield
    }
}
