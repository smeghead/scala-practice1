package paiza.denno2088

object c_a_01 {

    case class Point(x: Int, y: Int)
    case class Cell(point: Point, value: Int, fixed: Boolean, isGoal: Boolean)
    case class Wall()
    case class Board(n: Int, matrix: Array[Array[Cell | Wall]])

    def createBoard(lines: Array[String]): Board = {
        val matrix = lines.zipWithIndex.map {
            case (line, y) => {
                line.split("").zipWithIndex.map {
                    case (c, x) => {
                        c match {
                            case "A" => Cell(Point(x, y), 0, true, false)
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
            case _ => None
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

    def getUnfixedSmallestCell(b: Board): Cell = {
        val cells = getCells(b)
        val sorted = cells.sortBy(c => c.value).filter(_.fixed == false)
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

    def updateCell(b: Board, p: Point, fn: Cell => Cell): Board = {
        val matrix = b.matrix.map(_.map(cell => cell match {
            case c: Cell => {
                if (c.point == p) fn(c) else c
            }
            case _ => cell
        }))
        b.copy(matrix = matrix)
    }

    def getActiveCell(b: Board, p: Point): Option[Cell] = {
        val yLen = b.matrix.length
        val xLen = b.matrix(0).length

        if (p.x < 0) {
            return None
        }
        if (p.y < 0) {
            return None
        }
        if (p.x > xLen) {
            return None
        }
        if (p.y > yLen) {
            return None
        }
        getCell(b, p)
    }
    
    def getNeighborhoodCells(b: Board, p: Point): IndexedSeq[Cell] = {
        val cells = IndexedSeq(
            getActiveCell(b, Point(p.x - 1, p.y)),
            getActiveCell(b, Point(p.x + 1, p.y)),
            getActiveCell(b, Point(p.x, p.y - 1)),
            getActiveCell(b, Point(p.x, p.y + 1)),
        )

        cells.flatten
    }

    def seek(b: Board): Board = {
        // val smallCell = getUnfixedSmallestCell(b)
b        
    }
}
