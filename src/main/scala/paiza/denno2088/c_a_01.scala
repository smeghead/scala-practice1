package paiza.denno2088

object c_a_01 {

    // case class Point(x: Int, y: Int)
    // case class Cell(point: Point, value: Int, fixed: Boolean, isGoal: Boolean)
    // case class Wall()
    // case class Board(matrix: Vector[Vector[Cell | Wall]])
    case class Point(x: Int, y: Int)
    sealed trait Tile
    case class Cell(point: Point, value: Int, fixed: Boolean, isGoal: Boolean) extends Tile
    case class Wall() extends Tile
    case class Board(matrix: Vector[Vector[Tile]])

    var unfixedPoints = scala.collection.mutable.Map[Point, Int]()

    def updateCacheUnfixedCells(cell: Cell): Cell = {
        unfixedPoints.update(cell.point, cell.value)
        cell
    }

    def removeCacheFixedCells(cell: Cell): Cell = {
        unfixedPoints.remove(cell.point)
        cell
    }

    def createBoard(lines: Vector[String]): Board = {
        unfixedPoints.clear()
        val matrix = lines.zipWithIndex.map {
            case (line, y) => {
                line.split("").zipWithIndex.map {
                    case (c, x) => {
                        c match {
                            case "A" => updateCacheUnfixedCells(Cell(Point(x, y), 0, false, false))
                            case "B" => Cell(Point(x, y), Int.MaxValue, false, true)
                            case "." => Cell(Point(x, y), Int.MaxValue, false, false)
                            case "#" => Wall()
                        }
                    }
                }.toVector
            }
        }
        Board(matrix)
    }

    def spreadGoal(b: Board): Board = {
        val goal = getCells(b).find(c => c.isGoal)

        def visibleCells(b: Board, p: Point, moveFn : Point => Point): Board = {
            val nextPoint = moveFn(p)
            if (getActiveCell(b, nextPoint).isDefined) {
                visibleCells(updateCell(b, nextPoint, c => c.copy(isGoal = true)), nextPoint, moveFn)
            } else {
                b
            }
        }
        val moveFns: Vector[Point => Point] = Vector(
            p => p.copy(y = p.y + 1),
            p => p.copy(y = p.y - 1),
            p => p.copy(x = p.x + 1),
            p => p.copy(x = p.x - 1),
        )
        moveFns.foldLeft(b) { (acc, fn) => visibleCells(acc, goal.map(_.point).getOrElse(null), fn) }
    }

    def getCell(b: Board, p: Point): Option[Cell] = {
        b.matrix(p.y)(p.x) match {
            case c: Cell => Some(c)
            case _ => None
        }
    }

    def getCells(b: Board): Vector[Cell] = {
        val cells = for (
            y <- 0 until b.matrix.length;
            x <- 0 until b.matrix(0).length
        ) yield b.matrix(y)(x)

        cells.collect {
            case cell: Cell => Some(cell)
            case wall: Wall => None
        }.flatten.toVector
    }

    def getUnfixedSmallestCell(b: Board): Option[Cell] = {
        val points = unfixedPoints.toList.sortBy(_._2)
        if (points.isEmpty) {
            return None
        }
        val p = points(0)._1
        getCell(b, p)
    }

    def display(b: Board): String = {
        b.matrix.map[String](cells => {
            cells.map[String](c => {
                c match {
                    case c: Cell => "%s%s ".format(
                        if (c.isGoal) "G" else if (c.value == Int.MaxValue) "*" else c.value.toString(),
                        if (c.fixed) "+" else "-")
                    case _ => "## "
                }
            }).mkString("")
        }).mkString("\n")
    }

    def updateCellFix(b: Board, p: Point): Board = {
        updateCell(b, p, c => removeCacheFixedCells(c.copy(fixed = true)))
    }

    def updateCellValue(b: Board, p: Point, value: Int): Board = {
        updateCell(b, p, c => updateCacheUnfixedCells(c.copy(value = value)))
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
        val yRange = 0 until b.matrix.length
        val xRange = 0 until b.matrix(0).length

        if ( ! xRange.contains(p.x)) {
            return None
        }
        if ( ! yRange.contains(p.y)) {
            return None
        }
        getCell(b, p)
    }
    
    def getNeighborhoodCells(b: Board, p: Point): IndexedSeq[Cell] = {
        IndexedSeq(
            getActiveCell(b, Point(p.x - 1, p.y)),
            getActiveCell(b, Point(p.x + 1, p.y)),
            getActiveCell(b, Point(p.x, p.y - 1)),
            getActiveCell(b, Point(p.x, p.y + 1)),
        ).flatten
    }

    def seek(b: Board): Board = {
        val smallCellOption = getUnfixedSmallestCell(b)
        if (smallCellOption.isEmpty) {
            return b
        }
        val smallCell = smallCellOption.get
        val bFixed = updateCellFix(b, smallCell.point)

        val neighborCells = getNeighborhoodCells(bFixed, smallCell.point).filter(c => c.fixed == false)

        neighborCells.foldLeft(bFixed) { (acc, c) => {
            if (c.value > smallCell.value + 1) {
                updateCellValue(acc, c.point, smallCell.value + 1)
            } else {
                acc
            }
        }}
    }

    def allFixed(b: Board): Boolean = {
        getCells(b).forall(_.fixed)
    }

    def fixedGoal(b: Board): Option[Int] = {
        val fixedGoalCell = getCells(b).find(c => c.fixed && c.isGoal)
        fixedGoalCell.map(c => Some(c.value)).getOrElse(None)
    }

    def start(lines: Vector[String]): Int = {
        val b = spreadGoal(createBoard(lines))

        def rec(b: Board): Int = {
            // println("")
            // println(display(b))
            val smallCellOption = getUnfixedSmallestCell(b)
            if (smallCellOption.isEmpty) {
                return -1
            }
            val answer = fixedGoal(b)
            if (answer.isDefined) {
                return if (answer.get == Int.MaxValue) -1 else answer.get
            }
            if (allFixed(b)) {
                return -1
            }
            rec(seek(b))
        }
        rec(b)
    }
}
