package paiza.denno2088

object c_a_01 {

    case class Point(x: Int, y: Int)
    case class Cell(value: Int, fixed: Boolean, lastPoint: Option[Point], isGoal: Boolean)
    case class Wall()
    case class Board(n: Int, matrix: Array[Array[Cell | Wall]])

    def createBoard(lines: Array[String]): Board = {
        val matrix = lines.map(_.split("").map(
            _ match {
                case "A" => Cell(0, false, null, false)
                case "B" => Cell(Int.MaxValue, false, null, true)
                case "." => Cell(Int.MaxValue, false, null, false)
                case "#" => Wall()
            }))
        Board(lines.length, matrix)
    }

    def getCell(b: Board, p: Point): Option[Cell] = {
        b.matrix(p.y)(p.x) match {
            case c: Cell => Some(c)
            case _ => null
        }
    }
}
