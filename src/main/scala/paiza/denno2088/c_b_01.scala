package paiza.denno2088

object c_b_01 {
    case class Board(n: Int, matrix: Array[Array[Boolean]])

    case class Point(x: Int, y: Int)

    def createBoard(lines: Array[String]): Board = {
        val matrix = lines.map(_.split("").map(_ == "."))
        Board(lines.length, matrix)
    }

    def exist(b: Board, p: Point): Boolean = b.matrix(p.x)(p.y)

    def startPoints(b: Board): IndexedSeq[Point] = {
        val points = for (
            x <- 0 until b.n - 1;
            y <- 0 until b.n - 1
        ) yield Point(x, y)
        points.filter(p => exist(b, p))
    }

    def countSquare(b: Board, p: Point): Int = {
        val checker = for (
            i <- 1 until Math.min(b.n - p.x, b.n - p.y)
        ) yield {
            exist(b, Point(p.x, p.y + i)) && exist(b, Point(p.x + i, p.y)) && exist(b, Point(p.x + i, p.y + i))
        }
        checker.filter(identity).length
    }

    def countAll(b: Board, points: IndexedSeq[Point]): Int = {
        points.map(countSquare(b, _)).sum
    }
}
