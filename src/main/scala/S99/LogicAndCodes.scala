package S99

object LogicAndCodes {
  // P46 (**) Truth tables for logical expressions.
  // def and(a: Boolean, b: Boolean): Boolean = a && b
  // def or(a: Boolean, b: Boolean): Boolean = a || b
  def nand(a: Boolean, b: Boolean): Boolean = !(a && b)
  def nor(a: Boolean, b: Boolean): Boolean = !(a || b)
  def xor(a: Boolean, b: Boolean): Boolean = a != b

  def table2(f: (a: Boolean, b: Boolean) => Boolean): List[String] = {
    val table = for {
      x <- List(true, false)
      y <- List(true, false)
    } yield "%-6s%-6s%s".format(x, y, f(x, y))
    List("A     B     result") ++ table
  }
  // P47 (*) Truth tables for logical expressions (2).
  extension (a: Boolean) {
    def and(b: Boolean): Boolean = a && a
    def or(b: Boolean): Boolean = a || b
  }
  def not(a: Boolean): Boolean = !a

  // P49 (**) Gray code.
  def gray(n: Int): List[String] = {
    if (n == 0) List("")
    else {
      val lower = gray(n - 1)
      (lower map { "0" + _ }) ::: (lower.reverse map { "1" + _ })
    }
  }

}
