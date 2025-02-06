package S99

object WorkingWithLists {
  // P01  (*) Find the last element of a list.
  def last[A](xs: List[A]): A = xs match {
    case h :: Nil => h
    case _ :: tail => last(tail)
    case _ => throw new NoSuchElementException
  }

  // def last[A](xs: List[A]): A = xs.last

  // def last(xs: List[Int]): Int = {
  //   xs.reverse.head
  // }

  // P02 (*) Find the last but one element of a list.
  def penultimate[A](xs: List[A]): A = xs match {
    case head :: _ :: Nil => head
    case _ :: tail => penultimate(tail)
    case _ => throw new NoSuchElementException
  }
}
