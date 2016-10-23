def queens(n: Int): Set[List[Int]] = {
  def placeQueens(k: Int): Set[List[Int]] = {
    if(k == 0) Set(List())
    else
      for {
        queens <- placeQueens(k - 1)
        col <- 0 until n
        if isSafe(col, queens)
      } yield col :: queens
  }

  def isSafe(targetCol: Int, otherQueens: List[Int]): Boolean = {
    val targetRow = otherQueens.length
    val occupiedRows = (targetRow - 1 to 0 by -1) zip otherQueens
    occupiedRows forall {
      case (r,c) => targetCol != c && math.abs(targetCol - c) != targetRow - r
    }
  }

  placeQueens(n)
}

def show(queens: List[Int]) = {
  val lines =
    for (col <- queens.reverse)
      yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString
  "\n" + (lines mkString "\n")
}

(queens(5) map show) mkString "\n"