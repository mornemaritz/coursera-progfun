

object expr
{
  def show(e: Expr): String = e match {
    case Number(n) => n.toString()
    case Sum(e1, e2) => show(e1) + " + " + show(e2)
    case Prod(Sum(x, y), r) => "(" + show(y) + " + " + show(x) + ")" + " * " + show(r)
    case Prod(l, r) => show(l) + " * " + show(r)
    case Var(v) => v
  }

  show(Sum(Prod(Number(2), Var("x")), Var("y")))
  show(Prod(Sum(Number(2), Var("x")), Var("y")))

  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List(x)
    //case y :: ys =>
  }
}

