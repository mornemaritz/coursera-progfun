def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 => xs.takeWhile(y => y == x) :: pack(xs.dropWhile(z => z == x))
}

def pack2[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 =>
    val (first,rest) = xs span (y => y == x)
    first :: pack2(rest)
}

def encode[T](xs: List[T]): List[(T,Int)] = {
  pack(xs) map (l => (l.head, l.length))
}

def sum(xs: List[Int]) = (xs foldLeft 0) (_ + _)
def product(xs: List[Int]) = (xs foldLeft 1)(_ * _)

pack(List("a","a","a","b","c","c","a"))
pack2(List("a","a","a","b","c","c","a"))
encode(List("a","a","a","b","c","c","a"))
sum(List(1,7,9,12,45))
product(List(1,7,9,12,45))
