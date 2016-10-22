import patmat.Huffman

Huffman.decodedSecret

def concatR[T](xs: List[T], ys: List[T]): List[T] = {
  (xs foldRight ys)(_ :: _)
}

def concatL[T](xs: List[T], ys: List[T]): List[T] = {
  xs.foldLeft(ys)(_ :: _)
}

val l1 = List(1,2,3,4,5)
val l2 = List(6,7,8,9,0)

concatR(l1,l2)