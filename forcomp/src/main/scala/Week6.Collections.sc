var xs = Array(1,2,3,44)
xs map(x => x * 2)
val w = "Hello World"
w filter(c => c.isUpper)
var r: Range = 1 until 5
val s: Range = 1 to 5
val r3: Range = 1 to 10 by 3
var rn: Range = 6 to 1 by -2
(1 to 10 by 3).map(x => x * x)
w exists(c => c.isUpper)
w forall(c => c.isUpper)
val pairs = rn zip w
w flatMap(c => List(c, '.'))
xs.sum
xs.max
xs.min

// ScalarProd zip with bracketed map
val scalarProd = (r zip r3).map(xy => xy._1 * xy._2).sum
// ScalarProd zip with braced map
var sp2 = (r zip r3).map{ case (x,y) => x * y }.sum

def isPrime(n: Int): Boolean = (2 until n).forall(n % _ != 0)

// Task:
// find all pairs of positive integers i and j, such that j is less than i and
// i is bounded by some positive integer n and i + j is prime
// 1. Using map, foldright (to flatten) and filter
val m = (1 until 7).map(i => (1 until i) map (j => (i,j)))
val n = (m foldRight Seq[(Int,Int)]())(_ ++ _)
n.filter(pair => isPrime(pair._1 + pair._2))
// 2. Using flatMap to flatten
(1 until 7).flatMap(i =>
    (1 until i) map (j => (i,j)))
      .filter(pair => isPrime(pair._1 + pair._2))
// 3. Using "for"
for {
  i <- 1 until 7
  j <- 1 until i
  if isPrime(i + j)
} yield (i,j)
//  4. Using "for" with brackets
for (i <- 1 until 7; j <- 1 until i; if isPrime(i + j)) yield  (i,j)

// ScalarProd revisited with "for"
(for ((x,y) <- r zip r3) yield x * y).sum





