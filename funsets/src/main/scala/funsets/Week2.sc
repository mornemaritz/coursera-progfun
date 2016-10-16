import funsets.FunSets

import math.abs

def mapReduce(f: Int => Int, combine: (Int, Int) => Int, defaultValue: Int)(a : Int, b: Int): Int = {
  if(a > b) defaultValue
  else combine(f(a), mapReduce(f,combine,defaultValue)(a + 1, b))
}

def sum(f: Int => Int)(a: Int, b: Int) = mapReduce(f, (x,y) => x + y, 0)(a, b)
def prod(f: Int => Int)(a: Int, b: Int) = mapReduce(f, (x,y) => x * y, 1)(a, b)

def fact(n: Int) = mapReduce(x => x, (y,z) => y * z, 1)(1,n)

def fullFact(n: Int): Int = {
  if(n == 0) 1
  else n * fullFact(n - 1)
}

fact(5)
fullFact(5)

val tolerance = 0.0001

def isCloseEnough(x: Double, y: Double): Boolean = {
  abs((x - y) / x) / x < tolerance
}

def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  def iterate(guess: Double): Double = {
    println(guess)
    val next = f(guess)
    if(isCloseEnough(guess, next)) next
    else iterate(next)
  }

  iterate(firstGuess)
}

fixedPoint(x => 1 + x/2)(1)

def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

def sqrt(x: Double) = fixedPoint(averageDamp(y => x / y))(1.0)

sqrt(4)

val s1 = FunSets.singletonSet(1)
val s2 = FunSets.singletonSet(2)
val s3 = FunSets.singletonSet(3)

FunSets.toString(FunSets.union(s1,s2))




