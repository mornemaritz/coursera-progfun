import objsets._

trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T]{
  def isEmpty = false
}

class Nil[T] extends List[T]{
  def isEmpty: Boolean = true
  def tail: Nothing = throw new NoSuchElementException("Nil.head")
  def head: Nothing = throw new NoSuchElementException("Nil.tail")
}

def nth[T](n: Int, list: List[T]): T = {
  if(list.isEmpty) throw new IndexOutOfBoundsException
  else if(n == 0) list.head
  else nth(n - 1, list.tail)
}

def nth2[T](n: Int, list: List[T]): T = {

  def getElem(i: Int, list: List[T]): T = {
    if(list.isEmpty) throw new IndexOutOfBoundsException
    if(i == n) list.head
    else getElem(i + 1, list.tail)
  }

  getElem(0, list)
}

val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))

val set1 = new Empty
val set2 = set1.incl(new Tweet("a", "a body", 20))
val set3 = set2.incl(new Tweet("b", "b body", 20))
val c = new Tweet("c", "c body", 7)
val d = new Tweet("d", "d body", 9)
val set4c = set3.incl(c)
val set4d = set3.incl(d)
val set5 = set4c.incl(d)

set5.descendingByRetweet

TweetReader.allTweets

List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus").map(s => s.toLowerCase()).distinct