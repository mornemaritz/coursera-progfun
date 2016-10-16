
abstract class  IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

object Empty extends IntSet {
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  def contains(x: Int): Boolean = false
  def union(other: IntSet): IntSet = other
  override def toString = "."
}

 class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {

   def incl(x: Int): IntSet =
     if(x < elem) new NonEmpty(elem, left incl x, right)
     else if(x > elem) new NonEmpty(elem, left, right incl x)
     else this

   def contains(x: Int): Boolean =
     if(x < elem) left contains x
     else if (x > elem) right contains x
     else true

   def union(other: IntSet): IntSet =
     ((left union right) union other) incl elem

   override def toString = "{" + left +  elem + right + "}"
 }


val t1 = new NonEmpty(3, Empty, Empty)
val t2 = t1 incl 4
val t3 = new NonEmpty(6, new NonEmpty(10, Empty, Empty), Empty)
t2 union t3

val a: Array[NonEmpty] = Array(new NonEmpty(1, Empty, Empty))
val b: Array[IntSet] = a
b(0) = Empty
val s: NonEmpty = a(0)



