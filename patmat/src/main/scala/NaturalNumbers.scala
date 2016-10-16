
/**
  * Created by dev on 14/10/2016.
  */
abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat{
  def isZero: Boolean = true
  def +(that: Nat): Nat = that
  def -(that: Nat): Nat =  if (that.isZero) this else throw new UnsupportedOperationException("Cannot go negative")
  def predecessor: Nat = throw new UnsupportedOperationException("Cannot go negative")
}

class Succ(n: Nat) extends Nat {
  def isZero: Boolean = false
  def predecessor: Nat = n
  def +(that: Nat): Nat = {
    if(that.isZero)
      this
    else
      new Succ(n + that)
    //this.successor + that.predecessor
  }
  def -(that: Nat): Nat = {
    if(that.isZero)
      this
    else
      n - that.predecessor
    //this.predecessor - that.predecessor
  }
}

/* 24 */
trait Expr {
  def numValue: Int
  def leftOp: Expr
  def rightOp: Expr
  def varValue: String

  def eval: Int = this match {
    case Number(n) => n
    case Sum(e1, e2) => e1.eval + e1.eval
    case Prod(l, r) => l.eval * r.eval
  }
}

case class Number(val numValue: Int) extends Expr {
  def leftOp: Expr = throw new Error("Number.leftOp")
  def rightOp: Expr = throw new Error("Number.rightOp")
  def varValue: String = throw new Error("Number.varValue")
}
case class Var(val varValue: String) extends Expr {
  def leftOp: Expr = throw new Error("Var.leftOp")
  def rightOp: Expr = throw new Error("Var.rightOp")
  def numValue: Int = throw new Error("Var.numValue")
}

case class Sum(val leftOp: Expr, val rightOp: Expr) extends Expr {
  def numValue: Int = throw new Error("Sum.numValue")
  def varValue: String = throw new Error("Sum.varValue")
}

case class Prod(val leftOp: Expr, val rightOp: Expr) extends Expr {
  def numValue: Int = throw new Error("Prod.numValue")
  def varValue: String = throw new Error("Prod.varValue")
}