package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      
      def buildRow(source: List[Int]): List[Int] = {
        if(source.length == 1)
          List(1,1)
        else
          List(1) ::: next(List.empty, source) ::: List(1) 
      }
      
      def next(destination: List[Int], source: List[Int]): List[Int] = {
        if(source.length == 2)
          destination ::: List(source.head + source.tail.head)
        else
          next(destination ::: List(source.head + source.tail.head), source.tail)
      }
      
      def loop(row: Int, cols: List[Int]): Int = {
        // If we've built the row preceding the one we want the value for, we can calculate the value.
        if (row == r - 1)
          cols(c - 1) + cols(c)
        else
          loop(row + 1, buildRow(cols))
      }
      
      if(c == 0 || r == c) // First element in a row is always 1 as well as elements where the row and col are identical
        1
      else
        loop(1, List(1,1)) // Start at level 1
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def loop(state: Int, characters: List[Char]): Boolean = {
        if(state < 0 || characters.length == 0)
          state == 0
        else
          loop(state + getState(characters.head), characters.tail)
      }

      def getState(character: Char): Int = character match {
        case '(' => 1
        case ')' => -1
        case _ => 0
      }

      loop(0, chars)
    }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

      def count(remainder: Int, sortedCoinsRemaining: List[Int]): Int = {
        (remainder, sortedCoinsRemaining) match  {
          case (0, _) => 1
          case (r, _) if 0 > r => 0
          case (r, c) if sortedCoinsRemaining.isEmpty && 0 != r => 0
          case _ => count(remainder - sortedCoinsRemaining.head, sortedCoinsRemaining) + count(remainder, sortedCoinsRemaining.tail)
        }
      }

      money match {
        case (0) => 0
        case _ => count(money, coins)
      }
    }
  }
