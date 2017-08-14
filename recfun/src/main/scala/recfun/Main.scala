package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println("Parentheses Balancing")
    val strings = List(")((", "((dfkmdfs)())", "(this is just) a test", ":-)", "(if (zero? x) max (/ 1 x))", "", "a")
    for (string <- strings) {
      println(string + ":\n" + balance(string.toList))
      println()
    }

    println("Counting change")
    val tests = List((1, List(1)), (2, List(1, 2)), (0, List(1, 2, 3)), (-1, List()), (1, List()), (6, List(1, 2, 3)) )
    for (test <- tests) {
      println(test + ":\n" + countChange(test._1, test._2))
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (c == 0 || c == r) 1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def loop(chars: List[Char], cntr: Int): Int = {
        if (chars.isEmpty) cntr
        else {
          if (chars.head == '(') loop(chars.tail, cntr+1)
          else if (chars.head == ')' && cntr > 0) loop(chars.tail, cntr-1)
          else if (chars.head == ')') cntr-1
          else loop(chars.tail, cntr)
        }
      }
      loop(chars, 0) == 0
    }
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

      if ( money == 0 ) 1
      else if ( money > 0 && coins.nonEmpty) {
        countChange(money - coins.head, coins) + countChange(money, coins.tail)
      }
      else 0
    }
  }
