package recfun
import common._

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
    if (c < 0 || c > r) 0
    else if (r == 0) 1 
    else pascal(c, r-1) + pascal(c-1, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balance_loop(count: Int, closed: Boolean, res_str: List[Char]): Boolean = {
      if (res_str.isEmpty) count == 0 && closed
      else {
        val c = res_str.head
        val new_count = if (c == '(') count+1 else if (c == ')') count-1 else count
        val clo = if (c == ')') true else if (c == '(') false else closed
        balance_loop(new_count, clo, res_str.tail)
      }
    }
    balance_loop(0, true, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (coins.isEmpty) 0
    else {
    	val curr_coin = coins.head
    	val can_use = (curr_coin <= money)
    	countChange(money, coins.tail) + (if (can_use) countChange(money-curr_coin, coins) else 0)
    }
  }
}
