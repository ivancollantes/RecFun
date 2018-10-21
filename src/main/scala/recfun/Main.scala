package recfun

import scala.annotation.tailrec

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
    def pascal(c: Int, r: Int): Int = if(c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      @tailrec
      def innerBalance(remainingChars: List[Char], accumulator: Int): Boolean = {

        def updateAccumulator(char: Char, accumulator: Int): Int =
          if(char == '(') accumulator + 1
          else if (char == ')') accumulator - 1
          else accumulator

        val currentAccumulator: Int = updateAccumulator(remainingChars.head, accumulator)

        val balance: Boolean = if(currentAccumulator == 0) true else false

        if(currentAccumulator < 0) false
        else if (remainingChars.tail.isEmpty) balance
        else innerBalance(remainingChars.tail, currentAccumulator )
      }

      innerBalance(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

      def innerCountChange(coins: List[Int], numberOfCoins: Int, money: Int): Int = {
        if (money < 0) 0
        else if (money == 0) 1
        else if (numberOfCoins == 0) 0
        else innerCountChange(coins, numberOfCoins - 1, money) + innerCountChange(coins, numberOfCoins, money - coins(numberOfCoins-1))
      }

      innerCountChange(coins, coins.length, money)
    }
  }
