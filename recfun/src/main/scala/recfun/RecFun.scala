package recfun

import scala.collection.{immutable, mutable}

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    def getPascalRow(n: Int): Array[Int] = {
      if (n == 0) {
        Array(1)
      } else {
        val previousRow = getPascalRow(n - 1)
        previousRow.head +: Range(0, previousRow.length - 1).map { i => previousRow(i) + previousRow(i + 1) }.toArray :+ previousRow.last
      }
    }

    getPascalRow(r)(c)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def isBalanced(n: Int, chars: List[Char]): Boolean = {
      if (n < 0) {
        false
      } else if (chars.isEmpty) {
        n == 0
      } else if (chars.head == '(') {
        isBalanced(n + 1, chars.tail)
      } else if (chars.head == ')') {
        isBalanced(n - 1, chars.tail)
      } else {
        isBalanced(n, chars.tail)
      }
    }

    isBalanced(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) {
      1
    } else if (money < 0 || coins.isEmpty) {
      0
    } else {
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
}
