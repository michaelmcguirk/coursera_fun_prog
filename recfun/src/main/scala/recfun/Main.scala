package recfun

import scala.collection.mutable.ListBuffer

object Main {
  def main(args: Array[String]) {
    println(sumInts(1,5))
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
      if (c == 0 || c == r) 1
      else pascal(c - 1, r -1) + pascal(c, r - 1)
    }
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balance2(chars: List[Char], numOpenPer: Int): Boolean = {
        if (chars.isEmpty) {
          numOpenPer == 0
        } else {
          val h = chars.head
          val n =
            if (h == '(') numOpenPer + 1
            else if (h == ')') numOpenPer - 1
            else numOpenPer
          if (n >= 0) balance2(chars.tail, n)
          else false
        }
      }
      balance2(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def change(lastCoinColl: List[(Int, Int)], count: Int): Int = {
        if (lastCoinColl.isEmpty) {
          count
        } else {
          val buffer = ListBuffer[(Int, Int)]()
          var newCount = count
          for ((lastCoin, total) <- lastCoinColl) {
            if (total < money) {
              for (c <- coins) {
                if (c >= lastCoin) {
                  val aC = (c, total + c)
                  buffer += aC
                }
              }
            } else if (total == money) { newCount += 1 }
          }
          change(buffer.toList, newCount)
        }
      }
      val coinMap = coins.map { c => (c, c) }
      change(coinMap, 0)
    }

  def sumInts(a: Int, b: Int): Int ={
    def loop(a: Int, acc: Int): Int =
      if (a > b) acc
      else loop(a+1, a + acc)
    loop(a, 0)
  }

  }
