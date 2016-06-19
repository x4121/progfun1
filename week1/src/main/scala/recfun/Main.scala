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
  def pascal(c: Int, r: Int): Int =
    if (c > r)
      throw new Exception("NO")
    else if (c == 0 || c == r)
      1
    else
      pascal(c, r - 1) + pascal(c - 1, r - 1)

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def balance(open: Int, chars: List[Char]): Boolean =
      if (chars.isEmpty)
        open == 0
      else if (chars.head == '(')
        balance(open + 1, chars.tail)
      else if (chars.head == ')') {
        if (open == 0)
          false
        else
          balance(open - 1, chars.tail)
      } else
        balance(open, chars.tail)

    balance(0, chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0 || coins.isEmpty)
      0
    else
      coins.sortWith(_ <= _).map { coin =>
        if (money == coin)
          1
        else
          countChange(money - coin, coins.filter(_ >= coin))
      }.sum
  }
}
