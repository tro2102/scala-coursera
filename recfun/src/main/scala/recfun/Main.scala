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
    if (c == r || c == 0) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def doBalance(chars: List[Char], parenStack: List[Char]): Boolean = {
      if(chars.isEmpty) parenStack.isEmpty
      else{
        val head = chars.head

        if (head == '(') doBalance(chars.tail, parenStack.::(head))
        else if (head == ')') {
          if(parenStack.isEmpty) false
          else doBalance(chars.tail, parenStack.tail)
        }
        else doBalance(chars.tail, parenStack)
      }
    }

    doBalance(chars, List[Char]())
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}
