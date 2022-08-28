object ex02 {
  // 01
  def fib(n: Int): Int = {
    def go(a: Int, b: Int, n: Int): Int = {
      if (n > 1) go(a+b,a,n-1)
      else if (n==1) a+b
      else 0
    }
    go(0, 1, n)
  }
  // 02
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if (n == as.length-1) true
      else if (ordered(as(n), as(n+1))) loop(n+1)
      else false
    }
    loop(0)
  }
  //03
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => ((b: B) => f(a,b))
  }
  //04
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }
  //05
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }
}