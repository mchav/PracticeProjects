import scala.annotation.tailrec

object Main{
  val zero = BigInt(0)
  def main( args: Array[String] ): Unit = {
    val n: BigInt = BigInt(args(1).toInt)
    println(fibonacci(n))
  }

  def fibonacci(n: BigInt): BigInt = {
    @tailrec def fib_tail( n: BigInt, a: BigInt, b: BigInt): BigInt = n match {
      case `zero` => a 
      case _      => fib_tail( n-1, b, a+b )
    }
    fib_tail( n, 0, 1)
  }
}
