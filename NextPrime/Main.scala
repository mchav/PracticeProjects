import annotation.tailrec
import java.util.Scanner

object Main {
  def main( args: Array[String] ): Unit = {
    val in = new Scanner(System.in)
    var input = ""
    var current_prime = 2
    while (input != "q") {
      print(current_prime)
      current_prime = prime_after(current_prime)
      input = in.nextLine()
    }
  }

  def isPrime(n: Int): Boolean = {
    !(2 to Math.sqrt(n).toInt).exists(x => n % x == 0)
  }

  @tailrec def prime_after(n: Int): Int = {
    if (isPrime(n + 1)) n + 1
    else prime_after(n + 1)
  }
}
