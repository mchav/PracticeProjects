import java.util.Arrays
object Main {
  // implements Spigot's algorithm - found here
  def pow(x: BigInt, y: Int): BigInt = {
    List.fill(y)(x).fold(1: BigInt) { (i: BigInt,j: BigInt) => i * j }
  }

  def pi_digts(n: Int) = {
    var i = 0
    var i_fact: BigInt = 1
    var denom_fact: BigInt = 1
    var total: BigDecimal = 1
    for (i <- 1 to n) {
      i_fact *= i
      denom_fact *= (2 * i + 1)
      total += BigDecimal(i_fact) / BigDecimal(denom_fact) 
      
    }
    total * 2
  }

  def main(args: Array[String]): Unit = {
    // cbt passes in second argument into context
    if (args.length == 0) {
      println("No arguments given")
      return
    }
    val n = args(0).toInt
    lazy val pi = pi_digts(1500) // interactive precision
    println(pi.toString.take(n + 2)) // account for decimal point and 3
  }
}