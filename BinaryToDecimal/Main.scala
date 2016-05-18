import annotation.tailrec
object Main {
  def main( args: Array[String] ): Unit = {
    println( toDecimal("1111111") )
  }

  def toDecimal(bin: String): Int = {
    val bases = List.fill(bin.length)(2).zipWithIndex.map(x => Math.pow( x._1 , x._2).toInt).reverse
    val digits = bin.map(x => x - 48 )
    val cols = (digits, bases).zipped map (_ * _)
    cols.foldLeft(0)(_ + _)
  }

  def toBinary(n: Int): String = {
    def isPrime(num: Int): Boolean = {
      !(2 to Math.sqrt(num).toInt).exists(x => num % x == 0)
    }

    @tailrec def prime_after(num: Int): Int = {
      if (isPrime(num + 1)) n + 1
      else prime_after(num + 1)
    }

    def binHelper(acc: StringBuilder, num: Int): String = num match {
      case 1 => acc.append(1).toString
      case _ => binHelper( acc.append((num % 2).toString), num / 2)
    }
    println("Hello")
    binHelper(new StringBuilder, n).reverse
  }
}
