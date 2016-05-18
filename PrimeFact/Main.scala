import annotation.tailrec
import collection.parallel.mutable.ParSeq

object Main {
  def main( args: Array[String] ): Unit = {
    val n = args(1).toInt
    println(factorise(n))
  }

  def factorise(n: Long): List[Long] = {
    @tailrec 
    def factors(tuple: (Long, Long, List[Long], Int)): List[Long] = {
      tuple match {
        case (1, _, acc, _)                 => acc
        case (n, k, acc, _) if (n % k == 0) => factors((n / k, k, acc ++ ParSeq(k), Math.sqrt(n / k).toInt))
        case (n, k, acc, sqr) if (k < sqr)  => factors(n, k + 1, acc, sqr)
        case (n, k, acc, sqr) if (k >= sqr) => factors((1, k, acc ++ ParSeq(n), 0))
      }
    }
    factors((n, 2, List[Long](), Math.sqrt(n).toInt))
  }
}
