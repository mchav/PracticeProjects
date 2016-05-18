import java.util.Scanner
object Main {
  def main( args: Array[String] ): Unit = {
    val in = new Scanner(System.in)
    print("Enter the width of the floor: ")
    val width = in.nextInt()
    print("Enter the height of the floor: ")
    val height = in.nextInt()
    print("Enter the cost per square metre: ")
    val cost = in.nextInt()

    print("Total cost: ")
    print(width * height * cost)
    println()
  }
}
