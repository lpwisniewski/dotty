import scala.annotation.mirror

object mirrors {
  @mirror def Plus(A: Int, B: Int): Int & Singleton = A + B
  @mirror def Three(A: Int, B: Int, C: Int): Int & Singleton = A + B + C
}

import mirrors._

object Test {

  def main(args: Array[String]): Unit = {
    val x: Three[1, 2, 3] = Three(1, 2, 3)
    val y: Plus[3, 3] = Three(1,2,3)

    val y2: 6 = x
    print(x)
  }
}