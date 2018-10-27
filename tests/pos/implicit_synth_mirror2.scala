import scala.annotation.mirror

object mirrors {
  @mirror def +(A: Int, B: Int): Int & Singleton = A + B
}

import mirrors._

object Test {
  val x = 5

  def main(args: Array[String]): Unit = {
    def foo(implicit y: x.type + 5): x.type + 5 = y

    println(foo)
  }
}