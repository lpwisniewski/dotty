import scala.annotation.mirror

object mirrors {
  @mirror def Fibo(A: Int): Int & Singleton = if(A <= 2) 1 else Fibo(A - 1) + Fibo(A - 2)
}

import mirrors._

object Test {

  def main(args: Array[String]): Unit = {
    val x: 144 = Fibo(12)
    println(x)
  }
}