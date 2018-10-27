import scala.annotation.mirror

object mirrors {
  @mirror def +(A: Int, B: Int): Int & Singleton = A + B
}

import mirrors._

object Test {
  def foo(implicit y: 5 + 5) = y

  foo
}