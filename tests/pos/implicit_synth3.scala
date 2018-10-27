object Test {

  val x = 5

  def foo(implicit y: this.type) = y

  println(foo.x)
}