object Test {
  val x = 5

  def foo(implicit y: x.type) = y

  foo
}