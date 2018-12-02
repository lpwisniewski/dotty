import scala.annotation.mirror

object mirrors {
  @mirror def -(A: Int, B: Int): Int & Singleton = if(A - B < 0) 0 else A - B
  @mirror def add(A: Int, B: Int): Int & Singleton = A + B
}

import mirrors._

object Test {

  case object ZeroList extends Cons[0, NilType.type](NilType){
    def size = 0
  }

  val zero = ZeroList

  class Cons[T <: Int & Singleton, +Next <: Cons[_, _]](val next: Next) {
    def --[T1 <: Int & Singleton, N](i: T1): Cons[i.type, this.type] = new Cons[i.type, this.type](this)
    def copy[NNext <: Cons[_, _]](nnext: NNext): Cons[T, NNext] = new Cons[T, NNext](nnext)
  }

  case object NilType extends Cons[0, ZeroList.type](ZeroList)


  def firstRec(i: quoted.Expr[Int], expr: quoted.Expr[Cons[_, _]], acc: quoted.Expr[Cons[_, _]]): quoted.Expr[Cons[_, _]] = '{
    if (~i != 0) ~firstRec('(~{i} - 1), '{{~expr} next}, '(~{expr} copy(~{acc}))) else ~acc
  }

  rewrite def first(iter: => Int, list: => Cons[_, _]) = ~(firstRec('(iter), '{list}, '{NilType}))



  def main(args: Array[String]): Unit = {
    val x: 5 = 5
    val truc: Cons[6, Cons[1, Cons[1, NilType.type]]] = NilType -- 1 -- 1 -- (add(x, 1))


    //First try with implicits
    abstract class ValueChanger[ITER <: Int & Singleton, OLD <: Int & Singleton, NEW <: Int & Singleton, NEXT <: Cons[_, _]] {
      def changeValue(n: NEW, cons: Cons[OLD, NEXT]): Cons[_, NEXT]
    }

    def changeVal[ITER <: Int & Singleton, OLD <: Int & Singleton, NEW <: Int & Singleton, NEXT <: Cons[_, _]](i: ITER, newVal: NEW, list: Cons[OLD, NEXT])(
      implicit changer: ValueChanger[ITER, OLD, NEW, NEXT]) = changer.changeValue(newVal, list)

    object ValueChanger {
      implicit def zeroValueChanger[OLD <: Int & Singleton, NEW <: Int & Singleton, NEXT <: Cons[_, _]]: ValueChanger[0, OLD, NEW, NEXT] = new ValueChanger[0, OLD, NEW, NEXT] {
        override def changeValue(n: NEW, cons: Cons[OLD, NEXT]): Cons[NEW, NEXT] = new Cons[NEW, NEXT](cons.next)
      }

      implicit def valueChanger[ITER <: Int & Singleton, OLD <: Int & Singleton, NEW <: Int & Singleton, NEXT <: Cons[_, _]](implicit i: ITER): ValueChanger[ITER, OLD, NEW, NEXT] = new ValueChanger[ITER, OLD, NEW, NEXT] {
        override def changeValue(n: NEW, cons: Cons[OLD, NEXT]): Cons[OLD, NEXT] = new Cons[OLD, NEXT](changeVal(mirrors.-(i, 1), n, cons.next)) // ERROR here as it can not find the next implicit
      }
    }

    //Second try with rewrites, but type is lost
    val tutu = first(2, truc) //Cons[_, _]
  }
}