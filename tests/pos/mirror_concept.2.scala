import scala.annotation.mirror

object mirrors {
  @mirror def -(A: Int, B: Int): Int & Singleton = A - B
  @mirror def +(A: Int, B: Int): Int & Singleton = A + B
  @mirror def <=(A: Int, B: Int): Boolean & Singleton = A <= B
  @mirror def add(A: Int, B: Int): Int & Singleton = A + B
}

import mirrors._

object Test {

  type Update[Xs <: Tuple, Pos <: Int, NewN <: Int & Singleton] <: Tuple = Pos match {
    case 0 =>
      Xs match {
        case Unit => Unit
        case x *: xs => NewN *: xs
      }
    case Int & Singleton =>
      Xs match {
        case Unit => Unit
        case x *: xs => x *: Update[xs, Pos - 1, NewN]
      }
  }

  type Last[Xs <: Tuple] = Xs match {
    case x *: Unit => x
    case x *: xs => Last[xs]
  }

  type First[Xs <: Tuple] = Xs match {
    case x *: xs => x
    case Unit => Unit
  }

  type RemoveLast[Xs <: Tuple] <: Tuple = Xs match {
    case x *: Unit => Unit
    case x *: x2 *: Unit => x *: Unit
    case x *: xs => x *: RemoveLast[xs]
  }

  type Tail[Xs <: Tuple] <: Tuple = Xs match {
    case x *: xs => xs
    case Unit => Unit
  }

  type Concat[Xs1 <: Tuple, Xs2 <: Tuple] <: Tuple = Xs1 match {
    case x *: xs => x *: Concat[xs, Xs2]
    case Unit => Xs2
  }

  trait MyBool
  trait True extends MyBool
  trait False extends MyBool

  type FirstN[D <: Tuple, A <: Int] = FirstNRec[D, A, Unit]

  type FirstNRec[D <: Tuple, A <: Int, Acc <: Tuple] <: Tuple = A match {
    case 0 => Acc
    case Int & Singleton => D match {
      case x *: xs => Concat[FirstNRec[xs, A - 1, x *: Acc], Unit]
    }
  }

  type ReverseRec[D <: Tuple, Acc <: Tuple] <: Tuple = D match {
    case x *: xs => Concat[ReverseRec[xs, x *: Acc], Unit]
    case Unit => Acc
  }

  type Reverse[D <: Tuple] = ReverseRec[D, Unit]

  type AfterN[D <: Tuple, A <: Int] <: Tuple = A match {
    case 0 => D
    case Int & Singleton => D match {
      case x *: xs => Concat[AfterN[xs, A - 1], Unit]
    }
  }

  type Nth[D <: Tuple, A <: Int] = A match {
    case 0 => D match {
      case x *: xs => x
    }
    case Int & Singleton => D match {
      case x *: xs => Nth[xs, A - 1]
    }
  }

  type Concatenerise[D1 <: Tuple, D2 <: Tuple, A <: Int, Acc <: Tuple] = A match {
    case 0 => D1 match {
      case x1 *: xs1 => D2 match {
        case x2 *: xs2 => Concat[Concat[Reverse[Acc], (x1 + x2) *: Unit], xs2]
      }
    }
    case Int & Singleton => D1 match {
      case x1 *: xs1 => D2 match {
        case x2 *: xs2 => Concatenerise[xs1, xs2, A - 1, x1 *: Acc]
      }
    }
  }



  class Tensor[D <: Tuple](val data: List[Int]) {
    def dot[D2 <: Tuple](t: Tensor[D2])(implicit ev: First[D2] =:= Last[D]): Tensor[Concat[D, Tail[D2]]] = new Tensor[Concat[D, Tail[D2]]](data)
  }

  class Concatener[Axis <: Int & Singleton] {
    type A = Axis
    def apply[D1 <: Tuple, D2 <: Tuple](t1: Tensor[D1], t2: Tensor[D2])(
      implicit ev1: FirstN[D1, A] =:= FirstN[D2, A], ev2: AfterN[D1, A + 1] =:= AfterN[D2, A + 1]): Tensor[Concatenerise[D1, D2, A, Unit]] = ???
  }

  def main(args: Array[String]): Unit = {

    val tensor1 = new Tensor[(6, 2, 1, 3, 4)](Nil)
    val tensor2 = new Tensor[(6, 2, 3, 3, 4)](Nil)

    val concat = new Concatener[2]

    val toto: Tensor[(6, 2, 4, 3, 4)] = concat(tensor1, tensor2)
  }
}