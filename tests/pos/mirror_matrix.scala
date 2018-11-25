import scala.annotation.mirror

object mirrors {
  @mirror def +(A: Int, B: Int): Int & Singleton = A + B
  @mirror def *(A: Int, B: Int): Int & Singleton = A * B
}

import mirrors._


object Test {

  trait ConcatAxis[A1 <: Int & Singleton, B1 <: Int & Singleton, A2 <: Int & Singleton, B2 <: Int & Singleton] {
    type T
    def concat(m1: Matrix[A1, B1], m2: Matrix[A2, B2]): T
  }

  object ConcatAxis {
    def axisX[A1 <: Int & Singleton, B1 <: Int & Singleton, A2 <: Int & Singleton, B2 <: Int & Singleton](implicit eq: B1 =:= B2) = new ConcatAxis[A1, B1, A2, B2] {
      type T = Matrix[A1 + A2, B1]
      override def concat(m1: Matrix[A1, B1], m2: Matrix[A2, B2]) = new Matrix[A1 + A2, B1](m1.values ++ m2.values)
    }

    def axisY[A1 <: Int & Singleton, B1 <: Int & Singleton, A2 <: Int & Singleton, B2 <: Int & Singleton](implicit eq: A1 =:= A2) = new ConcatAxis[A1, B1, A2, B2] {
      type T = Matrix[A1, B1 + B2]

      override def concat(m1: Matrix[A1, B1], m2: Matrix[A2, B2]) = {
        val indexedM1 = m1.values.zipWithIndex
        val indexedM2 = m2.values.zipWithIndex
        val res = indexedM1.map{(a, b) => a ++ indexedM2.find((a2, b2) => b2 == b).map(_._1).get}

        new Matrix[A1, B1 + B2](res)
      }
    }
  }

  import ConcatAxis._

  class Matrix[X <: Int & Singleton, Y <: Int & Singleton](val values: List[List[Int]]) {

    private def checkSizeMatrix(implicit x: X, y: Y) = {
      if(values.length != x) throw new Exception("Input matrix does not have a correct shape.")
      values.foreach(list => if(list.length != y) throw new Exception("Input matrix does not have a correct shape."))
    }

    def shape(implicit x: X, y: Y): (Int, Int) = (x, y)

    def dot[B<: Int & Singleton](matrix: Matrix[Y, B])(implicit x: X, y: Y, b: B): Matrix[X, B] = {
      val tmp3DMatrix = (for {
        xi <- 0 until x
        bi <- 0 until b
        i <- 0 until y
      } yield values(xi)(i) * matrix.values(i)(bi))

      val tmp1 = tmp3DMatrix.zipWithIndex.map{(value, i) => (value, i / y)}.groupBy{(value, i) => i}.map(a => (a._1, a._2.map(a => a._1))).toList.sortBy(tuple => tuple._1).map(tuple => tuple._2.sum)
      val tmp2 = tmp1.zipWithIndex.map{(value, i) => (value, i / b)}.groupBy{(value, i) => i}.map{(i, indexedValues) => indexedValues.map{(value, i) => value}.toList}.toList
      new Matrix[X, B](tmp2)
    }

    def flatten: Matrix[X * Y, 1] = new Matrix[X * Y, 1](values.flatten.map(a => a :: Nil))

    def concat[A <: Int & Singleton, B <: Int & Singleton, Concat <: ConcatAxis](m: Matrix[A, B], concat: Concat[X, Y, A, B]): concat.T = concat.concat(this, m)
  }

  def main(args: Array[String]): Unit = {
    val m1 = new Matrix[3, 4](List(List(1,2,3,4), List(1,2,3,4), List(1,2,3,4)))
    val m2 = new Matrix[4, 3](List(List(1,2,3), List(1,2,3), List(1,2,3), List(1,2,3)))
    val m3 = new Matrix[3, 4](List(List(1,2,3,4), List(1,2,3,4), List(1,2,3,4)))

    val res1: Matrix[3, 3] = m1.dot(m2)
    val res2: Matrix[3, 8] = m3.concat(m1, ConcatAxis.axisY)
    val res3: Matrix[6, 4] = m3.concat(m1, ConcatAxis.axisX)
  }
}