import scala.annotation.mirror
import scala.language.implicitConversions

object mirrors {
  @mirror def -(A: Int): Int & Singleton = - A
  @mirror def +(A: Int, B: Int): Int & Singleton = A + B
}

import mirrors._

object Test {

  abstract class ISUContainer { self =>
    type S <: Int & Singleton
    type KG <: Int & Singleton
    type MOL <: Int & Singleton
    type CD <: Int & Singleton
    type A <: Int & Singleton
    type K <: Int & Singleton
    type M <: Int & Singleton

    type PS <: Int & Singleton
    type PKG <: Int & Singleton
    type PMOL <: Int & Singleton
    type PCD <: Int & Singleton
    type PA <: Int & Singleton
    type PK <: Int & Singleton
    type PM <: Int & Singleton

    def value: Double

    def *(v: ISUContainer)(implicit pow1: (self.PS, self.PKG, self.PMOL, self.PCD, self.PA, self.PK, self.PM),
                                    pow2: (v.PS, v.PKG, v.PMOL, v.PCD, v.PA, v.PK, v.PM)) =
      new ISUContainer {
        type S = v.S + self.S
        type KG = v.KG + self.KG
        type MOl = v.MOL + self.MOL
        type CD = v.CD + self.CD
        type A = v.A + self.A
        type K = v.K + self.K
        type M = v.M + self.M

        type PS =  self.PS
        type PKG = self.PKG
        type PMOL = self.PMOL
        type PCD = self.PCD
        type PA = self.PA
        type PK = self.PK
        type PM = self.PM

        override def value = {
          val p1: Array[Int] = pow1.toArray.map(_.asInstanceOf[Int])
          val p2: Array[Int] = pow2.toArray.map(_.asInstanceOf[Int])

          val zipped: Array[(Int, Int)] = p1 zip p2
          val sum: Array[Int] = zipped.map(t => t._2 - t._1)
          val total: Int = sum.sum
          self.value * v.value * scala.math.pow(10, total)
        }
      }

    override def toString: String = "ISUContainer: " + value
  }

  implicit class IntIpv(i: Int) {
    def kg = new ISUContainer {
      type S = 0
      type KG = 1
      type MOL = 0
      type CD = 0
      type A = 0
      type K = 0
      type M = 0

      type PS = 0
      type PKG = 0
      type PMOL = 0
      type PCD = 0
      type PA = 0
      type PK = 0
      type PM = 0

      def value: Double = i
    }

    def g = new ISUContainer {
      type S = 0
      type KG = 1
      type MOL = 0
      type CD = 0
      type A = 0
      type K = 0
      type M = 0

      type PS = 0
      type PKG = -[3]
      type PMOL = 0
      type PCD = 0
      type PA = 0
      type PK = 0
      type PM = 0

      def value: Double = i
    }

    def sec = new ISUContainer {
      type S = 1
      type KG = 0
      type MOL = 0
      type CD = 0
      type A = 0
      type K = 0
      type M = 0

      type PS = 0
      type PKG = 0
      type PMOL = 0
      type PCD = 0
      type PA = 0
      type PK = 0
      type PM = 0

      def value: Double = i
    }

    def mol = new ISUContainer {
      type S = 0
      type KG = 0
      type MOL = 1
      type CD = 0
      type A = 0
      type K = 0
      type M = 0

      type PS = 0
      type PKG = 0
      type PMOL = 0
      type PCD = 0
      type PA = 0
      type PK = 0
      type PM = 0

      def value: Double = i
    }

    def cd = new ISUContainer {
      type S = 0
      type KG = 0
      type MOL = 0
      type CD = 1
      type A = 0
      type K = 0
      type M = 0

      type PS = 0
      type PKG = 0
      type PMOL = 0
      type PCD = 0
      type PA = 0
      type PK = 0
      type PM = 0

      def value: Double = i
    }

    def A = new ISUContainer {
      type S = 0
      type KG = 0
      type MOL = 0
      type CD = 0
      type A = 1
      type K = 0
      type M = 0

      type PS = 0
      type PKG = 0
      type PMOL = 0
      type PCD = 0
      type PA = 0
      type PK = 0
      type PM = 0

      def value: Double = i
    }

    def K = new ISUContainer {
      type S = 0
      type KG = 0
      type MOL = 0
      type CD = 0
      type A = 0
      type K = 1
      type M = 0

      type PS = 0
      type PKG = 0
      type PMOL = 0
      type PCD = 0
      type PA = 0
      type PK = 0
      type PM = 0

      def value: Double = i
    }

    def m = new ISUContainer {
      type S = 0
      type KG = 0
      type MOL = 0
      type CD = 0
      type A = 0
      type K = 0
      type M = 1

      type PS = 0
      type PKG = 0
      type PMOL = 0
      type PCD = 0
      type PA = 0
      type PK = 0
      type PM = 0

      def value: Double = i
    }
  }

  def main(args: Array[String]): Unit = {
    def toto = 6 kg
    def tutu = 6 g
    def pouet = toto * tutu

    println(pouet)
  }
}