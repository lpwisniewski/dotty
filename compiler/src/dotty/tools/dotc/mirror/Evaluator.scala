package dotty.tools
package dotc
package mirror

import core._
import typer._
import ast.{tpd, _}
import Trees._
import Types._
import Contexts._
import Constants._
import Names._
import Symbols._

object Evaluator {

  import tpd._

  /** partially evaluate mirror types */
  def reduce(tp: AppliedType)(cont: Type => Boolean)(implicit ctx: Context): Boolean = {
    val reduced: Type = reduce(tp)
    reduced != tp && cont(reduced)
  }

  def reduce(tp: Type)(implicit ctx: Context): Type = tp match {
    case AppliedType(typcon: TypeRef, args) =>
      val args2 = args.map(arg => reduce(arg))

      val reducible = args2.forall(_.isInstanceOf[ConstantType])

      if (reducible) {
        val sym = typcon.typeSymbol
        val values = args2.map { case ConstantType(const) => tpd.Literal(const) }
        applyEval(sym, values, getTree(sym)).tpe
      } else AppliedType(typcon, args2)
    case t@ TermRef(pre, design) =>
      val reduced = reduce(t.denot.info)
      if(reduced.isInstanceOf[ConstantType]) reduced
      else t
    case _ =>
      tp
  }

  private def eval(expr: Tree)(implicit ctx: Context): Tree = expr match {
    case If(cond, thenp, elsep) =>
      if (eval(cond).tpe =:= ConstantType(Constant(true))) eval(thenp)
      else eval(elsep)
    case Select(_, _) => ConstFold(expr)
    case app @ Apply(s @ Select(xt, opt), yt :: Nil) =>
      (xt.tpe.widenTermRefExpr, yt.tpe.widenTermRefExpr) match {
        case (ConstantType(_), ConstantType(_)) => ConstFold(expr)
        case _ => ConstFold(app.copy(fun = s.copy(eval(xt), opt), eval(yt) :: Nil))
      }
    case Apply(f: Ident, args) => applyEval(f.symbol, args, expr)
    case _ => ConstFold(expr)
  }

  private def applyEval(sym: Symbol, args: List[tpd.Tree], els: => Tree)(implicit ctx: Context): tpd.Tree = {
    val expr = getTree(sym)
    val names = getNames(sym)

    val evaluatedArgs = args map {
      case t: tpd.Literal => t
      case a: tpd.Apply =>
        eval(a).tpe match {
          case ConstantType(const) => tpd.Literal(const)
          case _ => return els
        }
    }

    val env: Map[TermName, Literal] = names.zip(evaluatedArgs).toMap

    val treeMap = new TreeMap {
      override def transform(t: Tree)(implicit ctx: Context): tpd.Tree = t match {
        case i@Ident(name) => env.getOrElse(name.asTermName, i)
        case _ => super.transform(t)
      }
    }
    val exprClosed = treeMap.transform(expr)

    eval(exprClosed)
  }

  def getNames(sym: Symbol)(implicit ctx: Context): List[TermName] = {
    val methSym = sym.owner.asClass.typeRef.member(sym.name.toTermName).symbol
    methSym.info.paramNamess.flatten
  }

  def getTree(sym: Symbol)(implicit ctx: Context): Tree = {
    val methSym = sym.owner.asClass.typeRef.member(sym.name.toTermName).symbol
    methSym.unforcedAnnotation(defn.BodyAnnot) match {
      case Some(annot) => annot.tree
      case None => EmptyTree
    }
  }

}
