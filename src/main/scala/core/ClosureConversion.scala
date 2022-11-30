package core

import Syntax.*

object ClosureConversion:
  def closureConvert(d: Def): Def = d match
    case Def(x, t, v) =>
      val v2 = closureConvert(Nil, v)
      Def(x, t, v2)

  def closureConvert(ds: Defs): Defs = ds.map(closureConvert)

  def closureConvert(k: List[(Name, Type)], e: Expr): Expr = e match
    case Lam(_, _, _, _) =>
      val (as, rt, b0) = e.flattenLam
      val arity = as.size
      val b = closureConvert(as.reverse ++ k, b0)
      val fvs = b.freeLocals.distinct.filterNot(_ < arity).map(_ - arity)
      val fas = fvs.map(k(_))
      val b2 = fvs.zipWithIndex.foldRight(b) { case ((fv, i), b) =>
        b.subst(fv + arity, Local(as.size + fas.size - i - 1))
      }
      b2.lams(fas ++ as, rt).app(fvs.map(Local.apply))

    case App(fn, arg) => App(closureConvert(k, fn), closureConvert(k, arg))
    case Let(x, t, v, b) =>
      Let(x, t, closureConvert(k, v), closureConvert((x, t) :: k, b))
    case If(c, a, b) =>
      If(closureConvert(k, c), closureConvert(k, a), closureConvert(k, b))
    case BinopExpr(op, a, b) =>
      BinopExpr(op, closureConvert(k, a), closureConvert(k, b))
    case _ => e
