package core

import Syntax.*

object ClosureConversion:
  def closureConvert(d: Def): Def = d match
    case DDef(x, t, v) =>
      val v2 = closureConvert(Nil, v)
      DDef(x, t, v2)
    case DData(x, cs) => d

  def closureConvert(ds: Defs): Defs =
    // println(ds.mkString("\n"))
    ds.map(closureConvert)

  def closureConvert(k: List[(Name, Type)], e: Expr): Expr = e match
    case Lam(_, _, _, _) =>
      val (as, rt, b0) = e.flattenLam
      val arity = as.size
      val b = closureConvert(as.reverse ++ k, b0)
      val fvs = b.freeLocals.distinct.filterNot(_ < arity).map(_ - arity)
      val fas = fvs.map(k(_))
      val b2 = b.psubst(fvs.zipWithIndex.map { case (fv, i) =>
        (fv + arity) -> Local(as.size + fas.size - i - 1)
      }.toMap)
      b2.lams(fas ++ as, rt).app(fvs.map(Local.apply))

    case App(fn, arg) => App(closureConvert(k, fn), closureConvert(k, arg))
    case Let(x, t, v, b) =>
      Let(x, t, closureConvert(k, v), closureConvert((x, t) :: k, b))
    case If(c, a, b) =>
      If(closureConvert(k, c), closureConvert(k, a), closureConvert(k, b))
    case BinopExpr(op, a, b) =>
      BinopExpr(op, closureConvert(k, a), closureConvert(k, b))
    case Con(x, t, tas, as) => Con(x, t, tas, as.map(closureConvert(k, _)))
    case Case(t, rt, cs) =>
      Case(
        closureConvert(k, t),
        rt,
        cs.map((x, vs, e) =>
          (
            x,
            vs,
            closureConvert(
              vs.map { case (x, (t1, t2)) => (x, t2) }.reverse ++ k,
              e
            )
          )
        )
      )
    case _ => e
