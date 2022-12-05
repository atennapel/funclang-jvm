package core

import Syntax.*
import GenSym.uniq

object LambdaLifting:
  def lambdaLift(ds: Defs): Defs = ds.flatMap(lambdaLift)

  def lambdaLift(d: Def): Defs = d match
    case DDef(x, t, v) =>
      // note: do not lift top-level lambdas
      val (as, rt, b) = v.flattenLam
      val (ds, b1) = lambdaLift(b)(x)
      ds ++ List(DDef(x, t, b1.lams(as, rt)))
    case DData(x, cs) => ???

  def lambdaLift(e: Expr)(implicit topName: Name): (Defs, Expr) = e match
    case Lam(_, _, _, _) =>
      val (as, rt, b0) = e.flattenLam
      val arity = as.size
      val (ds, b) = lambdaLift(b0)
      val dx = s"$topName$$lambdalift$$$uniq"
      (
        DDef(
          dx,
          as.foldRight(rt) { case ((_, pt), rt) => TFun(pt, rt) },
          b.lams(as, rt)
        ) :: ds,
        Global(dx)
      )

    case App(fn, arg) =>
      val (ds1, fn1) = lambdaLift(fn)
      val (ds2, arg1) = lambdaLift(arg)
      (ds1 ++ ds2, App(fn1, arg1))
    case Let(x, t, v, b) =>
      val (ds1, v1) = lambdaLift(v)
      val (ds2, b1) = lambdaLift(b)
      (ds1 ++ ds2, Let(x, t, v1, b1))
    case If(c, a, b) =>
      val (ds1, c1) = lambdaLift(c)
      val (ds2, a1) = lambdaLift(a)
      val (ds3, b1) = lambdaLift(b)
      (ds1 ++ ds2 ++ ds3, If(c1, a1, b1))
    case BinopExpr(op, a, b) =>
      val (ds1, a1) = lambdaLift(a)
      val (ds2, b1) = lambdaLift(b)
      (ds1 ++ ds2, BinopExpr(op, a1, b1))
    case _ => (Nil, e)
