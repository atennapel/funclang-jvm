package core

import Syntax.*
import GenSym.uniq

object LambdaLifting:
  def lambdaLift(ds: Defs): Defs = ds.flatMap(lambdaLift)

  def lambdaLift(d: Def): Defs = d match
    case DDef(x, refs, t, v) =>
      // note: do not lift top-level lambdas
      val (as, rt, b) = v.flattenLam
      val (ds, b1) = lambdaLift(b)(x)
      ds ++ List(DDef(x, refs, t, b1.lams(as, rt)))
    case DData(x, cs) => List(d)

  def lambdaLift(e: Expr)(implicit topName: Name): (Defs, Expr) = e match
    case Lam(_, _, _, _) =>
      val (as, rt, b0) = e.flattenLam
      val arity = as.size
      val (ds, b) = lambdaLift(b0)
      val dx = s"$topName$$lambdalift$$$uniq"
      (
        DDef(
          dx,
          b.globals,
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
    case Con(x, t, tas, as) =>
      val (nds, nas) =
        as.map(lambdaLift).foldLeft[(Defs, List[Expr])]((Nil, Nil)) {
          case (acc, (ds, e)) => (acc._1 ++ ds, acc._2 ++ List(e))
        }
      (nds, Con(x, t, tas, nas))
    case Case(t, rt, cs) =>
      val (ds1, t1) = lambdaLift(t)
      val (nds, ncs) =
        cs.map((x, vs, e) => {
          val (nds, e1) = lambdaLift(e)
          (nds, (x, vs, e1))
        }).foldLeft[(Defs, List[(Name, List[(Name, (Type, Type))], Expr)])](
          (Nil, Nil)
        ) { case (acc, (ds, (x, vs, e))) =>
          (acc._1 ++ ds, acc._2 ++ List((x, vs, e)))
        }
      (ds1 ++ nds, Case(t1, rt, ncs))
    case _ => (Nil, e)
