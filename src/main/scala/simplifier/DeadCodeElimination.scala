package simplifier

import core.Syntax.*
import Util.*

object DeadCodeElimination:
  def eliminateDeadCode(ds: Defs): Option[Defs] =
    orL[Def](eliminateDeadCode, ds)

  def eliminateDeadCode(d: Def): Option[Def] = d match
    case DDef(x, t, v) => eliminateDeadCode(v).map(v => DDef(x, t, v))
    case d             => None

  def eliminateDeadCode(e: Expr): Option[Expr] = e match
    case Let(x, t, v, b) =>
      eliminateDeadCode(b) match
        case Some(b1) =>
          if b1.freeLocals.contains(0) then
            eliminateDeadCode(v) match
              case None     => Some(Let(x, t, v, b1))
              case Some(v1) => Some(Let(x, t, v1, b1))
            eliminateDeadCode(v).map(Let(x, t, _, b1))
          else Some(b1.shift(-1, 0))
        case None =>
          if b.freeLocals.contains(0) then
            eliminateDeadCode(v).map(Let(x, t, _, b))
          else Some(b.shift(-1, 0))

    case Lam(x, t1, t2, b) => eliminateDeadCode(b).map(Lam(x, t1, t2, _))
    case App(fn, arg) => or2[Expr](eliminateDeadCode, fn, arg).map(App.apply)
    case If(c, a, b)  => or3[Expr](eliminateDeadCode, c, a, b).map(If.apply)
    case BinopExpr(op, a, b) =>
      or2[Expr](eliminateDeadCode, a, b).map(BinopExpr(op, _, _))
    case Con(x, t, tas, as) =>
      orL[Expr](eliminateDeadCode, as).map(Con(x, t, tas, _))
    case Case(t, rt, cs) =>
      val cs1 = orL[(Name, List[(Name, (Type, Type))], Expr)](
        (x, vs, e) => eliminateDeadCode(e).map(e => (x, vs, e)),
        cs
      )
      (eliminateDeadCode(t), cs1) match
        case (None, None)        => None
        case (Some(t), None)     => Some(Case(t, rt, cs))
        case (None, Some(cs))    => Some(Case(t, rt, cs))
        case (Some(t), Some(cs)) => Some(Case(t, rt, cs))
    case _ => None
