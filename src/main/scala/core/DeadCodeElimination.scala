package core

import Syntax.*

object DeadCodeElimination:
  def eliminateDeadCode(ds: Defs): Defs = ds.map(eliminateDeadCode)

  def eliminateDeadCode(d: Def): Def = d match
    case DDef(x, t, v) => DDef(x, t, eliminateDeadCode(v))
    case d             => d

  def eliminateDeadCode(e: Expr): Expr = e match
    case Let(x, t, v, b) =>
      val b1 = eliminateDeadCode(b)
      if b1.freeLocals.contains(0) then Let(x, t, eliminateDeadCode(v), b1)
      else b1.shift(-1, 0)

    case Lam(x, t1, t2, b) => Lam(x, t1, t2, eliminateDeadCode(b))
    case App(fn, arg)      => App(eliminateDeadCode(fn), eliminateDeadCode(arg))
    case If(c, a, b) =>
      If(eliminateDeadCode(c), eliminateDeadCode(a), eliminateDeadCode(b))
    case BinopExpr(op, a, b) =>
      BinopExpr(op, eliminateDeadCode(a), eliminateDeadCode(b))
    case Con(x, t, tas, as) => Con(x, t, tas, as.map(eliminateDeadCode))
    case Case(t, rt, cs) =>
      Case(
        eliminateDeadCode(t),
        rt,
        cs.map((x, vs, e) => (x, vs, eliminateDeadCode(e)))
      )
    case _ => e
