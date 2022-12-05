package core

import Syntax.*
import GenSym.uniq

object BetaReduction:
  def betaReduce(ds: Defs): Defs = ds.map(betaReduce)

  def betaReduce(d: Def): Def = d match
    case DDef(x, t, v) => DDef(x, t, betaReduce(v))
    case d             => d

  def betaReduce(e: Expr): Expr = e match
    case Lam(x, t1, t2, b) => Lam(x, t1, t2, betaReduce(b))
    case App(fn, arg) =>
      betaReduce(fn) match
        case Lam(x, t1, t2, b) => Let(x, t1, betaReduce(arg), b)
        case fn                => App(fn, betaReduce(arg))
    case Let(x, t, v, b) =>
      Let(x, t, betaReduce(v), betaReduce(b))
    case If(c, a, b) =>
      If(betaReduce(c), betaReduce(a), betaReduce(b))
    case BinopExpr(op, a, b) =>
      BinopExpr(op, betaReduce(a), betaReduce(b))
    case _ => e
