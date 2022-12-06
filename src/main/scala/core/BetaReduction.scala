package core

import Syntax.*
import GenSym.uniq

object BetaReduction:
  def betaReduce(ds: Defs): Defs = ds.map(betaReduce)

  def betaReduce(d: Def): Def = d match
    case DDef(x, t, v) => DDef(x, t, betaReduce(v))
    case d             => d

  def betaReduce(e: Expr): Expr = e match
    case App(_, _) =>
      val (fn, as) = e.flattenApp
      betaReduce(fn) match
        case l @ Lam(_, _, _, _) =>
          val (ps, rt, b) = l.flattenLam
          if as.size < ps.size then
            ps.take(as.size)
              .zip(as)
              .zipWithIndex
              .foldRight(b.lams(ps.drop(as.size), rt)) {
                case ((((x, ty), v), i), b) =>
                  Let(x, ty, betaReduce(v.shift(i, 0)), b)
              }
          else if as.size == ps.size then
            ps.zip(as).zipWithIndex.foldRight(b) {
              case ((((x, ty), v), i), b) =>
                Let(x, ty, betaReduce(v.shift(i, 0)), b)
            }
          else
            ps.zip(as.take(ps.size))
              .zipWithIndex
              .foldRight(b) { case ((((x, ty), v), i), b) =>
                Let(x, ty, betaReduce(v.shift(i, 0)), b)
              }
              .app(as.drop(ps.size))
        case fn => fn.app(as.map(betaReduce))
    case Lam(x, t1, t2, b) => Lam(x, t1, t2, betaReduce(b))
    case Let(x, t, v, b) =>
      Let(x, t, betaReduce(v), betaReduce(b))
    case If(c, a, b) =>
      If(betaReduce(c), betaReduce(a), betaReduce(b))
    case BinopExpr(op, a, b) =>
      BinopExpr(op, betaReduce(a), betaReduce(b))
    case _ => e

// (\x y z. f x y z) a b
// let x = a; let y = b; \z. f x y z

// (\x y z. f x y z) a b c
// let x = a; let y = b; let z = c; f x y z

// (\x y z. f x y z) a b c d
// (let x = a; let y = b; let z = c; f x y z) d
