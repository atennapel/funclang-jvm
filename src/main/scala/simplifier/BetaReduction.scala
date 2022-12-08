package simplifier

import core.Syntax.*
import core.GenSym.uniq
import Util.*

object BetaReduction:
  def betaReduce(ds: Defs): Option[Defs] = orL[Def](betaReduce, ds)

  def betaReduce(d: Def): Option[Def] = d match
    case DDef(x, refs, t, v) => betaReduce(v).map(DDef(x, refs, t, _))
    case d                   => None

  def betaReduce(e: Expr): Option[Expr] = e match
    case App(_, _) =>
      val (fn, as) = e.flattenApp
      val fno = betaReduce(fn)
      val fnchanged = fno.isDefined
      val fn1 = fno.getOrElse(fn)
      fn1 match
        case l @ Lam(_, _, _, _) =>
          val (ps, rt, b) = l.flattenLam
          if as.size < ps.size then
            Some(
              ps.take(as.size)
                .zip(as)
                .zipWithIndex
                .foldRight(b.lams(ps.drop(as.size), rt)) {
                  case ((((x, ty), v), i), b) =>
                    val vv = v.shift(i, 0)
                    Let(x, ty, betaReduce(vv).getOrElse(vv), b)
                }
            )
          else if as.size == ps.size then
            Some(ps.zip(as).zipWithIndex.foldRight(b) {
              case ((((x, ty), v), i), b) =>
                val vv = v.shift(i, 0)
                Let(x, ty, betaReduce(vv).getOrElse(vv), b)
            })
          else
            Some(
              ps.zip(as.take(ps.size))
                .zipWithIndex
                .foldRight(b) { case ((((x, ty), v), i), b) =>
                  val vv = v.shift(i, 0)
                  Let(x, ty, betaReduce(vv).getOrElse(vv), b)
                }
                .app(as.drop(ps.size))
            )
        case fn =>
          orL[Expr](betaReduce, as) match
            case None if !fnchanged => None
            case None               => Some(fn.app(as))
            case Some(as)           => Some(fn.app(as))
    case Lam(x, t1, t2, b) => betaReduce(b).map(Lam(x, t1, t2, _))
    case Let(x, t, v, b) =>
      or2[Expr](betaReduce, v, b).map(Let(x, t, _, _))
    case If(c, a, b) =>
      c match
        case BoolLit(true)  => Some(a)
        case BoolLit(false) => Some(b)
        case _              => or3[Expr](betaReduce, c, a, b).map(If.apply)
    case BinopExpr(op, a, b) =>
      tryBinop(op, a, b) match
        case Some(e) => Some(e)
        case None    => or2[Expr](betaReduce, a, b).map(BinopExpr(op, _, _))
    case Con(x, t, tas, as) => orL[Expr](betaReduce, as).map(Con(x, t, tas, _))
    case Case(t, rt, cs) =>
      t match
        case Con(x, ty, tas, as) =>
          val (_, ps, b) = cs.find((y, _, _) => x == y).getOrElse(cs.last)
          if ps.isEmpty then Some(b)
          else Some(b.psubst((0 until ps.size).zip(as.reverse).toMap))
        case _ =>
          val cs1 = orL[(Name, List[(Name, (Type, Type))], Expr)](
            (x, vs, e) => betaReduce(e).map(e => (x, vs, e)),
            cs
          )
          (betaReduce(t), cs1) match
            case (None, None)        => None
            case (Some(t), None)     => Some(Case(t, rt, cs))
            case (None, Some(cs))    => Some(Case(t, rt, cs))
            case (Some(t), Some(cs)) => Some(Case(t, rt, cs))
    case _ => None

  private def tryBinop(op: Binop, a: Expr, b: Expr): Option[Expr] =
    (op, a, b) match
      case (BAdd, IntLit(a), IntLit(b)) => Some(IntLit(a + b))
      case (BAdd, IntLit(0), b)         => Some(b)
      case (BAdd, a, IntLit(0))         => Some(a)
      case (BMul, IntLit(a), IntLit(b)) => Some(IntLit(a * b))
      case (BMul, IntLit(1), b)         => Some(b)
      case (BMul, a, IntLit(1))         => Some(a)
      case (BMul, IntLit(0), _)         => Some(IntLit(0))
      case (BMul, _, IntLit(0))         => Some(IntLit(0))
      case (BSub, IntLit(a), IntLit(b)) => Some(IntLit(a - b))
      case (BSub, a, IntLit(0))         => Some(a)
      case (BLt, IntLit(a), IntLit(b))  => Some(BoolLit(a < b))
      case _                            => None

// (\x y z. f x y z) a b
// let x = a; let y = b; \z. f x y z

// (\x y z. f x y z) a b c
// let x = a; let y = b; let z = c; f x y z

// (\x y z. f x y z) a b c d
// (let x = a; let y = b; let z = c; f x y z) d
