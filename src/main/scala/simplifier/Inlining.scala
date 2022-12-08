package simplifier

import core.Syntax.*
import Util.*

object Inlining:
  private type Globals = Map[Name, Expr]

  def doInline(ds: Defs): Option[Defs] =
    implicit val globals: Globals = ds.flatMap {
      case DDef(x, _, v) => Some(x -> v)
      case _             => None
    }.toMap
    orL[Def](doInline, ds)

  def doInline(d: Def)(implicit globals: Globals): Option[Def] = d match
    case DDef(x, t, v) => doInline(v).map(DDef(x, t, _))
    case _             => None

  def doInline(e: Expr)(implicit globals: Globals): Option[Expr] = e match
    case Let(x, t, v_, b_) =>
      val bo = doInline(b_)
      val b = bo.getOrElse(b_)
      val bc = bo.isDefined
      val vo = doInline(v_)
      val v = vo.getOrElse(v_)
      val vc = vo.isDefined
      val usageCount = b.freeLocals.count(_ == 0)
      val small = isConstant(v)
      val shouldInline = small || usageCount == 1
      if shouldInline then Some(b.substBody(v))
      else if vc || bc then Some(Let(x, t, v, b))
      else None
    case Global(x) =>
      val v = globals(x)
      if isConstant(v) then Some(v)
      else None
    case App(f__, a) =>
      val (f_, as_) = e.flattenApp
      val fo = doInline(f_)
      val f = fo.getOrElse(f_)
      val fc = fo.isDefined
      val aso = orL[Expr](doInline, as_)
      val as = aso.getOrElse(as_)
      val asc = aso.isDefined
      f match
        case Global(x) if globalHeuristic(globals(x), as) =>
          Some(globals(x).app(as))
        case _ =>
          if fc || asc then Some(f.app(as))
          else None

    case Lam(x, t1, t2, b) => doInline(b).map(Lam(x, t1, t2, _))
    case If(c, a, b) =>
      or3[Expr](doInline, c, a, b).map(If.apply)
    case BinopExpr(op, a, b) =>
      or2[Expr](doInline, a, b).map(BinopExpr(op, _, _))
    case Con(x, t, tas, as) => orL[Expr](doInline, as).map(Con(x, t, tas, _))
    case Case(t, rt, cs) =>
      val cs1 = orL[(Name, List[(Name, (Type, Type))], Expr)](
        (x, vs, e) => doInline(e).map(e => (x, vs, e)),
        cs
      )
      (doInline(t), cs1) match
        case (None, None)        => None
        case (Some(t), None)     => Some(Case(t, rt, cs))
        case (None, Some(cs))    => Some(Case(t, rt, cs))
        case (Some(t), Some(cs)) => Some(Case(t, rt, cs))
    case _ => None

  private def isConstant(e: Expr): Boolean = e match
    case IntLit(_)  => true
    case BoolLit(_) => true
    case UnitLit    => true
    case Local(_)   => true
    case Global(_)  => true
    case _          => false

  private def globalHeuristic(e: Expr, as: List[Expr]): Boolean = (e, as) match
    case (Lam(x, t, rt, b), a :: as) =>
      def affinelyUsed = b.freeLocals.count(_ == 0) < 2
      (isConstant(a) || affinelyUsed) && globalHeuristic(b, as)
    case _ => true
