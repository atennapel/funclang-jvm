package surface

import Syntax.*
import core.Syntax as C

import scala.collection.mutable

object Typechecking:
  private val globals: mutable.Map[Name, Option[C.Type]] = mutable.Map.empty

  private type Ctx = List[(Name, C.Type)]

  private def lookup(x: Name, ix: Int = 0)(implicit
      ctx: Ctx
  ): Option[(C.Ix, C.Type)] =
    ctx match
      case Nil                   => None
      case (y, t) :: _ if x == y => Some((ix, t))
      case _ :: tl               => lookup(x, ix + 1)(tl)

  private def unify(a: C.Type, b: C.Type): Unit =
    if a == b then () else throw new Exception(s"type mismatch: $a ~ $b")

  private def checkType(t: Type)(implicit ctx: Ctx): C.Type = t match
    case TUnit      => C.TUnit
    case TBool      => C.TBool
    case TInt       => C.TInt
    case TFun(a, b) => C.TFun(checkType(a), checkType(b))

  private def checkValue(v: Expr, t: Option[Type])(implicit
      ctx: Ctx
  ): (C.Expr, C.Type) = t match
    case None =>
      val (cc, ct) = infer(v)
      (cc, ct)
    case Some(t) =>
      val ct = checkType(t)
      val cc = check(v, ct)
      (cc, ct)

  private def check(e: Expr, ty: C.Type)(implicit ctx: Ctx): C.Expr =
    (e, ty) match
      case (Lam(x, t, b), C.TFun(t1, t2)) =>
        t.foreach(t => {
          val ct = checkType(t)
          unify(ct, t1)
        })
        val cb = check(b, t2)((x -> t1) :: ctx)
        C.Lam(x, t1, t2, cb)
      case (Let(x, t, v, b), ty) =>
        val (cv, ct) = checkValue(v, t)
        val cb = check(b, ty)((x -> ct) :: ctx)
        C.Let(x, ct, cv, cb)
      case (If(c, a, b), ty) =>
        val cc = check(c, C.TBool)
        val ca = check(a, ty)
        val cb = check(b, ty)
        C.If(cc, ca, cb)
      case _ =>
        val (ce, ct) = infer(e)
        unify(ct, ty)
        ce

  private def infer(e: Expr)(implicit ctx: Ctx): (C.Expr, C.Type) = e match
    case UnitLit    => (C.UnitLit, C.TUnit)
    case BoolLit(v) => (C.BoolLit(v), C.TBool)
    case IntLit(v)  => (C.IntLit(v), C.TInt)
    case Var(x) =>
      lookup(x) match
        case Some((i, t)) => (C.Local(i), t)
        case None =>
          globals.get(x) match
            case None =>
              throw new Exception(s"undefined local or global variable $x")
            case Some(None) =>
              throw new Exception(s"global used before type is known $x")
            case Some(Some(t)) => (C.Global(x), t)
    case If(c, a, b) =>
      val cc = check(c, C.TBool)
      val (ca, ct) = infer(a)
      val cb = check(b, ct)
      (C.If(cc, ca, cb), ct)
    case Let(x, t, v, b) =>
      val (cv, ct) = checkValue(v, t)
      val (cb, crt) = infer(b)((x -> ct) :: ctx)
      (C.Let(x, ct, cv, cb), crt)
    case App(f, a) =>
      val (cf, cft) = infer(f)
      cft match
        case C.TFun(t1, t2) =>
          val ca = check(a, t1)
          (C.App(cf, ca), t2)
        case _ => throw new Exception(s"not a function in application $e: $cft")
    case Lam(x, Some(t), b) =>
      val ct = checkType(t)
      val (cb, crt) = infer(b)((x -> ct) :: ctx)
      (C.Lam(x, ct, crt, cb), C.TFun(ct, crt))
    case BinopExpr(op, a, b) =>
      val ca = check(a, C.TInt)
      val cb = check(b, C.TInt)
      op match
        case BAdd => (C.BinopExpr(C.BAdd, ca, cb), C.TInt)
        case BMul => (C.BinopExpr(C.BMul, ca, cb), C.TInt)
        case BSub => (C.BinopExpr(C.BSub, ca, cb), C.TInt)
        case BLt  => (C.BinopExpr(C.BLt, ca, cb), C.TBool)
    case _ => throw new Exception(s"cannot infer $e")

  def typecheck(e: Expr): (C.Expr, C.Type) =
    infer(e)(Nil)

  def typecheck(d: Def): C.Def = d match
    case Def(x, t, b) =>
      implicit val ctx: Ctx = Nil
      t match
        case None =>
          val (ctm, cty) = infer(b)
          globals += (x -> Some(cty))
          C.Def(x, cty, ctm)
        case Some(ty) =>
          val cty = globals(x).get
          val ctm = check(b, cty)
          C.Def(x, cty, ctm)

  def typecheck(d: Defs): C.Defs =
    globals.clear()
    d.foreach { case Def(x, t, _) =>
      globals += (x -> t.map(t => checkType(t)(Nil)))
    }
    d.map(typecheck)
