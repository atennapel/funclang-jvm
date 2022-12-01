package surface

import Syntax.*
import core.Syntax as C

import scala.collection.mutable

object Typechecking:
  // type metas
  private enum TMetaEntry:
    case Unsolved
    case Solved(ty: C.Type)
  import TMetaEntry.*

  private val tmetas: mutable.ArrayBuffer[TMetaEntry] =
    mutable.ArrayBuffer.empty

  private def unsolvedTMetas: List[C.TMetaId] = tmetas.zipWithIndex
    .filter { case (Unsolved, _) => true; case _ => false }
    .map(_._2)
    .toList

  private def freshTMeta(): C.Type =
    val id = tmetas.size
    tmetas += Unsolved
    C.TMeta(id)

  private def force(t: C.Type): C.Type = t match
    case C.TMeta(id) =>
      tmetas(id) match
        case Unsolved   => t
        case Solved(ty) => force(ty)
    case _ => t

  private def zonk(t: C.Type): C.Type = force(t) match
    case C.TFun(a, b) => C.TFun(zonk(a), zonk(b))
    case t            => t

  private def zonk(t: C.Expr): C.Expr = t match
    case C.App(fn, arg)        => C.App(zonk(fn), zonk(arg))
    case C.Lam(x, t1, t2, b)   => C.Lam(x, zonk(t1), zonk(t2), zonk(b))
    case C.Let(x, t, v, b)     => C.Let(x, zonk(t), zonk(v), zonk(b))
    case C.If(c, a, b)         => C.If(zonk(c), zonk(a), zonk(b))
    case C.BinopExpr(op, a, b) => C.BinopExpr(op, zonk(a), zonk(b))
    case t                     => t

  private def occurs(id: C.TMetaId, t: C.Type): Boolean = force(t) match
    case C.TFun(a, b) => occurs(id, a) || occurs(id, b)
    case C.TMeta(id2) => id == id2
    case _            => false

  private def solve(id: C.TMetaId, t: C.Type): Unit =
    if occurs(id, t) then throw new Exception(s"occurs check failed ?$id := $t")
    tmetas(id) = Solved(t)

  private def unify(a: C.Type, b: C.Type): Unit = (force(a), force(b)) match
    case (C.TUnit, C.TUnit)               => ()
    case (C.TBool, C.TBool)               => ()
    case (C.TInt, C.TInt)                 => ()
    case (C.TFun(t1, t2), C.TFun(t3, t4)) => unify(t1, t3); unify(t2, t4)
    case (C.TMeta(id), t)                 => solve(id, t)
    case (t, C.TMeta(id))                 => solve(id, t)
    case (a, b) => throw new Exception(s"cannot unify $a ~ $b")

  // contexts
  private val globals: mutable.Map[Name, C.Type] = mutable.Map.empty
  private type Ctx = List[(Name, C.Type)]

  private def lookup(x: Name, ix: Int = 0)(implicit
      ctx: Ctx
  ): Option[(C.Ix, C.Type)] =
    ctx match
      case Nil                   => None
      case (y, t) :: _ if x == y => Some((ix, t))
      case _ :: tl               => lookup(x, ix + 1)(tl)

  // type checking
  private def checkType(t: Type)(implicit ctx: Ctx): C.Type = t match
    case TUnit      => C.TUnit
    case TBool      => C.TBool
    case TInt       => C.TInt
    case TFun(a, b) => C.TFun(checkType(a), checkType(b))
    case THole      => freshTMeta()

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
    (e, force(ty)) match
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
      case (Hole, ty) => throw new Exception(s"hole found, expected type: $ty")
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
            case Some(t) => (C.Global(x), t)
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
      force(cft) match
        case C.TFun(t1, t2) =>
          val ca = check(a, t1)
          (C.App(cf, ca), t2)
        case ty =>
          val ta = freshTMeta()
          val tb = freshTMeta()
          unify(ty, C.TFun(ta, tb))
          val ca = check(a, ta)
          (C.App(cf, ca), tb)
    case Lam(x, t, b) =>
      val ct = t.fold(freshTMeta())(checkType)
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
    case Hole => throw new Exception("cannot infer a hole")

  def typecheck(e: Expr): (C.Expr, C.Type) =
    infer(e)(Nil)

  def typecheck(d: Def): Option[C.Def] = d match
    case DDecl(_, _) => None
    case DDef("main", _, b) =>
      val cty = globals("main")
      val ctm = check(b, cty)(Nil)
      Some(C.Def("main", cty, ctm))
    case DDef(x, _, b) =>
      implicit val ctx: Ctx = Nil
      val cty = globals(x)
      val ctm = check(b, cty)
      Some(C.Def(x, cty, ctm))

  def typecheck(d: Defs): C.Defs =
    globals.clear()
    tmetas.clear()
    d.foreach {
      case DDecl("main", ty) =>
        val cty = C.TFun(C.TUnit, C.TInt)
        unify(checkType(ty)(Nil), cty)
        globals += ("main" -> cty)
      case DDef("main", t, _) =>
        val ty = C.TFun(C.TUnit, C.TInt)
        t.foreach(t => unify(checkType(t)(Nil), ty))
        globals += ("main" -> ty)
      case DDecl(x, t) =>
        globals += (x -> checkType(t)(Nil))
      case DDef(x, t, _) =>
        val ct = t.map(t => checkType(t)(Nil))
        globals.get(x) match
          case None     => globals += (x -> ct.getOrElse(freshTMeta()))
          case Some(ty) => ct.foreach(unify(_, ty))
    }
    val cds = d.flatMap(typecheck).map { case C.Def(x, t, b) =>
      C.Def(x, zonk(t), zonk(b))
    }
    val utms = unsolvedTMetas
    if utms.nonEmpty then
      throw new Exception(
        s"unsolved type metas: ${utms.map(id => s"?$id").mkString(", ")}\n" ++
          s"${cds.map(d => s"${d.name} : ${d.ty}").mkString("\n")}"
      )
    cds
