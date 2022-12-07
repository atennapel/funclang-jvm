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
    case C.Con(x, t, as)       => C.Con(x, zonk(t), as.map(zonk))
    case C.Case(x, t, cs) =>
      C.Case(x, zonk(t), cs.map((x, ps, b) => (x, ps, zonk(b))))
    case t => t

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
    case (C.TVar(x), C.TVar(y)) if x == y => ()
    case (C.TCon(x), C.TCon(y)) if x == y => ()
    case (C.TFun(t1, t2), C.TFun(t3, t4)) => unify(t1, t3); unify(t2, t4)
    case (C.TMeta(id), t)                 => solve(id, t)
    case (t, C.TMeta(id))                 => solve(id, t)
    case (a, b) => throw new Exception(s"cannot unify $a ~ $b")

  // polymorphism
  private def inst(
      t: C.Type,
      map: mutable.Map[Name, C.Type] = mutable.Map.empty
  ): C.Type = t match
    case C.TVar(x) =>
      map.get(x) match
        case Some(t) => t
        case None =>
          val m = freshTMeta()
          map += x -> m
          m
    case C.TFun(t1, t2) => C.TFun(inst(t1, map), inst(t2, map))
    case t              => t

  private def freeTMetas(t: C.Type, tms: mutable.ArrayBuffer[C.TMetaId]): Unit =
    force(t) match
      case C.TMeta(id)    => if !tms.contains(id) then tms += id
      case C.TFun(t1, t2) => freeTMetas(t1, tms); freeTMetas(t2, tms)
      case _              => ()

  private def gen(t: C.Type): C.Type =
    val tms = mutable.ArrayBuffer.empty[C.TMetaId]
    freeTMetas(t, tms)
    tms.zipWithIndex.foreach((id, i) => solve(id, C.TVar(s"t$i")))
    zonk(t)

  // contexts
  private val tglobals: mutable.Map[Name, List[Name]] = mutable.Map.empty
  private val tcons: mutable.Map[Name, (C.Type, List[C.Type])] =
    mutable.Map.empty
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
    case TUnit                           => C.TUnit
    case TBool                           => C.TBool
    case TInt                            => C.TInt
    case TVar(x)                         => C.TVar(x)
    case TCon(x) if tglobals.contains(x) => C.TCon(x)
    case TCon(x)    => throw new Exception(s"undefined type constructor $x")
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
    case Var(x) if x.head.isUpper =>
      tcons.get(x) match
        case None => throw new Exception(s"undefined constructor $x")
        case Some((ty, as)) =>
          (
            C.Con(x, ty, (0 until as.size).reverse.map(i => C.Local(i)).toList)
              .lams(as.zipWithIndex.map((t, i) => (s"a$i", t)), ty),
            as.foldRight(ty)((a, rt) => C.TFun(a, rt))
          )
    case Var(x) =>
      lookup(x) match
        case Some((i, t)) => (C.Local(i), t)
        case None =>
          globals.get(x) match
            case None =>
              throw new Exception(s"undefined local or global variable $x")
            case Some(t) => (C.Global(x), inst(t))
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
    case Case(t, cs) =>
      val (ct, ty) = infer(t)
      val dtype = force(ty) match
        case C.TCon(x) => x
        case ty @ C.TMeta(id) if cs.size > 0 =>
          val conName = cs.head._1
          tcons.get(conName) match
            case Some((dty @ C.TCon(x), _)) => unify(ty, dty); x
            case _ => throw new Exception(s"cannot case on $t : $ty")
        case ty => throw new Exception(s"cannot case on $t : $ty")
      val cons = tglobals(dtype)
      val usedcons = cs.map(_._1)
      val hasOtherwise = usedcons.last == "_"
      if usedcons.init.contains("_") then
        throw new Exception(s"otherwise case must be last")
      val usedcons2 = usedcons.filterNot(_ == "_")
      if !Set.from(usedcons2).subsetOf(Set.from(cons)) then
        throw new Exception(s"case mismatch $cons vs $cs")
      var rty: Option[C.Type] = None
      val ncs = cs.map((x, vs, t) => {
        if x == "_" then
          if vs.nonEmpty then
            throw new Exception(s"otherwise case cannot have parameters: $vs")
          val ct = rty match
            case None =>
              val (ct, rty1) = infer(t)
              rty = Some(rty1)
              ct
            case Some(rty) => check(t, rty)
          (x, Nil, ct)
        else
          val ts = tcons(x)._2
          if vs.size != ts.size then
            throw new Exception(s"case parameter arity mismatch: $x")
          val nctx: Ctx = vs.zip(ts).reverse ++ ctx
          val ct = rty match
            case None =>
              val (ct, rty1) = infer(t)(nctx)
              rty = Some(rty1)
              ct
            case Some(rty) => check(t, rty)(nctx)
          (x, vs.zip(ts), ct)
      })
      val rrty = rty.get
      (C.Case(ct, rrty, ncs), rrty)
    case Hole => throw new Exception("cannot infer a hole")

  def typecheck(e: Expr): (C.Expr, C.Type) =
    infer(e)(Nil)

  def typecheck(d: Def): Option[C.Def] =
    d match
      case DDecl(_, _)    => None
      case DData(_, _, _) => None
      case DDef("main", _, b) =>
        val cty = globals("main")
        val ctm = check(b, cty)(Nil)
        Some(C.DDef("main", cty, ctm))
      case DDef(x, _, b) =>
        implicit val ctx: Ctx = Nil
        val cty = globals(x)
        val ctm = check(b, cty)
        val cty1 = gen(cty)
        globals += x -> cty1
        Some(C.DDef(x, cty1, ctm))

  def typecheck(d: Defs): C.Defs =
    globals.clear()
    tmetas.clear()
    val datadefs = d.flatMap {
      case DDecl("main", ty) =>
        val cty = C.TFun(C.TUnit, C.TInt)
        unify(checkType(ty)(Nil), cty)
        globals += ("main" -> cty)
        None
      case DDef("main", t, _) =>
        val ty = C.TFun(C.TUnit, C.TInt)
        t.foreach(t => unify(checkType(t)(Nil), ty))
        globals += ("main" -> ty)
        None
      case DDecl(x, t) =>
        globals += (x -> checkType(t)(Nil))
        None
      case DDef(x, t, _) =>
        val ct = t.map(t => checkType(t)(Nil))
        globals.get(x) match
          case None     => globals += (x -> ct.getOrElse(freshTMeta()))
          case Some(ty) => ct.foreach(unify(_, ty))
        None
      case DData(x, tvs, cs) =>
        if tglobals.contains(x) then
          throw new Exception(s"duplicate data definition $x")
        tglobals += x -> cs.map(_._1)
        val cs1 = cs.map((cx, as) => {
          if tcons.contains(cx) then
            throw new Exception(s"duplicate constructor definition $cx in $x")
          val as1 = as.map(a => checkType(a)(Nil))
          tcons += cx -> (C.TCon(x), as1)
          (cx, as1)
        })
        Some(C.DData(x, cs1))
    }
    val cds = datadefs ++ d.flatMap(typecheck).map {
      case C.DDef(x, t, b) =>
        C.DDef(x, zonk(t), zonk(b))
      case C.DData(x, cs) => C.DData(x, cs.map((x, t) => (x, t.map(zonk))))
    }
    val utms = unsolvedTMetas
    if utms.nonEmpty then
      throw new Exception(
        s"unsolved type metas: ${utms.map(id => s"?$id").mkString(", ")}\n" ++
          s"${cds
              .map {
                case C.DDef(x, t, _) => s"$x : $t";
                case C.DData(x, cs) =>
                  s"data $x | ${cs.map((y, as) => s"$y ${as.mkString(" ")}").mkString(" | ")}"
              }
              .mkString("\n")}"
      )
    cds
