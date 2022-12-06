package core

import Syntax.*
import ir.Syntax as IR
import EtaExpansion.*
import BetaReduction.*
import DeadCodeElimination.*
import ClosureConversion.*
import LambdaLifting.*

object Compiler:
  private def transformMain(x: Name): Name =
    if x == "main" then IR.mainName else x

  private type Arity = Int
  private type Globals = Map[Name, (Arity, Type)]

  def compile(ds: Defs): IR.Defs =
    // println(ds.mkString("\n"))
    // println("------------")
    val dsm3 = etaExpand(ds)
    // println(dsm3.mkString("\n"))
    // println("------------")
    val dsm2 = betaReduce(dsm3)
    // println(dsm2.mkString("\n"))
    // println("------------")
    val ds0 = eliminateDeadCode(dsm2)
    // println(ds0.mkString("\n"))
    // println("------------")
    val ds1 = closureConvert(ds0)
    // println(ds1.mkString("\n"))
    // println("------------")
    val ds2 = lambdaLift(ds1)
    // println(ds2.mkString("\n"))
    implicit val globals: Globals =
      ds2.flatMap {
        case DDef(x, t, v) => Some(x -> (getArity(v), t)); case _ => None
      }.toMap
    // println(globals)
    ds2.map(compile)

  private def getArity(v: Expr): Arity =
    val (as, _, _) = v.flattenLam
    as.size

  def compile(t: Type): IR.IRType = t match
    case TBool      => IR.TBool
    case TInt       => IR.TInt
    case TUnit      => IR.TUnit
    case TFun(_, _) => IR.TFun
    case TVar(_)    => IR.TPoly
    case TCon(x)    => IR.TCon(x)
    case TMeta(id)  => throw new Exception(s"cannot compile type meta ?$id")

  // precondition: d is closure-converted and lambda-lifted
  def compile(d: Def)(implicit globals: Globals): IR.Def = d match
    case DDef(x, ty, v) =>
      val (as, rt, b) = v.flattenLam
      val ps1 = as.map((x, t) => compile(t))
      val ps2 = if as.isEmpty then None else Some(IR.NEL.of(ps1))
      val rt1 = if as.isEmpty then compile(ty) else compile(rt)
      val (b1, et) = compile(as.map((_, t) => t).reverse, b)
      IR.DDef(transformMain(x), ps2, rt1, wrapExpr(b1, et, rt))
    case DData(x, cs) => IR.DData(x, cs.map((c, as) => (c, as.map(compile))))

  // precondition: e is closure-converted and lambda-lifted
  def compile(k: List[Type], e: Expr)(implicit
      globals: Globals
  ): (IR.Expr, Type) = e match
    case Local(i) => (IR.Local(k.size - i - 1), k(i))
    case App(f0, a) =>
      val (f, as) = e.flattenApp
      val cas = as.map(compile(k, _))
      f match
        case Global(x) =>
          val (arity, ty) = globals(x)
          val y = transformMain(x)
          val tcas = typedArgs(ty, cas)
          (arity, cas.size) match
            case (a, k) if k == a =>
              (
                IR.Global(y, Some(IR.NEL.of(tcas.map(wrapExpr)))),
                appN(ty, cas.size)
              )
            case (a, k) if k < a =>
              (
                IR.Global(y, Some(IR.NEL.of(cas.map((e, t) => box(t, e))))),
                appN(ty, cas.size)
              )
            case (a, k) =>
              val rt = appN(ty, cas.size)
              val as1 = tcas.take(a).map(wrapExpr)
              val as2 = cas.drop(a).map((e, t) => box(t, e))
              (
                unbox(rt, IR.Global(y, Some(IR.NEL.of(as1 ++ as2)))),
                rt
              )
        case f =>
          val (cf, ft) = compile(k, f)
          val rt = appN(ft, cas.size)
          (unbox(rt, IR.App(cf, IR.NEL.of(cas.map((e, t) => box(t, e))))), rt)
    case Global(x) => (IR.Global(x), globals(x)._2)
    case Let(x, t, v, b) =>
      val ty = compile(t)
      val (cv, _) = compile(k, v)
      val (cb, rt) = compile(t :: k, b)
      (IR.Let(ty, cv, cb), rt)
    case If(c, a, b) =>
      val (cc, _) = compile(k, c)
      val (ca, rty) = compile(k, a)
      val (cb, _) = compile(k, b)
      (IR.If(cc, ca, cb), rty)
    case BinopExpr(op, a, b) =>
      val (cop, rty) = compile(op)
      val (ca, _) = compile(k, a)
      val (cb, _) = compile(k, b)
      (IR.BinopExpr(cop, ca, cb), rty)
    case IntLit(v)       => (IR.IntLit(v), TInt)
    case BoolLit(v)      => (IR.BoolLit(v), TBool)
    case UnitLit         => (IR.UnitLit, TUnit)
    case Lam(_, _, _, _) => throw new Exception("cannot compile a lambda")
    case Con(x, ty, as) =>
      (
        IR.Con(
          x,
          compile(ty),
          as.map(e => {
            val (ce, t) = compile(k, e)
            (ce, compile(t))
          })
        ),
        ty
      )

  private def wrapExpr(e: IR.Expr, t1: Type, t2: Type): IR.Expr = (t1, t2) match
    case (TUnit, TVar(_)) => box(t1, e)
    case (TBool, TVar(_)) => box(t1, e)
    case (TInt, TVar(_))  => box(t1, e)
    case (TVar(_), TUnit) => unbox(t2, e)
    case (TVar(_), TBool) => unbox(t2, e)
    case (TVar(_), TInt)  => unbox(t2, e)
    case _                => e

  private def box(t: Type, e: IR.Expr): IR.Expr = t match
    case TFun(_, _) => e
    case TVar(_)    => e
    case TCon(_)    => e
    case t =>
      val ct = compile(t)
      e match
        case IR.Unbox(ct2, e) if ct == ct2 => e
        case IR.Box(_, _)                  => e
        case e                             => IR.Box(ct, e)

  private def unbox(t: Type, e: IR.Expr): IR.Expr = t match
    case TFun(_, _) => e
    case TVar(_)    => e
    case TCon(_)    => e
    case t =>
      val ct = compile(t)
      e match
        case IR.Box(ct2, e) if ct == ct2 => e
        case IR.Unbox(_, _)              => e
        case e                           => IR.Unbox(ct, e)

  def compile(o: Binop): (IR.Binop, Type) = o match
    case BAdd => (IR.BAdd, TInt)
    case BMul => (IR.BMul, TInt)
    case BSub => (IR.BSub, TInt)
    case BLt  => (IR.BLt, TBool)

  private def appN(t: Type, n: Int): Type = (t, n) match
    case (t, 0)          => t
    case (TFun(_, t), n) => appN(t, n - 1)
    case _ => throw new Exception("impossible: appN failed, typechecking bug")

  private def typedArgs(
      t: Type,
      as: List[(IR.Expr, Type)]
  ): List[(IR.Expr, Type, Type)] = (t, as) match
    case (TFun(t1, t2), (e, et) :: as) => (e, et, t1) :: typedArgs(t2, as)
    case (_, Nil)                      => Nil
    case _ =>
      throw new Exception("impossible: typedArgs failed, typechecking bug")
