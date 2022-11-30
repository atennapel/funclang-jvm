package core

import Syntax.*
import ir.Syntax as IR
import ClosureConversion.*
import LambdaLifting.*

object Compiler:
  def compile(ds: Defs): IR.Defs =
    val ds1 = closureConvert(ds)
    val ds2 = lambdaLift(ds1)
    ds2.map(compile)

  def compile(t: Type): IR.IRType = t match
    case TBool      => IR.TBool
    case TInt       => IR.TInt
    case TUnit      => IR.TUnit
    case TFun(_, _) => IR.TFun

  // precondition: d is closure-converted and lambda-lifted
  def compile(d: Def): IR.Def = d match
    case Def(x, ty, v) =>
      val (as, rt, b) = v.flattenLam
      val ps1 = as.map((x, t) => compile(t))
      val ps2 = if as.isEmpty then None else Some(IR.NEL.of(ps1))
      val rt1 = if as.isEmpty then compile(ty) else compile(rt)
      val b1 = compile(as.size, b)
      IR.Def(x, ps2, rt1, b1)

  // precondition: e is closure-converted and lambda-lifted
  def compile(k: Lvl, e: Expr): IR.Expr = e match
    case Local(i) => IR.Local(k - i - 1)
    case App(f0, a) =>
      val (f, as) = e.flattenApp
      val cas = as.map(compile(k, _))
      f match
        case Global(x) => IR.Global(x, Some(IR.NEL.of(cas)))
        case f         => IR.App(compile(k, f), IR.NEL.of(cas))
    case Let(x, t, v, b) => IR.Let(compile(t), compile(k, v), compile(k + 1, b))
    case If(c, a, b)     => IR.If(compile(k, c), compile(k, a), compile(k, b))
    case BinopExpr(op, a, b) =>
      IR.BinopExpr(compile(op), compile(k, a), compile(k, b))
    case Global(name)    => IR.Global(name)
    case IntLit(v)       => IR.IntLit(v)
    case BoolLit(v)      => IR.BoolLit(v)
    case UnitLit         => IR.UnitLit
    case Lam(_, _, _, _) => throw new Exception("cannot compile a lambda")

  def compile(o: Binop): IR.Binop = o match
    case BAdd => IR.BAdd
    case BMul => IR.BMul
    case BSub => IR.BSub
    case BLt  => IR.BLt
