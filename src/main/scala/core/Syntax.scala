package core

object Syntax:
  type Name = String
  type Ix = Int
  type Lvl = Int

  enum Type:
    case TUnit
    case TBool
    case TInt
    case TFun(param: Type, retrn: Type)
  export Type.*

  enum Binop:
    case BAdd
    case BMul
    case BSub
    case BLt
  export Binop.*

  enum Expr:
    case Local(ix: Ix)
    case Global(name: Name)
    case App(fn: Expr, arg: Expr)
    case Lam(name: Name, ty: Type, retrn: Type, body: Expr)
    case Let(name: Name, ty: Type, value: Expr, body: Expr)
    case IntLit(value: Int)
    case BoolLit(value: Boolean)
    case UnitLit
    case If(cond: Expr, ifTrue: Expr, ifFalse: Expr)
    case BinopExpr(op: Binop, left: Expr, right: Expr)

    def app(args: List[Expr]): Expr = args.foldLeft(this)(App.apply)

    def lams(params: List[(Name, Type)], rt: Type): Expr =
      params.foldRight(this) { case ((x, t), b) => Lam(x, t, rt, b) }

    def freeLocals: List[Ix] = this match
      case Local(i)        => List(i)
      case Lam(_, _, _, b) => b.freeLocals.filterNot(_ == 0).map(_ - 1)
      case Let(_, _, v, b) =>
        v.freeLocals ++ b.freeLocals.filterNot(_ == 0).map(_ - 1)
      case App(f, a)          => f.freeLocals ++ a.freeLocals
      case If(c, a, b)        => c.freeLocals ++ a.freeLocals ++ b.freeLocals
      case BinopExpr(o, a, b) => a.freeLocals ++ b.freeLocals
      case _                  => Nil

    def shift(d: Int, c: Int): Expr = this match
      case Local(k) if k >= c => Local(k + d)
      case Lam(x, t, rt, b)   => Lam(x, t, rt, b.shift(d, c + 1))
      case Let(x, t, v, b)    => Let(x, t, v.shift(d, c), b.shift(d, c + 1))
      case App(f, a)          => App(f.shift(d, c), a.shift(d, c))
      case If(cond, t, f) => If(cond.shift(d, c), t.shift(d, c), f.shift(d, c))
      case BinopExpr(op, l, r) => BinopExpr(op, l.shift(d, c), l.shift(d, c))
      case _                   => this

    def subst(j: Int, s: Expr): Expr = this match
      case Local(k) if k == j => s
      case Lam(x, t, rt, b)   => Lam(x, t, rt, b.subst(j + 1, s.shift(1, 0)))
      case Let(x, t, v, b) =>
        Let(x, t, v.subst(j, s), b.subst(j + 1, s.shift(1, 0)))
      case App(f, a)      => App(f.subst(j, s), a.subst(j, s))
      case If(cond, t, f) => If(cond.subst(j, s), t.subst(j, s), f.subst(j, s))
      case BinopExpr(op, l, r) => BinopExpr(op, l.subst(j, s), l.subst(j, s))
      case _                   => this

    def beta(arg: Expr): Expr = this match
      case Lam(x, t, rt, b) => b.subst(0, arg.shift(1, 0)).shift(-1, 0)
      case f                => App(f, arg)

    def flattenLam: (List[(Name, Type)], Type, Expr) = this match
      case Lam(x, t, rt, b) =>
        val (as, rt2, b2) = b.flattenLam
        val rt3 = if as.isEmpty then rt else rt2
        ((x, t) :: as, rt3, b2)
      case t => (Nil, TUnit, t)

    def flattenApp: (Expr, List[Expr]) = this match
      case App(f, a) =>
        val (hd, as) = f.flattenApp
        (hd, as ++ List(a))
      case t => (t, Nil)
  export Expr.*

  final case class Def(name: Name, ty: Type, value: Expr)
  type Defs = List[Def]
