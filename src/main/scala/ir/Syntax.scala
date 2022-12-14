package ir

object Syntax:
  type Name = String
  type Lvl = Int
  type Arity = Int // 0 means it's not a function

  val mainName = "main$"

  final case class NEL[A](head: A, tail: List[A]):
    inline def size: Int = tail.size + 1
    def map[B](fn: A => B): NEL[B] = NEL(fn(head), tail.map(fn))
    inline def foreach[U](fn: A => U): Unit = toList.foreach(fn)
    inline def toList: List[A] = head :: tail
    inline def apply(i: Int): A = toList(i)
    inline def take(n: Int): List[A] = toList.take(n)
    inline def drop(n: Int): List[A] = toList.drop(n)
    inline def dropRight(n: Int): List[A] = toList.dropRight(n)
  object NEL:
    def of[A](es: A*): NEL[A] = NEL(es.head, es.tail.toList)
    def of[A](l: List[A]): NEL[A] = NEL(l.head, l.tail)

  enum Binop:
    case BAdd
    case BMul
    case BSub
    case BLt
  export Binop.*

  enum Expr:
    case Local(lvl: Lvl)
    case Global(name: Name, args: Option[NEL[Expr]] = None)
    case App(fn: Expr, args: NEL[Expr])
    case Let(ty: IRType, value: Expr, body: Expr)
    case IntLit(value: Int)
    case BoolLit(value: Boolean)
    case UnitLit
    case If(cond: Expr, ifTrue: Expr, ifFalse: Expr)
    case BinopExpr(op: Binop, left: Expr, right: Expr)
    case Box(ty: IRType, expr: Expr)
    case Unbox(ty: IRType, expr: Expr)
    case Con(name: Name, ty: IRType, args: List[(Expr, IRType)])
    case Case(
        scrut: Expr,
        ty: IRType,
        cases: List[(Name, List[(IRType, IRType)], Expr)]
    )

    def globals: Set[Name] = this match
      case Local(_) => Set.empty
      case UnitLit  => Set.empty
      case Global(x, args) =>
        Set(x) ++ args
          .map(l =>
            l.toList.foldLeft(Set.empty[Name])((s, e) => s ++ e.globals)
          )
          .getOrElse(Set.empty[Name])
      case App(fn, args) =>
        fn.globals ++ args.toList.foldLeft(Set.empty[Name])((s, e) =>
          s ++ e.globals
        )
      case Let(_, v, b)       => v.globals ++ b.globals
      case IntLit(_)          => Set.empty
      case BoolLit(_)         => Set.empty
      case If(a, b, c)        => a.globals ++ b.globals ++ c.globals
      case BinopExpr(_, a, b) => a.globals ++ b.globals
      case Box(_, e)          => e.globals
      case Unbox(_, e)        => e.globals
      case Con(_, _, as) =>
        as.map(_._1).foldLeft(Set.empty[Name])((s, e) => s ++ e.globals)
      case Case(t, _, cs) =>
        t.globals ++ cs
          .map((_, _, e) => e.globals)
          .foldLeft(Set.empty[Name])((s, e) => s ++ e)

  export Expr.*

  enum IRType:
    case TUnit
    case TBool
    case TInt
    case TFun
    case TPoly
    case TCon(name: Name)
  export IRType.*

  enum Def:
    case DDef(
        name: Name,
        params: Option[NEL[IRType]],
        retrn: IRType,
        body: Expr
    )
    case DData(name: Name, cons: List[(Name, List[IRType])])

    def arity = this match
      case DDef(_, ps, _, _) => ps.map(_.size).getOrElse(0)
      case DData(_, cs)      => cs.size
  export Def.*

  type Defs = List[Def]
