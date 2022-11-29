package ir

object Syntax:
  type Name = String
  type Lvl = Int
  type Arity = Int // 0 means it's not a function

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
    case If(cond: Expr, ifTrue: Expr, ifFalse: Expr)
    case BinopExpr(op: Binop, left: Expr, right: Expr)

    def globals: Set[Name] = this match
      case Local(_) => Set.empty
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

  export Expr.*

  enum IRType:
    case TInt
    case TBool
    case TFun
  export IRType.*

  final case class Def(
      name: Name,
      params: Option[NEL[IRType]],
      retrn: IRType,
      body: Expr
  ):
    def arity = params.map(_.size).getOrElse(0)

  type Defs = List[Def]
