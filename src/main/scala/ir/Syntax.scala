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

  enum Expr:
    case Local(lvl: Lvl)
    case Global(name: Name, args: Option[NEL[Expr]] = None)
    case App(fn: Expr, args: NEL[Expr])
    case IntLit(value: Int)
    case BoolLit(value: Boolean)
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
