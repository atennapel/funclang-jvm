package simplifier

object Util:
  def or2[A](f: A => Option[A], a: A, b: A): Option[(A, A)] =
    (f(a), f(b)) match
      case (None, None)       => None
      case (Some(a), None)    => Some((a, b))
      case (None, Some(b))    => Some((a, b))
      case (Some(a), Some(b)) => Some((a, b))

  def or3[A](f: A => Option[A], a: A, b: A, c: A): Option[(A, A, A)] =
    (f(a), f(b), f(c)) match
      case (Some(a), Some(b), Some(c)) => Some((a, b, c))
      case (Some(a), Some(b), None)    => Some((a, b, c))
      case (Some(a), None, Some(c))    => Some((a, b, c))
      case (None, Some(b), Some(c))    => Some((a, b, c))
      case (Some(a), None, None)       => Some((a, b, c))
      case (None, Some(b), None)       => Some((a, b, c))
      case (None, None, Some(c))       => Some((a, b, c))
      case (None, None, None)          => None

  def orL[A](f: A => Option[A], l: List[A]): Option[List[A]] =
    val l1 = l.map(x => (x, f(x)))
    if l1.forall((_, o) => o.isEmpty) then None
    else Some(l1.map((x, o) => o.getOrElse(x)))
