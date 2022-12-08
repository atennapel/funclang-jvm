package core

import Syntax.*
import GenSym.uniq

object EtaExpansion:
  def etaExpand(ds: Defs): Defs = ds.map(etaExpand)

  def etaExpand(d: Def): Def = d match
    case DDef(x, refs, t, v) =>
      val (as, rt, b) = v.flattenLam
      val (ps, rt2) = params(t)
      if as.size == ps.size then d
      else if as.size > ps.size then
        throw new Exception("lambda type arity mismatch in eta-expansion")
      else
        val missing = ps.size - as.size
        val missingparams = ps.drop(as.size).map(t => (s"x$$$uniq", t))
        val tv = b
          .shift(missing, 0)
          .app((0 until missing).reverse.map(i => Local(i)).toList)
          .lams(as ++ missingparams, rt2)
        val td = DDef(x, refs, t, tv)
        td
    case d => d

  private def params(t: Type): (List[Type], Type) = t match
    case TFun(a, b) =>
      val (ps, rt) = params(b)
      (a :: ps, rt)
    case t => (Nil, t)
