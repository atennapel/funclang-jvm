package simplifier

import core.Syntax.*
import Util.*

object CaseCommutation:
  def commute(ds: Defs): Option[Defs] = orL[Def](commute, ds)

  def commute(d: Def): Option[Def] = d match
    case DDef(x, refs, t, v) => commute(v).map(DDef(x, refs, t, _))
    case d                   => None

  def commute(e: Expr): Option[Expr] = None
