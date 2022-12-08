package simplifier

import core.Syntax.*
import Util.*
import BetaReduction.*
import DeadCodeElimination.*
import Inlining.*

import scala.annotation.tailrec

object Simplifier:
  val LIMIT = 1000

  @tailrec
  def simplify(ds: Defs, iteration: Int = 0): Defs =
    println(s"--- simplify $iteration ---")
    println(ds.mkString("\n"))
    if iteration < LIMIT then
      val (ds1, changed) = pipeline(
        ds,
        List(
          ("betaReduce", betaReduce),
          ("eliminateDeadCode", eliminateDeadCode),
          ("inline", doInline)
        )
      )
      if !changed then ds1
      else simplify(ds1, iteration + 1)
    else ds

  private def step(f: Defs => Option[Defs], ds: Defs): (Defs, Boolean) =
    f(ds) match
      case None     => (ds, false)
      case Some(ds) => (ds, true)

  private def pipeline(
      ds: Defs,
      fs: List[(String, Defs => Option[Defs])]
  ): (Defs, Boolean) =
    fs.foldLeft((ds, false)) { case ((ds, changed), (x, f)) =>
      val (ds1, c1) = step(f, ds)
      if c1 then println(s"changed: $x")
      (ds1, changed || c1)
    }
