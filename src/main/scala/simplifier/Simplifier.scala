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
          betaReduce,
          eliminateDeadCode,
          doInline
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
      fs: List[Defs => Option[Defs]]
  ): (Defs, Boolean) =
    fs.foldLeft((ds, false)) { case ((ds, changed), f) =>
      val (ds1, c1) = step(f, ds)
      (ds1, changed || c1)
    }
