package simplifier

import core.Syntax.*
import Util.*
import BetaReduction.*
import DeadCodeElimination.*

import scala.annotation.tailrec

object Simplifier:
  val LIMIT = 1000

  @tailrec
  def simplify(ds: Defs, iteration: Int = 0): Defs =
    if iteration < LIMIT then
      betaReduce(ds) match
        case None =>
          eliminateDeadCode(ds) match
            case None     => ds
            case Some(ds) => simplify(ds, iteration + 1)
        case Some(ds) =>
          eliminateDeadCode(ds) match
            case None     => simplify(ds, iteration + 1)
            case Some(ds) => simplify(ds, iteration + 1)
    else ds
