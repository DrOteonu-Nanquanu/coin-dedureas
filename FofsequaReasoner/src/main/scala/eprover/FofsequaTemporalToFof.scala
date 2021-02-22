package org.nanquanu.fofsequa_reasoner.eprover

import org.nanquanu.fofsequa._
import scala.collection.immutable.HashMap

object FofsequaTemporalToFof {
  type Substitutions = FofsequaToFof.Substitutions

  def stringify(document: List[TemporalStatement], instant: Instant): String = document.foldRight("")((stmt, result) => stringify(HashMap.empty, stmt, instant) + result)

  def fofify(statement: String): String = "fof" + statement

  def stringify(substitutions: Substitutions, stmt: TemporalStatement, instant: Instant): String = {
    val (output, statement) = stmt match {
      case TrueAlwaysStatement(stmt) => (true, stmt)
      case TrueRangeStatement(range, stmt) => (on_interval(instant, range), stmt)
    }

    if(output) {
      FofsequaToFof.stringify(substitutions, statement)
    }
    else {
      ""
    }
  }

  def on_interval(instant: Instant, interval: TimeRange): Bool = null
}
