package org.nanquanu.fofsequa

import java.text.ParseException

import scala.util.parsing.combinator._

/*
 Parser
*/
object FofseqTemporalParser extends FolseqParserBase {
  def fofsequa_temporal_document: Parser[List[TemporalStatement]] = temporal_statement.*

  def temporal_statement: Parser[TemporalStatement] =
    statement ^^ {TrueAlwaysStatement(_)} |
    "ValidBetween(" ~ timeStamp ~ "," ~ timeStamp ~ "," ~ statement ~ ")" ^^
      { case _ ~ start ~ _~ end ~ _ ~ stmt ~ _ => TrueRangeStatement(TimeRange(Some(start), Some(end)), stmt) } |
    "ValidFrom(" ~ timeStamp ~ "," ~ statement ~ ")" ^^
      { case _ ~ start ~ _ ~ stmt ~ _ => TrueRangeStatement(TimeRange(Some(start), None), stmt) }
    "ValidTo(" ~ timeStamp ~ "," ~ statement ~ ")" ^^
      { case _ ~ end ~ _ ~ stmt ~ _ => TrueRangeStatement(TimeRange(None, Some(end)), stmt) }

  def timeStamp: Parser[Int] = "[0-9]*".r ^^ {_.toInt}
}

abstract class TemporalStatement

case class TrueRangeStatement(range: TimeRange, statement: Statement) extends TemporalStatement

case class TrueAlwaysStatement(statement: Statement) extends TemporalStatement

case class TimeRange(start: Option[Int], end: Option[Int])


