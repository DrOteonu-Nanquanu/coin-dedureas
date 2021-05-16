package org.nanquanu.fofsequa

import java.text.ParseException

import scala.util.parsing.combinator._

/*
 Parser
*/
object FofseqTemporalParser extends FolseqParserBase {
  def fofsequa_temporal_document: Parser[List[TemporalStatement]] = (temporal_statement ~ ';' ^^ { case stmt ~ _ => stmt } ).*

  def temporal_statement: Parser[TemporalStatement] =
    true_always_statement |
    "ValidBetween(" ~ timeStamp ~ "," ~ timeStamp ~ "," ~ statement ~ ")" ^^
      { case _ ~ start ~ _~ end ~ _ ~ stmt ~ _ => TrueRangeStatement(TimeRange(Some(start), Some(end)), stmt) } |
    "ValidFrom(" ~ timeStamp ~ "," ~ statement ~ ")" ^^
      { case _ ~ start ~ _ ~ stmt ~ _ => TrueRangeStatement(TimeRange(Some(start), None), stmt) }
    "ValidTo(" ~ timeStamp ~ "," ~ statement ~ ")" ^^
      { case _ ~ end ~ _ ~ stmt ~ _ => TrueRangeStatement(TimeRange(None, Some(end)), stmt) }

  def timeStamp: Parser[Int] = "[0-9]*".r ^^ {_.toInt}

  def true_always_statement: Parser[TemporalStatement] =
    unary_connective ~ true_always_statement ^^ {
      case connective ~ stmt => TrueAlwaysStatement(UnaryConnectiveAlwaysStatement(connective, stmt))
    } |
    "(" ~ temporal_statement ~ binary_connective ~ temporal_statement ~ ")" ^^ {
      case _ ~ statement1 ~ connective ~ statement2 ~ _ => TrueAlwaysStatement(BinaryConnectiveAlwaysStatement(statement1, connective, statement2))
    } |
    // quantified_formula ^^ { formula => TrueAlwaysStatement(QuantifiedAlwaysStatement(formula.quantifier, formula.arguments, (formula.statement)))} |
    temporal_quantified_formula |
    temporal_atom
    


  def temporal_quantified_formula: Parser[TemporalStatement] = quantifier ~ "[" ~ quantifier_arguments ~ "]:" ~ temporal_statement ^^ {
    case quant ~ _ ~ quantArgs ~ _ ~ stmt => TrueAlwaysStatement(QuantifiedAlwaysStatement(quant, quantArgs, stmt))
  }

  def temporal_atom : Parser[TemporalStatement] = fol_atom ^^ {
    atom => TrueAlwaysStatement(AtomAlwaysStatement(atom.predicate, atom.terms))
  }

}

abstract class TemporalStatement

case class TrueRangeStatement(range: TimeRange, statement: Statement) extends TemporalStatement {
  override def toString: String = range match {
    case TimeRange(Some(start), Some(end)) =>
      "ValidBetween(" +
        start.toString + ", " +
        end.toString + ", " +
        statement.toString + ")"

    case TimeRange(Some(start), None) =>
      "ValidFrom(" +
        start.toString + ", " +
        statement.toString + ")"

    case TimeRange(None, Some(end)) =>
      "ValidTo(" +
        end.toString + ", " +
        statement.toString + ")"

    case _ => throw new IllegalArgumentException("TrueRangeStatement has start = None and end = None")
  }
}

case class TrueAlwaysStatement(statement: AlwaysStatement) extends TemporalStatement {
  override def toString: String = statement.toString
}

case class TimeRange(start: Option[Int], end: Option[Int])


sealed abstract class AlwaysStatement
case class BinaryConnectiveAlwaysStatement(pre_statement: TemporalStatement, connective: BinaryConnective, post_statement: TemporalStatement) extends AlwaysStatement {
  override def toString: String = pre_statement.toString + connective.toString + post_statement.toString
}
case class UnaryConnectiveAlwaysStatement(connective: UnaryConnective, post_statement: TemporalStatement) extends AlwaysStatement {
  override def toString: String = connective.toString + post_statement.toString
}
case class QuantifiedAlwaysStatement(quantifier: Quantifier, arguments: QuantifierArguments, statement: TemporalStatement) extends AlwaysStatement{
  override def toString: String = quantifier.toString + "[" + arguments.toString + "]:" + statement.toString
}

case class AtomAlwaysStatement(predicate: FolPredicate, terms: Seq[FolTerm]) extends AlwaysStatement{
  override def toString: String = predicate.toString + terms.map(_.toString).mkString("(", ",", ")")
}
