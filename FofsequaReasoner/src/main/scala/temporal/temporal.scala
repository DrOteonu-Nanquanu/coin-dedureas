package org.nanquanu.fofsequa_reasoner.temporal

import org.nanquanu.fofsequa._
import org.nanquanu.fofsequa_reasoner._
import org.nanquanu.fofsequa_reasoner.eprover._
import scala.util.{Failure, Success, Try}
import org.nanquanu.fofsequa_reasoner.errors.{Cli_exception, Format_exception, Kb_parse_exception, Query_parse_exception, Invalid_query_exception}

object TemporalReasoner {
  def answer(knowledge_base: String, query: String): Try[String] = {
    parse(knowledge_base, query)
    .flatMap({ case (parsed_kb, parsed_query) =>
      answer_parsed(parsed_kb, parsed_query) })
    .map(_.toString)
  }

  def parse_query(query: String): Try[TemporalStatement] = FofseqTemporalParser.parseAll(FofseqTemporalParser.temporal_statement, query) match {
    case FofseqTemporalParser.Success(parsed, next) => Success(parsed)
    case error: FofseqTemporalParser.NoSuccess => return Failure(Query_parse_exception(error.msg))
  }
  
  def parse_knowledge_base(knowledge_base: String): Try[List[TemporalStatement]] = FofseqTemporalParser.parseAll(FofseqTemporalParser.fofsequa_temporal_document, knowledge_base) match {
    case FofseqTemporalParser.Success(parsed, next) => Success(parsed)
    case error: FofseqTemporalParser.NoSuccess => return Failure(Kb_parse_exception(error.msg))
  }

  def parse(knowledge_base: String, query: String): Try[(List[TemporalStatement], TemporalStatement)] =
    parse_knowledge_base(knowledge_base).flatMap(kb =>
      parse_query(query).map((kb, _))
    )


  def answer_parsed(knowledge_base: List[TemporalStatement], query: TemporalStatement): Try[TemporalStatement] = 
    answer_to_constant_set(knowledge_base, query)
      .map(Reasoner.to_constant_set _)
      .flatMap(substitute_constant_set(query, _))

  def extract_query_info(query: TemporalStatement): Try[(Quantifier, QuantifierArguments, Statement, TimeRange)] = query match {
      case TrueAlwaysStatement(QuantifiedAlwaysStatement(quantifier, arguments, statement)) => {
        convert_to_non_temporal(statement) match {
          case Some(non_temporal_statement) => Success((quantifier, arguments, non_temporal_statement, TimeRange(None, None)))
          case None => Failure(Query_parse_exception("The query can only contain temporal constructs at the top-level"))
        }
      }

      case TrueRangeStatement(range, QuantifiedStatement(quantifier, quantifier_arguments, inner_statement)) =>
        Success((quantifier, quantifier_arguments, inner_statement, range))
    

      case _ => Failure(Query_parse_exception("Expected query to start with either a quantifier or a temporal construct"))
  }

  def answer_to_constant_set(knowledge_base: List[TemporalStatement], query: TemporalStatement): Try[List[List[QuotedString]]] = {
    val (quantifer, arguments, non_temporal_query_statement, query_range) = extract_query_info(query) match {
      case Success(info) => info
      case Failure(err) => return Failure(err)
    }

    val non_temporal_query = QuantifiedStatement(quantifer, arguments, non_temporal_query_statement)

    val filtered_kb = filter_time_range(query_range, knowledge_base)

    val non_temporal_kb = no_none(
      filtered_kb.map(assert_always_true _ andThen convert_to_non_temporal)
    ) match {
      case None => throw new NotImplementedError("currently only top-level temporal statements are allowed")
      case Some(kb) => kb
    }

    Reasoner.answer_tuples(non_temporal_kb, non_temporal_query)
  }

  def filter_time_range(range: TimeRange, statements: List[TemporalStatement]): List[TemporalStatement] =
    statements.filter(contains_range(range, _))

  def contains_range(range: TimeRange, statement: TemporalStatement): Boolean = {
    val TimeRange(stmt_start, stmt_end) = extract_time_range(statement)
    val TimeRange(start, end) = range

    val starts_later = stmt_start match {
      case None => true
      case Some(statement_start_timestamp) => start match {
        case None => false
        case Some(start_timestamp) => start_timestamp >= statement_start_timestamp
      }
    }

    val ends_earlier = stmt_end match {
      case None => true
      case Some(statement_end_timestamp) => end match {
        case None => false
        case Some(end_timestamp) => end_timestamp <= statement_end_timestamp
      }
    }

    starts_later && ends_earlier
  }

  def extract_time_range(statement: TemporalStatement): TimeRange = statement match {
    case TrueAlwaysStatement(_) => TimeRange(None, None)
    case TrueRangeStatement(stmt_range, _) => TimeRange(stmt_range.start, stmt_range.end)
  }

  def assert_always_true(statement: TemporalStatement): TemporalStatement = statement match {
    case TrueRangeStatement(_, stmt) => convert_to_temporal(stmt)
    case _ => statement
  }

  def convert_to_temporal(statement: Statement): TemporalStatement = TrueAlwaysStatement(statement match {
    case BinaryConnectiveStatement(first, operator, last) => {
      val first_converted = convert_to_temporal(first)

      val last_converted = convert_to_temporal(last)
      BinaryConnectiveAlwaysStatement(first_converted, operator, last_converted)
    }
    case UnaryConnectiveStatement(connective, inner_statement) => {
      val inner_converted = convert_to_temporal(inner_statement)
      UnaryConnectiveAlwaysStatement(connective, inner_converted)
    }
    case QuantifiedStatement(quantifier, arguments, inner_statement) => {
      val inner_converted = convert_to_temporal(inner_statement)
      QuantifiedAlwaysStatement(quantifier, arguments, inner_converted)
    }
    case AtomStatement(predicate, terms) => AtomAlwaysStatement(predicate, terms)
  })

  def convert_to_non_temporal(statement: TemporalStatement): Option[Statement] = statement match {
    case TrueRangeStatement(_, _) => None
    case TrueAlwaysStatement(always_statement) => always_statement match {
      case BinaryConnectiveAlwaysStatement(first, operator, last) =>
        convert_to_non_temporal(first).flatMap(
          first_converted => convert_to_non_temporal(last).map(
            last_converted => BinaryConnectiveStatement(first_converted, operator, last_converted)
          )
        )
      case UnaryConnectiveAlwaysStatement(connective, inner_statement) =>
        convert_to_non_temporal(inner_statement).map(UnaryConnectiveStatement(connective, _))
      case QuantifiedAlwaysStatement(quantifier, arguments, inner_statement) =>
        convert_to_non_temporal(inner_statement).map(QuantifiedStatement(quantifier, arguments, _))
      case AtomAlwaysStatement(predicate, terms) => Some(AtomStatement(predicate, terms))
    }
  }

  // Returns None if any element of `list` is None, or Some(cleaned) otherwise, where cleaned is equal to list, but without the wrapping of Some(_)
  def no_none[A](list: List[Option[A]]): Option[List[A]] = list.foldRight(Some(List()) : Option[List[A]])((next, acc) => {
    acc.flatMap(ls => next.map(nxt => nxt :: ls))
  })

  def substitute_constant_set(query: TemporalStatement, answer: ConstantSet): Try[TemporalStatement] =
    query match {
      case TrueAlwaysStatement(QuantifiedAlwaysStatement(quantifier, ConstantSetQuantifierArguments(variables, PatternVar(_)), statement)) => {
        Success(
          TrueAlwaysStatement(QuantifiedAlwaysStatement(
            quantifier,
            ConstantSetQuantifierArguments(variables, answer), statement
          ))
        )
      }
      case TrueRangeStatement(range, QuantifiedStatement(quantifier, ConstantSetQuantifierArguments(variables, PatternVar(_)), statement)) => Success(
        TrueRangeStatement(
          range,
          QuantifiedStatement(
            quantifier,
            ConstantSetQuantifierArguments(
              variables,
              answer
            ),
            statement
          )
        )
      )

      case _ => throw new Error("expected quantified top-level statement")
    }
}
