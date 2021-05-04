package org.nanquanu.fofsequa_reasoner.temporal

import org.nanquanu.fofsequa._

object TemporalReasoner {
  def answer(query: String, kb: String): String = {
    throw new NotImplementedError()
  }

  def answer(query: TemporalStatement, kb: List[TemporalStatement]): TemporalStatement = {
    val (quantifer, arguments, query_statement) = query match {
      case TrueAlwaysStatement(QuantifiedAlwaysStatement(quantifier, arguments, statement)) => {
        (quantifier, arguments, statement)
      }

      case _ => throw new Error("expected quantified top-level statement")
    }

    val query_range = extract_time_range(query_statement)

    val filtered_kb = filter_time_range(query_range, kb)

    val non_temporal_kb = no_none(
      filtered_kb.map(assert_always_true _ andThen convert_to_non_temporal)
    ) match {
      case None => throw new NotImplementedError("currently only top-level temporal statements are allowed")
      case Some(kb) => kb
    }

    val answer_tuples = null
    throw new NotImplementedError()
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
        case Some(start_timestamp) => start_timestamp > statement_start_timestamp
      }
    }

    val ends_earlier = stmt_end match {
      case None => true
      case Some(statement_end_timestamp) => end match {
        case None => false
        case Some(end_timestamp) => end_timestamp < statement_end_timestamp
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
}
