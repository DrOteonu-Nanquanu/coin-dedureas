package org.nanquanu.fofsequa_reasoner
import eprover._
import org.nanquanu.fofsequa
import org.nanquanu.fofsequa._
import org.nanquanu.fofsequa_reasoner.errors.{Cli_exception, Format_exception, Kb_parse_exception, Query_parse_exception, Invalid_query_exception}

import scala.collection.immutable.HashMap
import scala.io.{Source, StdIn}
import scala.util.{Failure, Success, Try}

object Reasoner {
  /* This object puts everything together to create the query-answering functions.
   * The final function that answers a Fofsequa query string based on a Fofsequa knowledge_base string, and returns the Fofsequa answer as a string, is called `answer`
   */

  def answer(knowledge_base: String, query: String): Try[String] = 
    parse(knowledge_base, query)                            // Parse query and kb
    .flatMap({case (parsed_knowledge_base, parsed_query) => 
      answer_parsed(parsed_knowledge_base, parsed_query)    // Apply reasoner
    })
    .map(_.toString)                                        // Convert back to text

  def parse(fsq_knowledge_base: String, fsq_query: String): Try[(List[Statement], Statement)] =
    parse_knowledge_base(fsq_knowledge_base).flatMap(parsed_knowledge_base =>
        parse_query(fsq_query).map((parsed_knowledge_base, _)))

  def answer_parsed(knowledge_base: List[Statement], query: Statement): Try[Statement] =
    answer_tuples(knowledge_base, query)          // Apply E prover
      .map(to_constant_set)                       // Convert answer to `BasicConstantSet`
      .flatMap(substitute_constant_set(query, _)) // Substitute the answer in the original question

  // Parses a knowledge base in Fofsequa syntax into a list of statements
  def parse_knowledge_base(fsq_knowledge_base: String): Try[List[Statement]] =
    FolseqParser.parseAll(FolseqParser.fofsequa_document, fsq_knowledge_base) match {
      case FolseqParser.Success(result, next) => Success(result)
      case error: FolseqParser.NoSuccess => return Failure(Kb_parse_exception(error.msg))
    }

  // Parses a query in Fofsequa syntax into a statement
  def parse_statement(fsq_statement: String): Try[Statement] =
    FolseqParser.parseAll(FolseqParser.statement, fsq_statement) match {
      case FolseqParser.Success(result, next) => Success(result)
      case error: FolseqParser.NoSuccess => return Failure(Query_parse_exception(error.msg))
    }

  val parse_query = parse_statement _

  private val missing_quantifier_over_pattern_variable_exception = Failure(Invalid_query_exception("Query should start with a quantifier over a pattern variable"))

  // Returns the length of the tuple that is quantified over
  private def tuple_size(query: Statement): Try[Int] = query match {
    case QuantifiedStatement(quantifier, arguments, statement) => {
      quantifier match {
        case Exists() => return Failure(Invalid_query_exception("Existential quantifiers with a pattern variable aren't supported"))
        case _ => ()
      }

      arguments match {
        case ConstantSetQuantifierArguments(variables, constant_set) => constant_set match {
          case BasicConstantSet(constants) => return missing_quantifier_over_pattern_variable_exception
          case PatternVar(name) => Success(variables.length)
        }
        case BasicQuantifierArguments(variables) => return missing_quantifier_over_pattern_variable_exception
      }
    }
    // case _ => Failure(new IllegalArgumentException("Expected top-level statement of the query to be quantified"))
    case _ => missing_quantifier_over_pattern_variable_exception
  }

  // Converts the knowledge_base and query to fof, applies E prover on it, and extracts the answer tuples from E's answer
  def answer_tuples(knowledge_base: List[Statement], query: Statement): Try[List[List[QuotedString]]] = tuple_size(query).map(tuple_size =>
    Eprover.get_answer_tuples(Eprover.evaluate_TPTP(
      FofsequaToFof.to_tptp(knowledge_base, query),
    ), tuple_size)
  )
  
  // Converts the result from `answer_tuples` to a BasicConstantSet
  def to_constant_set(tuples: List[List[QuotedString]]): BasicConstantSet = BasicConstantSet(
    tuples.map (list_of_constant_names => {
      val answers_as_constants = list_of_constant_names.map ({
        case SingleQuotedString(constant_name) =>
          Constant(LowercaseID(constant_name.toLowerCase()))
        case DoubleQuotedString(digital_entity_text) =>
          DigitalEntity(digital_entity_text)
      })
      ConstantTuple (answers_as_constants)
    })
  )

  // Replaces the pattern variable in `query` with `answer`
  def substitute_constant_set(query: Statement, answer: BasicConstantSet): Try[Statement] = query match {
      case QuantifiedStatement(quantifier, arguments, statement) => arguments match {
        case ConstantSetQuantifierArguments(variables, constant_set) => constant_set match {
          case PatternVar(_name) => {
            Success(QuantifiedStatement(quantifier, ConstantSetQuantifierArguments(variables, answer), statement))
          }
          case _ => Failure(Format_exception("No pattern variable found in query statement"))
        }
        case _ => Failure(Format_exception("Query statement doesn't quantify over constant set"))
      }
      case _ => Failure(Format_exception("Query statement isn't quantified"))
    }
}
