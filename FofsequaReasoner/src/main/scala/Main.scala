package org.nanquanu.fofsequa_reasoner

import eprover._
import org.nanquanu.fofsequa
import org.nanquanu.fofsequa._
import org.nanquanu.fofsequa_reasoner.errors.{Cli_exception, Format_exception, Kb_parse_exception, Query_parse_exception, Invalid_query_exception}

import org.nanquanu.fofsequa_reasoner.temporal._

import scala.collection.immutable.HashMap
import scala.io.{Source, StdIn}
import scala.util.{Failure, Success, Try}

object FofsequaReasoner {
  def main(args: Array[String]): Unit = {
    console_interface(args)
  }



  // Reads a KB and query from the commandline arguments and STDIN, evaluates them, and outputs the result
  def console_interface(args: Array[String]): Unit = {
    var maybe_kb: Option[String] = None
    var maybe_query: Option[String] = None
    var temporal = false

    // Read commandline argumetns
    for(i <- 0 to args.length - 1) {
      lazy val next = args(i + 1)

      args(i) match {
        case "--kb" | "-k" => maybe_kb = Some(next)
        case "--query" | "-q" => maybe_query = Some(next)
        case "--temporal" | "-t" => temporal = true
        case _ => ()
      }
    }

    val knowledge_base = maybe_kb match {
      case Some(file_name) => read_file(file_name) match {
        case Success(kb) => kb
        case Failure(err) => throw err
      }
      case None => read_input_statements()
    }

    val query = maybe_query match {
      case Some(q) => q
      case None => read_input_statements()
    }

    val answer =
      if(temporal) { TemporalReasoner.answer(knowledge_base, query) }
      else { Reasoner.answer(knowledge_base, query) }

    // Output result or error
    answer match {
      case Success(statement) => println(statement)
      case Failure(exception) => throw exception
    }
  }

  // Read statements from STDIN
  def read_input_statements(): String = {
    var query = new StringBuilder
    do {
      query.append(StdIn.readLine())
    }
    while(query.charAt(query.length - 1) == ';')

    query.toString
  }

  def read_file(file_path: String): Try[String] = {
    val file = try {
      Source.fromFile(file_path)
    } catch {
      case error: Throwable => return Failure(error)
    }

    val lines = try file.getLines() mkString "\n" finally file.close()

    Success(lines)
  }

  // Evaluate `query` on the knowledge base in the file named `file_path`
  def evaluate_file(file_path: String, query: String): Try[String] = {
    val file = try {
      Source.fromFile(file_path)
    } catch {
      case error: Throwable => return Failure(error)
    }

    val lines = try file.getLines() mkString "\n" finally file.close()

    Reasoner.answer(lines, query)
    // evaluate_fofsequa_to_string(lines, query)
  }

  def evaluate_to_answer_tuples(knowledge_base: String, parsed_goal: Statement): Try[List[List[QuotedString]]] = {
    val parsed_knowledge_base = FolseqParser.parseAll(FolseqParser.fofsequa_document, knowledge_base) match {
      case FolseqParser.Success(result, next) => result
      case error: FolseqParser.NoSuccess => return Failure(Kb_parse_exception(error.msg))
    }

    // TODO: check if containing valid quantifier over pattern variable

    val missing_quantifier_over_pattern_variable_exception = Failure(Invalid_query_exception("Goal should start with a quantifier over a pattern variable"))

    // Get the amount of elements each tuple should contain in the constant tuple set that will replace the pattern variable
    val tuple_size = parsed_goal match {
      case QuantifiedStatement(quantifier, arguments, statement) => {
        quantifier match {
          case Exists() => return Failure(Invalid_query_exception("Existential quantifiers with a pattern variable aren't supported"))
          case _ => ()
        }

        arguments match {
          case ConstantSetQuantifierArguments(variables, constant_set) => constant_set match {
            case BasicConstantSet(constants) => return  missing_quantifier_over_pattern_variable_exception
            case PatternVar(name) => variables.length
          }
          case BasicQuantifierArguments(variables) => return missing_quantifier_over_pattern_variable_exception
        }
      }
    }

    val eprover_answer = Eprover.evaluate_TPTP(FofsequaToFof.to_tptp(parsed_knowledge_base, parsed_goal))

    Success(Eprover.get_answer_tuples(eprover_answer, tuple_size))
  }

  def evaluate_fofsequa(knowledge_base: String, goal: String): Try[Statement] = {
    val parsed_goal = FolseqParser.parseAll(FolseqParser.statement, goal) match {
      case FolseqParser.Success(result, next) => result
      case error: FolseqParser.NoSuccess => return Failure(Query_parse_exception(error.msg))
    }

    // Get the set of tuples that should be substitute the pattern variable
    val answer_constants = evaluate_to_answer_tuples(knowledge_base, parsed_goal) match {
      case error: Failure[List[List[QuotedString]]] => return Failure(error.exception)

      case Success(answer_tuples) => answer_tuples.map (list_of_constant_names => {
        // Convert list of constant names to constant tuple
        val answers_as_constants = list_of_constant_names.map (
          {
            case SingleQuotedString(constant_name) => Constant(LowercaseID(constant_name.toLowerCase()))
            case DoubleQuotedString(digital_entity_text) => DigitalEntity(digital_entity_text)
          }
        )
        ConstantTuple (answers_as_constants)
      })
    }

    // Substitute the pattern variable for the calculated set of answer tuples
    val substituted = parsed_goal match {
      case QuantifiedStatement(quantifier, arguments, statement) => arguments match {
        case ConstantSetQuantifierArguments(variables, constant_set) => constant_set match {
          case PatternVar(_name) => {
            QuantifiedStatement(quantifier, ConstantSetQuantifierArguments(variables, BasicConstantSet(answer_constants)), statement)
          }
          case _ => return Failure(Format_exception("No pattern variable found in query statement"))
        }
        case _ => return Failure(Format_exception("Query statement doesn't quantify over constant set"))
      }
      case _ => return Failure(Format_exception("Query statement isn't quantified"))
    }

    Success(substituted)
  }

  def evaluate_fofsequa_to_string(knowledge_base: String, goal: String): Try[String] = evaluate_fofsequa(knowledge_base, goal).map(_.toString())
}
