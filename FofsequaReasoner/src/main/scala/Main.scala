package org.nanquanu.fofsequa_reasoner
import eprover._
import org.nanquanu.fofsequa
import org.nanquanu.fofsequa._
import org.nanquanu.fofsequa_reasoner.errors.{Format_exception, Kb_parse_exception, Query_parse_exception, Reasoning_exception, Reasoning_parse_exception}

import scala.io.{Source, StdIn}
import scala.util.{Failure, Success, Try}

object Main {
  def main(args: Array[String]): Unit = {
    test.Test.test()
    // console_interface(args)
  }

  def console_interface(args: Array[String]): Unit = {
    for(arg <- args) {
      println(arg)
    }

    // evaluate_file("./test_fofsequa_kb.txt", read_input_query)
    test.Test.test()
  }

  def read_input_query: String = {
    var query = StringBuilder.newBuilder
    do {
      query.append(StdIn.readLine())
    }
    while(query.charAt(query.length - 1) != ';')

    query.deleteCharAt(query.length - 1)

    query.toString
  }

  def evaluate_file(file_path: String, query: String): Try[String] = {
    val file = Source.fromFile(file_path)
    val lines = try file.getLines() mkString "\n" finally file.close()

    evaluate_fofsequa_to_string(lines, query)
  }

  def evaluate_to_answer_tuples(knowledge_base: String, parsed_goal: Statement): Try[List[List[String]]] = {
    val parsed_knowledge_base = FolseqParser.parseAll(FolseqParser.fofsequa_document, knowledge_base) match {
      case FolseqParser.Success(result, next) => result
      case error: FolseqParser.NoSuccess => return Failure(Kb_parse_exception(error))
    }

    val eprover_answer = Eprover.evaluate_TPTP(FofsequaToFof.to_tptp(parsed_knowledge_base, parsed_goal))

    Success(Eprover.get_answer_tuples(eprover_answer))
  }

  def evaluate_fofsequa(knowledge_base: String, goal: String): Try[Statement] = {
    val parsed_goal = FolseqParser.parseAll(FolseqParser.statement, goal) match {
      case FolseqParser.Success(result, next) => result
      case error: FolseqParser.NoSuccess => return Failure(Query_parse_exception(error))
    }

    // TODO: support constant tuples
    val answer_constants = evaluate_to_answer_tuples(knowledge_base, parsed_goal) match {
      case error: Failure[List[List[String]]] => return Failure(error.exception)
      case Success(answer_tuples) => answer_tuples.map (answers => {
        val answers_as_constants = answers.map (
        constant_name => Constant (LowercaseID (constant_name.toLowerCase () ) )
        )
        ConstantTuple (answers_as_constants)
      })
    }
    println("parsed goal:")
    println(parsed_goal)
    println("-------")
    // turn eprover's answer into a statement into the answer lang
    val substituted = parsed_goal match {
      case QuantifiedStatement(quantifier, arguments, statement) => arguments match {
        case ConstantSetQuantifierArguments(variables, constant_set) => constant_set match {
          case PatternVar(name) => {
            println("variables_length = " + variables.length)
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
