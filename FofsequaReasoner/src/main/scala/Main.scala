import eprover._
import org.nanquanu.fofsequa._

import scala.io.{Source, StdIn}

object Main {
  def main(args: Array[String]): Unit = {
    Test.test()
    // console_interface(args)
  }

  def console_interface(args: Array[String]): Unit = {
    for(arg <- args) {
      println(arg)
    }

    evaluate_file("./test_fofsequa_kb.txt", read_input_query)
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

  def evaluate_file(file_path: String, query: String): Boolean = {
    val file = Source.fromFile(file_path)
    val lines = try file.getLines() mkString "\n" finally file.close()

    evaluate_fofsequa(lines, query) match {
      case Some(answer) => {
        println(answer)
        true
      }
      case None => false
    }
  }

  def evaluate_fofsequa(knowledge_base: String, goal: String): Option[String] = {
    val parsed_knowledge_base = FolseqParser.parseAll(FolseqParser.fofsequa_document, knowledge_base) match {
      case FolseqParser.Success(result, next) => result
      case FolseqParser.Error(message, next) => {
        println(message)
        return None
      }
      case FolseqParser.Failure(message, next) => {
        println(message)
        return None
      }
      case FolseqParser.NoSuccess(message, next) => {
        println(message)
        return None
      }
      case _ => {
        println("parsing knowledge base not succesfull")
        return None
      }
    }

    val parsed_goal = FolseqParser.parseAll(FolseqParser.statement, goal) match {
      case FolseqParser.Success(result, next) => result
      case FolseqParser.Error(message, next) => {
        println(message)
        return None
      }
      case FolseqParser.Failure(message, next) => {
        println(message)
        return None
      }
      case FolseqParser.NoSuccess(message, next) => {
        println(message)
        return None
      }
      case _ => {
        println("parsing goal not succesful")
        return None
      }
    }

    val eprover_answer = Eprover.evaluate_TPTP(FofsequaToFof.to_tptp(parsed_knowledge_base, parsed_goal))
    println(eprover_answer)
    val answer_constants = Eprover.get_answer_tuples(eprover_answer).map(answer => Constant(LowercaseID(answer))).toArray

    // TODO: turn eprover's answer into a statement into the answer lang
    val substituted = parsed_goal match {
      case QuantifiedStatement(quantifier, arguments, statement) => arguments match {
        case ConstantSetQuantifierArguments(variable, constant_set) => constant_set match {
          case PatternVar(name) => QuantifiedStatement(quantifier, ConstantSetQuantifierArguments(variable, BasicConstantSet(answer_constants)), statement)
          case _ => None
        }
        case _ => None
      }
      case _ => None
    }

    Some(substituted.toString)
  }
}
