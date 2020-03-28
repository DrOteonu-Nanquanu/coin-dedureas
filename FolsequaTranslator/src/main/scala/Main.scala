import parser._
import eprover._

import scala.util.Success
import sys.process._
import scala.util.parsing.combinator._

object Main {
  def main(args: Array[String]): Unit = {
    println("Hello World")

    test()
  }

  def evaluate_fofsequa(knowledge_base: String, goal: String): Option[String] = {
    val parsed_knowledge_base = FolseqParser.parse(FolseqParser.fofsequa_document, knowledge_base) match {
      case FolseqParser.Success(result, next) => result
      case _ => {
        println("parsing knowledge base not succesfull")
        return None
      }
    }

    val parsed_goal = FolseqParser.parse(FolseqParser.statement, goal) match {
      case FolseqParser.Success(result, next) => result
      case _ => {
        println("parsing goal not succesful")
        return None
      }
    }

    val eprover_answer = Eprover.evaluate_TPTP(FofsequaToFof.to_tptp(parsed_knowledge_base, parsed_goal))
    val answer_variables = Eprover.get_answer_tuples(eprover_answer)

    // TODO: turn eprover's answer into a statement into the answer lang
    Some(eprover_answer)
  }

  def test(): Unit = {
    println()
    test_parse()
    println()

    /*
    println()
    test_stringify()
    println()

    println()
    test_translate()
    println()

    println()
    test_prove()
    println()

    println()
    test_fofsequa()
    println()*/
  }

  def test_fofsequa() = {
    evaluate_fofsequa("""P('x')""", """![x from s_]: P(x)""") match {
      case Some(output) => println(output)
      case None => println("something went wrong")
    }
  }

  def test_prove() = {
    // val result = Eprover.execute("/home/jcmaas/Documents/coin-dedureas/example_db.tptp")
    // println(result)

    println(Eprover.evaluate_TPTP(
      """
        |fof(axiom, axiom, livesIn("Marit", "Norway")).
        |fof(goal, question, ?[X]: livesIn(X, "Norway")).""".stripMargin))
  }

  def test_parse() = {
    println("test parse")

    val tests = Array(
      """P('a')""",
      """P('a') and P('b')""",
      """![a from p_]: P(a, 'b')"""
    )

    for(test <- tests) {
      val parsed = FolseqParser.parse(FolseqParser.fofsequa_document, test)
      println(parsed.get)
      println(test)
      println(parsed.get.map(_.toString))
    }

    println(FolseqParser.parse(FolseqParser.variableList, "a,b"))
    println(FolseqParser.parse(FolseqParser.quantifierArguments, "a,b"))
    println(FolseqParser.parse(FolseqParser.quantifiedFormula, "![a,b]: P(a, b)"))
    println(FolseqParser.parse(FolseqParser.quantifiedFormula, "![a]: P(a, b)"))

    // println(FolseqParser.parse(FolseqParser.fofsequa_document, """P("a") and P("b")"""))
    // println(FolseqParser.parse(FolseqParser.quantifiedFormula, """![a from p_]: P(a, "b")"""))
  }

  def test_stringify() = {
    println("test stringify")
    println(FofsequaToFof.stringify(AtomStatement(FolPredicate(UppercaseID("P")), Array())))
  }

  def test_translate() = {
    println("test translate")

    val tests = Array(
      "![x]: P(x)",
      "![x,y]: R(x, y)",
      "(![x]: (P(x) and (Q('a', x) => R(x))) or P('a'))",
      "P(x, y)",
      """P('x');
        |R('y')""".stripMargin,
    )

    for(test_fofsequa_string <- tests) {
      val parsed = FolseqParser.parseAll(FolseqParser.fofsequa_document, test_fofsequa_string)

      if (parsed.successful) {
        println("Parsed correctly: " + test_fofsequa_string)
        println(parsed.get)
        println(FofsequaToFof.stringify(parsed.get))
      }
      else {
        println("Parse error in:" + test_fofsequa_string)
        println(parsed)
      }
    }
  }
}
