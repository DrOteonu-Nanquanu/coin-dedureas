import eprover._

import scala.util.Success
import sys.process._
import scala.util.parsing.combinator._
import org.nanquanu.fofsequa._

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

  def test(): Unit = {
    var successful = true;

    println()
    successful &&= test_parse()
    println()


    println()
    test_stringify()
    println()

    println()
    successful &&= test_translate()
    println()

    println()
    test_prove()
    println()

    println()
    successful &&= test_fofsequa()
    println()

    println("successful: " ++ successful.toString)
  }

  def test_fofsequa(): Boolean = {
    evaluate_fofsequa("""P('x')""", """![x from s_]: P(x)""") match {
      case Some(output) => { println(output); true }
      case None => {println("something went wrong"); false}
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

  def test_parse(): Boolean = {
    println("test parse")

    var success = true

    val tests = Array(
      """P('a')""",
      """P('a') and P('b')""",
      """![a from p_]: P(a, 'b')"""
    )

    for(test <- tests) {
      val parsed = FolseqParser.parse(FolseqParser.fofsequa_document, test)

      if (parsed.successful) {
        println(parsed.get)
        println(test)
        println(parsed.get.map(_.toString))
      }
      else {
        success = false
      }
    }

    val tests_2 = List(
      (FolseqParser.variable_list, "a,b"),
      (FolseqParser.quantifier_arguments, "a,b"),
      (FolseqParser.quantified_formula, "![a,b]: P(a, b)"),
      (FolseqParser.quantified_formula, "![a]: P(a, b)")
    )

    for((parser, statement) <- tests_2) {
      val parsed = FolseqParser.parse(parser, statement)

      if (parsed.successful) {
        println(parsed.get)
        println(statement)
      }
      else {
        success = false
      }
    }

    // println(FolseqParser.parse(FolseqParser.fofsequa_document, """P("a") and P("b")"""))
    // println(FolseqParser.parse(FolseqParser.quantifiedFormula, """![a from p_]: P(a, "b")"""))

    success
  }

  def test_stringify() = {
    println("test stringify")
    println(FofsequaToFof.stringify(AtomStatement(FolPredicate(UppercaseID("P")), List[FolTerm]())))
  }

  def test_translate(): Boolean = {
    println("test translate")

    val tests = Array(
      "![x]: P(x)",
      "![x,y]: R(x, y)",
      "(![x]: (P(x) and (Q('a', x) => R(x))) or P('a'))",
      "P(x, y)",
      """P('x');
        |R('y')""".stripMargin,
    )

    var success = true

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
        success = false
      }
    }

    success
  }
}
