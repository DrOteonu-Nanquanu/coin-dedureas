package org.nanquanu.fofsequa_reasoner.test
import org.nanquanu.fofsequa_reasoner._
import org.nanquanu.fofsequa_reasoner.Main.evaluate_fofsequa_to_string
import eprover.{Eprover, FofsequaToFof}
import org.nanquanu.fofsequa.{AtomStatement, FolPredicate, FolseqParser, UppercaseID}

import scala.util.{Failure, Success}

/*
  This is the old test code. It is currently unused, since we're using ScalaTest now.
  The new tests are located in src/test/scala/
  Some of these might be converted to ScalaTest tests in the future.
 */

object Test {

  def test(): Unit = {
    var successful = true;

    println()
    successful &&= test_parse()
    if(!successful) {
      println("error test_parse")
    }
    println()


    println()
    test_stringify()
    if(!successful) {
      println("error test_stringify")
    }
    println()

    println()
    successful &&= test_translate()
    if(!successful) {
      println("error test_translate")
    }
    println()

    println()
    test_prove()
    println()

    println()
    successful &&= test_fofsequa()
    if(!successful) {
      println("error test_fofsequa")
    }
    println()

    println()
    successful &&= test_file
    if(!successful) {
      println("error test_file")
    }
    println()

    println("successful: " ++ successful.toString)
  }

  def test_file: Boolean = {
    /*Main.evaluate_file("./test_fofsequa_kb.txt", "![x from s_]: Q(x)") &&
    Main.evaluate_file("./test_fofsequa_kb2.txt", "![x, y from s_]: R(x, y)") &&
    Main.evaluate_file("./test_fofsequa_kb2.txt", "![x, y, z from s_]: T(x, y, z)")8/
     */
    true
  }

  def test_fofsequa(): Boolean = {
    evaluate_fofsequa_to_string("""P('x')""", """![x from s_]: P(x)""") match {
      case Success(output) => { println(output); true }
      case Failure(exception) => {println("something went wrong"); false}
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
    println(FofsequaToFof.stringify(AtomStatement(FolPredicate(UppercaseID("P")), List())))
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
