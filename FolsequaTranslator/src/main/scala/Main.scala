import parser._
import eprover._
import sys.process._

object Main {
  def main(args: Array[String]): Unit = {
    println("Hello World")

    test()

    // println(FofsequaToFof.stringify())
  }

  def test(): Unit = {
    println()
    test_parse()

    println()
    test_stringify()

    println()
    test_translate()
    println()
  }

  def test_prove() = {
    // val result = Eprover.execute("/home/jcmaas/Documents/coin-dedureas/example_db.tptp")
    // println(result)

    println(Eprover.evaluate_TPTP("""fof(goal, conjecture, livesIn("Marit", "Norway"))."""))
  }

  def test_parse() = {
    println("test parse")

    val tests = Array(
      """P('a')""",
      """P('a') and P('b')""",
      """![a from p_]: P(a, 'b')"""
    )

    for(test <- tests) {
      println(FolseqParser.parse(FolseqParser.fofsequa_document, test))
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
      "![x]: (P(x) and (Q('a', x) => R(x))) or P('a')"
    )

    for(test_fofsequa_string <- tests) {
      val parsed = FolseqParser.parse(FolseqParser.statement, test_fofsequa_string)

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
