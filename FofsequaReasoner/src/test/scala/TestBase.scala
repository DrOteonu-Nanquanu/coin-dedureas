import org.nanquanu.fofsequa_reasoner
import org.nanquanu.fofsequa_reasoner.eprover.Eprover
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.{Failure, Success}
// import org.scalatest.funsuite.AnyFunSuite
import org.nanquanu.fofsequa_reasoner.FofsequaReasoner
import org.nanquanu.fofsequa_reasoner.Reasoner

class Test_base extends AnyFlatSpec {
  // Eprover.PATH_TO_EPROVER = Some("./eprover-executable/PROVER/eprover")

  "Complete test on test files" should "succeed" in {
    for((file, query) <- List (
      ("test_fofsequa_kb.txt", "![x from s_]: Q(x)"),
      ("test_fofsequa_kb2.txt", "![x, y from s_]: R(x, y)"),
      ("test_fofsequa_kb2.txt", "![x, y, z from s_]: T(x, y, z)"),
      ("test_fofsequa_kb2.txt", "![x from s_]: ?[y]: R(x, y)"),
      ("test_fofsequa_kb3.fsq", "![x, y from s_]: Description(x, y)")
    )) {
      assert(FofsequaReasoner.evaluate_file(file, query) match {
        case Success(value) => {
          println(value)
          true
        }
        case Failure(exception) => {
          println("error in file '" ++ file + "' with file query '" ++ query ++ "'")

          println(exception)
          false
        }
      })
    }
  }

  "Quantifiers over constant sets" should "parse and resolve correctly" in {
    val kb = "![x from {'a', 'b', 'c'}]: P(x);"
    val query = "![x from s_]: P(x)"

    println(
      fofsequa_reasoner.eprover.FofsequaToFof.stringify_document(
        org.nanquanu.fofsequa.FolseqParser.parse_document_or_throw(kb)
    ))

    println(
      Reasoner.answer(kb, query)
    )
  }

  "Quantifiers over constant tuple sets" should "parse and resolve correctly" in {
    val kb = "![x, y from {<'a', 'b'>, <'c', 'd'>}]: R(x, y);"
    val query = "![x, y from s_]: R(x, y)"

    println(
      fofsequa_reasoner.eprover.FofsequaToFof.stringify_document(
        org.nanquanu.fofsequa.FolseqParser.parse_document_or_throw(kb)
        ))

    println(
      Reasoner.answer(kb, query)
    )
  }

  "Temporal Statement" should "answer correctly" in {
    val kb = """P('x');"""

    val query = "![x from s_]: Q(x)"

    fofsequa_reasoner.temporal.TemporalReasoner.answer(kb, query) match {
      case Success(answer) => println(answer)
      case Failure(err) => throw err
    }
  }
}
