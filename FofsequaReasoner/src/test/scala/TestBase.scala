import org.nanquanu.fofsequa_reasoner.eprover.Eprover
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.{Failure, Success}
// import org.scalatest.funsuite.AnyFunSuite
import org.nanquanu.fofsequa_reasoner.FofsequaReasoner

class Test_base extends AnyFlatSpec {
  // Eprover.PATH_TO_EPROVER = Some("./eprover-executable/PROVER/eprover")

  "Complete test on test files" should "succeed" in {
    for((file, query) <- List (
      ("test_fofsequa_kb.txt", "![x from s_]: Q(x)"),
      ("test_fofsequa_kb2.txt", "![x, y from s_]: R(x, y)"),
      ("test_fofsequa_kb2.txt", "![x, y, z from s_]: T(x, y, z)"),
      ("test_fofsequa_kb2.txt", "![x from s_]: ?[y]: R(x, y)"), // TODO: this query returns ![x from {<'r','p'>,<'x','y'>,<'p','r'>,<'y','x'>}]:?[y]:R(x,y)
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
}
