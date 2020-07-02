import org.scalatest.flatspec.AnyFlatSpec

import scala.util.{Failure, Success}
// import org.scalatest.funsuite.AnyFunSuite
import org.nanquanu.fofsequa_reasoner.Main

class TestBase extends AnyFlatSpec {
  "Complete test on test files" should "succeed" in {
    for((file, query) <- List (
      ("test_fofsequa_kb.txt", "![x from s_]: Q(x)"),
      ("test_fofsequa_kb2.txt", "![x, y from s_]: R(x, y)"),
      ("test_fofsequa_kb2.txt", "![x, y, z from s_]: T(x, y, z)"),
    )) {
      assert(Main.evaluate_file(file, query) match {
        case Success(value) => true
        case Failure(exception) => {
          println("error in file '" ++ file + "' with file query '" ++ query ++ "'")

          println(exception)
          false
        }
      })
    }

    assert(Main.evaluate_file("test_fofsequa_kb.txt", "![x from s_]: Q(x)").isSuccess)
    assert(Main.evaluate_file("test_fofsequa_kb2.txt", "![x, y from s_]: R(x, y)").isSuccess)
    assert(Main.evaluate_file("test_fofsequa_kb2.txt", "![x, y, z from s_]: T(x, y, z)").isSuccess)
  }
}
