package eprover

import java.io.{File, PrintWriter}
import sys.process._

object Eprover {
  val PATH_TO_EPROVER: String = "./eprover-executable/PROVER/eprover"

  def evaluate_TPTP(tptp: String) : String = {
    val file_name = "/tmp/evaluate.tptp"

    val file = new File(file_name)
    val writer = new PrintWriter(file)
    writer.write(tptp)
    writer.close

    val result = execute(file_name)

    file.delete

    result
  }

  val eprover_options = " --auto -s --answers "

  def execute(file_name: String) : String = ((PATH_TO_EPROVER + eprover_options + file_name) lineStream_!).mkString("\n")

  def get_answer_tuples(eprover_answer: String): List[List[String]] = {
    val answer_start = "# SZS answers Tuple [["

    eprover_answer.split("\n").toList.flatMap(line => {
      val (start, end) = line.splitAt(answer_start.length)

      if(start == answer_start) {
        val (quoted_variables, tail) = end.splitAt(end.indexOf(']'))
        val variable_names = quoted_variables.filter(_ != ' ')
          .split(",").map(
          quoted_variable => quoted_variable.slice(1, quoted_variable.length - 1)
        )

        if(tail != "]|_]"){
          throw new Error("unexpected tail: " + tail)
        }

        List(variable_names.toList)
      }
      else {
        List()
      }
    })
  }
}
