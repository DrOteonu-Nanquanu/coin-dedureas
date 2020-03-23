package eprover

import java.io.{File, PrintWriter}
import sys.process._

object Eprover {
  val PATH_TO_EPROVER: String = "./eprover-executable/PROVER/eprover"

  def evaluate_TPTP(tptp: String) : String = {
    print(tptp)

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
}
