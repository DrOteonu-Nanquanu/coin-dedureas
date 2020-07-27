package org.nanquanu.fofsequa_reasoner.eprover

import java.io.{File, PrintWriter}

import scala.io.{Source, StdIn}
import sys.process._

object Eprover {
  var PATH_TO_EPROVER: Option[String] = Some("eprover")
    //(Eprover.getClass.getProtectionDomain.getCodeSource.getLocation + "../../../eprover-executable/PROVER/eprover").substring(5)
    //"./eprover-executable/PROVER/eprover"

  def path_to_eprover() = PATH_TO_EPROVER match {
    case Some(path) => path
    case None => {
      val path = StdIn.readLine();
      PATH_TO_EPROVER = Some(path)
      path
    }
  }

  // Evaluate Eprover on the given tptp string
  def evaluate_TPTP(tptp: String) : String = {
    val file_name = "/tmp/evaluate.tptp"

    val file = new File(file_name)

    try {
      val writer = new PrintWriter(file)
      writer.write(tptp)
      writer.close

      val result = execute(file_name)
      result
    }
    finally {
      file.delete
    }
  }

  val eprover_arguments = " --auto -s --answers "

  // Execute Eprover on a file
  def execute(file_name: String) : String = ((path_to_eprover() + eprover_arguments + file_name) lineStream_!).mkString("\n")

  // Extract the answer tuples from Eprover's answer
  def get_answer_tuples(eprover_answer: String, expected_elements_per_tuple: Int): List[List[String]] = {
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

        List(variable_names.take(expected_elements_per_tuple).toList)
      }
      else {
        List()
      }
    })
  }
}
