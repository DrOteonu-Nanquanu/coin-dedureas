package org.nanquanu.fofsequa_reasoner.eprover

import java.io.{File, FileWriter, PrintWriter}
import java.nio.file.{Files, Paths}

import scala.io.{Source, StdIn}
import sys.process._
import scala.language.postfixOps

object Eprover {
  var path_to_eprover: Option[String] = None
    //Some("eprover")
    //(Eprover.getClass.getProtectionDomain.getCodeSource.getLocation + "../../../eprover-executable/PROVER/eprover").substring(5)
    //"./eprover-executable/PROVER/eprover"
  val config_file_path = "./fofsequa.conf"

  // Tries to locate the eprover executable and set `path_to_eprover`. Returns whether this was successful.
  def locate_eprover_executable(): Boolean = {

    val config_file_exists =
      if(Files.exists(Paths.get(config_file_path))) true
      else {
        println("Could not locate configuration file 'fofsequa.conf'. Should it be created ? Enter y, yes, n, or no")

        StdIn.readLine() match {
          case "y" | "yes" => create_config_file()
          case _ => false
        }
      }

    if(config_file_exists) {
      val config_file = Source.fromFile(config_file_path)

      val config_file_lines = try {
        config_file.getLines().toList
      }
      catch {
        case exception: Throwable => {
          println("Could not read config file")
          throw exception
        }
      }
      finally config_file.close()

      // Test whether the config file contains a line with a path to eprover
      val found_eprover_path_in_config = config_file_lines.exists(line => {
        if(line.contains('=')) {
          val (variable_name, tail): (String, String) = line.splitAt(line.indexOf('='))
          val variable_value = tail.substring(1)

          variable_name match {
            case "eprover_path" => {
              if(Files.exists(Paths.get(variable_value))) {
                path_to_eprover = Some(variable_value)
              } else {
                println("Found eprover_path variable but couldn't find the executable at the specified path.")
              }

              true
            }
            case _ => false
          }
        }
        else {
          false
        }
      })

      if(!found_eprover_path_in_config) {
        println("Variable eprover_path does not exist in config file. Add the following line to " + config_file_path + ": eprover_path=<path_to_executable>")
      }
      // Return
      found_eprover_path_in_config
    }
    else false  //config file does not exist
  }

  def create_config_file(path_to_eprover: String = "./eprover-executable/PROVER/eprover") = {
    try {
      val config_file = new File(config_file_path)
      val writer = new FileWriter(config_file)
      writer.append("eprover_path=" + path_to_eprover)
      writer.close()
      true
    }
    catch {
      case exception: Throwable => {
        println("Could not create config file")
        throw exception
      }
    }
  }

  def get_path_to_eprover(): String = path_to_eprover match {
    case Some(path) => path
    case None => {
      locate_eprover_executable()
      path_to_eprover match {
        case Some(path) => path
        case None => throw new Exception("eprover executable not found")
      }
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
  def execute(file_name: String) : String = ((get_path_to_eprover() + eprover_arguments + file_name) lineStream_!).mkString("\n")

  // Extract the answer tuples from Eprover's answer
  def get_answer_tuples(eprover_answer: String, expected_elements_per_tuple: Int): List[List[QuotedString]] = {
    val answer_start = "# SZS answers Tuple [["

    eprover_answer.split("\n").toList.flatMap(line => {
      val (start, end) = line.splitAt(answer_start.length)

      if(start == answer_start) {
        val (quoted_variables, tail) = end.splitAt(end.indexOf(']'))
        // println("quoted variables = " + quoted_variables)
        val variable_names = quoted_variables
          //.filter(_ != ' ')
          .split(", ").map(
          quoted_variable => {
            val unquoted = quoted_variable.slice(1, quoted_variable.length - 1)
            val quotation_mark = quoted_variables[0]
            quotation_mark match {
              case '"' => DoubleQuotedString(unquoted)
              case ''' => SingleQuotedString(unquoted)
            }
          }
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

abstract class QuotedString {
  abstract def text: String
}
case class SingleQuotedString(quoted_text: String) extends QuotedString {
  override def text = quoted_text
}
case class DoubleQuotedString(quoted_text: String) extends QuotedString {
  override def text = quoted_text
}
