package org.nanquanu.fofsequa_reasoner.eprover

import java.io.{BufferedWriter, File, FileNotFoundException, FileWriter, PrintWriter}
import java.nio.file.{Files, Paths}

import scala.io.{Source, StdIn}
import scala.jdk.CollectionConverters
import sys.process._
import scala.language.postfixOps

object Eprover {
  var path_to_eprover: Option[String] = None
    //Some("eprover")
    //(Eprover.getClass.getProtectionDomain.getCodeSource.getLocation + "../../../eprover-executable/PROVER/eprover").substring(5)
    //"./eprover-executable/PROVER/eprover"

  // Tries to locate the eprover executable and set `path_to_eprover`. Returns whether this was successful.
  def locate_eprover_executable(): Boolean = {
    val config_file_path = "./fofsequa.conf"

    val config_file_exists =
      if(Files.exists(Paths.get(config_file_path))) true
      else {
        println("Could not locate configuration file 'fofsequa.conf'. Should it be created ? Enter y, yes, n, or no")

        StdIn.readLine() match {
          case "y" | "yes" => {
            try {
              val config_file = new File(config_file_path)
              val writer = new FileWriter(config_file)
              writer.append("eprover_path=./eprover-executable/PROVER/eprover")
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

      // Return whether the config file contains a line with a path to eprover
      config_file_lines.exists(line => {
        if(line.contains('=')) {
          val (variable_name, tail): (String, String) = line.splitAt(line.indexOf('='))
          val variable_value = tail.substring(1)

          variable_name match {
            case "eprover_path" => {
              if(Files.exists(Paths.get(variable_value))) {
                path_to_eprover = Some(variable_value)
                true
              } else {
                false
              }
            }
            case _ => false
          }
        }
        else {
          false
        }
      })
    }
    else false  //config file does not exist
  }

  def get_path_to_eprover(): String = path_to_eprover match {
    case Some(path) => path
    case None => {
      if(locate_eprover_executable()) {
        path_to_eprover match {
          case Some(path) => path
          case None => throw new Exception("Unreachable code")
        }
      }
      else {
        throw new FileNotFoundException("eprover executable not found")
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
