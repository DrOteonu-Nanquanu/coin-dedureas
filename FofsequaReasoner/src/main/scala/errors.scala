package org.nanquanu.fofsequa_reasoner.errors

import org.nanquanu.fofsequa.errors
import org.nanquanu.fofsequa

sealed abstract class Reasoning_exception() extends Exception

sealed class Reasoning_parse_exception(val error: fofsequa.FolseqParser.NoSuccess) extends Reasoning_exception {
  override def getMessage() = error.msg
}
case class Kb_parse_exception(override val error: fofsequa.FolseqParser.NoSuccess) extends Reasoning_parse_exception(error)
case class Query_parse_exception(override val error: fofsequa.FolseqParser.NoSuccess) extends Reasoning_parse_exception(error)
case class Format_exception(message: String) extends Reasoning_exception {
  override def getMessage = message
}

case class Cli_exception(message: String) extends Exception {
  override def getMessage: String = message
}