package org.nanquanu.fofsequa.errors

import org.nanquanu.fofsequa.FofseqParser

case class Parse_exception(error: FofseqParser.NoSuccess) extends Exception {
  override def getMessage = error.msg
}
