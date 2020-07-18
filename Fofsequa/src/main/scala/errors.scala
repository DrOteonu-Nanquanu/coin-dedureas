package org.nanquanu.fofsequa.errors

import org.nanquanu.fofsequa.FolseqParser

case class Parse_exception(error: FolseqParser.NoSuccess) extends Exception {
  override def getMessage = error.msg
}
