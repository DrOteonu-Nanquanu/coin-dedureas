package org.nanquanu.fofsequa.errors

import org.nanquanu.fofsequa.FolseqParser

case class Parse_exception(error: String) extends Exception {
  override def getMessage = error
}
