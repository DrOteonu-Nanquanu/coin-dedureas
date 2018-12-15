import parser.FolseqTPTPParser

import scala.util.parsing.combinator._

object Main {
  def main(args: Array[String]): Unit = {
    println("Hello World")

    val parseResult = FolseqTPTPParser.parse(FolseqTPTPParser.statement, "Predicate(function(\"argOne\", argTwo), argThree)")
    println(parseResult.get.Content)
  }
}
