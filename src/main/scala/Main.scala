import parser.FolseqTPTPParser
import sys.process._

object Main {
  def main(args: Array[String]): Unit = {
    println("Hello World")

    println(FolseqTPTPParser.parse(FolseqTPTPParser.quantifiedFormula, """![a from p_]: P(a, "b")"""))

    //val result = Eprover.execute("/home/jcmaas/Documents/coin-dedureas/example_db.tptp")

    //println(result)
  }
}
