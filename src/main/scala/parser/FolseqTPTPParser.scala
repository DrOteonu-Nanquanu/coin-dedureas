package parser

import scala.util.parsing.combinator._

object FolseqTPTPParser extends RegexParsers {
  def word = "word".r

  def lowercaseID = "[a-z][a-zA-Z]*".r ^^ {TPTPElement.fromAny}
  def uppercaseID = "[A-Z][a-zA-Z]*".r ^^ {TPTPElement.fromAny}
  def folPredicate = uppercaseID
  def variable = lowercaseID
  def constant = "\"" ~ lowercaseID ~ "\"" ^^ {case _ ~ name ~ _ => new TPTPElement("\"" + name + "\"", false) }

  def folFunction = lowercaseID
  def folTermList : Parser[TPTPElement] = folTerm ~ ", *".r ~ folTermList ^^ {case term ~ _ ~ termList => new TPTPElement(term + ", " + termList, false)} | folTerm
  def folTerm = folFunction ~ "(" ~ folTermList ~ ")" ^^ {case function ~ _ ~ termList ~ _  => new TPTPElement(function + "(" + termList + ")", false)} | constant | variable

  def folAtom : Parser[TPTPElement] = folPredicate ~ "(" ~ folTermList ~ ")" ^^ {case predicate ~ _ ~ termList ~ _ => new TPTPElement(predicate + "(" + termList + ")", false)}
  def unaryConnective = "not" ^^ {_.toString}
  def binaryConnective = "and"  ^^ {_.toString} | "or" ^^ {_.toString} | "=>"  ^^ {_.toString} | "<=>" ^^ {_.toString}

  def statement : Parser[TPTPElement] = unaryConnective ~ statement ^^ {case connective ~ stmt => new TPTPElement(connective.toString + stmt.toString, false)} |
    "(" ~ statement ~ binaryConnective ~ statement ~ ")" ^^ {case _ ~ statement1 ~ connective ~ statement2 ~ _ => new TPTPElement(statement1.toString + connective.toString + statement2.toString, false)} |
    folAtom
}

class TPTPElement(content: String, isConjecture: Boolean = false) {
  val Content = content
  val IsConjecture = isConjecture
}

object TPTPElement {
  def combine(elements: Array[Any]): TPTPElement = {
    elements.foldLeft(new TPTPElement("", false))((result: TPTPElement, item: Any) => if(item.isInstanceOf[TPTPElement]) {
      val itemCast = item.asInstanceOf[TPTPElement]
      new TPTPElement(result.Content + itemCast.Content, result.IsConjecture || itemCast.IsConjecture)
    }
    else {
      new TPTPElement(result.Content + item.toString, result.IsConjecture)
    })
  }

  def fromAny(element: Any): TPTPElement = new TPTPElement(element.toString, false)
}