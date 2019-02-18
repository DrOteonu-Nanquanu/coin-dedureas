package parser

import scala.util.parsing.combinator._

object FolseqTPTPParser extends RegexParsers {
  def lowercaseID = "[a-z][a-zA-Z]*".r ^^ {TPTPElement.fromAny}
  def uppercaseID = "[A-Z][a-zA-Z]*".r ^^ {TPTPElement.fromAny}
  def folPredicate = uppercaseID
  def variable = lowercaseID
  def constant = "\"" ~ lowercaseID ~ "\"" ^^ {case _ ~ name ~ _ => TPTPElement.combine(Array("\"", name, "\"")) }

  def folFunction = lowercaseID
  def folTermList : Parser[TPTPElement] = folTerm ~ ", *".r ~ folTermList ^^ {case term ~ _ ~ termList => TPTPElement.combine(Array(term, ", " , termList))} | folTerm
  def folTerm = folFunction ~ "(" ~ folTermList ~ ")" ^^ {case function ~ _ ~ termList ~ _  => TPTPElement.combine(Array(function, "(", termList, ")"))} | constant | variable

  def folAtom : Parser[TPTPElement] = folPredicate ~ "(" ~ folTermList ~ ")" ^^ {case predicate ~ _ ~ termList ~ _ => TPTPElement.combine(Array(predicate, "(", termList, ")"))}
  def unaryConnective = "not" ^^ {TPTPElement.fromAny}
  def binaryConnective = "and"  ^^ {TPTPElement.fromAny} | "or" ^^ {TPTPElement.fromAny} | "=>"  ^^ {TPTPElement.fromAny} | "<=>" ^^ {TPTPElement.fromAny}

  def statement : Parser[TPTPElement] = unaryConnective ~ statement ^^ {case connective ~ stmt => TPTPElement.combine(Array(connective, stmt))} |
    "(" ~ statement ~ binaryConnective ~ statement ~ ")" ^^ {case _ ~ statement1 ~ connective ~ statement2 ~ _ => TPTPElement.combine(Array(statement1, connective, statement2))} |
    quantifiedFormula | folAtom
  //currently, each statement with a binaryConnective has to be surrounded by brackets, this shouldn't be needed and should change in the future
  def quantifiedFormula = quantifier ~ "[" ~ quantifierArguments ~ "]:" ~ statement ^^ {case quant ~ _ ~ quantArguments ~ _ ~ stmt => TPTPElement.combine(Array(quant, "[", quantArguments, "]:", stmt))}
  def quantifier = "!" ^^ {TPTPElement.fromAny} | "?" ^^ {TPTPElement.fromAny}
  def quantifierArguments = variable ~ " from " ~ constantSet ^^ {case v ~ _ ~ constant_set => TPTPElement.combine(Array(v, constant_set))} | variableList ^^ {TPTPElement.fromAny}
  def variableList: Parser[TPTPElement] = variable | variable ~ variableList ^^ {case v ~ v_list => TPTPElement.combine(Array(v, v_list))}
  def constantSet = "{" ~ constantSetElements ~ "}" ^^ {case _ ~ elements ~ _ => TPTPElement.combine(Array("{", elements, "}"))} | patternVar
  def patternVar =  lowercaseID ~ "_" ^^ {case id ~ _ => TPTPElement.combine(Array(id, "_"))}
  def constantSetElements: Parser[TPTPElement] = constant | constant ~ constantSetElements ^^ {case const ~ constSetElements => TPTPElement.combine(Array(const, constSetElements))}

  def fofsequa_document: Parser[String] = statement ^^ {case stmt => stmt.toFOF} | statement ~ fofsequa_document ^^ {case stmt ~ doc => stmt.toFOF + doc}
}

case class TPTPElement(content: String, isConjecture: Boolean = false) {
  def toFOF =
    if(isConjecture) {
      "fof(conj, conjecture, " + content + ")"
    } else {
      "fof(stmt, axiom, " + content + ")"
    }
}

object TPTPElement {
  def combine(elements: Array[Any]): TPTPElement = {
    elements.foldLeft(new TPTPElement("", false))((result: TPTPElement, item: Any) =>
      if(item.isInstanceOf[TPTPElement]) {
        val itemCast = item.asInstanceOf[TPTPElement]
        new TPTPElement(result.content + itemCast.content, result.isConjecture || itemCast.isConjecture)
      }
      else {
        new TPTPElement(result.content + item.toString, result.isConjecture)
      }
    )
  }

  def fromAny(element: Any): TPTPElement = new TPTPElement(element.toString, false)
}

case class FofsequaDocument(statements: Array[Statement])

sealed abstract class Statement
case class BinaryConnectiveStatement(pre_statement: Statement, connective: BinaryConnective, post_statement: Statement) extends Statement
case class UnaryConnectiveStatement(connective: UnaryConnective, post_statement: Statement) extends Statement
case class QuantifiedStatement(quantifier: Quantifier, arguments: QuantifierArguments, statement: Statement) extends Statement

sealed abstract class Quantifier
case class ForAll() extends Quantifier
case class Exists() extends Quantifier

sealed abstract class QuantifierArguments
case class ConstantSetQuantiferArguments(variable: Variable, constant_set: ConstantSet) extends QuantifierArguments
case class BasicQuantiferArguments(variables: Array[Variable]) extends QuantifierArguments

sealed abstract class ConstantSet
case class BasicConstantSet(constants: Array[Constant]) extends ConstantSet
case class PatternVar(name: LowercaseID) extends ConstantSet

case class Constant(id: LowercaseID)
case class Variable(id: LowercaseID)
case class LowercaseID(name: String)
case class UppercaseID(name: String)

sealed abstract class UnaryConnective
case class Not() extends UnaryConnective

sealed abstract class BinaryConnective
case class And()
case class Or()
case class IfThen()
case class Iff()