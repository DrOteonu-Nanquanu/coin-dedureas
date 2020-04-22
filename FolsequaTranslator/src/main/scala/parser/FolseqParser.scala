package parser

import scala.util.parsing.combinator._

object FolseqParser extends RegexParsers {
  // TODO: Numbers aren't supported yet

  // <fofsequa_document> ::= <fofsequa_statement>+
  def fofsequa_document: Parser[List[Statement]] = statement ~ ((";" ~ statement) ^^ {case _ ~ stmt => stmt}).* ^^ {
    case first_statement ~ statement_list => first_statement :: statement_list
  }

  // TODO: Currently, each statement with a binaryConnective has to be surrounded by brackets, this shouldn't be needed and should change in the future
  //<fofsequa_statement> ::= <unary_connective> <fofsequa_statement> | (<fofsequa_statement> <binary connective> <fofsequa_statement) | <quantified_formula> | <fol_atom>
  def statement : Parser[Statement] =
    unary_connective ~ statement ^^ { case connective ~ stmt => UnaryConnectiveStatement(connective, stmt) } |
    "(" ~ statement ~ binary_connective ~ statement ~ ")" ^^ { case _ ~ statement1 ~ connective ~ statement2 ~ _ => BinaryConnectiveStatement(statement1, connective, statement2) } |
    quantified_formula |
    fol_atom

  // <unary_connective> ::= not
  def unary_connective = "not" ^^ { (x) => Not() }

  // <binary_connective> ::= and | or | => | <=>
  def binary_connective : Parser[BinaryConnective] = "and"  ^^ { (x) => And() } |
    "or" ^^ { (x) => Or() } |
    "=>"  ^^ { (x) => IfThen()} |
    "<=>" ^^ { (x) => Iff() }

  // <fol_atom> ::= <fol_predicate>(<fol_term_list>) | <fol_term> = <fol_term>
  def fol_atom : Parser[AtomStatement] = fol_predicate ~ "(" ~ fol_term_list ~ ")" ^^ { case predicate ~ _ ~ termList ~ _ => AtomStatement(predicate, termList) }

  // <fol_predicate> ::= <uppercase_id> | <predifined_predicate>
  def fol_predicate = uppercase_ID ^^ { FolPredicate(_) }

  // sequence of alphabetical characters, starting with upper case
  def uppercase_ID = "[A-Z][a-zA-Z]*".r ^^ { UppercaseID(_) }

  // <fol_term_list> ::= <fol_term> | <fol_term_list>, <fol_term_list>
  def fol_term_list : Parser[Array[FolTerm]] = fol_term ~ ", *".r ~ fol_term_list ^^ { case term ~ _ ~ termList => Array(term) ++ termList } | fol_term ^^ { Array(_) }

  // <fol_term> ::= <var> | <constant> | <fol_function>(<fol_term_list>)
  def fol_term =
    variable ^^ { VariableTerm(_) } |
    constant ^^ { ConstantTerm(_) } |
    fol_function ~ "(" ~ fol_term_list ~ ")" ^^ { case function ~ _ ~ termList ~ _  => FunctionApplication(function, termList) }

  // <var> ::= <lowercase_id>
  def variable = lowercase_ID ^^ { Variable(_) }

  // sequence of alphabetical characters, starting with lower case
  def lowercase_ID = "[a-z][a-zA-Z]*".r ^^ { LowercaseID(_) }

  // <constant> ::= '<lowercase_id>' | <decimal_integer>
  def constant = "\'" ~ lowercase_ID ~ "\'" ^^ { case _ ~ name ~ _ => Constant(name) }

  // <fol_function> ::= <lowercase_id>
  def fol_function = lowercase_ID ^^ { FolFunction(_) }

  // <quantified_formula> ::= <quantifier>[<quantifier_arguments>]: <fofsequa_statement>
  def quantified_formula = quantifier ~ "[" ~ quantifier_arguments ~ "]:" ~ statement ^^ { case quant ~ _ ~ quantArguments ~ _ ~ stmt => QuantifiedStatement(quant, quantArguments, stmt) }

  // <quantifier> ::= ! | ?
  def quantifier = "!" ^^ {(x) => ForAll()} | "?" ^^ {x => Exists()}

  // <quantifier_arguments> ::= <fol_variable_list> | <var> from <constant_set>
  def quantifier_arguments = variable ~ "from" ~ constantSet ^^ {case v ~ _ ~ constant_set => ConstantSetQuantifierArguments(v, constant_set)} |
    variable_list ^^ { BasicQuantifierArguments(_) }

  // <fol_variable_list> ::= <var> | <fol_variable_list>, <fol_variable_list>
  def variable_list: Parser[List[Variable]] = variable ~ ("," ~ variable ^^ {case _comma ~ varname => varname}).* ^^ {case first_var ~ var_list => first_var :: var_list}

  // <constant_set> ::= {<constant_set_elements>} | <pattern_var>
  def constantSet = "{" ~ constantSetElements ~ "}" ^^ {case _ ~ elements ~ _ => BasicConstantSet(elements)} | patternVar

  // <constant_set_elements> ::= <constant> | <constant>, <constant_set_elements>
  def constantSetElements: Parser[Array[Constant]] = constant ^^ { Array(_) } | constant ~ "," ~ constantSetElements ^^ { case const ~ _ ~ constSetElements => Array(const) ++ constSetElements }

  // <pattern_var> ::= <lowercase_id>_
  def patternVar =  lowercase_ID ~ "_" ^^ { case id ~ _ => PatternVar(id) }
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

sealed abstract class Statement
case class BinaryConnectiveStatement(pre_statement: Statement, connective: BinaryConnective, post_statement: Statement) extends Statement {
  override def toString: String = pre_statement.toString + connective.toString + post_statement.toString
}
case class UnaryConnectiveStatement(connective: UnaryConnective, post_statement: Statement) extends Statement {
  override def toString: String = connective.toString + post_statement.toString
}
case class QuantifiedStatement(quantifier: Quantifier, arguments: QuantifierArguments, statement: Statement) extends Statement{
  override def toString: String = quantifier.toString + "[" + arguments.toString + "]:" + statement.toString
}

case class AtomStatement(predicate: FolPredicate, terms: Array[FolTerm]) extends Statement{
  override def toString: String = predicate.toString + terms.map(_.toString).mkString("(", ",", ")")
}

sealed abstract class Quantifier {
  def inverse(): Quantifier
}
case class ForAll() extends Quantifier {
  override def toString: String = "!"

  override def inverse(): Quantifier = Exists()
}
case class Exists() extends Quantifier {
  override def toString: String = "?"

  override def inverse(): Quantifier = ForAll()
}

sealed abstract class QuantifierArguments
case class ConstantSetQuantifierArguments(variable: Variable, constant_set: ConstantSet) extends QuantifierArguments {
  override def toString: String = variable.toString + " from " + constant_set.toString
}
case class BasicQuantifierArguments(variables: List[Variable]) extends QuantifierArguments {
  override def toString: String = variables.map(_.toString).mkString(",")
}

sealed abstract class ConstantSet
case class BasicConstantSet(constants: Array[Constant]) extends ConstantSet {
  override def toString: String = constants.map(_.toString).mkString("{", ",", "}")
}
case class PatternVar(name: LowercaseID) extends ConstantSet {
  override def toString: String = name.toString
}

case class Constant(id: LowercaseID)  {
  override def toString: String = "'" + id.toString + "'"
}
case class Variable(id: LowercaseID) {
  override def toString: String = id.toString
}
case class LowercaseID(name: String) {
  override def toString: String = name.toString
}
case class UppercaseID(name: String) {
  override def toString: String = name.toString
}

sealed abstract class UnaryConnective
case class Not() extends UnaryConnective {
  override def toString: String = "not"
}

sealed abstract class BinaryConnective
case class And() extends BinaryConnective {
  override def toString: String = "&"
}
case class Or() extends BinaryConnective {
  override def toString: String = "|"
}
case class IfThen() extends BinaryConnective {
  override def toString: String = "=>"
}
case class Iff() extends BinaryConnective {
  override def toString: String = "<=>"
}

case class FolPredicate(name: UppercaseID) {
  override def toString: String = name.toString
}

sealed abstract class FolTerm()
case class FunctionApplication(function: FolFunction, terms: Array[FolTerm]) extends FolTerm {
  override def toString: String = function.toString + "(" + terms.toString + ")"
}
case class ConstantTerm(constant: Constant) extends FolTerm {
  override def toString: String = constant.toString
}
case class VariableTerm(variable: Variable) extends FolTerm {
  override def toString: String = variable.toString
}

case class FolFunction(name: LowercaseID) {
  override def toString: String = name.toString
}