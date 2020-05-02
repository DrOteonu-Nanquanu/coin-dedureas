package parser

import scala.util.parsing.combinator._

object FolseqParser extends RegexParsers {
  def lowercaseID = "[a-z][a-zA-Z]*".r ^^ { new LowercaseID(_) }
  def uppercaseID = "[A-Z][a-zA-Z]*".r ^^ { new UppercaseID(_) }
  def folPredicate = uppercaseID ^^ { new FolPredicate(_) }
  def variable = lowercaseID ^^ { new Variable(_) }
  def constant = "\'" ~ lowercaseID ~ "\'" ^^ { case _ ~ name ~ _ => new Constant(name) }

  def folFunction = lowercaseID ^^ { new FolFunction(_) }
  def folTermList : Parser[Array[FolTerm]] = folTerm ~ ", *".r ~ folTermList ^^ { case term ~ _ ~ termList => Array(term) ++ termList } | folTerm ^^ { Array(_) }
  def folTerm = folFunction ~ "(" ~ folTermList ~ ")" ^^ { case function ~ _ ~ termList ~ _  => new FunctionApplication(function, termList) } | constant ^^ { new ConstantTerm(_) } | variable ^^ { new VariableTerm(_) }

  def folAtom : Parser[AtomStatement] = folPredicate ~ "(" ~ folTermList ~ ")" ^^ { case predicate ~ _ ~ termList ~ _ => new AtomStatement(predicate, termList) }
  def unaryConnective = "not" ^^ { (x) => new Not() }
  def binaryConnective : Parser[BinaryConnective] = "and"  ^^ { (x) => new And() } | "or" ^^ { (x) => new Or() } | "=>"  ^^ { (x) => new IfThen()} | "<=>" ^^ { (x) => new Iff() }

  def statement : Parser[Statement] = unaryConnective ~ statement ^^ { case connective ~ stmt => new UnaryConnectiveStatement(connective, stmt) } |
    "(" ~ statement ~ binaryConnective ~ statement ~ ")" ^^ { case _ ~ statement1 ~ connective ~ statement2 ~ _ => new BinaryConnectiveStatement(statement1, connective, statement2) } |
    quantifiedFormula | folAtom
  //currently, each statement with a binaryConnective has to be surrounded by brackets, this shouldn't be needed and should change in the future
  def quantifiedFormula = quantifier ~ "[" ~ quantifierArguments ~ "]:" ~ statement ^^ { case quant ~ _ ~ quantArguments ~ _ ~ stmt => new QuantifiedStatement(quant, quantArguments, stmt) }
  def quantifier = "!" ^^ {(x) => new ForAll()} | "?" ^^ {(x) => new Exists()}
  def quantifierArguments = variable ~ "from" ~ constantSet ^^ {case v ~ _ ~ constant_set => new ConstantSetQuantifierArguments(v, constant_set)} | 
    variableList ^^ { new BasicQuantifierArguments(_) }
  /*def variableList: Parser[Array[Variable]] = variable ^^ { Array(_) } |
    variable ~ "," ~ variableList ^^ { case v ~ _ ~ v_list => Array(v) ++ v_list }*/
  def variableList: Parser[List[Variable]] = variable ~ ("," ~ variable ^^ {case _comma ~ varname => varname}).* ^^ {case first_var ~ var_list => first_var :: var_list}
  def constantSet = "{" ~ constantSetElements ~ "}" ^^ {case _ ~ elements ~ _ => new BasicConstantSet(elements)} | patternVar
  def patternVar =  lowercaseID ~ "_" ^^ { case id ~ _ => new PatternVar(id) }
  def constantSetElements: Parser[Array[Constant]] = constant ^^ { Array(_) } | constant ~ "," ~ constantSetElements ^^ { case const ~ _ ~ constSetElements => Array(const) ++ constSetElements }

//  def fofsequa_document: Parser[Array[Statement]] = statement ^^ { Array(_) } | statement ~ "\n" ~ fofsequa_document ^^ {case stmt ~ _ ~ doc => Array(stmt) ++ doc }
  def fofsequa_document: Parser[List[Statement]] = statement ~ ((";" ~ statement) ^^ {case _ ~ stmt => stmt}).* ^^ {
    case first_statement ~ statement_list => first_statement :: statement_list
  }
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