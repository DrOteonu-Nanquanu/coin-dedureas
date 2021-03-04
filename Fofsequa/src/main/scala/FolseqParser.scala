package org.nanquanu.fofsequa

import java.text.ParseException

import scala.util.parsing.combinator._

trait ReplaceableParser[P[_]] {
  type Return
  def parse : P[Return]
}

/*
 Parser
*/
trait FolseqParserBase extends RegexParsers {
  // TODO: Numbers aren't supported yet

  val document: ReplaceableParser[Parser] = new ReplaceableParser[Parser] {
    type Return = List[Statement]
    def parse = ((statement.parse ~ ";") ^^ {case stmt ~ _ => stmt}).*
  }

  // <fofsequa_document> ::= <fofsequa_statement>+
  val fofsequa_document: ReplaceableParser[Parser] = new ReplaceableParser {
    type Return = List[Statement]
    def parse = ((statement.parse ~ ";") ^^ {case stmt ~ _ => stmt}).*
  }

  // TODO: Currently, each statement with a binaryConnective has to be surrounded by brackets, this shouldn't be needed and should change in the future
  //<fofsequa_statement> ::= <unary_connective> <fofsequa_statement> | (<fofsequa_statement> <binary connective> <fofsequa_statement) | <quantified_formula> | <fol_atom>
  val statement : ReplaceableParser[Parser] = new ReplaceableParser[Parser] {
    type Return = Statement
    def parse = unary_connective ~ statement.parse ^^ { case connective ~ stmt => UnaryConnectiveStatement(connective, stmt) } |
    "(" ~ statement.parse ~ binary_connective ~ statement.parse ~ ")" ^^ { case _ ~ statement1 ~ connective ~ statement2 ~ _ => BinaryConnectiveStatement(statement1, connective, statement2) } |
    quantified_formula |
    fol_atom
  }

  // <unary_connective> ::= not
  val unary_connective : ReplaceableParser[Parser] = new ReplaceableParser[Parser] {
    type Return = UnaryConnective
    def parse = "not" ^^ { (x) => Not() }
  }

  // <binary_connective> ::= and | or | => | <=>
  val binary_connective : ReplaceableParser[Parser] = new ReplaceableParser[Parser] {
    type Return = BinaryConnective
    def parse = "and"  ^^ { (x) => And() } |
    "or" ^^ { (x) => Or() } |
    "=>"  ^^ { (x) => IfThen()} |
    "<=>" ^^ { (x) => Iff() }
  }

  // <fol_atom> ::= <fol_predicate>(<fol_term_list>) | <fol_term> = <fol_term>
  val fol_atom : ReplaceableParser[Parser] = new ReplaceableParser[Parser] {
    type Return = AtomStatement
    def parse = fol_predicate ~ "(" ~ fol_term_list ~ ")" ^^ { case predicate ~ _ ~ termList ~ _ => AtomStatement(predicate, termList) }
  }

  // <fol_predicate> ::= <uppercase_id> | <predifined_predicate>
  val fol_predicate : ReplaceableParser[Parser] = new ReplaceableParser[Parser] {
    type Return = FolPredicate
    def parse = uppercase_ID ^^ { FolPredicate(_) }
  }

  // sequence of alphabetical characters, starting with upper case
  val uppercase_ID : ReplaceableParser[Parser] = new ReplaceableParser[Parser] {
    def parse = "[A-Z](([A-Za-z]|_)*[A-Za-z])?".r /*("[A-Z]" + identifier_base_regex).r*/  ^^ { UppercaseID(_) }
  }

  // sequence of characters and underscores that doesn't end in an underscore
  val identifier_base_regex : ReplaceableParser[Parser] = new ReplaceableParser[Parser] {
    def parse = "(([A-z]|_)*[A-z])?"//.r ^^ {s: String => s}  // for some reason `{_: String}` doesn't work
  }

  // <fol_term_list> ::= <fol_term> | <fol_term_list>, <fol_term_list>
  val fol_term_list : ReplaceableParser[Parser] = new ReplaceableParser[Parser] {
    type Return = List[FolTerm]
    def parse = fol_term ~ ", *".r ~ fol_term_list ^^ { case term ~ _ ~ termList => List(term) ++ termList } | fol_term ^^ { List(_) }
  }

  // <fol_term> ::= <digital_entitiy> |  <var> | <constant> | <fol_function>(<fol_term_list>)
  val fol_term: ReplaceableParser[Parser] = new ReplaceableParser[Parser] {
    type Return = FolTerm
    digital_entity ^^ DigitalEntityTerm |
    variable ^^ VariableTerm |
    constant ^^ ConstantTerm |
    fol_function ~ "(" ~ fol_term_list ~ ")" ^^ { case function ~ _ ~ termList ~ _  => FunctionApplication(function, termList) }
  }

  // <digital_entitiy> ::= "([A-z0-9]| )+"
  val digital_entity : ReplaceableParser[Parser] = new ReplaceableParser[Parser] {
    def parse = " *\"".r ~ "[A-z0-9 \\-,.!?]+".r ~ "\" *".r ^^ { case _ ~ text ~ _ => DigitalEntity(text)}
  }

  // <var> ::= <lowercase_id>
  val variable : ReplaceableParser[Parser] = new ReplaceableParser[Parser] {
    def parse = lowercase_ID ^^ { Variable(_) }
  }

  // sequence of alphabetical characters, starting with lower case
  val lowercase_ID : ReplaceableParser[Parser] = new ReplaceableParser[Parser] {
    def parse = "[a-z](([A-Za-z]|_)*[A-Za-z])?".r /*("[a-z]" + identifier_base_regex).r*/ ^^ { LowercaseID(_) }
  }

  // <constant> ::= '<lowercase_id>' | <decimal_integer>
  val constant : ReplaceableParser[Parser] = new ReplaceableParser[Parser] {
    def parse = "\'" ~ lowercase_ID ~ "\'" ^^ { case _ ~ name ~ _ => Constant(name) }
  }

  // <fol_function> ::= <lowercase_id>
  val fol_function : ReplaceableParser[Parser] = new ReplaceableParser[Parser] {
    def parse = lowercase_ID ^^ { FolFunction(_) }
  }

  // <quantified_formula> ::= <quantifier>[<quantifier_arguments>]: <fofsequa_statement>
  val quantified_formula: ReplaceableParser[Parser] = new ReplaceableParser[Parser] {
    type Return = QuantifiedStatement
    def parse = quantifier ~ "[" ~ quantifier_arguments ~ "]:" ~ statement.parse ^^ { case quant ~ _ ~ quantArguments ~ _ ~ stmt => QuantifiedStatement(quant, quantArguments, stmt) }
  }

  // <quantifier> ::= ! | ?
  val quantifier: ReplaceableParser[Parser] = new ReplaceableParser[Parser] {
    type Return = Quantifier
    def parse = "!" ^^ {_ => ForAll()} | "?" ^^ {_ => Exists()}
  }

  // <quantifier_arguments> ::= <fol_variable_list> | <var> from <constant_set>
  val quantifier_arguments: ReplaceableParser[Parser] = new ReplaceableParser[Parser] {
    type Return = QuantifierArguments
    def parse = variable_list ~ "from" ~ constant_set ^^ {case v_list ~ _ ~ c_set => ConstantSetQuantifierArguments(v_list, c_set)} |
    variable_list ^^ { BasicQuantifierArguments(_) }
  }

  // <fol_variable_list> ::= <var> | <var>, <fol_variable_list>
  val variable_list: ReplaceableParser[Parser] = new ReplaceableParser[Parser] {
    type Return = List[Variable]
    def parse = variable ~ ("," ~ variable ^^ {case _comma ~ varname => varname}).* ^^ {case first_var ~ var_list => first_var :: var_list}
  }

  // <constant_set> ::= {<constant_set_elements>} | <pattern_var>
  val constant_set: ReplaceableParser[Parser] = new ReplaceableParser[Parser] {
    type Return = ConstantSet
    def parse = "{" ~ constant_set_elements ~ "}" ^^ {case _ ~ elements ~ _ => BasicConstantSet(elements)} | patternVar
  }

  // <constant_set_elements> ::= <constant> | <constant>, <constant_set_elements>
  val constant_set_elements: ReplaceableParser[Parser] = new ReplaceableParser[Parser] {
    type Return = List[ConstantTuple]
    def parse = constant_tuple ~ ("," ~ constant_tuple ^^ {case _comma ~ tuple => tuple}).* ^^ {case c ~ c_list => c :: c_list}// constant ^^ { List(_) } | constant ~ "," ~ constantSetElements ^^ { case const ~ _ ~ constSetElements => List(const) ++ constSetElements }
  }

  // <constant_tuple> ::= <constant> | "<"<constant>(, <constant>)+">"
  val constant_tuple: ReplaceableParser[Parser] = new ReplaceableParser[Parser] {
    type Return = ConstantTuple
    def parse = constant_like ^^ { const => ConstantTuple(List(const))} |
    ("<" ~ constant_like ~ ("," ~ constant_like ^^ {case _ ~ const => const}).+ ~ ">") ^^ {case _ ~ first_constant ~ constant_list ~ _ => ConstantTuple(first_constant :: constant_list) }
  }

  val constant_like: ReplaceableParser[Parser] = new ReplaceableParser[Parser] {
    type Return = ConstantLike
    def parse = constant.parse | digital_entity.parse
  }

  // <pattern_var> ::= <lowercase_id>_
  val patternVar: ReplaceableParser[Parser] = new ReplaceableParser[Parser] {
    type Return = PatternVar
    def parse = lowercase_ID ~ "_" ^^ { case id ~ _ => PatternVar(id) }
  }
  
  /*
    Data structures
  */

  sealed abstract class Statement
  case class BinaryConnectiveStatement(pre_statement: statement.Return, connective: binary_connective.Return, post_statement: statement.Return) extends Statement {
    override def toString: String = pre_statement.toString + connective.toString + post_statement.toString
  }
  case class UnaryConnectiveStatement(connective: unary_connective.Return, post_statement: statement.Return) extends Statement {
    override def toString: String = connective.toString + post_statement.toString
  }
  case class QuantifiedStatement(inner_quantifier: quantifier.Return, arguments: quantifier_arguments.Return, inner_statement: statement.Return) extends Statement{
    override def toString: String = quantifier.toString + "[" + arguments.toString + "]:" + statement.toString
  }

  case class AtomStatement(predicate: fol_predicate.Return, terms: Seq[fol_term.Return]) extends Statement{
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
  case class ConstantSetQuantifierArguments(variables: Seq[variable.Return], inner_constant_set: constant_set.Return) extends QuantifierArguments {
    override def toString: String = (
        if(variables.length == 1) variables.head.toString
        else variables.mkString(",")
      ) + " from " + constant_set.toString
  }
  case class BasicQuantifierArguments(variables: Seq[variable.Return]) extends QuantifierArguments {
    override def toString: String = variables.map(_.toString).mkString(",")
  }

  sealed abstract class ConstantSet
  case class BasicConstantSet(constants: Seq[constant_tuple.Return]) extends ConstantSet {
    override def toString: String = constants.map(_.toString).mkString("{", ",", "}")
  }
  case class PatternVar(name: lowercase_id.Return) extends ConstantSet {
    override def toString: String = name.toString + "_"
  }

  case class ConstantTuple(constants: Seq[constant_like.Return]) {
    override def toString: String = if(constants.length == 1) {
      constants.head.toString
    }
    else {
      constants.map(_.toString).mkString("<", ",", ">")
    }
  }

  case class Constant(id: lowercase_id.Return) extends ConstantLike {
    override def toString: String = "'" + id.toString + "'"
  }
  case class Variable(id: lowercase_id.Return) {
    override def toString: String = id.toString
  }
  case class LowercaseID(name: String) {
    override def toString: String = name
  }
  case class UppercaseID(name: String) {
    override def toString: String = name
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

  sealed trait ConstantLike

  case class FolPredicate(name: uppercase_id.Return) {
    override def toString: String = name.toString
  }

  sealed abstract class FolTerm()

  case class DigitalEntityTerm(digitalEntity: digital_entity.Return) extends FolTerm {
    override def toString: String = '"' + digitalEntity.toString + '"'
  }
  case class DigitalEntity(text: String) extends ConstantLike {
    override def toString: String = '"' + text + '"'
  }

  case class FunctionApplication(function: fol_function.Return, terms: Seq[fol_term.Return]) extends FolTerm {
    override def toString: String = function.toString + terms.mkString("(", ",", ")")
  }
  case class ConstantTerm(inner_constant: constant.Return) extends FolTerm {
    override def toString: String = constant.toString
  }
  case class VariableTerm(inner_variable: variable.Return) extends FolTerm {
    override def toString: String = variable.toString
  }

  case class FolFunction(name: lowercase_id.Return) {
    override def toString: String = name.toString
  }
}

object FolseqParser extends FolseqParserBase {
  def parse_or_throw[R](string_to_parse: String, parser: Parser[R]): R = {
    this.parseAll(parser, string_to_parse) match {
      case Success(result, next) => result
      case error: NoSuccess => throw errors.Parse_exception(error)
    }
  }
  val parse_statement_or_throw = parse_or_throw(_: String, statement)
  val parse_document_or_throw = parse_or_throw(_: String, fofsequa_document)

}


