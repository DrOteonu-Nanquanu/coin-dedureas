package eprover

import parser._

object FolsequaToTPTP {
    def from_parsed_folsequa (document: Array[Statement]) : String = {
        document.foldLeft("")((accumulator, statement) => accumulator + parse(statement))
    }

    def parse(statement: Statement) : String = statement match {
        case BinaryConnectiveStatement(pre_statement, connective, post_statement) => "(" + parse(pre_statement) + parse(connective) + parse(post_statement) + ")"
        case UnaryConnectiveStatement(connective, post_statement) => parse(connective) + parse(post_statement)
        case QuantifiedStatement(quantifier, arguments, statement) => parse(quantifier) + "[" + parse(arguments) + "]" + parse(statement)
        case AtomStatement(predicate, terms) => parse(predicate) + "(" + parse(terms) + ")"
    }

    def parse(connective: BinaryConnective) = connective match {
        case And() => "&"
        case Or() => "|"
        case IfThen() => "=>"
        case Iff() => "<=>"
    }

    def parse(connective: UnaryConnective) = connective match { case Not() => "~" }

    def parse(quantifier: Quantifier) = quantifier match {
        case ForAll() => "!"
        case Exists() => "?"
    }

    def parse(arguments: QuantifierArguments) = arguments match {
        case BasicQuantifierArguments(variables) => variables.mkString(" ")
        case ConstantSetQuantifierArguments(variable, constant_set) => {}
    }

    def parse(predicate: FolPredicate): String = predicate.name.name

    def parse(terms: Array[FolTerm]) = terms.map(term => term match {
        case ConstantTerm(constant) => constant.id.name
    }).mkString(", ")
}