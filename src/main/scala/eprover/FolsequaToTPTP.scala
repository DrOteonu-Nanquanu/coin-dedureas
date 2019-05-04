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
}