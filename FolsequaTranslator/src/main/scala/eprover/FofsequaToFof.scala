package eprover

import parser._

object FofsequaToFof {
    def from_parsed_folsequa (document: Array[Statement]) : String = {
        document.foldLeft("")((accumulator, statement) => accumulator + stringify(statement))
    }

    def stringify(statement: Statement) : String = statement match {
        case BinaryConnectiveStatement(pre_statement, connective, post_statement) => "(" + stringify(pre_statement) + stringify(connective) + stringify(post_statement) + ")"
        case UnaryConnectiveStatement(connective, post_statement) => stringify(connective) + stringify(post_statement)
        case QuantifiedStatement(quantifier, arguments, statement) => stringify(quantifier) + "[" + stringify(arguments) + "]: " + stringify(statement)
        case AtomStatement(predicate, terms) => stringify(predicate) + "(" + stringify(terms) + ")"
    }

    def stringify(connective: BinaryConnective) = connective match {
        case And() => "&"
        case Or() => "|"
        case IfThen() => "=>"
        case Iff() => "<=>"
    }

    def stringify(connective: UnaryConnective) = connective match { case Not() => "~" }

    def stringify(quantifier: Quantifier) = quantifier match {
        case ForAll() => "!"
        case Exists() => "?"
    }

    def stringify(arguments: QuantifierArguments): String = arguments match {
        case BasicQuantifierArguments(variables) => variables.map((variable: Variable) => stringify(variable)).mkString(" ")
        case ConstantSetQuantifierArguments(variable, constant_set) => {throw new Error("not implemented")}
    }

    def stringify(variable: Variable): String = stringify(variable.id).capitalize

    def stringify(id: LowercaseID): String = id.name

    def stringify(predicate: FolPredicate): String = predicate.name.name

    def stringify(terms: Array[FolTerm]) : String = terms.map(term => term match {
        case ConstantTerm(constant) => "'" + constant.id.name + "'"
        case FunctionApplication(function, terms) => stringify(function) + "(" + stringify(terms) + ")"
        case VariableTerm(variable) => stringify(variable)
    }).mkString(", ")

    def stringify(function: FolFunction) = function.name.name
}