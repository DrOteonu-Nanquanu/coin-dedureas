package eprover

// import jdk.jshell.spi.ExecutionControl.NotImplementedException
import parser._

object FofsequaToFof {
    def stringify (document: List[Statement]) : String =
        document.foldLeft("")((accumulator, statement) =>
            accumulator + stringify(statement) + ".\n"
        )

    def stringify(statement: Statement) : String = statement match {
        case BinaryConnectiveStatement(pre_statement, connective, post_statement) => "(" + stringify(pre_statement) + stringify(connective) + stringify(post_statement) + ")"
        case UnaryConnectiveStatement(connective, post_statement) => stringify(connective) + stringify(post_statement)
        case QuantifiedStatement(quantifier, arguments, statement) => stringify(quantifier) + "[" + stringify(arguments) + "]: " + stringify(statement)
        case AtomStatement(predicate, terms) => stringify(predicate) + "(" + stringify(terms) + ")"
    }

    def stringify(connective: BinaryConnective) : String = connective match {
        case And() => "&"
        case Or() => "|"
        case IfThen() => "=>"
        case Iff() => "<=>"
    }

    def stringify(connective: UnaryConnective) : String = connective match { case Not() => "~" }

    def stringify(quantifier: Quantifier) : String = quantifier match {
        case ForAll() => "!"
        case Exists() => "?"
    }

    def stringify(arguments: QuantifierArguments): String = arguments match {
        case BasicQuantifierArguments(variables) => variables.map((variable: Variable) => stringify(variable)).mkString(" ")
        case ConstantSetQuantifierArguments(variable, constant_set) => {throw new Error("not implemented")}
    }

    def stringify(constant_set: ConstantSet): String = throw new Exception("not implemented") // todo

    def stringify(variable: Variable): String = stringify(variable.id).capitalize

    def stringify(id: LowercaseID): String = id.name

    def stringify(predicate: FolPredicate): String = stringify(predicate.name)

    def stringify(uppercase_ID: UppercaseID): String = uppercase_ID.name

    def stringify(terms: Array[FolTerm]) : String = terms.map(
        term => stringify(term)
    ).mkString(", ")

    def stringify(term: FolTerm): String = term match {
        case ConstantTerm(constant) => stringify(constant)
        case FunctionApplication(function, terms) => stringify(function) + "(" + stringify(terms) + ")"
        case VariableTerm(variable) => stringify(variable)
    }

    def stringify(constant: Constant) : String = "'" + stringify(constant.id) + "'"

    def stringify(function: FolFunction) : String = stringify(function.name)
}