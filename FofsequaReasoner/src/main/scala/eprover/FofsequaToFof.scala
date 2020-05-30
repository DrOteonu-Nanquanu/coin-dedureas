package fofsequa_reasoner.eprover

// import jdk.jshell.spi.ExecutionControl.NotImplementedException
import org.nanquanu.fofsequa._

object FofsequaToFof {
    def to_tptp(knowledge_base: Seq[Statement], goal: Statement) : String =
        stringify(knowledge_base) +
          "fof(goal, question," + stringify(goal, true) + ")."

    def stringify (document: Seq[Statement]) : String =
        document.foldLeft("")((accumulator, statement) =>
            accumulator + "fof(knowledge_base, axiom," + stringify(statement) + ").\n"
        )

    def stringify(statement: Statement, is_upper_level: Boolean = false) : String = statement match {
        case BinaryConnectiveStatement(pre_statement, connective, post_statement) => "(" + stringify(pre_statement) + stringify(connective) + stringify(post_statement) + ")"
        case UnaryConnectiveStatement(connective, post_statement) => stringify(connective) + stringify(post_statement)

        case QuantifiedStatement(quantifier, arguments, statement) => {
            stringify(QuantifiedStatement(quantifier, arguments, statement), is_upper_level)
        }

        case AtomStatement(predicate, terms) => stringify(predicate) + "(" + stringify_term_list(terms) + ")"
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

    def stringify(statement : QuantifiedStatement, is_upper_level: Boolean): String = statement match {
        case QuantifiedStatement(quantifier, arguments, statement) =>
        {
            arguments match {
                case BasicQuantifierArguments(variables) => quantified_statement(
                    quantifier,
                    variables.map((variable: Variable) => stringify(variable)).mkString(", "),
                    statement
                )

                case ConstantSetQuantifierArguments(variable_list, constant_set) => {
                    constant_set match {
                        case BasicConstantSet(constants) => {
                            val connector = " " + (quantifier match {
                                case ForAll() => stringify(And())
                                case Exists() => stringify(Or())
                            }) + " "

                            // TODO: for each constant, create a statement which is equal to `statement` with `variable` replaced by the constant
                            // then concat these together with `connector` in between them
                            throw new Error("Quantifying over constant sets isn't supported yet")
                        }
                        // throw new Error("can't directly stringify BasicConstantSet, only through stringify(QuantifiedStatement)")
                        case PatternVar(name) => {
                            if(is_upper_level) {
                                quantified_statement(
                                    quantifier match {
                                        case ForAll() => Exists()
                                        case Exists() => throw new Error("Existential quantifiers aren't supported when quantifying over pattern variables. Use the existential quantifier \"!\" instead.")
                                    },
                                    stringify_variable_list(variable_list),
                                    statement
                                )
                            }
                            else {
                                throw new Error("Statements quantifying over a pattern variable can only be the first thing in a statement")
                            }
                        }
                    }
                }
            }
        }
    }

    def quantified_statement(quantifier: Quantifier, arguments: String, statement: Statement) = {
        stringify(quantifier) + "[" + arguments +  "]: " + stringify(statement)
    }
    def stringify(constant_set: ConstantSet): String = throw new Exception("not implemented") // todo

    def stringify(variable: Variable): String = stringify(variable.id).capitalize

    def stringify_variable_list(variable_list: Seq[Variable]): String = variable_list.map(stringify).mkString(",")

    def stringify(id: LowercaseID): String = id.name

    def stringify(predicate: FolPredicate): String = stringify(predicate.name)

    def stringify(uppercase_ID: UppercaseID): String = uppercase_ID.name

    def stringify_term_list(terms: Seq[FolTerm]) : String = terms.map(
        term => stringify(term)
    ).mkString(", ")

    def stringify(term: FolTerm): String = term match {
        case ConstantTerm(constant) => stringify(constant)
        case FunctionApplication(function, terms) => stringify(function) + "(" + stringify_term_list(terms) + ")"
        case VariableTerm(variable) => stringify(variable)
    }

    def stringify(constant: Constant) : String = "'" + stringify(constant.id) + "'"

    def stringify(function: FolFunction) : String = stringify(function.name)
}