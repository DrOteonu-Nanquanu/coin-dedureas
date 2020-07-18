package org.nanquanu.fofsequa_reasoner.eprover

// import jdk.jshell.spi.ExecutionControl.NotImplementedException
import org.nanquanu.fofsequa._

import scala.collection.immutable
import scala.collection.immutable.HashMap

object FofsequaToFof {
    def to_tptp(knowledge_base: Seq[Statement], goal: Statement) : String =
        stringify_document(immutable.HashMap.empty, knowledge_base) +
          "fof(goal, question," + stringify(immutable.HashMap.empty[Variable, Constant], goal, true) + ")."

    def stringify_document(document: Seq[Statement]): String = stringify_document(immutable.HashMap.empty, document)
    // def stringify_document = stringify_document(immutable.HashMap.empty, _)


    def stringify_document(substitutions: immutable.HashMap[Variable, Constant], document: Seq[Statement]) : String =
        document.foldLeft("")((accumulator, statement) =>
            accumulator + "fof(knowledge_base, axiom," + stringify(substitutions, statement) + ").\n"
        )

    def stringify(substitutions: immutable.HashMap[Variable, Constant], statement: Statement, is_upper_level: Boolean = false) : String = statement match {
        case BinaryConnectiveStatement(pre_statement, connective, post_statement) => "(" + stringify(substitutions, pre_statement) + stringify(substitutions, connective) + stringify(substitutions, post_statement) + ")"
        case UnaryConnectiveStatement(connective, post_statement) => stringify(substitutions, connective) + stringify(substitutions, post_statement)

        case QuantifiedStatement(quantifier, arguments, statement) => {
            stringify(substitutions, QuantifiedStatement(quantifier, arguments, statement), is_upper_level)
        }

        case AtomStatement(predicate, terms) => stringify(substitutions, predicate) + "(" + stringify_term_list(substitutions, terms) + ")"
    }

    def stringify(substitutions: immutable.HashMap[Variable, Constant], connective: BinaryConnective) : String = connective match {
        case And() => "&"
        case Or() => "|"
        case IfThen() => "=>"
        case Iff() => "<=>"
    }

    def stringify(substitutions: immutable.HashMap[Variable, Constant], connective: UnaryConnective) : String = connective match { case Not() => "~" }

    def stringify(substitutions: immutable.HashMap[Variable, Constant], quantifier: Quantifier) : String = quantifier match {
        case ForAll() => "!"
        case Exists() => "?"
    }

    def stringify(substitutions: immutable.HashMap[Variable, Constant], statement : QuantifiedStatement, is_upper_level: Boolean): String = statement match {
        case QuantifiedStatement(quantifier, arguments, statement) =>
        {
            arguments match {
                case BasicQuantifierArguments(variables) => quantified_statement(
                    substitutions,
                    quantifier,
                    variables.map((variable: Variable) => stringify(substitutions, variable)).mkString(", "),
                    statement
                )

                case ConstantSetQuantifierArguments(variable_list, constant_set) => {
                    constant_set match {
                        case BasicConstantSet(constants) => {
                            val connector = " " + (quantifier match {
                                case ForAll() => stringify(substitutions, And())
                                case Exists() => stringify(substitutions, Or())
                            }) + " "

                            // TODO: for each constant, create a statement which is equal to `statement` with `variable` replaced by the constant
                            // then concat these together with `connector` in between them
                            // throw new Error("Quantifying over constant sets isn't supported yet")


                            constants.map(constant_tuple => {
                                val additional_substitutions = variable_list.zip(constant_tuple.constants)
                                stringify(substitutions ++ additional_substitutions, statement)
                            })
                          .mkString(' ' + connector + ' ')
                        }
                        // throw new Error("can't directly stringify BasicConstantSet, only through stringify(substitutions, QuantifiedStatement)")
                        case PatternVar(name) => {
                            if(is_upper_level) {
                                quantified_statement(
                                    substitutions,
                                    quantifier match {
                                        case ForAll() => Exists()
                                        case Exists() => throw new Error("Existential quantifiers aren't supported when quantifying over pattern variables. Use the existential quantifier \"!\" instead.")
                                    },
                                    stringify_variable_list(substitutions, variable_list),

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

    def quantified_statement(substitutions: HashMap[Variable, Constant], quantifier: Quantifier, arguments: String, statement: Statement) = {
        stringify(substitutions, quantifier) + "[" + arguments +  "]: " + stringify(substitutions, statement)
    }
    def stringify(substitutions: immutable.HashMap[Variable, Constant], constant_set: ConstantSet): String = throw new Exception("not implemented") // todo

    def stringify(substitutions: immutable.HashMap[Variable, Constant], variable: Variable): String = substitutions.get(variable) match {
        case Some(constant) => stringify(substitutions, constant)
        case None => stringify(substitutions, variable.id).capitalize
    }

    def stringify_variable_list(substitutions: HashMap[Variable, Constant], variable_list: Seq[Variable]): String = variable_list.map(stringify(substitutions, _)).mkString(",")

    def stringify(substitutions: immutable.HashMap[Variable, Constant], id: LowercaseID): String = id.name

    def stringify(substitutions: immutable.HashMap[Variable, Constant], predicate: FolPredicate): String = stringify(substitutions, predicate.name)

    def stringify(substitutions: immutable.HashMap[Variable, Constant], uppercase_ID: UppercaseID): String = uppercase_ID.name

    def stringify_term_list(substitutions: HashMap[Variable, Constant], terms: Seq[FolTerm]) : String = terms.map(
        term => stringify(substitutions, term)
    ).mkString(", ")

    def stringify(substitutions: immutable.HashMap[Variable, Constant], term: FolTerm): String = term match {
        case ConstantTerm(constant) => stringify(substitutions, constant)
        case FunctionApplication(function, terms) => stringify(substitutions, function) + "(" + stringify_term_list(substitutions, terms) + ")"
        case VariableTerm(variable) => stringify(substitutions, variable)
    }

    def stringify(substitutions: immutable.HashMap[Variable, Constant], constant: Constant) : String = "'" + stringify(substitutions, constant.id) + "'"

    def stringify(substitutions: immutable.HashMap[Variable, Constant], function: FolFunction) : String = stringify(substitutions, function.name)
}