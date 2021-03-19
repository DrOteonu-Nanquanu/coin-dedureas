import org.nanquanu.fofsequa._
import org.scalatest
import org.scalatest.flatspec.AnyFlatSpec
import FofseqParser._

class TestBase extends AnyFlatSpec {
  def parse_equals[R](parser: FofseqParser.Parser[R], fsq_text: String, expected_result: R): scalatest.Assertion = {
    val parse_result = FofseqParser.parseAll(parser, fsq_text)

    parse_result match {
      case FofseqParser.Success(digital_entity, _) => assert(digital_entity == expected_result)
      case FofseqParser.NoSuccess(error, _) => throw new Exception(f"""Failed to parse: "$fsq_text\"\n""" + error)
    }
  }

  "Digital entities" should "parse correctly" in {
    parse_equals(FofseqParser.digital_entity.parse, "\"abc 123 def4\"", FofseqParser.DigitalEntity("abc 123 def4"))
    parse_equals(FofseqParser.digital_entity.parse, "\"??A-b c-d.,fe!!\"", FofseqParser.DigitalEntity("??A-b c-d.,fe!!"))
  }

  "Empty digital entity" should "not parse" in {
    val parse_result = FofseqParser.parseAll(FofseqParser.digital_entity.parse, "\"\"")

    assert(!parse_result.successful)
  }

  "Constant tuple of digital-entities" should "parse correctly" in {
    val parse_result = FofseqParser.parseAll(FofseqParser.constant_tuple.parse, """<"a","abc 123">""")
    parse_result match {
      case FofseqParser.Success(constant_tuple, _) =>
        assert(constant_tuple == ConstantTuple(List(
          FofseqParser.DigitalEntity("a"),
          FofseqParser.DigitalEntity("abc 123"),
        )))
      case FofseqParser.NoSuccess(error, _) => throw new Exception(error)
    }
  }

  "Constant tuple of a single digital-entity" should "parse correctly" in {
    parse_equals(FofseqParser.constant_tuple.parse, "\"abc 123\"", FofseqParser.ConstantTuple(List(DigitalEntity("abc 123"))))
    parse_equals(FofseqParser.constant_tuple.parse, "  \"abc 123\" ", FofseqParser.ConstantTuple(List(DigitalEntity("abc 123"))))
  }

  "Constant tuple of constants and digital-entities" should "parse correctly" in {
    parse_equals(FofseqParser.constant_tuple.parse, """<"abc 123", 'a', 'b'>""", FofseqParser.ConstantTuple(List(
      FofseqParser.DigitalEntity("abc 123"),
      FofseqParser.Constant(FofseqParser.LowercaseID("a")),
      FofseqParser.Constant(FofseqParser.LowercaseID("b")),
    )))

    parse_equals(FofseqParser.constant_tuple.parse, """<'a', 'b', "abc 123">""", FofseqParser.ConstantTuple(List(
      FofseqParser.Constant(FofseqParser.LowercaseID("a")),
      FofseqParser.Constant(FofseqParser.LowercaseID("b")),
      FofseqParser.DigitalEntity("abc 123"),
    )))
  }

  "Constant_like" should "parse both constants and digital-entities correctly" in {
    for((string : String, parsed: ConstantLike) <- List(
      ("\"abc 123\"", FofseqParser.DigitalEntity("abc 123")),
      ("'abc'", FofseqParser.Constant(FofseqParser.LowercaseID("abc"))),
    )) {
      parse_equals(FofseqParser.constant_like.parse, string, parsed)

      /* val parse_result = FofseqParser.parseAll(FofseqParser.constant_like, string)

      parse_result match {
        case FofseqParser.Success(constant_tuple, _) =>
          assert(constant_tuple == parsed)
        case FofseqParser.NoSuccess(error, _) => throw new Exception(f"Failed to parse: "{}"" + error)
      }*/
    }
  }

  "Quantifiers over constant sets" should "parse and resolve correctly" in {
    val kb = "![x from {'a', 'b', 'c'}]: P(x)"

    parse_equals(FofseqParser.quantifier_arguments.parse, "x from {'a', 'b', 'c'}", FofseqParser.ConstantSetQuantifierArguments(
      List(FofseqParser.Variable(FofseqParser.LowercaseID("x"))),
      FofseqParser.BasicConstantSet(List(
        FofseqParser.ConstantTuple(List(
          FofseqParser.Constant(FofseqParser.LowercaseID("a")),
        )),
        FofseqParser.ConstantTuple(List(
          Constant(FofseqParser.LowercaseID("b")),
        )),
        ConstantTuple(List(
          Constant(FofseqParser.LowercaseID("c")),
        ))
      ))
    ))

    parse_equals(
      FofseqParser.statement.parse,
      kb,

      QuantifiedStatement(ForAll(), ConstantSetQuantifierArguments(
        List(Variable(LowercaseID("x"))),
        BasicConstantSet(List(
          ConstantTuple(List(
            Constant(LowercaseID("a")),
          )),
          ConstantTuple(List(
            Constant(LowercaseID("b")),
          )),
          ConstantTuple(List(
            Constant(LowercaseID("c")),
          ))
        ))
      ), AtomStatement(FolPredicate(UppercaseID("P")), List(VariableTerm(Variable(LowercaseID("x")))))),

    )
  }

  "Temporal statement" should "parse correctly" in {
    // val parse_result = FofseqTemporalParser.parseAll(FofseqTemporalParser.temporal_statement, "ValidFrom(123, P('a'))")
    // 
    // assert(parse_result.successful)
  }
}
