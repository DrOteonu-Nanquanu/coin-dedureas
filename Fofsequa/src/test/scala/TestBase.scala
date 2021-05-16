import org.nanquanu.fofsequa._
import org.scalatest
import org.scalatest.flatspec.AnyFlatSpec

class TestBase extends AnyFlatSpec {
  def parse_equals[R](parser: FolseqParser.Parser[R], fsq_text: String, expected_result: R): scalatest.Assertion = {
    val parse_result = FolseqParser.parseAll(parser, fsq_text)

    parse_result match {
      case FolseqParser.Success(digital_entity, _) => assert(digital_entity == expected_result)
      case FolseqParser.NoSuccess(error, _) => throw new Exception(f"""Failed to parse: "$fsq_text\"\n""" + error)
    }
  }

  "Digital entities" should "parse correctly" in {
    parse_equals(FolseqParser.digital_entity, "\"abc 123 def4\"", DigitalEntity("abc 123 def4"))
    parse_equals(FolseqParser.digital_entity, "\"??A-b c-d.,fe!!\"", DigitalEntity("??A-b c-d.,fe!!"))
  }

  "Empty digital entity" should "not parse" in {
    val parse_result = FolseqParser.parseAll(FolseqParser.digital_entity, "\"\"")

    assert(!parse_result.successful)
  }

  "Constant tuple of digital-entities" should "parse correctly" in {
    val parse_result = FolseqParser.parseAll(FolseqParser.constant_tuple, """<"a","abc 123">""")
    parse_result match {
      case FolseqParser.Success(constant_tuple, _) =>
        assert(constant_tuple == ConstantTuple(List(
          DigitalEntity("a"),
          DigitalEntity("abc 123"),
        )))
      case FolseqParser.NoSuccess(error, _) => throw new Exception(error)
    }
  }

  "Constant tuple of a single digital-entity" should "parse correctly" in {
    parse_equals(FolseqParser.constant_tuple, "\"abc 123\"", ConstantTuple(List(DigitalEntity("abc 123"))))
    parse_equals(FolseqParser.constant_tuple, "  \"abc 123\" ", ConstantTuple(List(DigitalEntity("abc 123"))))
  }

  "Constant tuple of constants and digital-entities" should "parse correctly" in {
    parse_equals(FolseqParser.constant_tuple, """<"abc 123", 'a', 'b'>""", ConstantTuple(List(
      DigitalEntity("abc 123"),
      Constant(LowercaseID("a")),
      Constant(LowercaseID("b")),
    )))

    parse_equals(FolseqParser.constant_tuple, """<'a', 'b', "abc 123">""", ConstantTuple(List(
      Constant(LowercaseID("a")),
      Constant(LowercaseID("b")),
      DigitalEntity("abc 123"),
    )))
  }

  "Constant_like" should "parse both constants and digital-entities correctly" in {
    for((string, parsed) <- List(
      ("\"abc 123\"", DigitalEntity("abc 123")),
      ("'abc'", Constant(LowercaseID("abc"))),
    )) {
      parse_equals(FolseqParser.constant_like, string, parsed)

      /* val parse_result = FolseqParser.parseAll(FolseqParser.constant_like, string)

      parse_result match {
        case FolseqParser.Success(constant_tuple, _) =>
          assert(constant_tuple == parsed)
        case FolseqParser.NoSuccess(error, _) => throw new Exception(f"Failed to parse: "{}"" + error)
      }*/
    }
  }

  "Quantifiers over constant sets" should "parse and resolve correctly" in {
    val kb = "![x from {'a', 'b', 'c'}]: P(x)"

    parse_equals(FolseqParser.quantifier_arguments, "x from {'a', 'b', 'c'}", ConstantSetQuantifierArguments(
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
    ))

    parse_equals(
      FolseqParser.statement,
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
    val parse_result = FofseqTemporalParser.parseAll(FofseqTemporalParser.temporal_statement, "ValidFrom(123, P('a'))")

    assert(parse_result.successful)
  }

  "Complex temporal statements" should "parse correctly" in {
    val successful = List(
      FofseqTemporalParser.parseAll(FofseqTemporalParser.temporal_statement, "P('a')"),
      FofseqTemporalParser.parseAll(FofseqTemporalParser.temporal_statement, "(P('a') and Q('x'))"),
      FofseqTemporalParser.parseAll(FofseqTemporalParser.temporal_statement, "ValidFrom(123, Q('x'))"),
      FofseqTemporalParser.parseAll(FofseqTemporalParser.temporal_statement, "(P('a') and ValidFrom(123, Q('x')))"),
      FofseqTemporalParser.parseAll(FofseqTemporalParser.temporal_statement, "not (P('a') and ValidFrom(123, Q('x')))"),
      FofseqTemporalParser.parseAll(FofseqTemporalParser.temporal_statement, "not ![x]: ValidBetween(3, 12, P(x))"),
    ).map(_.successful)
  
    val all_true = successful.foldLeft(true)((l, r) => l && r)

    assert(all_true)
  }

  "Multi-line documents" should "parse correctly" in {
   val parsed_docs = List(
     """
P('a');"""
    ).map(FofseqTemporalParser.parseAll(FofseqTemporalParser.fofsequa_temporal_document, _))

   for(parsed <- parsed_docs) {
     parsed match {
       case FofseqTemporalParser.Success(parsed, _) => ()
       case FofseqTemporalParser.Error(err, _) => throw new Exception(err)
     }
   }
  
  }
}
