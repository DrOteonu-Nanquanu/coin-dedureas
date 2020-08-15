import org.nanquanu.fofsequa._
import org.scalatest.flatspec.AnyFlatSpec

class TestBase extends AnyFlatSpec{
  "Digital entities" should "parse successfully" in {
    val parse_result = FolseqParser.parseAll(FolseqParser.digital_entity, "\"abc 123 def4\"")
    parse_result match {
      case FolseqParser.Success(digital_entity, _) => assert(digital_entity == DigitalEntity("abc 123 def4"))
      case FolseqParser.NoSuccess(error, _) => throw new Exception(error)
    }
  }
}
