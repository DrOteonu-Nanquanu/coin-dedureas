import java.io._

import scala.reflect.ClassTag
import scala.util.Random


object Main {
  val basicAxioms = """
fof(livesInTransitivity, axiom,
    ![Pr1, Pr2, Person] :
        ((isPartOf(Pr1, Pr2) & livesIn(Person, Pr1)) =>
livesIn(Person, Pr2))).

fof(hasTypeTransitivity, axiom,
    ![A, B, C] :
        ((hasType(A, B) & hasType(B, C)) => hasType(A, C))).

fof(degreeLevelTransitivity, axiom, ![Dgr1, Dgr2, Dgr3]:
    ((higherLevel(Dgr1, Dgr2) & higherLevel(Dgr2, Dgr3)) =>
higherLevel(Dgr1, Dgr3))).

fof(greaterThanTransitivity, axiom, ![X, Y, Z]: (($greater(X, Y) & $greater(Y, Z)) => $greater(X, Z))).

fof(numberOrder, axiom, $greater(1, 2) & $greater(2, 3) & $greater(3, 4) & $greater(4, 5) & $greater(5, 6) & $greater(6, 7) & $greater(7, 8) & $greater(8, 9) & $greater(9, 10)).

fof(numberFacts, axiom, ![X, Y]: ($greater(X, Y) => $less(Y, X)) & ![X, Y]: (($less(X, Y) | X = Y) => $lesseq(X, Y)) & ![X, Y]: (($greater(X, Y) | X = Y) => $greatereq(X, Y))).
"""

  def main(args: Array[String]): Unit = {
    Random.setSeed(256)

    // generate knowledge base
    val knowledge_base = new KnowledgeBase()
    val axioms_as_tptp = knowledge_base.to_TPTP()
    val queries = generate_queries(knowledge_base)

    val file_name = "test.txt"

    val file = new File(file_name)
    val writer = new PrintWriter(file)
    writer.write(basicAxioms)
    writer.write(axioms_as_tptp)
    writer.write(queries)

    writer.close()
  }



  def generate_queries(kr_base: KnowledgeBase) = {
    def random_degree = randomElement(kr_base.degree_levels)
    def random_country = randomElement(kr_base.countries)
    def random_language = randomElement(kr_base.languages)
    def random_study_type = randomElement(kr_base.study_types)
    def random_study = randomElement(kr_base.studies)
    def random_language_type = randomElement(kr_base.language_types)

    var stringBuilder = new StringBuilder()
    stringBuilder.append(s"""fof(query, question, ?[Person]: ?[Field]: (hasDegree(Person, Field, $random_degree) & hasType(Field, $random_study_type))).\n""")
    stringBuilder.append(s"""fof(query, question, ?[Person]: ?[Field]: (hasDegree(Person, Field, $random_degree) & livesIn(Person, $random_country))).\n""")
    stringBuilder.append(s"""fof(query, question, ?[Person]: (hasDegree(Person, $random_study, $random_degree) & ?[Language]: (speaks(Person, language) & hasType(Language, $random_language_type)))).\n""")
    stringBuilder.append(s"""fof(query, question, ?[Person]: ?[Level]: (skillLevel(Person, “ScalaProgramming”, Level) & ${"$greatereq"}(Level, 6))).\n)""")


    stringBuilder.toString()
  }

  def randomElement[T](elements: Array[T]): T = elements(Random.nextInt(elements.length))

  def random_flatmap[X, Y:ClassTag](elements: Array[X], min_size: Int, max_size: Int, initializer: (X) => Y) =
    elements.flatMap((element: X) =>
      Array.fill(Random.nextInt(max_size - min_size + 1) + min_size)(0)
        .map((value) => initializer(element))
    )
}

class KnowledgeBase {

  import Main.{randomElement, random_flatmap}

  // generate 1000 people
  val people = ConstantGenerator.generate_constant_array(1000)
  // generate 100 languages and 6 types
  val languages = ConstantGenerator.generate_constant_array(100)
  val language_types = ConstantGenerator.generate_constant_array(6)

  // generate 100 educations/studies and 10 types
  val studies = ConstantGenerator.generate_constant_array(100)
  val study_types = ConstantGenerator.generate_constant_array(10)

  // generate 300 fields of expertise and make 100 of them rated
  val fields_of_expertise = ConstantGenerator.generate_constant_array(300)

  // generate generate 50 countries, 150 provinces and 400 towns
  val countries = ConstantGenerator.generate_constant_array(50)
  val provinces = ConstantGenerator.generate_constant_array(150)
  val towns = ConstantGenerator.generate_constant_array(400)

  // generate 10 levels of degrees
  val degree_levels = ConstantGenerator.generate_constant_array(10)

  // assign each person 1 to 3 languages
  val people_languages = random_flatmap(people, 1, 3,
    (_: String, randomElement(languages))
  )

  val languages_types = random_flatmap(languages, 1, 1,
    (_: String, randomElement(language_types))
  )

  // assign each person zero to 4 educations
  val people_educations = random_flatmap(people, 0, 4,
    (_: String, randomElement(studies), randomElement(degree_levels))
  )

  val studies_types = random_flatmap(studies, 1, 2,
    (_: String, randomElement(study_types))
  )

  // assign each person zero to 10 fields of expertise
  val people_fields_of_expertise = random_flatmap(people, 0, 10,
    (_: String, randomElement(fields_of_expertise))
  )
  val people_skill_levels = ;


  // assign each person 1 town
  val people_towns = people.map((person) => (person, randomElement(towns)))

  // assign a province to each town, and a country to each province
  val town_provinces = towns.map((town) => (town, randomElement(provinces)))
  val province_countries = provinces.map((province) => (province, randomElement(countries)))

  def to_TPTP() = {
    var builder = new StringBuilder()

    /*
    for ((person, language) <- people_languages) {
      builder.append(s"fof(language, axiom, speaks($person, $language))")
    }

    for ((person, education, degree) <- people_educations) {
      builder.append(s"fof(language, axiom, hasDegree($person, $education, $degree))")
    }

    for ((person, expertise) <- people_fields_of_expertise) {
      builder.append(s"fof(language, axiom, hasExpertise($person, $expertise))")
    }
    */
    add_to_StringBuilder(builder, people_languages, "speaks")
    add_to_StringBuilder(builder, people_educations, "hasDegree")
    add_to_StringBuilder(builder, people_fields_of_expertise, "hasExpertise")
    add_to_StringBuilder(builder, people_towns, "livesIn")
    add_to_StringBuilder(builder, town_provinces, "isPartOf")
    add_to_StringBuilder(builder, province_countries, "isPartOf")
    add_to_StringBuilder(builder, studies_types, "hasType")
    add_to_StringBuilder(builder, languages_types, "hasType")

    builder.toString()
  }

  private def add_to_StringBuilder[T](builder: StringBuilder, tuple_array: Array[T], axiom_name: String): Unit = {
    builder.append("fof(kr_base, axiom, ")
    for (tuple <- tuple_array) {
      builder.append(s"$axiom_name$tuple & ")
    }
    builder.delete(builder.length() - 2, builder.length())
    builder.append(").\n")
  }
}

object ConstantGenerator {
  var last_constant = ""

  def get_next_constant() : String = {
    var i = 0
    var increment_next = true
    var next_constant = ""

    var continue = true;

    while(continue) {
      if(i >= last_constant.length) {
        if(increment_next) {
          next_constant += 'a'
        }
        continue = false
      }
      else {
        var currentChar = last_constant.charAt(i).toInt

        if(increment_next) {
          if ((currentChar >= 'a'.toInt && currentChar <= 'y'.toInt) || (currentChar >= 'A'.toInt && currentChar <= 'Y'.toInt)) {
            currentChar += 1
            increment_next = false
          }
          else if (currentChar == 'z'.toInt) {
            currentChar = 'A'
            increment_next = false
          }
          else if (currentChar == 'Z'.toInt) {
            currentChar = 'a'
          }
        }
        next_constant += currentChar.toChar
        i += 1
      }
    }

    last_constant = next_constant
    return '"' + next_constant + '"'
  }

  def generate_constant_array(size: Int) : Array[String] =
    Array.fill(size)(0).map(_ => get_next_constant())
}