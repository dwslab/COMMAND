import de.unima.dws.oamatching.core.OntologyLoader
import de.unima.dws.oamatching.matcher.MatcherRegistry
import de.unima.dws.oamatching.matcher.elementlevel.TokenizedStringMatcher
import de.unima.dws.oamatching.measures.StringMeasureHelper

/**
 * Created by mueller on 26/03/15.
 */
class TokenizedMatcherSpec extends UnitSpec {

  MatcherRegistry.initMatchers()
  val tokenizer: (String) => List[String] = StringMeasureHelper.combine_two_tokenizer(StringMeasureHelper.tokenize_camel_case, StringMeasureHelper.tokenize_low_dash) _

  "The tokenizer" should "work correctly"  in {
    val test_string_1 = "has_the_first_name"
    val test_string_2 = "hasFirstName"
    val tokenized_1 = tokenizer(test_string_1)
    val tokenized_2 = tokenizer(test_string_2)

    assert(tokenized_1.size == 4)
    assert(tokenized_2.size == 3)
  }


  "The tokenized scorer" should "align ontology correctly" in {
    val test_matcher = MatcherRegistry.getMatcherByName("lin").get.asInstanceOf[TokenizedStringMatcher]

    val onto1 = OntologyLoader.load_fast_ontology("ontos/2014/conference/Conference.owl")
    val onto2 = OntologyLoader.load_fast_ontology("ontos/2014/conference/confOf.owl")

    val alignment = test_matcher.align(onto1, onto2, 0.7)

    alignment.correspondences.foreach(a => {
      println(a)
    })
    assert(alignment.containsCorrespondenceRemainder("has_the_first_name", "hasfirstname"))

  }

}
