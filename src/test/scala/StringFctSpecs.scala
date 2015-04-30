import de.unima.dws.oamatching.measures.StringMeasureHelper

/**
 * Created by mueller on 16/04/15.
 */
class StringFctSpecs extends UnitSpec {
  "The first letter capitalizer " should "capitalize the first letter" in {

    val test = "mother"
    val result = StringMeasureHelper.upper_case_first_letter(test)
    assert("Mother".equals(result))
    assert(test.equalsIgnoreCase(result))
  }

  "The tokenizer  " should "work correctly" in {

    val test1 = "mother_test"
    val test2 = "motherTest"
    val test3 = "mother test"

    val tokenized1 = StringMeasureHelper.tokenize_combined_all(test1)
    assert(tokenized1.size == 2)
    val tokenized2 = StringMeasureHelper.tokenize_combined_all(test2)
    assert(tokenized2.size == 2)
    val tokenized3 = StringMeasureHelper.tokenize_combined_all(test3)
    assert(tokenized3.size == 2)
  }
}
