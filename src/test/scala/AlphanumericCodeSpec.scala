import de.unima.dws.oamatching.measures.StringMeasureHelper

/**
 * Created by mueller on 01/04/15.
 */
class AlphanumericCodeSpec  extends UnitSpec{


  "The Regex to check the alpha-numeric Code " should "evaluate to true " in {
    val test1 = StringMeasureHelper.isAlphanumericalCode("NCI_C12223")
    val test2 = StringMeasureHelper.isAlphanumericalCode("NCI_C12224")
    val test3 = StringMeasureHelper.isAlphanumericalCode("ABCD123")


    assert(test1==test2==test3==true)
  }

  it should "evaluate to false" in {
    val test1 = StringMeasureHelper.isAlphanumericalCode("Mucosa_of_the_Upper_Lip")
    val test2 = StringMeasureHelper.isAlphanumericalCode("Tonsillar_Pillar")
    val test3 = StringMeasureHelper.isAlphanumericalCode("author")
    val test4 = StringMeasureHelper.isAlphanumericalCode("periodicity")
    val test5 = StringMeasureHelper.isAlphanumericalCode("Conference_document")
    val test6 = StringMeasureHelper.isAlphanumericalCode("hasSubjectArea")


    assert(test1==false)
    assert(test2==false)
    assert(test3==false)
    assert(test4==false)
    assert(test5==false)
    assert(test6==false)

  }
}
