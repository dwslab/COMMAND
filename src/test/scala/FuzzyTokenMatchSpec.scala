import de.unima.dws.fuzzytoken.{FuzzyDiceMeasure, FuzzyCosineMeasure, FuzzyJaccardMeasure, FuzzyTokenMatch}
import de.unima.dws.oamatching.measures.{StringMeasures, StringMeasureHelper}

/**
 * Created by mueller on 21/03/15.
 */
class FuzzyTokenMatchSpec extends UnitSpec {


  "An Fuzzy match " should "work" in {
    val test_match = new FuzzyJaccardMeasure(StringMeasureHelper.tokenize_low_whiteSpace,StringMeasures.computeLevenShteinSim)

    val result = test_match.computeOverlap("nba mcgrady","macgrady nba",0.5)

    println(result)
    assert(result == (15.toDouble/17.toDouble))
  }

 it should "work with cosine as well" in {
    val test_match = new FuzzyCosineMeasure(StringMeasureHelper.tokenize_low_whiteSpace,StringMeasures.computeLevenShteinSim)

    val result = test_match.computeOverlap("nba mcgrady","macgrady nba",0.5)

   println(result)
  }

  it should "work with dice as well" in {
    val test_match = new FuzzyDiceMeasure(StringMeasureHelper.tokenize_low_whiteSpace,StringMeasures.computeLevenShteinSim)

    val result = test_match.computeOverlap("nba mcgrady","macgrady nba",0.5)

   println(result)
  }

  it should "return for fuzzy jaccard the same results as in the original implementation" in {
    val test_match = new FuzzyJaccardMeasure(StringMeasureHelper.tokenize_low_dash,StringMeasures.computeLevenShteinSim)

    val result1 = test_match.computeOverlap("www_yahoomail_com","www_yahhoomail_com",0.85)
    assert(result1=== 0.935484 +- 0.001)

    val result2 = test_match.computeOverlap("nurse_practitioner_jobs_in_college_settings","nurse_practitioner_jobs_in_college_settings",0.85)
    assert(result2=== 1.0 +- 0.001)

    val result3 = test_match.computeOverlap("collegeboards","collegeboard",0.85)
    assert(result3=== 0.857143 +- 0.001)

    val result4 = test_match.computeOverlap("novellist_cross","novelist_cross",0.85)
    assert(result4=== 0.894737 +- 0.001)
  }


  it should "return for fuzzy dice the same results as in the original implementation" in {
    val test_match = new FuzzyDiceMeasure(StringMeasureHelper.tokenize_low_dash,StringMeasures.computeLevenShteinSim)

    val result1 = test_match.computeOverlap("www_yahoomail_com","www_yahhoomail_com",0.85)
    assert(result1=== 0.966667 +- 0.001)

    val result2 = test_match.computeOverlap("nurse_practitioner_jobs_in_college_settings","nurse_practitioner_jobs_in_college_settings",0.85)
    assert(result2=== 1.0 +- 0.001)

    val result3 = test_match.computeOverlap("g_e_d","g_e_d_online",0.85)
    assert(result3=== 0.857143 +- 0.001)

    val result4 = test_match.computeOverlap("real_estate_atlanta","atlanta_real_estate",0.85)
    assert(result4=== 1.0 +- 0.001)

    val result5 = test_match.computeOverlap("collegeboards","collegeboard",0.85)
    assert(result5=== 0.923077 +- 0.001)
  }

  it should "return for fuzzy cosine the same results as in the original implementation" in {
    val test_match = new FuzzyCosineMeasure(StringMeasureHelper.tokenize_low_dash,StringMeasures.computeLevenShteinSim)

    val result1 = test_match.computeOverlap("www_yahoomail_com","www_yahhoomail_com",0.85)
    assert(result1=== 0.966667 +- 0.001)

    val result2 = test_match.computeOverlap("nurse_practitioner_jobs_in_college_settings","nurse_practitioner_jobs_in_college_settings",0.85)
    assert(result2=== 1.0 +- 0.001)

    val result3 = test_match.computeOverlap("g_e_d","g_e_d_online",0.85)
    assert(result3=== 0.866025 +- 0.001)

    val result4 = test_match.computeOverlap("real_estate_atlanta","atlanta_real_estate",0.85)
    assert(result4=== 1.0 +- 0.001)

    val result5 = test_match.computeOverlap("collegeboards","collegeboard",0.85)
    assert(result5=== 0.923077 +- 0.001)
  }




  it should "behave better " in {
    val measure = StringMeasures.computeFuzzyJaccard("has_the_first_name","hasFirstName")

    assert(measure == 0.75)
  }


}
