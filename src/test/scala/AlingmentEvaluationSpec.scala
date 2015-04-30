import de.unima.dws.oamatching.core.{Alignment, MatchingCell}

/**
 * This Unit test, tests the alingment creation and how it's evaluated
 */
class AlingmentEvaluationSpec extends UnitSpec {

  val correspondences = List(MatchingCell("1:A", "2:A", 1.0, "=", "NN", "NN"), MatchingCell("1:B", "2:B", 1.0, "=", "NN", "NN"), MatchingCell("1:C", "2:C", 1.0, "=", "NN", "NN"))
  val reference_alignment = new Alignment("1", "2", null, null,null,null ,correspondences)


  val correspondences_2 = List(MatchingCell("1:A", "2:A", 1.0, "=", "NN", "NN"), MatchingCell("1:B", "2:B", 1.0, "=", "NN", "NN"), MatchingCell("1:C", "2:C", 1.0, "=", "NN", "NN"))
  val reference_alignment_2 = new Alignment("1", "2", null, null,null,null ,correspondences)


  "An Alignment" should "increase the size when a different correspondence is added" in {
    val init_size = reference_alignment.correspondences.size
    reference_alignment.addToCorrespondences(MatchingCell("1:D", "2:D", 1.0, "=", "NN", "NN"))
    val added_size = reference_alignment.correspondences.size
    assert(init_size + 1 == added_size)
  }

  it should "not increase the size when a already defined correspondence is added" in {
    val init_size = reference_alignment.correspondences.size
    reference_alignment.addToCorrespondences(MatchingCell("1:A", "2:A", 1.0, "=", "NN", "NN"))
    val added_size = reference_alignment.correspondences.size
    assert(init_size == added_size)
  }

  it should "evaluate to a F1-Measure of 1 when evaluated with itself" in {
    val eval_res = reference_alignment.evaluate(reference_alignment)
    assert(eval_res.f1Measure == 1.0)
  }

  it should "evaluate to a F1-Measure of 0.5 when having only one true positive" in {

    val correspondences = List(MatchingCell("1:A", "2:A", 1.0, "=", "NN", "NN"))
    val test_alignment = new Alignment("1", "2", null, null,null,null, correspondences)

    val eval_res = test_alignment.evaluate(reference_alignment_2)

    assert(eval_res.f1Measure == 0.5)
  }

  it should "evaluate to a precision of 0.75 and a recall of 1.0 when having only one false positive and 3 true positives and 0 false negatives" in {

    val correspondences = List(MatchingCell("1:D", "2:D", 1.0, "=", "NN", "NN"), MatchingCell("1:A", "2:A", 1.0, "=", "NN", "NN"), MatchingCell("1:B", "2:B", 1.0, "=", "NN", "NN"), MatchingCell("1:C", "2:C", 1.0, "=", "NN", "NN"))
    val test_alignment = new Alignment("1", "2", null, null,null,null ,correspondences)

    val eval_res = test_alignment.evaluate(reference_alignment_2)

    assert(eval_res.precision == 0.75)
    assert(eval_res.recall == 1.0)
    assert(eval_res.f1Measure === 0.857 +- 0.001)

  }

  it should "evaluate 0 for all measures when the it is compared to an alignment that has nothing in common" in {
    val correspondences = List(MatchingCell("1:X", "2:X", 1.0, "=", "NN", "NN"), MatchingCell("1:Y", "2:Y", 1.0, "=", "NN", "NN"), MatchingCell("1:Z", "2:Z", 1.0, "=", "NN", "NN"))
    val test_alignment = new Alignment("1", "2", null, null,null,null, correspondences)
    val eval_res = test_alignment.evaluate(reference_alignment_2)

    assert(eval_res.precision == 0.0)
    assert(eval_res.recall == 0.0)
    assert(eval_res.f1Measure === 0.0 +- 0.001)
  }


}