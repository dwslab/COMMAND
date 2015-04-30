import de.unima.dws.oamatching.measures.SemanticMeasures

/**
 * Created by mueller on 27/04/15.
 */
class UMLSSpec extends UnitSpec{

  "The UMLSSynonym Finder " should "find a synonym" in {
    val a = "right wrist ligament"
    val b = "ligament of right wrist"

    val similarity = SemanticMeasures.isUMLSSynonym(a,b)

    assert(similarity == 1.0)
  }

  it should " not find a synonym" in {
    val a = "articular cartilage of fourth metacarpal facet of left hamate"
    val b = "ligament of right wrist"

    val similarity = SemanticMeasures.isUMLSSynonym(a,b)

    assert(similarity == 0.0)
  }
}
