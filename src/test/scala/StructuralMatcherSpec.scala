import de.unima.dws.oamatching.core.OntologyLoader
import de.unima.dws.oamatching.matcher.elementlevel.SimpleStringFunctionMatcher
import de.unima.dws.oamatching.matcher.structurallevel.{GraphBasedUsedClassMatcher, GraphBasedUsedPropertyMatcher}
import de.unima.dws.oamatching.measures.{StringMeasureHelper, StringMeasures}
import de.unima.dws.oamatching.pipeline.MatchingProblem

/**
 * Created by mueller on 25/03/15.
 */
class StructuralMatcherSpec extends UnitSpec {

  val base_matcher = new SimpleStringFunctionMatcher(true, true, true, true, StringMeasureHelper.measure_lower_cased(StringMeasures.computeJaccard))
  val used_property_matcher = new GraphBasedUsedPropertyMatcher()
  val used_class_matcher = new GraphBasedUsedClassMatcher()

  "Used Property matcher" should "work " in {
    val onto1 = OntologyLoader.load_fast_ontology("ontos/2014/conference/cmt.owl")
    val onto2 = OntologyLoader.load_fast_ontology("ontos/2014/conference/Conference.owl")
    val initial_alignment = base_matcher.align(onto1, onto2, 0.6)
    val problem = MatchingProblem(onto1,onto2,null,null,"test")
    used_property_matcher.align(problem,initial_alignment,0.1)
  }

  "Used Class matcher" should "work " in {
    val onto1 = OntologyLoader.load_fast_ontology("ontos/2014/conference/cmt.owl")
    val onto2 = OntologyLoader.load_fast_ontology("ontos/2014/conference/Conference.owl")
    val initial_alignment = base_matcher.align(onto1, onto2, 0.6)
    val problem = MatchingProblem(onto1,onto2,null,null,"test")
    val created = used_class_matcher.align(problem,initial_alignment,0.0)
    println("TEST")
    println(created.correspondences)
  }


}
