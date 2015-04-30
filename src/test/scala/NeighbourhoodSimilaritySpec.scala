import de.unima.dws.oamatching.core.{AlignmentParser, OntologyLoader}
import de.unima.dws.oamatching.matcher.elementlevel.SimpleStringFunctionMatcher
import de.unima.dws.oamatching.matcher.structurallevel.{NeighborHoodSimilarityMatcher, PropertiesMatcher}
import de.unima.dws.oamatching.measures.{StringMeasures, StringMeasureHelper}
import de.unima.dws.oamatching.pipeline.MatchingProblem

/**
 * Created by mueller on 25/03/15.
 */
class NeighbourhoodSimilaritySpec extends UnitSpec {
  val base_matcher = new SimpleStringFunctionMatcher(true, true, true, true, StringMeasureHelper.measure_lower_cased(StringMeasures.computeLevenShteinSim))
  val n_matcher = new NeighborHoodSimilarityMatcher(NeighborHoodSimilarityMatcher.STRATEGY_MAX)

  "The neighbourhood similarity matcher " should "work" in {

  }

  "The child hierachy" should "be parsed correctly" in {

    val onto1 = OntologyLoader.load_fast_ontology("ontos/testontos/hierachy/onto1.owl")

    val top = onto1.class_name_to_IRI("http://www.semanticweb.org/mueller/ontologies/2015/2/hierachy#top")
    val descendents = n_matcher.getChildren(top,0,onto1.parent_to_child_classes_map)
    val top_5_1 = onto1.class_name_to_IRI("http://www.semanticweb.org/mueller/ontologies/2015/2/hierachy#top_5_1_1")
    assert(descendents.get(top_5_1) == 5)

    val top_5_2 = onto1.class_name_to_IRI("http://www.semanticweb.org/mueller/ontologies/2015/2/hierachy#top_5_2_1")
    assert(descendents.get(top_5_2) == 5)

    val top_2 = onto1.class_name_to_IRI("http://www.semanticweb.org/mueller/ontologies/2015/2/hierachy#top_2")
    assert(descendents.get(top_2) == 2)
    assert(descendents.get(top) == 0)
  }

  "The parents hierachy" should "be parsed correctly" in {

    val onto1 = OntologyLoader.load_fast_ontology("ontos/testontos/hierachy/onto1.owl")


    val top_5_1 = onto1.class_name_to_IRI("http://www.semanticweb.org/mueller/ontologies/2015/2/hierachy#top_5_1_1")

    val descendents = n_matcher.getParents(top_5_1,0,onto1.child_to_parents_classes_map)
    val top = onto1.class_name_to_IRI("http://www.semanticweb.org/mueller/ontologies/2015/2/hierachy#top")
    assert(descendents.get(top) == 5)

  }


  "The NeighborhoodSimilarity " should " be computed correctly" in {

    val onto1 = OntologyLoader.load_fast_ontology("ontos/testontos/hierachy/tree1.owl")
    val onto2 = OntologyLoader.load_fast_ontology("ontos/testontos/hierachy/tree2.owl")
    val initial_alignment = base_matcher.align(onto1, onto2, 0.6)
    val problem = MatchingProblem(onto1,onto2,null,null,"test")
    val created =  n_matcher.align(problem,initial_alignment,0.6)

    println(created.correspondences)
  }

  "The NeighborhoodSimilarity " should "work with reallife ontologies" in {

    val onto1 = OntologyLoader.load_fast_ontology("ontos/2014/conference/confOf.owl")
    val onto2 = OntologyLoader.load_fast_ontology("ontos/2014/conference/iasted.owl")

    val reference = AlignmentParser.parseRDFWithOntos("ontos/2014/conference/reference-alignment/confOf-iasted.rdf","ontos/2014/conference/cmt.owl","ontos/2014/conference/Conference.owl")
    val initial_alignment = base_matcher.align(onto1, onto2, 0.4)

    println(reference.correspondences.size)
    val initial_result = initial_alignment.evaluate(reference)
    println(initial_result)

    val problem = MatchingProblem(onto1,onto2,null,null,"test")
    val created =  n_matcher.align(problem,initial_alignment,0.6)

    val created_result = created.evaluate(reference)
    println(created_result)
    println(created.correspondences)
  }

  "The NeighborhoodSimilarity " should "work with conference ekaw ontologies" in {

    val onto1 = OntologyLoader.load_fast_ontology("ontos/2014/conference/Conference.owl")
    val onto2 = OntologyLoader.load_fast_ontology("ontos/2014/conference/ekaw.owl")

    val reference = AlignmentParser.parseRDFWithOntos("ontos/2014/conference/reference-alignment/conference-ekaw.rdf","ontos/2014/conference/Conference.owl","ontos/2014/conference/ekaw.owl")
    val initial_alignment = base_matcher.align(onto1, onto2, 0.4)

    println(reference.correspondences.size)
    val initial_result = initial_alignment.evaluate(reference)

    val problem = MatchingProblem(onto1,onto2,null,null,"test")
    val created =  n_matcher.align(problem,initial_alignment,0.6)
    val created_result = created.evaluate(reference)

  }
}
