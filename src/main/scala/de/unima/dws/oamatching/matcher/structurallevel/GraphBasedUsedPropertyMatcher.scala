package de.unima.dws.oamatching.matcher.structurallevel

import de.unima.dws.oamatching.core.matcher.StructuralLevelMatcher
import de.unima.dws.oamatching.core.{FastOntology, MatchingCell, Alignment, Cell}
import org.semanticweb.owlapi.model._

import scala.collection.JavaConversions._

/**
 * Graphbased Matcher:
 * check all matched properties and match also domain and range
 * with half of confidence of matched property
 * Bla <-------new with c = 0.45--------> Blub
 * |                                      |
 * foo <--already matched with c = 0.9--> foo
 * |                                      |
 * v                                      v
 * test <-------new with c = 0.45--------> check
 * @author Sven Hertling (Original Author)
 * @author Alexander C. Mueller (Scala version)
 *
 */
class GraphBasedUsedPropertyMatcher extends StructuralLevelMatcher {

  /**
   * Implements Hot Match graph-based used property matcher,
   * so matches the classes by inspecting the domain and range
   * of properties already matches, consequently it's a very simply similarity propagation
   * @param onto1 ontology 1
   * @param onto2 ontology 2
   * @param initial_Alignment orignal input alignment
   * @param threshold threshold used
   * @return
   */
  override protected def align(onto1: FastOntology, onto2: FastOntology, initial_Alignment: Alignment, threshold: Double): Alignment = {

    def matchPairWise(owl_classes1: Set[IRI], owl_classes2: Set[IRI], measure: Double,match_type:String): Set[MatchingCell] = {
      val cells: Set[Option[MatchingCell]] = for (owl_class1 <- owl_classes1;
                                          owl_class2 <- owl_classes2) yield {
        val candidate =MatchingCell(owl_class1.toString, owl_class2.toString, measure, "=", Cell.TYPE_CLASS,match_type)
        if (initial_Alignment.correspondences.contains(candidate) ) {
          //replace if sim higher if not keep old one
          val elem_in = initial_Alignment.correspondences.filter(_.equals(candidate)).head
          if(elem_in.measure >= measure){
            Option.empty
          }else {
            Option(candidate)
          }

        } else {
          Option(candidate)
        }
      }
      cells.filter(cell => cell.isDefined).map(cell => cell.get)
    }

    val produced_correspondences: Set[Set[MatchingCell]] = initial_Alignment.getPresentMatchTypesinAlignment().map(match_type => {

      val filtered_alingment = initial_Alignment.getNewAlignmentWithMatchType(match_type)

      val additional_correspondences = for (cell <- filtered_alingment.correspondences) yield {
        val iri_1 = IRI.create(cell.entity1)
        val iri_2 = IRI.create(cell.entity2)
        val similarity = cell.measure / 2


        if(onto1.base_values.object_properties.contains(iri_1) && onto2.base_values.object_properties.contains(iri_2) && similarity >= threshold){
          //object properties
          val domain_1 = onto1.object_property_to_domain_map.get(iri_1).get
          val domain_2 = onto2.object_property_to_domain_map.get(iri_2).get

          val range_1 = onto1.object_property_to_range_map.get(iri_1).get
          val range_2 = onto2.object_property_to_range_map.get(iri_2).get

          Option(matchPairWise(domain_1, domain_2, similarity,match_type).toList ::: matchPairWise(range_1, range_2, similarity,match_type).toList)
        } else if (onto1.base_values.data_properties.contains(iri_1) && onto2.base_values.data_properties.contains(iri_2) && similarity >= threshold){
          //data properties

          val domain_1 = onto1.data_property_to_domain_map.get(iri_1).get
          val domain_2 = onto2.data_property_to_domain_map.get(iri_2).get

          Option(matchPairWise(domain_1, domain_2, similarity,match_type).toList)
        }else {
          Option.empty
        }

      }

      additional_correspondences.filter(cells => cells.isDefined).map(cells => cells.get).flatten.toSet
    })


    val copied_alignment = new Alignment(initial_Alignment)

    copied_alignment.addAllCorrespondeces(produced_correspondences.flatten.toSet)
    copied_alignment
  }
}
