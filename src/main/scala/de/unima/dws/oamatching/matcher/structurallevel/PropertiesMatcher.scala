package de.unima.dws.oamatching.matcher.structurallevel

import de.unima.dws.oamatching.core.matcher.StructuralLevelMatcher
import de.unima.dws.oamatching.core.{Alignment, Cell, FastOntology, MatchingCell}
import de.unima.dws.oamatching.pipeline.MatchingSelector
import org.semanticweb.owlapi.model.IRI

/**
 * Created by mueller on 25/03/15.
 */
class PropertiesMatcher extends StructuralLevelMatcher {
  override protected def align(onto1: FastOntology, onto2: FastOntology, initial_Alignment: Alignment, threshold: Double): Alignment = {


    val produced_matching: Set[Vector[Option[MatchingCell]]] = initial_Alignment.getPresentMatchTypesinAlignment().map(match_type => {

      val filtered_alignment = initial_Alignment.getNewAlignmentWithMatchType(match_type)

      //selection function

      val selected_matchings = MatchingSelector.fuzzyGreedyRankSelectorDelta(0.05)(filtered_alignment.asMatchRelationMap(),0.5, onto1, onto2)

      val selected_alignment = new Alignment(filtered_alignment.onto1, filtered_alignment.onto2, filtered_alignment.onto1_reference, filtered_alignment.onto2_reference, filtered_alignment.i_onto1, filtered_alignment.i_onto2,selected_matchings)

      val obj_alignments = for (obj_prop_1 <- onto1.base_values.object_properties;
                                obj_prop_2 <- onto2.base_values.object_properties) yield {
        val domain1 = onto1.object_property_to_domain_map.get(obj_prop_1)
        val domain2 = onto2.object_property_to_domain_map.get(obj_prop_2)

        val domain_match = uris_match(domain1.get, domain2.get, onto1, onto2, filtered_alignment)

        val range1 = onto1.object_property_to_range_map.get(obj_prop_1)
        val range2 = onto2.object_property_to_range_map.get(obj_prop_2)

        val range_match = uris_match(range1.get, range2.get, onto1, onto2, filtered_alignment)


        if (range_match > 0.0 && domain_match > 0.0) {
          val measure = (range_match+range_match)/2
          if(measure > threshold){
            Option(MatchingCell(obj_prop_1.toString, obj_prop_2.toString, measure, "=", Cell.TYPE_OBJECT_PROPERTY, match_type))
          }else {
            Option.empty
          }

        } else {
          Option.empty
        }
      }

      val data_alignments = for (data_prop_1 <- onto1.base_values.data_properties;
                                 data_prop_2 <- onto2.base_values.data_properties) yield {
        val domain1 = onto1.data_property_to_domain_map.get(data_prop_1)
        val domain2 = onto2.data_property_to_domain_map.get(data_prop_2)

        val domain_match = uris_match(domain1.get, domain2.get, onto1, onto2, filtered_alignment)

        if (domain_match>0.0) {
          if(domain_match>= threshold) {
            Option(MatchingCell(data_prop_1.toString, data_prop_2.toString, domain_match, "=", Cell.TYPE_DT_PROPERTY, match_type))
          }else {
            Option.empty
          }
        } else {
          Option.empty
        }
      }

      data_alignments ++ obj_alignments
    })

    val cells = produced_matching.flatten.filter(_.isDefined).map(_.get).toSet

    val copied_alingment = new Alignment(initial_Alignment)

    copied_alingment.addAllCorrespondecesKeepHigher(cells)

    copied_alingment

  }

  private def uris_match(iris_1: Set[IRI], iris_2: Set[IRI], onto1: FastOntology, onto2: FastOntology, alignment: Alignment): Double = {

    val tester = for (iri_1 <- iris_1;
                      iri_2 <- iris_2;
                      measure = uri_match(iri_1, iri_2, onto1, onto2, alignment)) yield {

      measure
    }

    (tester.sum.toDouble / tester.size.toDouble)
  }

  private def uri_match(iri_1: IRI, iri_2: IRI, onto1: FastOntology, onto2: FastOntology, alignment: Alignment): Double = {
    val base_measure= uri_match_single(iri_1, iri_2, alignment)
    if (base_measure > 0.0) {
      base_measure
    } else {
      //now parents
      val iri_2_parent = onto2.child_to_parents_classes_map.get(iri_2)
      val iri2_parent_match = if (iri_2_parent.isDefined && iri_2_parent.get.size > 0) {
        val measure = uri_match_single(iri_1, iri_2_parent.get.head, alignment)
        if (measure > 0.0) {
          measure
        } else {
          0.0
        }
      } else {
        0.0
      }


      val iri_1_parent = onto1.child_to_parents_classes_map.get(iri_1)
      val iri1_parent_match = if (iri_1_parent.isDefined && iri_1_parent.get.size > 0) {
        val measure = uri_match_single(iri_1_parent.get.head, iri_2, alignment)
        if (measure > 0.0) {
          measure
        } else {
          0.0
        }
      } else {
        0.0
      }
      if (iri1_parent_match > 0.0 && iri2_parent_match > 0.0) {
        (iri1_parent_match + iri2_parent_match)/2
      } else if (iri1_parent_match > 0.0){
        iri1_parent_match
      }else if(iri2_parent_match > 0.0){
        iri2_parent_match
      }else {
        0.0
      }
    }

  }

  private def uri_match_single(iri_1: IRI, iri_2: IRI, alignment: Alignment): Double = {
    val iri_1_string = iri_1.toString
    val iri_2_string = iri_2.toString

    if (iri_1_string.equals(iri_2_string)) {
      0.0
    } else {
      if (alignment.containsCorrespondence(iri_1_string, iri_2_string)) {
        val measure = alignment.getCorrespondence(iri_1_string, iri_2_string).measure
        measure
      } else {
        0.0
      }
    }
  }


}
