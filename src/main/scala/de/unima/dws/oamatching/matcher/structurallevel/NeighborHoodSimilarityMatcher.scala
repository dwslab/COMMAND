package de.unima.dws.oamatching.matcher.structurallevel

import com.typesafe.scalalogging.slf4j.LazyLogging
import de.unima.dws.oamatching.core.matcher.StructuralLevelMatcher
import de.unima.dws.oamatching.core.{Alignment, Cell, FastOntology, MatchingCell}
import de.unima.dws.oamatching.pipeline.MatchingSelector
import org.semanticweb.owlapi.model.IRI

/**
 * Created by mueller on 25/03/15.
 */
class NeighborHoodSimilarityMatcher(val strategy: Int) extends StructuralLevelMatcher with LazyLogging {


  override protected def align(onto1: FastOntology, onto2: FastOntology, initial_Alignment: Alignment, threshold: Double): Alignment = {

    val produced_matchings: Set[Set[MatchingCell]] = initial_Alignment.getPresentMatchTypesinAlignment().map(match_type => {

      val filtered_alignment: Alignment = initial_Alignment.getNewAlignmentWithMatchType(match_type)

      val selected_matchings = MatchingSelector.fuzzyGreedyRankSelectorDelta(0.05)(filtered_alignment.asMatchRelationMap(), 0.5, onto1, onto2)
      val selected_alignment = new Alignment(filtered_alignment.onto1, filtered_alignment.onto2, filtered_alignment.onto1_reference, filtered_alignment.onto2_reference, filtered_alignment.i_onto1, filtered_alignment.i_onto2, selected_matchings)

      val cells_to_add = filtered_alignment.correspondences.map(cell => {
        getMatchingByNeighborHood(cell, onto1, onto2, selected_alignment, match_type, threshold)
      }).toList.flatten.groupBy(identity)


      val min_value = cells_to_add.map { case (x, ys) => ys.minBy(_.measure)}

      min_value.toSet

    })

    val copied_alignment = new Alignment(initial_Alignment)

    copied_alignment.addAllCorrespondeces(produced_matchings.flatten.toSet)

    copied_alignment
  }


  def getMatchingByNeighborHood(cell: MatchingCell, onto1: FastOntology, onto2: FastOntology, alignment: Alignment, match_type: String, threshold: Double): Set[MatchingCell] = {

    //super classes
    val sub_entities_onto1 = getDirectChildren(cell.owl_type, cell.entity1, onto1)
    val sub_entities_onto2 = getDirectChildren(cell.owl_type, cell.entity2, onto2)

    val matching_from_sub_classes: Set[MatchingCell] = for (entity1 <- sub_entities_onto1.get;
                                                            entity2 <- sub_entities_onto2.get;
                                                            candidate = matchNeighborHood(entity1, entity2, alignment, cell.owl_type, onto1, onto2, match_type, threshold);
                                                            if (candidate.isDefined)) yield {
      candidate.get
    }

    //super classes
    val super_entities_onto1 = getDirectParents(cell.owl_type, cell.entity1, onto1)
    val super_entities_onto2 = getDirectParents(cell.owl_type, cell.entity2, onto2)

    val matching_from_super_classes: Set[MatchingCell] = for (entity1 <- super_entities_onto1.get;
                                                              entity2 <- super_entities_onto2.get;
                                                              candidate = matchNeighborHood(entity1, entity2, alignment, cell.owl_type, onto1, onto2, match_type, threshold)
                                                              if (candidate.isDefined)) yield {
      candidate.get
    }

    //resolve potentially double matched cells
    val intersected_sections: Set[MatchingCell] = for (cell_e1 <- matching_from_sub_classes;
                                                       cell_e2 <- matching_from_super_classes; if (cell_e1.equals(cell_e2))) yield {
      val measure = if (strategy.equals(NeighborHoodSimilarityMatcher.STRATEGY_MIN)) {
        Math.max(cell_e1.measure, cell_e2.measure)
      } else if (strategy.equals(NeighborHoodSimilarityMatcher.STRATEGY_MAX)) {
        Math.min(cell_e1.measure, cell_e2.measure)
      } else {
        (cell_e1.measure + cell_e2.measure) / 2
      }
      MatchingCell(cell_e1.entity1, cell_e1.entity2, measure, cell_e1.relation, cell_e1.owl_type, cell_e1.match_type)
    }

    val resulting_matchings = matching_from_sub_classes ++ matching_from_super_classes ++ intersected_sections

    resulting_matchings
  }

  /**
   * Match based on its neighborhood
   * @param entity1
   * @param entity2
   * @param alignment
   * @param owl_type
   * @param onto1
   * @param onto2
   * @param match_type
   * @return
   */
  def matchNeighborHood(entity1: IRI, entity2: IRI, alignment: Alignment, owl_type: String, onto1: FastOntology, onto2: FastOntology, match_type: String, threshold: Double): Option[MatchingCell] = {
    val entity1_parents: Option[Map[IRI, Integer]] = getParents(entity1, onto1, owl_type)
    val entity1_children = getChildren(entity1, onto1, owl_type)

    val entity2_parents = getParents(entity2, onto2, owl_type)
    val entity2_children = getChildren(entity2, onto2, owl_type)

    //first match parents

    val new_measure_parent = getSimilarity(alignment, match_type, entity1_parents, entity2_parents)

    //first match parents
    val new_measure_child = getSimilarity(alignment, match_type, entity1_children, entity2_children)

    val measure = if (strategy == NeighborHoodSimilarityMatcher.STRATEGY_MAX) {
      Math.max(new_measure_parent, new_measure_child)
    } else if (strategy == NeighborHoodSimilarityMatcher.STRATEGY_MIN) {
      Math.min(new_measure_parent, new_measure_child)
    } else {
      (new_measure_child + new_measure_parent) / 2
    }
    //println(s"Child Measure $entity1 $entity2 " + new_measure_child)
    //println(s"Parent Measure  $entity1 $entity2 " + new_measure_parent)

    if (measure >= threshold) {
      Option(MatchingCell(entity1.toString, entity2.toString, measure, "=", owl_type, match_type))
    } else {
      Option.empty
    }

  }


  def getSimilarity(alignment: Alignment, match_type: String, entities_1: Option[Map[IRI, Integer]], entities_2: Option[Map[IRI, Integer]]): Double = {


    if (entities_1.isEmpty || entities_2.isEmpty) {
      0.0
    } else {
      val sim = for (entity1_parent <- entities_1.get;
                     entity2_parent <- entities_2.get;
                     cell = alignment.getCorrespondence(entity1_parent._1.toString, entity2_parent._1.toString, match_type);
                     if (entity1_parent._2 > 0 && entity2_parent._2 > 0)) yield {
        val sim = if (cell.isDefined) {

          val entity_sum = (entity1_parent._2.toDouble + entity2_parent._2.toDouble)


          val measure = if (entity_sum > 0.0) cell.get.measure / Math.sqrt(entity_sum * 0.5) else 0.0
          measure
        } else {
          0.0
        }
        val entity_1_norm = if (entity1_parent._2.toDouble > 0.0) 0.5 / Math.sqrt(entity1_parent._2.toDouble) else 0.0
        val entity_2_norm = if (entity2_parent._2.toDouble > 0.0) 0.5 / Math.sqrt(entity2_parent._2.toDouble) else 0.0
        (sim, entity_1_norm, entity_2_norm)
      }

      val sim_parent_tupled = sim.unzip3
      //TODO discuss divided by 2
      val new_measure = sim_parent_tupled._1.sum / (sim_parent_tupled._2.sum + sim_parent_tupled._3.sum)
      if (new_measure > 1.0) {
        1.0
      } else {
        new_measure
      }
    }
  }

  def getDirectChildren(owl_type: String, entity: String, onto1: FastOntology): Option[Set[IRI]] = {
    val entity_iri: Option[IRI] = getIRI(entity, onto1)
    val parent_to_child_map: Map[IRI, Set[IRI]] = getParentToChildMap(owl_type, onto1)
    if (entity_iri.isDefined) {
      parent_to_child_map.get(entity_iri.get)
    } else {
      Option.empty
    }
  }


  def getDirectParents(owl_type: String, entity: String, onto1: FastOntology): Option[Set[IRI]] = {

    val entity_iri: Option[IRI] = getIRI(entity, onto1)

    val child_to_parent_Map: Map[IRI, Set[IRI]] = getChildToParentMap(owl_type, onto1)
    if (entity_iri.isDefined) {
      child_to_parent_Map.get(entity_iri.get)
    } else {
      Option.empty
    }

  }


  def getChildToParentMap(owl_type: String, onto1: FastOntology): Map[IRI, Set[IRI]] = {
    if (owl_type.equals(Cell.TYPE_CLASS)) {
      onto1.child_to_parents_classes_map
    } else if (owl_type.equals(Cell.TYPE_DT_PROPERTY)) {
      onto1.child_to_parents_data_propterties_map
    } else {
      //object property
      onto1.child_to_parents_object_propterties_map
    }
  }


  def getIRI(entity: String, onto1: FastOntology): Option[IRI] = {
    if (onto1.class_name_to_IRI.get(entity).isDefined) {
      onto1.class_name_to_IRI.get(entity)
    } else if (onto1.object_property_name_to_IRI.get(entity).isDefined) {
      onto1.object_property_name_to_IRI.get(entity)
    } else if (onto1.data_property_name_to_IRI.get(entity).isDefined) {
      onto1.data_property_name_to_IRI.get(entity)
    } else {
      Option.empty
    }
  }

  def getParentToChildMap(owl_type: String, onto1: FastOntology): Map[IRI, Set[IRI]] = {
    if (owl_type.equals(Cell.TYPE_CLASS)) {
      onto1.parent_to_child_classes_map
    } else if (owl_type.equals(Cell.TYPE_DT_PROPERTY)) {
      onto1.parent_to_child_data_properties_map
    } else {
      //object property
      onto1.parent_to_child_object_properties_map
    }
  }

  /**
   * Get alle children
   * @param parent_iri
   * @param onto
   * @param owl_type
   * @return
   */
  def getChildren(parent_iri: IRI, onto: FastOntology, owl_type: String): Option[Map[IRI, Integer]] = {

    def child_map = getParentToChildMap(owl_type, onto)
    getChildren(parent_iri, 0, child_map)
  }

  /**
   * Get alle children
   * @param child_iri
   * @param onto
   * @param owl_type
   * @return
   */
  def getParents(child_iri: IRI, onto: FastOntology, owl_type: String): Option[Map[IRI, Integer]] = {

    def parent_map = getChildToParentMap(owl_type, onto)
    getParents(child_iri, 0, parent_map)
  }

  def getChildren(parent: IRI, start_depth: Integer, children_map: Map[IRI, Set[IRI]]): Option[Map[IRI, Integer]] = {
    val children = children_map.get(parent).get

    if (children.size == 0) {
      if (start_depth == 0) {
        //don't self
        Option.empty
      } else {
        Option(Map(parent -> start_depth))
      }

    }else if(children.size == 1 && children.head.equals(parent)){
      Option.empty
    } else {

      try {
        val new_elems = children.map(new_parent => {
          getChildren(new_parent, start_depth + 1, children_map)
        }).filter(_.isDefined).map(_.get).flatten.toMap
        Option(Map(parent -> start_depth) ++ new_elems)
      }
      catch {
        case e: Throwable => {
          logger.error("error in Neighborhood Matcher", e)
          Option(Map(parent -> start_depth))
        }
      }

      //add self
    }
  }

  def getParents(child: IRI, start_depth: Integer, parents_map: Map[IRI, Set[IRI]]): Option[Map[IRI, Integer]] = {
    val parents = parents_map.get(child).get

    if (parents.size == 0) {
      if (start_depth == 0) {
        //don't self
        Option.empty
      } else {
        Option(Map(child -> start_depth))
      }
    //avoid cyclical effects
    } else if(parents.size == 1 && parents.head.equals(child)){
      Option.empty
    }else {
      try {
        val new_elems = parents.map(new_parent => {
          getParents(new_parent, start_depth + 1, parents_map)
        }).filter(_.isDefined).map(_.get).flatten.toMap

        //add self
        Option(Map(child -> start_depth) ++ new_elems)
      }
      catch {
        case e: Throwable => {
          logger.error("error in Neighborhood Matcher", e)
          Option(Map(child -> start_depth))
        }
      }


    }
  }

}

object NeighborHoodSimilarityMatcher {
  val STRATEGY_MAX = 0
  val STRATEGY_MIN = 1
}
