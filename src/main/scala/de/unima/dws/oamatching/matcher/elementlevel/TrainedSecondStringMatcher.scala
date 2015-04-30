package de.unima.dws.oamatching.matcher.elementlevel

import com.wcohen.ss.api.StringWrapper
import com.wcohen.ss.{AbstractTokenizedStringDistance, BasicStringWrapperIterator}
import de.unima.dws.oamatching.core.{FastOntology, Alignment}
import de.unima.dws.oamatching.core.matcher.ExtractedFields
import org.semanticweb.owlapi.model.{OWLEntity, OWLOntology}

import scala.collection.JavaConversions._
import scala.collection.immutable.List

/**
 *
 * Class that performs a matching based on second string techniques
 *
 */
class TrainedSecondStringMatcher(override val similarity: Boolean,
                                 override val useLabel: Boolean,
                                 override val useFragment: Boolean,
                                 override val useComment: Boolean,
                                 override val preprocess_function: (String) => String,
                                 val distance: AbstractTokenizedStringDistance) extends TrainedMatcher(similarity, useLabel, useFragment, useComment, preprocess_function) {


  /**
   * Performs Training for further matching
   * @param ontology1
   * @param ontology2

   */
  override def init(ontology1: FastOntology, ontology2: FastOntology): Unit = {

    val labels: List[StringWrapper] = ontology1.base_values.classes.map(entity=> distance.prepare(preprocess_function(flattenExtractedFields(ontology1.classes_to_names.get(entity).get)).toLowerCase)).toList :::
      ontology2.base_values.classes.map(entity=> distance.prepare(preprocess_function(flattenExtractedFields(ontology2.classes_to_names.get(entity).get)).toLowerCase)).toList :::
      ontology1.base_values.data_properties.map(entity=> distance.prepare(preprocess_function(flattenExtractedFields(ontology1.data_properties_to_names.get(entity).get)).toLowerCase)).toList :::
      ontology2.base_values.data_properties.map(entity=> distance.prepare(preprocess_function(flattenExtractedFields(ontology2.data_properties_to_names.get(entity).get)).toLowerCase)).toList :::
      ontology1.base_values.object_properties.map(entity=> distance.prepare(preprocess_function(flattenExtractedFields(ontology1.object_properties_to_names.get(entity).get)).toLowerCase)).toList :::
      ontology2.base_values.object_properties.map(entity=> distance.prepare(preprocess_function(flattenExtractedFields(ontology2.object_properties_to_names.get(entity).get)).toLowerCase)).toList

    // transform to java
    val j_iter: java.util.Iterator[StringWrapper] = labels.iterator

    distance.train(new BasicStringWrapperIterator(j_iter))
  }

  def flattenExtractedFields(fields: ExtractedFields): String = {
    val flattened = fields.fragment.getOrElse("") + " " + fields.label.getOrElse("") + " " + fields.comment.getOrElse("")

    flattened.replace("  ", " ").trim
  }



  override def pre_processed_score(entity1: String, entity2: String): Double = {
    val score = distance.score(entity1, entity2)
    score
  }
  /**
   * Override implementation to include training phase trained element-wise element-wise ontology matcher
   * @param onto1
   * @param onto2
   * @param threshold
   * @return
   */
  override def align(onto1: FastOntology, onto2: FastOntology, threshold: Double): Alignment = {

    //get elements of the ontology
    init(onto1, onto2)

    super.align(onto1, onto2, threshold)
  }
}
