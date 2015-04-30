package de.unima.dws.oamatching.matcher.elementlevel

import de.unima.dws.oamatching.core.MatchingCell
import de.unima.dws.oamatching.core.matcher.ElementLevelMatcher
import org.semanticweb.owlapi.model.{OWLClass, OWLEntity, OWLOntology, OWLProperty}

/**
 * Created by mueller on 22/01/15.
 */
abstract class PreProcessedMatcher(override val similarity: Boolean,
                                   override val useLabel: Boolean,
                                   override val useFragment: Boolean,
                                   override val useComment: Boolean,
                                   val preprocess_function: (String) => String) extends ElementLevelMatcher(similarity, useLabel, useFragment, useComment) {


  def pre_processed_score(entity1: String, entity2: String): Double

  override  def score(entity1:String, entity2:String):Double = {
    pre_processed_score(preprocess_function(entity1), preprocess_function(entity2))
  }


}
