package de.unima.dws.oamatching.matcher.elementlevel

import de.unima.dws.oamatching.core.matcher.ElementLevelMatcher
import org.semanticweb.owlapi.model.{OWLOntology, OWLClass, OWLProperty}

/**
 * Created by mueller on 21/01/15.
 */
class SimpleStringFunctionMatcher(override val similarity:Boolean,
                                  override val useLabel: Boolean,
                                  override val useFragment: Boolean,
                                  override val useComment: Boolean,
                                  val matching_function:(String,String) => Double)  extends ElementLevelMatcher(similarity,useLabel,useFragment,useComment) {


  override  def score(entity1:String,entity2:String):Double = {
    matching_function(entity1,entity2)
  }


}
