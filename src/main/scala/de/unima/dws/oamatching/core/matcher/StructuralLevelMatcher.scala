package de.unima.dws.oamatching.core.matcher

import de.unima.dws.oamatching.core.{FastOntology, Alignment}
import de.unima.dws.oamatching.pipeline.MatchingProblem
import org.semanticweb.owlapi.model.OWLOntology

/**
 * Created by mueller on 23/01/15.
 */
abstract class StructuralLevelMatcher extends Matcher{

  protected def align(onto1:FastOntology,  onto2:FastOntology,initial_Alignment:Alignment,threshold:Double) :Alignment

  def align(problem: MatchingProblem, initial_Alignment:Alignment, threshold: Double): Alignment = {
    align(problem.ontology1,problem.ontology2,initial_Alignment,threshold)
  }

   override def align(problem: MatchingProblem, threshold: Double): Alignment = {
    align(problem,null,threshold)
  }

  override protected def align(onto1: FastOntology, onto2: FastOntology, threshold: Double): Alignment ={
    align(onto1,onto2,null,threshold)
  }


}
