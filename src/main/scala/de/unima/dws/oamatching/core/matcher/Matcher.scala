package de.unima.dws.oamatching.core.matcher

import de.unima.dws.oamatching.config.Config
import de.unima.dws.oamatching.core.{FastOntology, Alignment}
import de.unima.dws.oamatching.pipeline.MatchingProblem
import org.semanticweb.owlapi.model.{OWLLiteral, OWLEntity, OWLOntology}
import scala.collection.JavaConversions._

/**
 * Created by mueller on 22/01/15.
 */
abstract class Matcher {

  protected def align( onto1:FastOntology,  onto2:FastOntology,threshold:Double) :Alignment

  def align(problem:MatchingProblem, threshold:Double):Alignment = {
    align(problem.ontology1,problem.ontology2,threshold)
  }

}
