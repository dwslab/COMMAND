package de.unima.dws.oamatching.pipeline.registry

import de.unima.dws.oamatching.core.{FastOntology, MatchRelation}
import de.unima.dws.oamatching.pipeline.MatchingSelector

import scala.collection.immutable.Map

/**
 * Created by mueller on 21/04/15.
 */
object SelectionRegistry {
  val SELECTION_METHODS_BY_NAME: Map[String, (Double) => (Map[MatchRelation, Double], Double, FastOntology, FastOntology) => Map[MatchRelation, Double]] = Map("greedy_rank" -> MatchingSelector.greedyRankSelectorSimpleExp,
    "threshold" -> MatchingSelector.thresholdingOnly,
    "greedy_rank_fuzzy_delta" -> MatchingSelector.fuzzyGreedyRankSelectorDelta,
    "greedy_rank_fuzzy_ratio" -> MatchingSelector.fuzzyGreedyRankSelectorDelta,
    "hungarian" -> MatchingSelector.hungarianMethodSelection,
    "debugging_based" -> MatchingSelector.debuggingBasedOneToOneSelector)

  /**
   * Returns a configured selection function
   * @param fuzzy
   * @param name
   * @return
   */
  def configureSelectionMethod(fuzzy: Double, name: String): (Map[MatchRelation, Double], Double, FastOntology, FastOntology) => Map[MatchRelation, Double] = {

    def method = SELECTION_METHODS_BY_NAME.getOrElse(name, MatchingSelector.thresholdingOnly _)
    method(fuzzy)
  }
}
