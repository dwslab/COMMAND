package de.unima.dws.oamatching.pipeline

import de.unima.dws.algorithms.MWBMatchingAlgorithm
import de.unima.dws.oamatching.core.{Alignment, FastOntology, MatchRelation}

import scala.collection.mutable
import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}

/**
 * Created by mueller on 23/01/15.
 */

case class HungarianMethodProblem(problem: MWBMatchingAlgorithm, left_map: Map[String, Int], right_map: Map[String, Int])

object MatchingSelector {

  def thresholdingOnly(fuzyy: Double)(raw_matchings: Map[MatchRelation, Double], threshold: Double, sourceOnto: FastOntology, targetOnto: FastOntology): Map[MatchRelation, Double] = {
    raw_matchings.filter(tuple => tuple._2 >= threshold)
  }

  def debuggingBasedOneToOneSelector(fuzyy: Double)(raw_matchings: Map[MatchRelation, Double], threshold: Double, sourceOnto: FastOntology, targetOnto: FastOntology): Map[MatchRelation, Double] = {
    val matchings: Map[MatchRelation, Double] = raw_matchings.filter(tuple => tuple._2 >= threshold)

    val result = MatchingPruner.debugMatchRelations(matchings, sourceOnto, targetOnto)

    result
  }


  def hungarianMethodSelection(fuzzy: Double)(raw_matchings: Map[MatchRelation, Double], threshold: Double, sourceOnto: FastOntology, targetOnto: FastOntology): Map[MatchRelation, Double] = {
    val matchings: Map[MatchRelation, Double] = raw_matchings.filter(tuple => tuple._2 >= threshold)

    val max_matchings = getMaxPerMatchType(matchings)

    val problem = prepareHungarianMethodAlgorithm(max_matchings)

    val result = solveHungarianMethodProblem(problem, max_matchings)

    result

  }

  def greedyRankSelectorSimpleDelta(fuzzy: Double)(raw_matchings: Map[MatchRelation, Double], threshold: Double, sourceOnto: FastOntology, targetOnto: FastOntology): Map[MatchRelation, Double] = {
    val matchings: Map[MatchRelation, Double] = raw_matchings.filter(tuple => tuple._2 >= threshold)

    val sorted_matchings = matchings.toList.sortWith(_._2 > _._2)

    //sorted_matchings.foreach(println _)

    var already_contained_left: MutableSet[String] = MutableSet[String]()
    var already_contained_right: MutableSet[String] = MutableSet[String]()

    //iterate over them

    val selected_matchings_raw: List[Option[(MatchRelation, Double)]] = for (matching <- sorted_matchings) yield {
      if ((!already_contained_left.contains(matching._1.left)) && (!already_contained_right.contains(matching._1.right))) {

        already_contained_left.add(matching._1.left)
        already_contained_right.add(matching._1.right)
        Option(matching)
      } else {
        Option.empty
      }
    }

    selected_matchings_raw.filter(_.isDefined).map(_.get).map { case (relation, score) => {
      val new_relation: MatchRelation = MatchRelation(relation.left, relation.relation, relation.right, relation.owl_type, Alignment.TYPE_NONE)
      (new_relation, score)
    }
    }.toMap
  }

  def greedyRankSelectorSimple = greedyRankSelectorSimpleDelta(1.0) _

  def hungarianMethodSimple = hungarianMethodSelection(1.0) _

  def greedyRankSelectorSimpleExp: (Double) => (Map[MatchRelation, Double], Double, FastOntology, FastOntology) => Map[MatchRelation, Double] = greedyRankSelectorSimpleDelta _

  def fuzzyGreedyRankSelectorRatio: (Double) => (Map[MatchRelation, Double], Double, FastOntology, FastOntology) => Map[MatchRelation, Double] = fuzzyGreedyRankSelector(selectFuzzySingleRatio) _

  def fuzzyGreedyRankSelectorDelta: (Double) => (Map[MatchRelation, Double], Double, FastOntology, FastOntology) => Map[MatchRelation, Double] = fuzzyGreedyRankSelector(selectFuzzySingleDelta) _


  def fuzzyGreedyRankSelector(select_fct: (Double, Double, (MatchRelation, Double), FastOntology, FastOntology) => Option[(MatchRelation, Double)])(fuzzy_value: Double)(raw_matchings: Map[MatchRelation, Double], threshold: Double, sourceOnto: FastOntology, targetOnto: FastOntology): Map[MatchRelation, Double] = {
    val matchings: Map[MatchRelation, Double] = raw_matchings.filter(tuple => tuple._2 >= threshold)

    val sorted_matchings = matchings.toList.sortWith(_._2 > _._2)

    //sorted_matchings.foreach(println _)

    var already_contained_left: MutableSet[String] = MutableSet[String]()
    var already_contained_right: MutableSet[String] = MutableSet[String]()

    var already_contained_left_threshold: MutableMap[String, Double] = new mutable.HashMap[String, Double]()
    var already_contained_right_threshold: MutableMap[String, Double] = new mutable.HashMap[String, Double]()

    //iterate over them
    val selected_matchings_raw: List[Option[(MatchRelation, Double)]] =
      for (matching <- sorted_matchings) yield {
        //case when not present
        if ((!already_contained_left_threshold.contains(matching._1.left)) && (!already_contained_right_threshold.contains(matching._1.right))) {

          already_contained_left_threshold.put(matching._1.left, matching._2)
          already_contained_right_threshold.put(matching._1.right, matching._2)
          Option(matching)
        } else {
          //case one left is already in selected -> right not
          if (already_contained_left_threshold.contains(matching._1.left) && (!already_contained_right_threshold.contains(matching._1.right))) {
            val already_contained_sim_value: Double = already_contained_left_threshold.get(matching._1.left).getOrElse(0.0)

            select_fct(fuzzy_value, already_contained_sim_value, matching, sourceOnto, targetOnto)

            //case two right is already in selected -> left not
          } else if (already_contained_right_threshold.contains(matching._1.right) && (!already_contained_left_threshold.contains(matching._1.left))) {
            val already_contained_sim_value: Double = already_contained_right_threshold.get(matching._1.right).getOrElse(0.0)

            select_fct(fuzzy_value, already_contained_sim_value, matching, sourceOnto, targetOnto)

            //both are in -> return option empty
          } else {
            Option.empty
          }
        }
      }

    selected_matchings_raw.filter(_.isDefined).map(_.get).map { case (relation, score) => {
      val new_relation: MatchRelation = MatchRelation(relation.left, relation.relation, relation.right, relation.owl_type, relation.match_type)
      (new_relation, score)
    }
    }.toMap
  }


  /**
   * Function to define wether or not to select an element already in the selected matchings should be considered
   * @param ratio_threshold
   * @param matching
   * @return
   */
  def selectFuzzySingleRatio(ratio_threshold: Double, already_contained_sim_value: Double, matching: (MatchRelation, Double), sourceOnto: FastOntology, targetOnto: FastOntology): Option[(MatchRelation, Double)] = {
    val ratio = already_contained_sim_value / matching._2
    // E.g left has already a relation with 1.0 similarity and now a second comes with 0.95 => ratio = 1.0/0.95= 1.0526315789
    if (ratio < ratio_threshold) {
      //take it
      Option(matching)
    } else {
      //leave it
      Option.empty
    }
  }

  def selectFuzzySingleDelta(delta_threshold: Double, already_contained_sim_value: Double, matching: (MatchRelation, Double), sourceOnto: FastOntology, targetOnto: FastOntology): Option[(MatchRelation, Double)] = {

    val delta = Math.abs(already_contained_sim_value - matching._2)
    // E.g left has already a relation with 1.0 similarity and now a second comes with 0.95 => delta = 1.0-0.95= 0.05
    if (delta <= delta_threshold) {
      //take it
      Option(matching)
    } else {
      //leave it
      Option.empty
    }
  }

  /**
   *
   * @param matchings
   * @return
   */
  def getIndexEncodedListofClasses(matchings: Map[MatchRelation, Double]): (Map[String, Int], Map[String, Int]) = {
    val left_map: Map[String, Int] = matchings.map(_._1.left).toSet.zipWithIndex.toMap
    val right_map: Map[String, Int] = matchings.map(_._1.right).toSet.zipWithIndex.toMap

    //map to owl_type

    (left_map, right_map)
  }

  /**
   *
   * @param matchings
   * @return
   */
  def prepareHungarianMethodAlgorithm(matchings: Map[MatchRelation, Double]): HungarianMethodProblem = {
    val encoded_tuple: (Map[String, Int], Map[String, Int]) = MatchingSelector.getIndexEncodedListofClasses(matchings)
    val encoded_left = encoded_tuple._1
    val encoded_right = encoded_tuple._2


    val algo = new MWBMatchingAlgorithm(encoded_left.size, encoded_right.size)


    matchings.foreach { case (relation, value) => {
      val left = encoded_left.get(relation.left).get
      val right = encoded_right.get(relation.right).get

      algo.setWeight(left, right, value + 1.0)
    }
    }

    HungarianMethodProblem(algo, encoded_left, encoded_right)
  }

  /**
   *
   * @param problem
   * @param matchings
   * @return
   */
  def solveHungarianMethodProblem(problem: HungarianMethodProblem, matchings: Map[MatchRelation, Double]): Map[MatchRelation, Double] = {

    val matching_array = problem.problem.getMatching.zipWithIndex

    val left_index_to_String: Map[Int, String] = problem.left_map.map(tuple => tuple._2 -> tuple._1).toMap
    val right_index_to_String: Map[Int, String] = problem.right_map.map(tuple => tuple._2 -> tuple._1).toMap

    //get matchings
    val matchings_result = matching_array.map { case (right_index, left_index) => {

      if (right_index == -1) {
        Option.empty
      } else {
        val right = right_index_to_String.get(right_index).get
        val left = left_index_to_String.get(left_index).get
        Option((left, right))
      }
    }
    }.toList.filter(_.isDefined).map(_.get)

    val result: Map[MatchRelation, Double] = matchings.filter { case (relation, value) => {
      matchings_result.contains((relation.left, relation.right))
    }
    }
    result
  }

  def getMaxPerMatchType(matchings: Map[MatchRelation, Double]): Map[MatchRelation, Double] = {
    val grouped_matchings: Map[String, Map[MatchRelation, Double]] = matchings.groupBy { case (relation, measure) => {
      relation.left + "-" + relation.right
    }
    }

    grouped_matchings.map { case (key, grouped) => {
      grouped.maxBy(_._2)
    }
    }
  }


}