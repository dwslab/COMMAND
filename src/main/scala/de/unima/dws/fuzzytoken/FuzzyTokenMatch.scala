package de.unima.dws.fuzzytoken

import scala.collection.immutable.Iterable
import scala.collection.mutable

/**
 * Created by mueller on 18/03/15.
 */
abstract class FuzzyTokenMatch(tokenizer: String => List[String], string_matching: (String, String) => Double) {
  def computeOverlap(a: String, b: String, threshold: Double): Double = {
    computeTokenized(tokenizer(a), tokenizer(b), threshold)
  }

  protected def computeTokenized(a: List[String], b: List[String], threshold: Double): Double = {
    val bi_graph = createBiPariteGraph(a, b, threshold)
    if (bi_graph.size > 0) {
      val max_graph = extractMaximumWeightBiPariteGraph(bi_graph)
      val max_sum = computeWeight(max_graph)
      computeSimilarity(max_sum, a.size.toDouble, b.size.toDouble)
    } else {
      0.0
    }

  }

  protected def computeSimilarity(card_graph: Double, card_a: Double, card_b: Double): Double

  /**
   *
   * @param a
   * @param b
   * @param threshold
   * @return
   */
  protected def createBiPariteGraph(a: List[String], b: List[String], threshold: Double): Map[String, Map[String, Double]] = {
    val biparite_graph = a.map(token_a => {
      val inner_map: Map[String, Double] = b.map(token_b => {
        val score = string_matching(token_a.toLowerCase(), token_b.toLowerCase())
        if (score > threshold) {
          Option(token_b -> score)
        } else {
          Option.empty
        }
      }).filter(_.isDefined).map(_.get).toMap
      token_a -> inner_map
    }).toMap

    biparite_graph
  }

  private def extractMaximumWeightBiPariteGraph(graph: Map[String, Map[String, Double]]): Map[String, (String, Double)] = {
    val candidates_and_used_rhs = generateCandidatesForMaximumBiGraph(graph)

    val candidates = candidates_and_used_rhs._1
    val used_rhs = candidates_and_used_rhs._2

    pruneIteratively(graph, candidates, used_rhs)

  }

  private def computeWeight(maxGraph: Map[String, (String, Double)]): Double = {
    val weight: Double = maxGraph.map(_._2._2).sum

    weight
  }

  private def pruneIteratively(graph: Map[String, Map[String, Double]], candidates: Map[String, (String, Double)], used: mutable.MutableList[String]): Map[String, (String, Double)] = {

    var weight = 0.0
    var delta = 0.1
    var i = 0
    var candidates_new = candidates
    var resolved_conflicts = (Map[String, (String, Double)](), mutable.MutableList[String]())
    //repeat until convergence
    while (delta > 0.01) {
      val conflicts: Map[String, Option[Iterable[(String, (String, Double))]]] = detectConflicts(candidates_new, used)
      resolved_conflicts = resolveConflicts(graph, used, conflicts)
      val weight_new = computeWeight(resolved_conflicts._1)
      delta = weight_new - weight
      weight = weight_new
      candidates_new = resolved_conflicts._1
      i = i + 1
    }

    resolved_conflicts._1
  }

  private def detectConflicts(candidates: Map[String, (String, Double)], used: mutable.MutableList[String]): Map[String, Option[Iterable[(String, (String, Double))]]] = {
    val conflicts_by_rhs: Map[String, Option[Iterable[(String, (String, Double))]]] = used.distinct.map(rhs => {
      val matchings_with_rhs = candidates.map(tuple => {
        //get rhs
        val rhs_cand = tuple._2._1

        if (rhs.equals(rhs_cand)) {
          Option(tuple)
        } else {
          Option.empty
        }
      }).filter(_.isDefined).map(_.get)

      rhs -> {
        if (matchings_with_rhs.size > 1) {
          Option(matchings_with_rhs)
        } else {
          Option.empty
        }
      }

    }).toMap

    conflicts_by_rhs
  }


  /**
   * Tries to resolve the conflict
   * @param candidates
   * @param used
   * @param conflicts
   * @return
   */
  private def resolveConflicts(candidates: Map[String, Map[String, Double]], used: mutable.MutableList[String], conflicts: Map[String, Option[Iterable[(String, (String, Double))]]]): (Map[String, (String, Double)], mutable.MutableList[String]) = {

    //pick highest of conflicts
    val resolved_conflicts: Map[String, (String, Double)] = conflicts.map(conflict_by_rhs => {
      if (conflict_by_rhs._2.isDefined) {
        //here is  conflict that needs to be resolved
        val conflict = conflict_by_rhs._2.get
        Option(conflict.maxBy(_._2._2))
      } else {
        Option.empty
      }
    }).filter(_.isDefined).map(_.get).toMap


    val already_used_rhs_in_conflicts = resolved_conflicts.map(_._2._1).toSet
    //extract lhs from resolved conflicts

    val already_used_lhs: Set[String] = resolved_conflicts.map(_._1).toSet

    val not_used_keys = candidates.filterKeys(key => !already_used_lhs.contains(key)).keySet

    //add non conflict matchings
    val non_conflict_matchings: Map[String, (String, Double)] = not_used_keys.map(key => {
      if (candidates.get(key).get.size > 0) {
        val filtered = candidates.get(key).get.filterNot(rhs => already_used_rhs_in_conflicts.contains(rhs._1))
        if (filtered.size > 0) {
          Option(key -> filtered.maxBy(_._2))
        } else {
          Option.empty
        }

      } else {
        Option.empty
      }

    }).filter(_.isDefined).map(_.get).toMap


    //merge both
    val result_map = non_conflict_matchings ++ resolved_conflicts

    //create rhs used list
    val used_rhs_list = new mutable.MutableList[String]()
    result_map.foreach(tuple => {
      used_rhs_list.+=(tuple._2._1)
    })

    (result_map, used_rhs_list)
  }


  /**
   * This method generates a candidate set for the futher processing that will be pruned afterwards
   * @param graph
   * @return
   */
  private def generateCandidatesForMaximumBiGraph(graph: Map[String, Map[String, Double]]): (Map[String, (String, Double)], mutable.MutableList[String]) = {

    //get candidate Set -> simply for each lhs edge the max of it's lhs side
    val candidate_graph = graph.map(lhs_to_rhs => {
      if (lhs_to_rhs._2.size > 0) {
        Option(lhs_to_rhs._1 -> lhs_to_rhs._2.maxBy(_._2))
      } else {
        Option.empty
      }

    }).filter(_.isDefined).map(_.get).toMap

    //create growable set of used rhs ex
    val use_rhs_list = new mutable.MutableList[String]()
    candidate_graph.foreach(lhs_to_max => {
      use_rhs_list.+=(lhs_to_max._2._1)
    })


    (candidate_graph, use_rhs_list)
  }


}
