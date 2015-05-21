package de.unima.dws.oamatching.pipeline

import java.io.File

import de.dwslab.alcomox.ontology.IOntology
import de.unima.dws.oamatching.config.Config
import de.unima.dws.oamatching.core.{AggregatedEvaluationResult, Alignment, EvaluationResult, EvaluationResultAggregator}
import de.unima.dws.oamatching.matcher.MatcherRegistry
import de.unima.dws.oamatching.pipeline.evaluation.{Evaluation, EvaluationDataSetParser}

/**
 * Created by mueller on 13/04/15.
 */
object SingleMatcherPipeline extends EvaluationDataSetParser {
  MatcherRegistry.initMatchers()

  def matchSingleProblem(problem: MatchingProblem, debug: Boolean, threshold: Double, matcher_name: String): Alignment = {


    val matcher = MatcherRegistry.getMatcherByName(matcher_name).get

    val relations = matcher.align(problem, threshold).asMatchRelationMap()

    val selected = MatchingSelector.greedyRankSelectorSimple(relations, 0.7, null, null)

    val i_onto1 = new IOntology(problem.ontology1.path)
    val i_onto2 = new IOntology(problem.ontology2.path)

    val final_alignment: Alignment = new Alignment(problem.ontology1.name, problem.ontology2.name, problem.ontology1, problem.ontology2, i_onto1, i_onto2, selected)

    if (debug) {
      MatchingPruner.debugAlignment(final_alignment)
    } else {
      final_alignment
    }

  }

  def matchAndEvaluateSingle(problem: MatchingProblem, reference: Alignment, debug: Boolean, threshold: Double, matcher_name: String): EvaluationResult = {
    val alignment = matchSingleProblem(problem, debug, threshold, matcher_name)

    alignment.evaluate(reference)
  }


  def matchAllAndEvalAllBaseMatcher(problem: MatchingProblem, reference: Alignment, debug: Boolean): Map[String, EvaluationResult] = {

    MatcherRegistry.matcher_by_name.keys.par.map(name => {

      val result = name -> matchAndEvaluateSingle(problem, reference, debug, 0.8, name)

      result
    }).toMap.seq
  }

  def matchAllBaseMatcherForMJ(problem: MatchingProblem, reference: Alignment, debug: Boolean): EvaluationResult = {


    val tester_file = new File("matchings/" + problem.data_set_name + "/matchings/" + problem.name + "_raw_matchings" + ".csv")


    val feature_vector: FeatureVector = if (tester_file.exists()) {
      VectorUtil.readFromMatchingFile(tester_file, problem.name)
    } else {
      MatchingPipelineCore.matchAllIndividualMatchers(problem)
    }
    val alignment = Evaluation.calcMajorityVoteAlignment(feature_vector, reference)

    val post_alignment = if (debug) {
      MatchingPruner.debugAlignment(alignment)
    } else {
      alignment
    }

    val result = post_alignment.evaluate(reference)

    println(result)
    result
  }


  def calcMJVoteBaseLine(): AggregatedEvaluationResult = {
    val matchingProblems = parseProblems(Config.loaded_config.getString("pipeline.dataset_type"), Config.loaded_config.getString("pipeline.path_to_dataset"))

    val result = matchingProblems.map(problem => {
      matchAllBaseMatcherForMJ(problem.matching_problem, problem.reference, Config.loaded_config.getBoolean("pipeline.debug_alignment"))
    }).toList

    val agg_res = EvaluationResultAggregator.aggregateEvaluationResults(result)

    println(agg_res)
    agg_res
  }


  def calcBestAndAverageBestBaseLine(): AggregatedEvaluationResult = {
    val matchingProblems = parseProblems(Config.loaded_config.getString("pipeline.dataset_type"), Config.loaded_config.getString("pipeline.path_to_dataset"))
    val results = matchingProblems.map(problem => {
      println("start problem " + problem.matching_problem.name)
      val base_results: Map[String, EvaluationResult] = matchAllAndEvalAllBaseMatcher(problem.matching_problem, problem.reference, Config.loaded_config.getBoolean("pipeline.debug_alignment"))

      val best_base = base_results.maxBy(_._2.f1Measure)

      (base_results, best_base)
    }).unzip



    val base_results: Seq[Map[String, EvaluationResult]] = results._1

    val matcher_names = base_results.head.keySet

    val results_per_matcher: Map[String,AggregatedEvaluationResult] = matcher_names.map(matcher_name => {
      val base_results_un_agg = base_results.map(results => {
        results.get(matcher_name)
      }).filter(_.isDefined).map(_.get).toList
      matcher_name -> EvaluationResultAggregator.aggregateEvaluationResults(base_results_un_agg)
    }).toMap


    println(results_per_matcher)
    println("best Average matcher")
    val best_avg_matcher: (String, AggregatedEvaluationResult) = results_per_matcher.maxBy(_._2.macro_eval_res.f1Measure)

    println(best_avg_matcher)


    val best_base_results = results._2


    println("Best matcher")
    best_base_results.foreach(tuple => println(tuple._1))

    val best_baseline = EvaluationResultAggregator.aggregateEvaluationResults(best_base_results.map(_._2).toList)

    println(best_baseline)
    best_baseline
  }

}
