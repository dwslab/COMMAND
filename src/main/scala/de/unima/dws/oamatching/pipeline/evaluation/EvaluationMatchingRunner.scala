package de.unima.dws.oamatching.pipeline.evaluation

import de.unima.dws.oamatching.config.Config
import de.unima.dws.oamatching.core._
import de.unima.dws.oamatching.matcher.MatcherRegistry
import de.unima.dws.oamatching.pipeline._
import de.unima.dws.oamatching.pipeline.util.ResultLogger


/**
 * Created by mueller on 28/01/15.
 */

case class EvaluationMatchingTaskWithParameters(matching_problem: MatchingProblem, config: RunConfiguration, reference: Alignment)

case class EvaluationMatchingTask(matching_problem: MatchingProblem, reference: Alignment)


object EvaluationMatchingRunner extends EvaluationDataSetParser {


  def matchAndEvaluateOAEI(path_to_prob: String,problem:String, config: RunConfiguration): Unit = {
      matchAndEvaluateOnlyCore(parseProblems(problem,path_to_prob), config,problem)
  }

  def matchAndEvaluateOnlyCore(matchingTasks: Seq[EvaluationMatchingTask], config: RunConfiguration, run_name: String) = {
    val matching_tasks_with_parameters = matchingTasks.map(task => EvaluationMatchingTaskWithParameters(task.matching_problem, config, task.reference))

    val matching_task_results: List[EvaluationResult] = matching_tasks_with_parameters.map(task => {
      matchAndEvaluateCorePlatformOnly(task)
    }).toList


    val average_core_result = computeAggregatedResults(matching_task_results)
    ResultLogger.log_result(run_name, "average_core_pipleine", average_core_result);
    println(average_core_result)
  }


  /**
   * TODO Maybe make it composable
   * @param eval_task
   * @return
   */
  def matchAndEvaluateSingle(eval_task: EvaluationMatchingTaskWithParameters): EvaluationRoundResult = {
    Evaluation.evaluate(eval_task.matching_problem, eval_task.reference, eval_task.config)
  }

  def matchAndEvaluateCorePlatformOnly(eval_task: EvaluationMatchingTaskWithParameters): EvaluationResult = {
    Evaluation.evaluateOnlyCore(eval_task.matching_problem, eval_task.reference, eval_task.config)
  }

  def computeAggregatedResults(eval_results: List[EvaluationResult]): AggregatedEvaluationResult = {
    EvaluationResultAggregator.aggregateEvaluationResults(eval_results)
  }

  /**
   *
   * @param base_matcher_results
   * @return
   */
  def computeAggregatedResultsForAllBaseMatchers(base_matcher_results: List[Map[String, EvaluationResult]]): Map[String, AggregatedEvaluationResult] = {

    //get eval results by matcher
    val unique_elements: List[String] = base_matcher_results.map(eval_res_map => eval_res_map.keys).flatten.distinct.filterNot(_.contains("structural"))


    println(unique_elements)

    val result_per_matcher: List[(String, List[Option[EvaluationResult]])] = unique_elements.map(matcher => (matcher, base_matcher_results.map { round_result => round_result.get(matcher)}))


    val max_size_of_matchings = result_per_matcher.maxBy(tuple => {
      tuple._2.filter(_.isDefined).size
    })._2.filter(_.isDefined).size


    val filtered_results_per_matcher = result_per_matcher.filter(tuple => {
      tuple._2.filter(_.isDefined).size >= max_size_of_matchings
    })


    // filter results that were used in every matcher

    //now aggregate for each matcher
    val aggregated_result_per_matcher: Map[String, AggregatedEvaluationResult] = filtered_results_per_matcher.map { case (matcher_name, eval_results) => (matcher_name, EvaluationResultAggregator.aggregateEvaluationResultsOption(eval_results))}.toMap

    aggregated_result_per_matcher
  }

}
