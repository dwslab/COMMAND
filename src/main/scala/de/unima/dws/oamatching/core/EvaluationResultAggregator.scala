package de.unima.dws.oamatching.core

/**
 * Created by mueller on 22/01/15.
 */

case class EvaluationResult(precision: Double, recall: Double, f1Measure: Double, truePositives: Int, falsePositives: Int, FalseNegatives: Int, problem:String, f2measure:Double);

case class AggregatedEvaluationResult(micro_eval_res: EvaluationResult, macro_eval_res: EvaluationResult)

object EvaluationResultAggregator {

  /**
   * Return and EvaluationResult Object based on tp,fp,fn
   * @param truePositives
   * @param falsePositives
   * @param falseNegatives
   * @return
   */
  def createEvaluationResult(truePositives: Int, falsePositives: Int, falseNegatives: Int, problem:String): EvaluationResult = {
    val precision: Double = if (truePositives + falsePositives > 0) {
      (truePositives).toDouble / (truePositives + falsePositives).toDouble
    } else {
      0.0
    }


    val recall = if (truePositives + falseNegatives > 0) {
      (truePositives).toDouble / (truePositives + falseNegatives).toDouble
    } else {
      0.0
    }

    val fMeasure: Double = if (precision + recall > 0.0) {
      (2 * ((precision * recall) / (precision + recall)))
    } else {
      0.0
    }



    val f2Measure: Double = if (precision + recall > 0.0) {
      (5 * ((precision * recall) / (4*precision + recall)))
    } else {
      0.0
    }


    EvaluationResult(precision, recall, fMeasure, truePositives, falsePositives, falseNegatives,problem,f2Measure)
  }

  /**
   * Aggregate Results in Micro and Macro average way
   * @param evaluation_results
   * @return
   */
  def aggregateEvaluationResults(evaluation_results: List[EvaluationResult]): AggregatedEvaluationResult = {

    val tupled_eval_res = {
      evaluation_results.map(eval_res => EvaluationResult.unapply(eval_res).get)
    }
    val no_of_result = evaluation_results.size
    val sum = EvaluationResult.tupled(tupled_eval_res.reduceLeft[(Double, Double, Double, Int, Int, Int,String,Double)] { case (previous, tuples) => EvaluationResult.unapply(EvaluationResult(previous._1 + tuples._1, previous._2 + tuples._2, previous._3 + tuples._3, previous._4 + tuples._4, previous._5 + tuples._5, previous._6 + tuples._6,"agg",previous._8+tuples._8)).get})


    val macro_precision = sum.precision / no_of_result.toDouble
    val macro_recall = sum.recall / no_of_result.toDouble
    val macro_fMeasure: Double = if((macro_precision + macro_precision) > 0){
      (2 * ((macro_precision * macro_recall) / (macro_precision + macro_recall)))
    }else {
      0.0
    }

    val macro_f2Measure: Double = if((macro_precision + macro_precision) > 0){
      (5 * ((macro_precision * macro_recall) / (4*macro_precision + macro_recall)))
    }else {
      0.0
    }

    val macro_average = EvaluationResult(sum.precision / no_of_result.toDouble,
      sum.recall / no_of_result.toDouble,
      macro_fMeasure,
      sum.truePositives,
      sum.falsePositives,
      sum.FalseNegatives,"agg",macro_f2Measure)

    //micro avg calc
    //calculate with check for 0 in denominator
    val precision: Double = if( (sum.truePositives + sum.falsePositives) > 0){
      (sum.truePositives).toDouble / (sum.truePositives + sum.falsePositives).toDouble
    }else {
      0.0
    }


    val recall=  if(  (sum.truePositives + sum.FalseNegatives) > 0){
      (sum.truePositives).toDouble / (sum.truePositives + sum.FalseNegatives).toDouble
    }else {
      0.0
    }

    //calculate with check for 0 in denominator
    val fMeasure: Double = if((precision + recall) > 0){
      (2 * ((precision * recall) / (precision + recall)))
    }else {
      0.0
    }


    val f2Measure: Double = if (precision + recall > 0.0) {
      (5 * ((precision * recall) / (4*precision + recall)))
    } else {
      0.0
    }



    val micro_avg = EvaluationResult(precision, recall, fMeasure, sum.truePositives, sum.falsePositives, sum.FalseNegatives,"agg",f2Measure)

    AggregatedEvaluationResult(micro_avg, macro_average)

  }

  /**
   * Aggregate but with optionated data
   * @param evaluation_results
   * @return
   */
  def aggregateEvaluationResultsOption(evaluation_results: List[Option[EvaluationResult]]): AggregatedEvaluationResult = {
    //get results tupled
    val filtered_eval_res = evaluation_results.filter(eval_res => eval_res.isDefined).map(elm => elm.get);

    aggregateEvaluationResults(filtered_eval_res)

  }

}
