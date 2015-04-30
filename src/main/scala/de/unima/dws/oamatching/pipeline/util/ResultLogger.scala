package de.unima.dws.oamatching.pipeline.util

import java.io.File

import com.github.tototoshi.csv.CSVWriter
import de.unima.dws.oamatching.config.Config
import de.unima.dws.oamatching.core.{AggregatedEvaluationResult, EvaluationResult}
import org.apache.log4j.{Level, Logger, PropertyConfigurator}

object ResultLogger {
  //PropertyConfigurator.configure("log4j.properties");
  
  //text experiment logger
  val exp_logger = Logger.getLogger("EXECUTION_LOG");

  
  //logging each value
  val matcher_writer = CSVWriter.open("matching_matcher_result.csv")
  matcher_writer.writeRow(List[String]("dataset", "matcher", "precision", "recall", "f1-measure"))

  val result_writer = CSVWriter.open("meta_matcher_result.csv")
  result_writer.writeRow(List[String]("dataset", "matcher","res_type", "precision", "recall", "f1-measure"))

  /**
   *
   * @param dataset
   * @param matcher name
   * @param result
   */
  def log_matcher_result(dataset: String, matcher: String, result: EvaluationResult):Unit = {
    val entry = List[String](dataset, matcher, result.precision.toString, result.recall.toString, result.f1Measure.toString)
    matcher_writer.writeRow(entry);
    matcher_writer.flush
  }

  /**
   *
   * @param dataset
   * @param matcher
   * @param res_type macro or micro
   * @param result
   */
  def log_result(dataset: String, matcher: String, res_type:String, result: EvaluationResult):Unit = {
    val entry = List[String](dataset, matcher,res_type, result.precision.toString, result.recall.toString, result.f1Measure.toString)
    result_writer.writeRow(entry);
    result_writer.flush
  }

  /**
   *
   * @param dataset
   * @param matcher
   * @param aggregatedEvaluationResult
   */
  def log_result(dataset: String, matcher: String, aggregatedEvaluationResult: AggregatedEvaluationResult):Unit = {
        log_result(dataset,matcher,"micro",aggregatedEvaluationResult.micro_eval_res)
        log_result(dataset,matcher,"macro",aggregatedEvaluationResult.macro_eval_res)
  }

  def log(message: String) {
    exp_logger.log(Level.DEBUG, message)
  }

}