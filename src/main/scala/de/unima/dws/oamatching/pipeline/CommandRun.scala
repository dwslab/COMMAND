package de.unima.dws.oamatching.pipeline

import java.io.File

import com.typesafe.scalalogging.slf4j.LazyLogging
import de.unima.alcomox.ontology.IOntology
import de.unima.dws.oamatching.analysis.RapidminerJobs
import de.unima.dws.oamatching.config.Config
import de.unima.dws.oamatching.core._
import de.unima.dws.oamatching.matcher.MatcherRegistry
import de.unima.dws.oamatching.pipeline.evaluation.EvaluationMatchingRunner
import de.unima.dws.oamatching.pipeline.registry.{ScoreNormalizationRegistry, OutlierRegistry}
import de.unima.dws.oamatching.pipeline.util.TimeTaker

import scala.collection.JavaConversions._

/**
 * Created by mueller on 28/01/15.
 */
case class RunConfiguration(threshold: Double, class_threshold: Double, dp_threshold: Double, op_threshold: Double, normalization: String, data_set_name: String, path_to_dataset: String, separated: Boolean, matching_pipline: (MatchingProblem, Double, Double) => (Alignment, FeatureVector), matching_pipline_separated: (MatchingProblem, Double, Double, Double, Double) => (Alignment, FeatureVector))

case class RunConfigurationOneProblem(threshold: Double, class_threshold: Double, dp_threshold: Double, op_threshold: Double, normalization: String, matchingProblem: MatchingProblem, evaluate: Boolean, reference: Option[Alignment], separated: Boolean, matching_pipline: (MatchingProblem, Double, Double) => (Alignment, FeatureVector), matching_pipline_separated: (MatchingProblem, Double, Double, Double, Double) => (Alignment, FeatureVector))

object CommandRun extends LazyLogging {


  //init
  MatcherRegistry.initMatchers()
  RapidminerJobs.init()

  /**
   * Runs a single Problem
   * @param config
   * @return
   */
  def runSingleProblem(config: RunConfigurationOneProblem): Option[EvaluationResult] = {
    TimeTaker.takeTime("pipeline_and_evaluate")
    val remove_corelated_for_structural = 0.5
    //create alignment with pipeline
    val alignment: Alignment = config.matching_pipline_separated(config.matchingProblem, config.class_threshold, config.dp_threshold, config.op_threshold, remove_corelated_for_structural)._1
    //write alignment to file
    AlignmentParser.writeRDF(alignment, "alignments/" + config.matchingProblem.name + ".rdf")
    //check if evaluation neccessary
    val total = TimeTaker.takeTime("pipeline_and_evaluate")
    if (config.evaluate) {
      val evaluation_result = alignment.evaluate(config.reference.get)
      Option(evaluation_result)
    } else {
      Option.empty
    }

  }


  /**
   * Runs the whole platform on a specified dataset
   * @param config
   */
  def runRound(config: RunConfiguration): Unit = {
    TimeTaker.takeTime("pipeline_and_evaluate")

    val path_to_ds = Config.loaded_config.getString("oaei.path_to_dataset")
    val ds_name = Config.loaded_config.getString("oaei.dataset_type")
    EvaluationMatchingRunner.matchAndEvaluateOAEI(path_to_ds, ds_name, config)
    val total = TimeTaker.takeTime("pipeline_and_evaluate")

    println(total)
  }

  def getMatchingProblem(onto1: String, onto2: String, data_set_name: String): MatchingProblem = {
    val file_onto1: File = new File(onto1)
    val file_onto2: File = new File(onto2)

    val l_onto1 = OntologyLoader.load_fast_ontology(file_onto1.getPath)
    val l_onto2 = OntologyLoader.load_fast_ontology(file_onto2.getPath)
    val i_onto1 = new IOntology(file_onto1.getPath)
    val i_onto2 = new IOntology(file_onto2.getPath)
    val match_problem = MatchingProblem(l_onto1, l_onto2, i_onto1, i_onto2, data_set_name)
    match_problem
  }

  /**
   * Parse the pipeline config for one matching problem
   * @return
   */
  def parseRunOneProblemConfig(): RunConfigurationOneProblem = {
    val class_threshold = Config.loaded_config.getDouble("pipeline.class_threshold")
    val dp_threshold = Config.loaded_config.getDouble("pipeline.dp_threshold")
    val op_threshold = Config.loaded_config.getDouble("pipeline.op_threshold")

    val norm_technique = Config.loaded_config.getString("pipeline.norm")

    val process_type = Config.loaded_config.getString("pipeline.outlier_method")

    val pre_pro = Config.loaded_config.getString("pipeline.prepro.type")

    val rapidminerProcess = if (pre_pro.equals("pca_variant")) {
      OutlierRegistry.getProcessPCAVariant(process_type, true)
    } else if (pre_pro.equals("pca_fixed")) {
      OutlierRegistry.getProcessPCAFixed(process_type, true)
    } else if (pre_pro.equals("remove_corr")) {
      OutlierRegistry.getProcessRemoveCorrelated(process_type, true)
    } else {
      //default
      OutlierRegistry.getProcessPCAVariant(process_type, true)
    }

    val sourceOnto = Config.loaded_config.getString("pipeline.data.source_onto")
    val targetOnto = Config.loaded_config.getString("pipeline.data.target_onto")

    val problem_name = Config.loaded_config.getString("pipeline.problem_name")

    val match_problem = getMatchingProblem(sourceOnto, targetOnto, problem_name)
    val evaluate = Config.loaded_config.hasPath("pipeline.evaluation")
    val reference = if (evaluate) {
      val path_to_ref = Config.loaded_config.getString("pipeline.evaluation.reference_alignment")
      val reference = AlignmentParser.parseRDF(path_to_ref)
      Option(reference)
    } else {
      Option.empty
    }

    val mining_params: Map[String, Double] = Config.loaded_config.getObject("pipeline.mining").unwrapped().map(tuple => tuple._1 -> tuple._2.toString.toDouble).toMap
    val pre_pro_params: Map[String, Double] = Config.loaded_config.getObject("pipeline.prepro.values").unwrapped().map(tuple => tuple._1 -> tuple._2.toString.toDouble).toMap
    //build pipeline

    val parameters: Map[String, Map[String, Double]] = Map("mining" -> mining_params, pre_pro -> pre_pro_params)

    val outlier_function = RapidminerJobs.rapidminerOutlierDetection(rapidminerProcess.get, Config.loaded_config.getString("rapidminerconfig.tmp"), process_type, parameters, pre_pro) _
    val outlier_function_separated = RapidminerJobs.rapidminerOutlierDetectionSeparated(rapidminerProcess.get, Config.loaded_config.getString("rapidminerconfig.tmp"), process_type, parameters, pre_pro) _
    val norm_function = ScoreNormalizationRegistry.getNormFunction(norm_technique)
    val matching_pipline: (MatchingProblem, Double, Double) => (Alignment, FeatureVector) = MatchingPipelineCore.createMatchingPipeline(outlier_function)(norm_function)
    val matching_pipline_separated: (MatchingProblem, Double, Double, Double, Double) => (Alignment, FeatureVector) = MatchingPipelineCore.createMatchingPipelineSeparated(outlier_function_separated)(norm_function)

    RunConfigurationOneProblem(class_threshold, class_threshold, dp_threshold, op_threshold, norm_technique, match_problem, evaluate, reference, true, matching_pipline, matching_pipline_separated)
  }


  /**
   * parse an OAEI problem and evaluates it
   * @return
   */
  def parseRunConfig(): RunConfiguration = {

    val threshold = Config.loaded_config.getDouble("pipeline.class_threshold")
    val class_threshold = Config.loaded_config.getDouble("pipeline.class_threshold")
    val dp_threshold = Config.loaded_config.getDouble("pipeline.dp_threshold")
    val op_threshold = Config.loaded_config.getDouble("pipeline.op_threshold")

    val norm_technique = Config.loaded_config.getString("pipeline.norm")
    val data_set = Config.loaded_config.getString("oaei.dataset_type")
    val path_to_data_set = Config.loaded_config.getString("oaei.path_to_dataset")

    val process_type = Config.loaded_config.getString("pipeline.outlier_method")

    val pre_pro = Config.loaded_config.getString("pipeline.prepro.type")

    val rapidminerProcess = if (pre_pro.equals("pca_variant")) {
      OutlierRegistry.getProcessPCAVariant(process_type, true)
    } else if (pre_pro.equals("pca_fixed")) {
      OutlierRegistry.getProcessPCAFixed(process_type, true)
    } else if (pre_pro.equals("remove_corr")) {
      OutlierRegistry.getProcessRemoveCorrelated(process_type, true)
    } else {
      //default
      OutlierRegistry.getProcessPCAVariant(process_type, true)
    }

    val mining_params: Map[String, Double] = Config.loaded_config.getObject("pipeline.mining").unwrapped().map(tuple => tuple._1 -> tuple._2.toString.toDouble).toMap
    val pre_pro_params: Map[String, Double] = Config.loaded_config.getObject("pipeline.prepro.values").unwrapped().map(tuple => tuple._1 -> tuple._2.toString.toDouble).toMap
    //build pipeline

    val parameters: Map[String, Map[String, Double]] = Map("mining" -> mining_params, pre_pro -> pre_pro_params)

    val outlier_function = RapidminerJobs.rapidminerOutlierDetection(rapidminerProcess.get, Config.loaded_config.getString("rapidminerconfig.tmp"), process_type, parameters, pre_pro) _
    val outlier_function_separated = RapidminerJobs.rapidminerOutlierDetectionSeparated(rapidminerProcess.get, Config.loaded_config.getString("rapidminerconfig.tmp"), process_type, parameters, pre_pro) _
    val norm_function = ScoreNormalizationRegistry.getNormFunction(norm_technique)
    val matching_pipline: (MatchingProblem, Double, Double) => (Alignment, FeatureVector) = MatchingPipelineCore.createMatchingPipeline(outlier_function)(norm_function)
    val matching_pipline_separated: (MatchingProblem, Double, Double, Double, Double) => (Alignment, FeatureVector) = MatchingPipelineCore.createMatchingPipelineSeparated(outlier_function_separated)(norm_function)

    RunConfiguration(threshold, class_threshold, dp_threshold, op_threshold, norm_technique, data_set, path_to_data_set, true, matching_pipline, matching_pipline_separated)
  }


}