package de.unima.dws.oamatching.pipeline

import java.io.File

import com.typesafe.scalalogging.slf4j.LazyLogging
import de.unima.alcomox.ontology.IOntology
import de.unima.dws.oamatching.analysis.{SeparatedResults, SparkJobs}
import de.unima.dws.oamatching.config.Config
import de.unima.dws.oamatching.core.matcher.{Matcher, StructuralLevelMatcher}
import de.unima.dws.oamatching.core.{Alignment, FastOntology, MatchRelation}
import de.unima.dws.oamatching.matcher.MatcherRegistry
import de.unima.dws.oamatching.pipeline.registry.SelectionRegistry

import scala.collection.immutable.Map
import scala.collection.parallel.ForkJoinTaskSupport
import scala.collection.parallel.immutable.ParMap


case class MatchingProblem(ontology1: FastOntology, ontology2: FastOntology, debug_onto1: IOntology, debug_onto2: IOntology, name: String, data_set_name: String = "test")

case class MatchingEvaluationProblem(ontology1: FastOntology, ontology2: FastOntology, reference: Alignment, name: String)

/**
 * Core Single to implement matching of two ontologies
 * Created by mueller on 21/01/15.
 */
object MatchingPipelineCore extends LazyLogging {

  val paralellity = Config.loaded_config.getInt("pipeline.max_threads")


  def createMatchingPipeline(outlierFct: (String, FeatureVector) => (Int, Map[String, (Double, Double)], Map[MatchRelation, Double]))(normFct: (Int, Map[String, (Double, Double)], Map[MatchRelation, Double]) => Iterable[(MatchRelation, Double)]): (MatchingProblem, Double, Double) => (Alignment, FeatureVector) = {
    matchProblem(outlierFct)(normFct)
  }

  def createMatchingPipelineSeparated(outlierFct: (String, FeatureVector) => SeparatedResults)(normFct: (Int, Map[String, (Double, Double)], Map[MatchRelation, Double]) => Iterable[(MatchRelation, Double)]): (MatchingProblem, Double, Double, Double, Double) => (Alignment, FeatureVector) = {
    matchProblemSeparated(outlierFct)(normFct)
  }

  /**
   * To execute matching process
   * @param problem

   * @return
   */
  def matchProblem(outlierFct: (String, FeatureVector) => (Int, Map[String, (Double, Double)], Map[MatchRelation, Double]))(normFct: (Int, Map[String, (Double, Double)], Map[MatchRelation, Double]) => Iterable[(MatchRelation, Double)])(problem: MatchingProblem, threshold: Double, remove_correlated_threshold: Double): (Alignment, FeatureVector) = {
    val start_time = System.currentTimeMillis()

    val filtered_outlier_analysis_vector: FeatureVector = createFeatureVector(problem, remove_correlated_threshold, true)


    val outlier_analysis_result: (Int, Map[String, (Double, Double)], Map[MatchRelation, Double]) = outlierFct(problem.name, filtered_outlier_analysis_vector)

    //post processing, so normalization and feature selection
    val alignment: Alignment = postProcessMatchings(normFct, threshold, outlier_analysis_result, problem)

    (alignment, filtered_outlier_analysis_vector)
  }

  def matchProblemSeparated(outlierFct: (String, FeatureVector) => SeparatedResults)(normFct: (Int, Map[String, (Double, Double)], Map[MatchRelation, Double]) => Iterable[(MatchRelation, Double)])(problem: MatchingProblem, class_threshold: Double, dp_threshold: Double, op_threshold: Double, remove_correlated_threshold: Double): (Alignment, FeatureVector) = {
    val start_time = System.currentTimeMillis()
    val onto1_namespace = problem.ontology1.name
    val onto2_namespace = problem.ontology2.name


    val reuse_vectors = Config.loaded_config.getBoolean("oaei.reuse_vectors")
    //check if matching already exists then use this one

    val tester_file = new File("matchings/" + problem.data_set_name + "/matchings/" + problem.name + "_raw_matchings" + ".csv")
    //create feature vector only if some config parameter is set
    val filtered_outlier_analysis_vector: FeatureVector = if (tester_file.exists()) {
      if(reuse_vectors){
        FeatureVector(problem.name, null, null, null, null)
      }else {
        VectorUtil.readFromMatchingFile(tester_file, problem.name)
      }
    } else {
      createFeatureVector(problem, remove_correlated_threshold, true)
    }
    //check if feature vector already exists

    val outlier_analysis_result_separated = outlierFct(problem.name, filtered_outlier_analysis_vector)

    //post processing, so normalization and feature selection
    val alignment: Alignment = postProcessSeparatedMatchings(normFct, class_threshold, dp_threshold, op_threshold, outlier_analysis_result_separated, problem)

    (alignment, filtered_outlier_analysis_vector)
  }


  /**
   * Creates a Feature Vector for a given problem
   * @param problem
   * @param remove_correlated_threshold
   * @return
   */
  def createFeatureVector(problem: MatchingProblem, remove_correlated_threshold: Double, name_space_filter: Boolean): FeatureVector = {

    logger.info("Start element Level Matching")
    val onto1_namespace = problem.ontology1.name
    val onto2_namespace = problem.ontology2.name
    val allowed_namespaces = List(onto1_namespace, onto2_namespace)

    val individual_matcher_results: FeatureVector = matchAllIndividualMatchers(problem)
    logger.info("Element Level Matching Done")

    logger.info("Start remove correlated")
     val uncorrelated_matcher_results: FeatureVector = removeCorrelatedMatchers(individual_matcher_results, remove_correlated_threshold)
    logger.info("Remove correlated done")

     val structural_matcher_results: Option[FeatureVector] =  matchAllStructuralMatchers(problem, uncorrelated_matcher_results)

     val outlier_analysis_vector: FeatureVector = if (structural_matcher_results.isDefined) VectorUtil.combineFeatureVectors(List(individual_matcher_results, structural_matcher_results.get), problem.name).get else individual_matcher_results
    logger.info("Vector Combination done")

    //name space filtering
    val name_space_filtered = if (name_space_filter) {
      val filtered_outlier_analysis_vector: FeatureVector = MatchingPruner.featureVectorNameSpaceFilter(outlier_analysis_vector, allowed_namespaces)
      //val filtered_outlier_analysis_vector: FeatureVector = MatchingPruner.featureVectorNameSpaceFilter(individual_matcher_results, allowed_namespaces)
      filtered_outlier_analysis_vector
    } else {
      //outlier_analysis_vector
      individual_matcher_results
    }
    logger.info("Pre feature selection Vector Size" + name_space_filtered.matcher_name_to_index.size)

    //First pre feature selection
    val feature_selected = if (Config.loaded_config.getBoolean("general.feature_selection")) {
      VectorUtil.selectFeatures(name_space_filtered)
    } else {
      name_space_filtered
    }
    logger.info("After feature selection Vector Size" + feature_selected.matcher_name_to_index.size)
    feature_selected
  }

  /**
   * Post processing of the results:
   * @param normFct
   * @param threshold
   * @param outlier_analysis_result
   * @return
   */
  def postProcessMatchings(normFct: (Int, Map[String, (Double, Double)], Map[MatchRelation, Double]) => Iterable[(MatchRelation, Double)], threshold: Double, outlier_analysis_result: (Int, Map[String, (Double, Double)], Map[MatchRelation, Double]), problem: MatchingProblem): Alignment = {

    val selected = normalizeAndSelectSingle(normFct, outlier_analysis_result, threshold, problem.ontology1, problem.ontology2)

    val alignment = new Alignment(problem.ontology1.name, problem.ontology2.name, null, null, problem.debug_onto1, problem.debug_onto2, selected)

    if (Config.loaded_config.getBoolean("pipeline.debug_alignment")) {
      MatchingPruner.debugAlignment(alignment, outlier_analysis_result._3, threshold)
    } else {
      alignment
    }
  }

  /**
   *
   * @param normFct
   * @param class_threshold
   * @param dp_threshold
   * @param op_threshold
   * @param outlier_analysis_result
   * @param problem
   * @return
   */
  def postProcessSeparatedMatchings(normFct: (Int, Map[String, (Double, Double)], Map[MatchRelation, Double]) => Iterable[(MatchRelation, Double)], class_threshold: Double, dp_threshold: Double, op_threshold: Double, outlier_analysis_result: SeparatedResults, problem: MatchingProblem): Alignment = {

    val selected_classes = normalizeAndSelectSingle(normFct, outlier_analysis_result.class_matchings, class_threshold, problem.ontology1, problem.ontology2)
    val selected_dps = normalizeAndSelectSingle(normFct, outlier_analysis_result.dp_matchings, dp_threshold, problem.ontology1, problem.ontology2)
    val selected_ops = normalizeAndSelectSingle(normFct, outlier_analysis_result.op_matchings, op_threshold, problem.ontology1, problem.ontology2)

    val final_matchings = selected_classes ++ selected_dps ++ selected_ops

    val alignment = new Alignment(problem.ontology1.name, problem.ontology2.name, problem.ontology1, problem.ontology2, problem.debug_onto1, problem.debug_onto2, final_matchings)

    if (Config.loaded_config.getBoolean("pipeline.debug_alignment")) {
      val raw_matchings = normFct.tupled(outlier_analysis_result.class_matchings).toMap ++ normFct.tupled(outlier_analysis_result.dp_matchings).toMap ++ normFct.tupled(outlier_analysis_result.op_matchings).toMap
      MatchingPruner.debugAlignment(alignment)
    } else {
      alignment
    }

  }

  def normalizeAndSelectSingle(normFct: (Int, Map[String, (Double, Double)], Map[MatchRelation, Double]) => Iterable[(MatchRelation, Double)], outlier_analysis_result: (Int, Map[String, (Double, Double)], Map[MatchRelation, Double]), threshold: Double, sourceOnto: FastOntology, targetOnto: FastOntology): Map[MatchRelation, Double] = {
    val final_result: Iterable[(MatchRelation, Double)] = normFct.tupled(outlier_analysis_result)

    val method_name = Config.loaded_config.getString("pipeline.selection.method")
    val fuzzy_value = Config.loaded_config.getDouble("pipeline.selection.fuzzy")

    val selection_fct  = SelectionRegistry.configureSelectionMethod(fuzzy_value, method_name)
    val selected: Map[MatchRelation, Double] = selection_fct(final_result.toMap, threshold, sourceOnto, targetOnto)
    // val selected: Map[MatchRelation, Double] = MatchingSelector.greedyRankSelectorSimple(final_result.toMap, threshold, sourceOnto, targetOnto)
    selected
  }

  /**
   * Calls an Individual matcher, for feature vector creation
   * @param matcher
   * @param problem
   * @return
   */
  def matchIndividualMatcher(matcher: Matcher, problem: MatchingProblem): Map[MatchRelation, Double] = {
    val threshold = Config.loaded_config.getDouble("general.base_threshold")

    matcher.align(problem, threshold).asMatchRelationMap()
  }

  /**
   * Method that matches all individual matcher and consequently returns a Feature Vector with it's results
   * @param problem
   * @return
   */
  def matchAllIndividualMatchers(problem: MatchingProblem): FeatureVector = {

    val par_collection = MatcherRegistry.matcher_by_name.par


    par_collection.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(paralellity))


    val vector: ParMap[String, Map[MatchRelation, Double]] = par_collection.map({ case (name, matcher) => {

      val starttime = System.currentTimeMillis()
      logger.info(s"start $name")

      val result = try {
        matchIndividualMatcher(matcher, problem)
      } catch {
        case e: Throwable => {
          logger.error("Failed at base matcher", e)
          null
        }
      }

      val totaltime = System.currentTimeMillis() - starttime
      logger.info(s"finshed $name in $totaltime")
      (name, result)


    }
    }).toMap

    val matcher_name_to_index: Map[String, Int] = vector.keys.toList.zipWithIndex.toMap
    val matcher_index_to_name: Map[Int, String] = matcher_name_to_index.map(tuple => (tuple._2, tuple._1)).toMap
    val vector_per_matchings = VectorUtil.createInvertedVector(vector.seq)

    FeatureVector(problem.name, vector.seq, vector_per_matchings, matcher_name_to_index, matcher_index_to_name)
  }

  /**
   * Performs structural matching for all available structural matcher with all remaining initial mappings
   * @param problem
   * @param featureVector
   * @return
   */
  def matchAllStructuralMatchers(problem: MatchingProblem, featureVector: FeatureVector): Option[FeatureVector] = {

    val results: List[FeatureVector] = MatcherRegistry.structural_matcher_by_name.par.map { case matcher =>
      matchStructuralMatcher(matcher._2, problem, featureVector)
    }.toList
    //combine and return results
    VectorUtil.combineFeatureVectors(results, problem.name)

  }

  /**
   * Matches one Structural matcher for all available initial mappings
   * @param matcher
   * @param problem
   * @param featureVector
   * @return
   */
  def matchStructuralMatcher(matcher: StructuralLevelMatcher, problem: MatchingProblem, featureVector: FeatureVector): FeatureVector = {

    val results: Map[String, Map[MatchRelation, Double]] = for (matcher_res <- featureVector.vector) yield {
      (matcher_res._1 + matcher.getClass.getName.replace(".", ""), matchStructuralMatcherWithAlignment(matcher, problem, matcher_res._2))
    }
    //return a feature vector
    VectorUtil.createVectorFromResult(results, problem.name)
  }

  /**
   * Matches one structural matcher for one initial mapping
   * @param matcher
   * @param problem
   * @param correspondences
   * @return
   */
  def matchStructuralMatcherWithAlignment(matcher: StructuralLevelMatcher, problem: MatchingProblem, correspondences: Map[MatchRelation, Double]): Map[MatchRelation, Double] = {
    //create alignment frm correspondences
    val starttime = System.currentTimeMillis()

    val initial_alignment = new Alignment(null, null, 0.0, correspondences)

    val res = matcher.align(problem, initial_alignment, 0.0).asMatchRelationMap()

    res
  }


  /**
   * Method to remove correlated feature for structural matchers
   *
   * @param feature_vector
   * @return
   */
  def removeCorrelatedMatchers(feature_vector: FeatureVector, threshold: Double) = {
    SparkJobs.removeCorrelatedFeatures(feature_vector, threshold)
  }
}
