package de.unima.dws.oamatching.pipeline

import java.io.File

import com.typesafe.scalalogging.slf4j.LazyLogging
import de.dwslab.alcomox.ontology.IOntology
import de.unima.dws.oamatching.analysis.SeparatedResults
import de.unima.dws.oamatching.config.Config
import de.unima.dws.oamatching.core.matcher.{ExtractedFields, Matcher, StructuralLevelMatcher}
import de.unima.dws.oamatching.core._
import de.unima.dws.oamatching.matcher.MatcherRegistry
import de.unima.dws.oamatching.measures.{StringMeasureHelper, SynonymFinder}
import de.unima.dws.oamatching.pipeline.registry.SelectionRegistry
import de.unima.dws.oamatching.pipeline.util.TimeTaker
import org.apache.commons.math.linear.OpenMapRealMatrix
import org.apache.commons.math.stat.correlation.PearsonsCorrelation
import org.semanticweb.owlapi.model.IRI

import scala.collection.immutable.Map
import scala.collection.mutable
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
    TimeTaker.takeTime("single_pipeline")
    val reuse_vectors = Config.loaded_config.getBoolean("oaei.reuse_vectors")
    //check if matching already exists then use this one

    val tester_file = new File("matchings/" + problem.data_set_name + "/matchings/" + problem.name + "_raw_matchings" + ".csv")
    //create feature vector only if some config parameter is set
    val filtered_outlier_analysis_vector: FeatureVector = if (tester_file.exists()) {
      if (reuse_vectors) {
        FeatureVector(problem.name, null, null, null, null)
      } else {
        VectorUtil.readFromMatchingFile(tester_file, problem.name)
      }
    } else {
      createFeatureVector(problem, remove_correlated_threshold, true)
    }
    //check if feature vector already exists
    TimeTaker.takeTime("aggregration")
    val outlier_analysis_result_separated = outlierFct(problem.name, filtered_outlier_analysis_vector)
    TimeTaker.takeTime("aggregration")
    //post processing, so normalization and feature selection
    TimeTaker.takeTime("extraction")
    val alignment: Alignment = postProcessSeparatedMatchings(normFct, class_threshold, dp_threshold, op_threshold, outlier_analysis_result_separated, problem)


    TimeTaker.takeTime("extraction")
    TimeTaker.takeTime("single_pipeline")
    (alignment, filtered_outlier_analysis_vector)
  }

  def filterMatchingProblem(problem: MatchingProblem): MatchingProblem = {
    val classes_to_keep_o1 = mutable.HashSet[IRI]()
    val classes_to_keep_o2 = mutable.HashSet[IRI]()


    for (class_o1: IRI <- problem.ontology1.base_values.classes;
         class_o2: IRI <- problem.ontology2.base_values.classes) {
      val classes_to_names_o1: ExtractedFields = problem.ontology1.classes_to_names.get(class_o1).get
      val classes_to_names_o2: ExtractedFields = problem.ontology2.classes_to_names.get(class_o2).get

      val names_1 = List(classes_to_names_o1.label, classes_to_names_o1.fragment, classes_to_names_o1.comment)
      val names_2 = List(classes_to_names_o2.label, classes_to_names_o2.fragment, classes_to_names_o2.comment)


      val scores: List[Double] = for (name_1 <- names_1;
                                      name_2 <- names_2;
                                      if (name_1.isDefined && name_2.isDefined)) yield {
        val scores: List[Double] = for (token_1 <- StringMeasureHelper.tokenize_combined_all(name_1.get);
                                        token_2 <- StringMeasureHelper.tokenize_combined_all(name_2.get)) yield {

          SynonymFinder.get_syn_overlap_score(token_1.toLowerCase, token_2.toLowerCase)
        }
        val best_score: Double = scores.sum / scores.size.toDouble
        best_score
      }
      val syn_sim_score = scores.sum / scores.size.toDouble
      if (syn_sim_score >= 0.25) {
        classes_to_keep_o1.+=(class_o1)
        classes_to_keep_o2.+=(class_o2)
        println(class_o1.toString + " ---- " + class_o2.toString + " :" + syn_sim_score)
      } else {
        Option.empty
      }
    }


    val op_to_keep_o1 = mutable.HashSet[IRI]()
    val op_to_keep_o2 = mutable.HashSet[IRI]()


    for (object_prop_o1: IRI <- problem.ontology1.base_values.object_properties;
         object_prop_o2: IRI <- problem.ontology2.base_values.object_properties) {
      val object_prop_to_names_o1: ExtractedFields = problem.ontology1.object_properties_to_names.get(object_prop_o1).get
      val object_prop_to_names_o2: ExtractedFields = problem.ontology2.object_properties_to_names.get(object_prop_o2).get

      val names_1 = List(object_prop_to_names_o1.label, object_prop_to_names_o1.fragment, object_prop_to_names_o1.comment)
      val names_2 = List(object_prop_to_names_o2.label, object_prop_to_names_o2.fragment, object_prop_to_names_o2.comment)


      val scores: List[Double] = for (name_1 <- names_1;
                                      name_2 <- names_2;
                                      if (name_1.isDefined && name_2.isDefined)) yield {
        val scores: List[Double] = for (token_1 <- StringMeasureHelper.tokenize_combined_all(name_1.get);
                                        token_2 <- StringMeasureHelper.tokenize_combined_all(name_2.get)) yield {
          val stem1 = StringMeasureHelper.porter_stem(token_1.toLowerCase)
          val stem2 = StringMeasureHelper.porter_stem(token_2.toLowerCase)
          SynonymFinder.get_syn_overlap_score(stem1, stem2)
        }
        val best_score: Double = scores.sum / scores.size.toDouble
        best_score
      }
      val syn_sim_score = scores.sum / scores.size.toDouble
      if (syn_sim_score >= 0.25) {
        op_to_keep_o1.+=(object_prop_o1)
        op_to_keep_o2.+=(object_prop_o2)
        println(object_prop_o1.toString + " ---- " + object_prop_o2.toString + " :" + syn_sim_score)
      } else {
        Option.empty
      }
    }


    val dp_to_keep_o1 = mutable.HashSet[IRI]()
    val dp_to_keep_o2 = mutable.HashSet[IRI]()


    for (data_prop_o1: IRI <- problem.ontology1.base_values.data_properties;
         data_prop_o2: IRI <- problem.ontology2.base_values.data_properties) {
      val data_prop_to_names_o1: ExtractedFields = problem.ontology1.data_properties_to_names.get(data_prop_o1).get
      val data_prop_to_names_o2: ExtractedFields = problem.ontology2.data_properties_to_names.get(data_prop_o2).get

      val names_1 = List(data_prop_to_names_o1.label, data_prop_to_names_o1.fragment, data_prop_to_names_o1.comment)
      val names_2 = List(data_prop_to_names_o2.label, data_prop_to_names_o2.fragment, data_prop_to_names_o2.comment)


      val scores: List[Double] = for (name_1 <- names_1;
                                      name_2 <- names_2;
                                      if (name_1.isDefined && name_2.isDefined)) yield {
        val scores: List[Double] = for (token_1 <- StringMeasureHelper.tokenize_combined_all(name_1.get);
                                        token_2 <- StringMeasureHelper.tokenize_combined_all(name_2.get)) yield {
          val stem1 = StringMeasureHelper.porter_stem(token_1.toLowerCase)
          val stem2 = StringMeasureHelper.porter_stem(token_2.toLowerCase)
          SynonymFinder.get_syn_overlap_score(stem1, stem2)
        }
        val best_score: Double = scores.sum / scores.size.toDouble
        best_score
      }
      val syn_sim_score = scores.sum / scores.size.toDouble
      if (syn_sim_score > 0.25) {
        dp_to_keep_o1.+=(data_prop_o1)
        dp_to_keep_o2.+=(data_prop_o2)
        println(data_prop_o1.toString + " ---- " + data_prop_o2.toString + " :" + syn_sim_score)
      } else {
        Option.empty
      }
    }

    // Rebuild Problem
    // rebuild ontology 1

    val new_entities_o1 = EntitiesOntology(classes_to_keep_o1.toVector,op_to_keep_o1.toVector,dp_to_keep_o1.toVector)
    val new_entities_o2 = EntitiesOntology(classes_to_keep_o2.toVector,op_to_keep_o2.toVector,dp_to_keep_o2.toVector)

    val new_onto1 = OntologyLoader.updateBaseEntities(problem.ontology1, new_entities_o1)
    val new_onto2 = OntologyLoader.updateBaseEntities(problem.ontology2, new_entities_o2)
    MatchingProblem(new_onto1,new_onto2,problem.debug_onto1,problem.debug_onto2,problem.name,problem.data_set_name)
  }


  /**
   * Creates a Feature Vector for a given problem
   * @param problem
   * @param remove_correlated_threshold
   * @return
   */
  def createFeatureVector(problem: MatchingProblem, remove_correlated_threshold: Double, name_space_filter: Boolean): FeatureVector = {

    TimeTaker.takeTime("feature_vector")

    TimeTaker.takeTime("filter_problems")
    val filtered_problem = filterMatchingProblem(problem)
    //val filtered_problem = problem
    SynonymFinder.save_to_json()
    TimeTaker.takeTime("filter_problems")

    logger.info("Start element Level Matching")
    val onto1_namespace = filtered_problem.ontology1.name
    val onto2_namespace = filtered_problem.ontology2.name
    val allowed_namespaces = List(onto1_namespace, onto2_namespace)

    val individual_matcher_results: FeatureVector = matchAllIndividualMatchers(filtered_problem)
    logger.info("Element Level Matching Done")

    logger.info("Start remove correlated")
    val uncorrelated_matcher_results: FeatureVector = removeCorrelatedMatchers(individual_matcher_results, remove_correlated_threshold)
    logger.info("Remove correlated done")

    val structural_matcher_results: Option[FeatureVector] = matchAllStructuralMatchers(filtered_problem, uncorrelated_matcher_results)

    val outlier_analysis_vector: FeatureVector = if (structural_matcher_results.isDefined) VectorUtil.combineFeatureVectors(List(individual_matcher_results, structural_matcher_results.get), filtered_problem.name).get else individual_matcher_results
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

    TimeTaker.takeTime("feature_vector")
    print(feature_selected.transposed_vector.size)
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

    val selection_fct = SelectionRegistry.configureSelectionMethod(fuzzy_value, method_name)
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
    removeCorrelatedFeatures(feature_vector, threshold)
  }


  /**
   * Function to remove correlated features from the feature vector with apache spark capabilites
   * @param feature_vector
   * @param threshold
   * @return
   */
  def removeCorrelatedFeatures(feature_vector: FeatureVector, threshold: Double): FeatureVector = {
    logger.info("start spark job to remove correlated attributes")
    val no_of_matcher: Int = feature_vector.matcher_index_to_name.size


    val test: List[Array[Double]] = feature_vector.transposed_vector.view.map { case (matching, matchermap) => matchermap.values.toArray }.toList

    val matrix = new OpenMapRealMatrix(test.size, no_of_matcher)
    for ((column, index) <- test.zipWithIndex) {
      matrix.setRow(index, column)
    }

    val start_time = System.currentTimeMillis()
    val pearson = new PearsonsCorrelation(matrix)

    val total_time = System.currentTimeMillis() - start_time

    println("Pearson took total time: " + total_time)
    val correlation_matrix = pearson.getCorrelationMatrix()


    val to_be_removed: List[Int] = for (row <- List.range(0, test.size - 1);
                                        column <- List.range(0, no_of_matcher);
                                        if column > row;
                                        if correlation_matrix.getColumn(column)(row) > threshold) yield {
      column
    }


    val filtered_vector = feature_vector.vector.filterKeys(matcher_name => {
      val matcher_index = feature_vector.matcher_name_to_index(matcher_name)
      !to_be_removed.toSet.contains(matcher_index)
    })

    val filtered_feature_vector = VectorUtil.createVectorFromResult(filtered_vector, feature_vector.data_set_name)

    logger.info("Total number of parameters to be removed by remove correlated spark Job" + to_be_removed.size)

    System.gc()

    filtered_feature_vector
  }
}
