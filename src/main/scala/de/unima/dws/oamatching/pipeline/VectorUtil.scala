package de.unima.dws.oamatching.pipeline

import java.io.File

import _root_.de.unima.dws.oamatching.core.MatchRelation
import com.github.tototoshi.csv.CSVReader
import de.unima.dws.oamatching.config.Config
import org.apache.commons.math.stat.descriptive.moment.Mean
import org.apache.commons.math3.stat.descriptive.moment.StandardDeviation

import scala.Predef
import scala.collection.immutable.{Iterable, Map}

case class FeatureVector(data_set_name: String, vector: Map[String, Map[MatchRelation, Double]], transposed_vector: Map[MatchRelation, Map[String, Double]], matcher_name_to_index: Map[String, Int], matcher_index_to_name: Map[Int, String])

/**
 * Created by mueller on 23/01/15.
 */
object VectorUtil {

  //for feature selection
  val mean_computer = new Mean()
  val stdev_computer = new StandardDeviation()

  /**
   * This function removes elements from the feature vector specified by
   * @param feature_vector
   * @param indices_to_remove
   * @return
   */
  def reduceFeatureVector(feature_vector: FeatureVector, indices_to_remove: Set[Int]): FeatureVector = {

    //remove those from the feature vector that are in the indices_to_remove set, only keep those that are in there
    val vector = feature_vector.vector.filterNot({ case (key, relation_map) => indices_to_remove.contains(feature_vector.matcher_name_to_index.get(key).get)})
    //rebuild misc things
    val matcher_name_to_index: Map[String, Int] = vector.keys.toList.zipWithIndex.toMap
    val matcher_index_to_name: Map[Int, String] = matcher_name_to_index.map(tuple => (tuple._2, tuple._1)).toMap
    val vector_per_matchings = createInvertedVector(vector)

    FeatureVector(feature_vector.data_set_name, vector, vector_per_matchings, matcher_name_to_index, matcher_index_to_name)
  }

  /**
   * Inverts the feature vector, to not have any more the results by matchername, but by matchingrelation, so it's basically a conversion from row to a column vector
   * @param vector
   * @return
   */
  def createInvertedVector(vector: Map[String, Map[MatchRelation, Double]]): Map[MatchRelation, Map[String, Double]] = {
    val unique_matchings: Set[MatchRelation] = vector.map({ case (name, matchings) => matchings.keySet}).flatten.toSet


    val vector_per_matchings: Map[MatchRelation, Map[String, Double]] = unique_matchings.view.map(matching => (matching, vector.map(tuple => (tuple._1, tuple._2.get(matching).getOrElse(0.0))))).toMap
    //val vector_per_matchings: Map[MatchRelation, Map[String, Double]] = unique_matchings.map(matching => (matching, vector.filter(tuple => tuple._2.contains(matching)).map(tuple => (tuple._1, tuple._2.get(matching).getOrElse(0.0))))).toMap
    vector_per_matchings
  }

  /**
   * Creates a Feature Vector from a result of map of results of  matchers by matchername as key
   * @param result
   * @return
   */
  def createVectorFromResult(result: Map[String, Map[MatchRelation, Double]], name: String): FeatureVector = {
    val matcher_name_to_index: Map[String, Int] = result.keys.toList.zipWithIndex.toMap
    val matcher_index_to_name: Map[Int, String] = matcher_name_to_index.view.map(tuple => (tuple._2, tuple._1)).toMap
    val vector_per_matchings = VectorUtil.createInvertedVector(result)

    FeatureVector(name, result, vector_per_matchings, matcher_name_to_index, matcher_index_to_name)
  }

  /**
   * Function combines a collection of vector and joins them to one vector
   * @param feature_vectors
   * @return
   */
  def combineFeatureVectors(feature_vectors: Iterable[FeatureVector], name: String): Option[FeatureVector] = {
    if (feature_vectors.size != 0) {


      val list_of_maps = feature_vectors.map(vector => vector.vector)
      val size1: Int = list_of_maps.map(res_map => res_map.size).reduceLeft(_ + _)
      val whole_map: Map[String, Map[MatchRelation, Double]] = list_of_maps.reduceLeft(_ ++ _)


      //println("Should be the same " + size1 + "---" + whole_map.size)
      //println(whole_map.keys)
      Option.apply(VectorUtil.createVectorFromResult(whole_map, name))
    } else {
      Option.empty
    }
  }

  def readFromMatchingFile(matching_file:File, name:String):FeatureVector = {
    val metaDataFields = List("left", "relation", "right", "owl_type", "match_type")

    val reader_head = CSVReader.open(matching_file)

    val matcher_names = reader_head.iterator.next().filterNot(name => metaDataFields.contains(name))

    println(matcher_names)
    reader_head.close()

    val reader = CSVReader.open(matching_file)
    var dim_size: Int = 0
    //maybe change to iterator
    val lines = reader.allWithHeaders


   val results: Map[String, Map[MatchRelation, Double]] =  matcher_names.map(matcher => {

      matcher -> lines.map(tuple => {
        val matcher_result = tuple.get(matcher).get.toDouble

        val left = tuple.get("left").get
        val right = tuple.get("right").get

        val owl_type = tuple.get("owl_type").get
        val match_type = tuple.get("match_type").get


         MatchRelation(left,"=",right,owl_type,match_type)-> matcher_result
      }).toMap

    }).toMap

    reader.close()
    createVectorFromResult(results,name)
  }

  def selectFeatures(vector: FeatureVector): FeatureVector = {
    val sum_of_scores_per_feature: Map[String, Double] = vector.vector.map { case (name, relations) => {
      (name, stdev_computer.evaluate(relations.values.toArray))
    }
    }.toMap


   val min_stdev =  Config.loaded_config.getDouble("general.min_stdev")
    val sum_of_scores_per_feature_filtered = sum_of_scores_per_feature.filter { case (feature, stdev) => (stdev >min_stdev)}

    val filtered_results = vector.vector.filter { case (feature, relations) => {
      sum_of_scores_per_feature_filtered.contains(feature)
    }
    }

    createVectorFromResult(filtered_results, vector.data_set_name)
  }
}