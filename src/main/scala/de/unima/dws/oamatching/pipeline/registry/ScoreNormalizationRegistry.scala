package de.unima.dws.oamatching.pipeline.registry

import de.unima.dws.oamatching.core.MatchRelation
import org.apache.commons.math.stat.descriptive.moment.Mean
import org.apache.commons.math3.special.{Erf, Gamma}
import org.apache.commons.math3.stat.descriptive.moment.StandardDeviation

import scala.collection.immutable.Map

/**
 * Created by mueller on 10/02/15.
 */
object ScoreNormalizationRegistry {
  val stdev_computer = new StandardDeviation()
  val mean_computer = new Mean()


  def getNormFunction(normFCT:String): (Int, Map[String, (Double, Double)],Map[MatchRelation, Double]) => Iterable[(MatchRelation, Double)] = {
    normFCT match {
      case "none" => noNormalization _
      case "euclidean_max"  =>  normalizeByMaxEuclideanDistance _
      case "gaussianscale" => normalizeByGaussianScaling _
      case "zscore" => normalizeByZScore _
      case "gamma" => normalizeByGammaScaling _
      case other =>  noNormalization _
    }
  }

  def normalizeByMaxEuclideanDistance(dimensions: Int, max_min_by_dim:Map[String, (Double, Double)],relations: Iterable[(MatchRelation, Double)]): Iterable[(MatchRelation, Double)] = {
    //val maxDistance = Math.sqrt(dimensions.toDouble * 4)

    //calc max distance by min max difference

    val squared =  max_min_by_dim.map(tuple => Math.pow((tuple._2._1-tuple._2._2),2.0))
    val squared_sum = if(squared.size>0){squared.reduceLeft(_+_)}else {0.0}
    val max_distance = Math.sqrt(squared_sum)

    relations.view.map { case (match_relation, distance) => {
      (match_relation, (distance / max_distance))
    }
    }
  }

  /**
   *  As Proposed by Kriegel et.al Interpreting and Unifying Outlier Scores
   * @param dimensions
   * @param relations
   * @return
   */
  def normalizeByGaussianScaling(dimensions: Int, max_min_by_dim:Map[String, (Double, Double)], relations: Iterable[(MatchRelation, Double)]): Iterable[(MatchRelation, Double)] = {

    val values = relations.unzip._2.toArray

    val stdev = stdev_computer.evaluate(values)
    val mean = mean_computer.evaluate(values)


    relations.view.map { case (match_relation, distance) => {

      val pre_erf = (distance - mean)/(stdev*Math.sqrt(2))
      val scaled = Math.max(0,Erf.erf(pre_erf))

      (match_relation, scaled)
    }
    }
  }


  def normalizeByGammaScaling(dimensions: Int, max_min_by_dim:Map[String, (Double, Double)], relations: Iterable[(MatchRelation, Double)]): Iterable[(MatchRelation, Double)] = {

    def cdfGamma(k:Double,theta:Double) (param:Double):Double = {

      if(param > 0.0){
        try{
          Gamma.regularizedGammaP(k,param/theta)
        }catch{
          case e:Throwable => {
            println(s"K: $k")
            println(s"theta $theta")
            println(s"param $theta")
            0.0
          }
        }

      }else {
        0.0
      }

    }

    val values = relations.unzip._2.toArray

    val expected_mean = mean_computer.evaluate(values)
   // val expected_mean_square = mean_computer.evaluate(values.map(Math.pow(_,2.0)))
    //val expected_mean_square = mean_computer.evaluate(values.map(Math.pow(_,2.0)))
    val expected_stdev= stdev_computer.evaluate(values)

    val theta =  (expected_stdev*expected_stdev)/expected_mean
    val k = (expected_mean*expected_mean) / (expected_stdev*expected_stdev)
    def cdfGamma_parameterized = cdfGamma(k,theta) _

    def mean_cdf = cdfGamma_parameterized(expected_mean)

    relations.view.map { case (match_relation, distance) => {

      val pre_selected = (cdfGamma_parameterized(distance)- mean_cdf )/(1-mean_cdf)
      val scaled = Math.max(0,pre_selected)

      (match_relation, scaled)
    }
    }
  }
  def normalizeByZScore(dimensions: Int, max_min_by_dim:Map[String, (Double, Double)], relations: Iterable[(MatchRelation, Double)]): Iterable[(MatchRelation, Double)] = {
    val values = relations.unzip._2.toArray

    val stdev = stdev_computer.evaluate(values)
    val mean = mean_computer.evaluate(values)

    val z_scaled = relations.view.map { case (match_relation, distance) => {

      val scaled = Math.max(0,(distance - mean) / (stdev))

      (match_relation, scaled)
    }
    }

    val max_z:Double = if(z_scaled.size > 0) {
      z_scaled.maxBy(tuple => tuple._2)._2
    }else {
      0.0
    }

    z_scaled.map { case (match_relation, distance) => {
      (match_relation, distance/max_z)
    }
    }
  }




  def noNormalization(dimensions: Int, max_min_by_dim:Map[String, (Double, Double)], relations: Iterable[(MatchRelation, Double)]): Iterable[(MatchRelation, Double)] = {
    relations
  }

}
