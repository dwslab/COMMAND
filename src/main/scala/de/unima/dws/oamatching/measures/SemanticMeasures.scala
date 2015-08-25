package de.unima.dws.oamatching.measures


import com.typesafe.scalalogging.slf4j.LazyLogging
import de.unima.dws.alex.simservice.{Config => Sim_Service_Config}
import de.unima.dws.oamatching.analysis.SparkJobs
import de.unima.dws.oamatching.config.Config
import org.apache.spark.mllib.feature.Word2VecModel
import org.jsoup.Jsoup
import play.api.libs.json._

import scalaj.http.{Http, HttpResponse}


/**
 * Created by mueller on 31/01/15.
 */
object SemanticMeasures extends LazyLogging {

  //init SimService config

  def word2VecSimilarityMeasure: (String, String) => Double = word2VecCosineSimilarity(SparkJobs.word_2_vec_model) _

  def word2VecSimilarityMeasureStemmed: (String, String) => Double = word2VecCosineSimilarity(SparkJobs.word_2_vec_model_stemmed) _

  def word2VecCosineSimilarity(model: Word2VecModel)(term1: String, term2: String): Double = {
    try {

      val value = SparkJobs.cousineSimilarityBetweenTerms(model, term1.trim, term2.trim)

      value

    } catch {
      case e: Exception => {
        //println(e)
        0.0
      }
    }
  }


  def isUMLSSynonym(phrase1: String, phrase2: String): Double = {
    UMLSSynonymFinder.isSynonym(phrase1, phrase2)

  }


  /**
   * @deprecated
   * @param term1
   * @param term2
   * @return
   */
  def esaSim(term1: String, term2: String): Double = {

    try {
      val response: HttpResponse[String] = Http("http://localhost:8890/esaservice")
        .param("task", "esa")
        .param("term1", term1)
        .param("term2", term2).asString

      if (response.isNotError) {
        val intermediate = Json.parse(response.body)
        val res = intermediate.as[JsString].value.toDouble
        if (res < 0) {
          0.0
        } else {
          res
        }
      } else {
        println("No connection")
        0.0
      }
    }
    catch {
      case e: Throwable => 0.0
    }

  }

  /**
   * @deprecated
   * @param phrase1
   * @param phrase2
   * @return
   */
  def callSTSServiceUMBC(phrase1: String, phrase2: String): Double = {

    try {
      val response: HttpResponse[String] = Http("http://swoogle.umbc.edu/StsService/GetStsSim")
        .param("operation", "api")
        .param("phrase1", phrase1)
        .param("phrase2", phrase2).asString

      response.body.toDouble
    } catch {
      case e: Exception => {
        println("fail")
        0.0
      }
    }

  }

  /**
   * @deprecated
   *
   * @param term
   * @param to_check
   * @return
   */
  def isSynonymOf(term: String, to_check: String): Double = {
    val response: HttpResponse[String] = Http(Config.BIG_HUGE_THESAURUS_SERVICE_URL + "/" + term + "/json").asString
    if (!response.is2xx) {
      0.0
    } else {
      val json_parse = response.body.toString
      val result_json: JsValue = Json.parse(json_parse)

      //get all synonyms
      val syn_sets: Seq[JsValue] = result_json \\ "syn"

      val syn_terms = syn_sets.map(syn_set => {
        val syn_set_arr = syn_set.asInstanceOf[JsArray]
        for (syn_word <- syn_set_arr.value) yield {
          syn_word.asInstanceOf[JsString].as[String]
        }
      }).flatten.toSeq

      println(syn_terms)
      if (syn_terms.contains(to_check)) {
        1.0
      } else {
        0.0
      }
    }
  }


  def getSynonymBigHugeThesaurus(term: String): Seq[String] = {
    try {
      val response: HttpResponse[String] = Http(Config.BIG_HUGE_THESAURUS_SERVICE_URL + "/" + term + "/json").asString

      if (!response.is2xx) {
        Seq()
      } else {
        val json_parse = response.body.toString
        val result_json: JsValue = Json.parse(json_parse)

        //get all synonyms
        val syn_sets: Seq[JsValue] = result_json \\ "syn"

        val syn_terms = syn_sets.map(syn_set => {
          val syn_set_arr = syn_set.asInstanceOf[JsArray]
          for (syn_word <- syn_set_arr.value) yield {
            syn_word.asInstanceOf[JsString].as[String]
          }
        }).flatten.toSeq
        syn_terms
      }
    }catch{
      case e: Exception=> Seq()
    }
  }


  def getTopNWordsHacky(term: String, pos:String):Seq[String] = {
      print("get")
    try{
      val response = Http("http://semanticwebarchive.cs.umbc.edu/SimService/GetSimilarity").postForm(List(("operation", "top_sim"),
        ("word", term),
        ("pos", pos), ("N", "100"),
        ("sim_type", "concept"), ("corpus", "webbase"),
        ("query", "Get Top-N Most Similar Words"))).asString
      if (!response.is2xx) {
        Seq()
      } else {
        val doc = Jsoup.parse(response.body)
        val elements = doc.select("textarea")
        val parsed_html = elements.html()
        val synonyms = parsed_html.split(",").map(splitted_string => splitted_string.split("_")(0).trim()).toList
        synonyms.toSeq
      }
    }catch{
      case e: Exception=> Seq()
    }

  }


}
