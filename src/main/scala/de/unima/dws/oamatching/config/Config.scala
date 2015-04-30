package de.unima.dws.oamatching.config

import scala.io.Source
import com.typesafe.config.ConfigFactory
import de.unima.dws.alex.simservice.{Config => Sim_Service_Config, SimService}

object Config {


  val loaded_config = ConfigFactory.load()

  //Redis configuration
  val REDIS_HOST: String = "localhost"
  val REDIS_PORT: Int = 6379
  val REDIS_THRESHOLD_HASH_KEY = "MA_THRESHOLDS"

  val DEFAULT_MATCHER_THRESHOLD = 0.8;
  //webserbice based config
  val UMBC_STS_SERVICE_URL = "http://swoogle.umbc.edu/StsService/GetStsSim"
  val UMBC_PHRASE_SIM_SERVICE_URL = "http://swoogle.umbc.edu/SimService/GetSimilarity"
  val BIG_HUGE_THESAURUS_SERVICE_URL = "http://words.bighugelabs.com/api/2/74a732bd0c883ad86cc768493c0ccbac"

  //external models
  var WORD_2_VEC_MODEL_PATH: String = loaded_config.getString("resources.w2v.model_stemmed")
  var WORD_2_VEC_STEMMED_MODEL_PATH: String =  loaded_config.getString("resources.w2v.model_normal")

  //Matcher configuration files
  val PATH_TO_MATCHER_CONFIG = "matcher_config.csv"

  val PATH_TO_STOP_LIST = loaded_config.getString("resources.stoplist")

  //load umbc models paths
  Sim_Service_Config.conceptModelFileWebbase =loaded_config.getString("resources.umbc.model_c_webbase")
  Sim_Service_Config.relationModelFileWebbase = loaded_config.getString("resources.umbc.model_r_webbase")
  Sim_Service_Config.conceptModelFileGigaWords = loaded_config.getString("resources.umbc.model_c_giga")
  Sim_Service_Config.relationModelFileGigaWords = loaded_config.getString("resources.umbc.model_r_giga")
  Sim_Service_Config.posTaggerModelFile =  loaded_config.getString("resources.umbc.model_pos")

}