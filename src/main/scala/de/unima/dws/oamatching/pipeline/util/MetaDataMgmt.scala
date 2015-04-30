package de.unima.dws.oamatching.pipeline.util

import com.redis.{RedisClientPool, RedisClient}
import de.unima.dws.oamatching.config.Config

/**
 * Created by mueller on 29/01/15.
 */
object MetaDataMgmt {
  val redis_client_pool = new RedisClientPool(Config.REDIS_HOST, Config.REDIS_PORT)
  //val redis_client = new RedisClient(Config.REDIS_HOST, Config.REDIS_PORT)

  /**
   * Stores an optimal threshold for a given matcher for a specific data_set, implicite assumes that the data_set name is unique, tobe changed
   * @param data_set name of the dataset
   * @param matcher name of the matcher
   * @param value threshold to be store
   */
  def storeThreshold(data_set:String, matcher:String, value:Double):Unit = {
    redis_client_pool.withClient {
      redis_client => redis_client.hset(Config.REDIS_THRESHOLD_HASH_KEY,data_set+"_"+matcher,value)
    }
  }

  /**
   *
   * @param data_set name of the dataset
   * @param matcher name of the matcher
   * @return threshold of the matcher for the data_set
   */
  def getThreshold(data_set:String, matcher:String):Option[Double] = {

    val result =  redis_client_pool.withClient {
      redis_client =>redis_client.hget[Double](Config.REDIS_THRESHOLD_HASH_KEY, data_set.toLowerCase()+"_"+matcher)(com.redis.serialization.Format.default, com.redis.serialization.Parse.Implicits.parseDouble)
    }


    result
  }
}
