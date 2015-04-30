package de.unima.dws.oamatching.pipeline.util

import com.typesafe.scalalogging.slf4j.LazyLogging
import de.unima.dws.oamatching.config.Config

import scala.collection.mutable

/**
 * Created by mueller on 31/03/15.
 */
object TimeTaker extends LazyLogging {

  val starttimes: mutable.Map[String, Long] = new mutable.HashMap[String, Long]()

  val log_times = Config.loaded_config.getBoolean("general.log_times")

  def takeTime(time_key: String): Long = {
    if (starttimes.contains(time_key)) {
      val starttime = starttimes.get(time_key).get
      val total_time = System.currentTimeMillis() - starttime

      if (log_times) {
        //TODO log
        logger.info(s"Took for $time_key total time: $total_time")
      }
      total_time
    } else {
      val start_time = System.currentTimeMillis()
      starttimes.put(time_key, start_time)
      start_time
    }
  }
}
