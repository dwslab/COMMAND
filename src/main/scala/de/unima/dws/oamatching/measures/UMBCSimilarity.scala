package de.unima.dws.oamatching.measures

import de.unima.dws.alex.simservice.SimService

/**
 * Created by mueller on 30/04/15.
 */
object UMBCSimilarity {
  var semantic_sim: SimService = SimService.createSimService(SimService.MODEL_WEBBASE)

  def umbcPhraseSim(phrase1: String, phrase2: String): Double = {

    try {
      semantic_sim.getSimilarity(phrase1, phrase2, false)
    }
    catch {
      case e: Throwable => {
        //println(e)
        //logger.error(s"error at umbc phrase sim: $phrase1, $phrase2", e)
        0.0
      }
    }

  }


  def umbcSim(term1: String, term2: String): Double = {
    try {
      val res = semantic_sim.getSimilarity(term1, term2, true)
      res
    }
    catch {
      case e: Throwable => {
        //logger.info("error at umbc sim",e)
        0.0
      }
    }
  }

  def umbcPosSim(term1: String, term2: String): Double = {
    semantic_sim.getPosSimilarity(term1, term2, true)
  }
}
