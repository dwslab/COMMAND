package de.unima.dws.oamatching.pipeline.command

import com.typesafe.scalalogging.slf4j.LazyLogging
import de.unima.dws.oamatching.analysis.{SparkJobs, RapidminerJobs}
import de.unima.dws.oamatching.pipeline.CommandRun

/**
 * Runs a single Platform combination
 */
object RunSinglePipeline extends App with LazyLogging {
  val config = CommandRun.parseRunOneProblemConfig()
  logger.info("Start COMMAND for Matching Problem" + config.matchingProblem.name)
  val eval_result = CommandRun.runSingleProblem(config)
  logger.info("Done with COMMAND for Matching Problem" + config.matchingProblem.name)
  if (eval_result.isDefined) {
    logger.info("We have evaluated the result!")
    logger.info("Result: " + eval_result.get.toString)
  }
  logger.info("Alignment was written to file:" + " alignments/" + config.matchingProblem.name + ".rdf")


  RapidminerJobs.quit()
  SparkJobs.sc.stop()


}
