package de.unima.dws.oamatching.pipeline.command

import de.unima.dws.oamatching.pipeline.CommandRun

/**
 * Starting Point for an OAEI Run
 */
object RunOAEIRound extends App {
  val config = CommandRun.parseRunConfig()
  CommandRun.runRound(config)
}