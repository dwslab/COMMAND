package de.unima.dws.oamatching.pipeline.evaluation

/**
 * Created by mueller on 01/03/15.
 */

import java.io.{File, FileFilter}

class FolderFilter extends FileFilter {
  def accept(file:File):Boolean = {
    return file.isDirectory
  }
}