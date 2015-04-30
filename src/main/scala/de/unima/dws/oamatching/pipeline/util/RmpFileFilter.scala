package de.unima.dws.oamatching.pipeline.util

import java.io.{File, FileFilter}

/**
 * Created by mueller on 24/02/15.
 */
class RmpFileFilter(val name:String) extends  FileFilter{


  def accept(file:File):Boolean = {
    return file.getName().contains(name)
  }
}

