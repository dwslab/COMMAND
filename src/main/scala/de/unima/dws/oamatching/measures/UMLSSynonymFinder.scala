package de.unima.dws.oamatching.measures

import de.unima.dws.oamatching.config.Config

import scala.collection.mutable
import scala.io.Source

/**
 * Created by mueller on 27/04/15.
 */
object UMLSSynonymFinder{


  val synonymMap = parseUMLSDataSet()

  //parse umls

  def parseUMLSDataSet(): mutable.Map[String, Int] = {

    val starttime = System.currentTimeMillis()
    val synGroupMap = mutable.Map[String, Int]()
    val path_to_model_file = Config.loaded_config.getString("measures.umls.path")


    val it_file = Source.fromFile(path_to_model_file).getLines()

    while (it_file.hasNext) {
      val line = it_file.next()
      val input = line.split("\t")


      if (input(2).equals("label")) {
        synGroupMap.put(input(1).trim, input(0).trim.toInt)
      } else {
        //don't care
      }

    }

    println("Total time to parse UMLS input= "  + ((System.currentTimeMillis()-starttime)/1000))

    synGroupMap
  }

  def isSynonym(phrase1:String,phrase2:String):Double = {
    val id1 = synonymMap.get(phrase1)
    val id2 = synonymMap.get(phrase2)

    if (id1.isDefined && id2.isDefined) {

      if(id1.get==id2.get){
        1.0
      }else {
        0.0
      }
    }else {
      0.0
    }
  }


}