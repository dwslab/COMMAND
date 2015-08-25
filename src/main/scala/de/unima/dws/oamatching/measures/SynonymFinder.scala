package de.unima.dws.oamatching.measures

/**
 * Created by mueller on 22/08/15.
 */

import java.io.{File, PrintWriter}

import de.unima.dws.oamatching.config.Config
import play.api.libs.json.Json

import scala.collection.mutable
import scala.util.parsing.json.JSONArray

import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._


object SynonymFinder {

  val synonyms: mutable.Map[String, Set[String]] = get_saved_synonyms()

  def get_synonyms_for_word(word: String): Set[String] = {
    if (synonyms.contains(word)) {
      synonyms.get(word).get
    } else {
      val syn_noun = SemanticMeasures.getTopNWordsHacky(word, "NN")
      val syn_verb = SemanticMeasures.getTopNWordsHacky(word, "VB")

      val syn_huge_thesaurus = SemanticMeasures.getSynonymBigHugeThesaurus(word)
      val syns = (syn_noun ++ syn_verb ++ syn_huge_thesaurus).toSet

      synonyms.put(word, syns)
      syns
    }
  }

  def get_syn_overlap_score(term1: String, term2: String): Double = {
    val syns1 = get_synonyms_for_word(term1)
    val syns2 = get_synonyms_for_word(term2)
    calculate_syn_overlap(syns1, syns2)
  }

  def calculate_syn_overlap(syns_word_1: Set[String], syns_word_2: Set[String]): Double = {
    val intersect_size = syns_word_1.intersect(syns_word_2).size
    val union_size = syns_word_1.union(syns_word_2).size

    return math.pow((intersect_size.toDouble / union_size.toDouble), 0.5)
  }

  def get_saved_synonyms(): mutable.Map[String, Set[String]] = {
    val folder = Config.loaded_config.getString("resources.synonymlocation")

    val file_name = "synonyms.json"

    val json_file = new File(folder + File.separator + file_name)

    if (json_file.exists) {
      val lines = scala.io.Source.fromFile(json_file.toString()).mkString
      val json = parse(lines)

      val test2 = for(child <- json.children) yield {
        child.values.asInstanceOf[Map[String,List[String]]]
      }
      val file_content = test2.reduceLeft(_ ++ _).map(tuple => tuple._1 -> tuple._2.toSet)

      collection.mutable.Map(file_content.toSeq: _*)
    } else {
      mutable.Map[String, Set[String]]()
    }
  }

  def save_to_json() = {
    val folder = Config.loaded_config.getString("resources.synonymlocation")

    val file_name = "synonyms.json"

    val payload = compact(render(synonyms))
    val json_file = new File(folder + File.separator + file_name)
    val pw = new PrintWriter(json_file)
    pw.write(payload)

    pw.flush()
    pw.close()
  }
}
