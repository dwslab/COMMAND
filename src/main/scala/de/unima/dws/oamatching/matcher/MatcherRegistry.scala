package de.unima.dws.oamatching.matcher

import com.github.tototoshi.csv.CSVReader
import com.wcohen.ss._
import com.wcohen.ss.tokens.SimpleTokenizer
import de.unima.dws.alex.simservice.SimService
import de.unima.dws.oamatching.config.Config
import de.unima.dws.oamatching.core.matcher.{Matcher, StructuralLevelMatcher}
import de.unima.dws.oamatching.matcher.elementlevel.{PreProcessedStringMatcher, SimpleStringFunctionMatcher, TokenizedStringMatcher, TrainedSecondStringMatcher}
import de.unima.dws.oamatching.matcher.structurallevel._
import de.unima.dws.oamatching.measures.{UMBCSimilarity, SemanticMeasures, StringMeasureHelper, StringMeasures}
import fr.inrialpes.exmo.ontosim.string.StringDistances
import scala.collection.mutable.{Map => MutableMap}

object MatcherRegistry {

  //testing
  val KEY_SIM = "sim"
  val KEY_DIS = "dis"

  val KEY_ELEM = "E"
  //element level matcher that do only need a string fct
  val KEY_STRUCT = "S"
  // key structural matcher
  val KEY_TRAINED = "T" //key for second string matcher

  val KEY_STEMMED = "stemmed"
  val KEY_LEMMATIZED = "lemma"

  //element level matcher
  var matcher_by_name: Map[String, Matcher] = Map[String, Matcher]()


  //structural level matcher
  var structural_matcher_by_name: Map[String, StructuralLevelMatcher] = Map[String, StructuralLevelMatcher]()


  /**
   * Init the available matcher from a config file
   * @param path
   */
  def initFromConfigFile(path: String) = {

    val reader = CSVReader.open(path)

    val mapped_values = reader.allWithHeaders.foreach(row => {
      val matcher_name = row.get("name").get
      val matcher_technique = row.get("technique").get
      val matcher_type = row.get("type").get
      val sim_dis = row.get("sim_type").getOrElse("sim")
      val tokenized = row.get("tokenized").getOrElse("false").toBoolean
      val string_norm = row.get("normalized").getOrElse("none")
      val stop_filter = row.get("stopfilter").getOrElse("false").toBoolean

      val use_label = row.get("use_label").getOrElse("false").toBoolean
      val use_fragment= row.get("use_fragment").getOrElse("false").toBoolean
      val use_comment = row.get("use_comment").getOrElse("false").toBoolean

      val matcher = initMatcher(matcher_technique, matcher_type, sim_dis, tokenized, string_norm, stop_filter,use_label,use_fragment, use_comment)

      if (matcher.isDefined) {
        if (matcher_type.equals(KEY_STRUCT)) {
          structural_matcher_by_name += ((matcher_name, matcher.get.asInstanceOf[StructuralLevelMatcher]))
        } else {
          matcher_by_name += ((matcher_name, matcher.get))
        }
      }
    })

    println("Initialized: " + (matcher_by_name.size + structural_matcher_by_name.size))
  }

  def initMatchers() = {
    initFromConfigFile(Config.PATH_TO_MATCHER_CONFIG)
  }

  def initMatcher(matcher_name: String, matcher_type: String, sim_dis: String, tokenized: Boolean, string_norm: String, stop_filter: Boolean, use_label: Boolean, use_fragment: Boolean, use_comment: Boolean): Option[Matcher] = {

    val matcher = matcher_type match {
      case "E" => Option(initElementLevelMatcher(matcher_name, sim_dis, tokenized, string_norm, stop_filter, use_label,use_fragment,use_comment))
      case "S" => Option(initStructuralMatcher(matcher_name))
      case "T" => Option(initTrainedMatcher(matcher_name,use_label,use_fragment,use_comment))
      case "P" => Option(initPreprocessedMatcher(matcher_name, sim_dis, tokenized, string_norm, stop_filter,use_label,use_fragment,use_comment))
      case _ => Option.empty
    }
    matcher
  }

  def initPreprocessedMatcher(matcher_name: String, sim_dis: String, tokenized: Boolean, string_norm: String, stop_filter: Boolean, use_label: Boolean, use_fragment: Boolean, use_comment: Boolean): Matcher = {


    def measure_fct = get_string_matching_function(matcher_name)

    var preprocess: (String) => String = null
    val tokenizer = StringMeasureHelper.tokenize_combined_all
    val tokens_to_string = StringMeasureHelper.token_list_to_String _

    if (tokenized) {

      preprocess = tokens_to_string compose StringMeasureHelper.stemMultiple _ compose tokenizer compose StringMeasureHelper.minimalPreprocess
    } else {
      preprocess = StringMeasureHelper.minimalPreprocess
    }

    new PreProcessedStringMatcher(true, use_label, use_fragment, use_comment, preprocess, measure_fct)

  }

  /**
   * Inits an Element level Matcher based on the given configuration
   * @param matcher_name
   * @param sim_dis
   * @param tokenized
   * @param string_norm
   * @return Matcher initialized
   */
  def initElementLevelMatcher(matcher_name: String, sim_dis: String, tokenized: Boolean, string_norm: String, stop_filter: Boolean, use_label: Boolean, use_fragment: Boolean, use_comment: Boolean): Matcher = {
    val is_similarity = sim_dis match {
      case KEY_DIS => false
      case KEY_SIM => true
      case _ => true
    }
    val tokenizer: (String) => List[String] = StringMeasureHelper.tokenize_combined_all
    val stemmed_tokenizer: (String) => List[String] = StringMeasureHelper.stemMultiple _ compose tokenizer
    val stop_filtered_tokenizer = StringMeasureHelper.stopWordFilter _ compose tokenizer
    val stop_filtered_stemmed_tokenizer = StringMeasureHelper.stemMultiple _ compose stop_filtered_tokenizer

    val lemmatized_tokenizer: (String) => List[String] = StringMeasureHelper.lemmatizeMultiple _ compose tokenizer
    val stop_filtered_lemmatized_tokenizer = StringMeasureHelper.lemmatizeMultiple _ compose stop_filtered_tokenizer


    /*
        string_norm match {
          case KEY_STEMMED => new TokenizedStringMatcher(is_similarity, StringMeasureHelper.minimalPreprocess, stemmed_tokenizer, measure_fct)
          case _ => new TokenizedStringMatcher(is_similarity, StringMeasureHelper.minimalPreprocess, tokenizer, measure_fct)
        }
     */

    //get function
    val measure_fct: (String, String) => Double = get_string_matching_function(matcher_name)

    tokenized match {
      case false => new SimpleStringFunctionMatcher(is_similarity, use_label, use_fragment, use_comment, measure_fct)
      case true => {
        stop_filter match {
          case true => string_norm match {
            case KEY_STEMMED => new TokenizedStringMatcher(is_similarity, use_label, use_fragment, use_comment, StringMeasureHelper.remove_punctuation, stop_filtered_stemmed_tokenizer, measure_fct)
            case KEY_LEMMATIZED => new TokenizedStringMatcher(is_similarity, use_label, use_fragment, use_comment, StringMeasureHelper.remove_punctuation, stop_filtered_stemmed_tokenizer, measure_fct)
            case _ => new TokenizedStringMatcher(is_similarity, use_label, use_fragment, use_comment, StringMeasureHelper.remove_punctuation, stop_filtered_tokenizer, measure_fct)
          }
          case false => string_norm match {
            case KEY_STEMMED => new TokenizedStringMatcher(is_similarity, use_label, use_fragment, use_comment, StringMeasureHelper.remove_punctuation, stemmed_tokenizer, measure_fct)
            case KEY_LEMMATIZED => new TokenizedStringMatcher(is_similarity, use_label, use_fragment, use_comment, StringMeasureHelper.remove_punctuation, stemmed_tokenizer, measure_fct)
            case _ => new TokenizedStringMatcher(is_similarity, use_label, use_fragment, use_comment, StringMeasureHelper.remove_punctuation, tokenizer, measure_fct)
          }
        }
      }
    }

  }

  /**
   * Return an newly initialized structural Level Matcher
   * @param matcher_name
   * @return
   */
  def initStructuralMatcher(matcher_name: String): StructuralLevelMatcher = {
    matcher_name match {
      case "graphBasedUsedPropMatcher" => new GraphBasedUsedPropertyMatcher()
      case "graphBasedUsedClassMatcher" => new GraphBasedUsedClassMatcher()
      case "propertiesMatcher" => new PropertiesMatcher()
      case "neighborhoodMatcher" => new NeighborHoodSimilarityMatcher(NeighborHoodSimilarityMatcher.STRATEGY_MIN)
    }
  }

  /**
   * Inits an Second String based matcher ignores all other parameters
   * @param matcher_name
   * @return
   */
  def initTrainedMatcher(matcher_name: String, use_label: Boolean, use_fragment: Boolean, use_comment: Boolean): Matcher = {
    val tokenizer = StringMeasureHelper.tokenize_combined_all
    val tokens_to_string = StringMeasureHelper.token_list_to_String _
    val simple_preprocessing = StringMeasureHelper.to_lower_case_single _ compose tokens_to_string compose tokenizer compose StringMeasureHelper.porter_stem compose StringMeasureHelper.minimalPreprocess

    val kernel_second_string_fct = getSecondTrainingFunction(matcher_name)
    new TrainedSecondStringMatcher(true,use_label, use_fragment, use_comment, simple_preprocessing, kernel_second_string_fct )
  }


  /**
   * @param measure
   * @return
   */
  def get_string_matching_function(measure: String): (String, String) => Double = {

    measure match {
      case "hammingDistance" => StringMeasureHelper.measure_ignored_punctuation_lower_cased(StringDistances.hammingDistance)
      case "jaroWinklerMeasure" => StringMeasureHelper.measure_lower_cased(StringMeasures.computeJaroWinkler)
      case "jaroMeasure" => StringMeasureHelper.measure_lower_cased(StringMeasures.computeJaro)
      case "levenshteinDistance" => StringMeasureHelper.measure_ignored_punctuation_lower_cased(StringDistances.levenshteinDistance)
      case "needlemanWunsch2Distance" => StringMeasureHelper.measure_lower_cased(StringDistances.needlemanWunsch2Distance)
      case "ngramDistance" => StringMeasureHelper.measure_ignored_punctuation_lower_cased(StringDistances.ngramDistance)
      case "smoaDistance" => StringMeasureHelper.measure_ignored_punctuation_lower_cased(StringDistances.smoaDistance)
      case "subStringDistance" => StringMeasureHelper.measure_ignored_punctuation_lower_cased(StringDistances.subStringDistance)
      case "equalDistance" => StringMeasureHelper.measure_ignored_punctuation_lower_cased(StringDistances.equalDistance)
      case "equalSimilarity" => StringMeasureHelper.measure_ignored_punctuation_lower_cased(StringMeasures.computeEquality)
      case "word2Vec" => StringMeasureHelper.measure_lower_cased(SemanticMeasures.word2VecSimilarityMeasure)
      case "word2VecStemmed" => StringMeasureHelper.measure_lower_cased(SemanticMeasures.word2VecSimilarityMeasureStemmed)
      case "umbcSim" => StringMeasureHelper.measure_lower_cased(UMBCSimilarity.umbcSim)
      case "umbcphrasesim" => UMBCSimilarity.umbcPhraseSim
      case "umbcstssim" => StringMeasureHelper.measure_lower_cased(SemanticMeasures.callSTSServiceUMBC)
      case "fuzzyjaccard" => StringMeasures.computeFuzzyJaccard
      case "fuzzycosine" => StringMeasures.computeFuzzyCosine
      case "fuzzydice" => StringMeasures.computeFuzzyDice
      case "jaccard" => StringMeasureHelper.measure_lower_cased(StringMeasures.computeJaccard)
      case "mongeElkan" => StringMeasureHelper.measure_lower_cased(StringMeasures.computeMongeElkan)
      case "prefix" => StringMeasureHelper.measure_ignored_punctuation_lower_cased(StringMeasures.computePrefixBiDirectional)
      case "suffix" => StringMeasureHelper.measure_ignored_punctuation_lower_cased(StringMeasures.computeSuffixBiDirectional)
      case "lin" => StringMeasureHelper.measure_lower_cased(StringMeasures.computeLin)
      case "path" => StringMeasureHelper.measure_lower_cased(StringMeasures.computePath)
      case "jiangConrath" =>  StringMeasureHelper.measure_lower_cased(StringMeasures.computeJiangConrath)
      case "wuPalmer" =>  StringMeasureHelper.measure_lower_cased(StringMeasures.computeWuPalmer)
      case "umls" => StringMeasureHelper.measure_ignored_punctuation_lower_cased(SemanticMeasures.isUMLSSynonym)
      case _ => StringMeasureHelper.measure_lower_cased(StringDistances.equalDistance) //default is equal distance
    }
  }

  def getSecondTrainingFunction(second_string_measure: String) = {
    second_string_measure match {
      case "simple_tfidf" => new TFIDF(new SimpleTokenizer(true, false))
      case "soft_tfidf_jaro" => new SoftTFIDF(new SimpleTokenizer(true, false), new JaroWinkler(), 0.9)
      case "jaro_winkler_tfidf" => new JaroWinklerTFIDF()
      case "level2_jaro" => new Level2Jaro()
      case "level2_jara_winkler" => new Level2JaroWinkler()
      case "level2_monge_elkan" => new Level2MongeElkan()
      case _ => new TFIDF(new SimpleTokenizer(true, false))

    }
  }


  /**
   * @param name
   * @return
   */
  def getMatcherByName(name: String): Option[Matcher] = {
    matcher_by_name.get(name)
  }

}