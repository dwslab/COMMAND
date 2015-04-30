package de.unima.dws.oamatching.analysis

import java.io._

import com.github.tototoshi.csv.CSVReader
import com.typesafe.scalalogging.slf4j.LazyLogging
import de.unima.dws.oamatching.config.Config
import de.unima.dws.oamatching.core.MatchRelation
import de.unima.dws.oamatching.measures.{StringMeasureHelper, SemanticMeasures}
import de.unima.dws.oamatching.pipeline.util.{CosineSimilarity, TxtFileFilter}
import de.unima.dws.oamatching.pipeline.{VectorUtil, FeatureVector}
import epic.preprocess.MLSentenceSegmenter
import org.apache.spark.mllib.feature.{Word2VecModel, Word2Vec}
import org.apache.spark.mllib.linalg.distributed.RowMatrix
import org.apache.spark.mllib.linalg.{Vector, Vectors}
import org.apache.spark.mllib.stat.{Statistics, MultivariateStatisticalSummary}
import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkContext, SparkConf}

import scala.collection.immutable.{Iterable, Map}
import scala.io.Source

/**
 * Scala Object does contain all Spark Jobs used in the analysis pipeline
 * Created by mueller on 23/01/15.
 */
object SparkJobs extends  LazyLogging{


  val conf = new SparkConf()
    .setAppName("Alex Master Thesis")
    .setMaster("local[2]")
    .set("spark.executor.memory", "4g")
    //.set("spark.rdd.compress", "true")

  val sc = new SparkContext(conf)
  val word_2_vec_model = loadWord2VecModel(Config.WORD_2_VEC_MODEL_PATH)
  val word_2_vec_model_stemmed = loadWord2VecModel(Config.WORD_2_VEC_STEMMED_MODEL_PATH)

  /**
   * Function to remove correlated features from the feature vector with apache spark capabilites
   * @param feature_vector
   * @param threshold
   * @return
   */
  def removeCorrelatedFeatures(feature_vector: FeatureVector, threshold: Double): FeatureVector = {
    logger.info("start spark job to remove correlated attributes")
    val no_of_matcher: Int = feature_vector.matcher_index_to_name.size

    //now create spark dataset
    val features: RDD[Vector] =    sc.parallelize( feature_vector.transposed_vector.view.map({ case (matchrelation, matchermap) => Vectors.dense(matchermap.values.toArray)}).toList)

    //correlate
    val correlMatrix = Statistics.corr(features)


    //based on the correlMatrix now perform
    //create list of triples in the form (row_index,column_index, correlation)
    val row_column_value_correlMatrix: Array[(Int, Int, Double)] = correlMatrix.toArray.zipWithIndex.map({ case (value, index) => {
      val column_index = (index % no_of_matcher)
      val row_index = Math.floor(index.toDouble / no_of_matcher.toDouble).toInt
      (row_index, column_index, value)
    }
    })

    //get those attributes with the a too high correlation, keep row attribute, remove column attribute
    val to_be_removed_parameters = row_column_value_correlMatrix.filter({ case (row, column, correlation) => (column > row) && correlation >= threshold}).map(triple => triple._2).toSet

    //rebuild feature vector

    val filtered_vector = feature_vector.vector.filterKeys(matcher_name => {
      val matcher_index = feature_vector.matcher_name_to_index(matcher_name)
      !to_be_removed_parameters.contains(matcher_index)
    })

    val filtered_feature_vector = VectorUtil.createVectorFromResult(filtered_vector, feature_vector.data_set_name)

    //println(to_be_removed_parameters)
    logger.info("Total number of parameters to be removed by remove correlated spark Job" + to_be_removed_parameters.size)

    System.gc()

    filtered_feature_vector
  }


  def trainWord2Vec(name: String, path: String): Unit = {


    val file: File = new File("vectortest_train.csv")
    val reader = CSVReader.open(file)

    val sentenceSplitter = MLSentenceSegmenter.bundled().get
    val tokenizer = new epic.preprocess.TreebankTokenizer()

    val text_values: List[Seq[String]] = reader.allWithHeaders.map(tuple => {
      val startime = System.currentTimeMillis()
      val text = tuple.get("text").get.replaceAll("[^\\w\\s]", "");
      val sentences: IndexedSeq[IndexedSeq[String]] = sentenceSplitter(text).map(tokenizer).toIndexedSeq
      val words_seq: Seq[String] = sentences.map(sentence => sentence.map(word => word.toLowerCase)).flatten.toSeq
      words_seq
    });
    reader.close()


    val words_parallel: RDD[Seq[String]] = sc.parallelize(text_values)

    words_parallel.saveAsTextFile("/Users/mueller/Documents/master-thesis/ontologymatcher/" + name + ".txt")


    val word2vec = new Word2Vec()

    word2vec.setVectorSize(100)
    val model: Word2VecModel = word2vec.fit(words_parallel)

    storeWord2VecModel(model, Config.WORD_2_VEC_MODEL_PATH)

    val res = cousineSimilarityBetweenTerms(model, "salad", "veggie")
  }

  def cousineSimilarityBetweenTerms(model: Word2VecModel, term1: String, term2: String): Double = {
    val vec_1: Vector = model.transform(term1)
    val vec_2: Vector = model.transform(term2)
    val cosine_sim = CosineSimilarity.cosineSimilarity(vec_1.toArray, vec_2.toArray)
    cosine_sim
  }


  def loadWord2VecModel(file: String): Word2VecModel = {
    val file_in = new FileInputStream(file)
    val obj_in = new ObjectInputStream(file_in);

    val model: Word2VecModel = obj_in.readObject().asInstanceOf[Word2VecModel]

    obj_in.close()
    file_in.close()

    model
  }

  def storeWord2VecModel(model: Word2VecModel, file: String): Unit = {
    val file_out = new FileOutputStream(file)
    val obj_out = new ObjectOutputStream(file_out)
    obj_out.writeObject(model);
    obj_out.flush()
    obj_out.close()
    file_out.flush()
    file_out.close()
  }


  def readWebBaseFiles(path: String): Unit = {
    val folder: File = new File(path)

    val files = folder.listFiles(new TxtFileFilter)
    val sentenceSplitter = MLSentenceSegmenter.bundled().get
    val tokenizer = new epic.preprocess.TreebankTokenizer()
    var i = 0;
    val training_data_raw: Array[RDD[Seq[String]]] = for (file <- files; if i < 10) yield {

      val rdd_lines: Iterator[Option[Seq[String]]] = for (line <- Source.fromFile(file).getLines) yield {
        if (line.isEmpty) {
          Option.empty
        } else {
          val text = line.replace("\"", "")
          val sentences: IndexedSeq[IndexedSeq[String]] = sentenceSplitter(text).map(tokenizer).toIndexedSeq
          val words_seq = sentences.map(sentence => sentence.map(word => {
            StringMeasureHelper.porter_stem(word.toLowerCase())
          })).flatten.toSeq
          Option(words_seq)
        }
      }

      val filtered_rdd_lines = rdd_lines.filter(line => line.isDefined).map(line => line.get).toList

      println(s"File $i done")
      i = i + 1
      sc.parallelize(filtered_rdd_lines)
    }

    val rdd_file = training_data_raw.toSeq.reduceLeft((A, B) => {
      A.union(B)
    })

    val starttime = System.currentTimeMillis()

    val word2vec = new Word2Vec()

    word2vec.setVectorSize(100)
    val model: Word2VecModel = word2vec.fit(rdd_file)

    println("Training time: " +(System.currentTimeMillis() - starttime))
    storeWord2VecModel(model, Config.WORD_2_VEC_MODEL_PATH)

  }
}
