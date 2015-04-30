import de.unima.dws.oamatching.core.MatchRelation
import de.unima.dws.oamatching.pipeline.VectorUtil

import scala.collection.immutable.Map
import scala.collection.parallel.immutable.ParMap

/**
 * Created by mueller on 29/03/15.
 */
class FeatureSelectionTestCase extends  UnitSpec{
    "The Feature Selection" should " work as expected" in {

      val att_1 = MatchRelation("a","=","b","NN","NN")
      val att_2 = MatchRelation("c","=","d","NN","NN")
      val att_3 = MatchRelation("e","=","f","NN","NN")
      val att_4 = MatchRelation("f","=","g","NN","NN")

      val feature_1 = Map(att_1 -> 0.0, att_2->0.0, att_3->0.0, att_4->0.0)
      val feature_2 = Map(att_1 -> 0.0, att_2->1.0, att_3->1.0, att_4->0.0)
      val feature_3 = Map(att_1 -> 1.0, att_2->1.0, att_3->1.0, att_4->1.0)
      val feature_4 = Map(att_1 -> 0.0, att_2->1.0, att_3->0.0, att_4->0.0)

      val test_result: Map[String, Map[MatchRelation, Double]] = Map("f1" -> feature_1, "f2"-> feature_2, "f3"->feature_3,"f4"->feature_4)

      val test_vector = VectorUtil.createVectorFromResult(test_result, "test")

      val result_vector = VectorUtil.selectFeatures(test_vector)

      assert(result_vector.matcher_name_to_index.get("f1").isDefined == false)
      assert(result_vector.matcher_name_to_index.get("f3").isDefined == false)
      assert(result_vector.matcher_name_to_index.get("f2").isDefined == true)
      assert(result_vector.matcher_name_to_index.get("f4").isDefined == true)
    }
}
