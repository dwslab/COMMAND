import de.unima.dws.algorithms.MWBMatchingAlgorithm
import de.unima.dws.oamatching.core.MatchRelation
import de.unima.dws.oamatching.pipeline.MatchingSelector

import scala.collection.JavaConversions._

/**
 * Created by mueller on 03/04/15.
 */
class MaximumWeightSpec extends UnitSpec {
  //test case from ontology matching book

  val test_case: Map[MatchRelation, Double] = Map(MatchRelation("product", "=", "book", "nn", "nn") -> 0.84,
    MatchRelation("product", "=", "translator", "nn", "nn") -> 0.0,
    MatchRelation("product", "=", "publisher", "nn", "nn") -> 0.9,
    MatchRelation("product", "=", "writer", "nn", "nn") -> 0.12,
    MatchRelation("provider", "=", "book", "nn", "nn") -> 0.12,
    MatchRelation("provider", "=", "translator", "nn", "nn") -> 0.0,
    MatchRelation("provider", "=", "publisher", "nn", "nn") -> 0.84,
    MatchRelation("provider", "=", "writer", "nn", "nn") -> 0.6,
    MatchRelation("creator", "=", "book", "nn", "nn") -> 0.6,
    MatchRelation("creator", "=", "translator", "nn", "nn") -> 0.05,
    MatchRelation("creator", "=", "publisher", "nn", "nn") -> 0.12,
    MatchRelation("creator", "=", "writer", "nn", "nn") -> 0.84)


  val test_case_2: Map[MatchRelation, Double] = Map(MatchRelation("product", "=", "book", "nn", "nn1") -> 0.84,
    MatchRelation("product", "=", "book", "nn", "nn2") -> 0.92,
    MatchRelation("product", "=", "book", "nn", "nn3") -> 0.92,
    MatchRelation("product", "=", "translator", "nn", "nn1") -> 0.0,
    MatchRelation("product", "=", "translator", "nn", "nn2") -> 0.9,
    MatchRelation("product", "=", "translator", "nn", "nn2") -> 0.9,
    MatchRelation("product", "=", "dad", "nn", "nn1") -> 0.0,
    MatchRelation("product", "=", "dad", "nn", "nn2") -> 0.0,
    MatchRelation("product", "=", "dad", "nn", "nn3") -> 0.0,
    MatchRelation("xxx", "=", "xxx", "nn", "nn3") -> 0.0
  )
  "The hungarian method" should "should work for whole integers" in {
    val algo = new MWBMatchingAlgorithm(3, 3)

    algo.setWeight(0, 0, 0.0)
    algo.setWeight(0, 1, 1.0)
    algo.setWeight(0, 2, 0.0)

    algo.setWeight(1, 0, 2.0)
    algo.setWeight(1, 1, 3.0)
    algo.setWeight(1, 2, 0.0)

    algo.setWeight(2, 0, 0.0)
    algo.setWeight(2, 1, 6.0)
    algo.setWeight(2, 2, 1.0)


    val max_sum = algo.getMatching().toList.zipWithIndex.map { case (elem, index) => {
      val value = algo.weights.apply(index).apply(elem) - 1.0
      value
    }
    }.sum

    assert(max_sum == 8)
  }

  it should "should work for whole double values smaller than 1" in {
    val algo = new MWBMatchingAlgorithm(3, 3)

    algo.setWeight(0, 0, 0.0)
    algo.setWeight(0, 1, 0.1)
    algo.setWeight(0, 2, 0.0)

    algo.setWeight(1, 0, 0.2)
    algo.setWeight(1, 1, 0.3)
    algo.setWeight(1, 2, 0.0)

    algo.setWeight(2, 0, 0.0)
    algo.setWeight(2, 1, 0.6)
    algo.setWeight(2, 2, 0.1)


    val max_sum = algo.getMatching().toList.zipWithIndex.map { case (elem, index) => {
      val value = algo.weights.apply(index).apply(elem) - 1.0


      value
    }
    }.sum

    assert(max_sum == 0.8)
  }


  "The Relation Encoder" should "encode the matching relations correctly without duplicates" in {
    val encoded_tuple = MatchingSelector.getIndexEncodedListofClasses(test_case)
    val encoded_left = encoded_tuple._1
    val encoded_right = encoded_tuple._2

    assert(encoded_left.keys.contains("product") == true)
    assert(encoded_left.keys.contains("provider") == true)
    assert(encoded_left.keys.contains("creator") == true)

    assert(encoded_right.keys.contains("book") == true)
    assert(encoded_right.keys.contains("translator") == true)
    assert(encoded_right.keys.contains("publisher") == true)
    assert(encoded_right.keys.contains("writer") == true)
  }

  it should "solve the hungarian algorithms correctly" in {
    val problem = MatchingSelector.prepareHungarianMethodAlgorithm(test_case)

    val max_sum = problem.problem.getMatching().toList.zipWithIndex.map { case (elem, index) => {
      val value = problem.problem.weights.apply(index).apply(elem) - 1.0

      println(s"$index and $elem equal $value")
      value
    }
    }.sum

    assert(max_sum === 2.52 +- 0.05)

  }

  it should "create a proper matching" in {
    val problem = MatchingSelector.prepareHungarianMethodAlgorithm(test_case)

    val matchings = MatchingSelector.solveHungarianMethodProblem(problem, test_case)


  }

  "The selection component " should "select the best matching if for one matching exist multiple matchings with different matching types" in {
    val result = MatchingSelector.getMaxPerMatchType(test_case_2)

    assert(result.size == 4)

    assert(result.filter(_._1.left.contains("xxx")).size == 1)
    assert(result.filter(_._1.left.contains("product")).size == 3)
    assert(result.filter(_._1.right.contains("book")).size == 1)


    assert(result.filter(_._1.right.contains("book")).head._2 == 0.92)
    assert(result.filter(_._1.right.contains("dad")).head._2 == 0.0)
  }
}
