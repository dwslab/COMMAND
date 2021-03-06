package de.unima.dws.fuzzytoken

/**
 * Created by mueller on 21/03/15.
 */
class FuzzyCosineMeasure(tokenizer: String => List[String], string_matching: (String, String) => Double) extends  FuzzyTokenMatch(tokenizer, string_matching) {

  /**
   * Implements fuzzy Cosine Similarity
   * f_jaccard = |T_1 union_f T_2| / ( SQRT(|T_1|+|T_2|))
   * @param card_graph
   * @param card_a
   * @param card_b
   * @return
   */
  override def computeSimilarity(card_graph: Double, card_a: Double, card_b: Double): Double = {
    card_graph/(Math.sqrt(card_a*card_b))
  }

}
