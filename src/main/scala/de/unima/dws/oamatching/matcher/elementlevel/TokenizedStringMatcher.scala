package de.unima.dws.oamatching.matcher.elementlevel

/**
 * Created by mueller on 22/01/15.
 */
class TokenizedStringMatcher(override val similarity:Boolean,
                             override val useLabel: Boolean,override val useFragment: Boolean,override val useComment: Boolean,
                             override val preprocess_function:(String) => String,
                             val tokenizer: String => List[String],
                             override val stringmatching_fct:(String,String) => Double
                             ) extends  PreProcessedStringMatcher(similarity,useLabel,useFragment,useComment, preprocess_function,stringmatching_fct){


  override def score(a: String, b: String): Double = {

    val res  = scoreTokenized( tokenizer(a),tokenizer(b))

    res
  }

  protected def scoreTokenized(tokens_a:List[String], tokens_b:List[String]):Double = {
    var summed_score:Double  =0.0
    var counter:Int = 0

    for (term_a <- tokens_a; term_b <- tokens_b) {
      counter= counter +1
      summed_score = summed_score + pre_processed_score(preprocess_function(term_a), preprocess_function(term_b))
    }
    val res:Double =summed_score / Math.max(tokens_a.length,tokens_b.length)
    if(res > 1.0){
      1.0
    }else {
      res
    }
  }
}
