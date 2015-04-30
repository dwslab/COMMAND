package de.unima.dws.oamatching.matcher.elementlevel

/**
 * Created by mueller on 22/01/15.
 */
class PreProcessedStringMatcher(override val similarity:Boolean,
                                override val useLabel: Boolean,
                                override val useFragment: Boolean,
                                override val useComment: Boolean,
                                override val preprocess_function:(String) => String,
                                val stringmatching_fct:(String,String) => Double ) extends  PreProcessedMatcher(similarity, useLabel,useFragment,useComment, preprocess_function){
  override def pre_processed_score(entity1: String, entity2: String): Double = {
    if(cache.contains(entity1+"##"+entity2)){
      cache.get(entity1+"##"+entity2).get
    }else {
     val score =  stringmatching_fct(entity1, entity2)
      cache.put(entity1+"##"+entity2,score)
      score
    }

  }
}
