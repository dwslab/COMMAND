package de

import de.unima.dws.oamatching.measures.SynonymFinder

/**
 * Created by mueller on 22/08/15.
 */
object SynonymTester extends App {
  //val syns = SemanticMeasures.getSynonymBigHugeThesaurus("company")
  //  val test1 = SemanticMeasures.getTopNWordsHacky("lawyer","NN")
  //  val test2 = SemanticMeasures.getTopNWordsHacky("advocate","NN")
  //
  //  val test3 = SemanticMeasures.getTopNWordsHacky("house","NN")
  //  val test4 = SemanticMeasures.getTopNWordsHacky("home","NN")
  //  //print(syns)
  //  for(range<-List.range(0,100000)){
  //    val start_time = System.currentTimeMillis()
  //    val test1 = SemanticMeasures.getTopNWordsHacky("dinner","NN")
  //    val test2 = SemanticMeasures.getTopNWordsHacky("banquet","NN")
  //    val test_val = SynonymFinder.calculate_syn_overlap(test1.toSet,test2.toSet)
  //    print(math.pow(test_val,0.5))
  //    val end_time = System.currentTimeMillis()
  //    println(end_time-start_time)
  //  }
  //
  //  println(SynonymFinder.calculate_syn_overlap(test3.toSet,test4.toSet))

  SynonymFinder.get_syn_overlap_score("bike", "car")
  val start_time = System.currentTimeMillis()

  SynonymFinder.get_syn_overlap_score("house", "hut")
  val middle_time = System.currentTimeMillis()
  println(middle_time - start_time)
  println(SynonymFinder.get_syn_overlap_score("house", "hut"))
  val end_time = System.currentTimeMillis()
  println(end_time - middle_time)
  SynonymFinder.save_to_json()


}
