package de.unima.dws.oamatching.measures

import edu.cmu.lti.lexical_db.{ILexicalDatabase, NictWordNet}
import edu.cmu.lti.ws4j.impl.{JiangConrath, Lin, Path, WuPalmer}
import edu.cmu.lti.ws4j.util.WS4JConfiguration

object WordNetMeasureHelper {
  
	WS4JConfiguration.getInstance().setMFS(true)
	val db:ILexicalDatabase  = new NictWordNet()

	val lin:Lin = new Lin(db)
	val jianConrath = new JiangConrath(db)
	val wuPalmer = new WuPalmer(db)
	val path = new Path(db)


}