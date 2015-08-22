package de


import de.unima.dws.oamatching.core._

import scala.xml.{Node, NodeSeq, Text, XML}

/**
 * Created by mueller on 22/08/15.
 */
object Test_parsing extends App {
  //val xml = XML.loadFile("/Users/mueller/Coding/COMMAND/ontos/2014/conference/reference-alignment/cmt-confOf.rdf")
  val xml = XML.loadFile("/Users/mueller/Coding/COMMAND/ontos/2014/benchmarks/103/refalign.rdf")

  val cells = xml \\ "Cell"

  def get_entity_uri(entity: Node): String = {
    (entity \ "@{http://www.w3.org/1999/02/22-rdf-syntax-ns#}resource").toString
  }

  def isTextNode(node: Node): Boolean = {
    val filtered = node.child.filter { childNode => !childNode.isInstanceOf[Text] }

    filtered.isEmpty
  }

  def extract_onto_nam_space(node_name_space: NodeSeq): Option[String] = {
    if (node_name_space.size > 0) {
      try {
        val tmp_ont_node = node_name_space(0)
        val onto_namespace = if (isTextNode(tmp_ont_node)) {
          tmp_ont_node.text.toString
        } else {
          (tmp_ont_node(0) \ "Ontology" \ "@{http://www.w3.org/1999/02/22-rdf-syntax-ns#}about")(0).toString()
        }
        Option(onto_namespace)
      } catch {
        case e: Exception => Option.empty
      }

    } else {
      Option.empty
    }
  }

  cells.map(cell => {
    val entity1 = (cell \ "entity1")(0)
    val entity2 = (cell \ "entity1")(0)

    val measure = (cell \ "measure")(0).text.toString().toDouble

    val relation = (cell \ "relation")(0).text.toString()
    MatchingCell(get_entity_uri(entity1), get_entity_uri(entity2), 0.0, relation, Cell.TYPE_UNKOWN, Alignment.TYPE_NONE)
  })


  val alignment = AlignmentParser.parseRDF("/Users/mueller/Coding/COMMAND/ontos/2014/benchmarks/103/refalign.rdf")

  AlignmentParser.writeRDF(alignment, "tmp/test.rdf")

  val alignment2 = AlignmentParser.parseRDFWithOntos("/Users/mueller/Coding/COMMAND/ontos/2014/benchmarks/103/refalign.rdf","/Users/mueller/Coding/COMMAND/ontos/2014/benchmarks/101/onto.rdf","/Users/mueller/Coding/COMMAND/ontos/2014/benchmarks/103/onto.rdf")

  AlignmentParser.writeRDF(alignment2, "tmp/test2.rdf")




}
