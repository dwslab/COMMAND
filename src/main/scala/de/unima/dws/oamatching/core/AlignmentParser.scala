package de.unima.dws.oamatching.core

import java.io.{File, InputStream, PrintWriter}
import java.net.URI

import com.github.tototoshi.csv.CSVWriter
import com.hp.hpl.jena.rdf.model._
import com.hp.hpl.jena.rdf.model.impl.ResourceImpl
import com.hp.hpl.jena.util.FileManager
import de.dwslab.alcomox.ontology.IOntology
import org.semanticweb.owlapi.model.{OWLLiteral, OWLOntology}

import scala.collection.JavaConversions._
import scala.collection.convert.Wrappers.JIteratorWrapper
import scala.collection.immutable
import scala.xml._

/**
 * Created by mueller on 22/01/15.
 */
object AlignmentParser {
  /**
   * Parsing an RDF based alignment
   * @param path_to_alignment
   * @return
   */
  def parseRDF(path_to_alignment: String): Alignment = {


    val xml = XML.loadFile(path_to_alignment)

    val cells = xml \\ "Cell"

    def get_entity_uri(entity: Node): String = {
      (entity \ "@{http://www.w3.org/1999/02/22-rdf-syntax-ns#}resource").toString
    }

    val correspondences = cells.map(cell => {
      val entity1 = (cell \ "entity1")(0)
      val entity2 = (cell \ "entity1")(0)

      val measure = (cell \ "measure")(0).text.toString().toDouble

      val relation = (cell \ "relation")(0).text.toString().replaceAll("\\s", "")
      MatchingCell(get_entity_uri(entity1), get_entity_uri(entity2), 0.0, relation, Cell.TYPE_UNKOWN, Alignment.TYPE_NONE)
    }).toList

    val ont1_namespace_nodes = xml \\ "onto1"

    val ont1_namespace = extract_onto_name_space(ont1_namespace_nodes)

    val ont2_namespace_nodes = xml \\ "onto2"

    val ont2_namespace = extract_onto_name_space(ont2_namespace_nodes)


    new Alignment(ont1_namespace.getOrElse("onto1"), ont2_namespace.getOrElse("onto2"), correspondences)
  }

  def get_entity_uri(entity: Node): String = {
    (entity \ "@{http://www.w3.org/1999/02/22-rdf-syntax-ns#}resource").toString
  }

  def isTextNode(node: Node): Boolean = {
    val filtered = node.child.filter { childNode => !childNode.isInstanceOf[Text] }

    filtered.isEmpty
  }

  def extract_onto_name_space(node_name_space: NodeSeq): Option[String] = {
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


  def parseRDFWithOntos(path_to_alignment: String, onto1: OWLOntology, onto2: OWLOntology): Alignment = {


    val onto1_obj_properties: Vector[String] = onto1.getObjectPropertiesInSignature().toVector.map(property => property.getIRI.toString).toVector
    val onto2_obj_properties: Vector[String] = onto2.getObjectPropertiesInSignature().toVector.map(property => property.getIRI.toString).toVector

    val onto1_data_properties: Vector[String] = onto1.getDataPropertiesInSignature().toVector.map(property => property.getIRI.toString).toVector
    val onto2_data_properties: Vector[String] = onto2.getDataPropertiesInSignature().toVector.map(property => property.getIRI.toString).toVector

    val onto1_classes: Vector[String] = onto1.getClassesInSignature().map(o_class => o_class.getIRI.toString).toVector
    val onto2_classes: Vector[String] = onto2.getClassesInSignature().map(o_class => o_class.getIRI.toString).toVector


    val simple_alignment = parseRDF(path_to_alignment)


    val correspondences = simple_alignment.correspondences.map(cell => {
      val entity1= cell.entity1
      val entity2= cell.entity2

      val cell_type = if (onto1_classes.contains(entity1.toString) && onto2_classes.contains(entity2.toString)) {
        Cell.TYPE_CLASS
      } else if (onto1_obj_properties.contains(entity1.toString) && onto2_obj_properties.contains(entity2.toString)) {
        Cell.TYPE_OBJECT_PROPERTY
      } else if (onto1_data_properties.contains(entity1.toString) && onto2_data_properties.contains(entity2.toString)) {
        Cell.TYPE_DT_PROPERTY
      } else {
        // individuals
        Cell.TYPE_UNKOWN
      }

      MatchingCell(entity1.toString, entity2.toString, cell.measure, cell.relation.trim().replaceAll("\\s", ""), cell_type, Alignment.TYPE_NONE)
    }).toList

    val i_onto1 = new IOntology(onto1)
    val i_onto2 = new IOntology(onto2)

    new Alignment(simple_alignment.onto1, simple_alignment.onto2, null, null, i_onto1, i_onto2, correspondences)
  }

  /**
   * Add the relation owl type to the ontologies
   * @param path_to_alignment
   * @param path_to_onto1
   * @param path_to_onto2
   * @return
   */
  def parseRDFWithOntos(path_to_alignment: String, path_to_onto1: String, path_to_onto2: String): Alignment = {

    //parse ontos and make the IRIs of the classes and properties random access available
    val onto1: OWLOntology = OntologyLoader.load(path_to_onto1)
    val onto2: OWLOntology = OntologyLoader.load(path_to_onto2)

    parseRDFWithOntos(path_to_alignment, onto1, onto2)

  }

  def getOntoProperty(alignment_parent: Resource, alignment_onto2_query: Property): String = {

    val result = alignment_parent.listProperties(alignment_onto2_query).toList

    val onto2_namespace = if (result.size() > 0) {


      if (result.head.isInstanceOf[OWLLiteral]) {
        result.head.getLiteral.toString
      } else {
        if (result.head.getObject.isResource) {
          result.head.getObject().asResource().toString
        } else {
          ""
        }
      }


    } else {
      "nn"
    }

    onto2_namespace
  }

  /**
   * Parsing an RDF based alignment
   * @param alignment_file
   * @return
   */
  def parseRDF(alignment_file: File): Alignment = {
    parseRDF(alignment_file.toURI.toString)
  }


  def writeRDF(alignment: Alignment, file: String): Unit = {

    def getCell(id: Int, entity1: String, entity2: String, measure: Double, relation: String): Elem = {
      <map>
        <Cell>
          <entity1 rdf:resource={entity1}/>
          <entity2 rdf:resource={entity2}/>
          <measure rdf:datatype='xsd:float'>
            {measure}
          </measure>
          <relation>
            {relation}
          </relation>
        </Cell>
      </map>
    }

    def getRDFDoc(content: Elem): Elem = <rdf:RDF xmlns='http://knowledgeweb.semanticweb.org/heterogeneity/alignment'
                                                  xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#'
                                                  xmlns:xsd='http://www.w3.org/2001/XMLSchema#'>
      {content}
    </rdf:RDF>

    def getAlignment(onto1: Elem, onto2: Elem, cells: immutable.Seq[Elem]): Elem = {
      <Alignment>
        <xml>yes</xml>
        <level>0</level>
        <type>??</type>
        <onto1>
          {onto1}
        </onto1>
        <onto2>
          {onto2}
        </onto2>{cells}
      </Alignment>
    }

    def getOnto(about: String, location: String): Elem =
      <Ontology rdf:about={about}>
        <location>
          {location}
        </location>
      </Ontology>
    val cells = alignment.correspondences.toList.zipWithIndex.map { case (cell, index) =>
      getCell(index + 1, cell.entity1.toString, cell.entity2.toString, cell.measure, cell.relation)
    }
    val xml_cells: immutable.Seq[Elem] = collection.immutable.Seq(cells: _*)

    val onto1 = getOnto(alignment.onto1, "")

    val xml_onto1: Elem = getOnto(alignment.onto1, "")
    val xml_onto2: Elem = getOnto(alignment.onto2, "")


    val xml_alignment = getAlignment(xml_onto1, xml_onto2, xml_cells)

    val rdf_doc = getRDFDoc(xml_alignment)
    scala.xml.XML.save(file, rdf_doc, "UTF-8", true, null)

  }


  def writeFalseNegativesAnalysis(alignment: Alignment, reference: Alignment, name: String, selected_matchings: Map[MatchRelation, Double], raw_matchings: Map[MatchRelation, Double], threshold: Double): Unit = {
    val testFile = new PrintWriter("tmp/falsenegatives" + File.separator + name + "fn_log.txt", "UTF-8")
    val falseNegatives = alignment.getFalseNegatives(reference)

    //get best result for false negative
    val best_false_negative_result: Map[MatchingCell, (MatchRelation, Double)] = falseNegatives.map(elem => {
      val filtered = raw_matchings.filter { case (relation, measure) => {
        relation.left.equals(elem.entity1) && relation.right.equals(elem.entity2)
      }
      }
      if (filtered.size > 0) {
        Option(elem -> filtered.maxBy(_._2))
      } else {
        Option.empty
      }
    }).filter(_.isDefined).map(_.get).toMap

    falseNegatives.foreach(false_negative_cell => {
      //loop over all false negatives
      val res = best_false_negative_result.get(false_negative_cell)
      if (res.isDefined) {
        val negative_result = if (res.isDefined) res.get._2 else 0.0
        val rights = selected_matchings.filter { case (relation, measure) => {
          relation.right.equals(false_negative_cell.entity2)
        }
        }

        testFile.println("##############################")
        testFile.println("Analyse fn " + false_negative_cell)
        testFile.println(s"Is in Alignment? " + alignment.containsCorrespondence(false_negative_cell.entity1, false_negative_cell.entity2))
        testFile.println(s"Best false negative result $negative_result")

        rights.foreach(right_elem => {

          val score = right_elem._2 - negative_result
          testFile.println(s"Difference $score to " + right_elem._1)
        })
        val lefts = selected_matchings.filter { case (relation, measure) => {
          relation.left.equals(false_negative_cell.entity1)
        }
        }
        lefts.foreach(right_elem => {

          val score = right_elem._2 - negative_result
          testFile.println(s"Difference $score to " + right_elem._1)
        })
      }
    })

    testFile.flush()
    testFile.close()
  }


  def writeFalseNegativesToCSV(alignment: Alignment, reference: Alignment, name: String): Unit = {
    val falseNegatives = alignment.getFalseNegatives(reference)

    val csv_file = new File("tmp/falsenegatives" + File.separator + name + "_fn.csv")

    // println(csv_file.getAbsolutePath)
    if (!csv_file.exists()) {
      csv_file.createNewFile()
    }
    val writer = CSVWriter.open(csv_file)

    //print Headline
    val header: List[String] = List[String]("left", "relation", "right", "owl_type", "measure")


    writer.writeRow(header)
    falseNegatives.foreach(cell => {
      val row = List(cell.entity1, cell.relation, cell.entity2, cell.owl_type, cell.measure.toString)
      writer.writeRow(row)
    })

    writer.flush()
    writer.close()
  }

  def writeFalsePositivesToCSV(alignment: Alignment, reference: Alignment, name: String): Unit = {
    val falsePositives = alignment.getFalsePositives(reference)

    val csv_file = new File("tmp/falsenegatives" + File.separator + name + "_fp.csv")

    //println(csv_file.getAbsolutePath)
    if (!csv_file.exists()) {
      csv_file.createNewFile()
    }
    val writer = CSVWriter.open(csv_file)

    //print Headline
    val header: List[String] = List[String]("left", "relation", "right", "owl_type", "measure")


    writer.writeRow(header)
    falsePositives.foreach(cell => {
      val row = List(cell.entity1, cell.relation, cell.entity2, cell.owl_type, cell.measure.toString)
      writer.writeRow(row)
    })

    writer.flush()
    writer.close()
  }

  def writeTruePositivesToCSV(alignment: Alignment, reference: Alignment, name: String): Unit = {
    val truePositives = alignment.getTruePositives(reference)

    val csv_file = new File("tmp/falsenegatives" + File.separator + name + "_tp.csv")

    //println(csv_file.getAbsolutePath)
    if (!csv_file.exists()) {
      csv_file.createNewFile()
    }
    val writer = CSVWriter.open(csv_file)

    //print Headline
    val header: List[String] = List[String]("left", "relation", "right", "owl_type", "measure")


    writer.writeRow(header)
    truePositives.foreach(cell => {
      val row = List(cell.entity1, cell.relation, cell.entity2, cell.owl_type, cell.measure.toString)
      writer.writeRow(row)
    })

    writer.flush()
    writer.close()
  }


}
