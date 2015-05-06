package de.unima.dws.oamatching.analysis

import java.io.File

import de.unima.dws.oamatching.config.Config

import scala.xml.transform.{RewriteRule, RuleTransformer}
import scala.xml.{Elem, Node}

/**
 * Created by mueller on 27/01/15.
 */
object XMLTest {
  /*
  val parameters = List(("a", 1), ("b", 2), ("c", 3));

  println("search")

  val xml_file: Elem = scala.xml.XML.loadFile("/Users/mueller/Documents/master-thesis/RapidminerRepo/oacode_only_cluster.rmp")

  val test = (xml_file \\ "_").filter(_.attribute("name").isDefined).filter(node => node.attribute("name").get.toString().equals("ReadVector"))


  val meta_info = (test \ "_").filter(_.attribute("key").isDefined).filter(_.attribute("key").get.toString().equals("data_set_meta_data_information"))

  println(meta_info.theSeq)


  println(xml_file.tail.size);


  val test_param = <parameter value="123" key="0"/>


  println(transformXMLProcess(xml_file, Option.empty[FeatureVector]))
  */

  def transformXMLProcess(path_to_file:String, vector:  Map[String,Int], filename:String): String = {

    val xml_file: Elem = scala.xml.XML.loadFile(path_to_file)

    def getMetaDataParameter: Seq[Node] = {
      List(<parameter key="0" value="left.true.polynominal.attribute"/>,
          <parameter key="1" value="relation.true.binominal.attribute"/>,
          <parameter key="2" value="right.true.polynominal.attribute"/>,
          <parameter key="3" value="owl_type.true.polynominal.attribute"/>,
          <parameter key="4" value="match_type.true.polynominal.attribute"/>)
    }

    def transform_featureVector_toType(start_key: Int, vector: Map[String,Int]): Seq[Node] = {
      //if vector not present return emptyset
      if (vector.isEmpty) {
        Seq()
      } else {
        //get xml elements
        vector.map { case (matcher_name, index) => {
            <parameter key={(index+start_key)+""}  value={matcher_name+".true.real.attribute"} />
        }
        }.toSeq
      }
    }

    object list_rule_transformer extends RewriteRule {
      override def transform(n: Node): Seq[Node] = n match {
        case node@Elem(prefix, "list", attribs, scope, child@_*) => {

          if (attribs.get("key").isDefined) {
            if (attribs.get("key").get.toString().equals("data_set_meta_data_information")) {
              val params = getMetaDataParameter ++ transform_featureVector_toType(5, vector)
             //here insert xml
              Elem(prefix, node.label, attribs, scope, params: _*)
            } else {
              node
            }
          } else {
            node
          }

        }
        case other => other

      }
    }

    object list_transformer extends RuleTransformer(list_rule_transformer)

    object transform_operator_elem extends RewriteRule {
      override def transform(n: Node): Seq[Node] = n match {
        case node@Elem(prefix, "operator", attribs, scope, child@_*) => {

          if (attribs.get("name").isDefined) {
            if (attribs.get("name").get.toString.equals("ReadVector")) {
              list_transformer(node)
            } else {
              node
            }
          } else {
            node
          }

        }
        case other => other
      }
    }

    object operator_transformer extends RuleTransformer(transform_operator_elem)

    object transform_process_elem extends RewriteRule {
      override def transform(n: Node): Seq[Node] = n match {
        case sn@Elem(_, "process", _, _, _*) => operator_transformer(sn)
        case other => other
      }
    }

    object process_transformer extends RuleTransformer(transform_process_elem)

    val file_to_save = process_transformer(xml_file)
    val base_folder = Config.loaded_config.getString("rapidminerconfig.tmp_process_location")

    scala.xml.XML.save(base_folder+File.separator+filename, file_to_save, "UTF-8", true, null)

    base_folder+File.separator+filename
  }

}
