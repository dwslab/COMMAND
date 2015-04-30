package de.unima.dws.oamatching.pipeline

import com.typesafe.scalalogging.slf4j.LazyLogging
import de.unima.alcomox.mapping.{Correspondence, Mapping, SemanticRelation}
import de.unima.alcomox.ontology.IOntology
import de.unima.alcomox.{ExtractionProblem, Settings}
import de.unima.dws.oamatching.core._

import scala.collection.JavaConversions._

/**
 * Implements some post matching pruning techniques
 * Created by mueller on 23/01/15.
 */
object MatchingPruner extends LazyLogging {

  //Debugging settings
  Settings.BLACKBOX_REASONER = Settings.BlackBoxReasoner.PELLET
  Settings.ONE_TO_ONE = false
  Settings.REMOVE_INDIVIDUALS = true

  /**
   * Simple namespace filter,only return matching which ids are start with the once mentioned in the allowedNamespaces list
   * @param matchings matchings to filter
   * @param allowedNameSpaces list of allowed OWL namespaces
   * @return
   */
  def nameSpaceFilter(matchings: Map[MatchRelation, Double], allowedNameSpaces: List[String]): Map[MatchRelation, Double] = {
    def checkIfContainsPrefix(name: String, prefixes: List[String]): Boolean = {
      val counter: Int = prefixes.map(prefix => name.startsWith(prefix)).count(is_prefix => is_prefix == true)
      counter > 0
    }
    //filter for return
    matchings.filter { case (relation, double) => {
      (checkIfContainsPrefix(relation.left, allowedNameSpaces) && checkIfContainsPrefix(relation.right, allowedNameSpaces))

    }
    }

  }

  /**
   * Simple namespace filter,only return matching which ids are start with the once mentioned in the allowedNamespaces list
   * @param vector vector to filter
   * @param allowedNameSpaces list of allowed OWL namespaces
   * @return
   */
  def featureVectorNameSpaceFilter(vector: FeatureVector, allowedNameSpaces: List[String]): FeatureVector = {
    def checkIfContainsPrefix(name: String, prefixes: List[String]): Boolean = {
      val counter: Int = prefixes.map(prefix => name.startsWith(prefix)).count(is_prefix => is_prefix == true)
      counter > 0
    }

    //filter for return
    val filtered = vector.vector.map { case (matcher, matchings) => {
      val filtered_matchings = matchings.filter { case (relation, double) => {
        (checkIfContainsPrefix(relation.left, allowedNameSpaces) && checkIfContainsPrefix(relation.right, allowedNameSpaces))
      }
      }
      (matcher, filtered_matchings)
    }
    }

    VectorUtil.createVectorFromResult(filtered, vector.data_set_name)

  }

  /**
   * Removes incoherent correspondences from the alignment based on alcomo
   * @param alignment
   * @return
   */
  def debugAlignment(alignment: Alignment): Alignment = {


    val owlTypeMap: Map[String, String] = alignment.correspondences.map(cell => {
      val key = cell.entity1 + "=" + cell.entity2
      val value = cell.owl_type
      key -> value
    }).toMap

    val mapping = convertAlignmentToMapping(alignment)

    val ep = new ExtractionProblem(
      ExtractionProblem.ENTITIES_CONCEPTSPROPERTIES,
      ExtractionProblem.METHOD_GREEDY,
      ExtractionProblem.REASONING_COMPLETE
    );

    // attach ontologies and mapping to the problem
    ep.bindSourceOntology( new IOntology(alignment.onto1_reference.path));
    println(alignment.onto1_reference.path)
    println(alignment.onto2_reference.path)
    ep.bindTargetOntology( new IOntology(alignment.onto2_reference.path));
    ep.bindMapping(mapping);


    // solve the problem
    val result = try {
      ep.solve();


      val extracted: Mapping = ep.getExtractedMapping()

      println( ep.getDiscardedMapping.size() + " removed by debugging")
      logger.info("Debugging completed")
      convertMappingToAlignment(extracted, owlTypeMap, alignment)

    }
    catch {
      case e: Throwable => {
        logger.error("Error while Debugging Ontolog", e)
        println("error")
        alignment
      }
    };

    result
  }

  /**
   *
   * Debugs the match relations
   * @param matchings
   * @param onto1
   * @param onto2
   * @return
   */
  def debugMatchRelations(matchings: Map[MatchRelation, Double], onto1:FastOntology, onto2:FastOntology): Map[MatchRelation, Double] = {


    val owlTypeMap: Map[String, String] = matchings.map(tuple => {
      val key = tuple._1.left + "=" +  tuple._1.right
      val value =tuple._1.owl_type
      key -> value
    }).toMap

    val matchTypeMap: Map[String, String] = matchings.map(tuple => {
      val key = tuple._1.left + "=" +  tuple._1.right
      val value =tuple._1.match_type
      key -> value
    }).toMap

    val mapping = convertMatchRelationMapToMapping(matchings)

    val ep = new ExtractionProblem(
      ExtractionProblem.ENTITIES_CONCEPTSPROPERTIES,
      ExtractionProblem.METHOD_GREEDY,
      ExtractionProblem.REASONING_EFFICIENT
    );

    // attach ontologies and mapping to the problem
    ep.bindSourceOntology( new IOntology(onto1.path));
    ep.bindTargetOntology( new IOntology(onto2.path));
    ep.bindMapping(mapping);


    // solve the problem
    val result = try {
      ep.solve();


      val extracted: Mapping = ep.getExtractedMapping()
      logger.info("Debugging completed")
      val res = convertMappingToMatchingList(extracted, owlTypeMap, matchTypeMap,matchings)


      res
    }
    catch {
      case e: Throwable => {
        logger.error("Error while Debugging Ontolog", e)
        println("error")
        matchings
      }
    };

    result
  }

  /**
   * Removes incoherent correspondences from the alignment based on alcomo and replaces them with very similar ones and peforms debugging again
   * @param alignment
   * @return
   */
  def debugAlignment(alignment: Alignment, raw_matchings: Map[MatchRelation, Double], threshold: Double): Alignment = {


    val owlTypeMap: Map[String, String] = alignment.correspondences.map(cell => {
      val key = cell.entity1 + "=" + cell.entity2
      val value = cell.owl_type
      key -> value
    }).toMap

    val mapping = convertAlignmentToMapping(alignment)

    val ep = new ExtractionProblem(
      ExtractionProblem.ENTITIES_CONCEPTSPROPERTIES,
      ExtractionProblem.METHOD_GREEDY,
      ExtractionProblem.REASONING_EFFICIENT
    );

    // attach ontologies and mapping to the problem


    ep.bindSourceOntology( new IOntology(alignment.onto1_reference.path));
    ep.bindTargetOntology( new IOntology(alignment.onto2_reference.path));
    ep.bindMapping(mapping);
    ep.init()
    // solve the problem
    val result = try {
      ep.solve();
      val discarded: Mapping = ep.getDiscardedMapping

      val extracted: Mapping = ep.getExtractedMapping()
      logger.info("Debugging completed")
      val pre_result = convertMappingToAlignment(extracted, owlTypeMap, alignment)

      if (discarded.size() > 0) {
        val to_be_added = getSimilarMatchingForDiscarded(discarded, raw_matchings, threshold * 0.9)
        val new_matchings = to_be_added.map { case (relation, value) => {
          MatchingCell(relation.left, relation.right, value, relation.relation, relation.owl_type, relation.match_type)
        }
        }.toSet

        val size_before = pre_result.correspondences.size
        pre_result.addAllCorrespondeces(new_matchings)
        val size_after = pre_result.correspondences.size
        if (size_after < size_before) {
          println("not worked")
        }
        //now debug again
        pre_result
      } else {
        pre_result
      }


    }
    catch {
      case e: Throwable => {
        logger.error("Error while Debugging Ontology", e)
        println("error")
        alignment
      }
    };

    result
  }



  def debugAlignment(alignment: Alignment, raw_matchings: Map[MatchRelation, Double],  class_threshold: Double, dp_threshold: Double, op_threshold: Double): Alignment = {


    val owlTypeMap: Map[String, String] = alignment.correspondences.map(cell => {
      val key = cell.entity1 + "=" + cell.entity2
      val value = cell.owl_type
      key -> value
    }).toMap

    val mapping = convertAlignmentToMapping(alignment)

    val ep = new ExtractionProblem(
      ExtractionProblem.ENTITIES_CONCEPTSPROPERTIES,
      ExtractionProblem.METHOD_GREEDY,
      ExtractionProblem.REASONING_EFFICIENT
    );

    // attach ontologies and mapping to the problem
    ep.bindSourceOntology( new IOntology(alignment.onto1_reference.path));
    ep.bindTargetOntology( new IOntology(alignment.onto2_reference.path));
    ep.bindMapping(mapping);
    ep.init()
    // solve the problem
    val result = try {
      ep.solve();
      val discarded: Mapping = ep.getDiscardedMapping

      val extracted: Mapping = ep.getExtractedMapping()
      logger.info("Debugging completed")
      val pre_result = convertMappingToAlignment(extracted, owlTypeMap, alignment)

      if (discarded.size() > 0) {
        val to_be_added = getSimilarMatchingForDiscardedSeparated(discarded, raw_matchings, class_threshold*1.0, dp_threshold*1.0, op_threshold*1.0)
        val new_matchings = to_be_added.map { case (relation, value) => {
          MatchingCell(relation.left, relation.right, value, relation.relation, relation.owl_type, relation.match_type)
        }
        }.toSet

        val size_before = pre_result.correspondences.size
        pre_result.addAllCorrespondeces(new_matchings)
        val size_after = pre_result.correspondences.size
        if (size_after < size_before) {
          println("not worked")
        }
        //now debug again
       pre_result
      } else {
        pre_result
      }


    }
    catch {
      case e: Throwable => {
        logger.error("Error while Debugging Ontology", e)
        println("error")
        alignment
      }
    };

    result
  }


  def getSimilarMatchingForDiscarded(discarded: Mapping, raw_matchings: Map[MatchRelation, Double], threshold: Double): Map[MatchRelation, Double] = {


    val result_tmp = discarded.map(correspondence => {
      getSimilarMatchingForDiscardedSingle(correspondence, raw_matchings)
    }).unzip

    val result_option: Iterable[Option[(MatchRelation, Double)]] = result_tmp._1 ++ result_tmp._2

    val result_map: Map[MatchRelation, Double] = result_option.filter(_.isDefined).map(_.get).filter(elem => elem._2 > threshold).toMap

    result_map
  }

  /**
   *
   * @param discarded
   * @param raw_matchings
   * @param class_threshold
   * @param dp_threshold
   * @param op_threshold
   * @return
   */
  def getSimilarMatchingForDiscardedSeparated(discarded: Mapping, raw_matchings: Map[MatchRelation, Double], class_threshold: Double, dp_threshold: Double, op_threshold: Double): Map[MatchRelation, Double] = {


    val result_tmp = discarded.map(correspondence => {
      getSimilarMatchingForDiscardedSingle(correspondence, raw_matchings)
    }).unzip

    val result_option: Iterable[Option[(MatchRelation, Double)]] = result_tmp._1 ++ result_tmp._2

    val result_map: Map[MatchRelation, Double] = result_option.filter(_.isDefined).map(_.get).filter(elem => {
      if (elem._1.owl_type.equals(Cell.TYPE_CLASS)) {
        elem._2 >= class_threshold
      } else if (elem._1.owl_type.equals(Cell.TYPE_DT_PROPERTY)) {
        elem._2 >= dp_threshold
      } else if (elem._1.owl_type.equals(Cell.TYPE_OBJECT_PROPERTY)) {
        elem._2 >= op_threshold
      } else {
        false
      }
    }).toMap

    result_map
  }


  def getSimilarMatchingForDiscardedSingle(correspondence: Correspondence, raw_matchings: Map[MatchRelation, Double]): (Option[(MatchRelation, Double)], Option[(MatchRelation, Double)]) = {

    //best left alternatives
    val left_alternatives = raw_matchings.filter { case (relation, value) => {
      relation.left.equals(correspondence.getSourceEntityUri) && !relation.right.equals(correspondence.getTargetEntityUri)
    }
    }

    val best_left = if (left_alternatives.size > 0) Option(left_alternatives.maxBy(_._2)) else Option.empty


    //best right alternatives
    val right_alternatives = raw_matchings.filter { case (relation, value) => {
      relation.right.equals(correspondence.getTargetEntityUri) && !relation.left.equals(correspondence.getSourceEntityUri)
    }
    }

    val best_right = if (right_alternatives.size > 0) Option(right_alternatives.maxBy(_._2)) else Option.empty

    val res: (Option[(MatchRelation, Double)], Option[(MatchRelation, Double)]) = (best_left, best_right)

    res
  }


  /**
   * Converts an OAMatch Alignment to an Alcomox mapping
   * @param alignment
   * @return
   */
  def convertAlignmentToMapping(alignment: Alignment): Mapping = {

    val correspondances = alignment.correspondences.map(cell => {
      convertMatchingCellToCorrespondence(cell)
    })

    new Mapping(correspondances)
  }


  /**
   *
   * @param cell
   * @return
   */
  def convertMatchingCellToCorrespondence(cell: MatchingCell): Correspondence = {
    val correspondence = new Correspondence(cell.entity1, cell.entity2, new SemanticRelation(SemanticRelation.EQUIV), cell.measure)
    correspondence
  }

  /**
   *
   * @param matchings
   * @return
   */
  def convertMatchRelationMapToMapping(matchings: Map[MatchRelation,Double]): Mapping = {

    val correspondances = matchings.map(tuple => {
      convertMatchingToCorrespondence(tuple._1, tuple._2)
    }).toSet

    new Mapping(correspondances)
  }

  /**
   *
   * @param matchRelation
   * @param measure
   * @return
   */
  def convertMatchingToCorrespondence(matchRelation: MatchRelation, measure:Double) = {
    val correspondence = new Correspondence(matchRelation.left, matchRelation.right, new SemanticRelation(SemanticRelation.EQUIV), measure)
    correspondence
  }

  /**
   * Converts an alcomo mapping to an alignment suitable for oamatch
   * @param mapping
   * @param owlTypeMap
   * @param undebugedAlignment the undebuged version of this created alignment
   * @return
   */
  def convertMappingToAlignment(mapping: Mapping, owlTypeMap: Map[String, String], undebugedAlignment: Alignment): Alignment = {

    val correspondences: List[MatchingCell] = mapping.getCorrespondences().map(correspondence => {
      convertCorrespondenceToCell(correspondence, owlTypeMap)
    }).toList

    new Alignment(undebugedAlignment.onto1, undebugedAlignment.onto2, undebugedAlignment.onto1_reference, undebugedAlignment.onto2_reference, undebugedAlignment.i_onto1, undebugedAlignment.i_onto2, correspondences)
  }


  /**
   *
   * @param mapping
   * @param owlTypeMap
   * @param matchTypeMap
   * @param matchings
   * @return
   */
  def convertMappingToMatchingList(mapping: Mapping, owlTypeMap: Map[String, String],matchTypeMap: Map[String,String], matchings: Map[MatchRelation, Double]): Map[MatchRelation,Double] = {

     mapping.getCorrespondences().map(correspondence => {
       convertCorrespondenceToMatchRelation(correspondence, owlTypeMap,matchTypeMap,matchings)
    }).toMap

  }

  /**
   * Converts an alcomo mapping to a oamatch matching cell
   * @param correspondence
   * @param owlTypeMap
   * @return
   */
  def convertCorrespondenceToCell(correspondence: Correspondence, owlTypeMap: Map[String, String]): MatchingCell = {
    val measure = correspondence.getConfidence
    val entity1 = correspondence.getSourceEntityUri
    val entity2 = correspondence.getTargetEntityUri

    val relation = correspondence.getRelation.toString
    val owlType: String = owlTypeMap.getOrElse(entity1 + "=" + entity2, Cell.TYPE_UNKOWN)

    MatchingCell(entity1, entity2, measure, relation, owlType, Alignment.TYPE_NONE)
  }

  /**
   * Converts an alcomo mapping to a oamatch matching cell
   * @param correspondence
   * @param owlTypeMap
   * @return
   */
  def convertCorrespondenceToMatchRelation(correspondence: Correspondence, owlTypeMap: Map[String, String], matchTypeMap:Map[String, String], orginalRelations:Map[MatchRelation,Double]): (MatchRelation,Double) = {
    val measure = correspondence.getConfidence
    val entity1 = correspondence.getSourceEntityUri
    val entity2 = correspondence.getTargetEntityUri


    val owlType: String = owlTypeMap.getOrElse(entity1 + "=" + entity2, Cell.TYPE_UNKOWN)
    val matchType: String = matchTypeMap.getOrElse(entity1 + "=" + entity2, Cell.TYPE_UNKOWN)
   val relation  = MatchRelation(entity1,"=",entity2, owlType,matchType )

    val sim_score = orginalRelations.get(relation).getOrElse(0.0)

    relation ->sim_score
  }


}
