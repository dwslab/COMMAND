package de.unima.dws.oamatching.core

import de.unima.alcomox.ontology.IOntology

import scala.collection.mutable

/**
 * Created by mueller on 21/01/15.
 */
case class MatchRelation(left: String, relation: String, right: String, owl_type: String, match_type: String)

/**
 * Case class for a correspondance
 * @param entity1
 * @param entity2
 * @param measure
 * @param relation
 * @param owl_type
 */
case class MatchingCell(entity1: String, entity2: String, measure: Double, relation: String, owl_type: String, match_type: String) {
  def canEqual(other: Any): Boolean = other.isInstanceOf[MatchingCell]

  override def equals(other: Any): Boolean = other match {
    case that: MatchingCell => {
      (that canEqual this) &&
        (this.entity1.equals(that.entity1)) &&
        (this.entity2.equals(that.entity2)) &&
        relation.equals(that.relation) &&
        (match_type.equals(that.relation))
    }
    case _ => {
      true
    }

  }

  override def toString: String = {
    "[ entity1: " + entity1.toString + " ,entity2: " + entity2.toString + " ,relation: " + relation + " ," + "sim: " + measure + " ]"
  }

  override def hashCode(): Int = {
    val state = Seq(entity1, entity2, relation)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}


class Alignment(val onto1: String, val onto2: String, var onto1_reference: FastOntology, var onto2_reference: FastOntology, val i_onto1: IOntology, val i_onto2: IOntology) {

  var correspondences: mutable.Set[MatchingCell] = new mutable.HashSet[MatchingCell]


  /**
   * Copy constructor
   * @param alignment_to_Copy Alingment to Copy
   */
  def this(alignment_to_Copy: Alignment) = {
    this(alignment_to_Copy.onto1, alignment_to_Copy.onto2, alignment_to_Copy.onto1_reference, alignment_to_Copy.onto2_reference, alignment_to_Copy.i_onto1, alignment_to_Copy.i_onto2)
    this.correspondences = alignment_to_Copy.correspondences.map(cell => MatchingCell(cell.entity1, cell.entity2, cell.measure, cell.relation, cell.owl_type, cell.match_type))
  }


  def this(onto1: String, onto2: String, onto1_reference: FastOntology, onto2_reference: FastOntology, i_onto1: IOntology, i_onto2: IOntology, correspondences: List[MatchingCell]) {
    this(onto1, onto2, onto1_reference, onto2_reference, i_onto1, i_onto2)

    this.correspondences = this.correspondences.++(correspondences)
  }

  def this(onto1: String, onto2: String, onto1_reference: FastOntology, onto2_reference: FastOntology, i_onto1: IOntology, i_onto2: IOntology, matchings: Map[MatchRelation, Double]) {
    this(onto1, onto2, onto1_reference, onto2_reference, i_onto1, i_onto2)

    matchings.foreach({
      case (matchrelation, similiarity) => {
        val cell = MatchingCell(matchrelation.left, matchrelation.right, similiarity, matchrelation.relation, matchrelation.owl_type, matchrelation.match_type)
        this.correspondences.add(cell)
      }
    })

  }


  def this(onto1: String, onto2: String, correspondences: List[MatchingCell]) {
    this(onto1, onto2, null, null, null, null)


    this.correspondences = this.correspondences.++(correspondences)
  }

  /**
   * Constructor with URIS as strings
   * @param onto1
   * @param onto2
   * @param threshold
   * @param matchings
   */
  def this(onto1: String, onto2: String, threshold: Double, matchings: Map[MatchRelation, Double]) {
    this(onto1, onto2, null, null, null, null)
    matchings.filter(tuple => tuple._2 >= threshold).foreach({
      case (matchrelation, similiarity) => {
        val test = MatchingCell(matchrelation.left, matchrelation.right, similiarity, matchrelation.relation, matchrelation.owl_type, matchrelation.match_type)

        this.correspondences.add(test)
        test
      }
    })

  }

  /**
   * Constructor for creation from a feature vector
   * @param onto1
   * @param onto2
   * @param matchings
   */
  def this(onto1: String, onto2: String, matchings: Map[MatchRelation, Double]) {
    this(onto1, onto2, null, null, null, null)
    val corresp = matchings.foreach({
      case (matchrelation, similiarity) => {
        val test = MatchingCell(matchrelation.left, matchrelation.right, similiarity, matchrelation.relation, matchrelation.owl_type, matchrelation.match_type)
        this.correspondences.add(test)
        test

      }
    })

  }

  def addToCorrespondences(cell: MatchingCell): Unit = {

    correspondences.add(cell)
  }

  def addAllCorrespondeces(cells: Set[MatchingCell]): Unit = {
    correspondences = mutable.HashSet(cells.toSeq: _*) ++ correspondences
  }

  def addAllCorrespondecesKeepHigher(cells: Set[MatchingCell]): Unit = {
    correspondences = mutable.HashSet(cells.toSeq: _*) ++ correspondences
  }

  def addAllCorrespondeces(cells: mutable.Set[MatchingCell]): Unit = {
    correspondences = cells ++ correspondences
  }

  def removeCorrespondence(cell_to_remove: MatchingCell): Unit = {
    correspondences = correspondences.filterNot(cell => {
      cell.entity1.eq(cell_to_remove.entity1) && cell.entity2.eq(cell_to_remove.entity2) && cell.relation.eq(cell_to_remove.relation)
    })
  }

  /**
   * Returns the correspondences as a MatchRelationMap for internal pipeline handling
   * @return
   */
  def asMatchRelationMap(): Map[MatchRelation, Double] = correspondences.map(cell => {
    (MatchRelation(cell.entity1.toString, cell.relation, cell.entity2.toString, cell.owl_type, cell.match_type), cell.measure)
  }).toMap

  override def toString: String = {
    this.asMatchRelationMap().toString()
  }

  /**
   * Method that calculates Precision Recall and F-Measure given a reference alignment
   * @param reference
   * @return
   */
  def evaluate(reference: Alignment): EvaluationResult = {

    var tmp_tp = 0
    var tmp_fp = 0
    correspondences.foreach(cell_1 => {
      var fp_tester = false
      reference.correspondences.foreach(cell_2 => {
        if (cell_1.entity1.equals(cell_2.entity1) && cell_1.entity2.equals(cell_2.entity2) && cell_1.relation.equals(cell_2.relation)) {
          tmp_tp = tmp_tp + 1
          fp_tester = true
        }
      })

      if (!fp_tester) {
        tmp_fp = tmp_fp + 1
      }
    })

    var tmp_fn = 0
    reference.correspondences.foreach(cell_1 => {
      var fn_tester = false
      correspondences.foreach(cell_2 => {
        if (cell_1.entity1.equals(cell_2.entity1) && cell_1.entity2.equals(cell_2.entity2) && cell_1.relation.equals(cell_2.relation)) {
          fn_tester = true
        }
      })
      if (!fn_tester) {
        tmp_fn = tmp_fn + 1
      }
    })

    val name = reference.onto1 + "-" + reference.onto2
    EvaluationResultAggregator.createEvaluationResult(tmp_tp, tmp_fp, tmp_fn, name)

  }


  def getFalsePositives(reference: Alignment): Set[MatchingCell] = {

    val false_positives = for (cell_1 <- correspondences) yield {

      val tester = reference.correspondences.map(cell_2 => {
        if (cell_1.entity1.equals(cell_2.entity1) && cell_1.entity2.equals(cell_2.entity2) && cell_1.relation.equals(cell_2.relation)) {
          true
        } else {
          false
        }
      })

      if (tester.contains(true)) {
        Option.empty
      } else {
        Option(cell_1)
      }
    }

    false_positives.filter(_.isDefined).map(_.get).toSet
  }

  def getTruePositives(reference: Alignment): Set[MatchingCell] = {

    val true_positives = for (cell_1 <- correspondences) yield {

      val tester = reference.correspondences.map(cell_2 => {
        if (cell_1.entity1.equals(cell_2.entity1) && cell_1.entity2.equals(cell_2.entity2) && cell_1.relation.equals(cell_2.relation)) {
          true
        } else {
          false
        }
      })
      if (tester.contains(true)) {
        Option(cell_1)
      } else {
       Option.empty
      }
    }
    true_positives.filter(_.isDefined).map(_.get).toSet
  }

  def getFalseNegatives(reference: Alignment): Set[MatchingCell] = {
    val false_negatives = for (cell_1 <- reference.correspondences) yield {

      val tester = correspondences.map(cell_2 => {
        if (cell_1.entity1.equals(cell_2.entity1) && cell_1.entity2.equals(cell_2.entity2) && cell_1.relation.equals(cell_2.relation)) {
          true
        } else {
          false
        }
      })
      if (tester.contains(true)) {
        Option.empty
      } else {
        Option(cell_1)
      }
    }
    false_negatives.filter(_.isDefined).map(_.get).toSet
  }

  def getNewAlignmentWithMatchType(match_type: String): Alignment = {
    val new_alignment = new Alignment(this)

    new_alignment.correspondences = new_alignment.correspondences.filter(_.match_type.equals(match_type))

    new_alignment
  }

  def getPresentMatchTypesinAlignment(): Set[String] = {
    this.correspondences.groupBy(_.match_type).map(_._2.head.match_type).toSet
  }

  def containsCorrespondence(left: String, right: String): Boolean = {
    this.correspondences.filter(_.entity1.equalsIgnoreCase(left)).filter(_.entity2.equalsIgnoreCase(right)).size > 0
  }

  def containsCorrespondenceRemainder(left_remainder: String, right_remainder: String): Boolean = {
    this.correspondences.filter(_.entity1.toLowerCase().contains(left_remainder)).filter(_.entity2.toLowerCase().contains(right_remainder)).size > 0
  }

  def getCorrespondence(left: String, right: String): MatchingCell = {
    this.correspondences.filter(_.entity1.equalsIgnoreCase(left)).filter(_.entity2.equalsIgnoreCase(right)).head
  }

  def getCorrespondence(left: String, right: String, match_type: String): Option[MatchingCell] = {
    val filtered = this.correspondences.filter(_.entity1.equalsIgnoreCase(left)).filter(_.entity2.equalsIgnoreCase(right)).filter(_.match_type.equalsIgnoreCase(match_type))

    if (filtered.size > 0) {
      Option(filtered.head)
    } else {
      Option.empty
    }
  }
}


object Alignment {
  val TYPE_FRAGMENT_FRAGMENT: String = "FF"
  val TYPE_FRAGMENT_LABEL: String = "FL"
  val TYPE_LABEL_FRAGMENT: String = "LF"
  val TYPE_LABEL_LABEL: String = "LL"
  val TYPE_COMMENT_COMMENT: String = "CC"
  val TYPE_FRAGMENT_COMMENT: String = "FC"
  val TYPE_COMMENT_FRAGMENT: String = "CF"

  val TYPE_NONE: String = "NN"
}