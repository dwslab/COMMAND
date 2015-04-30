package de.unima.dws.oamatching.matcher.structurallevel

import java.util

import com.interdataworking.mm.alg.{MapPair, Match}
import de.unima.dws.alex.onto2graph.{Matcher, TestMatch}
import de.unima.dws.oamatching.core.matcher.StructuralLevelMatcher
import de.unima.dws.oamatching.core.{FastOntology, MatchingCell, Alignment, Cell}
import org.semanticweb.owlapi.model.{IRI, OWLOntology}

import scala.collection.JavaConversions._
import scala.collection.mutable

/**
 * Created by mueller on 26/01/15.
 */
class SimilarityFloodingMatcher  extends  StructuralLevelMatcher{

  override protected def align(onto1: FastOntology, onto2: FastOntology, initial_Alignment: Alignment, threshold: Double): Alignment = {
  /*
    val produced_correspondences: Set[mutable.Set[MatchingCell]] = initial_Alignment.getPresentMatchTypesinAlignment().map(match_type =>{
      val filtered_alignment = initial_Alignment.getNewAlignmentWithMatchType(match_type)
      val alignment: util.List[MapPair] = convertAlignmentToMapPair(filtered_alignment)
      val result:Array[MapPair] =  Matcher.structMatch(onto1,onto2,alignment,Match.FORMULA_TFF,Match.FG_PRODUCT)
      val result_alingment = convertMapPairToAlignment(result,threshold,onto1,onto2,match_type)
      result_alingment.correspondences
    })
    //if no alignment available create initial alignment with jaro Winkler

    val correspondences = produced_correspondences.flatten.toSet


    val copied_alignment = new Alignment(initial_Alignment)
    copied_alignment.addAllCorrespondeces(produced_correspondences.flatten.toSet)

    copied_alignment
    */
    initial_Alignment
  }

  protected def convertAlignmentToMapPair(alignment:Alignment):java.util.List[MapPair] ={
    val initMap = alignment.correspondences.map(cell => {
      new MapPair(cell.entity1.toString, cell.entity2.toString,cell.measure)
    })

    initMap.toList
  }

  protected def convertMapPairToAlignment(mapPairs:Array[MapPair],threshold:Double,onto1: OWLOntology, onto2: OWLOntology,match_type:String):Alignment = {

    val cells = mapPairs.map(pair=> {

      val iri_left = IRI.create(pair.getLeft.toString);
      val iri_right = IRI.create(pair.getRight.toString);

      val entity_left = onto1.getEntitiesInSignature(iri_left).head
      val entity_right = onto2.getEntitiesInSignature(iri_right).head

      //get owl datatype
      val owl_data_type = if (entity_left.isOWLClass) {Cell.TYPE_CLASS} else if(entity_left.isOWLDataProperty) {Cell.TYPE_DT_PROPERTY} else if(entity_left.isOWLObjectProperty){Cell.TYPE_OBJECT_PROPERTY} else {Cell.TYPE_UNKOWN}

     MatchingCell(pair.getLeft.toString,pair.getRight.toString,pair.sim,"=",owl_data_type,match_type)
    }).toList.filter(cell=> cell.measure >= threshold)

    new Alignment(null,null,cells)
  }

}
