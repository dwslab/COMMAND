package de.unima.dws.oamatching.matcher.structurallevel

import de.unima.dws.oamatching.core.matcher.StructuralLevelMatcher
import de.unima.dws.oamatching.core._
import org.semanticweb.owlapi.model.{IRI, OWLClass, OWLOntology}

import scala.collection.JavaConversions._
import scala.collection.{IterableView, mutable}

/**
 * Graphbased Matcher:
 * check all matched classes and match also properties between them
 * with mean value of both classes
 * foo <---already matched with c=0.5---> foo
 * |                                      |
 * blub <--new with c=(0.5+0.4)/2=0.45--> bla
 * |                                      |
 * v                                      v
 * bar <----already matched with c=0.4---> bar
 *
 * The Algorithm was enhanced to also be able to handle non 1:1 initial mappings
 * @author Sven Hertling (Original Version)
 * @author Alexander Mueller (Scala Port)
 */
class GraphBasedUsedClassMatcher extends StructuralLevelMatcher {
  override protected def align(onto1: FastOntology, onto2: FastOntology, initial_Alignment: Alignment, threshold: Double): Alignment = {

    val produced_alingments = initial_Alignment.getPresentMatchTypesinAlignment().map(match_type =>{
      //loop over alignment and build map for class access

      //get only elements for the matchingtype
      val filtered_alingment = initial_Alignment.getNewAlignmentWithMatchType(match_type)


      //build map
      val starttime = System.currentTimeMillis()
      val match_partner_map: Map[String, mutable.Set[(String, Double)]] = buildMap(filtered_alingment, onto1, onto2)


      //loop over object properties in onto1
      val candidates: Vector[Option[List[(String, (String, String, Double))]]] = for (prop_onto1 <- onto1.base_values.object_properties) yield {
        //get domain and range of property
        val domain: Set[IRI] = onto1.object_property_to_domain_map.get(prop_onto1).get
        val range =  onto1.object_property_to_range_map.get(prop_onto1).get

        if (domain.size < 1 || range.size< 1) {
          Option.empty
        } else {
          /*if (domain.head.getClassesInSignature().size() > 1) {
            println(domain.size())
          }

          if (range.head.getClassesInSignature().size() > 1) {
            println(range.size())
          }*/
          //assuming only one class per domain
          val domain_class = domain.head
          val range_class = range.head
          //check if domain and range have corresponding match in onto2
          val domain_match: Option[mutable.Set[(String, Double)]] = match_partner_map.get(domain_class.toString)
          val range_match: Option[mutable.Set[(String, Double)]] = match_partner_map.get(range_class.toString)

          if (domain_match.isDefined && range_match.isDefined) {
            //if yes add as candidate in a map by domain
            val sub_candidates = for (domain_class <- domain_match.get;
                                      range_class <- range_match.get) yield {
              val sim = (domain_class._2 + range_class._2) / 2
              (domain_class._1, (prop_onto1.toString, range_class._1, sim))
            }
            Option(sub_candidates.toList)
          } else {
            Option.empty
          }
        }

      }
      val step2time =System.currentTimeMillis();

      val filtered_candidates: List[(String, (String, String, Double))] = candidates.toList.filter(sub_category => sub_category.isDefined).map(sub_category => sub_category.get).flatten.toList

      val distinct_domains = filtered_candidates.unzip._1.distinct

      //println("post distinct_domains " + distinct_domains.size)

      val candidates_by_domain_class: Map[String, List[(String, String, Double)]] = distinct_domains.map(domain_class => (domain_class, filtered_candidates.filter(elm => elm._1.equals(domain_class)).unzip._2)).toMap


      //step 2 candidate pruning
      //loop over object properties in onto2
      val matchings = onto2.base_values.object_properties.map(prop_onto2 => {
        val domain = onto2.object_property_to_domain_map.get(prop_onto2).get

        if (domain.size < 1) {
          Option.empty
        } else {
          val domain_class = domain.head
          // get candidates with the domain
          val option_candidates: Option[List[(String, String, Double)]] = candidates_by_domain_class.get(domain_class.toString)

          if (option_candidates.isDefined) {
            val range = onto2.object_property_to_range_map.get(prop_onto2)
            val range_class = domain.head
            //get List to retrieve multiple candidates and check each of it
            val optional_final = option_candidates.get.map(candidate => {
              if (candidate._2.equals(range_class.toString)) {
                // yes = add alignment
                //we got a real matching

                Option(MatchRelation(candidate._1, "=", prop_onto2.toString, Cell.TYPE_OBJECT_PROPERTY,match_type ) -> candidate._3)
              } else {
                // no = do nothing
                Option.empty
              }
            })

            Option(optional_final.filter(optional => optional.isDefined).map(option => option.get))
          } else {
            Option.empty
          }
        }

      })

      val step3time =System.currentTimeMillis();


      //filter threshold and non defined
      val filtered_matchings: List[(MatchRelation, Double)] = matchings.filter(match_res => match_res.isDefined).toList.map(matcher_res => matcher_res.get).flatten.filter(tuple => tuple._2 >= threshold)

      val copied_alignment = new Alignment(filtered_alingment)

      val cells_to_add = filtered_matchings.map { case (relation, measure) => MatchingCell(relation.left, relation.right, measure, relation.relation, relation.owl_type,relation.match_type)}
      //check if the correspondences are already there

      copied_alignment.addAllCorrespondecesKeepHigher(cells_to_add.toSet)

      val finshedTime =System.currentTimeMillis();

      System.gc()

      copied_alignment
    })



    //combine produced alignments
    val new_corresp= produced_alingments.map(_.correspondences).flatten.toSet
    val new_alignment = new Alignment(initial_Alignment)
    new_alignment.addAllCorrespondeces(new_corresp)


    new_alignment
  }

  private def buildMap(alignment: Alignment, onto1: FastOntology, onto2: FastOntology): Map[String, mutable.Set[(String, Double)]] = {
    val fromMap: Map[String, mutable.Set[(String, Double)]] = onto1.base_values.classes.view.map(owl_class => {
      findAlignFromMap(owl_class, alignment)
    }).toMap

    fromMap
  }

  private def findAlignFromMap(owlClass: IRI, alignment: Alignment): (String, mutable.Set[(String, Double)]) = {
    val from_alignments: mutable.Set[MatchingCell] = alignment.correspondences.filter(cell => cell.entity1.toString.equals(owlClass.toString))

   (owlClass.toString, from_alignments.map(cell => (cell.entity2.toString, cell.measure)))

  }
}
