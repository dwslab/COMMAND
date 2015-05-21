package de.unima.dws.oamatching.core

import java.io.File

import de.unima.dws.oamatching.core.matcher.ExtractedFields
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary

import scala.collection.JavaConversions._

/**
 * Created by mueller on 21/01/15.
 */

case class EntitiesOntology(val classes:Vector[IRI], val object_properties:Vector[IRI], val data_properties:Vector[IRI])
case class FastOntology(val base_values:EntitiesOntology,
                        val parent_to_child_classes_map: Map[IRI, Set[IRI]],
                        val child_to_parents_classes_map: Map[IRI, Set[IRI]],
                        val parent_to_child_object_properties_map: Map[IRI, Set[IRI]],
                        val child_to_parents_object_propterties_map: Map[IRI, Set[IRI]],
                        val parent_to_child_data_properties_map: Map[IRI, Set[IRI]],
                        val child_to_parents_data_propterties_map: Map[IRI, Set[IRI]],
                        val object_property_to_domain_map: Map[IRI, Set[IRI]],
                        val object_property_to_range_map: Map[IRI, Set[IRI]],
                        val data_property_to_domain_map: Map[IRI, Set[IRI]],
                        val data_property_to_range_map: Map[IRI, Set[IRI]],
                        val classes_in_object_prop_domain: Map[IRI, Set[IRI]],
                        val classes_in_object_prop_range: Map[IRI, Set[IRI]],
                        val classes_in_data_prop_domain: Map[IRI, Set[IRI]],
                        val classes_to_names: Map[IRI, ExtractedFields],
                        val object_properties_to_names: Map[IRI, ExtractedFields],
                        val data_properties_to_names: Map[IRI, ExtractedFields],
                        val class_name_to_IRI:Map[String,IRI],
                        val object_property_name_to_IRI:Map[String,IRI],
                        val data_property_name_to_IRI:Map[String,IRI],
                        val name:String,
                         val path:String)

object OntologyLoader {
  val manager: OWLOntologyManager = OWLManager.createOWLOntologyManager()
  val factory = manager.getOWLDataFactory();
  val label: OWLAnnotationProperty = factory.getOWLAnnotationProperty(OWLRDFVocabulary.RDFS_LABEL.getIRI());
  val comment = factory.getOWLAnnotationProperty(OWLRDFVocabulary.RDFS_COMMENT.getIRI());


  def load(onto_file: File): OWLOntology = {
    val manager: OWLOntologyManager = OWLManager.createOWLOntologyManager()

    val onto_iri: IRI = IRI.create(onto_file)
    manager.loadOntology(onto_iri)
  }

  def load(onto_file: String): OWLOntology = {
    val f: File = new File(onto_file)
    load(f)
  }

  def load_fast_ontology(onto: String): FastOntology = {
    val onto_file: File = new File(onto)


    val onto_iri: IRI = IRI.create(onto_file)
    val owlOntology = manager.loadOntology(onto_iri)


    val onto_name = owlOntology.getOntologyID.getOntologyIRI.toString

    //get classes


    val class_name_to_iri_map: Map[String, IRI] = owlOntology.getClassesInSignature().filter(filterNotOWLThing).map(owlClass => {
      owlClass.toStringID -> owlClass.getIRI
    }).toMap

    val classes = class_name_to_iri_map.values.toVector


    val object_property_name_to_iri_map: Map[String, IRI] = owlOntology.getObjectPropertiesInSignature().map(prop => {
      prop.toStringID -> prop.getIRI
    }).toMap

    val object_properties = object_property_name_to_iri_map.values.toVector

    val data_proptery_name_to_iri_map: Map[String, IRI] = owlOntology.getDataPropertiesInSignature().map(prop => {
      prop.toStringID -> prop.getIRI
    }).toMap

    val data_properties = data_proptery_name_to_iri_map.values.toVector

    /*#################################################################################################################
                                            Class Hierachy
    ################################################################################################################*/
    val class_hierachy: (Set[(IRI, Set[IRI])], Set[(IRI, Set[IRI])]) = owlOntology.getClassesInSignature().filter(filterNotOWLThing).map(owlClass => {
      /*
        A sub class of B
        here we are B, so get A's
      */
      val sub_classes = owlOntology.getSubClassAxiomsForSuperClass(owlClass).map(classAxiom => {
        if (!classAxiom.getSubClass.isAnonymous) {
          val sub_classes = classAxiom.getSubClass.asConjunctSet().filter(elem => filterNotOWLThing(elem.asOWLClass())).map(sub_class => {
            sub_class.asOWLClass().getIRI
          }).toSet
          Option(sub_classes)
        } else {
          Option.empty
        }
      }).filter(_.isDefined).map(_.get).flatten.toSet

      /*
        A sub class of B
        here we are A, so get B's
      */
      val super_classes = owlOntology.getSubClassAxiomsForSubClass(owlClass).map(classAxiom => {
        if (!classAxiom.getSuperClass.isAnonymous) {
          val sub_classes = classAxiom.getSuperClass.asConjunctSet().filter(elem => filterNotOWLThing(elem.asOWLClass())).map(super_class => {
            super_class.asOWLClass().getIRI
          }).toSet
          Option(sub_classes)
        } else {
          Option.empty
        }
      }).filter(_.isDefined).map(_.get).flatten.toSet

      ((owlClass.getIRI -> sub_classes), (owlClass.getIRI -> super_classes))
    }).toSet.unzip
    val parent_to_child_classes: Map[IRI, Set[IRI]] = class_hierachy._1.toMap
    val child_to_parent_classes: Map[IRI, Set[IRI]] = class_hierachy._2.toMap

    /*#################################################################################################################
                                                  Data Properties Hierachy (consider removing elem types)
     ################################################################################################################*/
    val data_property_hierachy = owlOntology.getDataPropertiesInSignature().map(owlProp => {
      /*
        A sub class of B
        here we are B, so get A's
       */
      val subProperties = owlOntology.getDataSubPropertyAxiomsForSuperProperty(owlProp).map(axiom => {
        if (!axiom.getSubProperty.isAnonymous) {
          Option(axiom.getSubProperty.asOWLDataProperty().getIRI)
        } else {
          Option.empty
        }
      }).filter(_.isDefined).map(_.get).toSet

      /*
        A sub class of B
        here we are A, so get B's
       */
      val superProperties = owlOntology.getDataSubPropertyAxiomsForSubProperty(owlProp).map(axiom => {
        if (!axiom.getSuperProperty.isAnonymous) {
          Option(axiom.getSuperProperty.asOWLDataProperty().getIRI)
        } else {
          Option.empty
        }
      }).filter(_.isDefined).map(_.get).toSet
      ((owlProp.getIRI -> subProperties), (owlProp.getIRI -> superProperties))
    }).toSet.unzip
    val parent_to_child_data_props: Map[IRI, Set[IRI]] = data_property_hierachy._1.toMap
    val child_to_parent_data_props: Map[IRI, Set[IRI]] = data_property_hierachy._2.toMap




    /*#################################################################################################################
                                                       Object Properties Hierachy
        ################################################################################################################*/
    val object_property_hierachy = owlOntology.getObjectPropertiesInSignature().map(owlProp => {
      /*
        A sub class of B
        here we are B, so get A's
       */
      val subProperties = owlOntology.getObjectSubPropertyAxiomsForSuperProperty(owlProp).map(axiom => {
        if (!axiom.getSubProperty.isAnonymous) {
          Option(axiom.getSubProperty.asOWLObjectProperty().getIRI)
        } else {
          Option.empty
        }
      }).filter(_.isDefined).map(_.get).toSet

      /*
        A sub class of B
        here we are A, so get B's
       */
      val superProperties = owlOntology.getObjectSubPropertyAxiomsForSuperProperty(owlProp).map(axiom => {
        if (!axiom.getSuperProperty.isAnonymous) {
          Option(axiom.getSuperProperty.asOWLObjectProperty().getIRI)
        } else {
          Option.empty
        }
      }).filter(_.isDefined).map(_.get).toSet
      ((owlProp.getIRI -> subProperties), (owlProp.getIRI -> superProperties))
    }).toSet.unzip

    val parent_to_child_object_props: Map[IRI, Set[IRI]] = object_property_hierachy._1.toMap
    val child_to_parent_object_props: Map[IRI, Set[IRI]] = object_property_hierachy._2.toMap



    /*#################################################################################################################
                                                  Object Property domain and Range and the class usage in tit
      ################################################################################################################*/


    val object_prop_domain_range = owlOntology.getObjectPropertiesInSignature().map(owlProp => {

      val domain = owlOntology.getObjectPropertyDomainAxioms(owlProp).map(domain_axiom => {
        domain_axiom.getClassesInSignature.filter(filterNotOWLThing).map(owl_class => {
          owl_class.getIRI
        }).toSet
      }).flatten.toSet

      val range = owlOntology.getObjectPropertyRangeAxioms(owlProp).map(range_axiom => {
        range_axiom.getClassesInSignature.filter(filterNotOWLThing).map(owl_class => {
          owl_class.getIRI
        }).toSet
      }).flatten.toSet

      (owlProp.getIRI -> domain, owlProp.getIRI -> range)
    }).toSet.unzip

    val object_prop_domain: Map[IRI, Set[IRI]] = object_prop_domain_range._1.toMap
    val object_prop_range: Map[IRI, Set[IRI]] = object_prop_domain_range._2.toMap


    //invert the whole thing get object properties by class, separated domain and range
    val class_object_prop_domain_range = classes.map(owlClass => {
      //look through all domains and ranges to find the class in there
      val class_object_domain: Set[IRI] = object_prop_domain.map { case (owlProp, domain) => {
        if (domain.contains(owlClass)) {
          Option(owlProp)
        } else {
          Option.empty
        }
      }
      }.filter(_.isDefined).map(_.get).toSet

      val class_object_range: Set[IRI] = object_prop_range.map { case (owlProp, range) => {
        if (range.contains(owlClass)) {
          Option(owlProp)
        } else {
          Option.empty
        }
      }
      }.filter(_.isDefined).map(_.get).toSet

      (owlClass -> class_object_domain, owlClass -> class_object_range)
    }).toSet.unzip

    val class_object_prop_domain: Map[IRI, Set[IRI]] = class_object_prop_domain_range._1.toMap
    val class_object_prop_range: Map[IRI, Set[IRI]] = class_object_prop_domain_range._2.toMap


    /*#################################################################################################################
                                                 Classes in Data Property domain
      ################################################################################################################*/

    val data_prop_domain_range = owlOntology.getDataPropertiesInSignature().map(owlProp => {

      val domain = owlOntology.getDataPropertyDomainAxioms(owlProp).map(domain_axiom => {
        domain_axiom.getClassesInSignature.filter(filterNotOWLThing).map(owl_class => {
          owl_class.getIRI
        }).toSet
      }).flatten.toSet


      val range = owlOntology.getDataPropertyRangeAxioms(owlProp).map(range_axiom => {
        range_axiom.getClassesInSignature.filter(filterNotOWLThing).map(owl_class => {
          owl_class.getIRI
        }).toSet
      }).flatten.toSet

      (owlProp.getIRI -> domain, owlProp.getIRI -> range)
    }).toSet.unzip

    val data_prop_domain: Map[IRI, Set[IRI]] = data_prop_domain_range._1.toMap
    val data_prop_range: Map[IRI, Set[IRI]] = data_prop_domain_range._2.toMap


    //invert the whole thing get data properties by class only for domain for data props
    val class_data_prop: Map[IRI, Set[IRI]] = classes.map(owlClass => {
      //look through all domains to find the class in there
      val class_object_domain: Set[IRI] = data_prop_domain.map { case (owlProp, domain) => {
        if (domain.contains(owlClass)) {
          Option(owlProp)
        } else {
          Option.empty
        }
      }
      }.filter(_.isDefined).map(_.get).toSet

      owlClass -> class_object_domain
    }).toMap

    /*#################################################################################################################
                                       Extract Fields for Classes
    ################################################################################################################*/

    val owlClassToNames: Map[IRI, ExtractedFields] = owlOntology.getClassesInSignature().filter(filterNotOWLThing).map(owlClass => {
      //Fragment
      val local_name = Option(owlClass.getIRI.getFragment())


      val e_label = extractAnnotationOfType(owlOntology, owlClass, label)
      val e_comment: Option[String] = extractAnnotationOfType(owlOntology, owlClass, comment)
      val syn: Option[List[String]] = extractSyn(owlOntology, owlClass)

      //if comment is not defined use synonyms

      owlClass.getIRI -> ExtractedFields(Option.empty,Option.empty,local_name, e_label, e_comment, syn)
    }).toMap
    /*#################################################################################################################
                                             Extract Fields for Properties
      ################################################################################################################*/
    val owlObjectPropertiesToNames: Map[IRI, ExtractedFields] = owlOntology.getObjectPropertiesInSignature().map(owlProp => {
      //Fragment
      val local_name = Option(owlProp.getIRI.getFragment())

      val e_label = extractAnnotationOfType(owlOntology, owlProp, label)
      val e_comment = extractAnnotationOfType(owlOntology, owlProp, comment)
      val syn = extractSyn(owlOntology, owlProp)
      val domain = object_prop_domain.get(owlProp.getIRI())
      val domain_labels: Option[Set[String]] = extractDomainRangeLabels(domain,owlClassToNames)

      val range = object_prop_range.get(owlProp.getIRI())
      val range_labels: Option[Set[String]] = extractDomainRangeLabels(range,owlClassToNames)


      owlProp.getIRI -> ExtractedFields(domain_labels,range_labels,local_name, e_label, e_comment, syn)
    }).toMap


    val owlDataPropertiesToNames: Map[IRI, ExtractedFields] = owlOntology.getDataPropertiesInSignature().map(owlProp => {
      //Fragment
      val local_name = Option(owlProp.getIRI.getFragment())

      val e_label = extractAnnotationOfType(owlOntology, owlProp, label)
      val e_comment = extractAnnotationOfType(owlOntology, owlProp, comment)
      val syn = extractSyn(owlOntology, owlProp)

      val domain = data_prop_domain.get(owlProp.getIRI())

      val domain_labels: Option[Set[String]] = extractDomainRangeLabels(domain,owlClassToNames)


      owlProp.getIRI -> ExtractedFields(domain_labels,Option.empty,local_name, e_label, e_comment, syn)
    }).toMap

    manager.removeOntology(owlOntology)
    val base_values = EntitiesOntology(classes,object_properties,data_properties)
    FastOntology(base_values, parent_to_child_classes, child_to_parent_classes, parent_to_child_object_props, child_to_parent_object_props, parent_to_child_data_props, child_to_parent_data_props, object_prop_domain, object_prop_range, data_prop_domain, data_prop_range, class_object_prop_domain, class_object_prop_range, class_data_prop, owlClassToNames, owlObjectPropertiesToNames, owlDataPropertiesToNames, class_name_to_iri_map, object_property_name_to_iri_map, data_proptery_name_to_iri_map,onto_name,onto)
  }


  def extractDomainRangeLabels(domain: Option[Set[IRI]], classes_to_names: Map[IRI, ExtractedFields]): Option[Set[String]] = {
    if (domain.isDefined) {


      val domain_labels = domain.get.map(iri => {
        val opt_class_fields = classes_to_names.get(iri)
        if (opt_class_fields.isDefined) {
          if (opt_class_fields.get.fragment.isDefined) {
            Option(opt_class_fields.get.fragment.get)
          } else {
            Option.empty
          }
        } else {
          Option.empty
        }
      }).filter(_.isDefined).map(_.get)

      Option(domain_labels)
    } else {
      Option.empty
    }
  }

  def extractSyn(owlOntology: OWLOntology, owlEntity: OWLEntity): Option[List[String]] = {

    val synonym_label = owlEntity.getAnnotations(owlOntology).map(annotation => {
      val prop_iri = annotation.getProperty().getIRI()
      val remainder = prop_iri.getFragment()

      if (remainder.toLowerCase.contains("synonym")) {

        val ni = factory.getOWLNamedIndividual(annotation.getValue.asInstanceOf[IRI]);

        val synonym_label = ni.getAnnotations(owlOntology, label).map(synonym => {

          if (synonym.getValue.isInstanceOf[OWLLiteral]) {
            val element_string = synonym.getValue.asInstanceOf[OWLLiteral].getLiteral
            Option(element_string)
          } else {
            Option.empty
          }
        }).filter(_.isDefined).map(_.get)

        if (synonym_label.size > 0) {
          if(synonym_label.size>1){
            println("test")
          }
          val content = synonym_label.head
          Option(content)
        } else {
          Option.empty
        }

      } else {
        Option.empty
      }
    }).filter(_.isDefined).map(_.get).toList

    if (synonym_label.size > 0) {
      Option(synonym_label)
    } else {
      Option.empty
    }
  }

  def extractAnnotationOfType(owlOntology: OWLOntology, owlEntity: OWLEntity, annotation_type: OWLAnnotationProperty): Option[String] = {
    val extracted = owlEntity.getAnnotations(owlOntology, annotation_type).map(annotation => {
      if (annotation.getValue.isInstanceOf[OWLLiteral]) {
        val element_string = annotation.getValue.asInstanceOf[OWLLiteral].getLiteral
        Option(element_string)
      } else {
        Option.empty
      }
    })

    if (extracted.size > 0) {
      extracted.head
    } else {
      Option.empty
    }


  }

  def filterNotOWLThing(elem:OWLEntity):Boolean = {
    val iri_String = elem.getIRI.toString
    !(iri_String.isEmpty || iri_String.endsWith("owl#Thing") ||  iri_String.endsWith("owl:Thing"))
  }
}
