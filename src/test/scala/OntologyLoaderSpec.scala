import de.unima.dws.oamatching.core.OntologyLoader
import org.semanticweb.owlapi.model.IRI

/**
 * Created by mueller on 23/03/15.
 */
class OntologyLoaderSpec extends  UnitSpec{


  "The Ontology Loader" should "be parse the anatomy ontology correctly" in {
    val human_onto = OntologyLoader.load_fast_ontology("ontos/2014/anatomy/human.owl")


    val classes_size = human_onto.base_values.classes.size
    val data_props_size = human_onto.base_values.data_properties.size
    val object_props_size = human_onto.base_values.object_properties.size

    assert(classes_size == 3304)
    assert(data_props_size == 0)
    assert(object_props_size == 2)


    assert(classes_size >= human_onto.child_to_parents_classes_map.size)
    assert(classes_size >= human_onto.parent_to_child_classes_map.size)

  }

  it should "make sure that the leaf node in the class hierachy have no children " in {
    val human_onto = OntologyLoader.load_fast_ontology("ontos/2014/anatomy/human.owl")

    val follicular_dendritic_cell = IRI.create("http://human.owl#NCI_C12622")
    val children = human_onto.parent_to_child_classes_map.get(follicular_dendritic_cell)
    assert(children.get.size==0)

  }

  it should "parse a hierachy with more than one children correctly " in {
    val human_onto = OntologyLoader.load_fast_ontology("ontos/2014/anatomy/human.owl")

    val follicular_dendritic_cell = IRI.create("http://human.owl#NCI_C12583")
    val children = human_onto.parent_to_child_classes_map.get(follicular_dendritic_cell)
    assert(children.get.size==4)
  }

  /*it should "parse the synonyms of a class correctly " in {

    val human_onto = OntologyLoader.load_fast_ontology("ontos/2014/anatomy/human.owl")

    val class_with_syns = IRI.create("http://human.owl#NCI_C12219")
    val synonyms = human_onto.classes_to_names.get(class_with_syns).get.synonym
    assert(synonyms.isDefined)
    println(synonyms.get)
    assert(synonyms.get.size == 2)
  }*/

  it should "enable random access to IRI, by their iri string" in {
    val human_onto = OntologyLoader.load_fast_ontology("ontos/2014/anatomy/human.owl")
    val fragment = "NCI_C12219"
    val name = "http://human.owl#"+fragment
    val class_iri = human_onto.class_name_to_IRI.get("http://human.owl#NCI_C12219")


    assert(class_iri.isDefined)

    assert(human_onto.classes_to_names.get(class_iri.get).get.fragment.get.equals(fragment))

  }


}
