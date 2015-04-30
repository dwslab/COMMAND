package de.unima.dws.oamatching.core

import java.net.URI

import org.semanticweb.owlapi.model.OWLEntity

/**
 * Created by mueller on 21/01/15.
 */
class Cell(val entity1:URI, val entity2:URI,val measure: Double, val relation:String, val owl_type:String) {
  /**
   * Copy Constructor
   * @param cell_to_copy cell to copy
   */
  def this(cell_to_copy:Cell)= {
    this(cell_to_copy.entity1,cell_to_copy.entity2,cell_to_copy.measure, cell_to_copy.relation,cell_to_copy.owl_type)
  }

  def this(uri_entity1:String,uri_entity2:String,measure: Double, relation:String, owl_type:String ) = {
    this(URI.create(uri_entity1), URI.create(uri_entity2),measure,relation, owl_type  )
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[Cell]


  override def equals(other: Any): Boolean = other match {
    case that: Cell =>{
      (that canEqual this) &&
        (this.entity1.toString.equals(that.entity1.toString) || this.entity1.toString.equals(that.entity2.toString)  ) &&
        (this.entity2.toString.equals(that.entity2.toString) || this.entity2.toString.equals(that.entity1.toString)  ) &&
        relation.equals(that.relation)
    }
    case _ => {
     true
    }

  }

  override def toString:String = {
    "[ entity1: " + entity1.toString + " ,entity2: " + entity2.toString +" ,relation: "+relation +" ]"
  }


  override def hashCode(): Int = {
    val state = Seq(entity1, entity2, relation)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

object Cell {
  val TYPE_DT_PROPERTY = "dp"
  val TYPE_OBJECT_PROPERTY = "op"
  val TYPE_CLASS ="c"
  val TYPE_UNKOWN ="u"
}
