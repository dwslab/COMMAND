package de.unima.dws.oamatching.pipeline.evaluation

import java.io.File

import de.unima.alcomox.ontology.IOntology
import de.unima.dws.oamatching.core.{Alignment, AlignmentParser, OntologyLoader}
import de.unima.dws.oamatching.pipeline.MatchingProblem

/**
 * Trait that implements the OAEI Challenge Dataset parsing
 *
 * Created by mueller on 01/03/15.
 */
trait EvaluationDataSetParser {

  /**
   * Parses the problem by its name in a given folder
   * @param name
   * @param path_to_folder
   * @return
   */
  def parseProblems(name: String, path_to_folder: String): Seq[EvaluationMatchingTask] = {
    name match {
      case "benchmarks" => parseBenchmarks(path_to_folder)
      case "conference" => parseConference(path_to_folder)
      case "anatomy" =>  parseAnatomy(path_to_folder)
      case other => parseConference(path_to_folder)
    }
  }

  def parseAnatomy(path_to_folder:String):Seq[EvaluationMatchingTask] = {

    val mouse_onto_name = path_to_folder +File.separator+"mouse.owl"
    val human_onto_name = path_to_folder +File.separator+"human.owl"

    val ref = path_to_folder +File.separator+"reference.rdf"

    val onto1 = OntologyLoader.load_fast_ontology(mouse_onto_name)
    val onto2 = OntologyLoader.load_fast_ontology(human_onto_name)

    val ionto_1 = new IOntology(mouse_onto_name)
    val ionto_2 = new IOntology(human_onto_name)

    val name: String = "mouse-human"

    val matching_problem = MatchingProblem(onto1, onto2,ionto_1,ionto_2, name,"anatomy")

    val reference: Alignment = AlignmentParser.parseRDFWithOntos(ref,mouse_onto_name,human_onto_name)

    //quick hack TODO change
    reference.onto1_reference = onto1
    reference.onto2_reference = onto2


    Seq(EvaluationMatchingTask(matching_problem, reference))
  }

  /**
   * Parses the conference dataset from OAEI challenge
   * @param path_to_folder
   * @return
   */
  def parseConference(path_to_folder: String): Seq[EvaluationMatchingTask] = {
    val folder: File = new File(path_to_folder + File.separator + "reference-alignment/")

    val problems = for (ref_align_file <- folder.listFiles(new RdfFileFilter)) yield {

      val ontos: List[String] = ref_align_file.getName().split("-").toList
      val name_onto1: String = path_to_folder + File.separator + ontos(0).replaceAll("-", "") + ".owl"
      val name_onto2: String = path_to_folder + File.separator + ontos(1).replaceAll("-", "").replaceAll(".rdf", "") + ".owl"


      val onto1 = OntologyLoader.load_fast_ontology(name_onto1)
      val onto2 = OntologyLoader.load_fast_ontology(name_onto2)

      val ionto_1 = new IOntology(name_onto1)
      val ionto_2 = new IOntology(name_onto2)


      //parse alignments
      val reference: Alignment = AlignmentParser.parseRDFWithOntos(ref_align_file.getAbsolutePath(), name_onto1, name_onto2)

      //quick hack TODO change
      reference.onto1_reference = onto1
      reference.onto2_reference = onto2

      val name: String = ref_align_file.getName().dropRight(4)
      val matching_problem = MatchingProblem(onto1, onto2,ionto_1,ionto_2, name,"conference")

      EvaluationMatchingTask(matching_problem, reference)
    }

    problems
  }


  def parseBenchmarks(path_to_folder: String): Seq[EvaluationMatchingTask] = {

    val left_name: String = "101"
    val onto_left_file: File = new File(path_to_folder + File.separator + left_name + File.separator + "onto.rdf")
    val onto_left = OntologyLoader.load_fast_ontology(onto_left_file.getPath)

    val folder: File = new File(path_to_folder)

    val problems: Array[EvaluationMatchingTask] = for (benchmark_folders <- folder.listFiles(new FolderFilter)) yield {
      val onto_right_path: String = benchmark_folders.getAbsolutePath + File.separator + "onto.rdf"
      val onto_right = OntologyLoader.load_fast_ontology(onto_right_path)

      val right_name: String = benchmark_folders.getName

      val ref_align_path: String = benchmark_folders.getAbsolutePath + File.separator + "refalign.rdf"

      val reference_alignment: Alignment = AlignmentParser.parseRDFWithOntos(ref_align_path, onto_left_file.getAbsolutePath, onto_right_path)

      val ionto_1 = new IOntology(onto_left_file.getPath)
      val ionto_2 = new IOntology(onto_right_path)

      val name: String = left_name + "-" + right_name

      //quick hack TODO change
      reference_alignment.onto1_reference = onto_left
      reference_alignment.onto2_reference = onto_right


      val matching_problem = MatchingProblem(onto_left, onto_right,ionto_1,ionto_2, name,"benchmarks")

      EvaluationMatchingTask(matching_problem, reference_alignment)
    }

    problems
  }

  def parseSingle(f_onto1: String, f_onto2: String, f_reference: String): EvaluationMatchingTask = {

    //name of the problem is the name of the rdf file alignment without .rdf extension
    val name: String = f_reference.dropRight(4)
    val onto1 = OntologyLoader.load_fast_ontology(f_onto1)
    val onto2 = OntologyLoader.load_fast_ontology(f_onto2)

    val ionto_1 = new IOntology(f_onto1)
    val ionto_2 = new IOntology(f_onto2)

    val reference: Alignment = AlignmentParser.parseRDF(f_reference)
    //quick hack TODO change
    reference.onto1_reference = onto1
    reference.onto2_reference = onto2

    val matching_problem = MatchingProblem(onto1, onto2,ionto_1,ionto_2, name)

    EvaluationMatchingTask(matching_problem, reference)
  }


  def getListofProblemMatchingTasks(problems: Seq[EvaluationMatchingTask], path_to_matchings: String): List[(EvaluationMatchingTask, File)] = {

    //read matchings folder
    val file: File = new File(path_to_matchings)
    val list_of_raw_matchings = file.listFiles().toList

    //build pairs of matchings to reference alignment
    val problem_matching_pairs = problems.map(problem => {
      val name = problem.matching_problem.name

      list_of_raw_matchings.map(raw_file => {
        val id_part = raw_file.getName.split("_")(0)

        if (id_part.equals(name)) {
          Option(problem, raw_file)
        } else {
          Option.empty
        }
      }).filter(_.isDefined).head.get
    })

    problem_matching_pairs.toList
  }


}
