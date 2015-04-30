# COMMAND
COMMAND is a novel approach for dynamically selecting and combining ontology matchers. The key idea is to use outlier or anomaly detection for the combination of matchers. The system is completely unsupervised and can be used for any ontology matching problem of owl ontologies.


## Dependencies
- [Simple Build Tool (SBT)](http://www.scala-sbt.org/)
- Rapidminer
- Apache Spark
- ...

## Usage Guide

### How to install
In order to be able to install you have to use SBT to build it yourself. Currently no binary distribution is available. So first you have to install [SBT](http://www.scala-sbt.org/). Then navigate to the COMMAND folder and type

```bash
sbt update
sbt compile
```

### How to use

COMMAND is a highly flexible system that can be configured to match arbitrary ontology matching problems. But it also uses external resource for specific base matchers. Under the [matcher_config.csv](https://github.com/dwslab/COMMAND/blob/master/matcher_config.csv) the standard matcher combination is shown. This relies on the following external resources:
- Word2Vec Word representations trained with spark. Can be downloaded [10pWebbaseVector](www.concept2designs.de/word2vec/Webbase10P.zip)
- The UMLS medical methathesaurus lexicon, preprocessed of the [AML](https://github.com/AgreementMakerLight/AML-Project/blob/master/AgreementMakerLight/store/knowledge/UMLS.lexicon)
- [UMBC Similarity Models](http://swoogle.umbc.edu/SimService/index.html)

All this locations need to be specified in the application.conf.

An example configuration to match two ontologies is given below in the end.

Ontologies to test COMMAND can be obtained from the [Ontology Alignment Evaluation Initiative](http://oaei.ontologymatching.org/2014/). 

When having set up the configuration correctly one now has to type:

```bash
export JAVA_OPTS="-Xmx10g"
sbt 'run-main de.unima.dws.oamatching.pipeline.command.RunSinglePipeline'
```

This will create and evaluate an alignment. The alignment is serialized in RDF format to the folder alignments and in the runtime.log one can see the evaluation results in terms of Precision, Recall and F1Measure.

```javascript
rapidminerconfig {
  cleanup = true
  tmp = "tmp"
  tmp_process_location = "tmp/processes"
  tmp_matching_location = "tmp/matchings"
}

general {
  log_times = true
  base_threshold = 0.2
  log_results_on_server = true
  serveraddress = "http://128.199.50.209:3000/api/experiments"
  names_separated = true
  feature_selection = false
  min_stdev = 0.1
  feature_selection_values = {
    min = 1.0
    max = 0.5
  }
}


oaei {
  path_to_dataset =  "ontos/2014/benchmarks"
  dataset_type ="benchmarks"
  reuse_vectors = true
  timestamp_vector_name = false
}


pipeline {
  problem_name="test"
  data = {
    source_onto="ontos/2014/conference/edas.owl"
    target_onto="ontos/2014/conference/ekaw.owl"
  }
  ##if no evaluation is specified the alignment is not evaluated but created
  evaluation = {
    reference_alignment="ontos/2014/conference/reference-alignment/edas-ekaw.rdf" 
  }
  
  max_threads = 2
  selection = {
    method = "greedy_rank"
    fuzzy = 1.0
  }
  norm = "euclidean_max"
  outlier_method = "cblof_regular_db"
  debug_alignment = true
  class_threshold =0.3
  dp_threshold = 0.4
  op_threshold = 0.6
  prepro = {
    type = "pca_variant"
    values = {
      variance = 0.75
    }
  }
  mining = {
    minpoints=6.0
    alpha=99.0
    epsilon=1.0
  }
}

resources={
  umls = {
    path = "UMLS.lexicon"
  }
  umbc = {
    model_c_webbase="/Users/mueller/Coding/umbc_models/webbase_concept"
    model_r_webbase="/Users/mueller/Coding/umbc_models/webbase_relation"
    model_c_giga="/Users/mueller/Coding/umbc_models/Gigawords_concept"
    model_r_giga="/Users/mueller/Coding/umbc_models/Gigawords_relation"
    model_pos="/Users/mueller/Coding/umbc_models/wsj-left3words/wsj-0-18-left3words-distsim.tagger"
  }
  w2v = {
    model_stemmed = "/Users/mueller/Coding/Word2Vectors/Webbase10P/model_word2vec_stemmed.ser"
    model_normal = "/Users/mueller/Coding/Word2Vectors/Webbase10P/model_word2vec.ser"
  }
  stoplist="stoplist.txt"
}
```
