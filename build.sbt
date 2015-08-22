import sbt.ExclusionRule

name := "ontologymatcher"

version := "1.0"

scalaVersion := "2.10.4"

conflictManager := sbt.ConflictManager.latestRevision

libraryDependencies += "org.apache.spark" %% "spark-core" % "1.2.1" excludeAll(
  ExclusionRule(organization = "org.slf4j"),
  ExclusionRule(organization = "org.apache.commons"),
  ExclusionRule(organization = "org.scala-lang"),
  ExclusionRule(organization = "xml-apis")
  )

libraryDependencies += "org.apache.spark" %% "spark-mllib" % "1.2.1" excludeAll (
  ExclusionRule(organization = "org.slf4j"),
  ExclusionRule(organization = "xml-apis")
  )

libraryDependencies += "org.apache.jena" % "jena-core" % "3.0.0" excludeAll (
  ExclusionRule(organization = "org.slf4j")
  )

libraryDependencies += "net.sourceforge.owlapi" % "owlapi-distribution" % "4.0.0" excludeAll(
  ExclusionRule(organization = "org.slf4j"),
  ExclusionRule(organization = "xerces"),
  ExclusionRule(organization = "org.apache.xerces"),
  ExclusionRule(organization = "xml-apis")
  )

libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.1.1" excludeAll (
  ExclusionRule(organization = "org.slf4j"),
  ExclusionRule(organization = "xml-apis")
  )

libraryDependencies += "org.slf4j" % "slf4j-log4j12" % "1.7.8"

libraryDependencies += "secondstring" % "secondstring" % "20120620" excludeAll(
  ExclusionRule(organization = "org.slf4j"),
  ExclusionRule(organization = "net.sourceforge.owlapi"),
  ExclusionRule(organization = "org.apache.xerces"),
  ExclusionRule(organization = "xml-apis")
  )

libraryDependencies += "org.apache.lucene" % "lucene-analyzers-common" % "4.10.3" excludeAll(
  ExclusionRule(organization = "org.slf4j"),
  ExclusionRule(organization = "net.sourceforge.owlapi"),
  ExclusionRule(organization = "org.apache.xerces")
  )

libraryDependencies += "org.scalaj" %% "scalaj-http" % "1.1.1" excludeAll(
  ExclusionRule(organization = "org.slf4j"),
  ExclusionRule(organization = "xerces"),
  ExclusionRule(organization = "org.apache.xerces")
  )

libraryDependencies += "org.scalanlp" % "epic_2.10" % "0.3" excludeAll (
  ExclusionRule(organization = "org.slf4j"),
  ExclusionRule(organization = "xml-apis")
  )

libraryDependencies += "com.typesafe.play" % "play-json_2.10" % "2.4.0-M2" excludeAll(
  ExclusionRule(organization = "org.slf4j"),
  ExclusionRule(organization = "xerces"),
  ExclusionRule(organization = "org.apache.xerces"),
  ExclusionRule(organization = "xml-apis")
  )

libraryDependencies += "org.scalanlp" % "english_2.10" % "2015.1.25" excludeAll(
  ExclusionRule(organization = "org.slf4j"),
  ExclusionRule(organization = "net.sourceforge.owlapi"),
  ExclusionRule(organization = "xerces"),
  ExclusionRule(organization = "org.apache.xerces"),
  ExclusionRule(organization = "xml-apis")
  )

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.4.1" excludeAll(
  ExclusionRule(organization = "org.slf4j"),
  ExclusionRule(organization = "xerces"),
  ExclusionRule(organization = "org.apache.xerces"),
  ExclusionRule(organization = "xml-apis")
  )

libraryDependencies += "com.typesafe" % "config" % "1.2.1" excludeAll (
  ExclusionRule(organization = "org.slf4j")
  )

libraryDependencies += "com.typesafe.scala-logging" % "scala-logging-slf4j_2.10" % "2.1.2" excludeAll (
  ExclusionRule(organization = "org.slf4j"),
  ExclusionRule(organization = "xml-apis")
  )

libraryDependencies += "de.dwslab" % "Alcomo" % "1.01" excludeAll(
  ExclusionRule(organization = "org.slf4j"),
  ExclusionRule(organization = "net.sourceforge.owlapi"),
  ExclusionRule(organization = "com.github.ansell.owlapi"),
  ExclusionRule(organization = "xerces"),
  ExclusionRule(organization = "org.apache.xerces"),
  ExclusionRule(organization = "xml-apis")
  )


libraryDependencies += "org.openrdf.sesame" % "sesame-rio" % "2.8.5"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

resolvers += "Local Maven Repository" at "file:///Users/mueller/.m2/repository/"

resolvers += "Third Party" at "http://trianacode.org/maven/"

resolvers += "dws-public" at "https://breda.informatik.uni-mannheim.de/nexus/content/groups/public/"

