name := "ontologymatcher"

version := "1.0"

scalaVersion := "2.10.4"

libraryDependencies += "org.apache.spark" %% "spark-core" % "1.2.1"

libraryDependencies += "org.apache.spark" %% "spark-mllib" % "1.2.1"

libraryDependencies += "net.sourceforge.owlapi" % "owlapi-distribution" % "3.5.2"

libraryDependencies += "org.apache.jena" % "jena-core" % "2.12.1"

//gilibraryDependencies += "edu.cmu.lti" % "ws4j" % "1.0.1"

libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.1.1"

libraryDependencies += "net.debasishg" %% "redisclient" % "2.13"

libraryDependencies += "org.slf4j" % "slf4j-log4j12" % "1.7.8"

libraryDependencies += "secondstring" % "secondstring" % "20120620"

//libraryDependencies += "net.sf.jwordnet" % "jwnl" % "1.4_rc3"

//libraryDependencies += "fr.inrialpes.exmo" % "ontosim" % "2.4"

libraryDependencies += "org.apache.lucene" % "lucene-analyzers-common" % "4.10.3"

libraryDependencies += "org.slf4j" % "slf4j-log4j12" % "1.7.8"

//libraryDependencies += "de.unima.alex" % "owlsimflood" % "1.0"

libraryDependencies +=  "org.scalaj" %% "scalaj-http" % "1.1.1"

libraryDependencies += "org.scalanlp" % "epic_2.10" % "0.3"

libraryDependencies += "com.typesafe.play" % "play-json_2.10" % "2.4.0-M2"

libraryDependencies += "org.scalanlp" % "english_2.10" % "2015.1.25"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.4.1"

libraryDependencies += "org.jfree" % "jfreechart" % "1.0.19"

libraryDependencies += "com.itextpdf" % "itextpdf" % "5.0.6"

libraryDependencies += "com.typesafe" % "config" % "1.2.1"

libraryDependencies += "com.typesafe.scala-logging" % "scala-logging-slf4j_2.10" % "2.1.2"

libraryDependencies += "de.unima.dws" % "alcomo" % "1.01"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

resolvers += "Local Maven Repository" at "file:///Users/mueller/.m2/repository/"

resolvers += "Third Party" at "http://trianacode.org/maven/"