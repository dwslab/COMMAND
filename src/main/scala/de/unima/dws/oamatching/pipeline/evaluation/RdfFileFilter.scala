package de.unima.dws.oamatching.pipeline.evaluation

import java.io.{File, FileFilter}

class RdfFileFilter extends FileFilter {
		def accept(file:File):Boolean = {


		 return file.getName().endsWith(".rdf")
		}
}