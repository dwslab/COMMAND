package de.unima.dws.command;

import de.unima.dws.oamatching.pipeline.CommandRun;
import de.unima.dws.oamatching.pipeline.RunConfigurationOneProblem;

import java.io.File;
import java.net.URL;

/**
 * Created by mueller on 25/08/15.
 */
public class Command {
    public static void main (String args[]) throws Exception{
        URL url = alignTwoOntologies("graduate_example/mastersprogram1.owl","graduate_example/mastersprogram2.owl","alignments");
        System.out.println(url.toString());
    }


    public static URL alignTwoOntologies(String path_to_onto1, String path_to_onto2, String base_folder_for_alignments) throws Exception {

        RunConfigurationOneProblem config = CommandRun.parseRunOneProblemConfigWithPath(path_to_onto1, path_to_onto2);

        String alignment_file = CommandRun.runSingleProblemWithAlignment(config, base_folder_for_alignments);

        File f = new File(alignment_file);
        return f.toURI().toURL();
    }
}
