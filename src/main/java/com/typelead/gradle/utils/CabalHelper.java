package com.typelead.gradle.utils;

import java.io.File;
import java.util.Set;

import com.typelead.gradle.utils.FileUtils;
import com.typelead.gradle.eta.api.SourceRepository;

public class CabalHelper {
    public static void generateCabalFile(String projectName,
                                         String projectVersion,
                                         Set<String> dependencyConstraints,
                                         String workingDir) {
        StringBuilder sb = new StringBuilder();
        println(sb, "name: " + projectName);
        println(sb, "version: " + fixVersion(projectVersion));
        println(sb, "cabal-version: >= 1.10");
        println(sb, "build-type: Simple");
        println(sb, "library");
        println(sb, "    build-depends: base");
        for (String dependencyConstraint : dependencyConstraints) {
            println(sb, "                 , " + dependencyConstraint);
        }
        FileUtils.write(new File(workingDir, projectName + ".cabal"), sb.toString());
    }

    public static void generateCabalProjectFile
        (Set<SourceRepository> sourceRepositories, String workingDir) {
        StringBuilder sb = new StringBuilder();
        println(sb, "packages: .");
        for (SourceRepository sourceRepository : sourceRepositories) {
            println(sb, "source-repository-package");
            println(sb, "  type: git");
            println(sb, "  location: " + sourceRepository.getLocation());
            switch (sourceRepository.getCommitIdentifierType()) {
                case BRANCH:
                    sb.append("  branch: ");
                    break;
                case TAG:
                    sb.append("  tag: ");
                    break;
            }
            println(sb, sourceRepository.getCommitIdentifier());
        }
        FileUtils.write(new File(workingDir, "cabal.project"), sb.toString());
    }

    private static final String NEWLINE = System.lineSeparator();

    private static void println(StringBuilder sb, String message) {
        sb.append(message + NEWLINE);
    }

    /* TODO: Need to handle other cases and throw invalid if non-numeric digits are
             found. */
    private static String fixVersion(String version) {
        if (version.equals("unspecified")) {
            return "0.0.0";
        }
        return version;
    }
}
