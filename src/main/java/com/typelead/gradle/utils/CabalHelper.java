package com.typelead.gradle.utils;

import java.io.File;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import com.typelead.gradle.utils.FileUtils;
import com.typelead.gradle.utils.Collections;
import com.typelead.gradle.eta.api.SourceRepository;

public class CabalHelper {
    public static void generateCabalFile(String projectName,
                                         String projectVersion,
                                         List<String> dependencyConstraints,
                                         File workingDir) {
        generateCabalFile(projectName, projectVersion, null, null, null,
                          dependencyConstraints, workingDir);
    }

    public static void generateCabalFile(String projectName,
                                         String projectVersion,
                                         String maybeExecutable,
                                         List<String> sourceDirectories,
                                         List<String> modules,
                                         List<String> dependencyConstraints,
                                         File workingDir) {
        boolean hasModules = modules != null;
        StringBuilder sb = new StringBuilder();
        println(sb, "name: " + projectName);
        println(sb, "version: " + fixVersion(projectVersion));
        println(sb, "cabal-version: >= 1.10");
        println(sb, "build-type: Simple");
        if (maybeExecutable != null) {
            println(sb, "executable " + projectName);
            sb.append("    main-is: ");
            println(sb, maybeExecutable);
            if (hasModules) {
                println(sb, "    other-modules:");
            }
        } else {
            println(sb, "library");
            if (hasModules) {
                println(sb, "    exposed-modules:");
            }
        }

        if (modules != null) {
            for (String module : modules) {
                sb.append("        ");
                println(sb, module);
            }
        }

        if (Collections.isNonEmpty(sourceDirectories)) {
            sb.append("    hs-source-dirs: ");
            Iterator<String> it = sourceDirectories.iterator();
            sb.append(it.next());
            while (it.hasNext()) {
                sb.append(", ");
                sb.append(it.next());
            }
        }

        println(sb, "    build-depends: base");

        for (String dependencyConstraint : dependencyConstraints) {
            println(sb, "                 , " + dependencyConstraint);
        }

        FileUtils.write(new File(workingDir, projectName + ".cabal"), sb.toString());
    }

    public static void generateCabalProjectFile
        (final Set<SourceRepository> sourceRepositories,
         final File workingDir) {
        generateCabalProjectFile(sourceRepositories, null, workingDir);
    }

    public static void generateCabalProjectFile
        (final Set<SourceRepository> sourceRepositories,
         final Collection<File> packageDBs,
         final File workingDir) {
        StringBuilder sb = new StringBuilder();
        println(sb, "packages: .");
        if (Collections.isNonEmpty(sourceRepositories)) {
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
        }
        if (Collections.isNonEmpty(packageDBs)) {
            println(sb, "program-options");
            sb.append("  eta-options:");
            for (File packageDB : packageDBs) {
                sb.append(" -package-db ");
                sb.append(packageDB.getPath());
            }
            sb.append(NEWLINE);
        }
        FileUtils.write(new File(workingDir, "cabal.project"), sb.toString());
    }

    public static void generateCabalProjectLocalFile
        (final Collection<File> classpathFiles, final File workingDir) {
        final StringBuilder sb = new StringBuilder();
        if (Collections.isNonEmpty(classpathFiles)) {
            println(sb, "program-options");
            sb.append("  eta-options: -cp \"");
            Iterator<File> it = classpathFiles.iterator();
            char sep = File.pathSeparatorChar;
            sb.append(it.next());
            while (it.hasNext()) {
                sb.append(sep);
                sb.append(it.next().getPath());
            }
            println(sb, "\"");
        }
        FileUtils.write(new File(workingDir, "cabal.project.local"), sb.toString());
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
