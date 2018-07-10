package com.typelead.gradle.utils;

import java.io.File;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import com.typelead.gradle.utils.FileUtils;
import com.typelead.gradle.utils.Collections;
import com.typelead.gradle.eta.api.EtaOptions;
import com.typelead.gradle.eta.api.SourceRepository;
import static com.typelead.gradle.utils.PrintHelper.*;

public class CabalHelper {

    public static WriteResult generateCabalFile(String projectName,
                                         String projectVersion,
                                         List<String> dependencyConstraints,
                                         File workingDir) {
        return generateCabalFile(projectName, projectVersion, null, null, null, null,
                                 dependencyConstraints, workingDir);
    }

    public static WriteResult generateCabalFile
        (String projectName, String projectVersion, String maybeExecutable,
         List<String> sourceDirectories, List<String> modules, EtaOptions options,
         List<String> dependencyConstraints, File workingDir) {
        StringBuilder sb = new StringBuilder();
        println(sb, "name: " + projectName);
        println(sb, "version: " + projectVersion);
        println(sb, "cabal-version: >= 1.10");
        println(sb, "build-type: Simple");
        println(sb, "library");
        boolean hasModules = Collections.isNonEmpty(modules);
        if (hasModules) {
            println(sb, "    exposed-modules:");
            for (String module : modules) {
                print  (sb, "        ");
                println(sb, module);
            }
        }
        printStanzaCommon(sb, sourceDirectories, dependencyConstraints, options);

        if (maybeExecutable != null) {
            println(sb, "    eta-options: -shared " + maybeExecutable + " -this-unit-id main");
        }

        return snapshotWrite(new File(workingDir, projectName + ".cabal"),
                             sb.toString(),
                             new File(workingDir, projectName + ".cabal.snapshot"));
    }

    private static void printStanzaCommon(final StringBuilder sb,
                                          final List<String> sourceDirectories,
                                          final List<String> dependencyConstraints,
                                          final EtaOptions options) {
        if (Collections.isNonEmpty(sourceDirectories)) {
            print(sb, "    hs-source-dirs: ");
            Iterator<String> it = sourceDirectories.iterator();
            print(sb, it.next());
            while (it.hasNext()) {
                print(sb, ", ");
                print(sb, it.next());
            }
            print(sb, NEWLINE);
        }

        println(sb, "    build-depends: base");

        if (Collections.isNonEmpty(dependencyConstraints)) {
            for (String dependencyConstraint : dependencyConstraints) {
                println(sb, "                 , " + dependencyConstraint);
            }
        }

        if (options != null) {
            /* TODO: Abstract out the redundancies with functions. */
            print  (sb, "    default-language: ");
            println(sb, options.getLanguage());

            Iterator<String> extensionsIt =
                options.getExtensions().iterator();
            if (extensionsIt.hasNext()) {
                print  (sb, "    default-extensions: ");
                println(sb, extensionsIt.next());
                while (extensionsIt.hasNext()) {
                    print  (sb, "                        ");
                    println(sb, extensionsIt.next());
                }
            }
            Iterator<String> argsIt =
                options.getArgs().iterator();
            if (argsIt.hasNext()) {
                print(sb, "    eta-options: ");
                /* TODO: Handle arguments with spaces via quotes? */
                print(sb, argsIt.next());
                while (argsIt.hasNext()) {
                    print(sb, " ");
                    print(sb, argsIt.next());
                }
                print(sb, NEWLINE);
            }

            Iterator<String> cppIt =
                options.getCpp().iterator();
            if (cppIt.hasNext()) {
                print(sb, "    cpp-options: ");
                /* TODO: Handle arguments with spaces via quotes? */
                print(sb, cppIt.next());
                while (cppIt.hasNext()) {
                    print(sb, " ");
                    print(sb, cppIt.next());
                }
                print(sb, NEWLINE);
            }

            Iterator<String> installIncludesIt =
                options.getInstallIncludes().iterator();
            if (installIncludesIt.hasNext()) {
                print  (sb, "    install-includes: ");
                /* TODO: Handle arguments with spaces via quotes? */
                println(sb, installIncludesIt.next());
                while (installIncludesIt.hasNext()) {
                    print  (sb, "                      ");
                    println(sb, installIncludesIt.next());
                }
            }

            Iterator<String> includeDirsIt =
                options.getIncludeDirs().iterator();
            if (includeDirsIt.hasNext()) {
                print(sb, "    include-dirs: ");
                /* TODO: Handle arguments with spaces via quotes? */
                print(sb, includeDirsIt.next());
                while (includeDirsIt.hasNext()) {
                    print(sb, " ");
                    print(sb, includeDirsIt.next());
                }
                print(sb, NEWLINE);
            }
        }

    }

    public static WriteResult generateCabalProjectFile
        (final Set<SourceRepository> sourceRepositories,
         final File workingDir) {
        return generateCabalProjectFile(sourceRepositories, null, workingDir);
    }

    public static WriteResult generateCabalProjectFile
        (final Set<SourceRepository> sourceRepositories,
         final Collection<File> packageDBs,
         final File workingDir) {
        StringBuilder sb = new StringBuilder();
        println(sb, "packages: .");
        if (Collections.isNonEmpty(packageDBs)) {
            println(sb, "package-dbs:");
            for (File packageDB : packageDBs) {
                print  (sb, "  ");
                println(sb, packageDB.getAbsolutePath());
            }
            print  (sb, NEWLINE);
        }
        if (Collections.isNonEmpty(sourceRepositories)) {
            for (SourceRepository sourceRepository : sourceRepositories) {
                println(sb, "source-repository-package");
                println(sb, "  type: git");
                println(sb, "  location: " + sourceRepository.getLocation());
                switch (sourceRepository.getCommitIdentifierType()) {
                case BRANCH:
                    print(sb, "  branch: ");
                    break;
                case TAG:
                    print(sb, "  tag: ");
                    break;
                case COMMIT:
                    print(sb, "  commit: ");
                    break;
                }
                println(sb, sourceRepository.getCommitIdentifier());
            }
        }

        return snapshotWrite(new File(workingDir, "cabal.project"),
                             sb.toString(),
                             new File(workingDir, "cabal.project.snapshot"));
    }

    public static WriteResult generateCabalProjectLocalFile
        (final String projectName, final Collection<File> classpathFiles, final File workingDir) {
        final StringBuilder sb = new StringBuilder();
        if (Collections.isNonEmpty(classpathFiles)) {
            print  (sb, "package ");
            println(sb, projectName);
            print  (sb, "  eta-options: -cp \"");
            Iterator<File> it = classpathFiles.iterator();
            char sep = File.pathSeparatorChar;
            print(sb, it.next().getPath());
            while (it.hasNext()) {
                print(sb, sep);
                print(sb, it.next().getPath());
            }
            println(sb, "\"");
        }

        return snapshotWrite(new File(workingDir, "cabal.project.local"),
                             sb.toString(),
                             new File(workingDir, "cabal.project.local.snapshot"));
    }

    private static WriteResult snapshotWrite
        (File file, String fileContents, File snapshotFile) {

        boolean changed = SnapshotUtils.takeSnapshotAndCompare
            (snapshotFile, fileContents);

        if (changed) {
            FileUtils.write(file, fileContents);
        }

        return new WriteResult(file, changed);
    }

    public static class WriteResult {
        private final File    file;
        private final boolean changed;

        public WriteResult(File file, boolean changed) {
            this.file    = file;
            this.changed = changed;
        }

        public boolean isChanged() {
            return changed;
        }

        public File getFile() {
            return file;
        }
    }
}
