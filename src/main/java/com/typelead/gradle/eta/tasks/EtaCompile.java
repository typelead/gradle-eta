package com.typelead.gradle.eta.tasks;

import java.io.File;
import java.util.List;
import java.util.ArrayList;
import java.util.Set;

import groovy.lang.Closure;

import org.gradle.api.GradleException;
import org.gradle.api.Project;
import org.gradle.api.file.Directory;
import org.gradle.api.file.DirectoryProperty;
import org.gradle.api.file.FileCollection;
import org.gradle.api.file.RegularFile;
import org.gradle.api.tasks.CompileClasspath;
import org.gradle.api.tasks.Input;
import org.gradle.api.tasks.Internal;
import org.gradle.api.tasks.InputDirectory;
import org.gradle.api.tasks.InputFile;
import org.gradle.api.tasks.InputFiles;
import org.gradle.api.tasks.OutputFile;
import org.gradle.api.tasks.OutputDirectory;
import org.gradle.api.tasks.SourceTask;
import org.gradle.api.tasks.TaskAction;
import org.gradle.api.provider.Provider;

import com.typelead.gradle.utils.EtlasCommand;
import com.typelead.gradle.utils.CabalHelper;
import com.typelead.gradle.utils.ResolvedExecutable;
import com.typelead.gradle.eta.api.EtaExtension;
import com.typelead.gradle.eta.api.EtaOptions;
import com.typelead.gradle.eta.internal.ConfigurationUtils;

public class EtaCompile extends SourceTask {

    private final Provider<ResolvedExecutable> resolvedEta;
    private DirectoryProperty destinationDir;
    private DirectoryProperty classesDir;
    private Provider<FileCollection> classpathProvider;
    private List<File> extraClasspath = new ArrayList<File>();
    private Provider<File> packageDB;
    private Provider<File> outputJar;
    private Provider<RegularFile>  cabalProjectFile;
    private Provider<RegularFile>  cabalFile;
    private EtaOptions etaOptions;

    public EtaCompile() {
        final Project project = getProject();
        final EtaExtension extension =
            project.getRootProject().getExtensions().getByType(EtaExtension.class);
        this.resolvedEta = extension.getEta();

        this.destinationDir = project.getLayout().directoryProperty();
        this.classesDir     = project.getLayout().directoryProperty();

        /* TODO: Do these paths need to be portable? */
        this.packageDB = project.provider
            (() ->
             new File(getDestinationDir(), "dist/packagedb/eta-" + getEtaVersion()));

        this.outputJar = project.provider
            (() -> {
                File destinationDir = getDestinationDir();
                try {
                    return project.fileTree
                        (destinationDir, fileTree ->
                         fileTree.include("**/eta-" + getEtaVersion() + "/**/*"
                                          + project.getName() + "*.jar"))
                        .getSingleFile();

                } catch (IllegalStateException e) {
                    return new File(destinationDir,
                                    "eta-" + getEtaVersion() + "-stub-output.jar");
                }
            });

        getOutputs().upToDateWhen(task -> false);
    }

    @Input
    public String getEtaVersion() {
        return resolvedEta.get().getVersion();
    }

    @Internal
    public EtaOptions getOptions() {
        return etaOptions;
    }

    public void setOptions(EtaOptions etaOptions) {
        this.etaOptions = etaOptions;
    }

    public void options(Closure configure) {
        getProject().configure(etaOptions, configure);
    }

    @InputFile
    public File getCabalProjectFile() {
        return cabalProjectFile.get().getAsFile();
    }

    public void setCabalProjectFile(Provider<RegularFile> cabalProjectFile) {
        this.cabalProjectFile = cabalProjectFile;
    }

    @InputFile
    public File getCabalFile() {
        return cabalFile.get().getAsFile();
    }

    public void setCabalFile(Provider<RegularFile> cabalFile) {
        this.cabalFile = cabalFile;
    }

    public void setClassesDir(Provider<Directory> classesDir) {
        this.classesDir.set(classesDir);
    }

    @CompileClasspath
    public FileCollection getClasspath() {
        return classpathProvider.get();
    }

    public void setClasspath(Provider<FileCollection> classpathProvider) {
        this.classpathProvider = classpathProvider;
    }

    @CompileClasspath
    public List<File> getExtraClasspath() {
        return extraClasspath;
    }

    public void addExtraClasspath(File dirOrJarPath) {
        this.extraClasspath.add(dirOrJarPath);
    }

    /* TODO: This can be made more efficient! For large multi-module projects
             we will be sending *a lot* of package dbs! Instead, we can collect
             the .conf files and construct a new package db via eta-pkg recache
             on every build. */

    @OutputDirectory
    public File getPackageDB() {
        return packageDB.get();
    }

    @Internal
    public Provider<File> getPackageDBProvider() {
        return packageDB;
    }

    @OutputFile
    public File getOutputJarFile() {
        return outputJar.get();
    }

    @Internal
    public Provider<File> getOutputJarFileProvider() {
        return outputJar;
    }

    @OutputDirectory
    public File getDestinationDir() {
        return destinationDir.getAsFile().get();
    }

    public void setDestinationDir(Provider<Directory> destinationDir) {
        this.destinationDir.set(destinationDir);
    }

    @TaskAction
    public void compile() {

        final Project project = getProject();

        final File workingDir = getDestinationDir();

        /* Create cabal.project.local file that will contain configuration options:
           - Configure the `-cp` flag
        */

        Set<File> classpathFiles = getClasspath().getFiles();

        classpathFiles.addAll(getExtraClasspath());

        CabalHelper.generateCabalProjectLocalFile(project.getName(),
                                                  classpathFiles, workingDir);

        /* Fork an etlas process to fetch the dependencies. */

        final EtlasCommand etlas = new EtlasCommand(project);

        etlas.getWorkingDirectory().set(workingDir);

        boolean isUpToDate = etlas.build();
        setDidWork(!isUpToDate);

        Directory classesDir = this.classesDir.getOrNull();

        if (classesDir != null && !isUpToDate) {

            /* Extract the Jar file into the classes directory so the rest of the
               Gradle Java pipeline can work as intended.

               TODO: Add an option for the Eta compiler to generate classfiles directly.
            */

            project.copy(copySpec -> {
                    copySpec.from(project.zipTree(getOutputJarFile()));
                    copySpec.into(classesDir);
                });
        }

    }
}
