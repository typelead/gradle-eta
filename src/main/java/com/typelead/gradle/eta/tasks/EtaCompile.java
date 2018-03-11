package com.typelead.gradle.eta.tasks;

import java.io.File;
import java.util.List;
import java.util.ArrayList;
import java.util.Set;

import org.gradle.api.DefaultTask;
import org.gradle.api.GradleException;
import org.gradle.api.Project;
import org.gradle.api.file.Directory;
import org.gradle.api.file.DirectoryProperty;
import org.gradle.api.file.RegularFile;
import org.gradle.api.tasks.Input;
import org.gradle.api.tasks.InputDirectory;
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
import com.typelead.gradle.eta.internal.ConfigurationUtils;

public class EtaCompile extends SourceTask {

    private final Provider<ResolvedExecutable> resolvedEta;
    private Provider<String> targetConfiguration;
    private DirectoryProperty destinationDir;
    private DirectoryProperty classesDir;
    private List<File> extraClasspath = new ArrayList<File>();
    private Provider<File> packageDB;
    private Provider<File> outputJar;

    public EtaCompile() {
        final Project project = getProject();
        final EtaExtension extension =
            project.getRootProject().getExtensions().getByType(EtaExtension.class);
        this.resolvedEta = extension.getEta();

        this.destinationDir = project.getLayout().directoryProperty();
        this.classesDir     = project.getLayout().directoryProperty();

        /* TODO: Do these paths need to be portable? */
        this.packageDB = project.provider
            (() -> {
                File file = destinationDir
                  .file("dist-newstyle/packagedb/eta-" + getEtaVersion())
                  .get().getAsFile();
                if (file == null) {
                    /* TODO: This seems like a hack since null provider values are not
                       allowed. Maybe use Optional instead? */
                    file = project.getLayout().getBuildDirectory().getAsFile().get();
                }
                return file;
            });

        this.outputJar = project.provider
            (() -> {
                Set<File> files = project.fileTree(destinationDir.get(),
                                                   fileTree ->
                                                   fileTree.include
                                                   ("**/*" + project.getName()
                                                           + "*.jar"))
                                         .getFiles();

                /* TODO: This seems like a hack since null provider values are not
                   allowed. Maybe use Optional instead? */
                File file = project.getLayout().getBuildDirectory().getAsFile().get();
                if (files.size() > 1) {
                    throw new GradleException("More than one output jar file was found.");
                } else {
                    for (File f : files) {
                        file = f;
                        break;
                    }
                }
                return file;
            });
    }

    @Input
    public String getEtaVersion() {
        return resolvedEta.get().getVersion();
    }

    @Input
    public Provider<String> getTargetConfiguration() {
        return targetConfiguration;
    }

    public void setTargetConfiguration(Provider<String> targetConfiguration) {
        this.targetConfiguration = targetConfiguration;
    }

    @Input
    public Provider<File> getDestinationDir() {
        return destinationDir.getAsFile();
    }

    public void setDestinationDir(Provider<Directory> destinationDir) {
        this.destinationDir.set(destinationDir);
    }

    @Input
    public Provider<Directory> getClassesDir() {
        return classesDir;
    }

    public void setClassesDir(Provider<Directory> classesDir) {
        this.classesDir.set(classesDir);
    }

    @InputFiles
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
    public Provider<File> getPackageDB() {
        return packageDB;
    }

    @OutputFile
    public Provider<File> getOutputJarFile() {
        return outputJar;
    }

    @TaskAction
    public void compile() {

        final Project project = getProject();

        /* Create the destination directory if it doesn't exist. */

        File workingDir = destinationDir.getAsFile().get();

        if (!workingDir.exists() && !workingDir.mkdirs()) {
            throw new GradleException("Unable to create destination directory: "
                                      + workingDir.getAbsolutePath());
        }

        /* Create cabal.project.local file that will contain configuration options:
           - Configure the `-cp` flag
        */

        Set<File> classpathFiles = project.getConfigurations()
            .getByName(getTargetConfiguration().get()).getFiles();

        classpathFiles.addAll(getExtraClasspath());

        CabalHelper.generateCabalProjectLocalFile(classpathFiles, workingDir);

        /* Fork an etlas process to fetch the dependencies. */

        EtlasCommand etlas = new EtlasCommand(project);
        etlas.getWorkingDirectory().set(destinationDir.getAsFile());
        etlas.build();

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
