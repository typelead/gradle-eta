package com.typelead.gradle.eta.tasks;

import java.io.File;

import org.gradle.api.DefaultTask;
import org.gradle.api.GradleException;
import org.gradle.api.file.Directory;
import org.gradle.api.file.DirectoryProperty;
import org.gradle.api.file.RegularFile;
import org.gradle.api.tasks.Input;
import org.gradle.api.tasks.OutputFile;
import org.gradle.api.tasks.TaskAction;
import org.gradle.api.provider.Provider;
import org.gradle.api.artifacts.dsl.DependencyHandler;

import com.typelead.gradle.utils.EtlasCommand;

public class EtaFetchDependencies extends DefaultTask {

    private DirectoryProperty destinationDir;
    private Provider<String> targetConfiguration;

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

    // TODO: This is a HACK to get incremental task execution working.
    @OutputFile
    public Provider<RegularFile> getCabalProjectFile() {
        return destinationDir.file
            (EtaInstallDependencies.DEFAULT_CABAL_PROJECT_FILENAME);
    }

    @TaskAction
    public void fetchDependencies() {

        /* Create the destination directory if it doesn't exist. */

        File workingDir = destinationDir.getAsFile().get();

        if (!workingDir.exists() && !workingDir.mkdirs()) {
            throw new GradleException("Unable to create destination directory: "
                                      + workingDir.getAbsolutePath());
        }

        /* Fork an etlas process to fetch the dependencies. */

        EtlasCommand etlas = new EtlasCommand(getProject());
        etlas.getWorkingDirectory().set(workingDir);
        etlas.newDeps((fileDeps, mavenDeps) -> {
                /* Inject the dependencies into the target configuration. */
                String configurationName = getTargetConfiguration().get();
                DependencyHandler dependencies = getProject().getDependencies();
                dependencies.add(configurationName, fileDeps);
                for (String mavenDep : mavenDeps) {
                    dependencies.add(configurationName, mavenDep);
                }
            });
    }
}
