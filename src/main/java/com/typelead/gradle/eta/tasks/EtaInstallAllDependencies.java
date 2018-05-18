package com.typelead.gradle.eta.tasks;

import java.io.File;

import org.gradle.api.DefaultTask;
import org.gradle.api.Project;
import org.gradle.api.file.Directory;
import org.gradle.api.file.DirectoryProperty;
import org.gradle.api.file.RegularFile;
import org.gradle.api.tasks.InputFile;
import org.gradle.api.tasks.InputDirectory;
import org.gradle.api.tasks.TaskAction;
import org.gradle.api.provider.Provider;
import org.gradle.api.provider.Property;
import org.gradle.api.artifacts.Configuration;
import org.gradle.api.artifacts.dsl.DependencyHandler;

import com.typelead.gradle.utils.EtlasCommand;
import com.typelead.gradle.utils.ExtensionHelper;
import com.typelead.gradle.eta.api.EtaConfiguration;

public class EtaInstallAllDependencies extends DefaultTask {

    public static final String DEFAULT_CABAL_FILENAME = ".cabal";
    public static final String DEFAULT_DESTINATION_DIR = "eta-freeze";

    private final Project project;
    private Provider<File> freezeConfigFile;
    private DirectoryProperty destinationDir;
    private Provider<RegularFile> cabalFile;

    public EtaInstallAllDependencies() {
        this.project = getProject();
        this.destinationDir =
            project.getLayout().directoryProperty();

        destinationDir.set(project.getLayout().getBuildDirectory()
                           .dir(DEFAULT_DESTINATION_DIR));

        this.freezeConfigFile = project.provider
            (() -> destinationDir
             .file(EtaResolveDependencies.DEFAULT_FREEZE_CONFIG_FILENAME)
             .get().getAsFile());

        this.cabalFile = destinationDir
            .file(project.provider(() -> project.getName()
                                   + DEFAULT_CABAL_FILENAME));

        getOutputs().upToDateWhen(task -> false);
    }

    @InputFile
    public File getFreezeConfigFile() {
        return freezeConfigFile.get();
    }

    public void setFreezeConfigFile(Provider<File> freezeConfigFile) {
        this.freezeConfigFile = freezeConfigFile;
    }

    @InputDirectory
    public File getDestinationDir() {
        return destinationDir.getAsFile().get();
    }

    public void setDestinationDir(Provider<Directory> destinationDir) {
        this.destinationDir.set(destinationDir);
    }

    @InputFile
    public File getCabalFile() {
        return cabalFile.get().getAsFile();
    }

    @TaskAction
    public void installAllDependencies() {

        final File workingDir = getDestinationDir();

        /* Fork an etlas process to install the dependencies.  */

        final EtlasCommand etlas = new EtlasCommand(project);

        etlas.getWorkingDirectory().set(workingDir);

        boolean isUpToDate =
            etlas.deps (EtlasCommand.libTarget(getProject().getName()),
                        dependencyGraph -> {

                /* Inject the dependencies into the respective configurations. */

                for (Project p : project.getAllprojects()) {
                    DependencyHandler dependencies = p.getDependencies();
                    for (Configuration c: p.getConfigurations()) {
                        final EtaConfiguration etaConfiguration =
                          ExtensionHelper.getExtension(c, EtaConfiguration.class);
                        if (etaConfiguration != null) {
                            etaConfiguration.resolve
                                (project, dependencies, dependencyGraph);
                        }
                    }
                }
            });

        setDidWork(!isUpToDate);
    }
}
