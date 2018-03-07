package com.typelead.gradle.eta.tasks;

import java.io.File;
import java.util.Set;
import java.util.LinkedHashSet;
import java.util.stream.Stream;
import java.util.stream.Collectors;

import javax.inject.Inject;

import org.gradle.api.Project;
import org.gradle.api.file.RegularFile;
import org.gradle.api.file.DirectoryProperty;
import org.gradle.api.file.ProjectLayout;
import org.gradle.api.tasks.Input;
import org.gradle.api.tasks.OutputFile;
import org.gradle.api.tasks.TaskAction;
import org.gradle.api.tasks.TaskAction;
import org.gradle.api.provider.Provider;
import org.gradle.api.provider.ProviderFactory;
import org.gradle.api.artifacts.Configuration;

import com.typelead.gradle.utils.EtlasCommand;
import com.typelead.gradle.utils.ExtensionHelper;
import com.typelead.gradle.eta.api.EtaDependency;
import com.typelead.gradle.eta.api.EtaDirectDependency;
import com.typelead.gradle.eta.api.EtaGitDependency;
import com.typelead.gradle.eta.api.EtaConfiguration;
import com.typelead.gradle.eta.api.SourceRepository;
import com.typelead.gradle.eta.api.EtaExtension;
import com.typelead.gradle.eta.plugins.EtaBasePlugin;

public class EtaResolveDependencies extends AbstractEtlasTask {

    public static final String DEFAULT_FREEZE_CONFIG_FILENAME = "cabal.project.freeze";
    public static final String DEFAULT_DESTINATION_DIR = "eta-freeze";

    private DirectoryProperty destinationDir;
    private Provider<Set<EtaDependency>> dependencies;

    public EtaResolveDependencies() {
        this.dependencies   = getDefaultDependencyProvider(getProject());
        this.destinationDir = getProject().getLayout().directoryProperty();

        destinationDir.set
            (getProject().getLayout().getBuildDirectory().dir(DEFAULT_DESTINATION_DIR));

        setDescription("Resolve dependencies all the projects in a multi-project build"
                     + " to get a consistent snapshot.");
    }

    public Provider<Set<EtaDependency>>
        getDefaultDependencyProvider(final Project project) {
        return project.provider(() -> {
                Set<EtaDependency> allDependencies = new LinkedHashSet<>();
                for (Project p: project.getAllprojects()) {
                    for (Configuration c: p.getConfigurations()) {
                        final EtaConfiguration etaConfiguration =
                            ExtensionHelper.getExtension(c, EtaConfiguration.class);
                        allDependencies.addAll(etaConfiguration.getAllDependencies());
                    }
                }
                return allDependencies;
            });
    }

    @Input
    public Provider<Set<EtaDependency>> getInputDependencies() {
        return dependencies;
    }

    public void setInputDependencies
        (Provider<Set<EtaDependency>> inputDependencies) {
        dependencies = inputDependencies;
    }

    @Input
    public Provider<File> getDestinationDirectory() {
        return destinationDir.getAsFile();
    }

    public void setDestinationDirectory(Object dir) {
        destinationDir.set(getProject().file(dir));
    }

    @OutputFile
    public Provider<RegularFile> getDependencyConfigurationFile() {
        return destinationDir.file(DEFAULT_FREEZE_CONFIG_FILENAME);
    }

    @TaskAction
    public void resolveDependencies() {
        Stream.Builder<EtaGitDependency>    gitDependenciesBuilder
            = Stream.builder();
        Stream.Builder<EtaDirectDependency> directDependenciesBuilder
            = Stream.builder();

        for (EtaDependency dependency : dependencies.get()) {
            if (dependency instanceof EtaGitDependency) {
                gitDependenciesBuilder.add((EtaGitDependency) dependency);
            } else if (dependency instanceof EtaDirectDependency) {
                directDependenciesBuilder.add((EtaDirectDependency) dependency);
            }
        }

        Set<String> dependencyConstraints =
            directDependenciesBuilder.build()
            .map(Object::toString)
            .collect(Collectors.toSet());

        Set<SourceRepository> sourceRepositories =
            gitDependenciesBuilder.build()
            .map(EtaGitDependency::getSourceRepository)
            .collect(Collectors.toSet());

        /* Create the destination directory if it doesn't exist. */

        File workingDir = destinationDir.getAsFile().get();

        if (!workingDir.exists()) {
            workingDir.mkdirs();
        }

        /* Remove the cabal.project.freeze file from a previous run, if it exists. */

        File existingFreezeFile =
            getDependencyConfigurationFile().get().getAsFile();

        if (existingFreezeFile.exists()) {
            existingFreezeFile.delete();
        }

        etlas.getWorkingDirectory().set(workingDir);
        etlas.newFreeze(dependencyConstraints, sourceRepositories);
    }
}
