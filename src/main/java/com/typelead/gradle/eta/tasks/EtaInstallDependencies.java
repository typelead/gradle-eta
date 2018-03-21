package com.typelead.gradle.eta.tasks;

import java.io.File;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.List;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.stream.Collectors;

import org.gradle.api.Buildable;
import org.gradle.api.DefaultTask;
import org.gradle.api.GradleException;
import org.gradle.api.Project;
import org.gradle.api.file.Directory;
import org.gradle.api.file.DirectoryProperty;
import org.gradle.api.file.FileCollection;
import org.gradle.api.file.RegularFile;
import org.gradle.api.file.SourceDirectorySet;
import org.gradle.api.tasks.Input;
import org.gradle.api.tasks.InputFiles;
import org.gradle.api.tasks.OutputFile;
import org.gradle.api.tasks.TaskAction;
import org.gradle.api.tasks.SourceTask;
import org.gradle.api.provider.Provider;
import org.gradle.api.provider.Property;
import org.gradle.api.artifacts.Configuration;
import org.gradle.api.artifacts.dsl.DependencyHandler;

import com.typelead.gradle.utils.EtlasCommand;
import com.typelead.gradle.utils.CabalHelper;
import com.typelead.gradle.utils.PackageInfo;
import com.typelead.gradle.eta.api.EtaConfiguration;
import com.typelead.gradle.eta.api.EtaDependency;
import com.typelead.gradle.eta.api.EtaProjectDependency;
import com.typelead.gradle.eta.internal.DependencyUtils;
import com.typelead.gradle.eta.internal.ConfigurationUtils;

public class EtaInstallDependencies extends DefaultTask {

    public static final String DEFAULT_CABAL_FILENAME = ".cabal";
    public static final String DEFAULT_CABAL_PROJECT_FILENAME = "cabal.project";
    public static final String DEFAULT_DESTINATION_DIR = "eta";

    private final Project project;
    private Provider<String> projectName;
    private Provider<String> projectVersion;
    private FileCollection freezeConfigFile;
    private DirectoryProperty destinationDir;
    private SourceDirectorySet sourceDirectories;
    private Provider<List<String>> modulesProvider;
    private Provider<String> targetConfiguration;
    private Provider<Set<EtaDependency>> dependencies;
    private Provider<RegularFile> cabalProjectFile;
    private Provider<RegularFile> cabalFile;
    private Property<String> executable;

    public EtaInstallDependencies() {
        this.project = getProject();
        this.projectName = project.provider(() -> project.getName());
        this.projectVersion =
            project.provider(() -> project.getVersion().toString());
        this.freezeConfigFile = project.files();
        this.destinationDir =
            project.getLayout().directoryProperty();

        destinationDir.set(project.getLayout().getBuildDirectory()
                           .dir(DEFAULT_DESTINATION_DIR));

        this.modulesProvider = defaultModulesProvider();
        this.dependencies =
            project.provider
            (() -> ConfigurationUtils.getEtaConfiguration
             (project, EtaInstallDependencies.this.getTargetConfiguration())
             .getAllDependencies());

        this.cabalProjectFile = destinationDir.file(DEFAULT_CABAL_PROJECT_FILENAME);
        this.cabalFile = destinationDir
            .file(project.provider(() -> getProjectName() + DEFAULT_CABAL_FILENAME));
        this.executable = project.getObjects().property(String.class);

        setDescription("Install dependencies for the Eta project.");
    }

    @Input
    public String getProjectName() {
        return projectName.get();
    }

    @Input
    public String getProjectVersion() {
        return projectVersion.get();
    }

    @Input
    public String getTargetConfiguration() {
        return targetConfiguration.get();
    }

    public void setTargetConfiguration(Provider<String> targetConfiguration) {
        this.targetConfiguration = targetConfiguration;
    }

    @InputFiles
    public FileCollection getFreezeConfigFile() {
        return freezeConfigFile;
    }

    public void setFreezeConfigFile(FileCollection freezeConfigFile) {
        this.freezeConfigFile = freezeConfigFile;
    }

    @Input
    public File getDestinationDir() {
        return destinationDir.getAsFile().get();
    }

    public void setDestinationDir(Provider<Directory> destinationDir) {
        this.destinationDir.set(destinationDir);
    }

    @Input
    public FileCollection getSourceDirs() {
        return sourceDirectories.getSourceDirectories();
    }

    public FileCollection getSource() {
        return sourceDirectories;
    }

    public void setSource(SourceDirectorySet sourceDirectories) {
        this.sourceDirectories = sourceDirectories;
    }

    @Input
    public List<String> getModules() {
        return modulesProvider.get();
    }

    public Provider<List<String>> defaultModulesProvider() {
        return project.provider
            (() -> {
                final List<String> modules = new ArrayList<>();
                sourceDirectories.getAsFileTree()
                    .visit(file -> {
                            if (!file.isDirectory()) {
                                String moduleWithExtension =
                                    file.getPath().replace('/', '.');
                                String module = moduleWithExtension
                                    .substring(0, moduleWithExtension.lastIndexOf("."));
                                if (module.equals("Main")) {
                                    /* TODO: Handle case where there are two Main files. */
                                    executable.set(moduleWithExtension);
                                } else {
                                    modules.add(module);
                                }
                            }});
                return modules;
            });
    }

    @Input
    public Set<String> getDependencies() {
        return dependencies.get().stream()
            .map(Object::toString).collect(Collectors.toSet());
    }

    public Provider<RegularFile> getCabalProjectFile() {
        return cabalProjectFile;
    }

    public Provider<RegularFile> getCabalFile() {
        return cabalFile;
    }

    public void dependsOnOtherEtaProjects() {
        dependsOn(new Callable<List<Buildable>>() {
                @Override
                public List<Buildable> call() {
                    List<Buildable> buildables = new ArrayList<Buildable>();
                    String configurationName = getTargetConfiguration();
                    Set<EtaDependency> dependencies =
                        ConfigurationUtils.getEtaConfiguration
                        (project, getTargetConfiguration())
                        .getAllDependencies();
                    for (EtaDependency dep : dependencies) {
                        if (dep instanceof EtaProjectDependency) {
                            final EtaProjectDependency projectDep =
                                (EtaProjectDependency) dep;
                            buildables.add
                                (projectDep.getProject(project).getConfigurations()
                                 .findByName(projectDep.getTargetConfiguration())
                                 .getAllArtifacts());
                        }
                    }
                    return buildables;
                }
            });
    }

    @TaskAction
    public void installDependencies() {

        /* TODO: Handle the case where the .cabal file name can change if
                 the project name changes and the old cabal file must be deleted -
                 otherwise Etlas will yell! Or send the new cabal file as a
                 target.
        */

        /* Create the destination directory if it doesn't exist. */

        final File workingDir = getDestinationDir();

        if (!workingDir.exists() && !workingDir.mkdirs()) {
            throw new GradleException("Unable to create destination directory: "
                                      + workingDir.getAbsolutePath());
        }

        /* Ensure the freezeConfig FileCollection contains exactly one file.
           TODO: Find a better way to enforce this invariant? */

        File tmpFreezeConfig = null;

        try {
            tmpFreezeConfig = freezeConfigFile.getSingleFile();
        } catch (IllegalStateException e) {
            throw new GradleException("The freezeConfig file collection contains more than one element!", e);
        }

        final File freezeConfig = tmpFreezeConfig;

        /* Copy the project-global freeze file into the working directory. */

        project.copy(copySpec -> {
                copySpec.from(freezeConfig);
                copySpec.into(workingDir);
            });

        /* Calculate all the modules */

        final List<String> modules = getModules();

        /* Determine if it's an executable */

        String exec = executable.getOrNull();
        if (exec != null && exec.length() <= 0) {
            exec = null;
        }

        final String executableSpec = exec;

        /* Generate the .cabal & cabal.project files. */

        final String targetConfigurationName = getTargetConfiguration();

        Set<File> packageDBs = ConfigurationUtils
            .getEtaConfiguration(project.getConfigurations()
                                 .getByName(targetConfigurationName))
            .getAllArtifacts(project).stream()
            .map(Provider::get)
            .collect(Collectors.toSet());

        DependencyUtils.foldEtaDependencies
            (project,
             dependencies.get(),
             (directDeps, projectDeps) -> {

                /* Include the project dependencies in the Etlas
                   dependency list. */

                directDeps.addAll(projectDeps);

                CabalHelper.generateCabalFile
                    (project.getName(),
                     project.getVersion().toString(),
                     executableSpec,
                     getSourceDirs().getFiles().stream()
                     .map(File::getAbsolutePath)
                     .collect(Collectors.toList()),
                     modules,
                     directDeps,
                     workingDir);

            }, gitDeps -> CabalHelper.generateCabalProjectFile(gitDeps,
                                                               packageDBs,
                                                               workingDir));

        /* Fork an etlas process to install the dependencies.  */

        final EtlasCommand etlas = new EtlasCommand(project);

        etlas.getWorkingDirectory().set(workingDir);

        boolean isUpToDate = etlas.deps (dependencyGraph -> {

                /* Inject the dependencies into the respective configurations. */

                DependencyHandler dependencies = project.getDependencies();
                final EtaConfiguration targetEtaConfiguration =
                ConfigurationUtils.getEtaConfiguration(project,
                                                       getTargetConfiguration());

                targetEtaConfiguration.resolve(project, dependencies, dependencyGraph);
            });

        setDidWork(!isUpToDate);
    }
}
