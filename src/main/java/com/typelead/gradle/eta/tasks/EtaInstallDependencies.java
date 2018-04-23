package com.typelead.gradle.eta.tasks;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Map;
import java.util.LinkedHashMap;
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
import org.gradle.api.tasks.Internal;
import org.gradle.api.tasks.InputFiles;
import org.gradle.api.tasks.Nested;
import org.gradle.api.tasks.OutputFile;
import org.gradle.api.tasks.OutputDirectory;
import org.gradle.api.tasks.TaskAction;
import org.gradle.api.tasks.SourceTask;
import org.gradle.api.provider.Provider;
import org.gradle.api.provider.Property;
import org.gradle.api.artifacts.Configuration;
import org.gradle.api.artifacts.dsl.DependencyHandler;

import com.typelead.gradle.utils.EtlasCommand;
import com.typelead.gradle.utils.EtaInfo;
import com.typelead.gradle.utils.CabalHelper;
import static com.typelead.gradle.utils.CabalHelper.WriteResult;
import com.typelead.gradle.utils.PackageInfo;
import com.typelead.gradle.eta.api.EtaConfiguration;
import com.typelead.gradle.eta.api.EtaDependency;
import com.typelead.gradle.eta.api.EtaExtension;
import com.typelead.gradle.eta.api.EtaOptions;
import com.typelead.gradle.eta.api.EtaProjectDependency;
import com.typelead.gradle.eta.internal.DependencyUtils;
import com.typelead.gradle.eta.internal.ConfigurationUtils;

public class EtaInstallDependencies extends DefaultTask {

    public static final String DEFAULT_CABAL_FILENAME = ".cabal";
    public static final String DEFAULT_CABAL_PROJECT_FILENAME = "cabal.project";
    public static final String DEFAULT_DESTINATION_DIR = "eta";

    private final Project project;
    private final Provider<EtaInfo> etaInfo;
    private EtaOptions etaOptions;
    private Provider<String> projectName;
    private Provider<String> projectVersion;
    private FileCollection freezeConfigFile;
    private Provider<Boolean> freezeConfigChanged;
    private DirectoryProperty destinationDir;
    private SourceDirectorySet sourceDirectories;
    private Provider<List<String>> modulesProvider;
    private Provider<String> targetConfiguration;
    private Provider<Set<EtaDependency>> dependencies;
    private Provider<RegularFile> cabalProjectFile;
    private Provider<RegularFile> cabalFile;
    private Property<String> executable;
    private Map<String,Object> extraPackageDBs = new LinkedHashMap<String, Object>();
    private Provider<String> packageName;

    public EtaInstallDependencies() {
        this.project = getProject();
        this.projectVersion =
            project.provider(() -> project.getVersion().toString());
        this.freezeConfigFile = project.files();
        this.destinationDir =
            project.getLayout().directoryProperty();

        final EtaExtension extension =
            project.getRootProject().getExtensions().getByType(EtaExtension.class);
        this.etaInfo = extension.getEtaInfo();

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
            .file(project.provider(() -> getPackageName() + DEFAULT_CABAL_FILENAME));
        this.executable = project.getObjects().property(String.class);

        getOutputs().upToDateWhen(task -> false);
    }

    @Input
    public String getPackageName() {
        return packageName.get();
    }

    public void setPackageName(Provider<String> packageName) {
        this.packageName = packageName;
    }

    @Input
    public String getProjectVersion() {
        return projectVersion.get();
    }

    @Nested
    public EtaOptions getOptions() {
        return etaOptions;
    }

    public void setOptions(EtaOptions etaOptions) {
        this.etaOptions = etaOptions;
    }

    @Input
    public String getTargetConfiguration() {
        return targetConfiguration.get();
    }

    public void setTargetConfiguration(Provider<String> targetConfiguration) {
        this.targetConfiguration = targetConfiguration;
    }

    public Map<String,Object> getExtraPackageDBs() {
        return extraPackageDBs;
    }

    public void addExtraPackageDB(String packageName, Object file) {
        extraPackageDBs.put(packageName, file);
    }

    @InputFiles
    public FileCollection getFreezeConfigFile() {
        return freezeConfigFile;
    }

    public void setFreezeConfigFile(FileCollection freezeConfigFile) {
        this.freezeConfigFile = freezeConfigFile;
    }

    @Input
    public boolean isFreezeConfigChanged() {
        return freezeConfigChanged.get();
    }

    public void setFreezeConfigChanged(Provider<Boolean> freezeConfigChanged) {
        this.freezeConfigChanged = freezeConfigChanged;
    }

    @InputFiles
    public FileCollection getSourceDirs() {
        if (sourceDirectories != null) {
            return sourceDirectories.getSourceDirectories();
        } else {
            return project.files();
        }
    }

    @InputFiles
    public FileCollection getSource() {
        if (sourceDirectories != null) {
            return sourceDirectories;
        } else {
            return project.files();
        }
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
                if (sourceDirectories != null) {
                    sourceDirectories.getAsFileTree().visit
                        (file -> {
                            if (!file.isDirectory()) {
                                String moduleWithExtension =
                                    file.getPath().replace('/', '.');
                                String module = moduleWithExtension
                                    .substring(0, moduleWithExtension.lastIndexOf("."));
                                if (module.equals("Main")) {
                                    executable.set(moduleWithExtension);
                                } else {
                                    modules.add(module);
                                }
                            }});
                }
                return modules;
            });
    }

    @Input
    public Set<String> getDependencies() {
        return dependencies.get().stream()
            .map(Object::toString).collect(Collectors.toSet());
    }

    @OutputFile
    public File getCabalProjectFile() {
        return cabalProjectFile.get().getAsFile();
    }

    @Internal
    public Provider<RegularFile> getCabalProjectFileProvider() {
        return cabalProjectFile;
    }

    @OutputFile
    public File getCabalFile() {
        return cabalFile.get().getAsFile();
    }

    @Internal
    public Provider<RegularFile> getCabalFileProvider() {
        return cabalFile;
    }

    @OutputDirectory
    public File getDestinationDir() {
        return destinationDir.getAsFile().get();
    }

    public void setDestinationDir(Provider<Directory> destinationDir) {
        this.destinationDir.set(destinationDir);
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
                                (ConfigurationUtils.getConfiguration
                                 (projectDep.getProject(project),
                                  projectDep.getTargetConfiguration())
                                 .getAllArtifacts());
                        }
                    }
                    return buildables;
                }
            });
    }

    @TaskAction
    public void installDependencies() {

        final EtaOptions etaOptions = getOptions();

        etaOptions.validate(etaInfo.get());

        final File workingDir = getDestinationDir();

        copyFreezeConfigIfChanged(workingDir);

        /* Calculate all the modules */

        final List<String> modules = getModules();

        /* Determine if it's an executable */

        String exec = executable.getOrNull();
        if (exec != null && exec.length() <= 0) {
            exec = null;
        }

        final String executableSpec = exec;

        /* Generate the .cabal & cabal.project files. */

        final WriteResult[] writeResults = new WriteResult[2];

        final String targetConfigurationName = getTargetConfiguration();

        Set<File> packageDBs = ConfigurationUtils
            .getEtaConfiguration(ConfigurationUtils.getConfiguration
                                 (project, targetConfigurationName))
            .getAllArtifacts(project).stream()
            .map(Provider::get)
            .collect(Collectors.toSet());

        packageDBs.addAll(project.files(extraPackageDBs.values()).getFiles());

        DependencyUtils.foldEtaDependencies
            (project,
             dependencies.get(),
             (directDeps, projectDeps) -> {

                /* Include the project dependencies in the Etlas
                   dependency list. */

                directDeps.addAll(projectDeps);
                directDeps.addAll(extraPackageDBs.keySet());

                writeResults[0] = CabalHelper.generateCabalFile
                    (getPackageName(), project.getVersion().toString(), executableSpec,
                     getSourceDirs().getFiles().stream()
                     .map(File::getAbsolutePath)
                     .collect(Collectors.toList()),
                     modules, etaOptions, directDeps, workingDir);

            }, gitDeps -> {
                writeResults[1] =
                    CabalHelper.generateCabalProjectFile(gitDeps, packageDBs,
                                                         workingDir);
            });

        /* Delete existing *.cabal files to avoid errors when changing the project
           name. */

        final File oldCabalFile = writeResults[0].getFile();

        project.delete
            (project.fileTree
             (workingDir, fileTree -> {
                 fileTree.include("*.cabal");
                 fileTree.exclude
                     (fileTreeElement -> {
                         try {
                             return fileTreeElement.getFile().getCanonicalPath()
                                 .equals(oldCabalFile.getCanonicalPath());
                         } catch (IOException e) {
                             return true;
                         }
                     });}));

        /* Fork an etlas process to install the dependencies.  */

        final EtlasCommand etlas = new EtlasCommand(project);

        etlas.getWorkingDirectory().set(workingDir);

        boolean isUpToDate = etlas.deps (dependencyGraph -> {

                /* Inject the dependencies into the respective configurations. */

                DependencyHandler dependencies = project.getDependencies();
                final EtaConfiguration targetEtaConfiguration =
                  ConfigurationUtils.getEtaConfiguration
                    (project, getTargetConfiguration());

                targetEtaConfiguration.resolve(project, dependencies, dependencyGraph);
            });

        setDidWork(!isUpToDate);
    }

    private void copyFreezeConfigIfChanged(File workingDir) {

        if (isFreezeConfigChanged()) {
            /* Copy the project-global freeze file into the working directory. */

            File tmpFreezeConfig = null;

            try {
                tmpFreezeConfig = freezeConfigFile
                    .filter(file -> file.getName().endsWith(".freeze")).getSingleFile();
            } catch (IllegalStateException e) {
                throw new GradleException
                    ("The freezeConfig file collection contains more than one element!", e);
            }

            final File freezeConfig = tmpFreezeConfig;

            project.copy(copySpec -> {
                    copySpec.from(freezeConfig);
                    copySpec.into(workingDir);
                });
        }
    }
}
