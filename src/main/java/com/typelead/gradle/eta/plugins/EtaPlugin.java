package com.typelead.gradle.eta.plugins;

import java.io.File;
import java.util.Map;
import java.util.HashMap;
import java.util.Optional;

import javax.inject.Inject;

import org.gradle.api.Describable;
import org.gradle.api.Plugin;
import org.gradle.api.Project;

import org.gradle.api.file.Directory;
import org.gradle.api.file.DirectoryProperty;
import org.gradle.api.file.FileCollection;
import org.gradle.api.file.FileTreeElement;
import org.gradle.api.file.SourceDirectorySet;
import org.gradle.api.specs.Spec;
import org.gradle.api.tasks.SourceSet;
import org.gradle.api.tasks.SourceSetContainer;
import org.gradle.api.tasks.compile.AbstractCompile;
import org.gradle.api.plugins.BasePlugin;
import org.gradle.api.plugins.ApplicationPlugin;
import org.gradle.api.plugins.ApplicationPluginConvention;
import org.gradle.api.plugins.JavaPlugin;
import org.gradle.api.plugins.JavaPluginConvention;
import org.gradle.api.provider.Provider;
import org.gradle.api.artifacts.Configuration;
import org.gradle.api.internal.file.SourceDirectorySetFactory;
import org.gradle.api.internal.tasks.DefaultSourceSet;
import org.gradle.api.internal.tasks.DefaultSourceSetOutput;

import com.typelead.gradle.utils.ExtensionHelper;
import com.typelead.gradle.eta.api.EtaConfiguration;
import com.typelead.gradle.eta.api.EtaExtension;
import com.typelead.gradle.eta.api.EtaOptions;
import com.typelead.gradle.eta.api.LanguageExtension;
import com.typelead.gradle.eta.api.NamingScheme;
import com.typelead.gradle.eta.tasks.EtaResolveDependencies;
import com.typelead.gradle.eta.tasks.EtaInstallDependencies;
import com.typelead.gradle.eta.tasks.EtaCompile;
import com.typelead.gradle.eta.tasks.EtaRepl;
import com.typelead.gradle.eta.internal.ConfigurationUtils;
import com.typelead.gradle.eta.internal.DefaultEtaSourceSet;

/**
 * A {@link Plugin} which sets up an Eta project.
 */
@SuppressWarnings("WeakerAccess")
public class EtaPlugin implements Plugin<Project> {

    private Project project;
    private SourceDirectorySetFactory sourceDirectorySetFactory;

    @Inject
    public EtaPlugin(SourceDirectorySetFactory sourceDirectorySetFactory) {
        this.sourceDirectorySetFactory = sourceDirectorySetFactory;
    }


    @Override
    public void apply(Project project) {
        this.project = project;

        project.getPlugins().apply(EtaBasePlugin.class);
        project.getPlugins().apply(JavaPlugin.class);

        configureSourceSetDefaults();
        configureApplicationPluginIfPresent();
    }

    private void configureSourceSetDefaults() {
        JavaPluginConvention convention =
            project.getConvention().getPlugin(JavaPluginConvention.class);
        SourceSetContainer sourceSets = convention.getSourceSets();
        sourceSets.all(this::configureSourceSet);

        EtaCompile mainCompileTask = (EtaCompile) project.getTasks().findByName
            (NamingScheme.getCompileTaskName(SourceSet.MAIN_SOURCE_SET_NAME));
        EtaInstallDependencies testInstallDependenciesTask =
            (EtaInstallDependencies) project.getTasks().findByName
            (NamingScheme.getInstallDependenciesTaskName(SourceSet.TEST_SOURCE_SET_NAME));
        testInstallDependenciesTask.addExtraPackageDB
            (NamingScheme.getPackageName(project, SourceSet.MAIN_SOURCE_SET_NAME),
             mainCompileTask.getPackageDBProvider());
    }

    private void configureSourceSet(SourceSet sourceSet) {

        final EtaOptions etaOptions = createEtaOptions();

        final Provider<String> packageName =
            project.provider(() -> NamingScheme.getPackageName(project, sourceSet.getName()));
        final Provider<String> packageVersion =
            project.provider(() -> NamingScheme.fixVersion(project.getVersion().toString()));

        final DefaultEtaSourceSet etaSourceSet =
            project.getObjects().newInstance
            (DefaultEtaSourceSet.class, Optional.ofNullable(sourceSet), "eta",
             ((DefaultSourceSet) sourceSet).getDisplayName(),
             sourceDirectorySetFactory);

        ExtensionHelper.createConvention(sourceSet, "eta", etaSourceSet);

        final SourceDirectorySet etaSourceDirectorySet = etaSourceSet.getEta();

        etaSourceDirectorySet
            .srcDir("src/" + sourceSet.getName() + "/eta");

        /* Ensure that you exclude any Eta source files from the
           resources set. */

        sourceSet.getResources().getFilter()
            .exclude(element -> etaSourceSet.getEta().contains(element.getFile()));

        sourceSet.getAllSource().source(etaSourceDirectorySet);

        final EtaResolveDependencies resolveDependenciesTask
            = (EtaResolveDependencies) project.getRootProject().getTasks()
            .getByPath(EtaBasePlugin.ETA_RESOLVE_DEPENDENCIES_TASK_NAME);

        final FileCollection freezeConfigFile =
            resolveDependenciesTask.getOutputs().getFiles();

        final Provider<String> targetConfiguration
            = project.provider(() -> sourceSet.getCompileClasspathConfigurationName());

        final Provider<Directory> destinationDir
            = project.getLayout().getBuildDirectory()
            .dir(etaSourceSet.getRelativeOutputDir());

        /* Create the install dependencies task. */

        final EtaInstallDependencies installDependenciesTask =
            project.getTasks().create(etaSourceSet.getInstallDependenciesTaskName(),
                                      EtaInstallDependencies.class);

        installDependenciesTask.setPackageName(packageName);
        installDependenciesTask.setPackageVersion(packageVersion);
        installDependenciesTask.setTargetConfiguration(targetConfiguration);
        installDependenciesTask.setFreezeConfigFile(freezeConfigFile);
        installDependenciesTask.setFreezeConfigChanged
            (project.provider(() -> resolveDependenciesTask.getDidWork()));
        installDependenciesTask.setDestinationDir(destinationDir);
        installDependenciesTask.setOptions(etaOptions);
        installDependenciesTask.setSource(etaSourceDirectorySet);
        installDependenciesTask.dependsOn(resolveDependenciesTask);
        installDependenciesTask.setDescription("Installs dependencies for the " + sourceSet.getName() + " Eta source.");
        installDependenciesTask.dependsOnOtherEtaProjects();

        final EtaRepl replTask =
            project.getTasks().create(etaSourceSet.getReplTaskName(), EtaRepl.class);
        replTask.setPackageName(packageName);
        replTask.setDestinationDir(destinationDir);
        replTask.dependsOn(installDependenciesTask);

        /* The install dependencies tasks injects into this configuration so we must
           ensure that it runs before the Java compilation. */

        final AbstractCompile javaCompileTask = (AbstractCompile)
            project.getTasks().getByName(sourceSet.getCompileJavaTaskName());

        javaCompileTask.dependsOn(installDependenciesTask);

        /* Create the compile task. */

        EtaCompile compileTask =
            project.getTasks().create(etaSourceSet.getCompileTaskName(),
                                      EtaCompile.class);


        Provider<Directory> classesDir = project.provider
            (() -> {
                final DirectoryProperty buildDir =
                  project.getLayout().getBuildDirectory();
                if (sourceSet.getOutput().isLegacyLayout()) {
                    return buildDir.dir(buildDir.getAsFile().get().toPath()
                                        .relativize(sourceSet
                                                .getOutput()
                                                .getClassesDirs()
                                                .getSingleFile()
                                                .toPath())
                                        .toString()).get();
                }
                return buildDir.dir(etaSourceSet.getClassesDir()).get();
            });

        compileTask.setPackageName(packageName);
        compileTask.setPackageVersion(packageVersion);
        compileTask.setClasspath(project.provider
                                 (() -> sourceSet.getCompileClasspath()));
        compileTask.setCabalProjectFile(installDependenciesTask.getCabalProjectFileProvider());
        compileTask.setCabalFile(installDependenciesTask.getCabalFileProvider());
        compileTask.setDestinationDir(destinationDir);
        compileTask.addExtraClasspath(javaCompileTask.getDestinationDir());
        compileTask.setClassesDir(classesDir);
        compileTask.setOptions(etaOptions);
        compileTask.setSource(etaSourceDirectorySet);
        compileTask.dependsOn(javaCompileTask);
        compileTask.setDescription("Compiles the " + sourceSet.getName() + " Eta source.");

        /* NOTE: We have commented out the code below since it may be useful later.

           Map<String, Object> builtByOptions = new HashMap<String, Object>();
           builtByOptions.put("builtBy", compileTask);
           sourceSet.getOutput().dir(builtByOptions, classesDir);
        */

        /* Register the Eta classes directory as an output so that the Jar task
           will pick it up nicely. */

        etaSourceDirectorySet.setOutputDir
            (project.provider(() -> classesDir.get().getAsFile()));

        ((DefaultSourceSetOutput) sourceSet.getOutput()).addClassesDir
            (() -> etaSourceDirectorySet.getOutputDir());

        project.getTasks().findByName(sourceSet.getClassesTaskName())
            .dependsOn(compileTask);

        /* Register the package databases as artifacts that will be collected
           upon dependency resolution of project dependencies. */

        addArtifacts(compileTask.getPackageDBProvider(),
                     sourceSet.getApiElementsConfigurationName(),
                     sourceSet.getRuntimeConfigurationName(),
                     sourceSet.getRuntimeElementsConfigurationName());
    }

    private void addArtifacts(Provider<File> artifact, String... configurationNames) {
        for (String configurationName : configurationNames) {
            final Configuration configuration =
                project.getConfigurations().findByName(configurationName);
            if (configuration != null) {
                ConfigurationUtils.getEtaConfiguration(configuration).getArtifacts()
                    .add(artifact);
            }
        }
    }

    private void configureApplicationPluginIfPresent() {
        project.getPlugins().all
            (plugin -> {
                if (ApplicationPlugin.class.isInstance(plugin)) {
                    ApplicationPluginConvention pluginConvention =
                        project.getConvention().getPlugin
                        (ApplicationPluginConvention.class);
                    pluginConvention.setMainClassName("eta.main");
                }
            });
    }

    private EtaOptions createEtaOptions() {
        EtaOptions options = project.getObjects().newInstance(EtaOptions.class)
            .setExtensions(project.container(LanguageExtension.class));
        options.setProjectPath(project.getProjectDir().toPath());
        return options;
    }
}
