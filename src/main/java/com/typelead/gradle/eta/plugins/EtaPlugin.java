package com.typelead.gradle.eta.plugins;

import java.io.File;
import java.util.Map;
import java.util.HashMap;

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
import org.gradle.api.tasks.compile.AbstractCompile;
import org.gradle.api.plugins.BasePlugin;
import org.gradle.api.plugins.ApplicationPlugin;
import org.gradle.api.plugins.ApplicationPluginConvention;
import org.gradle.api.plugins.JavaPlugin;
import org.gradle.api.plugins.JavaPluginConvention;
import org.gradle.api.provider.Provider;
import org.gradle.api.artifacts.Configuration;
import org.gradle.api.internal.file.SourceDirectorySetFactory;
import org.gradle.api.internal.tasks.DefaultSourceSetOutput;

import com.typelead.gradle.utils.ExtensionHelper;
import com.typelead.gradle.eta.api.EtaConfiguration;
import com.typelead.gradle.eta.tasks.EtaResolveDependencies;
import com.typelead.gradle.eta.tasks.EtaInstallDependencies;
import com.typelead.gradle.eta.tasks.EtaCompile;
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
        project.getConvention().getPlugin(JavaPluginConvention.class)
               .getSourceSets().all(this::configureSourceSet);
    }

    private void configureSourceSet(SourceSet sourceSet) {

        final DefaultEtaSourceSet etaSourceSet =
            ExtensionHelper.createExtension
              (sourceSet, "eta", DefaultEtaSourceSet.class, sourceSet,
               sourceDirectorySetFactory);

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

        final Provider<String> sourceConfiguration
            = project.provider(() -> sourceSet.getCompileClasspathConfigurationName());

        final Provider<String> targetConfiguration
            = project.provider(() -> sourceSet.getCompileConfigurationName());

        final Provider<Directory> destinationDir
            = project.getLayout().getBuildDirectory()
            .dir(etaSourceSet.getRelativeOutputDir());

        /* Create the install dependencies task. */

        final EtaInstallDependencies installDependenciesTask =
            project.getTasks().create(etaSourceSet.getInstallDependenciesTaskName(),
                                      EtaInstallDependencies.class);

        installDependenciesTask.setSourceConfiguration(sourceConfiguration);
        installDependenciesTask.setTargetConfiguration(targetConfiguration);
        installDependenciesTask.setFreezeConfigFile(freezeConfigFile);
        installDependenciesTask.setDestinationDir(destinationDir);
        installDependenciesTask.setSource(etaSourceDirectorySet);
        installDependenciesTask.dependsOn(resolveDependenciesTask);
        installDependenciesTask.setDescription("Installs dependencies for the " + sourceSet.getName() + " Eta source.");
        installDependenciesTask.dependsOnOtherEtaProjects();

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
                                        .relativize(sourceSet.getOutput()
                                                    .getClassesDir().toPath())
                                        .toString()).get();
                }
                return buildDir.dir(etaSourceSet.getClassesDir()).get();
            });

        compileTask.setClasspath(project.provider
                                 (() -> sourceSet.getCompileClasspath()));
        compileTask.setCabalProjectFile(installDependenciesTask.getCabalProjectFile());
        compileTask.setCabalFile(installDependenciesTask.getCabalFile());
        compileTask.setDestinationDir(destinationDir);
        compileTask.addExtraClasspath(javaCompileTask.getDestinationDir());
        compileTask.setClassesDir(classesDir);
        compileTask.setSource(etaSourceDirectorySet);
        compileTask.dependsOn(javaCompileTask);
        compileTask.setDescription("Compiles the " + sourceSet.getName() + " Eta source.");

        /* Register the Eta classes directory as an output so that the Jar task
           will pick it up nicely. */

        Map<String, Object> builtByOptions = new HashMap<String, Object>();
        builtByOptions.put("builtBy", compileTask);

        etaSourceDirectorySet.setOutputDir
            (project.provider(() -> classesDir.get().getAsFile()));

        /* TODO: Are both classesDir and the output registration below required? */
        ((DefaultSourceSetOutput) sourceSet.getOutput()).addClassesDir
            (() -> etaSourceDirectorySet.getOutputDir());

        sourceSet.getOutput().dir(builtByOptions, classesDir);

        /* Register the package databases as artifacts that will be collected
           upon dependency resolution of project dependencies. */

        addArtifacts(compileTask.getPackageDB(),
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
}
