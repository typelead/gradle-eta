package com.typelead.gradle.eta.plugins;

import java.io.File;
import java.util.HashSet;
import java.util.Set;

import javax.inject.Inject;

import org.gradle.api.Plugin;
import org.gradle.api.Project;
import org.gradle.api.Task;
import org.gradle.api.artifacts.Configuration;
import org.gradle.api.file.Directory;
import org.gradle.api.file.FileCollection;
import org.gradle.api.file.SourceDirectorySet;
import org.gradle.api.internal.file.SourceDirectorySetFactory;
import org.gradle.api.provider.Provider;
import org.gradle.api.tasks.compile.JavaCompile;

import com.android.build.gradle.BasePlugin;
import com.android.build.gradle.BaseExtension;
import com.android.build.gradle.api.BaseVariant;
import com.android.build.gradle.internal.api.DefaultAndroidSourceSet;
import com.android.builder.model.SourceProvider;

import com.typelead.gradle.eta.android.AndroidHelper;
import com.typelead.gradle.eta.api.EtaExtension;
import com.typelead.gradle.eta.api.EtaSourceSet;
import com.typelead.gradle.eta.api.EtaOptions;
import com.typelead.gradle.eta.api.LanguageExtension;
import com.typelead.gradle.eta.api.NamingScheme;
import com.typelead.gradle.eta.tasks.EtaInstallDependencies;
import com.typelead.gradle.eta.tasks.EtaCompile;
import com.typelead.gradle.eta.tasks.EtaResolveDependencies;
import com.typelead.gradle.eta.internal.ConfigurationUtils;
import com.typelead.gradle.eta.internal.DefaultEtaSourceSet;
import com.typelead.gradle.utils.ExtensionHelper;

/**
 * A {@link Plugin} which sets up an Eta project.
 */
public class EtaAndroidPlugin implements Plugin<Project> {

    public static final String ETA_SOURCE_SET_NAME              = "eta";
    public static final String ETA_SOURCE_SET_DSL_NAME          = "eta";
    public static final String ETA_OPTIONS_DSL_NAME             = "etaOptions";

    private Project project;
    private final SourceDirectorySetFactory sourceDirectorySetFactory;
    private BasePlugin androidPlugin;
    private BaseExtension androidExtension;

    @Inject
    public EtaAndroidPlugin(SourceDirectorySetFactory sourceDirectorySetFactory) {
        this.sourceDirectorySetFactory = sourceDirectorySetFactory;
    }

    @Override
    public void apply(Project project) {
        this.project = project;

        project.getPlugins().apply(EtaBasePlugin.class);

        project.getPlugins().all
            (plugin -> {
                if (BasePlugin.class.isInstance(plugin)) {
                    /* TODO: Implement locking to deal with multiple initialization. */
                    configureAndroidProject((BasePlugin)plugin);
                }});
    }

    private void configureAndroidProject(BasePlugin androidPlugin) {
        this.androidPlugin = androidPlugin;
        this.androidExtension = AndroidHelper.getAndroidExtension(project);
        configureEtaSourceSetConvention();
        addEtaOptionsToDefaultConfig();
        configureBaseVariants();
    }

    private void configureEtaSourceSetConvention() {
        androidExtension.getSourceSets().all(sourceSet -> {
                project.getLogger().debug("Creating EtaSourceSet for source set " + sourceSet);

                EtaSourceSet etaSourceSet =
                    ExtensionHelper.createExtension(sourceSet,
                        ETA_SOURCE_SET_DSL_NAME, DefaultEtaSourceSet.class,
                        ETA_SOURCE_SET_NAME,
                        ((DefaultAndroidSourceSet) sourceSet).getDisplayName(),
                        sourceDirectorySetFactory);

                etaSourceSet.getEta().srcDir("src/" + sourceSet.getName() + "/eta");
            });
    }

    private void addEtaOptionsToDefaultConfig() {
        ExtensionHelper.createExtension
            (androidExtension, ETA_OPTIONS_DSL_NAME, EtaOptions.class)
            .setExtensions(project.container(LanguageExtension.class));
    }

    private void configureBaseVariants() {
        AndroidHelper.forEachVariant(androidExtension, this::configureBaseVariant);
    }

    private void configureBaseVariant(BaseVariant variant) {

        final EtaOptions etaOptions = createEtaOptions();

        final String variantName = variant.getName();

        project.getLogger()
            .debug("Processing variant " + variantName + " for Eta compilation.");

        final JavaCompile javaCompileTask = variant.getJavaCompile();

        if (javaCompileTask == null) {
            project.getLogger().info
                ("EtaAndroidPlugin: javaCompileTask is missing for "
                 + variantName + " so the Eta compilation tasks will be skipped.");
            return;
        }

        final SourceDirectorySet etaSourceDirectorySet =
            sourceDirectorySetFactory.create("eta", variantName + " Eta source");

        for (SourceProvider sourceProvider : variant.getSourceSets()) {
            final EtaSourceSet etaSourceSet =
                ExtensionHelper.getExtension(sourceProvider, EtaSourceSet.class);
            if (etaSourceSet != null) {
                etaSourceDirectorySet.source(etaSourceSet.getEta());
            }
        }

        final EtaResolveDependencies resolveDependenciesTask
            = (EtaResolveDependencies) project.getRootProject().getTasks()
            .getByPath(EtaBasePlugin.ETA_RESOLVE_DEPENDENCIES_TASK_NAME);

        final FileCollection freezeConfigFile =
            resolveDependenciesTask.getOutputs().getFiles();

        final Provider<String> targetConfiguration
            = project.provider(() -> variant.getCompileConfiguration().getName());

        final Provider<Directory> destinationDir
            = project.getLayout().getBuildDirectory()
            .dir(NamingScheme.getRelativeOutputDir(variant.getDirName()));

        /* Create the install dependencies task. */

        final EtaInstallDependencies installDependenciesTask =
            project.getTasks().create
            (NamingScheme.getInstallDependenciesTaskName(variantName),
             EtaInstallDependencies.class);

        installDependenciesTask.setTargetConfiguration(targetConfiguration);
        installDependenciesTask.setFreezeConfigFile(freezeConfigFile);
        installDependenciesTask.setFreezeConfigChanged
            (project.provider(() -> resolveDependenciesTask.getDidWork()));
        installDependenciesTask.setDestinationDir(destinationDir);
        installDependenciesTask.setOptions(etaOptions);
        installDependenciesTask.setSource(etaSourceDirectorySet);
        installDependenciesTask.dependsOn(resolveDependenciesTask);
        installDependenciesTask.setDescription
            ("Installs dependencies for the " + variantName + " Eta source.");
        installDependenciesTask.dependsOnOtherEtaProjects();

        /* Because the installDependenciesTask injects dependencies into the
           configuration, it must run *before* the preBuild phase since every task
           after that will resolve configurations. */

        variant.getPreBuild().dependsOn(installDependenciesTask);

        /* Create the compile task. */

        EtaCompile compileTask =
            project.getTasks().create(NamingScheme.getCompileTaskName(variantName),
                                      EtaCompile.class);

        compileTask.setCabalProjectFile(installDependenciesTask.getCabalProjectFileProvider());
        compileTask.setCabalFile(installDependenciesTask.getCabalFileProvider());
        compileTask.setDestinationDir(destinationDir);
        compileTask.setOptions(etaOptions);
        compileTask.setSource(etaSourceDirectorySet);
        compileTask.dependsOn(installDependenciesTask);
        compileTask.setDescription("Compiles the " + variantName + " Eta source.");

        /* Register the Eta output jar file with the Android build system. */

        Object etaClasspathKey =
            variant.registerPreJavacGeneratedBytecode
            (project.files(compileTask.getOutputJarFile()).builtBy(compileTask));

        /* Setup the classpath for Eta */

        compileTask.setClasspath
            (project.provider
             (() -> variant.getCompileClasspath(etaClasspathKey)
              .plus(project.files(AndroidHelper
                                  .getAndroidSDKClasspath(androidExtension)))));

        /* Register the package databases as artifacts that will be collected
           upon dependency resolution of project dependencies. */

        addArtifacts(compileTask.getPackageDBProvider(),
                     variant.getRuntimeConfiguration());
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

    private void addArtifacts(Provider<File> artifact, Configuration... configurations) {
        for (Configuration configuration : configurations) {
            if (configuration != null) {
                ConfigurationUtils.getEtaConfiguration(configuration).getArtifacts()
                    .add(artifact);
            }
        }
    }

    private EtaOptions createEtaOptions() {
        return project.getObjects().newInstance(EtaOptions.class)
            .setExtensions(project.container(LanguageExtension.class));
    }
}
