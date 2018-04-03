package com.typelead.gradle.eta.plugins;

import java.io.File;
import java.util.Optional;
import java.util.Set;
import java.nio.file.Paths;

import org.gradle.api.DomainObjectCollection;
import org.gradle.api.GradleException;
import org.gradle.api.Plugin;
import org.gradle.api.Project;
import org.gradle.api.artifacts.Configuration;
import org.gradle.api.artifacts.ProjectDependency;
import org.gradle.api.logging.Logger;
import org.gradle.api.logging.Logging;
import org.gradle.api.plugins.BasePlugin;
import org.gradle.api.plugins.JavaPlugin;
import org.gradle.api.plugins.JavaPluginConvention;
import org.gradle.api.tasks.SourceSet;

import com.typelead.gradle.utils.EtlasCommand;
import com.typelead.gradle.utils.ExtensionHelper;
import com.typelead.gradle.eta.api.EtaDependency;
import com.typelead.gradle.eta.api.EtaExtension;
import com.typelead.gradle.eta.api.NamingScheme;
import com.typelead.gradle.eta.api.ProguardFiles;
import com.typelead.gradle.eta.tasks.EtaSetupEnvironment;
import com.typelead.gradle.eta.tasks.EtaResolveDependencies;
import com.typelead.gradle.eta.tasks.EtaInjectDependencies;
import com.typelead.gradle.eta.internal.DefaultEtaConfiguration;
import com.typelead.gradle.eta.internal.DefaultEtaProjectDependency;
import com.typelead.gradle.eta.internal.EtlasMavenRepository;

/**
 * A {@link Plugin} which compiles and tests Eta sources.
 */
public class EtaBasePlugin implements Plugin<Project> {

    /* Constants */
    public static final String ETA_EXTENSION_NAME = "eta";
    public static final String TASK_GROUP_NAME = "EtaPlugin";

    public static final String ETA_INTERMEDIATES_DIRECTORY = "eta-intermediates";

    public static final String DEFAULT_ETA_MAIN_CLASS = "eta.main";

    public static final String ETA_CONFIGURATION_EXTENSION_NAME  = "eta";

    /* Tasks */
    public static final String ETA_SETUP_ENVIRONMENT_TASK_NAME = "setupEnvironmentEta";
    public static final String
        ETA_RESOLVE_DEPENDENCIES_TASK_NAME = "resolveDependenciesEta";

    private Project project;
    private EtlasMavenRepository mavenRepository;

    @Override
    public void apply(Project project) {
        this.project   = project;
        this.mavenRepository = new EtlasMavenRepository
            (project, new File(project.getGradle().getGradleUserHomeDir()
                               + File.separator + "caches" + File.separator
                               + "etlas"));

        project.getPlugins().apply(BasePlugin.class);

        EtaPluginConvention etaConvention = new EtaPluginConvention(project);
        project.getConvention().getPlugins().put("eta", etaConvention);

        createRootEtaExtension();

        addEtlasMavenRepository();

        addEtaExtensionForConfigurations();

        configureEtaRootTasks();

        configureInjectionTasks();

        createProguardFiles();
    }

    private void createRootEtaExtension() {
        if (isRootProject()) {
            project.getExtensions().create(EtaBasePlugin.ETA_EXTENSION_NAME,
                                           EtaExtension.class, project);
        }
    }

    private void addEtlasMavenRepository() {
        if (isRootProject()) {
            project.allprojects(p -> {
                    p.getRepositories().maven
                        (repo -> {
                            repo.setName("EtlasMaven");
                            repo.setUrl(mavenRepository.getDirectory().toURI());
                        });
                });
        }
    }

    private void addEtaExtensionForConfigurations() {
        project.getConfigurations().all(this::populateEtaConfiguration);

    }

    private void populateEtaConfiguration(final Configuration configuration) {

        final DefaultEtaConfiguration etaConfiguration =
            ExtensionHelper.createExtension
            (configuration, ETA_CONFIGURATION_EXTENSION_NAME,
             DefaultEtaConfiguration.class, configuration, mavenRepository);

        DomainObjectCollection<EtaDependency> dependencies =
            etaConfiguration.getDependencies();

        configuration.getDependencies().all
            (dependency -> {
                if (dependency instanceof ProjectDependency) {
                    final ProjectDependency projectDependency =
                        (ProjectDependency) dependency;
                    dependencies.add(new DefaultEtaProjectDependency
                                     (projectDependency.getDependencyProject(),
                                      projectDependency.getTargetConfiguration()));
                } else if (dependency instanceof EtaDependency) {
                    dependencies.add((EtaDependency) dependency);
                }
            });
    }

    private void configureEtaRootTasks() {
        /* The global, consistent dependency resolution must be done in the
           root project. */
        if (isRootProject()) {
            EtaSetupEnvironment setupEnvironmentTask =
                project.getTasks().create(ETA_SETUP_ENVIRONMENT_TASK_NAME,
                                          EtaSetupEnvironment.class);

            EtaResolveDependencies resolveDependenciesTask =
                project.getTasks().create(ETA_RESOLVE_DEPENDENCIES_TASK_NAME,
                                          EtaResolveDependencies.class);

            resolveDependenciesTask.setVersionsChanged
                (setupEnvironmentTask.getVersionsChanged());

            resolveDependenciesTask.dependsOn(setupEnvironmentTask);
        }
    }

    private void configureInjectionTasks() {
        if (isRootProject()) {
            project.getGradle().projectsEvaluated
                (gradle -> gradle.allprojects
                 (p -> {
                     JavaPluginConvention javaConvention =
                         p.getConvention().findPlugin(JavaPluginConvention.class);
                     EtaPluginConvention etaConvention =
                         p.getConvention().findPlugin(EtaPluginConvention.class);
                     if (javaConvention != null && etaConvention == null) {
                         javaConvention.getSourceSets().all
                             (sourceSet -> configureSourceSetInjection(p, sourceSet));
                     }
                 }));
        }
    }

    private void configureSourceSetInjection(final Project p, SourceSet sourceSet) {
        EtaInjectDependencies injectDependenciesTask = p.getTasks()
            .create(NamingScheme.getInjectDependenciesTaskName(sourceSet.getName()),
                    EtaInjectDependencies.class);
        injectDependenciesTask.setTargetConfiguration
            (project.provider(() -> sourceSet.getCompileClasspathConfigurationName()));
        injectDependenciesTask.dependsOnProjects();

        sourceSet.setCompileClasspath
            (sourceSet.getCompileClasspath()
             .plus(p.files().builtBy(injectDependenciesTask)));
    }

    private boolean isRootProject() {
        return project == project.getRootProject();
    }

    private void createProguardFiles() {
        ProguardFiles.createAll(project);
    }
}
