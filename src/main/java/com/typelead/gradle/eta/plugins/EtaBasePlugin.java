package com.typelead.gradle.eta.plugins;

import java.io.File;
import java.util.Optional;
import java.nio.file.Paths;

import org.gradle.api.GradleException;
import org.gradle.api.Plugin;
import org.gradle.api.Project;
import org.gradle.api.logging.Logger;
import org.gradle.api.logging.Logging;
import org.gradle.api.plugins.BasePlugin;
import org.gradle.api.plugins.JavaPlugin;

import com.typelead.gradle.utils.EtlasCommand;
import com.typelead.gradle.utils.ExtensionHelper;
import com.typelead.gradle.eta.api.EtaExtension;
import com.typelead.gradle.eta.tasks.EtaSetupEnvironment;
import com.typelead.gradle.eta.tasks.EtaResolveDependencies;
import com.typelead.gradle.eta.internal.DefaultEtaConfiguration;
import com.typelead.gradle.eta.internal.DefaultEtaDependencyHandler;

/**
 * A {@link Plugin} which compiles and tests Eta sources.
 */
public class EtaBasePlugin implements Plugin<Project> {

    /* Constants */
    public static final String ETA_EXTENSION_NAME = "eta";
    public static final String TASK_GROUP_NAME = "EtaPlugin";

    public static final String DEFAULT_ETA_MAIN_CLASS = "eta.main";

    public static final String ETA_DEPENDENCY_HANDLER_DSL_NAME  = "eta";
    public static final String ETA_CONFIGURATION_EXTENSION_NAME  = "eta";

    /* Tasks */
    public static final String ETA_SETUP_ENVIRONMENT_TASK_NAME = "setupEnvironmentEta";
    public static final String
        ETA_RESOLVE_DEPENDENCIES_TASK_NAME = "resolveDependenciesEta";

    /* Protected Fields */
    protected Project project;

    @Override
    public void apply(Project project) {
        this.project   = project;

        project.getPlugins().apply(BasePlugin.class);

        createRootEtaExtension();

        addEtaExtensionForConfigurations();

        configureEtaRootTasks();
    }

    private void createRootEtaExtension() {
        if (project == project.getRootProject()) {
            project.getExtensions().create(EtaPlugin.ETA_EXTENSION_NAME,
                                           EtaExtension.class, project);
        }
    }

    private void addEtaExtensionForConfigurations() {
        ExtensionHelper.createExtension(project.getDependencies(),
                                        ETA_DEPENDENCY_HANDLER_DSL_NAME,
                                        DefaultEtaDependencyHandler.class,
                                        project.getConfigurations());
        project.getConfigurations()
            .all(configuration ->
                 ExtensionHelper.createExtension(configuration,
                                                 ETA_CONFIGURATION_EXTENSION_NAME,
                                                 DefaultEtaConfiguration.class,
                                                 configuration));
    }

    private void configureEtaRootTasks() {
        /* The global, consistent dependency resolution must be done in the
           root project. */
        if (project == project.getRootProject()) {
            EtaSetupEnvironment setupEnvironmentTask =
                project.getTasks().create(ETA_SETUP_ENVIRONMENT_TASK_NAME,
                                          EtaSetupEnvironment.class);

            EtaResolveDependencies resolveDependenciesTask =
                project.getTasks().create(ETA_RESOLVE_DEPENDENCIES_TASK_NAME,
                                          EtaResolveDependencies.class);

            resolveDependenciesTask.dependsOn(setupEnvironmentTask);
        }
    }

}
