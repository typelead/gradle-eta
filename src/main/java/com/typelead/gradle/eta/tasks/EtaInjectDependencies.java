package com.typelead.gradle.eta.tasks;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

import org.gradle.api.Buildable;
import org.gradle.api.DefaultTask;
import org.gradle.api.Project;
import org.gradle.api.artifacts.Configuration;
import org.gradle.api.artifacts.Dependency;
import org.gradle.api.artifacts.ProjectDependency;
import org.gradle.api.artifacts.dsl.DependencyHandler;
import org.gradle.api.provider.Provider;
import org.gradle.api.tasks.Input;
import org.gradle.api.tasks.TaskAction;

import com.typelead.gradle.eta.api.EtaDependency;
import com.typelead.gradle.eta.api.EtaProjectDependency;
import com.typelead.gradle.eta.plugins.EtaBasePlugin;
import com.typelead.gradle.eta.internal.ConfigurationUtils;
import com.typelead.gradle.eta.internal.DefaultEtaConfiguration;
import com.typelead.gradle.utils.ExtensionHelper;

public class EtaInjectDependencies extends DefaultTask {

    private Provider<String> targetConfiguration;

    @Input
    public String getTargetConfiguration() {
        return targetConfiguration.get();
    }

    public void setTargetConfiguration(Provider<String> targetConfiguration) {
        this.targetConfiguration = targetConfiguration;
    }

    public void dependsOnProjects() {
        dependsOn(new Callable<List<Buildable>>() {
                @Override
                public List<Buildable> call() {
                    return getProject().getConfigurations()
                        .getByName(getTargetConfiguration())
                        .getAllDependencies().stream()
                        .filter(dependency -> dependency instanceof ProjectDependency)
                        .flatMap(dependency -> {
                                final ProjectDependency projectDependency =
                                    (ProjectDependency) dependency;
                                final Project project =
                                    projectDependency.getDependencyProject();
                                final String configurationName =
                                    projectDependency.getTargetConfiguration();
                                return ConfigurationUtils.getConfiguration
                                    (project, configurationName)
                                    .getAllArtifacts().stream();
                            })
                        .collect(Collectors.toList());
                }
            });
    }

    @TaskAction
    public void injectDependencies() {
        final Project project = getProject();
        injectProjectDependencies(project, project.getDependencies(),
                                  ConfigurationUtils.getConfiguration
                                  (project, getTargetConfiguration()));
    }

    private static final String INJECT_STATE_PROPERTY = "injectDependenciesState";

    private void injectProjectDependencies(final Project project,
                                           final DependencyHandler dependencies,
                                           final Configuration configuration) {
        ConcurrentHashMap<Dependency, Boolean> injectState =
            getInjectState(configuration);

        final String configurationName = configuration.getName();
        for (Dependency dependency : configuration.getDependencies()) {
            if (dependency instanceof ProjectDependency &&
                injectState.put(dependency, Boolean.TRUE) == null) {
                final ProjectDependency projectDependency =
                    (ProjectDependency) dependency;
                final Project targetProject = projectDependency.getDependencyProject();
                final String  targetConfiguration =
                    projectDependency.getTargetConfiguration();
                List<String> mavenDependencies;
                if (targetProject.getPlugins().hasPlugin(EtaBasePlugin.class)) {
                    mavenDependencies = ConfigurationUtils.getEtaConfiguration
                        (targetProject, targetConfiguration)
                        .getAllResolvedDependencies(project);
                } else {
                    mavenDependencies = DefaultEtaConfiguration
                        .searchForEtaProjectDependencies
                        (project, ConfigurationUtils.getConfiguration
                        (targetProject, targetConfiguration));
                }

                for (String mavenDependency : mavenDependencies) {
                    dependencies.add(configurationName, mavenDependency);
                }
            }
        }

        for (Configuration parent : configuration.getExtendsFrom()) {
            injectProjectDependencies(project, dependencies, parent);
        }
    }

    private ConcurrentHashMap<Dependency, Boolean> getInjectState
        (final Configuration configuration) {
        ConcurrentHashMap<Dependency, Boolean> injectState = null;
        if (!ExtensionHelper.hasExtProperty(configuration, INJECT_STATE_PROPERTY)) {
            synchronized (configuration) {
                if (!ExtensionHelper.hasExtProperty(configuration,
                                                    INJECT_STATE_PROPERTY)) {
                    injectState = new ConcurrentHashMap<Dependency, Boolean>();
                    ExtensionHelper.setExtProperty
                        (configuration, INJECT_STATE_PROPERTY, injectState);
                } else {
                    injectState = (ConcurrentHashMap<Dependency, Boolean>)
                        ExtensionHelper.getExtProperty(configuration,
                                                       INJECT_STATE_PROPERTY);
                }
            }
        } else {
            injectState = (ConcurrentHashMap<Dependency, Boolean>)
                ExtensionHelper.getExtProperty(configuration, INJECT_STATE_PROPERTY);
        }
        return injectState;
    }
}
