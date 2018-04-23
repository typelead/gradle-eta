package com.typelead.gradle.eta.internal;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;

import org.gradle.api.DomainObjectCollection;
import org.gradle.api.GradleException;
import org.gradle.api.Project;
import org.gradle.api.file.FileCollection;
import org.gradle.api.logging.Logger;
import org.gradle.api.artifacts.Configuration;
import org.gradle.api.artifacts.Dependency;
import org.gradle.api.artifacts.ProjectDependency;
import org.gradle.api.artifacts.dsl.DependencyHandler;
import org.gradle.api.provider.Provider;
import org.gradle.api.internal.DefaultDomainObjectCollection;

import com.typelead.gradle.utils.ExtensionHelper;
import com.typelead.gradle.utils.ImmutableDAG;
import com.typelead.gradle.utils.PackageInfo;
import com.typelead.gradle.eta.api.EtaDependency;
import com.typelead.gradle.eta.api.EtaProjectDependency;
import com.typelead.gradle.eta.api.EtaConfiguration;
import com.typelead.gradle.eta.api.HasPackageName;
import com.typelead.gradle.eta.internal.ConfigurationUtils;
import com.typelead.gradle.eta.internal.EtlasMavenRepository;
import com.typelead.gradle.eta.plugins.EtaBasePlugin;

public class DefaultEtaConfiguration implements EtaConfiguration {

    private final Configuration parentConfiguration;
    private final EtlasMavenRepository mavenRepository;

    private DomainObjectCollection<EtaDependency> dependencies =
        new DefaultDomainObjectCollection<EtaDependency>
        (EtaDependency.class, new LinkedHashSet<EtaDependency>());

    private Set<Provider<File>> artifacts = new LinkedHashSet<Provider<File>>();
    private List<String> resolvedMavenDependencies;
    private AtomicBoolean resolved = new AtomicBoolean();

    public DefaultEtaConfiguration(Configuration parentConfiguration,
                                   EtlasMavenRepository mavenRepository) {
        this.parentConfiguration = parentConfiguration;
        this.mavenRepository     = mavenRepository;
    }

    @Override
    public DomainObjectCollection<EtaDependency> getDependencies() {
        return dependencies;
    }

    @Override
    public Set<EtaDependency> getAllDependencies() {
        Set<EtaDependency> allDependencies = new LinkedHashSet<>();
        allDependencies.addAll(dependencies);
        for (Configuration configuration : parentConfiguration.getExtendsFrom()) {
            EtaConfiguration etaConfiguration =
                ExtensionHelper.getExtension(configuration, EtaConfiguration.class);
            allDependencies.addAll(etaConfiguration.getAllDependencies());
        }
        return allDependencies;
    }

    @Override
    public void resolve(final Project project, final DependencyHandler handler,
                        final ImmutableDAG<String, PackageInfo> dependencyGraph) {
        doResolve(project, handler, dependencyGraph, new HashSet<String>());
    }

    public void doResolve(final Project project, final DependencyHandler handler,
                          final ImmutableDAG<String, PackageInfo> dependencyGraph,
                          Set<String> resolvedDependencies) {

        final Logger logger = project.getLogger();
        final String configurationName = parentConfiguration.getName();

        Set<String> resolvedDeps = new HashSet<String>();

        for (Configuration configuration : parentConfiguration.getExtendsFrom()) {
            DefaultEtaConfiguration etaConfiguration =
                ExtensionHelper.getExtension(configuration,
                                             DefaultEtaConfiguration.class);
            etaConfiguration.doResolve(project, handler, dependencyGraph, resolvedDeps);
        }

        List<String> keys = new ArrayList<String>();

        for (EtaDependency dep : dependencies) {
            if (dep instanceof HasPackageName) {
                keys.add(((HasPackageName) dep).getPackageName());
            }
        }

        if (!resolved.get() && resolved.compareAndSet(false, true)) {

            logger.info("Resolving Eta Configuration '"
                        + parentConfiguration.getIncoming().getPath() + "'" );

            List<PackageInfo> packageInfos =
                dependencyGraph.differenceClosure(keys, resolvedDeps);

            if (packageInfos.size() > 0) {

                mavenRepository.installPackages(packageInfos, dependencyGraph);

                resolvedMavenDependencies = packageInfos.stream()
                    .filter(packageInfo -> keys.contains(packageInfo.getName()))
                    .map(mavenRepository::getMavenDependency)
                    .collect(Collectors.toList());

                for (String mavenDep : resolvedMavenDependencies) {
                    handler.add(configurationName, mavenDep);
                    logger.info("Injecting maven dependency '" + mavenDep + "'");
                }
            } else {
                resolvedMavenDependencies = Collections.emptyList();
            }
        }

        resolvedDependencies.addAll(resolvedDeps);
        resolvedDependencies.addAll(keys);
    }

    @Override
    public Set<Provider<File>> getArtifacts() {
        return artifacts;
    }

    @Override
    public Set<Provider<File>> getAllArtifacts(final Project project) {
        Set<Provider<File>> allArtifacts = new LinkedHashSet<Provider<File>>();
        allArtifacts.addAll(artifacts);
        for (EtaDependency dependency : getDependencies()) {
            if (dependency instanceof EtaProjectDependency) {
                final EtaProjectDependency projectDependency =
                    ((EtaProjectDependency) dependency);
                final EtaConfiguration etaConfiguration =
                    ConfigurationUtils.getEtaConfiguration
                    (projectDependency.getProject(project),
                     projectDependency.getTargetConfiguration());
                if (etaConfiguration != null) {
                    allArtifacts.addAll(etaConfiguration.getAllArtifacts(project));
                }
            }
        }
        for (Configuration configuration : parentConfiguration.getExtendsFrom()) {
            final EtaConfiguration etaConfiguration =
                ExtensionHelper.getExtension(configuration, EtaConfiguration.class);
            allArtifacts.addAll(etaConfiguration.getAllArtifacts(project));
        }
        return allArtifacts;

    }

    @Override
    public List<String> getResolvedDependencies() {
        if (resolved.get()) {
            return resolvedMavenDependencies;
        } else {
            return Collections.emptyList();
        }
    }

    @Override
    public List<String> getAllResolvedDependencies(final Project project) {
        List<String> allResolvedDependencies =
            new ArrayList<String>(getResolvedDependencies());

        for (EtaDependency dependency : dependencies) {
            if (dependency instanceof EtaProjectDependency) {
                final EtaProjectDependency projectDependency =
                    (EtaProjectDependency) dependency;
                final Project targetProject = projectDependency.getProject(project);
                final String  targetConfiguration =
                    projectDependency.getTargetConfiguration();
                List<String> mavenDependencies;
                if (targetProject.getPlugins().hasPlugin(EtaBasePlugin.class)) {
                    mavenDependencies = ConfigurationUtils.getEtaConfiguration
                        (targetProject, targetConfiguration)
                        .getAllResolvedDependencies(project);
                } else {
                    mavenDependencies = searchForEtaProjectDependencies
                        (project, ConfigurationUtils.getConfiguration
                         (targetProject, targetConfiguration));
                }
                allResolvedDependencies.addAll(mavenDependencies);
            }
        }

        for (Configuration configuration : parentConfiguration.getExtendsFrom()) {
            allResolvedDependencies.addAll
                (ConfigurationUtils.getEtaConfiguration(configuration)
                 .getAllResolvedDependencies(project));
        }

        return allResolvedDependencies;
    }

    public static List<String> searchForEtaProjectDependencies
        (final Project project, final Configuration configuration) {

        List<String> allMavenDependencies = new ArrayList<String>();
        for (Dependency dependency : configuration.getAllDependencies()) {
            if (dependency instanceof ProjectDependency) {
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
                    mavenDependencies = searchForEtaProjectDependencies
                        (project, ConfigurationUtils.getConfiguration
                         (targetProject, targetConfiguration));
                }
                allMavenDependencies.addAll(mavenDependencies);
            }
        }

        return allMavenDependencies;
    }
}
