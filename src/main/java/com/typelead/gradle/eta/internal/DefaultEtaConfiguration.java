package com.typelead.gradle.eta.internal;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;

import org.gradle.api.DomainObjectCollection;
import org.gradle.api.Project;
import org.gradle.api.file.FileCollection;
import org.gradle.api.logging.Logger;
import org.gradle.api.artifacts.Configuration;
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

public class DefaultEtaConfiguration implements EtaConfiguration {

    private final Configuration parentConfiguration;
    private final EtlasMavenRepository mavenRepository;

    private DomainObjectCollection<EtaDependency> dependencies =
        new DefaultDomainObjectCollection<EtaDependency>
        (EtaDependency.class, new LinkedHashSet<EtaDependency>());

    private Set<Provider<File>> artifacts = new LinkedHashSet<Provider<File>>();
    private List<String> resolvedMavenDependencies;
    private List<String> resolvedLocalMavenDependencies;
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

            logger.info("Resolving Eta Configuration '" + configurationName + "'");

            List<PackageInfo> packageInfos =
                dependencyGraph.differenceClosure(keys, resolvedDeps);

            if (packageInfos.size() > 0) {

                mavenRepository.installPackages(packageInfos, dependencyGraph);

                resolvedMavenDependencies = packageInfos.stream()
                    .flatMap(x -> x.getMavenDependencies().stream())
                    .collect(Collectors.toList());

                resolvedLocalMavenDependencies = packageInfos.stream()
                    .map(mavenRepository::getMavenDependency)
                    .collect(Collectors.toList());

                List<String> allMavenDeps =
                    new ArrayList<String>(resolvedMavenDependencies);

                allMavenDeps.addAll(resolvedLocalMavenDependencies);

                for (String mavenDep : allMavenDeps) {
                    handler.add(configurationName, mavenDep);
                    logger.info("Injecting maven dependency '" + mavenDep + "'");
                }
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
}
