package com.typelead.gradle.eta.internal;

import java.io.File;
import java.util.Set;
import java.util.LinkedHashSet;

import org.gradle.api.DomainObjectCollection;
import org.gradle.api.Project;
import org.gradle.api.artifacts.Configuration;
import org.gradle.api.provider.Provider;
import org.gradle.api.internal.DefaultDomainObjectCollection;

import com.typelead.gradle.utils.ExtensionHelper;
import com.typelead.gradle.eta.api.EtaDependency;
import com.typelead.gradle.eta.api.EtaProjectDependency;
import com.typelead.gradle.eta.api.EtaConfiguration;
import com.typelead.gradle.eta.internal.ConfigurationUtils;

public class DefaultEtaConfiguration implements EtaConfiguration {

    private Configuration parentConfiguration;

    private DomainObjectCollection<EtaDependency> dependencies =
        new DefaultDomainObjectCollection<EtaDependency>
        (EtaDependency.class, new LinkedHashSet<EtaDependency>());

    private Set<Provider<File>> artifacts = new LinkedHashSet<Provider<File>>();

    public DefaultEtaConfiguration(Configuration parentConfiguration) {
        this.parentConfiguration = parentConfiguration;
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
                allArtifacts.addAll(etaConfiguration.getAllArtifacts(project));
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
