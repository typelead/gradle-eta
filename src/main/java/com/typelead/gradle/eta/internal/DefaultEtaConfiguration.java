package com.typelead.gradle.eta.internal;

import java.util.Set;
import java.util.LinkedHashSet;

import org.gradle.api.DomainObjectCollection;
import org.gradle.api.artifacts.Configuration;
import org.gradle.api.internal.DefaultDomainObjectCollection;

import com.typelead.gradle.utils.ExtensionHelper;

import com.typelead.gradle.eta.api.EtaDependency;
import com.typelead.gradle.eta.api.EtaConfiguration;

public class DefaultEtaConfiguration implements EtaConfiguration {

    private Configuration parentConfiguration;

    private DomainObjectCollection<EtaDependency> dependencies =
        new DefaultDomainObjectCollection<EtaDependency>
        (EtaDependency.class, new LinkedHashSet<EtaDependency>());

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
}
