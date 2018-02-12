package com.typelead.gradle.eta.internal;


import java.util.LinkedHashSet;

import org.gradle.api.DomainObjectCollection;
import org.gradle.api.internal.DefaultDomainObjectCollection;

import com.typelead.gradle.eta.api.EtaDependency;
import com.typelead.gradle.eta.api.EtaConfiguration;

public class DefaultEtaConfiguration implements EtaConfiguration {

    DomainObjectCollection<EtaDependency>
        dependencies = new DefaultDomainObjectCollection<EtaDependency>(EtaDependency.class, new LinkedHashSet<EtaDependency>());

    public DefaultEtaConfiguration() {}

    @Override
    public DomainObjectCollection<EtaDependency> getDependencies() {
        return dependencies;
    }
}
