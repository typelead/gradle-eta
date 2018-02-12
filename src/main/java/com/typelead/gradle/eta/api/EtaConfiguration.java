package com.typelead.gradle.eta.api;

import org.gradle.api.DomainObjectCollection;

public interface EtaConfiguration {
    DomainObjectCollection<EtaDependency> getDependencies();
}
