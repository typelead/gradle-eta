package com.typelead.gradle.eta.api;

import java.util.Set;
import org.gradle.api.DomainObjectCollection;

public interface EtaConfiguration {
    DomainObjectCollection<EtaDependency> getDependencies();
    Set<EtaDependency> getAllDependencies();
}
