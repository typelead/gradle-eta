package com.typelead.gradle.eta.api;

import java.io.File;
import java.util.Set;

import org.gradle.api.DomainObjectCollection;
import org.gradle.api.Project;
import org.gradle.api.provider.Provider;

public interface EtaConfiguration {
    DomainObjectCollection<EtaDependency> getDependencies();
    Set<EtaDependency> getAllDependencies();
    Set<Provider<File>> getArtifacts();
    Set<Provider<File>> getAllArtifacts(Project project);
}
