package com.typelead.gradle.eta.api;

import java.io.File;
import java.util.Set;

import org.gradle.api.DomainObjectCollection;
import org.gradle.api.Project;
import org.gradle.api.provider.Provider;
import org.gradle.api.artifacts.dsl.DependencyHandler;

import com.typelead.gradle.utils.ImmutableDAG;
import com.typelead.gradle.utils.PackageInfo;

public interface EtaConfiguration {
    DomainObjectCollection<EtaDependency> getDependencies();
    Set<EtaDependency> getAllDependencies();
    void resolve(Project project, DependencyHandler dependencies,
                 ImmutableDAG<String, PackageInfo> dependencyGraph);
    Set<Provider<File>> getArtifacts();
    Set<Provider<File>> getAllArtifacts(Project project);
}
