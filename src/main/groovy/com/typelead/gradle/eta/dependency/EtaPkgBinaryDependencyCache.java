package com.typelead.gradle.eta.dependency;

import org.gradle.api.Project;

public class EtaPkgBinaryDependencyCache extends AbstractBinaryDependencyCache {

    public EtaPkgBinaryDependencyCache(Project project) {
        super(project, "eta-pkg");
    }
}
