package com.typelead.gradle.eta.dependency;

import org.gradle.api.Project;

public class EtaBinaryDependencyCache extends AbstractBinaryDependencyCache {

    public EtaBinaryDependencyCache(Project project) {
        super(project, "eta");
    }
}
