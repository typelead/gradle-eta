package com.typelead.gradle.eta.internal;

import org.gradle.api.Project;
import org.gradle.api.artifacts.Dependency;

import com.typelead.gradle.eta.api.EtaProjectDependency;
import com.typelead.gradle.eta.api.SourceRepository;

public class DefaultEtaProjectDependency implements EtaProjectDependency {

    private Project project;
    private String  targetConfiguration;

    public DefaultEtaProjectDependency(Project project) {
        this(project, Dependency.DEFAULT_CONFIGURATION);
    }

    public DefaultEtaProjectDependency(Project project, String targetConfiguration) {
        this.project = project;
        this.targetConfiguration = targetConfiguration;
    }

    @Override
    public Project getProject() {
        return project;
    }

    @Override
    public String getTargetConfiguration() {
        return targetConfiguration;
    }
}
