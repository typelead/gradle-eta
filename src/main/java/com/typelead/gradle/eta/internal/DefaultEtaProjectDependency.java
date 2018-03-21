package com.typelead.gradle.eta.internal;

import org.gradle.api.Project;
import org.gradle.api.artifacts.Dependency;

import com.typelead.gradle.eta.api.EtaProjectDependency;
import com.typelead.gradle.eta.api.SourceRepository;

public class DefaultEtaProjectDependency implements EtaProjectDependency {

    private String projectPath;
    private String targetConfiguration;

    public DefaultEtaProjectDependency(Project project) {
        this(project.getPath(), null);
    }

    public DefaultEtaProjectDependency(Project project, String targetConfiguration) {
        this(project.getPath(), targetConfiguration);
    }

    public DefaultEtaProjectDependency(String projectPath, String targetConfiguration) {
        this.projectPath = projectPath;
        this.targetConfiguration =
            targetConfiguration == null?
            Dependency.DEFAULT_CONFIGURATION : targetConfiguration;
    }

    @Override
    public Project getProject(final Project project) {
        return project.project(projectPath);
    }

    @Override
    public String getProjectPath() {
        return projectPath;
    }

    @Override
    public String getTargetConfiguration() {
        return targetConfiguration;
    }

    @Override
    public boolean equals(Object o) {
        if (!(o instanceof EtaProjectDependency)) {
            return false;
        }
        EtaProjectDependency projectDep = (EtaProjectDependency) o;
        return projectDep.getProjectPath().equals(projectPath)
            && projectDep.getTargetConfiguration().equals(targetConfiguration);
    }

    @Override
    public String toString() {
        return projectPath;
    }
}
