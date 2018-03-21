package com.typelead.gradle.eta.plugins;

import java.util.Map;

import org.gradle.api.Project;
import org.gradle.api.artifacts.SelfResolvingDependency;

import com.typelead.gradle.eta.internal.DefaultEtaDirectDependency;
import com.typelead.gradle.eta.internal.DefaultEtaGitDependency;

public class EtaPluginConvention {

    private final Project project;

    public EtaPluginConvention(final Project project) {
        this.project = project;
    }

    public SelfResolvingDependency[] eta(String... dependencyConstraints) {
        SelfResolvingDependency[] dependencies =
            new SelfResolvingDependency[dependencyConstraints.length];
        int i = 0;
        for (String dependencyConstraint : dependencyConstraints) {
            dependencies[i++] =
                DefaultEtaDirectDependency.create(project, dependencyConstraint);
        }
        return dependencies;
    }

    public SelfResolvingDependency eta(Map<String, String> dependencyAttributes) {
        return DefaultEtaGitDependency.create(project, dependencyAttributes);
    }
}
