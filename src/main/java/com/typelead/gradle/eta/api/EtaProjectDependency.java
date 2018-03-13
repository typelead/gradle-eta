package com.typelead.gradle.eta.api;

import org.gradle.api.Project;
import com.typelead.gradle.utils.VersionRange;

public interface EtaProjectDependency extends EtaDependency {
    Project getProject(Project project);
    String getProjectPath();
    String getTargetConfiguration();
}
