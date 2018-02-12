package com.typelead.gradle.utils;

import com.typelead.gradle.eta.config.EtaExtension;
import com.typelead.gradle.eta.tasks.EtlasTaskSpec;
import org.gradle.api.Project;
import org.gradle.api.file.FileCollection;

import java.io.File;
import java.util.Set;

/**
 * Helper utilities for programmatically obtaining the set of files
 * required at runtime for Eta projects.
 */
public abstract class EtaRuntimeUtils {

    public static FileCollection getRuntimeFileCollection(EtlasTaskSpec task, String component) {
        return getRuntimeFileCollection(task.getProject(), new EtlasCommand(task), component);
    }

    public static FileCollection getRuntimeFileCollection(Project project, EtlasCommand c, String component) {
        return project.files(c.depsClasspath(component).toArray());
    }

    public static Set<File> getRuntimeClasspath(EtlasTaskSpec task, String component) {
        return getRuntimeClasspath(task.getProject(), new EtlasCommand(task), component);
    }

    @SuppressWarnings("unused")
    public static Set<File> getRuntimeClasspath(Project project, EtaExtension ext, String component) {
        return getRuntimeClasspath(project, new EtlasCommand(project, ext), component);
    }

    @SuppressWarnings("WeakerAccess")
    public static Set<File> getRuntimeClasspath(Project project, EtlasCommand c, String component) {
        return getRuntimeFileCollection(project, c, component).getFiles();
    }
}
