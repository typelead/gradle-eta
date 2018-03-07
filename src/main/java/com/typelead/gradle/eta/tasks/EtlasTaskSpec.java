package com.typelead.gradle.eta.tasks;

import org.gradle.api.Task;
import org.gradle.api.provider.Property;

import com.typelead.gradle.utils.ResolvedExecutable;

public interface EtlasTaskSpec extends Task {

    Property<ResolvedExecutable> getEtlas();

    Property<ResolvedExecutable> getEta();

    Property<String> getBuildDirectory();

    void setBuildDirectory(String buildDirectory);
}
