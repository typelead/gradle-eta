package com.typelead.gradle.eta.tasks;

import org.gradle.api.Task;

import java.util.List;

public interface EtlasTaskSpec extends Task {

    String getEtlasBinary();

    void setEtlasBinary(String etlasBinary);

    String getEtlasVersion();

    // This should only be used internally to set the etlas version to check compatibility.
    void unsafeSetEtlasVersion(String etlasVersion);

    String getEtaVersion();

    void setEtaVersion(String etaVersion);

    List<String> getEtlasFlags();

    void setEtlasFlags(List<String> etlasFlags);

    List<String> getBuildFlags();

    void setBuildFlags(List<String> buildFlags);

    String getBuildDir();

    void setBuildDir(String buildDir);

    List<String> getComponents();

    void setComponents(List<String> component);

    List<String> getConfigureFlags();

    void setConfigureFlags(List<String> configureFlags);
}
