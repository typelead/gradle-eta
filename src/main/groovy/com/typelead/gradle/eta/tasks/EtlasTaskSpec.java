package com.typelead.gradle.eta.tasks;

import org.gradle.api.Task;

import java.util.List;

public interface EtlasTaskSpec extends Task {

    boolean getUseSandbox();

    void setUseSandbox(boolean useSandbox);

    String getSandboxConfig();

    void setSandboxConfig(String sandboxConfig);

    String getDefaultUserConfig();

    void setDefaultUserConfig(String defaultUserConfig);

    String getEtlasBinary();

    void setEtlasBinary(String etlasBinary);

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
