package com.typelead.gradle.eta.tasks;

import org.gradle.api.DefaultTask;
import org.gradle.api.Nullable;

import java.util.ArrayList;
import java.util.List;

public abstract class AbstractEtlasTask extends DefaultTask implements EtlasTaskSpec {

    @Nullable private String etlasBinary;
    @Nullable private String etlasVersion;
    private boolean useSandbox;
    private List<String> etlasFlags = new ArrayList<>();
    // We leave this `null` by default; otherwise, it'll get passed to commands when
    // it may not exist already, which will cause errors.
    @Nullable private String sandboxConfig;
    @Nullable private String defaultUserConfig;
    private List<String> buildFlags = new ArrayList<>();
    private String buildDir;
    private List<String> components = new ArrayList<>();
    private List<String> configureFlags = new ArrayList<>();

    @Override
    public boolean getUseSandbox() {
        return useSandbox;
    }

    @Override
    public void setUseSandbox(boolean useSandbox) {
        this.useSandbox = useSandbox;
    }

    @Override
    public String getSandboxConfig() {
        return sandboxConfig;
    }

    @Override
    public void setSandboxConfig(String sandboxConfig) {
        this.sandboxConfig = sandboxConfig;
    }

    @Override
    public String getDefaultUserConfig() {
        return defaultUserConfig;
    }

    @Override
    public void setDefaultUserConfig(String defaultUserConfig) {
        this.defaultUserConfig = defaultUserConfig;
    }

    @Override
    public String getEtlasBinary() {
        return etlasBinary;
    }

    @Override
    public void setEtlasBinary(String etlasBinary) {
        this.etlasBinary = etlasBinary;
    }

    @Override
    public String getEtlasVersion() {
        return etlasVersion;
    }

    @Override
    public void unsafeSetEtlasVersion(String etlasVersion) {
        this.etlasVersion = etlasVersion;
    }

    @Override
    public List<String> getEtlasFlags() {
        return etlasFlags;
    }

    @Override
    public void setEtlasFlags(List<String> etlasFlags) {
        this.etlasFlags = etlasFlags;
    }

    @Override
    public List<String> getBuildFlags() {
        return buildFlags;
    }

    @Override
    public void setBuildFlags(List<String> buildFlags) {
        this.buildFlags = buildFlags;
    }

    @Override
    public String getBuildDir() {
        return buildDir;
    }

    @Override
    public void setBuildDir(String buildDir) {
        this.buildDir = buildDir;
    }

    @Override
    public List<String> getComponents() {
        return components;
    }

    @Override
    public void setComponents(List<String> components) {
        this.components = components;
    }

    @SuppressWarnings("WeakerAccess")
    public List<String> getConfigureFlags() {
        return configureFlags;
    }

    @SuppressWarnings("unused")
    public void setConfigureFlags(List<String> configureFlags) {
        this.configureFlags = configureFlags;
    }
}
