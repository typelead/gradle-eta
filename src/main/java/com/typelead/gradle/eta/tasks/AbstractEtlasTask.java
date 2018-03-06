package com.typelead.gradle.eta.tasks;

import org.gradle.api.DefaultTask;
import org.gradle.api.Nullable;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import com.typelead.gradle.eta.api.EtaExtension;
import com.typelead.gradle.eta.plugins.EtaPlugin;

public abstract class AbstractEtlasTask extends DefaultTask implements EtlasTaskSpec {

    @Nullable
    private String etlasBinary;
    @Nullable
    private String etlasVersion;
    @Nullable
    private String etaVersion;
    private List<String> etlasFlags = new ArrayList<>();
    // We leave this `null` by default; otherwise, it'll get passed to commands when
    // it may not exist already, which will cause errors.
    @Nullable
    private List<String> buildFlags = new ArrayList<>();
    private String buildDir;
    private List<String> components = new ArrayList<>();
    private List<String> configureFlags = new ArrayList<>();

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
    public String getEtaVersion() {
        return etaVersion;
    }

    @Override
    public void setEtaVersion(String etaVersion) {
        this.etaVersion = etaVersion;
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

    public void configureWithExtension(EtaExtension extension) {
        configureWithExtension(extension, "default");
    }

    public void configureWithExtension(EtaExtension extension, String buildVariantPath) {
        setEtlasBinary(extension.getEtlasBinary());
        unsafeSetEtlasVersion(extension.getEtlasVersion());
        setEtaVersion(extension.getVersion());
        setGroup(EtaPlugin.TASK_GROUP_NAME);
        setEtlasFlags(extension.getEtlasFlags());
        setBuildFlags(extension.getBuildFlags());
        String buildDir =
            getProject().getBuildDir() + File.separator + extension.getBuildDir()
            + File.separator + buildVariantPath;
        setBuildDir(buildDir);
    }
}
