package com.typelead.gradle.eta.config;

import com.typelead.gradle.eta.plugins.EtaPlugin;
import org.gradle.api.Nullable;

import java.util.ArrayList;
import java.util.List;

/**
 * Configuration options for the {@link EtaPlugin}
 */
public class EtaExtension {

    @Nullable private String etlasBinary;
    private String etlasRepo = EtaPlugin.DEFAULT_ETLAS_REPO;
    @Nullable private String etlasVersion;
    private boolean useSystemEtlas = EtaPlugin.DEFAULT_USE_SYSTEM_ETLAS;

    private boolean useSandbox = EtaPlugin.DEFAULT_USE_SANDBOX;
    @Nullable private String sandboxConfig;
    @Nullable private String defaultUserConfig;
    private List<String> etlasFlags = new ArrayList<>();
    private List<String> buildFlags = new ArrayList<>();
    private String buildDir = EtaPlugin.DEFAULT_BUILD_DIR;

    @Nullable public String getEtlasBinary() {
        return etlasBinary;
    }

    public void setEtlasBinary(String etlasBinary) {
        this.etlasBinary = etlasBinary;
    }

    @Nullable public String getEtlasRepo() {
        return etlasRepo;
    }

    public void setEtlasRepo(String etlasRepo) {
        this.etlasRepo = etlasRepo;
    }

    @Nullable public String getEtlasVersion() {
        return etlasVersion;
    }

    public void setEtlasVersion(String etlasVersion) {
        this.etlasVersion = etlasVersion;
    }

    public Boolean getUseSystemEtlas() {
        return useSystemEtlas;
    }

    public void setUseSystemEtlas(Boolean useSystemEtlas) {
        this.useSystemEtlas = useSystemEtlas;
    }

    public boolean getUseSandbox() {
        return useSandbox;
    }

    public void setUseSandbox(boolean useSandbox) {
        this.useSandbox = useSandbox;
    }

    public String getSandboxConfig() {
        return sandboxConfig;
    }

    public void setSandboxConfig(String sandboxConfig) {
        this.sandboxConfig = sandboxConfig;
    }

    public String getDefaultUserConfig() {
        return defaultUserConfig;
    }

    public void setDefaultUserConfig(String defaultUserConfig) {
        this.defaultUserConfig = defaultUserConfig;
    }

    public List<String> getEtlasFlags() {
        return etlasFlags;
    }

    public void setEtlasFlags(List<String> etlasFlags) {
        this.etlasFlags = etlasFlags;
    }

    public List<String> getBuildFlags() {
        return buildFlags;
    }

    public void setBuildFlags(List<String> buildFlags) {
        this.buildFlags = buildFlags;
    }

    public String getBuildDir() {
        return buildDir;
    }

    public void setBuildDir(String buildDir) {
        this.buildDir = buildDir;
    }
}
