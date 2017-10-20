package com.typelead.gradle.eta.config;

import com.typelead.gradle.eta.dependency.BinaryDependency;
import com.typelead.gradle.eta.plugins.EtaPlugin;
import org.gradle.api.Nullable;

import java.util.ArrayList;
import java.util.List;

/**
 * Configuration options for the {@link EtaPlugin}
 */
public class EtaExtension {

    private String etaRepo = EtaPlugin.DEFAULT_ETA_BINARY_REPO;

    private BinaryConfig etlas = new BinaryConfig();
    private BinaryConfig eta = new BinaryConfig();
    private BinaryConfig etaPkg = new BinaryConfig();

    private boolean useSandbox = EtaPlugin.DEFAULT_USE_SANDBOX;
    @Nullable private String sandboxConfig;
    @Nullable private String defaultUserConfig;
    private List<String> etlasFlags = new ArrayList<>();
    private List<String> buildFlags = new ArrayList<>();
    private String buildDir = EtaPlugin.DEFAULT_BUILD_DIR;

    @Nullable public String getEtaRepo() {
        return etaRepo;
    }

    public void setEtaRepo(String etaRepo) {
        this.etaRepo = etaRepo;
    }

    public BinaryConfig getEtlas() {
        return etlas;
    }

    public void setEtlas(BinaryConfig etlas) {
        this.etlas = etlas;
    }

    public BinaryConfig getEta() {
        return eta;
    }

    public void setEta(BinaryConfig eta) {
        this.eta = eta;
    }

    public BinaryConfig getEtaPkg() {
        return etaPkg;
    }

    public void setEtaPkg(BinaryConfig etaPkg) {
        this.etaPkg = etaPkg;
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

    public static class BinaryConfig {
        @Nullable private String path;
        @Nullable private String version;
        private boolean useSystem = false;

        public BinaryConfig() {}

        public void update(@Nullable BinaryDependency dep) {
            if (dep == null) return;
            this.path = dep.getPath();
            this.version = dep.getVersion();
        }

        public boolean hasConfiguration() {
            return useSystem || path != null || version != null;
        }

        @Nullable
        public String getPath() {
            return path;
        }

        public void setPath(String path) {
            this.path = path;
        }

        @Nullable
        public String getVersion() {
            return version;
        }

        public void setVersion(String version) {
            this.version = version;
        }

        public boolean getUseSystem() {
            return useSystem;
        }

        public void setUseSystem(boolean useSystem) {
            this.useSystem = useSystem;
        }
    }
}
