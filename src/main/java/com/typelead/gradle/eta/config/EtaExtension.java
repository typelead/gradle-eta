package com.typelead.gradle.eta.config;

import com.typelead.gradle.eta.plugins.EtaPlugin;
import org.gradle.api.GradleException;
import org.gradle.api.Nullable;
import org.gradle.api.Project;

import java.util.ArrayList;
import java.util.List;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

/**
 * Configuration options for the {@link EtaPlugin}
 */
public class EtaExtension {

    private static final String DEFAULT_ETLAS_REPO =
        "http://cdnverify.eta-lang.org/eta-binaries";
    private static final boolean DEFAULT_USE_SYSTEM_ETLAS = false;
    private static final boolean DEFAULT_USE_SANDBOX = true;
    /* This is relative to the default Gradle build directory. */
    private static final String DEFAULT_BUILD_DIR = "eta";
    private static final String DEFAULT_SANDBOX_ROOT_DIR = "eta-sandbox";

    @Nullable
    private String etlasBinary;
    private String etlasRepo = DEFAULT_ETLAS_REPO;
    @Nullable
    private String etlasVersion;
    private boolean useSystemEtlas = DEFAULT_USE_SYSTEM_ETLAS;

    @Nullable
    private String etaVersion;

    private boolean useSandbox = DEFAULT_USE_SANDBOX;
    @Nullable
    private String sandboxConfig;
    @Nullable
    private String sandboxDir;
    @Nullable
    private String defaultUserConfig;
    private List<String> etlasFlags = new ArrayList<>();
    private List<String> buildFlags = new ArrayList<>();
    private String buildDir = DEFAULT_BUILD_DIR;
    private String sandboxRootDir = DEFAULT_SANDBOX_ROOT_DIR;

    /**
     * Sets default values based on properties; does not overwrite existing values.
     */
    public void setDefaultsFromProperties(Project project) {
        /* Helper for setting String properties. */

        BiConsumer<String, Consumer<String>> setStrProp = (k, setter) -> {
            Object v = project.findProperty("eta." + k);
            if (v == null) return;
            setter.accept(v.toString());
        };

        /* Helper for setting boolean properties */
        BiConsumer<String, Consumer<Boolean>> setBoolProp = (k, setter) -> {
            setStrProp.accept(k, s -> {
                    if (s.equalsIgnoreCase("true")) setter.accept(true);
                    if (s.equalsIgnoreCase("false")) setter.accept(false);
                    else throw new GradleException("Invalid property value for eta." + k + ": " + s);
                });
        };

        /* Helper for throwing exceptions when non-String properties are set. */
        Consumer<String> notSupported = k -> {
            if (project.findProperty("eta." + k) != null) {
                throw new GradleException("Setting eta." + k + " via a property is not supported");
            }
        };

        if (getEtlasBinary() == null) {
            setStrProp.accept("etlasBinary", this::setEtlasBinary);
        }

        if (getEtlasRepo().equals(DEFAULT_ETLAS_REPO)) {
            setStrProp.accept("etlasRepo", this::setEtlasRepo);
        }

        if (getEtlasVersion() == null) {
            setStrProp.accept("etlasVersion", this::setEtlasVersion);
        }

        if (getUseSystemEtlas() == DEFAULT_USE_SYSTEM_ETLAS) {
            setBoolProp.accept("useSystemEtlas", this::setUseSystemEtlas);
        }

        if (getVersion() == null) {
            setStrProp.accept("version", this::setVersion);
        }

        if (getSandboxConfig() == null) {
            setStrProp.accept("sandboxConfig", this::setSandboxConfig);
        }

        if (getDefaultUserConfig() == null) {
            setStrProp.accept("defaultUserConfig", this::setDefaultUserConfig);
        }

        notSupported.accept("etlasFlags");
        notSupported.accept("buildFlags");

        if (getBuildDir() == DEFAULT_BUILD_DIR) {
            setStrProp.accept("buildDir", this::setBuildDir);
        }

        if (getSandboxRootDir() == DEFAULT_SANDBOX_ROOT_DIR) {
            setStrProp.accept("sandboxRootDir", this::setSandboxRootDir);
        }
    }

    @Nullable
    public String getEtlasBinary() {
        return etlasBinary;
    }

    public void setEtlasBinary(String etlasBinary) {
        this.etlasBinary = etlasBinary;
    }

    @Nullable
    public String getEtlasRepo() {
        return etlasRepo;
    }

    public void setEtlasRepo(String etlasRepo) {
        this.etlasRepo = etlasRepo;
    }

    @Nullable
    public String getEtlasVersion() {
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

    @Nullable
    public String getVersion() {
        return etaVersion;
    }

    public void setVersion(String etaVersion) {
        this.etaVersion = etaVersion;
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

    public String getSandboxDir() {
        return sandboxDir;
    }

    public void setSandboxDir(String sandboxDir) {
        this.sandboxDir = sandboxDir;
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

    public String getSandboxRootDir() {
        return sandboxRootDir;
    }

    public void setSandboxRootDir(String sandboxRootDir) {
        this.sandboxRootDir = sandboxRootDir;
    }
}
