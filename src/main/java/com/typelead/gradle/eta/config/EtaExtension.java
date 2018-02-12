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

  @Nullable
  private String etlasBinary;
  private String etlasRepo = EtaPlugin.DEFAULT_ETLAS_REPO;
  @Nullable
  private String etlasVersion;
  private boolean useSystemEtlas = EtaPlugin.DEFAULT_USE_SYSTEM_ETLAS;

  private boolean useSandbox = EtaPlugin.DEFAULT_USE_SANDBOX;
  @Nullable
  private String sandboxConfig;
  @Nullable
  private String defaultUserConfig;
  private List<String> etlasFlags = new ArrayList<>();
  private List<String> buildFlags = new ArrayList<>();
  private String buildDir = EtaPlugin.DEFAULT_BUILD_DIR;

  /**
   * Sets default values based on properties; does not overwrite existing values.
   */
  public void setDefaultsFromProperties(Project project) {
    // Helper for setting String properties.
    BiConsumer<String, Consumer<String>> setStrProp = (k, setter) -> {
      Object v = project.findProperty("eta." + k);
      if (v == null) return;
      setter.accept(v.toString());
    };
    // Helper for setting boolean properties
    BiConsumer<String, Consumer<Boolean>> setBoolProp = (k, setter) -> {
      setStrProp.accept(k, s -> {
        if (s.equalsIgnoreCase("true")) setter.accept(true);
        if (s.equalsIgnoreCase("false")) setter.accept(false);
        else throw new GradleException("Invalid property value for eta." + k + ": " + s);
      });
    };
    // Helper for throwing exceptions when non-String properties are set.
    Consumer<String> notSupported = k -> {
      if (project.findProperty("eta." + k) != null) {
        throw new GradleException("Setting eta." + k + " via a property is not supported");
      }
    };
    if (getEtlasBinary() == null) setStrProp.accept("etlasBinary", this::setEtlasBinary);
    if (getEtlasRepo().equals(EtaPlugin.DEFAULT_ETLAS_REPO))
      setStrProp.accept("etlasRepo", this::setEtlasRepo);
    if (getEtlasVersion() == null) setStrProp.accept("etlasVersion", this::setEtlasVersion);
    if (getUseSystemEtlas() == EtaPlugin.DEFAULT_USE_SYSTEM_ETLAS)
      setBoolProp.accept("useSystemEtlas", this::setUseSystemEtlas);
    if (getUseSandbox() == EtaPlugin.DEFAULT_USE_SANDBOX)
      setBoolProp.accept("useSandbox", this::setUseSandbox);
    if (getSandboxConfig() == null) setStrProp.accept("sandboxConfig", this::setSandboxConfig);
    if (getDefaultUserConfig() == null)
      setStrProp.accept("defaultUserConfig", this::setDefaultUserConfig);
    notSupported.accept("etlasFlags");
    notSupported.accept("buildFlags");
    if (getBuildDir() == null) setStrProp.accept("buildDir", this::setBuildDir);
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
