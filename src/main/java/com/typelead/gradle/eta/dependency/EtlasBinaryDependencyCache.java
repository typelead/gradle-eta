package com.typelead.gradle.eta.dependency;

import com.typelead.gradle.utils.CommandLine;
import com.typelead.gradle.utils.Log;
import org.gradle.api.GradleException;
import org.gradle.api.Nullable;
import org.gradle.api.Project;
import org.gradle.api.invocation.Gradle;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class EtlasBinaryDependencyCache implements Log {

  private final String cacheDir;

  public EtlasBinaryDependencyCache(Project project) {
    Gradle gradle = project.getGradle();
    this.cacheDir = gradle.getGradleUserHomeDir() + "/caches/etlas";
  }

  @Nullable
  public String getBinaryPathForVersion(String version, Arch arch) {
    File etlas = new File(uncheckedBinaryPath(version, arch));
    if (!etlas.exists()) return null;
    if (!etlas.canExecute()) {
      throw new GradleException("Cached etlas binary is not executable: " + etlas.getPath());
    }
    try {
      return etlas.getCanonicalPath();
    } catch (IOException e) {
      throw new GradleException("Failed to get canonical path for etlas binary '" + etlas.getPath() + "'", e);
    }
  }

  public String putBinaryForVersion(String version, URL url, Arch arch) {
    Path target = Paths.get(uncheckedBinaryPath(version, arch));
    logger().info("Downloading etlas from: " + url + " ; caching to " + target);
    File dir = target.getParent().toFile();
    if (!dir.exists()) {
      if (!dir.mkdirs()) {
        throw new GradleException("Failed to mkdirs: " + dir.getPath());
      }
    }
    try (InputStream is = url.openStream()) {
      Files.copy(is, target);
    } catch (IOException e) {
      throw new GradleException("Failed to fetch file from url: " + url);
    }
    File result = target.toFile();
    if (!result.exists()) {
      throw new GradleException("Expected etlas to have downloaded to cache, but failed to find at: " + result.getPath());
    }
    if (!result.setExecutable(true)) {
      throw new GradleException("Failed to make etlas executable: " + result.getPath());
    }
    String path;
    try {
      path = result.getCanonicalPath();
    } catch (IOException e) {
      throw new GradleException("Failed to get canonical path for: " + result.getPath());
    }
    // This is mostly a hack to ensure we don't get stuck prompting the user
    // in the background about sending metrics. This will, by default, not send metrics,
    // so users will have to opt-in explicitly with etlas flags.
    File etlasConfig = new File(System.getProperty("user.home"), ".etlas/config");
    if (!etlasConfig.exists()) {
      logger().info("Initializing etlas config via: etlas user-config init");
      new CommandLine(path, "user-config", "init").executeAndLogOutput();
      if (!etlasConfig.exists()) {
        throw new GradleException("Initialized etlas config not found");
      }
    }
    logger().info("Updating etlas packages via: etlas update");
    new CommandLine(path, "update").executeAndLogOutput();
    return path;
  }

  private String uncheckedBinaryPath(String version, Arch arch) {
    return this.cacheDir + "/" + version + "/etlas" + arch.execExt;
  }
}
