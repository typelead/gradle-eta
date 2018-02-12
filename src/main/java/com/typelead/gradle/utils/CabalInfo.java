package com.typelead.gradle.utils;

import org.gradle.api.GradleException;
import org.gradle.api.Project;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class CabalInfo {

  public static CabalInfo get(Project project) {
    File projectDir = project.getProjectDir();
    File[] arr      = projectDir.listFiles();
    if (arr == null) {
      throw new GradleException("Project root is unexpectedly empty " + projectDir);
    }
    List<File> cabalFiles = Arrays.stream(arr)
      .filter(f -> f.getName().endsWith(".cabal"))
      .collect(Collectors.toList());
    if (cabalFiles.size() == 0) {
      throw new GradleException("Could not find cabal file in project root " + projectDir);
    }
    if (cabalFiles.size() > 1) {
      throw new GradleException(
        "Found more than one cabal file in project root "
          + projectDir + " " + cabalFiles);
    }
    File cabalFile = cabalFiles.get(0);
    List<String> lines;
    try {
      lines = Files.readAllLines(cabalFiles.get(0).toPath());
    } catch (IOException e) {
      throw new GradleException("Failed to read cabal file " + cabalFile);
    }
    return CabalInfoParser.parse(lines);
  }

  private final String name;
  private final boolean hasLibrary;
  private final List<String> executableNames;
  private final List<String> executableComponentNames;
  private final List<String> testNames;
  private final List<String> testComponentNames;
  private final List<String> benchmarkNames;
  private final List<String> benchmarkComponentNames;
  private final List<String> productionComponentNames;

  CabalInfo(String name,
            boolean hasLibrary,
            List<String> executableNames,
            List<String> testNames,
            List<String> benchmarkNames) {
    this.name = name;
    this.hasLibrary = hasLibrary;
    this.executableNames = Collections.unmodifiableList(executableNames);
    this.executableComponentNames = applyComponentPrefix("exe", executableNames);
    this.testNames = Collections.unmodifiableList(testNames);
    this.testComponentNames = applyComponentPrefix("test", testNames);
    this.benchmarkNames = Collections.unmodifiableList(benchmarkNames);
    this.benchmarkComponentNames = applyComponentPrefix("bench", benchmarkNames);
    this.productionComponentNames = Collections.unmodifiableList(
      Stream.concat(
        hasLibrary ? Stream.of("lib:" + name) : Stream.empty(),
        executableComponentNames.stream()
      ).collect(Collectors.toList())
    );
  }

  private static List<String> applyComponentPrefix(String prefix, List<String> names) {
    return Collections.unmodifiableList(
      names.stream().map(x -> prefix + ':' + x).collect(Collectors.toList())
    );
  }

  public List<String> getProductionComponentNames() {
    return productionComponentNames;
  }

  public List<String> getExecutableComponentNames() {
    return executableComponentNames;
  }

  public List<String> getTestComponentNames() {
    return testComponentNames;
  }

  public String getName() {
    return name;
  }

  public boolean hasLibrary() {
    return hasLibrary;
  }

  public List<String> getExecutableNames() {
    return executableNames;
  }

  public List<String> getTestNames() {
    return testNames;
  }

  public List<String> getBenchmarkNames() {
    return benchmarkNames;
  }
}
