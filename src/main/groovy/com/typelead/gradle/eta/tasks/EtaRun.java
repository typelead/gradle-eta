package com.typelead.gradle.eta.tasks;

import com.typelead.gradle.eta.plugins.EtaPlugin;
import com.typelead.gradle.utils.CabalInfo;
import com.typelead.gradle.utils.EtaRuntimeUtils;
import org.gradle.api.GradleException;
import org.gradle.api.file.FileCollection;
import org.gradle.api.internal.file.collections.SimpleFileCollection;
import org.gradle.api.tasks.TaskAction;

import java.io.File;
import java.io.IOException;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class EtaRun extends AbstractEtlasRun {

  private String component;

  @Override
  @TaskAction
  public void exec() {
    setDefaultComponent();
    setClasspath(patchedClasspath());
    super.exec();
  }

  @Override
  public String getMain() {
    return EtaPlugin.DEFAULT_ETA_MAIN_CLASS;
  }

  /**
   * Returns a patched classpath with our compiled jar from the current classpath.
   */
  private FileCollection patchedClasspath() {
    Set<File> cpOld = getClasspath().getFiles();
    Set<File> runtime = getRuntimeDependencies();
    Set<File> cp = new HashSet<>(cpOld.size() + runtime.size() + 1);
    cp.addAll(cpOld);
    cp.addAll(runtime);
    cp.add(getCompiledJar());
    return new SimpleFileCollection(cp);
  }

  private Set<File> getRuntimeDependencies() {
    return EtaRuntimeUtils.getRuntimeClasspath(this, component);
  }

  private void setDefaultComponent() {
    if (component != null) return;
    CabalInfo cabalInfo = CabalInfo.get(getProject());
    List<String> executables = cabalInfo.getExecutableComponentNames();
    if (executables.size() == 0) {
      throw new GradleException("No executable found in cabal file");
    }
    if (executables.size() > 1) {
      throw new GradleException(
        "Cannot infer executable, found " + executables.size()
          + " executables: " + executables
          + "; please disambiguate with 'component' in task config");
    }
    component = executables.get(0);
  }

  /**
   * Finds compiled 'component' jar.
   */
  private File getCompiledJar() {
    // Strip the "exe:" component prefix, if it exists.
    String name;
    if (component.contains(":")) {
      name = component.split(":", 2)[1];
    } else {
      name = component;
    }
    try {
      System.out.println("GOT THINGS: " + java.nio.file.Files.walk(java.nio.file.Paths.get(getProject().getRootDir().getPath(), getBuildDir(), "build")).collect(java.util.stream.Collectors.toList()));
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
    File jar = new File(
      getProject().getRootDir(),
      String.join(
        File.separator,
        getBuildDir(),
        "build",
        name,
        name + ".jar"
      )
    );
    if (!jar.exists()) {
      throw new GradleException("Compiled jar does not exist: " + jar);
    }
    return jar;
  }

  public String getComponent() {
    return component;
  }

  public void setComponent(String component) {
    this.component = component;
  }

  @Override
  public List<String> getComponents() {
    if (component == null) return Collections.emptyList();
    return Collections.singletonList(component);
  }

  @Override
  public void setComponents(List<String> components) {
    throw new GradleException("Use 'component' property for EtaRun tasks, not 'components'");
  }
}
