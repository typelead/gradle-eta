package com.typelead.gradle.utils;

import com.typelead.gradle.eta.tasks.EtlasTaskSpec;

import java.io.File;
import java.util.Set;

public abstract class EtaRuntimeUtils {

  public static Set<File> getRuntimeClasspath(EtlasTaskSpec task, String component) {
    return task.getProject().files(
      new EtlasCommand(task).depsClasspath(component).toArray()
    ).getFiles();
  }
}
