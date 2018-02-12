package com.typelead.gradle.eta.tasks;

import com.typelead.gradle.utils.EtlasCommand;
import org.gradle.api.tasks.TaskAction;

public class EtaInstallDeps extends AbstractEtlasTask {

  @TaskAction
  public void installDepsEta() {
    new EtlasCommand(this).installDependenciesOnly();
  }
}
