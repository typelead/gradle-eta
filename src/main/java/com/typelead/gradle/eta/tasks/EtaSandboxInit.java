package com.typelead.gradle.eta.tasks;

import com.typelead.gradle.utils.EtlasCommand;
import org.gradle.api.tasks.TaskAction;

public class EtaSandboxInit extends AbstractEtlasTask {

  @TaskAction
  public void sandboxInitEta() {
    new EtlasCommand(this).maybeInitSandbox();
  }
}
