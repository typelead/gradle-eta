package com.typelead.gradle.eta.tasks;

import com.typelead.gradle.utils.EtlasCommand;
import org.gradle.api.tasks.TaskAction;

public class EtaClean extends AbstractEtlasTask implements EtlasTaskSpec {

  @TaskAction
  public void cleanEta() {
    EtlasCommand c = new EtlasCommand(this);
    c.clean();
    c.deleteSandbox();
  }
}
