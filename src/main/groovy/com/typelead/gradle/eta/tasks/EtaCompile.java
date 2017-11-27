package com.typelead.gradle.eta.tasks;

import com.typelead.gradle.utils.CabalInfo;
import com.typelead.gradle.utils.EtlasCommand;
import org.gradle.api.tasks.TaskAction;

import java.util.List;

public class EtaCompile extends AbstractEtlasTask {

  @TaskAction
  public void compileEta() {
    EtlasCommand c = new EtlasCommand(this);
    c.maybeInitSandbox();
    c.installDependenciesOnly();
    if (!getConfigureFlags().isEmpty()) c.configure(getConfigureFlags());
    c.build();
  }

  @Override
  public final List<String> getComponents() {
    if (!super.getComponents().isEmpty()) return super.getComponents();
    CabalInfo cabalInfo = CabalInfo.get(getProject());
    return cabalInfo.getProductionComponentNames();
  }
}
