package com.typelead.gradle.eta.tasks;

import com.typelead.gradle.utils.EtlasCommand;
import org.gradle.api.tasks.TaskAction;

import java.util.List;

public class EtaSandboxAddSources extends AbstractEtlasTask {

  private List<String> sources;

  @TaskAction
  public void sandboxAddSourcesEta() {
    new EtlasCommand(this).sandboxAddSources(sources);
  }

  public List<String> getSources() {
    return sources;
  }

  public void setSources(List<String> sources) {
    this.sources = sources;
  }
}
