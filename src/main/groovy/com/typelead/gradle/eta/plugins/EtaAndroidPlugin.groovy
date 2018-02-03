package com.typelead.gradle.eta.plugins

import org.gradle.api.Plugin
import org.gradle.api.Project

/**
 * A {@link Plugin} which sets up an Eta project.
 */
class EtaAndroidPlugin implements Plugin<Project> {

  @Override
  public void apply(Project project) {
    project.pluginManager.apply EtaBasePlugin
  }
}