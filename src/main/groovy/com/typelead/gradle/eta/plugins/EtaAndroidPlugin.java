package com.typelead.gradle.eta.plugins;

import java.util.Optional;

import org.gradle.api.Plugin;
import org.gradle.api.Project;
import org.gradle.api.ProjectConfigurationException;
import org.gradle.api.NamedDomainObjectContainer;

import com.android.build.gradle.BaseExtension;
import com.android.build.gradle.api.AndroidSourceSet;

import com.typelead.gradle.eta.android.AndroidUtil;

/**
 * A {@link Plugin} which sets up an Eta project.
 */
public class EtaAndroidPlugin extends EtaBasePlugin implements Plugin<Project> {

  @Override
  public void apply(Project project) {
      super.apply(project);
      Plugin<Project> androidPlugin =
          AndroidUtil.getAndroidPlugin(project)
          .orElseThrow(() -> new ProjectConfigurationException("Please apply an Android plugin before applying the 'eta-android' plugin.", null));
      project.getLogger().lifecycle("Hello from Eta Android!");
      BaseExtension androidExtension = AndroidUtil.getAndroidExtension(project);
      NamedDomainObjectContainer<AndroidSourceSet>
          sourceSetsContainer = androidExtension.getSourceSets();
      sourceSetsContainer.forEach(set -> project.getLogger().lifecycle(set.getName()));
      project.getLogger().lifecycle(project.getProjectDir().getAbsolutePath());
  }

  @Override
  public void configureAfterEvaluate(Project project) {}
}
