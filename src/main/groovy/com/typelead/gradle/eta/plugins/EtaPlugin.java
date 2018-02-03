package com.typelead.gradle.eta.plugins;

import org.gradle.api.Plugin;
import org.gradle.api.Project;

/**
 * A {@link Plugin} which sets up an Eta project.
 */
@SuppressWarnings("WeakerAccess")
public class EtaPlugin extends EtaBasePlugin implements Plugin<Project> {

  public static final String ETA_EXTENSION_NAME = "eta";
  public static final String TASK_GROUP_NAME = "EtaPlugin";
  public static final String CLEAN_ETA_TASK_NAME = "cleanEta";
  public static final String SANDBOX_INIT_ETA_TASK_NAME = "sandboxInitEta";
  public static final String SANDBOX_ADD_SOURCES_ETA_TASK_NAME = "sandboxAddSourcesEta";
  public static final String INSTALL_DEPS_ETA_TASK_NAME = "installDepsEta";
  public static final String COMPILE_ETA_TASK_NAME = "compileEta";
  public static final String RUN_ETA_TASK_NAME = "runEta";
  public static final String TEST_DEPS_ETA_TASK_NAME = "installTestDepsEta";
  public static final String TEST_COMPILE_ETA_TASK_NAME = "testCompileEta";
  public static final String TEST_ETA_TASK_NAME = "testEta";

  public static final boolean DEFAULT_USE_SYSTEM_ETLAS = false;
  public static final String DEFAULT_ETLAS_REPO = "http://cdnverify.eta-lang.org/eta-binaries";
  public static final boolean DEFAULT_USE_SANDBOX = true;
  public static final String DEFAULT_BUILD_DIR = "build/etlas/dist";
  public static final String DEFAULT_SANDBOX_CONFIG = "cabal.sandbox.config";
  public static final String DEFAULT_ETA_MAIN_CLASS = "eta.main";

  @Override
  public void apply(Project project) {
      super.apply(project);
  }

  @Override
  public void configureAfterEvaluate(Project project) {}
}
