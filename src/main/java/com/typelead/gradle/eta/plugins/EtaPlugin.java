package com.typelead.gradle.eta.plugins;

import org.gradle.api.Plugin;
import org.gradle.api.Project;

import org.gradle.api.plugins.JavaPlugin;

import com.typelead.gradle.eta.tasks.*;
import org.gradle.api.tasks.TaskContainer;
import org.gradle.api.plugins.BasePlugin;
import org.gradle.api.UnknownTaskException;

/**
 * A {@link Plugin} which sets up an Eta project.
 */
@SuppressWarnings("WeakerAccess")
public class EtaPlugin extends EtaBasePlugin implements Plugin<Project> {

    public static final String CLEAN_ETA_TASK_NAME = "cleanEta";
    public static final String INSTALL_DEPS_ETA_TASK_NAME = "installDepsEta";
    public static final String COMPILE_ETA_TASK_NAME = "compileEta";
    public static final String RUN_ETA_TASK_NAME = "runEta";
    public static final String TEST_DEPS_ETA_TASK_NAME = "installTestDepsEta";
    public static final String TEST_COMPILE_ETA_TASK_NAME = "testCompileEta";
    public static final String TEST_ETA_TASK_NAME = "testEta";

    @Override
    public void apply(Project project) {
        super.apply(project);

        project.getPlugins().apply(JavaPlugin.class);
    }

}
