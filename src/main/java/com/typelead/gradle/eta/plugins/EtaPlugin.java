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
    public static final String SANDBOX_ADD_SOURCES_ETA_TASK_NAME = "sandboxAddSourcesEta";
    public static final String INSTALL_DEPS_ETA_TASK_NAME = "installDepsEta";
    public static final String COMPILE_ETA_TASK_NAME = "compileEta";
    public static final String RUN_ETA_TASK_NAME = "runEta";
    public static final String TEST_DEPS_ETA_TASK_NAME = "installTestDepsEta";
    public static final String TEST_COMPILE_ETA_TASK_NAME = "testCompileEta";
    public static final String TEST_ETA_TASK_NAME = "testEta";

    @Override
    public void configureBeforeEvaluate() {
        project.getPlugins().apply(JavaPlugin.class);

        // configureEtaCleanTask();
        // configureEtaSandboxInitTask();
        // configureEtaSandboxAddSourcesTask();
        // configureEtaInstallDepsTask();
        // configureEtaCompileTask();
        // configureEtaRunTask();
        // configureEtaTestDepsTask();
        // configureEtaTestCompileTask();
        // configureEtaTestTask();
    }

    @Override
    public void configureAfterEvaluate() {
        configureTasksAfterEvaluate();
        // configureBaseCleanTask();
        // configureJavaJarTask();
    }

    private void configureTasksAfterEvaluate() {
        project.getTasks().forEach(t -> {
                if (t instanceof AbstractEtlasTask) {
                    ((AbstractEtlasTask)t).configureWithExtension(extension, "default");
                }
                if (t instanceof EtaSandboxAddSources) {
                    t.dependsOn(project.getTasks().getByName(EtaBasePlugin.SANDBOX_INIT_ETA_TASK_NAME));
                }
                if (t instanceof EtaInstallDeps) {
                    t.dependsOn(project.getTasks().getByName(EtaPlugin.SANDBOX_ADD_SOURCES_ETA_TASK_NAME));
                }
                if (t instanceof EtaCompile) {
                    t.dependsOn(project.getTasks().getByName(EtaPlugin.INSTALL_DEPS_ETA_TASK_NAME));
                }
                if (t instanceof EtaRun) {
                    t.dependsOn(project.getTasks().getByName(EtaPlugin.COMPILE_ETA_TASK_NAME));
                }
                if (t instanceof EtaTestCompile) {
                    t.dependsOn(project.getTasks().getByName(EtaPlugin.COMPILE_ETA_TASK_NAME));
                }
                if (t instanceof EtaTest) {
                    t.dependsOn(project.getTasks().getByName(EtaPlugin.TEST_COMPILE_ETA_TASK_NAME));
                }
            });
    }

    private void configureEtaSandboxAddSourcesTask() {
        EtaSandboxAddSources task = project.getTasks().create(EtaPlugin.SANDBOX_ADD_SOURCES_ETA_TASK_NAME, EtaSandboxAddSources.class);
        task.setDescription("Make local packages available in the sandbox via 'etlas sandbox add-source'");
    }

    private void configureEtaInstallDepsTask() {
        EtaInstallDeps task = project.getTasks().create(EtaPlugin.INSTALL_DEPS_ETA_TASK_NAME, EtaInstallDeps.class);
        task.setDescription("Install project dependencies via 'etlas install --dependencies-only'");
    }

    private void configureEtaCompileTask() {
        EtaCompile task = project.getTasks().create(EtaPlugin.COMPILE_ETA_TASK_NAME, EtaCompile.class);
        task.setDescription("Compile Eta sources via 'etlas build'");
    }

    // private void configureEtaRunTask() {
    //     EtaRun task = project.getTasks().create(EtaPlugin.RUN_ETA_TASK_NAME, EtaRun.class);
    //     task.setDescription("Run a compiled Eta executable");
    // }

    private void configureEtaTestDepsTask() {
        EtaInstallTestDeps task = project.getTasks().create(EtaPlugin.TEST_DEPS_ETA_TASK_NAME, EtaInstallTestDeps.class);
        task.setDescription("Install dependencies for Eta tests");
    }

    private void configureEtaTestCompileTask() {
        EtaTestCompile task = project.getTasks().create(EtaPlugin.TEST_COMPILE_ETA_TASK_NAME, EtaTestCompile.class);
        task.setDescription("Compiles Eta test sources via 'etlas build'");
    }

    private void configureEtaTestTask() {
        EtaTest task = project.getTasks().create(EtaPlugin.TEST_ETA_TASK_NAME, EtaTest.class);
        task.setDescription("Run Eta tests");
    }

    /**
     * Update the 'clean' lifecycle task to include cleaning the Eta build.
     */
    private void configureBaseCleanTask() {
        final TaskContainer tc = project.getTasks();
        tc.getByName(BasePlugin.CLEAN_TASK_NAME)
          .dependsOn(tc.getByName(EtaPlugin.CLEAN_ETA_TASK_NAME));
    }

    /**
     * Update the 'jar' lifecycle task to include compiling Eta sources.
     */
    private void configureJavaJarTask() {
        final TaskContainer tc = project.getTasks();
        tc.getByName(JavaPlugin.JAR_TASK_NAME)
          .dependsOn(tc.getByName(EtaPlugin.COMPILE_ETA_TASK_NAME));
    }
}
