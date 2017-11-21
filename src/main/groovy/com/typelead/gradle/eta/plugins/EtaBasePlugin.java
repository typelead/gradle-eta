package com.typelead.gradle.eta.plugins;

import com.typelead.gradle.eta.config.EtaExtension;
import com.typelead.gradle.eta.dependency.EtlasBinaryDependency;
import com.typelead.gradle.eta.dependency.EtlasBinaryDependencyResolver;
import com.typelead.gradle.eta.tasks.*;
import org.gradle.api.GradleException;
import org.gradle.api.Plugin;
import org.gradle.api.Project;
import org.gradle.api.logging.Logger;
import org.gradle.api.logging.Logging;
import org.gradle.api.plugins.BasePlugin;

/**
 * A {@link Plugin} which compiles and tests Eta sources.
 */
public class EtaBasePlugin implements Plugin<Project> {

    private static final Logger LOG = Logging.getLogger(EtaBasePlugin.class);

    @Override
    public void apply(Project project) {
        configureConfigurations(project);
        configureEtaCleanTask(project);
        configureEtaCompileTask(project);
        configureEtaRuntimeTask(project);
        configureEtaRunTask(project);
        configureEtaTestDepsTask(project);
        configureEtaTestCompileTask(project);
        configureEtaTestTask(project);
        EtaExtension extension = project.getExtensions().create(
                EtaPlugin.ETA_EXTENSION_NAME, EtaExtension.class);
        // We must run these in an `afterEvaluate` block so that `extension` has been
        // populated with the user `eta { .. }` configuration.
        project.afterEvaluate(p -> {
            configureExtensionFromProperties(project, extension);
            configureOrDownloadEtlas(project, extension);
            configureTasksAfterEvaluate(project, extension);
            configureBaseCleanTask(p);
            configureBaseAssembleTask(p);
        });
    }

    private static void configureConfigurations(Project project) {
        project.getConfigurations().create(EtaPlugin.ETA_RUNTIME_CONFIGURATION_NAME)
                .setDescription("Configuration for Eta runtime tasks");
    }

    private static void configureExtensionFromProperties(Project project, EtaExtension extension) {
        extension.setDefaultsFromProperties(project);
    }

    private static void configureOrDownloadEtlas(Project project, EtaExtension extension) {
        EtlasBinaryDependencyResolver resolver = new EtlasBinaryDependencyResolver(project);
        EtlasBinaryDependency etlasDep;
        if (extension.getUseSystemEtlas()) {
            if (extension.getEtlasBinary() != null) {
                throw new GradleException("Invalid configuration, cannot set etlasBinary and useSystemEtlas together");
            }
            if (extension.getEtlasVersion() != null) {
                throw new GradleException("Invalid configuration, cannot set etlasVersion and useSystemEtlas together");
            }
            etlasDep = resolver.resolveInSystemPath();
            if (etlasDep == null) {
                throw new GradleException("Could not find etlas executable on system PATH");
            }
            LOG.info("Using etlas " + etlasDep.getVersion() + " from system PATH: " + etlasDep.getPath());
            extension.setEtlasBinary(etlasDep.getPath());
            extension.setEtlasVersion(etlasDep.getVersion());
        } else if (extension.getEtlasBinary() != null) {
            if (extension.getEtlasVersion() != null) {
                throw new GradleException("Invalid configuration, cannot set etlasVersion and etlasBinary together");
            }
            etlasDep = resolver.resolveLocalPath(extension.getEtlasBinary());
            extension.setEtlasVersion(etlasDep.getVersion());
        } else if (extension.getEtlasVersion() != null) {
            if (extension.getEtlasRepo() == null) {
                throw new GradleException("etlasVersion provided, but etlasRepo was unexpectedly null!");
            }
            etlasDep = resolver.resolveRemote(extension.getEtlasRepo(), extension.getEtlasVersion());
            extension.setEtlasBinary(etlasDep.getPath());
        } else {
            throw new GradleException("Etlas not configured, please specify etlasVersion in an eta { .. } block.");
        }
    }

    private static void configureTasksAfterEvaluate(Project project, EtaExtension extension) {
        project.getTasks().forEach(t -> {
            if (t instanceof EtlasTaskSpec) {
                EtlasTaskSpec task = (EtlasTaskSpec) t;
                task.setEtlasBinary(extension.getEtlasBinary());
                task.setGroup(EtaPlugin.TASK_GROUP_NAME);
                task.setUseSandbox(extension.getUseSandbox());
                task.setSandboxConfig(extension.getSandboxConfig());
                task.setDefaultUserConfig(extension.getDefaultUserConfig());
                task.setEtlasFlags(extension.getEtlasFlags());
                task.setBuildFlags(extension.getBuildFlags());
                task.setBuildDir(extension.getBuildDir());
            }
        });
    }

    private static void configureEtaCleanTask(Project project) {
        EtaClean task = project.getTasks().create(EtaPlugin.CLEAN_ETA_TASK_NAME, EtaClean.class);
        task.setDescription("Clean Eta build artifacts via 'etlas clean'");
    }

    private static void configureEtaCompileTask(Project project) {
        EtaCompile task = project.getTasks().create(EtaPlugin.COMPILE_ETA_TASK_NAME, EtaCompile.class);
        task.setDescription("Compile Eta sources via 'etlas build'");
    }

    private static void configureEtaRuntimeTask(Project project) {
        EtaRuntime task = project.getTasks().create(EtaPlugin.RUNTIME_ETA_TASK_NAME, EtaRuntime.class);
        task.setDescription("Set Eta runtime dependency via 'etlas deps --classpath'");
    }

    private static void configureEtaRunTask(Project project) {
        EtaRun task = project.getTasks().create(EtaPlugin.RUN_ETA_TASK_NAME, EtaRun.class);
        task.setDescription("Run a compiled Eta executable");
        task.dependsOn(
                project.getTasks().getByName(EtaPlugin.COMPILE_ETA_TASK_NAME),
                project.getTasks().getByName(EtaPlugin.RUNTIME_ETA_TASK_NAME)
        );
    }

    private static void configureEtaTestDepsTask(Project project) {
        EtaInstallTestDeps task = project.getTasks().create(EtaPlugin.TEST_DEPS_ETA_TASK_NAME, EtaInstallTestDeps.class);
        task.setDescription("Install dependencies for Eta tests");
    }

    private static void configureEtaTestCompileTask(Project project) {
        EtaTestCompile task = project.getTasks().create(EtaPlugin.TEST_COMPILE_ETA_TASK_NAME, EtaTestCompile.class);
        task.setDescription("Compiles Eta test sources via 'etlas build'");
        task.dependsOn(project.getTasks().getByName(EtaPlugin.COMPILE_ETA_TASK_NAME));
    }

    private static void configureEtaTestTask(Project project) {
        EtaTest task = project.getTasks().create(EtaPlugin.TEST_ETA_TASK_NAME, EtaTest.class);
        task.setDescription("Run Eta tests");
        task.dependsOn(project.getTasks().getByName(EtaPlugin.TEST_COMPILE_ETA_TASK_NAME));
    }

    /** Update the 'clean' lifecycle task to include cleaning the Eta build. */
    private static void configureBaseCleanTask(Project project) {
        project.getTasks().getByName(BasePlugin.CLEAN_TASK_NAME)
                .dependsOn(project.getTasks().getByName(EtaPlugin.CLEAN_ETA_TASK_NAME));
    }

    /** Update the 'assemble' lifecycle task to include compiling Eta sources. */
    private static void configureBaseAssembleTask(Project project) {
        project.getTasks().getByName(BasePlugin.ASSEMBLE_TASK_NAME)
                .dependsOn(project.getTasks().getByName(EtaPlugin.COMPILE_ETA_TASK_NAME));
    }
}
