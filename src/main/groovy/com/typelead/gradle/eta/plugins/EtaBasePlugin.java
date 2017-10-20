package com.typelead.gradle.eta.plugins;

import com.typelead.gradle.eta.config.EtaExtension;
import com.typelead.gradle.eta.dependency.*;
import com.typelead.gradle.eta.tasks.*;
import org.gradle.api.DefaultTask;
import org.gradle.api.GradleException;
import org.gradle.api.Plugin;
import org.gradle.api.Project;
import org.gradle.api.logging.Logger;
import org.gradle.api.logging.Logging;
import org.gradle.api.plugins.BasePlugin;

import java.util.function.Function;

/**
 * A {@link Plugin} which compiles and tests Eta sources.
 */
public class EtaBasePlugin implements Plugin<Project> {

    private static final Logger LOG = Logging.getLogger(EtaBasePlugin.class);

    @Override
    public void apply(Project project) {
        EtaExtension extension = project.getExtensions().create(
                EtaPlugin.ETA_EXTENSION_NAME, EtaExtension.class);
        configureConfigurations(project);
        configureEtaCleanTask(project);
        configureEtaCompileTask(project);
        configureEtaRuntimeTask(project);
        configureEtaRunTask(project);
        configureEtaTestDepsTask(project);
        configureEtaTestCompileTask(project);
        configureEtaTestTask(project);
        // We must run these in an `afterEvaluate` block so that `extension` has been
        // populated with the user `eta { .. }` configuration.
        project.afterEvaluate(p -> {
            configureBinaries(project, extension);
            configureTasksAfterEvaluate(project, extension);
            configureBaseCleanTask(p);
            configureBaseAssembleTask(p);
        });
    }

    private static void configureConfigurations(Project project) {
        project.getConfigurations().create(EtaPlugin.ETA_RUNTIME_CONFIGURATION_NAME)
                .setDescription("Configuration for Eta runtime tasks");
    }

    private static void configureBinaries(Project project, EtaExtension extension) {
        setEtaPkgDefaults(extension);
        extension.getEtlas().update(configureOrDownloadEtlas(project, extension));
        extension.getEta().update(configureOrDownloadEta(project, extension));
        extension.getEtaPkg().update(configureOrDownloadEtaPkg(project, extension));
    }

    private static BinaryDependency configureOrDownloadEtlas(Project project, EtaExtension extension) {
        BinaryDependency dep = configureOrDownloadBinary(
                "etlas",
                extension.getEtlas(),
                new DefaultBinaryDependencyResolver(
                        "etlas",
                        new EtlasBinaryDependencyCache(project),
                        new EtlasRemoteBinaryDependencyUrlFormat(extension.getEtaRepo())
                )
        );
        if (dep == null) throw new GradleException("Missing configuration for etlas");
        return dep;
    }

    private static BinaryDependency configureOrDownloadEta(Project project, EtaExtension extension) {
        return configureOrDownloadBinary(
                "eta",
                extension.getEta(),
                new DefaultBinaryDependencyResolver(
                        "eta",
                        new EtaBinaryDependencyCache(project),
                        new EtaRemoteBinaryDependencyUrlFormat(extension.getEtaRepo())
                )
        );
    }

    private static BinaryDependency configureOrDownloadEtaPkg(Project project, EtaExtension extension) {
        return configureOrDownloadBinary(
                "eta-pkg",
                extension.getEtaPkg(),
                new DefaultBinaryDependencyResolver(
                        "eta-pkg",
                        new EtaPkgBinaryDependencyCache(project),
                        new EtaPkgRemoteBinaryDependencyUrlFormat(extension.getEtaRepo())
                )
        );
    }

    /** Set the default configuration for etaPkg based on eta if it is not configured. */
    private static void setEtaPkgDefaults(EtaExtension extension) {
        if (extension.getEtaPkg().hasConfiguration()) return;
        if (extension.getEta().getVersion() != null) {
            extension.getEtaPkg().setVersion(extension.getEta().getVersion());
        } else if (extension.getEta().getUseSystem()) {
            extension.getEtaPkg().setUseSystem(true);
        }
    }

    private static BinaryDependency configureOrDownloadBinary(
            String name,
            EtaExtension.BinaryConfig binaryConfig,
            BinaryDependencyResolver resolver
    ) {
        if (binaryConfig.getUseSystem()) {
            if (binaryConfig.getPath() != null) {
                throw new GradleException(
                        "Invalid configuration, cannot set " + name + ".path and "
                        + name + ".useSystem together"
                );
            }
            if (binaryConfig.getVersion() != null) {
                throw new GradleException(
                        "Invalid configuration, cannot set " + name + ".version and "
                        + name + ".useSystem together"
                );
            }
            BinaryDependency dep = resolver.resolveSystemBinary();
            if (dep == null) throw new GradleException("Could not find " + name + " executable on system PATH");
            LOG.info("Using " + name + " version " + dep.getVersion() + " from system PATH: " + dep.getPath());
            return dep;
        } else if (binaryConfig.getPath() != null) {
            if (binaryConfig.getVersion() != null) {
                throw new GradleException(
                        "Invalid configuration, cannot set " + name + ".version and "
                        + name + ".path together"
                );
            }
            return resolver.resolveLocalBinary(binaryConfig.getPath());
        } else if (binaryConfig.getVersion() != null) {
            return resolver.resolveRemoteBinary(binaryConfig.getVersion());
        } else {
            return null;
        }
    }

    private static void configureTasksAfterEvaluate(Project project, EtaExtension extension) {
        project.getTasks().forEach(t -> {
            if (t instanceof EtlasTaskSpec) {
                EtlasTaskSpec task = (EtlasTaskSpec) t;
                task.setEtlasBinary(extension.getEtlas().getPath());
                task.setEtaBinary(extension.getEta().getPath());
                task.setEtaPkgBinary(extension.getEtaPkg().getPath());
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

    private static <A extends DefaultTask> A createEtaTask(
            Project project,
            String taskName,
            Class<A> taskClass,
            String description
    ) {
        A task = project.getTasks().create(taskName, taskClass);
        task.setGroup(EtaPlugin.TASK_GROUP_NAME);
        task.setDescription(description);
        return task;
    }

    private static void configureEtaCleanTask(Project project) {
        createEtaTask(
                project, EtaPlugin.CLEAN_ETA_TASK_NAME, EtaClean.class,
                "Clean Eta build artifacts via 'etlas clean'"
        );
    }

    private static void configureEtaCompileTask(Project project) {
        createEtaTask(
                project, EtaPlugin.COMPILE_ETA_TASK_NAME, EtaCompile.class,
                "Compile Eta sources via 'etlas build'"
        );
    }

    private static void configureEtaRuntimeTask(Project project) {
        createEtaTask(
                project, EtaPlugin.RUNTIME_ETA_TASK_NAME, EtaRuntime.class,
                "Set Eta runtime dependency via 'etlas deps --classpath'"
        );
    }

    private static void configureEtaRunTask(Project project) {
        createEtaTask(
                project, EtaPlugin.RUN_ETA_TASK_NAME, EtaRun.class,
                "Run a compiled Eta executable"
        ).dependsOn(
                project.getTasks().getByName(EtaPlugin.COMPILE_ETA_TASK_NAME),
                project.getTasks().getByName(EtaPlugin.RUNTIME_ETA_TASK_NAME)
        );
    }

    private static void configureEtaTestDepsTask(Project project) {
        createEtaTask(
                project, EtaPlugin.TEST_DEPS_ETA_TASK_NAME, EtaInstallTestDeps.class,
                "Install dependencies for Eta tests"
        );
    }

    private static void configureEtaTestCompileTask(Project project) {
        createEtaTask(
                project, EtaPlugin.TEST_COMPILE_ETA_TASK_NAME, EtaTestCompile.class,
                "Compiles Eta test sources via 'etlas build'"
        ).dependsOn(project.getTasks().getByName(EtaPlugin.COMPILE_ETA_TASK_NAME));
    }

    private static void configureEtaTestTask(Project project) {
        createEtaTask(
                project, EtaPlugin.TEST_ETA_TASK_NAME, EtaTest.class,
                "Run Eta tests"
        ).dependsOn(project.getTasks().getByName(EtaPlugin.TEST_COMPILE_ETA_TASK_NAME));
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
