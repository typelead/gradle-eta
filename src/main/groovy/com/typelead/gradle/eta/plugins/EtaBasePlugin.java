package com.typelead.gradle.eta.plugins;

import com.typelead.gradle.eta.config.EtaExtension;
import com.typelead.gradle.eta.dependency.EtlasBinaryDependency;
import com.typelead.gradle.eta.dependency.EtlasBinaryDependencyResolver;
import com.typelead.gradle.eta.tasks.*;
import com.typelead.gradle.utils.EtlasCommand;
import org.gradle.api.GradleException;
import org.gradle.api.Plugin;
import org.gradle.api.Project;
import org.gradle.api.artifacts.DependencySet;
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
        EtaExtension extension = project.getExtensions().create(
                EtaPlugin.ETA_EXTENSION_NAME, EtaExtension.class);
        configureConfigurations(project);
        // We must run these in an `afterEvaluate` block so that `extension` has been
        // populated with the user `eta { .. }` configuration.
        project.afterEvaluate(p -> {
            configureOrDownloadEtlas(project, extension);
            resolveDependencies(project, extension);
            configureEtaCleanTask(p, extension);
            configureEtaCompileTask(p, extension);
            configureEtaRunTask(p, extension);
            configureEtaTestDepsTask(p, extension);
            configureEtaTestCompileTask(p, extension);
            configureEtaTestTask(p, extension);
            configureBaseCleanTask(p);
            configureBaseAssembleTask(p);
        });
    }

    private static void configureConfigurations(Project project) {
        project.getConfigurations().create(EtaPlugin.ETA_RUNTIME_CONFIGURATION_NAME)
                .setDescription("Configuration for Eta runtime tasks");
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

    private static void resolveDependencies(Project project, EtaExtension extension) {
        EtlasCommand c = new EtlasCommand(project, extension);
        c.maybeInitSandbox();

        // We must install dependencies before we can call `etlas deps`
        // TODO: There needs to be a better way since we don't want to have to do this
        // every time gradle is invoked, e.g. `gradle clean` doesn't need to do this.
        c.installDependenciesOnly();

        DependencySet deps =
                project.getConfigurations()
                        .getByName(EtaPlugin.ETA_RUNTIME_CONFIGURATION_NAME)
                        .getDependencies();

        for (String dep : c.depsMaven()) {
            deps.add(project.getDependencies().create(dep));
            project.getDependencies().add(
                    EtaPlugin.ETA_RUNTIME_CONFIGURATION_NAME,
                    dep
            );
        }
        for (String dep : c.depsClasspath()) {
            deps.add(
                    project.getDependencies().add(
                            EtaPlugin.ETA_RUNTIME_CONFIGURATION_NAME,
                            project.files(dep)
                    )
            );
        }
    }

    private static <A extends EtlasTaskSpec> A configureEtlasTask(
            Project project, EtaExtension extension, Class<A> cls, String taskName) {
        A task = project.getTasks().create(taskName, cls);
        task.setEtlasBinary(extension.getEtlasBinary());
        task.setGroup(EtaPlugin.TASK_GROUP_NAME);
        task.setUseSandbox(extension.getUseSandbox());
        task.setSandboxConfig(extension.getSandboxConfig());
        task.setDefaultUserConfig(extension.getDefaultUserConfig());
        task.setEtlasFlags(extension.getEtlasFlags());
        task.setBuildFlags(extension.getBuildFlags());
        task.setBuildDir(extension.getBuildDir());
        return task;
    }

    private static void configureEtaCleanTask(Project project, EtaExtension extension) {
        EtaClean task = configureEtlasTask(project, extension, EtaClean.class, EtaPlugin.CLEAN_ETA_TASK_NAME);
        task.setDescription("Clean Eta build artifacts via 'etlas clean'");
    }

    private static void configureEtaCompileTask(Project project, EtaExtension extension) {
        EtaCompile task = configureEtlasTask(project, extension, EtaCompile.class, EtaPlugin.COMPILE_ETA_TASK_NAME);
        task.setDescription("Compile Eta sources via 'etlas build'");
    }

    private static void configureEtaRunTask(Project project, EtaExtension extension) {
        EtaRun task = configureEtlasTask(project, extension, EtaRun.class, EtaPlugin.RUN_ETA_TASK_NAME);
        task.setDescription("Run a compiled Eta executable");
        task.dependsOn(project.getTasks().getByName(EtaPlugin.COMPILE_ETA_TASK_NAME));
    }

    private static void configureEtaTestDepsTask(Project project, EtaExtension extension) {
        EtaInstallTestDeps task = configureEtlasTask(project, extension, EtaInstallTestDeps.class, EtaPlugin.TEST_DEPS_ETA_TASK_NAME);
        task.setDescription("Install dependencies for Eta tests");
    }

    private static void configureEtaTestCompileTask(Project project, EtaExtension extension) {
        EtaTestCompile task = configureEtlasTask(project, extension, EtaTestCompile.class, EtaPlugin.TEST_COMPILE_ETA_TASK_NAME);
        task.setDescription("Compiles Eta test sources via 'etlas build'");
    }

    private static void configureEtaTestTask(Project project, EtaExtension extension) {
        EtaTest task = configureEtlasTask(project, extension, EtaTest.class, EtaPlugin.TEST_ETA_TASK_NAME);
        task.setDescription("Run Eta tests");
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
