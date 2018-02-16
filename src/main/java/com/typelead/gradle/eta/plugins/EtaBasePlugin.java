package com.typelead.gradle.eta.plugins;

import com.typelead.gradle.eta.config.EtaExtension;
import com.typelead.gradle.eta.dependency.EtlasBinaryDependency;
import com.typelead.gradle.eta.dependency.EtlasBinaryDependencyResolver;
import com.typelead.gradle.utils.EtlasCommand;
import org.gradle.api.GradleException;
import org.gradle.api.Plugin;
import org.gradle.api.Project;
import org.gradle.api.logging.Logger;
import org.gradle.api.logging.Logging;
import org.gradle.api.plugins.BasePlugin;
import org.gradle.api.plugins.JavaPlugin;

import java.util.Optional;
import java.io.File;
import java.nio.file.Paths;

/**
 * A {@link Plugin} which compiles and tests Eta sources.
 */
public abstract class EtaBasePlugin {

    private static final Logger LOG = Logging.getLogger(EtaBasePlugin.class);

    public static final String ETA_EXTENSION_NAME = "eta";
    public static final String TASK_GROUP_NAME = "EtaPlugin";

    public static final boolean DEFAULT_USE_SYSTEM_ETLAS = false;
    public static final String DEFAULT_ETLAS_REPO = "http://cdnverify.eta-lang.org/eta-binaries";
    public static final boolean DEFAULT_USE_SANDBOX = true;
    public static final String DEFAULT_BUILD_DIR = "build/etlas/dist";
    public static final String DEFAULT_SANDBOX_CONFIG = "cabal.sandbox.config";
    public static final String DEFAULT_ETA_MAIN_CLASS = "eta.main";

    public static final String ETA_SEND_METRICS_PROPERTY = "etaSendMetrics";

    public abstract void configureBeforeEvaluate();
    public abstract void configureAfterEvaluate();

    protected Project project;
    protected EtaExtension extension;
    protected EtlasCommand etlasCommand;
    protected EtlasBinaryDependencyResolver resolver;

    public void apply(Project project) {
        this.project   = project;
        this.extension = project.getExtensions()
            .create(EtaPlugin.ETA_EXTENSION_NAME, EtaExtension.class);
        this.resolver = new EtlasBinaryDependencyResolver(project);

        project.getPlugins().apply(BasePlugin.class);

        configureBeforeEvaluate();

        /* We must run these in an `afterEvaluate` block so that `extension` has been
           populated with the user `eta { .. }` configuration. */
        project.afterEvaluate(p -> {
                /* WARNING: The ordering of statements below is very important. */
                extension.setDefaultsFromProperties(project);

                EtlasBinaryDependency etlasDep = configureOrDownloadEtlas();

                this.etlasCommand = new EtlasCommand(project, extension);

                String etlasVersion = extension.getEtlasVersion();

                if (etlasVersion == null) {
                    etlasVersion = etlasCommand.numericVersion();
                }

                extension.setEtlasVersion(etlasVersion);
                etlasCommand.setEtlasVersion(etlasVersion);

                ensureTelemetryPreferencesAndUpdate(etlasDep, etlasCommand.getSendMetrics());

                configureAfterEvaluate();
            });
    }

    private EtlasBinaryDependency configureOrDownloadEtlas() {
        EtlasBinaryDependency etlasDep = null;
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
            LOG.info("Using etlas from system PATH: " + etlasDep.getPath());
            extension.setEtlasBinary(etlasDep.getPath());
        } else if (extension.getEtlasBinary() != null) {
            if (extension.getEtlasVersion() != null) {
                throw new GradleException("Invalid configuration, cannot set etlasVersion and etlasBinary together");
            }
            etlasDep = resolver.resolveLocalPath(extension.getEtlasBinary());
        } else if (extension.getEtlasVersion() != null) {
            if (extension.getEtlasRepo() == null) {
                throw new GradleException("etlasVersion provided, but etlasRepo was unexpectedly null!");
            }
            etlasDep = resolver.resolveRemote(extension.getEtlasRepo(), extension.getEtlasVersion());
            extension.setEtlasBinary(etlasDep.getPath());
        } else {
            throw new GradleException("Etlas not configured, please specify etlasVersion in an eta { .. } block.");
        }
        return etlasDep;
    }

    private void ensureTelemetryPreferencesAndUpdate(EtlasBinaryDependency etlasDep, Optional<Boolean> sendMetrics) {
        checkForSendMetrics(sendMetrics);
        if (etlasDep.isFresh()) {
            project.getLogger().info("Updating etlas packages via 'etlas update'");
            etlasCommand.update();
        }
    }

    private void checkForSendMetrics(Optional<Boolean> sendMetrics) {
        File etlasConfig = project.file(Paths.get(System.getProperty("user.home"), ".etlas", "config"));
        if (!etlasConfig.exists()) {
            if(!sendMetrics.isPresent()) {
                throw new GradleException(etlasCommand.getWelcomeMessage() +
                                          "\nPlease re-run this command with:\n * `-PetaSendMetrics=true` for yes\n * `-PetaSendMetrics=false` for no.\n\nThis only needs to be done once.");
            }
        }
    }
}
