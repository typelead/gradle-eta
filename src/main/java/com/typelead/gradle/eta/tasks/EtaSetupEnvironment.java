package com.typelead.gradle.eta.tasks;

import java.io.File;
import java.nio.file.Paths;

import javax.inject.Inject;

import org.gradle.api.GradleException;
import org.gradle.api.DefaultTask;
import org.gradle.api.tasks.Input;
import org.gradle.api.tasks.Optional;
import org.gradle.api.tasks.OutputFile;
import org.gradle.api.tasks.TaskAction;
import org.gradle.api.provider.Property;
import org.gradle.api.provider.Provider;
import org.gradle.api.provider.ProviderFactory;

import com.typelead.gradle.utils.EtlasCommand;
import com.typelead.gradle.utils.ExecutableSpec;
import com.typelead.gradle.utils.NoSpec;
import com.typelead.gradle.utils.PathSpec;
import com.typelead.gradle.utils.PrintHelper;
import com.typelead.gradle.utils.ResolvedExecutable;
import com.typelead.gradle.utils.SystemSpec;
import com.typelead.gradle.utils.VersionSpec;
import com.typelead.gradle.eta.api.EtaExtension;
import com.typelead.gradle.eta.plugins.EtaBasePlugin;
import com.typelead.gradle.eta.internal.EtlasResolver;

public class EtaSetupEnvironment extends DefaultTask {

    private final Property<ResolvedExecutable> resolvedEta;
    private final Property<ResolvedExecutable> resolvedEtlas;
    private Provider<ExecutableSpec> etaSpec;
    private Provider<ExecutableSpec> etlasSpec;
    private Provider<String> etlasRepository;
    private Provider<File> resolvedEtlasPath;

    public EtaSetupEnvironment() {
        final EtaExtension extension =
            getProject().getRootProject().getExtensions().getByType(EtaExtension.class);
        this.resolvedEta = extension.getEta();
        this.resolvedEtlas = extension.getEtlas();

        this.etaSpec   = extension.getEtaSpec();
        this.etlasSpec = extension.getEtlasSpec();
        this.etlasRepository = extension.getEtlasRepository();
        this.resolvedEtlasPath = getProject()
            .provider(() -> new File(resolvedEtlas.get().getPath()));

        setDescription
            ("Setup the Eta & Etlas environment for the specified versions.");
    }


    @Input
    public Provider<ExecutableSpec> getEtaSpec() {
        return etaSpec;
    }

    @Input
    public Provider<ExecutableSpec> getEtlasSpec() {
        return etlasSpec;
    }

    @Input
    public Provider<String> getEtlasRepository() {
        return etlasRepository;
    }

    @OutputFile
    public Provider<File> resolvedEtlasPath() {
        return resolvedEtlasPath;
    }

    @TaskAction
    public void setupEnvironment() {

        EtlasCommand etlas = new EtlasCommand(getProject());

        ResolvedExecutable etlasExec = resolveEtlas();

        /* This is a bootstrap step since we need to populate
           this property in order for the following etlas
           invocation to work. */
        resolvedEtlas.set(etlasExec);

        ensureTelemetryPreferencesAndUpdate(etlas);

        if (etlasExec.getVersion() == null) {

            etlasExec.setVersion(etlas.numericVersion());

        }

        ResolvedExecutable etaExec = resolveEta(etlas);

        resolvedEta.set(etaExec);

        if (etaExec.isFresh()) {
            getProject().getLogger().lifecycle
                ("Installing Eta v" + etaExec.getVersion());

            etlas.installEta();
        }

        setDidWork(etlasExec.isFresh() || etaExec.isFresh());
    }

    private ResolvedExecutable resolveEtlas() {

        EtlasResolver resolver =
            new EtlasResolver(getProject().getGradle().getGradleUserHomeDir() +
                              "/caches/etlas");

        ExecutableSpec spec = etlasSpec.get();

        ResolvedExecutable resolvedEtlas = null;

        if (spec instanceof NoSpec) {

            throw new GradleException
                ("Etlas not configured, please supply a value for the 'etlasVersion'"
               + " property in an eta { .. } block.");

        } else if (spec instanceof SystemSpec) {

            resolvedEtlas = resolver.resolveInSystemPath();

            getProject().getLogger()
                .info("Using etlas from system PATH: " + resolvedEtlas.getPath());

        } else if (spec instanceof PathSpec) {

            resolvedEtlas = resolver.resolveLocalPath(((PathSpec) spec).getPath());

        } else if (spec instanceof VersionSpec) {

            String etlasRepo = etlasRepository.get();

            if (etlasRepo == null) {

                throw new GradleException
                    ("etlasVersion provided, but etlasRepo was unexpectedly null!");

            }

            resolvedEtlas = resolver.resolveRemote(etlasRepo,
                                                   ((VersionSpec) spec).getVersion());

        }

        return resolvedEtlas;
    }

    private ResolvedExecutable resolveEta(EtlasCommand etlas) {

        ResolvedExecutable resolvedEta;

        ExecutableSpec spec = etaSpec.get();

        boolean system = false;
        boolean fresh  = false;

        String message = null;

        String etaVersion = null;

        if (spec instanceof NoSpec) {

            message    = "latest available version";
            etaVersion = etlas.getLatestEtaVersion();

        } else if (spec instanceof SystemSpec) {

            system     = true;
            message    = "version available on your system";
            etaVersion = etlas.getGlobalEtaVersion();

        } else if (spec instanceof PathSpec) {

            throw new GradleException
                ("etaSpec should never have a value PathSpec");

        } else if (spec instanceof VersionSpec) {

            final String friendlyEtaVersion = ((VersionSpec) spec).getVersion();
            etaVersion = PrintHelper.machineVersion(friendlyEtaVersion);
            fresh = !etlas.getInstalledEtaVersions().contains(friendlyEtaVersion);

        }

        if (message != null) {

            getProject().getLogger().lifecycle
                ("WARNING: You have not explicitly set the version of Eta to be used, "
                 + "so the " + message + ", " + PrintHelper.friendlyVersion(etaVersion)
                 + ", will be used." + NEWLINE + NEWLINE
                 + "This is not recommended since it will make this build "
                 + "non-reproducible. Please supply a value for the 'version' property "
                 + "in an eta { .. } block.");

        }

        return new ResolvedExecutable(null, etaVersion, system, fresh);

    }

    private void ensureTelemetryPreferencesAndUpdate(EtlasCommand etlas) {

        File etlasConfig = getProject().file(Paths.get(System.getProperty("user.home"),
                                                       ".etlas", "config"));

        if (!etlasConfig.exists()) {

            if(!etlas.getSendMetrics().isPresent()) {

                throw new GradleException
                    (etlas.getWelcomeMessage()
                   + NEWLINE
                   + "Please re-run this command with:" + NEWLINE
                   + " * `-PetaSendMetrics=true` for yes" + NEWLINE
                   + " * `-PetaSendMetrics=false` for no." + NEWLINE
                   + NEWLINE
                   + "This only needs to be done once.");

            }
        }

        if (resolvedEtlas.get().isFresh()) {

            getProject().getLogger().info
                ("Updating etlas packages via 'etlas update'.");

            etlas.update();

        }
    }

    private static final String NEWLINE = System.lineSeparator();
}
