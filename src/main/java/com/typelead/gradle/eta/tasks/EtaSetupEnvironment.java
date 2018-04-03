package com.typelead.gradle.eta.tasks;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.LinkedHashSet;
import java.util.Set;
import java.nio.file.Paths;

import javax.inject.Inject;

import org.gradle.api.Project;
import org.gradle.api.GradleException;
import org.gradle.api.DefaultTask;
import org.gradle.api.tasks.Input;
import org.gradle.api.tasks.Internal;
import org.gradle.api.tasks.Optional;
import org.gradle.api.tasks.OutputFile;
import org.gradle.api.tasks.TaskAction;
import org.gradle.api.provider.Property;
import org.gradle.api.provider.Provider;
import org.gradle.api.provider.ProviderFactory;

import com.typelead.gradle.utils.EtlasCommand;
import com.typelead.gradle.utils.EtaInfo;
import com.typelead.gradle.utils.ExecutableSpec;
import com.typelead.gradle.utils.IOUtils;
import com.typelead.gradle.utils.FileUtils;
import com.typelead.gradle.utils.NoSpec;
import com.typelead.gradle.utils.PathSpec;
import com.typelead.gradle.utils.PrintHelper;
import com.typelead.gradle.utils.ResolvedExecutable;
import com.typelead.gradle.utils.SystemSpec;
import com.typelead.gradle.utils.SnapshotUtils;
import com.typelead.gradle.utils.VersionSpec;
import static com.typelead.gradle.utils.PrintHelper.*;
import static com.typelead.gradle.eta.plugins.EtaBasePlugin.*;
import com.typelead.gradle.eta.api.EtaExtension;
import com.typelead.gradle.eta.plugins.EtaBasePlugin;
import com.typelead.gradle.eta.internal.EtlasResolver;

public class EtaSetupEnvironment extends DefaultTask {

    public static final String INFO_FILENAME = "eta-info";
    public static final String SNAPSHOT_FILENAME = "eta-versions-snapshot";

    private final Property<ResolvedExecutable> resolvedEta;
    private final Property<EtaInfo> resolvedEtaInfo;
    private final Property<ResolvedExecutable> resolvedEtlas;
    private final Property<Boolean> versionsChanged;
    private Provider<ExecutableSpec> etaSpec;
    private Provider<ExecutableSpec> etlasSpec;
    private Provider<String> etlasRepository;

    public EtaSetupEnvironment() {
        final Project project = getProject();
        final EtaExtension extension =
            project.getRootProject().getExtensions().getByType(EtaExtension.class);
        this.resolvedEta     = extension.getEta();
        this.resolvedEtaInfo = extension.getEtaInfo();
        this.resolvedEtlas   = extension.getEtlas();

        this.etaSpec   = extension.getEtaSpec();
        this.etlasSpec = extension.getEtlasSpec();
        this.etlasRepository = extension.getEtlasRepository();
        this.versionsChanged = project.getObjects().property(Boolean.class);

        getOutputs().upToDateWhen(task -> false);

        setDescription
            ("Setup the Eta & Etlas environment for the specified versions.");
    }


    @Input
    public String getEtaSpec() {
        return etaSpec.get().toString();
    }

    @Input
    public String getEtlasSpec() {
        return etlasSpec.get().toString();
    }

    @Input
    public String getEtlasRepository() {
        return etlasRepository.get();
    }

    @OutputFile
    public File getVersionsSnapshot() {
        return getProject().getLayout().getBuildDirectory()
            .dir(ETA_INTERMEDIATES_DIRECTORY).get().file(SNAPSHOT_FILENAME).getAsFile();
    }

    @Internal
    private String getCacheDir() {
        return getProject().getGradle().getGradleUserHomeDir() + "/caches/etlas";
    }

    @Internal
    public Provider<Boolean> getVersionsChanged() {
        return versionsChanged;
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

        boolean changed = SnapshotUtils.takeSnapshotAndCompare
            (getVersionsSnapshot(), etaExec, etlasExec,
             getEtaSpec(), getEtlasSpec());

        resolvedEtaInfo.set(fetchEtaInfo(etlas, etaExec, changed));

        versionsChanged.set(Boolean.valueOf(changed));

        setDidWork(changed);
    }

    private ResolvedExecutable resolveEtlas() {

        EtlasResolver resolver = new EtlasResolver(getCacheDir());

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

            String etlasRepo = getEtlasRepository();

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

    private EtaInfo fetchEtaInfo(EtlasCommand etlas, ResolvedExecutable eta,
                                 boolean changed) {
        File infoFile = getProject().getLayout().getBuildDirectory()
            .dir(ETA_INTERMEDIATES_DIRECTORY).get().file(INFO_FILENAME).getAsFile();

        List<String> lines;

        if (!infoFile.exists() || changed) {
            lines = etlas.getLanguagesAndExtensions();
            final StringBuilder sb = new StringBuilder();
            for (String line: lines) {
                println(sb, line);
            }
            FileUtils.write(infoFile, sb.toString());
        } else {
            try {
                lines = new ArrayList<String>();
                BufferedReader in = null;
                try {
                    in = new BufferedReader(new FileReader(infoFile));
                    String line = null;
                    while ((line = in.readLine()) != null) {
                        lines.add(line);
                    }
                } finally {
                    if (in != null) {
                        in.close();
                    }
                }
            } catch (IOException io) {
                throw new GradleException
                    ("fetchEtaInfo: While reading " + infoFile.getPath() +
                     ", encountered an IOException" , io);
            }
        }

        Set<String> validLanguages  = new LinkedHashSet<String>();
        Set<String> validExtensions = new LinkedHashSet<String>();

        Iterator<String> it = lines.iterator();
        validLanguages.add(it.next());
        validLanguages.add(it.next());

        while (it.hasNext()) {
            validExtensions.add(it.next());
        }

        return new EtaInfo(PrintHelper.friendlyVersion(eta.getVersion()),
                           validLanguages, validExtensions);
    }

    private static final String NEWLINE = System.lineSeparator();
}
