package com.typelead.gradle.eta.api;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.gradle.api.Project;
import org.gradle.api.Nullable;
import org.gradle.api.GradleException;
import org.gradle.api.model.ObjectFactory;
import org.gradle.api.provider.Property;

import com.typelead.gradle.utils.ExecutableSpec;
import com.typelead.gradle.utils.EtaInfo;
import com.typelead.gradle.utils.NoSpec;
import com.typelead.gradle.utils.OverridingProperty;
import com.typelead.gradle.utils.PathSpec;
import com.typelead.gradle.utils.ResolvedExecutable;
import com.typelead.gradle.utils.SystemSpec;
import com.typelead.gradle.utils.VersionSpec;
import com.typelead.gradle.eta.plugins.EtaPlugin;
import static com.typelead.gradle.utils.PropertyParse.*;

/**
 * Configuration options for the {@link EtaPlugin}
 */
public class EtaExtension {

    private static final String DEFAULT_ETLAS_REPO =
        "http://cdnverify.eta-lang.org/eta-binaries";
    private static final boolean DEFAULT_USE_SYSTEM_ETLAS = false;
    private static final boolean DEFAULT_USE_SYSTEM_ETA   = false;

    private final Project project;

    private Property<ExecutableSpec> etaSpec;
    private Property<ExecutableSpec> etlasSpec;
    private Property<String> etlasRepository;

    private Property<ResolvedExecutable> resolvedEta;
    private Property<EtaInfo> resolvedEtaInfo;
    private Property<ResolvedExecutable> resolvedEtlas;

    private Property<Boolean> preInstallDependencies;

    public EtaExtension(final Project project) {
        this.project = project;

        final ObjectFactory objectFactory = project.getObjects();

        etaSpec = new OverridingProperty<ExecutableSpec>
            (project.provider(() -> {
                     String etaVersion = parseStringProperty(project, "version");
                     boolean useSystemEta = parseBooleanProperty(project, "useSystemEta",
                                                                 DEFAULT_USE_SYSTEM_ETA);
                     if (useSystemEta) {
                         return SystemSpec.getInstance();
                     } else if (etaVersion != null) {
                         return new VersionSpec(etaVersion);
                     } else {
                         return NoSpec.getInstance();
                     }
                 }),
             t -> t == NoSpec.getInstance(),
             objectFactory.property(ExecutableSpec.class));

        etlasSpec = new OverridingProperty<ExecutableSpec>
            (project.provider(() -> {
                    String etlasPath = parseStringProperty(project, "etlasPath");
                    String etlasVersion = parseStringProperty(project, "etlasVersion");
                    boolean useSystemEtlas =
                        parseBooleanProperty(project, "useSystemEtlas",
                                             DEFAULT_USE_SYSTEM_ETLAS);
                    if (etlasPath != null) {
                        return new PathSpec(etlasPath);
                    } else if (useSystemEtlas) {
                        return SystemSpec.getInstance();
                    } else if (etlasVersion != null) {
                        return new VersionSpec(etlasVersion);
                    } else {
                        return NoSpec.getInstance();
                    }
                }),
            Optional.of(NoSpec.getInstance()),
            objectFactory.property(ExecutableSpec.class));

        etlasRepository = new OverridingProperty<String>
            (project.provider
             (() -> parseStringProperty(project, "etlasRepository", DEFAULT_ETLAS_REPO)),
             Optional.empty(),
             objectFactory.property(String.class));

        resolvedEta     = objectFactory.property(ResolvedExecutable.class);
        resolvedEtaInfo = objectFactory.property(EtaInfo.class);
        resolvedEtlas   = objectFactory.property(ResolvedExecutable.class);

        preInstallDependencies = objectFactory.property(Boolean.class);
        preInstallDependencies.set(false);
    }


    public Property<ExecutableSpec> getEtaSpec() {
        return etaSpec;
    }

    public Property<ExecutableSpec> getEtlasSpec() {
        return etlasSpec;
    }

    public Property<ResolvedExecutable> getEta() {
        return resolvedEta;
    }

    public Property<EtaInfo> getEtaInfo() {
        return resolvedEtaInfo;
    }

    public Property<ResolvedExecutable> getEtlas() {
        return resolvedEtlas;
    }

    public Property<String> getEtlasRepository() {
        return etlasRepository;
    }

    public boolean shouldPreInstallDependencies() {
        return preInstallDependencies.get();
    }

    /* Setters so that users can conveniently specify property values. */

    public void setVersion(String etaVersion) {
        this.etaSpec.set(new VersionSpec(etaVersion));
    }

    public void setUseSystemEta(boolean useSystemEta) {
        if (useSystemEta) {
            this.etaSpec.set(SystemSpec.getInstance());
        }
    }

    public void setEtlasPath(String etlasPath) {
        this.etlasSpec.set(new PathSpec(etlasPath));
    }

    public void setEtlasVersion(String etlasVersion) {
        this.etlasSpec.set(new VersionSpec(etlasVersion));
    }

    public void setUseSystemEtlas(boolean useSystemEtlas) {
        if (useSystemEtlas) {
            this.etlasSpec.set(SystemSpec.getInstance());
        }
    }

    public void setEtlasRepository(String etlasRepository) {
        this.etlasRepository.set(etlasRepository);
    }

    public void setPreInstallDependencies(boolean preInstallDependencies) {
        this.preInstallDependencies.set(preInstallDependencies);
    }

}
