package com.typelead.gradle.eta.api;

import java.util.List;
import java.util.ArrayList;

import org.gradle.api.Project;
import org.gradle.api.Nullable;
import org.gradle.api.GradleException;
import org.gradle.api.model.ObjectFactory;
import org.gradle.api.provider.Property;

import com.typelead.gradle.utils.ExecutableSpec;
import com.typelead.gradle.utils.EtaInfo;
import com.typelead.gradle.utils.NoSpec;
import com.typelead.gradle.utils.PathSpec;
import com.typelead.gradle.utils.ResolvedExecutable;
import com.typelead.gradle.utils.SystemSpec;
import com.typelead.gradle.utils.VersionSpec;
import com.typelead.gradle.eta.plugins.EtaPlugin;

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

    public EtaExtension(final Project project) {
        this.project = project;

        final ObjectFactory objectFactory = project.getObjects();

        etaSpec = objectFactory.property(ExecutableSpec.class);
        etaSpec.set
            (project.provider(() -> {
                    String etaVersion = parseStringProperty("version");
                    boolean useSystemEta =
                        parseBooleanProperty("useSystemEta",
                                             DEFAULT_USE_SYSTEM_ETA);
                    if (useSystemEta) {
                        return SystemSpec.getInstance();
                    } else if (etaVersion != null) {
                        return new VersionSpec(etaVersion);
                    } else {
                        return NoSpec.getInstance();
                    }
                }));

        etlasSpec = objectFactory.property(ExecutableSpec.class);
        etlasSpec.set
            (project.provider(() -> {
                    String etlasPath = parseStringProperty("etlasPath");
                    String etlasVersion = parseStringProperty("etlasVersion");
                    boolean useSystemEtlas =
                        parseBooleanProperty("useSystemEtlas",
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
                }));

        etlasRepository = objectFactory.property(String.class);
        etlasRepository.set
            (project.provider
             (() -> parseStringProperty("etlasRepository", DEFAULT_ETLAS_REPO)));

        resolvedEta     = objectFactory.property(ResolvedExecutable.class);
        resolvedEtaInfo = objectFactory.property(EtaInfo.class);
        resolvedEtlas   = objectFactory.property(ResolvedExecutable.class);
    }


    private String parseStringProperty(final String name) {
        return parseStringProperty(name, null);
    }

    private String parseStringProperty(final String name, final String def) {
        Object v = project.findProperty("eta." + name);
        String value;
        if (v == null) {
            value = def;
        } else {
            value = v.toString();
        }
        return value;
    }

    private boolean parseBooleanProperty(final String name, final boolean def) {
        Object v = project.findProperty("eta." + name);
        boolean value;
        if (v == null) {
            value = def;
        } else {
            String booleanString = v.toString();
            if (booleanString.equalsIgnoreCase("true")) {
                value = true;
            } else if (booleanString.equalsIgnoreCase("false")) {
                value = false;
            } else throw new GradleException("Invalid property value for eta."
                                             + name + ": " + booleanString);
        }
        return value;
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

}
