package com.typelead.gradle.eta.tasks;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.gradle.api.DefaultTask;
import org.gradle.api.provider.Property;

import com.typelead.gradle.utils.ResolvedExecutable;
import com.typelead.gradle.utils.EtlasCommand;
import com.typelead.gradle.eta.api.EtaExtension;

public abstract class AbstractEtlasTask extends DefaultTask implements EtlasTaskSpec {

    protected final EtaExtension extension;
    protected final Property<ResolvedExecutable> resolvedEta;
    protected final Property<ResolvedExecutable> resolvedEtlas;
    protected final Property<String> buildDirectory;
    protected final EtlasCommand etlas;

    public AbstractEtlasTask() {
        this.extension =
            getProject().getExtensions().getByType(EtaExtension.class);
        this.resolvedEta    = extension.getEta();
        this.resolvedEtlas  = extension.getEtlas();
        this.buildDirectory = extension.getBuildDirectory();
        this.etlas          = new EtlasCommand(this);
    }

    public Property<ResolvedExecutable> getEta() {
        return resolvedEta;
    }

    public Property<ResolvedExecutable> getEtlas() {
        return resolvedEtlas;
    }

    public Property<String> getBuildDirectory() {
        return buildDirectory;
    }

    public void setBuildDirectory(String buildDirectory) {
        this.buildDirectory.set(buildDirectory);
    }
}
