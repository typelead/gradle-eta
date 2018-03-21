package com.typelead.gradle.eta.internal;

import org.gradle.api.Project;
import org.gradle.api.internal.file.FileCollectionInternal;
import org.gradle.api.internal.artifacts.dependencies.DefaultSelfResolvingDependency;

import com.typelead.gradle.eta.api.EtaDependency;

public class AbstractEtaDependency extends DefaultSelfResolvingDependency
    implements EtaDependency {

    public AbstractEtaDependency(final Project project) {
        super((FileCollectionInternal) project.files());
    }
}
