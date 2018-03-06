package com.typelead.gradle.eta.internal;

import com.typelead.gradle.eta.api.EtaGitDependency;
import com.typelead.gradle.eta.api.SourceRepository;

public class DefaultEtaGitDependency implements EtaGitDependency {

    private SourceRepository sourceRepository;

    public DefaultEtaGitDependency(SourceRepository sourceRepository) {
        this.sourceRepository = sourceRepository;
    }

    @Override
    public SourceRepository getSourceRepository() {
        return sourceRepository;
    }
}
