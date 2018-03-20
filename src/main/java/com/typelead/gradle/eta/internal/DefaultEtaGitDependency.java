package com.typelead.gradle.eta.internal;

import com.typelead.gradle.eta.api.EtaGitDependency;
import com.typelead.gradle.eta.api.SourceRepository;

public class DefaultEtaGitDependency implements EtaGitDependency {

    private final String packageName;
    private final SourceRepository sourceRepository;

    public DefaultEtaGitDependency(String packageName,
                                   SourceRepository sourceRepository) {
        this.packageName = packageName;
        this.sourceRepository = sourceRepository;
    }

    @Override
    public String getPackageName() {
        return packageName;
    }

    @Override
    public SourceRepository getSourceRepository() {
        return sourceRepository;
    }
}
