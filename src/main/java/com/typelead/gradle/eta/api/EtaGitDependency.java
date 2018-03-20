package com.typelead.gradle.eta.api;

public interface EtaGitDependency extends EtaDependency, HasPackageName {
    SourceRepository getSourceRepository();
}
