package com.typelead.gradle.eta.dependency;

public class EtlasRemoteBinaryDependencyUrlFormat extends AbstractRemoteBinaryDependencyUrlFormat {

    private final String repo;

    public EtlasRemoteBinaryDependencyUrlFormat(String repo) {
        this.repo = repo;
    }

    @Override
    String getUrlString(String version, String arch) {
        return repo + "/etlas-" + version + "/binaries/" + arch + "/etlas";
    }
}
