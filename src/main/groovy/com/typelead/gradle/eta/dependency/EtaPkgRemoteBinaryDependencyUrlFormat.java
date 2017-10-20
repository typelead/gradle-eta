package com.typelead.gradle.eta.dependency;

public class EtaPkgRemoteBinaryDependencyUrlFormat extends AbstractRemoteBinaryDependencyUrlFormat {

    private final String repo;

    public EtaPkgRemoteBinaryDependencyUrlFormat(String repo) {
        this.repo = repo;
    }

    @Override
    String getUrlString(String version, String arch) {
        return repo + "/eta-" + version + "/binaries/" + arch + "/eta-pkg";
    }
}
