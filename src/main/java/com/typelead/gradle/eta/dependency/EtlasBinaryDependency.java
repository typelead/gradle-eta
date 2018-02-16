package com.typelead.gradle.eta.dependency;

public class EtlasBinaryDependency {

    private final String path;
    private final String version;
    private final boolean fresh;

    EtlasBinaryDependency(String path, String version) {
        this(path, version, false);
    }

    EtlasBinaryDependency(String path, String version, boolean fresh) {
        this.path    = path;
        this.version = version;
        this.fresh   = fresh;
    }

    public String getPath() {
        return path;
    }

    public String getVersion() {
        return version;
    }

    public boolean isFresh() {
        return fresh;
    }
}
