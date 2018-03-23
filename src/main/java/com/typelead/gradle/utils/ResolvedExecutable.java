package com.typelead.gradle.utils;

public class ResolvedExecutable {
    private String  path;
    private String  version;
    private boolean system;
    private boolean fresh;

    public ResolvedExecutable(String path) {
        this(path, null);
    }

    public ResolvedExecutable(String path, String version) {
        this(path, version, false, false);
    }

    public ResolvedExecutable(String path, String version, boolean system) {
        this(path, version, system, false);
    }

    public ResolvedExecutable(String path, String version, boolean system,
                              boolean fresh) {
        this.path    = path;
        this.version = version;
        this.system  = system;
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

    public void setVersion(String version) {
        this.version = version;
    }

    public boolean isSystem() {
        return system;
    }

    @Override
    public String toString() {
        return "ResolvedExecutable{path=" + nonNullString(path)
            + ",version=" + nonNullString(version)
            + ",system=" + system
            + ",fresh=" + fresh + "}";
    }

    private static String nonNullString(String s) {
      return s == null? "" : s;
    }
}
