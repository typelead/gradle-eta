package com.typelead.gradle.utils;

public class VersionSpec extends ExecutableSpec {
    private String version;

    public VersionSpec(String version) {
        this.version = version;
    }

    public String getVersion() {
        return version;
    }
}
