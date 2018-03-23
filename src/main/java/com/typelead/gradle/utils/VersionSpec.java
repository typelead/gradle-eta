package com.typelead.gradle.utils;

public class VersionSpec extends ExecutableSpec {
    private final String version;

    public VersionSpec(String version) {
        this.version = version;
    }

    public String getVersion() {
        return version;
    }

    @Override
    public String toString() {
        return "VersionSpec{" + version + "}";
    }
}
