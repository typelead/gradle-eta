package com.typelead.gradle.utils;

public class PathSpec extends ExecutableSpec {
    private String path;

    public PathSpec(String path) {
        this.path = path;
    }

    public String getPath() {
        return path;
    }
}
