package com.typelead.gradle.utils;

public class PathSpec extends ExecutableSpec {
    private final String path;

    public PathSpec(String path) {
        this.path = path;
    }

    public String getPath() {
        return path;
    }

    @Override
    public String toString() {
        return "PathSpec{" + path + "}";
    }
}
