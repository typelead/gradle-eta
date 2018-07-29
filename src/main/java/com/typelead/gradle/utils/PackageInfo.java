package com.typelead.gradle.utils;

import java.io.File;

import java.util.List;

public class PackageInfo {
    private final String name;
    private String version;
    private String hash;
    private final String jarPath;
    private final List<String> mavenDependencies;

    public PackageInfo(String name, String jarPath, List<String> mavenDependencies) {
        this.name              = name;
        this.jarPath           = jarPath;
        this.mavenDependencies = mavenDependencies;
        initVersionAndHash();
    }

    private void initVersionAndHash() {
        if (jarPath == null || jarPath.length() == 0) {
            this.version = "0.0.0";
            this.hash = "inplace";
        } else {
            int index = jarPath.lastIndexOf(File.separator);
            String fileName = jarPath.substring(index + 1);
            String baseName = fileName.substring(0, fileName.length() - 4);
            String[] parts = baseName.split("-");
            int len = parts.length;
            this.version = parts[len - 2];
            this.hash    = parts[len - 1];
        }
    }

    public String getName() {
        return name;
    }

    public String getVersion() {
        return version;
    }

    public String getHash() {
        return hash;
    }

    public String getJarPath() {
        return jarPath;
    }

    public List<String> getMavenDependencies() {
        return mavenDependencies;
    }

    public String getFullVersion() {
        return getVersion() + "-" + getHash();
    }

    public String getIdentifier() {
        return getName() + "-" + getFullVersion();
    }
}
