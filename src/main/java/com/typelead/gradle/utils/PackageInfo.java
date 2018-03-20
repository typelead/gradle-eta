package com.typelead.gradle.utils;

import java.util.List;

public class PackageInfo {
    private final String packageName;
    private final String jarPath;
    private final List<String> mavenDependencies;

    public PackageInfo(String packageName, String jarPath, List<String> mavenDependencies) {
        this.packageName = packageName;
        this.jarPath = jarPath;
        this.mavenDependencies = mavenDependencies;
    }

    public String getPackageName() {
        return packageName;
    }

    public String getJarPath() {
        return jarPath;
    }

    public List<String> getMavenDependencies() {
        return mavenDependencies;
    }
}
