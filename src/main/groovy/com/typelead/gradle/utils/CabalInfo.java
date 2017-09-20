package com.typelead.gradle.utils;

import org.gradle.api.GradleException;
import org.gradle.api.Project;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

public class CabalInfo {

    public static CabalInfo get(Project project) {
        File[] arr = project.getRootDir().listFiles();
        if (arr == null) {
            throw new GradleException("Project root is unexpectedly empty " + project.getRootDir());
        }
        List<File> cabalFiles = Arrays.stream(arr)
                .filter(f -> f.getName().endsWith(".cabal"))
                .collect(Collectors.toList());
        if (cabalFiles.size() == 0) {
            throw new GradleException("Could not find cabal file in project root " + project.getRootDir());
        }
        if (cabalFiles.size() > 1) {
            throw new GradleException(
                    "Found more than one cabal file in project root "
                            + project.getRootDir() + " " + cabalFiles);
        }
        File cabalFile = cabalFiles.get(0);
        List<String> lines;
        try {
            lines = Files.readAllLines(cabalFiles.get(0).toPath());
        } catch (IOException e) {
            throw new GradleException("Failed to read cabal file " + cabalFile);
        }
        return CabalInfoParser.parse(lines);
    }

    private final String name;
    private final boolean hasLibrary;
    private final List<String> executableNames;
    private final List<String> testNames;
    private final List<String> benchmarkNames;

    CabalInfo(String name,
              boolean hasLibrary,
              List<String> executableNames,
              List<String> testNames,
              List<String> benchmarkNames) {
        this.name = name;
        this.hasLibrary = hasLibrary;
        this.executableNames = executableNames;
        this.testNames = testNames;
        this.benchmarkNames = benchmarkNames;
    }

    public List<String> getProductionComponentNames() {
        List<String> result = new ArrayList<>();
        if (hasLibrary) result.add("lib:" + name);
        for (String exe : executableNames) {
            result.add("exe:" + exe);
        }
        return result;
    }

    public List<String> getTestComponentNames() {
        List<String> result = new ArrayList<>();
        for (String test : testNames) {
            result.add("test:" + test);
        }
        return result;
    }

    public String getName() {
        return name;
    }

    public boolean hasLibrary() {
        return hasLibrary;
    }

    public List<String> getExecutableNames() {
        return executableNames;
    }

    public List<String> getTestNames() {
        return testNames;
    }

    public List<String> getBenchmarkNames() {
        return benchmarkNames;
    }
}
