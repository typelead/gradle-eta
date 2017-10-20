package com.typelead.gradle.eta.dependency;

import com.typelead.gradle.utils.Log;
import org.gradle.api.GradleException;
import org.gradle.api.Nullable;
import org.gradle.api.Project;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public abstract class AbstractBinaryDependencyCache implements BinaryDependencyCache, Log {

    protected final String cacheDir;
    protected final String binaryName;

    AbstractBinaryDependencyCache(Project project, String binaryName) {
        this.cacheDir = project.getGradle().getGradleUserHomeDir() + "/caches/" + binaryName;
        this.binaryName = binaryName;
    }

    protected String uncheckedBinaryPath(String version) {
        return this.cacheDir + "/" + version + "/" + binaryName;
    }

    @Override
    @Nullable
    public String getBinaryPathForVersion(String version) {
        File binary = new File(uncheckedBinaryPath(version));
        if (!binary.exists()) return null;
        if (!binary.canExecute()) {
            throw new GradleException("Cached " + binaryName + " binary is not executable: " + binary.getPath());
        }
        try {
            return binary.getCanonicalPath();
        } catch (IOException e) {
            throw new GradleException("Failed to get canonical path for " + binaryName + " binary: "  + binary.getPath(), e);
        }
    }

    @Override
    @Nullable
    public String putBinaryForVersion(String version, URL url) {
        Path target = Paths.get(uncheckedBinaryPath(version));
        logger().info("Downloading " + binaryName + " from " + url + " ; caching to " + target);
        File dir = target.getParent().toFile();
        if (!dir.exists()) {
            if (!dir.mkdirs()) {
                throw new GradleException("Failed to mkdirs: " + dir.getPath());
            }
        }
        try (InputStream is = url.openStream()) {
            Files.copy(is, target);
        } catch (IOException e) {
            throw new GradleException("Failed to fetch file from url: " + url);
        }
        File result = target.toFile();
        if (!result.exists()) {
            throw new GradleException("Expected " + binaryName + " to have downloaded to cache, but failed to find at: " + result.getPath());
        }
        if (!result.setExecutable(true)) {
            throw new GradleException("Failed to set " + binaryName + " as executable: " + result.getPath());
        }
        String path;
        try {
            path = result.getCanonicalPath();
        } catch (IOException e) {
            throw new GradleException("Failed to get canonical path for: " + result.getPath());
        }
        return path;
    }
}
