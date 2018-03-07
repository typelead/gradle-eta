package com.typelead.gradle.eta.internal;

import com.typelead.gradle.utils.CommandLine;
import com.typelead.gradle.utils.Log;
import com.typelead.gradle.utils.Arch;
import com.typelead.gradle.eta.plugins.EtaBasePlugin;

import org.gradle.api.GradleException;
import org.gradle.api.Nullable;
import org.gradle.api.Project;
import org.gradle.api.invocation.Gradle;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class EtlasCache implements Log {

    private final String cacheDir;

    public EtlasCache(String cacheDir) {
        this.cacheDir = cacheDir;
    }

    @Nullable
    public String getBinaryPathForVersion(String version, Arch arch) {
        File etlas = new File(uncheckedBinaryPath(version, arch));
        if (!etlas.exists()) return null;
        if (!etlas.canExecute()) {
            throw new GradleException("Cached etlas binary is not executable: " + etlas.getPath());
        }
        try {
            return etlas.getCanonicalPath();
        } catch (IOException e) {
            throw new GradleException("Failed to get canonical path for etlas binary '" + etlas.getPath() + "'", e);
        }
    }

    public String putBinaryForVersion(String version, URL url, Arch arch) {
        Path target = Paths.get(uncheckedBinaryPath(version, arch));
        logger().info("Downloading etlas from: " + url + " ; caching to " + target);
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
            throw new GradleException("Expected etlas to have downloaded to cache, but failed to find at: " + result.getPath());
        }
        if (!result.setExecutable(true)) {
            throw new GradleException("Failed to make etlas executable: " + result.getPath());
        }
        String path;
        try {
            path = result.getCanonicalPath();
        } catch (IOException e) {
            throw new GradleException("Failed to get canonical path for: " + result.getPath());
        }
        return path;
    }

    private String uncheckedBinaryPath(String version, Arch arch) {
        return this.cacheDir + "/" + version + "/etlas" + arch.execExt;
    }
}
