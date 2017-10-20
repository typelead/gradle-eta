package com.typelead.gradle.eta.dependency;

import com.typelead.gradle.utils.CommandLine;
import com.typelead.gradle.utils.SystemPathUtil;
import org.gradle.api.GradleException;
import org.gradle.api.Nullable;
import org.gradle.internal.os.OperatingSystem;

import java.io.File;
import java.io.IOException;
import java.net.URL;

public class DefaultBinaryDependencyResolver implements BinaryDependencyResolver {

    private final String name;
    private final BinaryDependencyCache cache;
    private final RemoteBinaryDependencyUrlFormat urlFormat;

    public DefaultBinaryDependencyResolver(
            String name,
            BinaryDependencyCache cache,
            RemoteBinaryDependencyUrlFormat urlFormat
    ) {
        this.name = name;
        this.cache = cache;
        this.urlFormat = urlFormat;
    }

    @Override
    @Nullable
    public BinaryDependency resolveSystemBinary() {
        File binary = SystemPathUtil.findExecutable(name);
        if (binary == null) return null;
        String path;
        try {
            path = binary.getCanonicalPath();
        } catch (IOException e) {
            throw new GradleException("Failed to get canonical path for " + name + " '" + binary.getPath() + "'", e);
        }
        return new BinaryDependency(path, getNumericVersion(path));
    }

    @Override
    public BinaryDependency resolveLocalBinary(String path) {
        File binary = new File(path);
        if (!binary.canExecute()) {
            throw new GradleException("Provided binary is not executable: " + path);
        }
        return new BinaryDependency(path, getNumericVersion(path));
    }

    @Override
    public BinaryDependency resolveRemoteBinary(String version) {
        String arch = getArch();
        String path = cache.getBinaryPathForVersion(version);
        if (path == null) {
            path = cache.putBinaryForVersion(version, urlFormat.getUrl(version, arch));
        }
        return new BinaryDependency(path, version);
    }

    private String getNumericVersion(String executable) {
        return new CommandLine(executable, "--numeric-version").executeAndGetStandardOutput().trim();
    }

    // TODO: This assumes a 64-bit architecture. If eta starts cross building for other arches
    // we'll need to detect that (e.g. on linux we can do `uname -m`)
    private String getArch() {
        OperatingSystem os = OperatingSystem.current();
        if (os.isLinux()) return "x86_64-linux";
        else if (os.isMacOsX()) return "x86_64-osx";
        else if (os.isWindows()) return "x86_64-windows";
        throw new GradleException(
                "Unsupported OS type '" + os.getName() +
                        "'; install etlas manually and configure with etlasBinary");
    }
}
