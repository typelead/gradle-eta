package com.typelead.gradle.eta.dependency;

import com.typelead.gradle.utils.CommandLine;
import com.typelead.gradle.utils.SystemPathUtil;
import org.gradle.api.GradleException;
import org.gradle.api.Nullable;
import org.gradle.api.Project;
import org.gradle.internal.os.OperatingSystem;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;

/**
 * Manages resolving an Etlas binary by either
 * <ul>
 *     <li>Locating one on the system PATH</li>
 *     <li>Using a configured local installation</li>
 *     <li>Downloading one from the repository</li>
 * </ul>
 */
public class EtlasBinaryDependencyResolver {

    private final Project project;

    public EtlasBinaryDependencyResolver(Project project) {
        this.project = project;
    }

    @Nullable
    public EtlasBinaryDependency resolveInSystemPath() {
        File etlas = SystemPathUtil.findExecutable("etlas");
        if (etlas == null) return null;
        String etlasPath;
        try {
            etlasPath = etlas.getCanonicalPath();
        } catch (IOException e) {
            throw new GradleException("Failed to get canonical path for etlas '" + etlas.getPath() + "'", e);
        }
        return new EtlasBinaryDependency(etlasPath, getEtlasVersion(etlasPath));
    }

    public EtlasBinaryDependency resolveLocalPath(String etlasPath) {
        File etlas = new File(etlasPath);
        if (!etlas.canExecute()) {
            throw new GradleException("Provided etlas binary is not executable: " + etlasPath);
        }
        return new EtlasBinaryDependency(etlasPath, getEtlasVersion(etlasPath));
    }

    public EtlasBinaryDependency resolveRemote(String repo, String version) {
        OperatingSystem os = OperatingSystem.current();
        String arch;
        if (os.isLinux()) arch = "x86_64-linux";
        else if (os.isMacOsX()) arch = "x86_64-osx";
        else if (os.isWindows()) arch = "x86_64-windows";
        else {
            throw new GradleException(
                    "Unsupported OS type '" + os.getName() +
                            "'; install etlas manually and configure with etlasBinary");
        }
        EtlasBinaryDependencyCache cache = new EtlasBinaryDependencyCache(project);
        String etlasPath = cache.getBinaryPathForVersion(version);
        if (etlasPath == null) {
            etlasPath = cache.putBinaryForVersion(version, getEtlasUrl(repo, version, arch));
        }
        return new EtlasBinaryDependency(etlasPath, version);
    }

    private String getEtlasVersion(String etlas) {
        return new CommandLine(etlas, "--numeric-version").executeAndGetStandardOutput();
    }

    private String getEtlasUrlString(String repo, String version, String arch) {
        return repo + "/etlas-" + version + "/binaries/" + arch + "/etlas";
    }

    private URL getEtlasUrl(String repo, String version, String arch) {
        try {
            return new URL(getEtlasUrlString(repo, version, arch));
        } catch (MalformedURLException e) {
            throw new GradleException("Malformed etlasRepo '" + repo + "'", e);
        }
    }
}
