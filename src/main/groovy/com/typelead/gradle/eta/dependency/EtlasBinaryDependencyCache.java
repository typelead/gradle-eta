package com.typelead.gradle.eta.dependency;

import com.typelead.gradle.utils.CommandLine;
import org.gradle.api.GradleException;
import org.gradle.api.Project;

import java.io.File;
import java.net.URL;

public class EtlasBinaryDependencyCache extends AbstractBinaryDependencyCache {

    public EtlasBinaryDependencyCache(Project project) {
        super(project, "etlas");
    }

    @Override
    public String putBinaryForVersion(String version, URL url) {
        String path = super.putBinaryForVersion(version, url);
        // This is mostly a hack to ensure we don't get stuck prompting the user
        // in the background about sending metrics. This will, by default, not send metrics,
        // so users will have to opt-in explicitly with etlas flags.
        File etlasConfig = new File(System.getProperty("user.home"), ".etlas/config");
        if (!etlasConfig.exists()) {
            logger().info("Initializing etlas config via: etlas user-config init");
            new CommandLine(path, "user-config", "init").executeAndLogOutput();
            if (!etlasConfig.exists()) {
                throw new GradleException("Initialized etlas config not found");
            }
        }
        logger().info("Updating etlas packages via: etlas update");
        new CommandLine(path, "update").executeAndLogOutput();
        return path;
    }
}
