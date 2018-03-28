package com.typelead.gradle.eta.internal;

import org.gradle.api.Project;
import org.gradle.api.artifacts.Configuration;
import org.gradle.api.artifacts.Dependency;

import com.typelead.gradle.utils.ExtensionHelper;
import com.typelead.gradle.eta.api.EtaConfiguration;

public class ConfigurationUtils {
    public static EtaConfiguration getEtaConfiguration
        (final Configuration configuration) {
        return ExtensionHelper.getExtension(configuration, EtaConfiguration.class);
    }

    public static EtaConfiguration getEtaConfiguration
        (final Project project, final String configurationName) {
        return ExtensionHelper.getExtension
            (getConfiguration(project, configurationName), EtaConfiguration.class);
    }

    public static Configuration getConfiguration
        (final Project project, String configurationName) {
        if (configurationName == null) {
            configurationName = Dependency.DEFAULT_CONFIGURATION;
        }
        return project.getConfigurations().getByName(configurationName);
    }
}
