package com.typelead.gradle.eta.internal;

import org.gradle.api.Project;
import org.gradle.api.artifacts.Configuration;

import com.typelead.gradle.utils.ExtensionHelper;
import com.typelead.gradle.eta.api.EtaConfiguration;

public class ConfigurationUtils {
    public static EtaConfiguration getEtaConfiguration
        (final Configuration configuration) {
        return ExtensionHelper.getExtension(configuration, EtaConfiguration.class);
    }

    public static EtaConfiguration getEtaConfiguration
        (final Project project, final String configurationName) {
        return ExtensionHelper.getExtension(project.getConfigurations()
                                            .getByName(configurationName),
                                            EtaConfiguration.class);
    }
}
