package com.typelead.gradle.eta.internal;

import java.util.Map;

import groovy.lang.MissingMethodException;

import com.typelead.gradle.utils.ExtensionHelper;
import com.typelead.gradle.eta.api.EtaDependencyHandler;
import com.typelead.gradle.eta.api.EtaDependency;
import com.typelead.gradle.eta.api.EtaConfiguration;

import org.gradle.api.artifacts.Configuration;
import org.gradle.api.artifacts.ConfigurationContainer;

public class DefaultEtaDependencyHandler implements EtaDependencyHandler {

    private final ConfigurationContainer configurationContainer;
    // private final DynamicConfigurationMethods dynamicConfigurationMethods
    //     = new DynamicConfigurationMethods();

    public DefaultEtaDependencyHandler(ConfigurationContainer configurationContainer) {
        this.configurationContainer = configurationContainer;
    }

    // @Override
    // public MethodAccess getAdditionalMethods() {
    //     return dynamicConfigurationMethods;
    // }

    @Override
    public EtaDependency add(String configurationName, String dependencyConstraint) {
        return add(configurationName, DefaultEtaDependency.create(dependencyConstraint));
    }

    public EtaDependency add(Configuration targetConfiguration, String dependencyConstraint) {
        return add(targetConfiguration, DefaultEtaDependency.create(dependencyConstraint));
    }

    @Override
    public EtaDependency add(String configurationName, Map<String, String> dependencyConstraintAttributes) {
        return add(configurationName, DefaultEtaDependency.create(dependencyConstraintAttributes));
    }

    public EtaDependency add(Configuration targetConfiguration, Map<String, String> dependencyConstraintAttributes) {
        return add(targetConfiguration, DefaultEtaDependency.create(dependencyConstraintAttributes));
    }

    private EtaDependency add(String configurationName, DefaultEtaDependency dependency) {
        return add(configurationContainer.findByName(configurationName), dependency);
    }

    private EtaDependency add(Configuration targetConfiguration, DefaultEtaDependency dependency) {
        ExtensionHelper.getExtension(targetConfiguration, EtaConfiguration.class)
            .getDependencies().add(dependency);
        return dependency;
    }

    public Object methodMissing(final String configurationName, final Object configurationArguments) {
        Object[] configurationParameters = (Object[]) configurationArguments;
        int numConfigurationParameters = configurationParameters.length;
        if (numConfigurationParameters == 1) {
            Configuration targetConfiguration = configurationContainer.findByName(configurationName);
            if (targetConfiguration != null) {
                Object argument = configurationParameters[0];
                if (argument instanceof Map) {
                    return add(targetConfiguration, (Map<String,String>)argument);
                } else if (argument instanceof String) {
                    return add(targetConfiguration, (String)argument);
                }
            }

        }
        throw new MissingMethodException(configurationName, getClass(), configurationParameters);
    }

    // private class DynamicConfigurationMethods implements MethodAccess {

    //     public DynamicConfigurationMethods() {}

    //     @Override
    //     public boolean hasMethod(String configurationName, Object... configurationParameters) {
    //         int numConfigurationParameters = configurationParameters.length;
    //         return numConfigurationParameters == 1
    //             && configurationContainer.findByName(configurationName) != null;
    //     }

    //     @Override
    //     public DynamicInvokeResult tryInvokeMethod(String configurationName, Object... configurationParameters) {
    //         int numConfigurationParameters = configurationParameters.length;
    //         if (numConfigurationParameters != 1) {
    //             return DynamicInvokeResult.notFound();
    //         }
    //         Configuration targetConfiguration = configurationContainer.findByName(configurationName);
    //         if (targetConfiguration == null) {
    //             return DynamicInvokeResult.notFound();
    //         }

    //         Object argument = configurationArguments[0];
    //         if (argument instanceof Map) {
    //             DynamicInvokeResult.found(add(targetConfiguration, (Map<String,String>)argument));
    //         } else if (argument instanceof String) {
    //             DynamicInvokeResult.found(add(targetConfiguration, (String)argument));
    //         } else {
    //             return DynamicInvokeResult.notFound();
    //         }
    //     }
    // }
}
