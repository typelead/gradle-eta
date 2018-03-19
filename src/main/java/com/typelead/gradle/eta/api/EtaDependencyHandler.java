package com.typelead.gradle.eta.api;

import java.util.Map;

public interface EtaDependencyHandler {

    /**
     * @param configurationName The configuration to add the Eta dependency.
     * @return The Eta dependency in a normalized form.
     */
    EtaDependency add(String configurationName, String dependencyConstraint);

    /**
     * @param configurationName The configuration to add the Eta dependency.
     * @return The Eta dependency in a normalized form.
     */
    EtaDependency add(String configurationName, Map<String,String> dependencyConstraintAttributes);

    /**
     * @param options A map that contains a 'path' key and an optional 'configuration'
                      key which specifies which project to depend on and which
                      configuration from that task to depend on.
     * @return The Eta dependency in a normalized form.
     */
    EtaDependency project(Map<String,?> options);
}
