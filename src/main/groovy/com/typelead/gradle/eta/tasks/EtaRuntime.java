package com.typelead.gradle.eta.tasks;

import com.typelead.gradle.eta.plugins.EtaPlugin;
import com.typelead.gradle.utils.EtlasCommand;
import org.gradle.api.tasks.TaskAction;

import java.util.HashSet;
import java.util.Set;

public class EtaRuntime extends AbstractEtlasTask {

    @TaskAction
    public void runtimeEta() {
        getRuntimeDependencies().forEach(it -> {
                getLogger().info("Adding runtimeEta dependency: " + it);
                getProject().getDependencies().add(
                        EtaPlugin.ETA_RUNTIME_CONFIGURATION_NAME,
                        getProject().files(it)
                );
        });
    }

    private Set<String> getRuntimeDependencies() {
        return new HashSet<>(new EtlasCommand(this).depsClasspath());
    }
}
