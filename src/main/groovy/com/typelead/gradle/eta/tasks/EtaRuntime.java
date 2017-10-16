package com.typelead.gradle.eta.tasks;

import com.typelead.gradle.eta.plugins.EtaPlugin;
import com.typelead.gradle.utils.EtlasCommand;
import org.gradle.api.tasks.TaskAction;

import java.util.List;

public class EtaRuntime extends AbstractEtlasTask {

    @TaskAction
    public void runtimeEta() {
        List<String> etaRuntimeClasspath = new EtlasCommand(this).depsClasspath();
        getLogger().info("Adding to etaRuntime configuration: " + etaRuntimeClasspath);
        getProject().getDependencies().add(
                EtaPlugin.ETA_RUNTIME_CONFIGURATION_NAME,
                getProject().files(etaRuntimeClasspath.toArray())
        );
    }
}
