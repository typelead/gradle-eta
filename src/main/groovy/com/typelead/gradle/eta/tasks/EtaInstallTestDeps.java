package com.typelead.gradle.eta.tasks;

import com.typelead.gradle.utils.EtlasCommand;
import org.gradle.api.tasks.TaskAction;

public class EtaInstallTestDeps extends AbstractEtlasTask {

    @TaskAction
    public void installTestDepsEta() {
        EtlasCommand c = new EtlasCommand(this);
        c.installTestDependenciesOnly();
    }
}
