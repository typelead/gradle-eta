package com.typelead.gradle.eta.tasks;

import com.typelead.gradle.utils.EtlasCommand;
import org.gradle.api.tasks.TaskAction;

import java.util.ArrayList;
import java.util.List;

public class EtaInstallTestDeps extends AbstractEtlasTask {

    @TaskAction
    public void installTestDepsEta() {
        EtlasCommand c = new EtlasCommand(this);
        c.configure(getConfigureFlags());
        c.installDependenciesOnly();
    }

    @Override
    public final List<String> getConfigureFlags() {
        ArrayList<String> flags = new ArrayList<>(super.getConfigureFlags());
        flags.add("--enable-tests");
        return flags;
    }
}
