package com.typelead.gradle.eta.tasks;

import com.typelead.gradle.utils.CabalInfo;
import com.typelead.gradle.utils.EtlasCommand;
import org.gradle.api.tasks.TaskAction;

import java.util.List;

public class EtaTestCompile extends AbstractEtlasTask {

    @TaskAction
    public void testCompileEta() {
        EtlasCommand c = new EtlasCommand(this);
        if (!getConfigureFlags().isEmpty()) c.configure(getConfigureFlags());
        c.build();
    }

    @Override
    public final List<String> getComponents() {
        if (!super.getComponents().isEmpty()) return super.getComponents();
        CabalInfo cabalInfo = CabalInfo.get(getProject());
        return cabalInfo.getTestComponentNames();
    }
}
