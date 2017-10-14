package com.typelead.gradle.eta.tasks;

import com.typelead.gradle.utils.CabalInfo;
import com.typelead.gradle.utils.EtlasCommand;
import org.gradle.api.tasks.TaskAction;

import java.util.ArrayList;
import java.util.List;

public class EtaTest extends AbstractEtlasTask {

    private List<String> testFlags = new ArrayList<>();

    @TaskAction
    public void testEta() {
        EtlasCommand c = new EtlasCommand(this);
        c.test(getTestFlags());
    }

    @Override
    public List<String> getComponents() {
        if (!super.getComponents().isEmpty()) return super.getComponents();
        CabalInfo cabalInfo = CabalInfo.get(getProject());
        // In this case, the component names must NOT be prefixed with "test:'
        return cabalInfo.getTestNames();
    }

    public List<String> getTestFlags() {
        return testFlags;
    }

    public void setTestFlags(List<String> testFlags) {
        this.testFlags = testFlags;
    }
}
