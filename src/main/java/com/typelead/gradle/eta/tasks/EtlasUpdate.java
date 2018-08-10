package com.typelead.gradle.eta.tasks;

import org.gradle.api.DefaultTask;
import org.gradle.api.tasks.TaskAction;

import com.typelead.gradle.utils.EtlasCommand;

public class EtlasUpdate extends DefaultTask {

    @TaskAction
    public void updateEtlas() {
        new EtlasCommand(getProject()).update();
    }
}
