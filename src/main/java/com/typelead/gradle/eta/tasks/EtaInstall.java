package com.typelead.gradle.eta.tasks;

import java.io.File;

import org.gradle.api.tasks.Input;
import org.gradle.api.tasks.OutputFile;
import org.gradle.api.tasks.TaskAction;

import com.typelead.gradle.utils.EtlasCommand;
import com.typelead.gradle.eta.api.EtaExtension;
import com.typelead.gradle.eta.plugins.EtaBasePlugin;

public class EtaInstall extends AbstractEtlasTask {

    @Input
    public String getEtaVersion() {
        return super.getEtaVersion();
    }

    @TaskAction
    public void installEta() {
        getProject().getLogger().lifecycle("Installing Eta v" + getEtaVersion() + ". This may take several minutes...");
        new EtlasCommand(this).installEta();
    }
}
