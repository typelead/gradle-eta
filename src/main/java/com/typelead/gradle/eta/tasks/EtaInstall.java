package com.typelead.gradle.eta.tasks;

import java.io.File;

import org.gradle.api.tasks.Input;
import org.gradle.api.tasks.OutputFile;
import org.gradle.api.tasks.TaskAction;

import com.typelead.gradle.utils.EtlasCommand;
import com.typelead.gradle.eta.config.EtaExtension;
import com.typelead.gradle.eta.plugins.EtaBasePlugin;

public class EtaInstall extends AbstractEtlasTask {

    private File sandboxConfigFile;

    @Override
    public void configureWithExtension(EtaExtension extension, String buildVariantPath) {
        super.configureWithExtension(extension, buildVariantPath);
        this.sandboxConfigFile =
            new File(getSandboxRootDir() + File.separator
                     + EtaBasePlugin.DEFAULT_SANDBOX_CONFIG);
    }

    @Input
    public String getEtaVersion() {
        return super.getEtaVersion();
    }

    /* TODO: This is a really bad hack. Fix with something better, like an
       `etlas select --paths` command. */
    @OutputFile
    public File getSandboxConfigFile() {
        return sandboxConfigFile;
    }

    @TaskAction
    public void installEta() {
        getProject().getLogger().lifecycle("Installing Eta v" + getEtaVersion() + ". This may take several minutes...");
        new EtlasCommand(this).installEta();
    }
}
