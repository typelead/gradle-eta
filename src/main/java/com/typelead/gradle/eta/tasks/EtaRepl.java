package com.typelead.gradle.eta.tasks;

import java.io.File;

import org.gradle.api.DefaultTask;
import org.gradle.api.Project;
import org.gradle.api.file.Directory;
import org.gradle.api.file.DirectoryProperty;
import org.gradle.api.tasks.TaskAction;
import org.gradle.api.provider.Provider;

import com.typelead.gradle.utils.EtlasCommand;

public class EtaRepl extends DefaultTask {

    private final Project project;
    private Provider<String> packageName;
    private DirectoryProperty destinationDir;

    public EtaRepl() {
        this.project = getProject();
        this.destinationDir =
            project.getLayout().directoryProperty();
    }

    public String getPackageName() {
        if (packageName != null) {
            return packageName.get();
        } else {
            return null;
        }
    }

    public void setPackageName(Provider<String> packageName) {
        this.packageName = packageName;
    }

    public File getDestinationDir() {
        return destinationDir.getAsFile().get();
    }

    public void setDestinationDir(Provider<Directory> destinationDir) {
        this.destinationDir.set(destinationDir);
    }

    @TaskAction
    public void repl() {

        final File workingDir = getDestinationDir();

        /* Fork an etlas process to start the repl.  */

        final EtlasCommand etlas = new EtlasCommand(project);

        etlas.getWorkingDirectory().set(workingDir);

        String packageName = getPackageName();
        etlas.repl(packageName == null? null : EtlasCommand.libTarget(packageName));
    }
}
