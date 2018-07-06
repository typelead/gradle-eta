package com.typelead.gradle.eta.tasks;

import java.io.File;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.LinkedList;
import java.util.Optional;

import org.gradle.api.DefaultTask;
import org.gradle.api.Project;
import org.gradle.api.file.Directory;
import org.gradle.api.file.DirectoryProperty;
import org.gradle.api.tasks.TaskAction;
import org.gradle.api.provider.Provider;
import org.gradle.api.provider.Property;

import com.typelead.gradle.utils.EtlasCommand;
import com.typelead.gradle.utils.OverridingProperty;
import static com.typelead.gradle.utils.PropertyParse.*;

public class EtaRepl extends DefaultTask {

    private final Project project;
    private Provider<String> packageName;
    private DirectoryProperty destinationDir;
    private Property<String[]> args;

    public EtaRepl() {
        this.project = getProject();
        this.destinationDir =
            project.getLayout().directoryProperty();
        Property<String[]> argsProp = (Property<String[]>)
            project.getObjects().property(String[].class);
        argsProp.set(new String[0]);
        this.args = new OverridingProperty<String[]>
            (project.provider(() -> {
                    String args = parseStringProperty(project, "args");
                    if (args == null) return null;
                    return args.split(" ");
                }),
             Optional.empty(),
             argsProp);
    }
    
    public List<String> getArgs() {
        return Arrays.asList(args.get());
    }

    public void setArgs(List<String> args) {
        this.args.set(Arrays.copyOf(args.toArray(), args.size(), String[].class));
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
        etlas.repl(packageName == null? null : EtlasCommand.libTarget(packageName), getArgs());
    }
}
