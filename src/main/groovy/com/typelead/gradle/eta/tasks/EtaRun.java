package com.typelead.gradle.eta.tasks;

import com.typelead.gradle.eta.plugins.EtaPlugin;
import com.typelead.gradle.utils.CabalInfo;
import org.gradle.api.GradleException;
import org.gradle.api.file.FileCollection;
import org.gradle.api.internal.file.collections.SimpleFileCollection;
import org.gradle.api.tasks.TaskAction;

import java.io.File;
import java.util.*;

public class EtaRun extends AbstractEtlasRun {

    private String component;

    @Override
    @TaskAction
    public void exec() {
        setDefaultComponent();
        setClasspath(patchedClasspath());
        super.exec();
    }

    @Override
    public String getMain() {
        return EtaPlugin.DEFAULT_ETA_MAIN_CLASS;
    }

    /** Returns a patched classpath with our compiled jar from the current classpath. */
    private FileCollection patchedClasspath() {
        Set<File> cpOld = getClasspath().getFiles();
        Set<File> runtime = getRuntimeDependencies();
        Set<File> cp = new HashSet<>(cpOld.size() + runtime.size() + 1);
        cp.addAll(cpOld);
        cp.addAll(runtime);
        cp.add(getCompiledJar());
        return new SimpleFileCollection(cp);
    }

    private Set<File> getRuntimeDependencies() {
        return getProject().getConfigurations()
                .getByName(EtaPlugin.ETA_RUNTIME_CONFIGURATION_NAME)
                .getFiles();
    }

    private void setDefaultComponent() {
        if (component != null) return;
        CabalInfo cabalInfo = CabalInfo.get(getProject());
        List<String> executables = cabalInfo.getExecutableNames();
        if (executables.size() == 0) {
            throw new GradleException("No executable found in cabal file");
        }
        if (executables.size() > 1) {
            throw new GradleException(
                    "Cannot infer executable, found " + executables.size()
                            + " executables: " + executables
                            + "; please disambiguate with 'component' in task config");
        }
        component = executables.get(0);
    }

    /** Finds compiled 'component' jar. */
    private File getCompiledJar() {
        File jar = new File(
                getProject().getRootDir() + "/" + getBuildDir() + "/build/"
                        + component + "/" + component + ".jar");
        if (!jar.exists()) {
            throw new GradleException("Compiled jar does not exist: " + jar);
        }
        return jar;
    }

    public String getComponent() {
        return component;
    }

    public void setComponent(String component) {
        this.component = component;
    }

    @Override
    public List<String> getComponents() {
        if (component == null) return Collections.emptyList();
        return Collections.singletonList(component);
    }

    @Override
    public void setComponents(List<String> components) {
        throw new GradleException("Use 'component' property for EtaRun tasks, not 'components'");
    }
}
