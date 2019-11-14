package com.typelead.gradle.eta.internal;

import javax.inject.Inject;
import java.util.Optional;

import groovy.lang.Closure;

import org.gradle.api.Action;
import org.gradle.api.Describable;
import org.gradle.api.file.SourceDirectorySet;
import org.gradle.api.internal.file.SourceDirectorySetFactory;
import org.gradle.api.tasks.SourceSet;

import com.typelead.gradle.eta.api.EtaSourceSet;
import com.typelead.gradle.eta.api.NamingScheme;

public class DefaultEtaSourceSet implements EtaSourceSet {

    private final SourceSet sourceSet;
    private final SourceDirectorySet eta;

    @Inject
    public DefaultEtaSourceSet(Optional<SourceSet> sourceSet,
                               String name,
                               String displayName,
                               SourceDirectorySetFactory sourceDirectorySetFactory) {
        this.sourceSet = sourceSet.isPresent() ? sourceSet.get() : null;
        this.eta = sourceDirectorySetFactory.create(name, displayName + " Eta source");
        eta.getFilter().include("**/*.eta", "**/*.hs");
    }

    @Override
    public SourceDirectorySet getEta() {
        return eta;
    }

    @Override
    public EtaSourceSet eta(Action<? super SourceDirectorySet> configureAction) {
        configureAction.execute(getEta());
        return this;
    }

    public String getCompileTaskName() {
        if (sourceSet != null) {
            return sourceSet.getCompileTaskName("eta");
        } else {
            return getTaskName("compile");
        }
    }

    public String getInstallDependenciesTaskName() {
        return getTaskName("installDependencies");
    }

    public String getReplTaskName() {
        return getTaskName("repl");
    }

    public String getRelativeOutputDir() {
        String prefix;
        if (sourceSet != null) {
            prefix = sourceSet.getName();
        } else {
            prefix = "main";
        }
        return NamingScheme.getRelativeOutputDir(prefix);
    }

    public String getClassesDir() {
        return "classes/" + eta.getName() + "/" + sourceSet.getName();
    }

    private String getTaskName(String verb) {
        return NamingScheme.getTaskName
            (verb, (sourceSet == null)? "main" : sourceSet.getName());
    }

}
