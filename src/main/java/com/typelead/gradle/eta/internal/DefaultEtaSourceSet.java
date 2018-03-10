package com.typelead.gradle.eta.internal;

import groovy.lang.Closure;

import org.gradle.api.Action;
import org.gradle.api.Describable;
import org.gradle.api.file.SourceDirectorySet;
import org.gradle.api.internal.file.SourceDirectorySetFactory;
import org.gradle.api.tasks.SourceSet;

import com.typelead.gradle.eta.api.EtaSourceSet;

public class DefaultEtaSourceSet implements EtaSourceSet {

    private final SourceSet sourceSet;
    private final SourceDirectorySet eta;

    public DefaultEtaSourceSet(SourceSet sourceSet,
                               SourceDirectorySetFactory sourceDirectorySetFactory) {
        this.sourceSet = sourceSet;

        final String displayName = ((Describable) sourceSet).getDisplayName();

        this.eta =
            sourceDirectorySetFactory.create("eta", displayName + " Eta source");
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
        return sourceSet.getCompileTaskName("eta");
    }

    public String getInstallDependenciesTaskName() {
        return getTaskName("installDependencies");
    }

    public String getFetchDependenciesTaskName() {
        return getTaskName("fetchDependencies");
    }

    public String getRelativeOutputDir() {
        return "eta/" + sourceSet.getName();
    }

    public String getClassesDir() {
        return "classes/" + eta.getName() + "/" + sourceSet.getName();
    }

    private String getTaskName(String verb) {
        return verb +  sourceSet.getName() + "Eta";
    }
}
