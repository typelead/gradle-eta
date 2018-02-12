package com.typelead.gradle.eta.internal;

import groovy.lang.Closure;

import org.gradle.api.Action;
import org.gradle.api.file.SourceDirectorySet;
import org.gradle.api.internal.file.SourceDirectorySetFactory;
import com.typelead.gradle.eta.api.EtaSourceSet;

import org.gradle.util.ConfigureUtil;

public class DefaultEtaSourceSet implements EtaSourceSet {
    private final SourceDirectorySet eta;
    private final SourceDirectorySet allEta;

    public DefaultEtaSourceSet(String name, String displayName, SourceDirectorySetFactory sourceDirectorySetFactory) {
        eta = sourceDirectorySetFactory.create(name, displayName + " Eta source");
        eta.getFilter().include("**/*.java", "**/*.eta", "**/*.hs");

        allEta = sourceDirectorySetFactory.create(displayName + " Eta source");
        allEta.source(eta);
        allEta.getFilter().include("**/*.eta");
    }

    @Override
    public SourceDirectorySet getEta() {
        return eta;
    }

    @Override
    public EtaSourceSet eta(Closure configureClosure) {
        ConfigureUtil.configure(configureClosure, getEta());
        return this;
    }

    @Override
    public EtaSourceSet eta(Action<? super SourceDirectorySet> configureAction) {
        configureAction.execute(getEta());
        return this;
    }

    @Override
    public SourceDirectorySet getAllEta() {
        return allEta;
    }
}
