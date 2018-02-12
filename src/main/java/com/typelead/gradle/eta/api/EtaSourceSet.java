package com.typelead.gradle.eta.api;

import groovy.lang.Closure;

import org.gradle.api.Action;
import org.gradle.api.file.SourceDirectorySet;

public interface EtaSourceSet {

    /**
     * Returns the source to be compiled by the Eta compiler for this source set.
     * This may contain both Java and Eta source files.
     *
     * @return The Eta source. Never returns null.
     */
    SourceDirectorySet getEta();

    /**
     * Configures the Eta source for this set.
     *
     * The given closure is used to configure the {@link SourceDirectorySet} which
     * contains the Eta source.
     *
     * @param configureClosure The closure to use to configure the Eta source.
     * @return this
     */
    EtaSourceSet eta(Closure configureClosure);

    /**
     * Configures the Eta source for this set.
     *
     * <p>The given action is used to configure the {@link SourceDirectorySet} which
     * contains the Eta source.
     *
     * @param configureAction The action to use to configure the Eta source.
     * @return this
     */
    EtaSourceSet eta(Action<? super SourceDirectorySet> configureAction);

    /**
     * All Eta source for this source set.
     *
     * @return the Eta source. Never returns null.
     */
    SourceDirectorySet getAllEta();

}
