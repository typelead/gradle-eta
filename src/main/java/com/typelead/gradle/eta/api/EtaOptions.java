package com.typelead.gradle.eta.api;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import groovy.lang.Closure;

import org.gradle.api.GradleException;
import org.gradle.api.NamedDomainObjectContainer;
import org.gradle.api.tasks.Input;

import com.typelead.gradle.utils.EtaInfo;
import com.typelead.gradle.eta.api.LanguageExtension;

public class EtaOptions {

    private String language = "Haskell2010";
    private NamedDomainObjectContainer<LanguageExtension> languageExtensions;
    private List<String> arguments       = Collections.emptyList();
    private List<String> cppOptions      = Collections.emptyList();
    private List<String> installIncludes = Collections.emptyList();
    private List<String> includeDirs     = Collections.emptyList();

    @Input
    public String getLanguage() {
        return language;
    }

    public void setLanguage(String language) {
        this.language = language;
    }

    @Input
    public List<String> getArgs() {
        return arguments;
    }

    public void setArgs(String... args) {
        this.arguments =  Arrays.asList(args);
    }

    @Input
    public List<String> getCpp() {
        return cppOptions;
    }

    public void setCpp(String... cpp) {
        this.cppOptions = Arrays.asList(cpp);
    }

    @Input
    public List<String> getInstallIncludes() {
        return installIncludes;
    }

    public void setInstallIncludes(String... includes) {
        this.installIncludes = Arrays.asList(includes);
    }

    public List<String> getIncludeDirs() {
        return includeDirs;
    }

    public void setIncludeDirs(String... includeDirs) {
        this.includeDirs = Arrays.asList(includeDirs);
    }

    @Input
    public Iterable<String> getExtensions() {
        return languageExtensions.getNames();
    }

    public EtaOptions setExtensions
        (NamedDomainObjectContainer<LanguageExtension> languageExtensions) {
        this.languageExtensions = languageExtensions;
        return this;
    }

    public void setExtensions(List<String> languageExtensions) {
        for (String languageExtension : languageExtensions) {
            this.languageExtensions.maybeCreate(languageExtension);
        }
    }

    public void extensions(Closure closure) {
        languageExtensions.configure(closure);
    }

    public void validate(final EtaInfo info) {
        if (!info.isValidLanguage(language)) {
            throw new GradleException
                ("Language '" + language + "' is not recognized by Eta v"
                 + info.getVersion());
        }

        languageExtensions.forEach
            (extension -> {
                if (!info.isValidExtension(extension.getName())) {
                    throw new GradleException
                        ("Extension '" + extension + "' is not recognized by Eta v"
                         + info.getVersion());
                }
            });
    }
}
