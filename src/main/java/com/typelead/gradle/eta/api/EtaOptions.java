package com.typelead.gradle.eta.api;

import org.gradle.api.NamedDomainObjectContainer;

public class EtaOptions {

    private NamedDomainObjectContainer<Language> language;
    private NamedDomainObjectContainer<LanguageExtension> languageExtensions;

    public EtaOptions(NamedDomainObjectContainer<Language> language, NamedDomainObjectContainer<LanguageExtension> languageExtensions) {
        this.language = language;
        this.languageExtensions = languageExtensions;
    }

    public NamedDomainObjectContainer<Language> getLanguage() {
        return language;
    }

    public void setLanguage(NamedDomainObjectContainer<Language> language) {
        this.language = language;
    }

    public NamedDomainObjectContainer<LanguageExtension> getLanguageExtensions() {
        return languageExtensions;
    }

    public void setLanguageExtensions(NamedDomainObjectContainer<LanguageExtension> languageExtensions) {
        this.languageExtensions = languageExtensions;
    }
}
