package com.typelead.gradle.eta.api;

public class LanguageExtension {
    private String name;

    public LanguageExtension(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }
}
