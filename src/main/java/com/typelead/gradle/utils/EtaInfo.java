package com.typelead.gradle.utils;

import java.util.Set;

public class EtaInfo {

    public final String version;
    public final Set<String> validLanguages;
    public final Set<String> validExtensions;

    public EtaInfo(String version, Set<String> validLanguages,
                   Set<String> validExtensions) {
        this.version         = version;
        this.validLanguages  = validLanguages;
        this.validExtensions = validExtensions;
    }

    public String getVersion() {
        return version;
    }

    public boolean isValidLanguage(String language) {
        return validLanguages.contains(language);
    }

    public boolean isValidExtension(String extension) {
        return validExtensions.contains(extension);
    }
}
