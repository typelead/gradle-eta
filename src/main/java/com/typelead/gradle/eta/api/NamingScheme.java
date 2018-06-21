package com.typelead.gradle.eta.api;

import java.util.ArrayList;
import java.util.regex.Pattern;
import java.util.regex.Matcher;

import org.gradle.api.Project;
import org.gradle.api.tasks.SourceSet;

import com.typelead.gradle.utils.Version;

public class NamingScheme {

    public static String getTaskName(String verb, String name) {
        if (name.equals("main")) {
            name = "";
        } else {
            name = capitalize(name);
        }
        return verb + name + "Eta";
    }

    public static String getCompileTaskName(String name) {
        return getTaskName("compile", name);
    }

    public static String getInstallDependenciesTaskName(String name) {
        return getTaskName("installDependencies", name);
    }

    public static String getInjectDependenciesTaskName(String name) {
        return getTaskName("injectDependencies", name);
    }

    public static String getRelativeOutputDir(String prefix) {
        return "eta/" + prefix;
    }

    private static String capitalize(String source) {
        if (source.length() > 0) {
            return Character.toUpperCase(source.charAt(0)) + source.substring(1);
        } else {
            return source;
        }
    }

    public static String getPackageName(Project project, String extraName) {
        String path = project.getPath().substring(1);
        if (path.equals("")) {
            path = project.getName();
        }
        String suffix;
        if (extraName.equals(SourceSet.MAIN_SOURCE_SET_NAME)) {
            suffix = "";
        } else {
            suffix = ":" + extraName;
        }
        return (path + suffix).replace(":", "-");
    }

    private final static String versionPatternTemplate = "\\d+\\.?";
    private final static Pattern versionPattern = Pattern.compile(versionPatternTemplate);
    private final static String zeroVersion = "0.0.0";

    public static String fixVersion(final String version) {
        final Matcher m = versionPattern.matcher(version);
        final StringBuilder sb = new StringBuilder();
        while (m.find()) {
            sb.append(m.group(0));
        }
        final String result = sb.toString();
        if (result.length() > 0) {
            return result;
        } else {
            return zeroVersion;
        }
    }
}
