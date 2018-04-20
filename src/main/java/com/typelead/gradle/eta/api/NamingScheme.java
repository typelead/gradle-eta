package com.typelead.gradle.eta.api;

import org.gradle.api.Project;

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
        return (path + ":" + extraName).replace(":", "-");
    }
}
