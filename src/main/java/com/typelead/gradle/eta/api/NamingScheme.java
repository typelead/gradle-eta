package com.typelead.gradle.eta.api;

public class NamingScheme {

    public static String getTaskName(String verb, String name) {
        name = capitalize(name);
        return verb + name + "Eta";
    }

    public static String getCompileTaskName(String name) {
        return getTaskName("compile", name);
    }

    public static String getInstallDependenciesTaskName(String name) {
        return getTaskName("installDependencies", name);
    }

    public static String getRelativeOutputDir(String prefix) {
        return "eta/" + prefix;
    }

    private static String capitalize(String source) {
        return Character.toUpperCase(source.charAt(0)) + source.substring(1);
    }
}
