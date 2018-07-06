package com.typelead.gradle.utils;

import org.gradle.api.GradleException;
import org.gradle.api.Project;

public class PropertyParse {

    public static String parseStringProperty(final Project project, final String name) {
        return parseStringProperty(project, name, null);
    }

    public static String parseStringProperty(final Project project, final String name,
                                             final String def) {
        Object v = project.findProperty("eta." + name);
        String value;
        if (v == null) {
            value = def;
        } else {
            value = v.toString();
        }
        return value;
    }

    public static boolean parseBooleanProperty(final Project project, final String name,
                                               final boolean def) {
        Object v = project.findProperty("eta." + name);
        boolean value;
        if (v == null) {
            value = def;
        } else {
            String booleanString = v.toString();
            if (booleanString.equalsIgnoreCase("true")) {
                value = true;
            } else if (booleanString.equalsIgnoreCase("false")) {
                value = false;
            } else throw new GradleException("Invalid property value for eta."
                                             + name + ": " + booleanString);
        }
        return value;
    }
}
