package com.typelead.gradle.utils;

import org.gradle.api.plugins.ExtensionAware;
import org.gradle.api.internal.HasConvention;

public class ExtensionHelper {
    public static void createConvention(Object conventionTarget, String conventionName, Object conventionObject) {
        assertConventionObject(conventionTarget);
        ((HasConvention) conventionTarget).getConvention().getPlugins()
            .put(conventionName, conventionObject);
    }

    public static <T> T getConvention(Object conventionTarget, Class<T> conventionType) {
        assertConventionObject(conventionTarget);
        return ((HasConvention) conventionTarget).getConvention()
            .findPlugin(conventionType);
    }


    public static <T> T createExtension(Object extensibleObject, String extensionName,
                                        Class<T> extensionType, Object... extensionArgs) {
        assertExtensibleObject(extensibleObject);
        return ((ExtensionAware) extensibleObject).getExtensions()
            .create(extensionName, extensionType, extensionArgs);
    }

    public static <T> T getExtension(Object extensibleObject, Class<T> extensionType) {
        assertExtensibleObject(extensibleObject);
        return ((ExtensionAware) extensibleObject).getExtensions()
            .findByType(extensionType);
    }

    public static boolean hasExtProperty(Object extensibleObject, String propertyName) {
        assertExtensibleObject(extensibleObject);
        return ((ExtensionAware) extensibleObject).getExtensions()
            .getExtraProperties().has(propertyName);
    }

    public static Object getExtProperty(Object extensibleObject, String propertyName) {
        assertExtensibleObject(extensibleObject);
        return ((ExtensionAware) extensibleObject).getExtensions()
            .getExtraProperties().get(propertyName);
    }

    public static void setExtProperty(Object extensibleObject, String propertyName, Object value) {
        assertExtensibleObject(extensibleObject);
        ((ExtensionAware) extensibleObject).getExtensions()
            .getExtraProperties().set(propertyName, value);
    }

    private static void assertExtensibleObject(Object extensibleObject) {
        if (!(extensibleObject instanceof ExtensionAware)) {
            String message;
            if (extensibleObject != null) {
                message = extensibleObject.getClass().toString();
            } else {
                message = "'null'";
            }
            throw new IllegalArgumentException
                ("Cannot extend a non-extensible object " + message);
        }
    }

    private static void assertConventionObject(Object conventionTarget) {
        if (!(conventionTarget instanceof HasConvention)) {
            String message;
            if (conventionTarget != null) {
                message = conventionTarget.getClass().toString();
            } else {
                message = "'null'";
            }
            throw new IllegalArgumentException
                ("Cannot create convention for a non-convention-aware object "
                 + message);
        }
    }
}
