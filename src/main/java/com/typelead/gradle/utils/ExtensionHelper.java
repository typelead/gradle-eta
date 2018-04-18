package com.typelead.gradle.utils;

import org.gradle.api.plugins.ExtensionAware;
import org.gradle.api.internal.HasConvention;

public class ExtensionHelper {
    public static void createConvention(Object conventionTarget, String conventionName, Object conventionObject) {
        if (!(conventionTarget instanceof HasConvention)) {
            throw new IllegalArgumentException("Cannot create convention for a non-convention-aware object " + conventionTarget.getClass());
        }

        ((HasConvention) conventionTarget).getConvention().getPlugins()
            .put(conventionName, conventionObject);
    }

    public static <T> T getConvention(Object conventionTarget, Class<T> conventionType) {
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

        return ((HasConvention) conventionTarget).getConvention()
            .findPlugin(conventionType);
    }


    public static <T> T createExtension(Object extensibleObject, String extensionName,
                                        Class<T> extensionType, Object... extensionArgs) {
        if (!(extensibleObject instanceof ExtensionAware)) {
            throw new IllegalArgumentException("Cannot extend a non-extensible object " + extensibleObject.getClass());
        }

        return ((ExtensionAware) extensibleObject).getExtensions()
            .create(extensionName, extensionType, extensionArgs);
    }

    public static <T> T getExtension(Object extensibleObject, Class<T> extensionType) {
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

        return ((ExtensionAware) extensibleObject).getExtensions()
            .findByType(extensionType);
    }
}
