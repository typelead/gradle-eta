package com.typelead.gradle.utils;

import org.gradle.api.plugins.ExtensionAware;

public class ExtensionHelper {
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
                message = "'null'";
            } else {
                message = extensibleObject.getClass().toString();
            }
            throw new IllegalArgumentException
                ("Cannot extend a non-extensible object " + message);
        }

        return ((ExtensionAware) extensibleObject).getExtensions()
            .findByType(extensionType);
    }
}
