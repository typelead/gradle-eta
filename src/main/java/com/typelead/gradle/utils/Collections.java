package com.typelead.gradle.utils;

import java.util.Collection;

public class Collections {
    public static <T> boolean isNonEmpty(Collection<T> collection) {
        return collection != null && collection.size() > 0;
    }
}
