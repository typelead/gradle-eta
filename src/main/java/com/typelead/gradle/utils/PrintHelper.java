package com.typelead.gradle.utils;

import java.util.Map;
import java.util.Iterator;

public class PrintHelper {
    public static <K, V> String toString(Map<K,V> map) {
        StringBuilder result = new StringBuilder();
        Iterator<Map.Entry<K,V>> it = map.entrySet().iterator();
        result.append("{");
        if (it.hasNext()) {
            Map.Entry<K,V> entry = it.next();
            result.append(entry.getKey().toString());
            result.append(": ");
            result.append(entry.getValue().toString());
            while (it.hasNext()) {
                entry = it.next();
                result.append(", ");
                result.append(entry.getKey().toString());
                result.append(": ");
                result.append(entry.getValue().toString());
            }
        }
        result.append("}");
        return result.toString();
    }
}
