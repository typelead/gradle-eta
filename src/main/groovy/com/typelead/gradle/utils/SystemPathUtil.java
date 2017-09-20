package com.typelead.gradle.utils;

import org.gradle.api.Nullable;

import java.io.File;

public abstract class SystemPathUtil {

    /**
     * Locate an executable on the system PATH; returns null if not found.
     * This emulates system commands like `which` (Unix) or `where` (Windows).
     */
    @Nullable
    public static File findExecutable(String name) {
        for (String dirName : System.getenv("PATH").split(File.pathSeparator)) {
            File[] children = new File(dirName).listFiles();
            if (children == null) continue;
            for (File child : children) {
                if (child.canExecute() && child.getName().equals(name)) {
                    return child;
                }
            }
        }
        return null;
    }
}
