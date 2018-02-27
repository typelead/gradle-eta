package com.typelead.gradle.utils;

import java.io.File;

public abstract class FileUtil {
    public static void removeDirectoryRecursive(File dir) {
        if (!dir.exists()) return;
        if (!dir.isDirectory()) return;
        for (String file : dir.list()) {
            File f = new File(dir, file);
            if (f.isDirectory()) {
                removeDirectoryRecursive(f);
            } else {
                f.delete();
            }
        }
        dir.delete();
    }
}
