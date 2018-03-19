package com.typelead.gradle.utils;

import java.io.File;
import java.io.PrintStream;
import java.io.FileOutputStream;
import java.io.FileNotFoundException;

import org.gradle.api.GradleException;

public abstract class FileUtils {
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

    public static void write(File file, String content) {
        try (PrintStream outputFile = new PrintStream(new FileOutputStream(file))) {
            outputFile.print(content);
            outputFile.flush();
        } catch (FileNotFoundException e) {
            throw new GradleException("File not found.", e);
        }

    }
}
