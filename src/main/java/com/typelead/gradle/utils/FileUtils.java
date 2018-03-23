package com.typelead.gradle.utils;

import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.io.FileOutputStream;
import java.io.FileNotFoundException;
import java.nio.file.Files;

import org.gradle.api.GradleException;

public abstract class FileUtils {

    public static void write(File file, String content) {
        try (PrintStream outputFile = new PrintStream(new FileOutputStream(file))) {
            outputFile.print(content);
            outputFile.flush();
        } catch (FileNotFoundException e) {
            throw new GradleException("[FileUtils.write] File not found.", e);
        }

    }

    public static String read(File file) {
        try {
            return new String(Files.readAllBytes(file.toPath()), "UTF-8");
        } catch (IOException e) {
            throw new GradleException("[FileUtils.read] File not found.", e);
        }
    }
}
