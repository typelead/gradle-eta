package com.typelead.gradle.utils;

import java.io.*;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.Scanner;
import java.util.function.Consumer;
import java.util.function.Function;

public abstract class IOUtils {

    public static String toString(InputStream is) {
        return toString(is, StandardCharsets.UTF_8);
    }

    public static String toString(InputStream is, Charset charset) {
        Scanner s = new Scanner(is, charset.name()).useDelimiter("\\A");
        return s.hasNext() ? s.next() : "";
    }

    public static void consumeLines(InputStream is, Consumer<String> f) {
        new Thread(() -> {
            BufferedReader in = new BufferedReader(new InputStreamReader(is));
            String line = null;
            StringBuilder sb = new StringBuilder();
            try {
                while ((line = in.readLine()) != null) {
                    f.accept(line);
                }
            } catch (IOException e) {
                throw new RuntimeException("Failed to read line from InputStream", e);
            }
        }).start();
    }

    public static String readFile(File file) {
        try {
            return new String(Files.readAllBytes(file.toPath()), StandardCharsets.UTF_8);
        } catch (IOException e) {
            throw new RuntimeException("Failed to read file " + file, e);
        }
    }

    public static void writeFile(File file, String content) {
        try {
            try(PrintWriter out = new PrintWriter(file)) {
                out.println(content);
            }
        } catch (FileNotFoundException e) {
            throw new RuntimeException("Could not write file " + file, e);
        }
    }
}
