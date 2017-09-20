package com.typelead.gradle.utils;

import java.io.InputStream;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Scanner;

public abstract class IOUtils {

    public static String toString(InputStream is) {
        return toString(is, StandardCharsets.UTF_8);
    }

    public static String toString(InputStream is, Charset charset) {
        Scanner s = new Scanner(is, charset.name()).useDelimiter("\\A");
        return s.hasNext() ? s.next() : "";
    }
}
