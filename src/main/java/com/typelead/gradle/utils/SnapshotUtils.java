package com.typelead.gradle.utils;

import java.io.File;
import java.io.UnsupportedEncodingException;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

import org.gradle.api.GradleException;

public class SnapshotUtils {

    public static boolean takeSnapshotAndCompare
        (File snapshotFile, Object... hashSources) {

        StringBuilder sb = new StringBuilder();

        for (Object hashSource : hashSources) {
            sb.append(hashSource);
        }

        boolean snapshotFileExists = snapshotFile.exists();

        String newHash = createHexHash(sb.toString());
        String oldHash = snapshotFileExists? FileUtils.read(snapshotFile) : "";

        boolean changed = !newHash.equals(oldHash);

        if (changed) {
            FileUtils.write(snapshotFile, newHash);
        }

        return changed;
    }

    private static String createHexHash(String source) {
        try {
            MessageDigest digest = MessageDigest.getInstance("SHA-256");

            digest.update(source.getBytes(StandardCharsets.UTF_8));

            byte[] bytes = digest.digest();

            StringBuilder sb = new StringBuilder();

            for (int i = 0; i < bytes.length;
                 sb.append(Integer.toHexString(bytes[i] & 0xFF)), i++);

            return sb.toString();
        } catch (NoSuchAlgorithmException e) {
            throw new GradleException("SHA-256 not supported", e);
        }
    }
}
