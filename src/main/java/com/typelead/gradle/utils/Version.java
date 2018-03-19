package com.typelead.gradle.utils;

import java.io.Serializable;

/**
 * Supports comparing Version numbers.
 */
public class Version implements Comparable<Version>, Serializable {

    private static final long serialVersionUID = 1L;

    private final int[] parts;

    public Version(int[] parts) {
        this.parts = parts;
    }

    public static Version create(String v) {
        if (v == null) throw new IllegalArgumentException("Version String must not be null");
        if (v.isEmpty()) throw new IllegalArgumentException("Version String must not be empty");
        String[] ps = v.split("\\.");
        int[] parts = new int[ps.length];
        try {
            for (int i = 0; i < ps.length; ++i) {
                parts[i] = Integer.parseInt(ps[i]);
            }
            return new Version(parts);
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("Invalid Version String: " + v, e);
        }
    }

    public boolean isAfter(Version v) {
        return compareTo(v) > 0;
    }

    public boolean isAfterOrEqualTo(Version v) {
        return compareTo(v) >= 0;
    }

    public boolean isBefore(Version v) {
        return compareTo(v) < 0;
    }

    public boolean isBeforeOrEqualTo(Version v) {
        return compareTo(v) <= 0;
    }

    /* This will take the last digit of the version and increase it by 1.
       i.e. increment(4.2.10) == 4.2.11
     */
    public Version increment() {
        int n = parts.length;
        int[] newParts = new int[n];
        System.arraycopy(parts, 0, newParts, 0, n);
        newParts[n - 1]++;
        return new Version(newParts);
    }

    @Override
    public boolean equals(Object o) {
        return o instanceof Version && compareTo((Version) o) == 0;
    }

    @Override
    public int compareTo(Version v) {
        int end = Math.max(this.parts.length, v.parts.length);
        for (int i = 0; i < end; ++i) {
            int x = this.parts.length >= i ? this.parts[i] : 0;
            int y = v.parts.length >= i ? v.parts[i] : 0;
            if (x == y) continue;
            if (x < y) return -1;
            else return 1;
        }
        return 0;
    }

    @Override
    public String toString() {
        if (parts != null && parts.length > 0) {
            int len = parts.length;
            String result = "" + parts[0];
            len--;
            if (len > 0) {
                for (int i = 1; i < parts.length; i++) {
                    result += "." + parts[i];
                }
            }
            return result;
        } else {
            return "";
        }
    }
}
