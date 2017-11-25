package com.typelead.gradle.utils;

/** Supports comparing Version numbers. */
public class Version implements Comparable<Version> {

    private final int[] parts;

    public Version(String v) {
        if (v == null) throw new IllegalArgumentException("Version String must not be null");
        if (v.isEmpty()) throw new IllegalArgumentException("Version String must not be empty");
        String[] ps = v.split(".");
        parts = new int[ps.length];
        try {
            for (int i = 0; i < ps.length; ++i) {
                parts[i] = Integer.parseInt(ps[i]);
            }
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

    @Override
    public boolean equals(Object o) {
        return o instanceof Version && compareTo((Version) o) == 0;
    }

    @Override
    public int compareTo(Version v) {
        int end = Math.max(this.parts.length, v.parts.length);
        for (int i = 0; i < end; ++i) {
            int x = this.parts.length >= i ? this.parts[i] : 0;
            int y =    v.parts.length >= i ?    v.parts[i] : 0;
            if (x == y) continue;
            if (x < y) return -1;
            else return 1;
        }
        return 0;
    }
}
