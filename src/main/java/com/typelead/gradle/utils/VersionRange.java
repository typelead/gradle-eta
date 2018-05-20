package com.typelead.gradle.utils;

import java.util.Optional;

public class VersionRange {

    private Version lowerBound;
    private boolean lowerStrict;
    private Version upperBound;
    private boolean upperStrict;

    public VersionRange(Version lowerBound, Version upperBound) {
        this.lowerBound = lowerBound;
        this.upperBound = upperBound;
    }

    public VersionRange(Version lowerBound, boolean lowerStrict, Version upperBound, boolean upperStrict) {
        this.lowerBound  = lowerBound;
        this.lowerStrict = lowerStrict;
        this.upperBound  = upperBound;
        this.upperStrict = upperStrict;
    }

    private static final String SYMBOLS_STRING = "[]()";

    private static final VersionRange anyVersion = new VersionRange(null, null);

    public static VersionRange anyVersion() {
        return anyVersion;
    }

    public static VersionRange create(String versionRange) {
        Version lowerBound  = null;
        Version upperBound  = null;
        boolean lowerStrict = false;
        boolean upperStrict = false;
        char first = versionRange.charAt(0);
        if (SYMBOLS_STRING.indexOf(first) >= 0) {
            char last = versionRange.charAt(versionRange.length() - 1);
            if (SYMBOLS_STRING.indexOf(last) >= 0) {
                String   inner      = versionRange.substring(1,versionRange.length() - 1);
                String[] lowerUpper = inner.split(",");
                boolean lastComma = inner.charAt(inner.length() - 1) == ',';
                if (lowerUpper.length == 2 || lastComma) {
                    String lower = lowerUpper[0];
                    String upper = lastComma? "" : lowerUpper[1];
                    switch (first) {
                        case ')':
                            throw versionRangeException(versionRange, "')' cannot start a version range.");
                        case '(':
                            if (lower.length() > 0) {
                                throw versionRangeException(versionRange, "Expected a ',' to come after a '('.");
                            }
                            break;
                        case ']':
                            lowerStrict = true;
                            break;
                    }
                    switch (last) {
                        case '(':
                            throw versionRangeException(versionRange, "'(' cannot end a version range.");
                        case ')':
                            if (upper.length() > 0) {
                                throw versionRangeException(versionRange, "Expected a ',' to precede a ')'.");
                            }
                            break;
                        case '[':
                            upperStrict = true;
                            break;
                    }
                    if (lower.length() > 0) {
                        lowerBound = Version.create(lower);
                    }
                    if (upper.length() > 0) {
                        upperBound = Version.create(upper);
                    }
                } else {
                    throw versionRangeException(versionRange, "Expected a single occurrence of ','.");
                }
            } else {
                throw versionRangeException(versionRange, "Expected last character to be ']', '[', or ')'.");
            }
        } else if(versionRange.endsWith(".+")) {
            lowerBound = Version.create(versionRange.substring(0, versionRange.length() - 2));
            upperBound = lowerBound.increment();
            upperStrict = true;
        } else {
            lowerBound = Version.create(versionRange);
            upperBound = lowerBound;
        }
        return new VersionRange(lowerBound, lowerStrict, upperBound, upperStrict);
    }

    private static IllegalArgumentException versionRangeException(String versionRange, String message) {
        return new IllegalArgumentException("Invalid Version Range: '" + versionRange + "' - " + message);
    }

    public static VersionRange create(Version lowerBound, Version upperBound) {
        return new VersionRange(lowerBound, upperBound);
    }

    public Version getLowerBound() {
        return lowerBound;
    }

    public Version getUpperBound() {
        return upperBound;
    }

    @Override
    public String toString() {
        Optional<Version> lowerBound = Optional.ofNullable(this.lowerBound);
        Optional<Version> upperBound = Optional.ofNullable(this.upperBound);
        if (lowerBound.equals(upperBound)) {
            if (lowerBound.isPresent()) {
                return "== " + lowerBound.get().toString();
            } else {
                return "";
            }
        } else {
            return lowerBound.map(version -> ">" + (lowerStrict? "" : "=")
                                                 + " " + version.toString()).orElse("")
                 + ((lowerBound.isPresent() && upperBound.isPresent())? " && " : "")
                 + upperBound.map(version -> "<" + (upperStrict? "" : "=") + " "
                                                 + version.toString()).orElse("");
        }

    }
}
