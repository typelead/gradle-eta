package com.typelead.gradle.eta.internal;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.Map;
import java.util.Optional;

import com.typelead.gradle.eta.api.EtaDirectDependency;

import com.typelead.gradle.utils.Version;
import com.typelead.gradle.utils.VersionRange;
import com.typelead.gradle.utils.PrintHelper;

public class DefaultEtaDirectDependency implements EtaDirectDependency {

    public static final String PACKAGE_ATTRIBUTE = "package";
    public static final String LOWER_ATTRIBUTE   = "lower";
    public static final String UPPER_ATTRIBUTE   = "upper";
    public static final Set<String> validAttributes =
        new HashSet(Arrays.asList(new String[] { "package", "lower", "upper" }));

    String packageName;
    VersionRange versionRange;

    public DefaultEtaDirectDependency(String packageName, VersionRange versionRange) {
        this.packageName  = packageName;
        this.versionRange = versionRange;
    }

    public static DefaultEtaDirectDependency create(String dependencyConstraint) {
        validateConstraint(dependencyConstraint);
        String[] parts = dependencyConstraint.split(":");
        String packageName = parts[0];
        VersionRange versionRange = VersionRange.create(parts[1]);
        return new DefaultEtaDirectDependency(packageName, versionRange);
    }

    public static void validateConstraint(String dependencyConstraint) {
        String[] parts = dependencyConstraint.split(":");
        if (parts.length != 2) {
            throw new IllegalArgumentException("Invalid Eta dependency constraint: " + dependencyConstraint + "\nA constraint must have exactly one ':'.");
        }
        String packageName = parts[0];
        if (packageName.length() == 0) {
            throw new IllegalArgumentException("Invalid Eta dependency constraint: " + dependencyConstraint + "\nPackage name must have at least one character.");
        }
    }

    public static DefaultEtaDirectDependency create
        (Map<String, String> dependencyConstraintAttributes) {
        validateAttributes(dependencyConstraintAttributes);
        String packageName = dependencyConstraintAttributes.get(PACKAGE_ATTRIBUTE);
        Version lowerBound =
            createVersion(dependencyConstraintAttributes.get(LOWER_ATTRIBUTE));
        Version upperBound =
            createVersion(dependencyConstraintAttributes.get(UPPER_ATTRIBUTE));
        return new DefaultEtaDirectDependency(packageName,
                                        VersionRange.create(lowerBound, upperBound));
    }

    private static void validateAttributes(Map<String, String> dependencyConstraintAttributes) {
        Set<String> collectedAttributes = dependencyConstraintAttributes.keySet();
        Set<String> differenceSet = new HashSet(collectedAttributes);
        differenceSet.removeAll(validAttributes);
        if (differenceSet.size() > 0) {
            throw etaDependencyException(dependencyConstraintAttributes, "Unrecognized attributes in Eta dependency: " + differenceSet.toString());
        }
        if (!collectedAttributes.contains(PACKAGE_ATTRIBUTE)) {
            throw etaDependencyException(dependencyConstraintAttributes, "Missing Eta dependency constraint attribute: 'package'");
        }

        if (dependencyConstraintAttributes.get(PACKAGE_ATTRIBUTE).length() == 0) {
            throw etaDependencyException(dependencyConstraintAttributes, "Invalid Eta dependency constraint attribute: 'package' - Must have at least one character.");
        }

        if (!collectedAttributes.contains(LOWER_ATTRIBUTE) &&
            !collectedAttributes.contains(UPPER_ATTRIBUTE)) {
            throw etaDependencyException(dependencyConstraintAttributes, "Invalid Eta dependency constraint: Must contain at least one of 'lower' or 'upper' attributes.");
        }
    }

    private static IllegalArgumentException etaDependencyException(Map<String, String> dependencyConstraintAttributes, String message) {
        return new IllegalArgumentException(message + "\n" + PrintHelper.toString(dependencyConstraintAttributes));
    }

    private static Version createVersion(String versionString) {
        return Optional.ofNullable(versionString)
                       .map(Version::create)
                       .orElse(null);
    }

    @Override
    public String getPackageName() {
        return packageName;
    }

    @Override
    public VersionRange getVersionRange() {
        return versionRange;
    }

    @Override
    public String toString() {
        return packageName + " " + versionRange.toString();
    }
}
