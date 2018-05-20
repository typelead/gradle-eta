package com.typelead.gradle.eta.internal;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.Map;
import java.util.Optional;

import org.gradle.api.Project;

import com.typelead.gradle.utils.Version;
import com.typelead.gradle.utils.VersionRange;
import com.typelead.gradle.eta.api.EtaDirectDependency;

public class DefaultEtaDirectDependency extends AbstractEtaDependency
    implements EtaDirectDependency {

    String packageName;
    VersionRange versionRange;

    public DefaultEtaDirectDependency(Project project, String packageName,
                                      VersionRange versionRange) {
        super(project);
        this.packageName  = packageName;
        this.versionRange = versionRange;
    }

    public static DefaultEtaDirectDependency create
        (Project project, String dependencyConstraint) {
        validateConstraint(dependencyConstraint);
        String[] parts = dependencyConstraint.split(":");
        String packageName = parts[0];
        VersionRange versionRange;
        if (parts.length > 1) {
            versionRange = VersionRange.create(parts[1]);
        } else {
            versionRange = VersionRange.anyVersion();
        }
        return new DefaultEtaDirectDependency(project, packageName, versionRange);
    }

    public static void validateConstraint(String dependencyConstraint) {
        String[] parts = dependencyConstraint.split(":");
        if (parts.length > 2) {
            throw new IllegalArgumentException("Invalid Eta dependency constraint: " + dependencyConstraint + "\nA constraint must have exactly one ':'.");
        }
        String packageName = parts[0];
        if (packageName.length() == 0) {
            throw new IllegalArgumentException("Invalid Eta dependency constraint: " + dependencyConstraint + "\nPackage name must have at least one character.");
        }
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
