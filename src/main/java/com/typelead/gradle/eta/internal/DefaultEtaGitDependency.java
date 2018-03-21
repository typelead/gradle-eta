package com.typelead.gradle.eta.internal;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.gradle.api.Project;

import com.typelead.gradle.utils.PrintHelper;
import com.typelead.gradle.eta.api.EtaGitDependency;
import com.typelead.gradle.eta.api.SourceRepository;
import static com.typelead.gradle.eta.api.SourceRepository.CommitIdentifierType;
import static com.typelead.gradle.eta.api.SourceRepository.CommitIdentifierType.*;

public class DefaultEtaGitDependency extends AbstractEtaDependency
    implements EtaGitDependency {

    private final String packageName;
    private final SourceRepository sourceRepository;

    public static final String PACKAGE_ATTRIBUTE  = "package";
    public static final String LOCATION_ATTRIBUTE = "location";
    public static final String COMMIT_ATTRIBUTE   = "commit";
    public static final String BRANCH_ATTRIBUTE   = "branch";
    public static final String TAG_ATTRIBUTE      = "tag";
    public static final Set<String> validAttributes =
        new HashSet(Arrays.asList(PACKAGE_ATTRIBUTE, LOCATION_ATTRIBUTE,
                                  COMMIT_ATTRIBUTE, BRANCH_ATTRIBUTE, TAG_ATTRIBUTE));

    public DefaultEtaGitDependency(Project project, String packageName,
                                   SourceRepository sourceRepository) {
        super(project);
        this.packageName = packageName;
        this.sourceRepository = sourceRepository;
    }

    public static DefaultEtaGitDependency create
        (Project project, Map<String, String> attributes) {
        validateAttributes(attributes);
        String packageName = attributes.get(PACKAGE_ATTRIBUTE);
        String location    = attributes.get(LOCATION_ATTRIBUTE);
        String commit      = attributes.get(COMMIT_ATTRIBUTE);
        String branch      = attributes.get(BRANCH_ATTRIBUTE);
        String tag         = attributes.get(TAG_ATTRIBUTE);
        CommitIdentifierType commitIdType;
        String commitId;
        if (commit != null) {
            commitIdType = COMMIT;
            commitId = commit;
        } else if (tag != null) {
            commitIdType = TAG;
            commitId = tag;
        } else {
            commitIdType = BRANCH;
            commitId = branch;
        }
        return new DefaultEtaGitDependency(project, packageName,
                                           new SourceRepository(location,
                                                                commitIdType,
                                                                commitId));
    }

    private static void validateAttributes(Map<String, String> attributes) {
        Set<String> collectedAttributes = attributes.keySet();
        Set<String> differenceSet = new HashSet(collectedAttributes);
        differenceSet.removeAll(validAttributes);
        if (differenceSet.size() > 0) {
            throw etaDependencyException(attributes, "Unrecognized attributes in Eta Git dependency: " + differenceSet.toString());
        }
        if (!collectedAttributes.contains(PACKAGE_ATTRIBUTE)) {
            throw etaDependencyException(attributes, "Missing Eta Git dependency attribute: 'package'");
        }

        if (attributes.get(PACKAGE_ATTRIBUTE).length() == 0) {
            throw etaDependencyException(attributes, "Invalid Eta Git dependency attribute: 'package' - Must have at least one character.");
        }

        if (!collectedAttributes.contains(LOCATION_ATTRIBUTE)) {
            throw etaDependencyException(attributes, "Missing Eta Git dependency attribute: 'location'");
        }

        if (attributes.get(LOCATION_ATTRIBUTE).length() == 0) {
            throw etaDependencyException(attributes, "Invalid Eta Git dependency attribute: 'location' - Must have at least one character.");
        }

        if (!collectedAttributes.contains(COMMIT_ATTRIBUTE) &&
            !collectedAttributes.contains(TAG_ATTRIBUTE) &&
            !collectedAttributes.contains(BRANCH_ATTRIBUTE)) {
            throw etaDependencyException(attributes, "Invalid Eta Git dependency: Must contain at least one of 'commit', 'tag', or 'branch' attributes.");
        }
    }

    private static IllegalArgumentException etaDependencyException
        (Map<String, String> attributes, String message) {
        return new IllegalArgumentException
            (message + "\n" + PrintHelper.toString(attributes));
    }


    @Override
    public String getPackageName() {
        return packageName;
    }

    @Override
    public SourceRepository getSourceRepository() {
        return sourceRepository;
    }

    @Override
    public String toString() {
        return packageName;
    }
}
