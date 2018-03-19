package com.typelead.gradle.eta.api;

public class SourceRepository {

    public enum CommitIdentifierType {
        BRANCH, TAG
    }

    private String location;
    private String commitIdentifier;
    private CommitIdentifierType commitIdentifierType;

    public SourceRepository(String location, CommitIdentifierType commitIdentifierType, String commitIdentifier) {
        this.location   = location;
        this.commitIdentifierType = commitIdentifierType;
        this.commitIdentifier     = commitIdentifier;
    }

    public String getLocation() {
        return location;
    }

    public String getCommitIdentifier() {
        return commitIdentifier;
    }

    public CommitIdentifierType getCommitIdentifierType() {
        return commitIdentifierType;
    }
}
