package com.typelead.gradle.eta.dependency;

import org.gradle.api.Nullable;

import java.net.URL;

public interface BinaryDependencyCache {

    @Nullable String getBinaryPathForVersion(String version);

    @Nullable String putBinaryForVersion(String version, URL url);
}
