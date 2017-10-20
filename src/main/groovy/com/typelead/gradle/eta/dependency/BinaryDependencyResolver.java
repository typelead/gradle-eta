package com.typelead.gradle.eta.dependency;

import org.gradle.api.Nullable;

public interface BinaryDependencyResolver {

    @Nullable BinaryDependency resolveSystemBinary();

    BinaryDependency resolveLocalBinary(String path);

    BinaryDependency resolveRemoteBinary(String version);
}
