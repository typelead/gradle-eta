package com.typelead.gradle.eta.api;

import com.typelead.gradle.utils.VersionRange;

public interface EtaDirectDependency extends EtaDependency, HasPackageName {
    VersionRange getVersionRange();
}
