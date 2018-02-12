package com.typelead.gradle.eta.api;

import com.typelead.gradle.utils.VersionRange;

public interface EtaDependency {
    String getPackageName();
    VersionRange getVersionRange();
}
