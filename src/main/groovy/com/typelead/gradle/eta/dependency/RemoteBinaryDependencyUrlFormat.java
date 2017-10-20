package com.typelead.gradle.eta.dependency;

import java.net.URL;

public interface RemoteBinaryDependencyUrlFormat {
    URL getUrl(String version, String arch);
}
