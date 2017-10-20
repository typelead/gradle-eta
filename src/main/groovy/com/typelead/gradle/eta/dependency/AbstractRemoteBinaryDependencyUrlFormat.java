package com.typelead.gradle.eta.dependency;

import org.gradle.api.GradleException;

import java.net.MalformedURLException;
import java.net.URL;

public abstract class AbstractRemoteBinaryDependencyUrlFormat implements RemoteBinaryDependencyUrlFormat {

    abstract String getUrlString(String version, String arch);

    @Override
    public URL getUrl(String version, String arch) {
        String url = getUrlString(version, arch);
        try {
            return new URL(url);
        } catch (MalformedURLException e) {
            throw new GradleException("Malformed eta binary url '" + url + "'", e);
        }
    }
}
