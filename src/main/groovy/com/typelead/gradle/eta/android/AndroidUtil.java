package com.typelead.gradle.eta.android;

import org.gradle.api.Plugin;
import org.gradle.api.Project;

import com.android.build.gradle.BaseExtension;

import java.util.Arrays;
import java.util.Optional;
import java.util.stream.Stream;

public class AndroidUtil {
    public static Optional<Plugin> getAndroidPlugin(Project project) {
        String[] androidPluginIds =
            { "com.android.application"
            , "android"
            , "com.android.library"
            , "android-library"
            , "com.android.model.application"
            , "com.android.model.library" };
        return Arrays.stream(androidPluginIds)
                     .map(id -> project.getPlugins().findPlugin(id))
                     .filter(plugin -> plugin != null)
                     .findFirst();
    }

    public static BaseExtension getAndroidExtension(Project project) {
        return (BaseExtension) project.getExtensions().getByName("android");
    }
}
