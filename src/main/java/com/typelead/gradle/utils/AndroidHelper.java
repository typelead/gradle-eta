package com.typelead.gradle.eta.android;

import java.io.File;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

import org.gradle.api.Plugin;
import org.gradle.api.Project;
import org.gradle.api.Action;

import com.android.build.gradle.BasePlugin;
import com.android.build.gradle.BaseExtension;
import com.android.build.gradle.AppExtension;
import com.android.build.gradle.LibraryExtension;
import com.android.build.gradle.FeatureExtension;
import com.android.build.gradle.TestExtension;
import com.android.build.gradle.TestedExtension;
import com.android.build.gradle.api.BaseVariant;

public class AndroidHelper {
    public static Optional<BasePlugin> getAndroidPlugin(Project project) {
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
                 .map(plugin -> (BasePlugin) plugin)
                 .findFirst();
    }

    public static BaseExtension getAndroidExtension(Project project) {
        return (BaseExtension) project.getExtensions().getByName("android");
    }

    public static void forEachVariant(BaseExtension extension, Action<BaseVariant> action) {
        if (extension instanceof AppExtension) {
            ((AppExtension)extension).getApplicationVariants().all(action);
        } else if (extension instanceof LibraryExtension) {
            ((LibraryExtension)extension).getLibraryVariants().all(action);
            if (extension instanceof FeatureExtension) {
                ((FeatureExtension)extension).getFeatureVariants().all(action);
            }
        } else if (extension instanceof TestExtension) {
            ((TestExtension)extension).getApplicationVariants().all(action);
        }

        if (extension instanceof TestedExtension) {
            ((TestedExtension)extension).getTestVariants().all(action);
            ((TestedExtension)extension).getUnitTestVariants().all(action);
        }
    }

    public static List<File> getAndroidSDKClasspath(BaseExtension extension) {
        return extension.getBootClasspath();
    }
}
