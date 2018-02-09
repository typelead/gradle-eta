package com.typelead.gradle.eta.plugins;

import java.util.Optional;

import org.gradle.api.Plugin;
import org.gradle.api.Project;
import org.gradle.api.ProjectConfigurationException;
import org.gradle.api.NamedDomainObjectContainer;
import org.gradle.api.file.FileCollection;
import org.gradle.api.logging.Logger;
import org.gradle.api.tasks.compile.JavaCompile;
import org.gradle.api.artifacts.Configuration;

import com.android.build.gradle.BasePlugin;
import com.android.build.gradle.BaseExtension;
import com.android.build.gradle.api.AndroidSourceSet;
import com.android.build.gradle.internal.core.GradleVariantConfiguration;
import com.android.build.gradle.internal.variant.BaseVariantData;

import com.typelead.gradle.utils.EtaRuntimeUtils;
import com.typelead.gradle.eta.tasks.EtaCompile;
import com.typelead.gradle.eta.android.AndroidUtil;
import com.typelead.gradle.utils.EtlasCommand;

/**
 * A {@link Plugin} which sets up an Eta project.
 */
public class EtaAndroidPlugin extends EtaBasePlugin implements Plugin<Project> {

    BasePlugin androidPlugin;
    Configuration etaImpl;
    FileCollection etaDeps;


    @Override
    public void apply(Project project) {
        super.apply(project);
        androidPlugin =
            AndroidUtil.getAndroidPlugin(project)
            .orElseThrow(() -> new ProjectConfigurationException("Please apply an Android plugin before applying the 'eta-android' plugin.", null));
        project.getLogger().lifecycle("Hello from Eta Android!");

        // BaseExtension androidExtension = androidPlugin.getExtension();
        // NamedDomainObjectContainer<AndroidSourceSet>
        //     sourceSetsContainer = androidExtension.getSourceSets();
    }

    @Override
    public void configureBeforeEvaluate() {
    }

    @Override
    public void configureAfterEvaluate() {
        project.getLogger().lifecycle("Hello from Eta Android After Evaluate!");
        String etaImplName = "implementationEta";
        etaImpl = project.getConfigurations().maybeCreate(etaImplName);
        etaDeps = EtaRuntimeUtils.getRuntimeFileCollection(project, new EtlasCommand(project, extension), "lib:eta-application");
        project.getDependencies().add(etaImplName, etaDeps);
        Configuration impl = project.getConfigurations().findByName("implementation");
        impl.extendsFrom(etaImpl);
        androidPlugin.getVariantManager().getVariantScopes()
            .forEach(variantScope -> {
                    BaseVariantData variantData = variantScope.getVariantData();
                    GradleVariantConfiguration config =
                        variantData.getVariantConfiguration();
                    // project.getLogger().lifecycle(config.getFullName());
                    // project.getLogger().lifecycle(config.getBaseName());
                    // project.getLogger().lifecycle(config.getDirName());
                    // project.getLogger().lifecycle(config.getDefaultSourceSet().getName());
                    // project.getLogger().lifecycle(config.getDefaultSourceSet().getJavaDirectories().toString());

                    JavaCompile javacTask = variantData.javacTask;
                    if (javacTask != null) {
                        // project.getLogger().lifecycle(javacTask.getName());
                        // project.getLogger().lifecycle(javacTask.getSource().getFiles().toString());
                        // project.getLogger().lifecycle(javacTask.getDestinationDir().toString());

                        String etaCompileTaskName =
                            javacTask.getName().replace("JavaWithJavac", "Eta");
                        EtaCompile etaCompileTask =
                            project.getTasks().create(etaCompileTaskName, EtaCompile.class);
                        etaCompileTask.configureWithExtension(extension);
                        javacTask.dependsOn(etaCompileTask);
                        javacTask.getClasspath().add(etaDeps);
                        project.getLogger().lifecycle(javacTask.getClasspath().getFiles().toString());
                    }
                });
    }
}
