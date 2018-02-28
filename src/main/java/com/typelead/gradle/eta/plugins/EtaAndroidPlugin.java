package com.typelead.gradle.eta.plugins;

import java.util.Optional;

import javax.inject.Inject;

import org.gradle.api.Plugin;
import org.gradle.api.Project;
import org.gradle.api.ProjectConfigurationException;
import org.gradle.api.NamedDomainObjectContainer;
import org.gradle.api.file.FileCollection;
import org.gradle.api.logging.Logger;
import org.gradle.api.tasks.compile.JavaCompile;
import org.gradle.api.artifacts.Configuration;
import org.gradle.api.internal.plugins.DslObject;
import org.gradle.api.file.ConfigurableFileTree;

import org.gradle.api.internal.file.SourceDirectorySetFactory;
import org.gradle.api.internal.tasks.DefaultSourceSet;

import com.android.build.gradle.BasePlugin;
import com.android.build.gradle.BaseExtension;
import com.android.build.gradle.api.AndroidSourceSet;
import com.android.builder.model.SourceProvider;

import com.android.build.gradle.internal.api.DefaultAndroidSourceSet;

import com.typelead.gradle.utils.EtaRuntimeUtils;
import com.typelead.gradle.utils.ExtensionHelper;
import com.typelead.gradle.eta.tasks.EtaCompile;
import com.typelead.gradle.eta.android.AndroidHelper;
import com.typelead.gradle.utils.EtlasCommand;
import com.typelead.gradle.eta.api.EtaSourceSet;
import com.typelead.gradle.eta.api.EtaConfiguration;
import com.typelead.gradle.eta.api.EtaOptions;
import com.typelead.gradle.eta.api.Language;
import com.typelead.gradle.eta.api.LanguageExtension;
import com.typelead.gradle.eta.internal.DefaultEtaSourceSet;

import org.gradle.api.artifacts.ResolutionStrategy;
import org.gradle.api.artifacts.ModuleVersionSelector;


/**
 * A {@link Plugin} which sets up an Eta project.
 */
public class EtaAndroidPlugin extends EtaBasePlugin implements Plugin<Project> {

    public static final String ETA_SOURCE_SET_NAME              = "eta";
    public static final String ETA_SOURCE_SET_DSL_NAME          = "eta";
    public static final String ETA_OPTIONS_DSL_NAME             = "etaOptions";

    private final SourceDirectorySetFactory sourceDirectorySetFactory;
    private BasePlugin androidPlugin;
    private BaseExtension androidExtension;

    @Inject
    public EtaAndroidPlugin(SourceDirectorySetFactory sourceDirectorySetFactory) {
        this.sourceDirectorySetFactory = sourceDirectorySetFactory;
    }

    @Override
    public void configureBeforeEvaluate() {
        androidPlugin =
            AndroidHelper.getAndroidPlugin(project)
            .orElseThrow(() -> new ProjectConfigurationException("Please apply an Android plugin before applying the 'eta-android' plugin.", null));
        androidExtension = AndroidHelper.getAndroidExtension(project);

        configureEtaSourceSetConvention();
        addEtaOptionsToDefaultConfig();
    }

    @Override
    public void configureAfterEvaluate() {}

    private void configureEtaSourceSetConvention() {
        androidExtension.getSourceSets().all(sourceSet -> {
                project.getLogger().debug("Creating EtaSourceSet for source set " + sourceSet);
                EtaSourceSet etaSourceSet =
                    ExtensionHelper.createExtension(sourceSet,
                        ETA_SOURCE_SET_DSL_NAME, DefaultEtaSourceSet.class,
                        ETA_SOURCE_SET_NAME,
                        ((DefaultAndroidSourceSet) sourceSet).getDisplayName(),
                        sourceDirectorySetFactory);
                etaSourceSet.getEta().srcDir("src/" + sourceSet.getName() + "/eta");
            });
    }

    private void addEtaOptionsToDefaultConfig() {
        ExtensionHelper.createExtension(androidExtension, ETA_OPTIONS_DSL_NAME,
                                        EtaOptions.class,
                                        project.container(Language.class),
                                        project.container(LanguageExtension.class));
    }
}
