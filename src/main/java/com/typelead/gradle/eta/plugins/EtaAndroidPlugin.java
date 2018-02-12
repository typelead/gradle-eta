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
import com.typelead.gradle.eta.android.AndroidUtil;
import com.typelead.gradle.utils.EtlasCommand;
import com.typelead.gradle.eta.api.EtaSourceSet;
import com.typelead.gradle.eta.api.EtaConfiguration;
import com.typelead.gradle.eta.api.EtaOptions;
import com.typelead.gradle.eta.api.Language;
import com.typelead.gradle.eta.api.LanguageExtension;
import com.typelead.gradle.eta.internal.DefaultEtaConfiguration;
import com.typelead.gradle.eta.internal.DefaultEtaSourceSet;
import com.typelead.gradle.eta.internal.DefaultEtaDependencyHandler;

import org.gradle.api.artifacts.ResolutionStrategy;
import org.gradle.api.artifacts.ModuleVersionSelector;


/**
 * A {@link Plugin} which sets up an Eta project.
 */
public class EtaAndroidPlugin extends EtaBasePlugin implements Plugin<Project> {

    public static final String ETA_SOURCE_SET_NAME              = "eta";
    public static final String ETA_SOURCE_SET_DSL_NAME          = "eta";
    public static final String ETA_OPTIONS_DSL_NAME             = "etaOptions";
    public static final String ETA_DEPENDENCY_HANDLER_DSL_NAME  = "eta";
    public static final String ETA_CONFIGURATION_EXTENSION_NAME = "eta";

    private final SourceDirectorySetFactory sourceDirectorySetFactory;
    private BasePlugin androidPlugin;
    private BaseExtension androidExtension;
    private Configuration etaImpl;
    private FileCollection etaDeps;

    @Inject
    public EtaAndroidPlugin(SourceDirectorySetFactory sourceDirectorySetFactory) {
        this.sourceDirectorySetFactory = sourceDirectorySetFactory;
    }

    @Override
    public void apply(Project project) {
        super.apply(project);
        androidPlugin =
            AndroidUtil.getAndroidPlugin(project)
            .orElseThrow(() -> new ProjectConfigurationException("Please apply an Android plugin before applying the 'eta-android' plugin.", null));
        androidExtension = AndroidUtil.getAndroidExtension(project);

        configureEtaSourceSetConvention();
        addEtaExtensionForConfigurations();
        addEtaOptionsToDefaultConfig();
    }

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

    private void addEtaExtensionForConfigurations() {
        ExtensionHelper.createExtension(project.getDependencies(),
                                        ETA_DEPENDENCY_HANDLER_DSL_NAME,
                                        DefaultEtaDependencyHandler.class,
                                        project.getConfigurations());
        project.getConfigurations().all(configuration ->
            ExtensionHelper.createExtension(configuration,
                ETA_CONFIGURATION_EXTENSION_NAME, DefaultEtaConfiguration.class)
        );
    }

    private void addEtaOptionsToDefaultConfig() {
        ExtensionHelper.createExtension(androidExtension, ETA_OPTIONS_DSL_NAME,
                                        EtaOptions.class,
                                        project.container(Language.class),
                                        project.container(LanguageExtension.class));
    }

    @Override
    public void configureBeforeEvaluate() {
    }

    @Override
    public void configureAfterEvaluate() {
    }
}
