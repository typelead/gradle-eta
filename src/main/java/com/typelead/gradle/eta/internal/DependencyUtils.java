package com.typelead.gradle.eta.internal;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.function.Consumer;
import java.util.function.BiConsumer;

import org.gradle.api.Project;
import org.gradle.api.tasks.SourceSet;

import com.typelead.gradle.eta.api.EtaDependency;
import com.typelead.gradle.eta.api.EtaGitDependency;
import com.typelead.gradle.eta.api.EtaDirectDependency;
import com.typelead.gradle.eta.api.EtaProjectDependency;
import com.typelead.gradle.eta.api.NamingScheme;
import com.typelead.gradle.eta.api.SourceRepository;
import com.typelead.gradle.eta.plugins.EtaPlugin;

public class DependencyUtils {
    public static void foldEtaDependencies
        (final Project project,
         Collection<EtaDependency> dependencies,
         BiConsumer<List<String>, List<String>> directProjectConsumer,
         Consumer<Set<SourceRepository>> gitConsumer) {

        List<String> directDependencies = new ArrayList<>();
        List<String> projectDependencies = new ArrayList<>();
        Set<SourceRepository> gitDependencies = new LinkedHashSet<>();

        for (EtaDependency dependency : dependencies) {
            if (dependency instanceof EtaDirectDependency) {
                directDependencies.add(((EtaDirectDependency) dependency).toString());
            } else if (dependency instanceof EtaProjectDependency) {
                final Project targetProject =
                    ((EtaProjectDependency) dependency).getProject(project);
                if (isEtaProject(targetProject)) {
                    projectDependencies.add
                        (NamingScheme.getPackageName(targetProject,
                                                     SourceSet.MAIN_SOURCE_SET_NAME));
                }
            } else if (dependency instanceof EtaGitDependency) {
                gitDependencies.add
                    (((EtaGitDependency) dependency).getSourceRepository());
            }
        }

        if (directProjectConsumer != null) {
            directProjectConsumer.accept(directDependencies, projectDependencies);
        }

        if (gitConsumer != null) {
            gitConsumer.accept(gitDependencies);
        }
    }

    private static boolean isEtaProject(final Project project) {
        return project.getPlugins().hasPlugin(EtaPlugin.class);
    }
}
