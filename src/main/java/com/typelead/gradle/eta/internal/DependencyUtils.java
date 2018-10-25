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
import com.typelead.gradle.utils.QuadConsumer;

public class DependencyUtils {
    public static void foldEtaDependencies
        (final Project project,
         final Collection<EtaDependency> dependencies,
         final QuadConsumer<List<String>, List<String>, List<String>, Set<SourceRepository>>
           directProjectConsumer) {

        List<String> directDependencies = new ArrayList<>();
        List<String> projectDependencies = new ArrayList<>();
        List<String> gitStringDependencies = new ArrayList<>();
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
                final EtaGitDependency gitDep = (EtaGitDependency) dependency;
                gitDependencies.add(gitDep.getSourceRepository());
                gitStringDependencies.add(gitDep.getPackageName());
            }
        }

        if (directProjectConsumer != null) {
            directProjectConsumer.accept(directDependencies, projectDependencies,
                                         gitStringDependencies, gitDependencies);
        }
    }

    private static boolean isEtaProject(final Project project) {
        return project.getPlugins().hasPlugin(EtaPlugin.class);
    }
}
