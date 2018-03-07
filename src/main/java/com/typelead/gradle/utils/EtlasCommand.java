package com.typelead.gradle.utils;

import java.io.File;
import java.io.BufferedReader;
import java.io.StringReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import org.gradle.api.Project;
import org.gradle.api.GradleException;
import org.gradle.api.file.ProjectLayout;
import org.gradle.api.provider.Provider;
import org.gradle.api.provider.Property;

import com.typelead.gradle.utils.ResolvedExecutable;
import com.typelead.gradle.eta.api.EtaExtension;
import com.typelead.gradle.eta.api.SourceRepository;
import com.typelead.gradle.eta.plugins.EtaBasePlugin;
import com.typelead.gradle.eta.plugins.EtaPlugin;
import com.typelead.gradle.eta.tasks.EtlasTaskSpec;

public class EtlasCommand {

    private final String projectName;
    private final String projectVersion;
    private final ProjectLayout projectLayout;

    private final Provider<ResolvedExecutable> resolvedEtlas;
    private final Provider<ResolvedExecutable> resolvedEta;
    private final Provider<String> buildDirectory;
    private final Property<File>   workingDirectory;
    private final Provider<Optional<Boolean>> sendMetrics;

    public EtlasCommand(EtlasTaskSpec task) {
        final Project project = task.getProject();

        this.projectName    = project.getName();
        this.projectVersion = project.getVersion().toString();
        this.projectLayout  = project.getLayout();

        this.resolvedEta      = task.getEta();
        this.resolvedEtlas    = task.getEtlas();
        this.buildDirectory   = task.getBuildDirectory();
        this.workingDirectory = project.getObjects().property(File.class);
        this.sendMetrics      = sendMetricsPropertyProvider(project);
    }

    private static Provider<Optional<Boolean>> sendMetricsPropertyProvider
        (final Project project) {
        return project.provider(() -> {
                Object value = project.findProperty
                    (EtaBasePlugin.ETA_SEND_METRICS_PROPERTY);
                if (value == null) {
                    return Optional.empty();
                } else {
                    String stringValue = (String) value;
                    if (value.equals("true") || value.equals("false")) {
                        return Optional.of(Boolean.valueOf(stringValue));
                    } else {
                        throw new GradleException
                            ("Invalid value '" + stringValue +
                             "' for the etaSendMetrics property." +
                             " Must be either 'true' or 'false'.");
                    }
                }
            });
    }

    public Property<File> getWorkingDirectory() {
        return workingDirectory;
    }

    public Optional<Boolean> getSendMetrics() {
        return sendMetrics.get();
    }

    private String getSendMetricsFlag() {
        return sendMetrics.get()
            .map(x -> x.equals(Boolean.TRUE)?
                 "--enable-send-metrics" : "--disable-send-metrics")
            .orElse(null);
    }

    private String getEtaVersionFlag() {
        ResolvedExecutable eta = resolvedEta.get();
        if (eta.isSystem()) {
            return null;
        }
        return "--select-eta=" + eta.getVersion();
    }

    public String getWelcomeMessage() {
        final StringBuffer output = new StringBuffer();
        final StringBuffer error  = new StringBuffer();
        CommandLine c = initCommandLine();
        c.getCommand().add("init");
        c.executeAndConsumeOutputWithProcess((process, outputLine) -> {
                output.append(outputLine + "\n");
                if (outputLine.indexOf("(y/n)") >= 0) {
                    process.destroy();
                    return Boolean.FALSE;
                } else {
                    return Boolean.TRUE;
                }
            }, (process, line) -> {
                error.append(line);
                return Boolean.TRUE;
            });
        return output.toString();
    }

    public String numericVersion() {
        CommandLine c = initCommandLine();
        c.getCommand().add("--numeric-version");
        return c.executeAndGetStandardOutput().trim();
    }

    public String getGlobalEtaVersion() {
        CommandLine c = initCommandLine();
        c.getCommand().addAll(Arrays.asList("exec", "eta", "--", "--numeric-version"));
        String first = c.executeAndGetStandardOutputLines().stream()
            .findFirst().orElse(null);
        if (first == null || first.length() <= 0) {
            throw new GradleException
                ("Unable to get the version of the existing Eta installation.");
        }
        return first;
    }
    public String getLatestEtaVersion() {
        CommandLine c = initCommandLine();
        c.getCommand().addAll(Arrays.asList("select", "--list"));
        String last = c.executeAndGetStandardOutputLines()
                       .stream().reduce((a, b) -> b).orElse(null);
        if (last == null || last.length() <= 0) {
            throw new GradleException
                ("Unable to get the latest available binary version of Eta.");
        }
        return last;
    }

    public void update() {
        CommandLine c = initCommandLine();
        c.getCommand().add("update");
        c.executeAndLogOutput();
    }

    public void installEta() {
        CommandLine c = initCommandLine();
        String versionFlag = getEtaVersionFlag();
        if (versionFlag != null) {
            c.getCommand().add(versionFlag);
        }
        c.getCommand().add("update");
        c.executeAndLogOutput();
    }

    public void newFreeze(Set<String>           dependencyConstraints,
                          Set<SourceRepository> sourceRepositories) {

        CabalHelper.generateCabalFile(projectName, projectVersion,
                                      dependencyConstraints, workingDirectory.get());

        CabalHelper.generateCabalProjectFile(sourceRepositories, workingDirectory.get());

        CommandLine c = initCommandLine();

        String versionFlag = getEtaVersionFlag();
        if (versionFlag != null) {
            c.getCommand().add(versionFlag);
        }
        c.getCommand().add( "new-freeze");
        c.executeAndLogOutput();
    }

    /**
     * This will also download dependencies via `etlas install --dependencies-only`
     */
    public List<String> depsClasspath(String component) {
        return defaultCommandLine("deps", component, "--classpath")
                  .executeAndGetStandardOutputLines()
                  .stream()
                  .filter(line -> !line.startsWith(" ")
                               && !line.contains("Notice:")
                               && line.contains(File.separator))
                  .collect(Collectors.toList());
    }

    private CommandLine initCommandLine() {
        CommandLine c = new CommandLine(resolvedEtlas.get().getPath());
        // TODO: Remove this workingDir business
        c.setWorkingDir(workingDirectory.getOrNull() == null?
                        projectLayout.getProjectDirectory().getAsFile()
                        : workingDirectory.get());
        String sendMetrics = getSendMetricsFlag();
        if (sendMetrics != null) {
            c.getCommand().add(sendMetrics);
        }
        return c;
    }

    private CommandLine defaultCommandLine(String command, String... args) {
        return defaultCommandLine(command, Arrays.asList(args));
    }

    private void defaultCommand(String command, String... args) {
        defaultCommand(command, Arrays.asList(args));
    }

    private void defaultCommand(String command, List<String> args) {
        defaultCommandLine(command, args).executeAndLogOutput();
    }

    private CommandLine defaultCommandLine(String command, List<String> args) {
        CommandLine c = initCommandLine();
        c.getCommand().add(command);
        List<String> commandArgs = new ArrayList<>(args);
        commandArgs.add("--builddir=" + buildDirectory.get());
        c.getCommand().addAll(commandArgs);
        return c;
    }

}
