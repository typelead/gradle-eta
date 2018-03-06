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

import com.typelead.gradle.eta.api.SourceRepository;
import com.typelead.gradle.eta.tasks.EtlasTaskSpec;
import com.typelead.gradle.eta.api.EtaExtension;
import com.typelead.gradle.eta.plugins.EtaPlugin;
import com.typelead.gradle.eta.plugins.EtaBasePlugin;

public class EtlasCommand {

    private final Project project;
    private final String etlasBinary;
    private String etlasVersion;
    private String etaVersion;
    private final List<String> etlasFlags;
    private final List<String> buildFlags;
    private final String buildDir;
    private final List<String> components;
    private final Optional<Boolean> sendMetrics;
    private final String workingDir;

    public EtlasCommand(Project project, EtaExtension extension) {
        this.project           = project;
        this.etlasBinary       = extension.getEtlasBinary();
        this.etlasVersion      = extension.getEtlasVersion();
        this.etaVersion        = extension.getVersion();
        this.etlasFlags        = extension.getEtlasFlags();
        this.buildFlags        = extension.getBuildFlags();
        /* TODO: Is this correct? */
        this.buildDir          = null;
        this.components        = Collections.emptyList();
        this.sendMetrics       = readSendMetricsProperty(project);
        this.workingDir        = null;
    }

    public EtlasCommand(EtlasTaskSpec task) {
        this(task, null);
    }

    public EtlasCommand(EtlasTaskSpec task, String workingDir) {
        this.project           = task.getProject();
        this.etlasBinary       = task.getEtlasBinary();
        this.etlasVersion      = task.getEtlasVersion();
        this.etaVersion        = task.getEtaVersion();
        this.etlasFlags        = task.getEtlasFlags();
        this.buildFlags        = task.getBuildFlags();
        this.buildDir          = task.getBuildDir();
        this.components        = task.getComponents();
        this.sendMetrics       = readSendMetricsProperty(project);
        this.workingDir        = workingDir;
    }

    public void setEtlasVersion(String etlasVersion) {
        this.etlasVersion = etlasVersion;
    }

    public void setEtaVersion(String etaVersion) {
        this.etaVersion = etaVersion;
    }

    public void configure(List<String> flags) {
        commandWithComponent("configure", flags);
    }

    public void reconfigure(List<String> flags) {
        commandWithComponent("reconfigure", flags);
    }

    public void enableTests() {
        reconfigure(Collections.singletonList("--enable-tests"));
    }

    public void clean() {
        commandWithComponent("clean");
    }

    public void build() {
        commandWithComponent("build");
    }

    public void update() {
        CommandLine c = initCommandLine();
        c.getCommand().add("update");
        c.executeAndLogOutput();
    }

    public void installEta() {
        CommandLine c = initCommandLine();
        c.getCommand().addAll(Arrays.asList("--select-eta=" + etaVersion, "update"));
        c.executeAndLogOutput();
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

    public String getLatestEtaVersion() {
        CommandLine c = initCommandLine();
        c.getCommand().addAll(Arrays.asList("select", "--list"));
        String last = c.executeAndGetStandardOutputLines()
                       .stream().reduce((a, b) -> b).orElse(null);
        if (last == null || last.length() <= 0) {
            throw new GradleException("Unable to get the latest available binary version of Eta.");
        }
        return last;
    }

    public void newFreeze(Set<String>           dependencyConstraints,
                          Set<SourceRepository> sourceRepositories) {
        String projectName    = project.getName();
        String projectVersion = project.getVersion().toString();

        CabalHelper.generateCabalFile(projectName, projectVersion,
                                      dependencyConstraints, workingDir);

        CabalHelper.generateCabalProjectFile(sourceRepositories, workingDir);

        CommandLine c = initCommandLine();
        c.getCommand().addAll(Arrays.asList("new-freeze"));
        c.executeAndLogOutput();
    }

    public void test(List<String> testFlags) {
        commandWithComponent("test", testFlags);
    }

    public void installDependenciesOnly() {
        defaultCommand("install", Collections.singletonList("--dependencies-only"));
    }

    public void installTestDependenciesOnly() {
        defaultCommand("install", Arrays.asList("--dependencies-only", "--enable-tests"));
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

    private void commandWithComponent(String command, List<String> commandArgs) {
        List<String> args = new ArrayList<>(commandArgs);
        args.addAll(components);
        defaultCommand(command, args);
    }

    private void commandWithComponent(String command, String... commandArgs) {
        commandWithComponent(command, Arrays.asList(commandArgs));
    }

    private CommandLine initCommandLine() {
        CommandLine c = new CommandLine(etlasBinary);
        c.setWorkingDir(workingDir == null? project.getProjectDir().getAbsolutePath()
                                          : workingDir);
        c.getCommand().addAll(etlasFlags);
        String sendMetrics = getSendMetricsFlag();
        if (sendMetrics != null) {
            c.getCommand().add(sendMetrics);
        }
        return c;
    }

    private CommandLine defaultCommandLine(String command, List<String> args) {
        CommandLine c = initCommandLine();
        c.getCommand().add(command);
        List<String> commandArgs = new ArrayList<>(args);
        commandArgs.addAll(buildFlags);
        commandArgs.add("--builddir=" + buildDir);
        c.getCommand().addAll(commandArgs);
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

    public Optional<Boolean> getSendMetrics() {
        return sendMetrics;
    }

    private String getSendMetricsFlag() {
        return sendMetrics.map(x -> x.equals(Boolean.TRUE)?
                               "--enable-send-metrics" : "--disable-send-metrics")
                          .orElse(null);
    }

    private Optional<Boolean> readSendMetricsProperty(Project project) {
        Object value = project.findProperty(EtaBasePlugin.ETA_SEND_METRICS_PROPERTY);
        if (value == null) {
            return Optional.empty();
        } else {
            String stringValue = (String) value;
            if (value.equals("true") || value.equals("false")) {
                return Optional.of(Boolean.valueOf(stringValue));
            } else {
                throw new GradleException("Invalid value '" + stringValue + "' for the etaSendMetrics property. Must be either 'true' or 'false'.");
            }
        }
    }
}
