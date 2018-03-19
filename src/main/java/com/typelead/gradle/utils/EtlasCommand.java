package com.typelead.gradle.utils;

import java.io.File;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.BiConsumer;
import java.util.stream.Collectors;

import org.gradle.api.Project;
import org.gradle.api.GradleException;
import org.gradle.api.provider.Provider;
import org.gradle.api.provider.Property;

import com.typelead.gradle.utils.ResolvedExecutable;
import com.typelead.gradle.eta.api.EtaExtension;

public class EtlasCommand {

    public static final String ETA_SEND_METRICS_PROPERTY = "etaSendMetrics";

    private final Provider<ResolvedExecutable> resolvedEtlas;
    private final Provider<ResolvedExecutable> resolvedEta;
    private final Property<File> workingDirectory;
    private final Provider<Optional<Boolean>> sendMetrics;

    public EtlasCommand(final Project project) {
        final EtaExtension extension =
            project.getRootProject().getExtensions().findByType(EtaExtension.class);

        this.resolvedEta      = extension.getEta();
        this.resolvedEtlas    = extension.getEtlas();
        this.workingDirectory = project.getObjects().property(File.class);
        this.sendMetrics      = sendMetricsPropertyProvider(project);
    }

    private static Provider<Optional<Boolean>> sendMetricsPropertyProvider
        (final Project project) {
        return project.provider(() -> {
                Object value = project.findProperty
                    (ETA_SEND_METRICS_PROPERTY);
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
        CommandLine c = initCommandLineWithEtaVersion();
        c.getCommand().add("update");
        c.executeAndLogOutput();
    }

    public void freeze() {
        CommandLine c = initCommandLineWithEtaVersion();
        c.getCommand().add("freeze");
        c.executeAndLogOutput();
    }

    public boolean deps(BiConsumer<List<File>, List<String>> filesAndMavenDeps) {
        CommandLine c = initCommandLineWithEtaVersion();
        c.getCommand().add("deps");
        List<String> allLines = c.executeAndGetStandardOutputLines();
        parseAndExecuteDependencyLines(allLines, filesAndMavenDeps);
        return isUpToDate(allLines);
    }

    public boolean build() {
        CommandLine c = initCommandLineWithEtaVersion();
        c.getCommand().add("build");
        List<String> allLines = c.executeLogAndGetStandardOutputLines();
        return isUpToDate(allLines);
    }

    public CommandLine initCommandLineWithEtaVersion() {
        CommandLine c = initCommandLine();
        String versionFlag = getEtaVersionFlag();
        if (versionFlag != null) {
            c.getCommand().add(versionFlag);
        }
        return c;
    }

    private CommandLine initCommandLine() {
        CommandLine c = new CommandLine(resolvedEtlas.get().getPath());
        File workingDir = workingDirectory.getOrNull();
        if (workingDir != null) {
            c.setWorkingDir(workingDir);
        }
        String sendMetrics = getSendMetricsFlag();
        if (sendMetrics != null) {
            c.getCommand().add(sendMetrics);
        }
        return c;
    }

    private static boolean isUpToDate(List<String> allLines) {
        Optional<String> upToDate = allLines.stream()
            .filter(line -> line.indexOf("Up to date") >= 0)
            .findAny();
        if (upToDate.isPresent()) {
            return true;
        } else {
            return false;
        }
    }

    private static void parseAndExecuteDependencyLines
        (List<String> allLines,
         BiConsumer<List<File>, List<String>> filesAndMavenDeps) {
        List<String> lines = allLines.stream()
            .filter(line -> line.startsWith("file:")
                    || line.startsWith("maven:"))
            .collect(Collectors.toList());
        List<File> files = new ArrayList<File>();
        List<String> mavenDeps = new ArrayList<String>();
        for (String line: lines) {
            if (line.startsWith("file:")) {
                files.add(new File(line.substring(5)));
            } else if (line.startsWith("maven:")) {
                mavenDeps.add(line.substring(6));
            } else {
                throw new GradleException("Bad output from `etlas deps`.");
            }
        }
        filesAndMavenDeps.accept(files, mavenDeps);
    }
}
