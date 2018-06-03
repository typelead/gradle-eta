package com.typelead.gradle.utils;

import org.gradle.api.GradleException;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Supplier;
import java.util.function.BiFunction;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.lang.ProcessBuilder.Redirect;

public class CommandLine implements Log {

    private List<String> command;
    private String workingDir;

    public CommandLine(String... command) {
        this.command = Arrays.stream(command).collect(Collectors.toList());
    }

    public List<String> getCommand() {
        return command;
    }

    public void setCommand(List<String> command) {
        this.command = command;
    }

    public String getWorkingDir() {
        return workingDir;
    }

    public void setWorkingDir(File workingDir) {
        this.workingDir = workingDir.getAbsolutePath();
    }

    public void execute() {
        StringBuilder stdOutBuilder = new StringBuilder();
        StringBuilder stdErrBuilder = new StringBuilder();
        Process p = executeAndConsumeOutput(stdOutLine -> {
                                                logger().info(stdOutLine);
                                                stdOutBuilder.append(stdOutLine).append(System.lineSeparator());
                                            },
                                            stdErrLine -> {
                                                logger().info(stdErrLine);
                                                stdErrBuilder.append(stdErrLine).append(System.lineSeparator());
                                            });
        checkExitCode(p, stdOutBuilder.toString(), stdErrBuilder.toString());
    }

    public String executeAndGetStandardOutput() {
        StringBuilder stdOutBuilder = new StringBuilder();
        StringBuilder stdErrBuilder = new StringBuilder();
        Process p = executeAndConsumeOutput(stdOutBuilder::append,
                                            stdErrLine -> {
                                                logger().info(stdErrLine);
                                                stdErrBuilder.append(stdErrLine).append(System.lineSeparator());
                                            });
        String stdOut = stdOutBuilder.toString();
        String stdErr = stdErrBuilder.toString();
        checkExitCode(p, stdOut, stdErr);
        return stdOutBuilder.toString();
    }

    public List<String> executeAndGetStandardOutputLines() {
        List<String> stdOutLines = new ArrayList<>();
        StringBuilder stdErrBuilder = new StringBuilder();
        Process p = executeAndConsumeOutput(stdOutLines::add,
                                            stdErrLine -> {
                                                logger().info(stdErrLine);
                                                stdErrBuilder.append(stdErrLine).append(System.lineSeparator());
                                            });
        String stdOut = String.join(System.lineSeparator(), stdOutLines);
        String stdErr = stdErrBuilder.toString();
        checkExitCode(p, stdOut, stdErr);
        return stdOutLines;
    }

    public void executeAndLogOutput() {
        executeAndLogOutput(false);
    }

    public void executeAndLogOutput(boolean loud) {
        Consumer<String> lineConsumer =
            loud? x -> logger().lifecycle(x) : x -> logger().info(x);
        Process p = executeAndConsumeOutput(lineConsumer,
                                            x -> logger().error(x));
        if (p.exitValue() == 0) return;
        throw new GradleException("Nonzero (" + p.exitValue()
                                  + ") exit code for command " + command
                                  + " with workingDir " + workingDir);
    }
    private static Predicate<String> alwaysFalse = x -> false;

    public List<String> executeLogAndGetStandardOutputLines() {
        return executeLogAndGetStandardOutputLines(alwaysFalse);
    }

    public List<String> executeLogAndGetStandardOutputLines(Predicate<String> logLoudly) {
        List<String> stdOutLines = new ArrayList<>();
        StringBuilder stdErrBuilder = new StringBuilder();
        Process p = executeAndConsumeOutput
            (x -> {
                if (logLoudly.test(x)) {
                    logger().lifecycle(x);
                } else {
                    logger().info(x);
                }
                stdOutLines.add(x);
            },
             x -> {
                logger().error(x);
                stdErrBuilder.append(x).append(System.lineSeparator());
            });
        String stdOut = String.join(System.lineSeparator(), stdOutLines);
        String stdErr = stdErrBuilder.toString();
        checkExitCode(p, stdOut, stdErr);
        return stdOutLines;
    }

    public void executeWithInputAndLogOutput(String input) {
        Process p = executeWithInputAndConsumeOutput(() -> input,
                                                     x -> logger().info(x),
                                                     x -> logger().error(x));
        if (p.exitValue() == 0) return;
        throw new GradleException("Nonzero (" + p.exitValue()
                                  + ") exit code for command " + command
                                  + " with workingDir " + workingDir);
    }

    public Process executeAndConsumeOutput(Consumer<String> consumeStdOutLine, Consumer<String> consumeStdErrLine) {
        return executeWithInputAndConsumeOutput(() -> null,
                                                consumeStdOutLine,
                                                consumeStdErrLine);
    }

    public Process executeAndConsumeOutputWithProcess(BiFunction<Process, String, Boolean> consumeStdOutLine, BiFunction<Process, String, Boolean> consumeStdErrLine) {
        final Process p = start();
        IOUtils.consumeLinesIgnoreFailure(p.getInputStream(),
                                          line -> consumeStdOutLine.apply(p, line));
        IOUtils.consumeLinesIgnoreFailure(p.getErrorStream(),
                                          line -> consumeStdErrLine.apply(p, line));
        try {
            p.waitFor();
        } catch (InterruptedException e) {
            Thread.interrupted();
            throw new GradleException("Command was interrupted: " + command, e);
        }
        return p;
    }

    public Process executeWithInputAndConsumeOutput(Supplier<String> supplyStdIn,
                                                    Consumer<String> consumeStdOutLine,
                                                    Consumer<String> consumeStdErrLine) {
        Process p = start();
        IOUtils.supplyInput(p.getOutputStream(), supplyStdIn);
        IOUtils.consumeLines(p.getInputStream(), consumeStdOutLine);
        IOUtils.consumeLines(p.getErrorStream(), consumeStdErrLine);
        try {
            p.waitFor();
        } catch (InterruptedException e) {
            Thread.interrupted();
            throw new GradleException("Command was interrupted: " + command, e);
        }
        return p;
    }

    private void checkExitCode(Process p, String stdOut, String stdErr) {
        if (p.exitValue() == 0) return;
        String message =
            "Nonzero (" + p.exitValue() + ") exit code for command " + command
            + " with workingDir " + workingDir;
        if (!stdOut.isEmpty()) message += "\nProcess Standard Output:\n" + stdOut;
        if (!stdErr.isEmpty()) message += "\nProcess Standard Error:\n" + stdErr;
        throw new GradleException(message);
    }

    public void fork() {
        logger().info("Forking external command: " + command + " in workingDir: " + workingDir);
        ProcessBuilder pb = new ProcessBuilder(command);
        if (workingDir != null) pb.directory(new File(workingDir));
        pb.redirectInput(Redirect.INHERIT);
        pb.redirectOutput(Redirect.INHERIT);
        pb.redirectError(Redirect.INHERIT);
        Process p = null;
        try {
            p = pb.start();
        } catch (IOException e) {
            throw new GradleException("IOException occurred when forking command "
                                      + command + " with workingDir " + workingDir, e);
        }
        try {
            p.waitFor();
        } catch (InterruptedException e) {
            Thread.interrupted();
            throw new GradleException("Forked command was interrupted: " + command, e);
        }
    }

    private Process start() {
        logger().info("Executing external command: " + command + " in workingDir: " + workingDir);
        ProcessBuilder pb = new ProcessBuilder(command);
        if (workingDir != null) pb.directory(new File(workingDir));
        try {
            return pb.start();
        } catch (IOException e) {
            throw new GradleException("IOException occurred when executing command " + command
                                      + " with workingDir " + workingDir,
                                      e);
        }
    }
}
