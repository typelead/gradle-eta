package com.typelead.gradle.utils;

import org.gradle.api.GradleException;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.Consumer;
import java.util.stream.Collectors;

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

    public void setWorkingDir(String workingDir) {
        this.workingDir = workingDir;
    }

    public void execute() {
        StringBuilder stdOutBuilder = new StringBuilder();
        StringBuilder stdErrBuilder = new StringBuilder();
        Process p = executeAndConsumeOutput(
                stdOutLine -> {
                    logger().info(stdOutLine);
                    stdOutBuilder.append(stdOutLine).append(System.lineSeparator());
                },
                stdErrLine -> {
                    logger().info(stdErrLine);
                    stdErrBuilder.append(stdErrLine).append(System.lineSeparator());
                }
        );
        checkExitCode(p, stdOutBuilder.toString(), stdErrBuilder.toString());
    }

    public String executeAndGetStandardOutput() {
        StringBuilder stdOutBuilder = new StringBuilder();
        StringBuilder stdErrBuilder = new StringBuilder();
        Process p = executeAndConsumeOutput(
                stdOutBuilder::append,
                stdErrLine -> {
                    logger().info(stdErrLine);
                    stdErrBuilder.append(stdErrLine).append(System.lineSeparator());
                }
        );
        String stdOut = stdOutBuilder.toString();
        String stdErr = stdErrBuilder.toString();
        checkExitCode(p, stdOut, stdErr);
        return stdOutBuilder.toString();
    }

    public List<String> executeAndGetStandardOutputLines() {
        List<String> stdOutLines = new ArrayList<>();
        StringBuilder stdErrBuilder = new StringBuilder();
        Process p = executeAndConsumeOutput(
                stdOutLines::add,
                stdErrLine -> {
                    logger().info(stdErrLine);
                    stdErrBuilder.append(stdErrLine).append(System.lineSeparator());
                }
        );
        String stdOut = String.join(System.lineSeparator(), stdOutLines);
        String stdErr = stdErrBuilder.toString();
        checkExitCode(p, stdOut, stdErr);
        return stdOutLines;
    }

    public void executeAndOutputToSystem() {
        executeAndConsumeOutput(
                System.out::println,
                System.err::println
        );
    }

    private Process executeAndConsumeOutput(Consumer<String> consumeStdOutLine,
                                            Consumer<String> consumeStdErrLine) {
        Process p = start();
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

    private Process start() {
        logger().info("Executing external command: " + command + " in workingDir: " + workingDir);
        ProcessBuilder pb = new ProcessBuilder(command);
        if (workingDir != null) pb.directory(new File(workingDir));
        try {
            return pb.start();
        } catch (IOException e) {
            throw new GradleException(
                    "IOException occurred when executing command " + command
                            + " with workingDir " + workingDir,
                    e);
        }
    }
}
