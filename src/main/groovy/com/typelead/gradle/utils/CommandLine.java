package com.typelead.gradle.utils;

import org.gradle.api.GradleException;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class CommandLine implements Log {

    private List<String> command;
    private String workingDir;

    public CommandLine(String... command) {
        this.command = Arrays.stream(command).collect(Collectors.toList());
    }

    public void executeAndOutputToSystem() {
        Process p = executeAndWait();
        System.out.println(stdOut(p));
        System.err.println(stdErr(p));
    }

    public String executeAndGetStandardOutput() {
        return stdOut(executeAndWait());
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

    public Process executeAndWait() {
        Process p = start();
        try {
            p.waitFor();
        } catch (InterruptedException e) {
            Thread.interrupted();
            throw new GradleException("Command was interrupted: " + command, e);
        }
        if (p.exitValue() != 0) {
            String message =
                    "Nonzero (" + p.exitValue() + ") exit code for command " + command
                            + " with workingDir " + workingDir;
            String out = stdOut(p);
            if (!out.isEmpty()) message += "\nProcess Standard Output:\n" + out;
            String err = stdErr(p);
            if (!err.isEmpty()) message += "\nProcess Standard Error:\n" + err;
            throw new GradleException(message);
        }
        return p;
    }

    private String stdErr(Process p) {
        return IOUtils.toString(p.getErrorStream());
    }

    private String stdOut(Process p) {
        return IOUtils.toString(p.getInputStream());
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
