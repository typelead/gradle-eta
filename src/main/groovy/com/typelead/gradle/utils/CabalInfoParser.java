package com.typelead.gradle.utils;

import org.gradle.api.GradleException;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public abstract class CabalInfoParser {

    public static CabalInfo parse(List<String> lines) {
        State s = new State();
        for (String line : lines) {
            Matcher m;
            m = NAME_PAT.matcher(line);
            if (m.find()) {
                s.name = m.group(1);
                continue;
            }
            m = LIB_PAT.matcher(line);
            if (m.find()) {
                s.hasLibrary = true;
                continue;
            }
            m = EXEC_PAT.matcher(line);
            if (m.find()) {
                s.executableNames.add(m.group(1));
                continue;
            }
            m = TEST_PAT.matcher(line);
            if (m.find()) {
                s.testNames.add(m.group(1));
                continue;
            }
            m = BENCH_PAT.matcher(line);
            if (m.find()) {
                s.benchmarkNames.add(m.group(1));
            }
        }
        if (s.name == null) {
            throw new GradleException("Failed to parse name from cabal file");
        }
        return new CabalInfo(
                s.name,
                s.hasLibrary,
                s.executableNames,
                s.testNames,
                s.benchmarkNames
        );
    }

    private static final Pattern NAME_PAT = pat("^name:\\s+([\\w\\-]+)");
    private static final Pattern LIB_PAT = pat("^library");
    private static final Pattern EXEC_PAT = pat("^executable ([\\w\\-]+)");
    private static final Pattern TEST_PAT = pat("^test-suite ([\\w\\-]+)");
    private static final Pattern BENCH_PAT = pat("^benchmark ([\\w\\-]+)");

    private static Pattern pat(String s) {
        return Pattern.compile(s, Pattern.CASE_INSENSITIVE);
    }

    private static final class State {
        String name;
        boolean hasLibrary = false;
        List<String> executableNames = new ArrayList<>();
        List<String> testNames = new ArrayList<>();
        List<String> benchmarkNames = new ArrayList<>();
    }
}
