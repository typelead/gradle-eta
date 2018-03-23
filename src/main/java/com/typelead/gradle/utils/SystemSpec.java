package com.typelead.gradle.utils;

public class SystemSpec extends ExecutableSpec {

    private static final SystemSpec INSTANCE = new SystemSpec();

    public static SystemSpec getInstance() {
        return INSTANCE;
    }

    private SystemSpec() {}

    @Override
    public String toString() {
        return "SystemSpec";
    }
}
