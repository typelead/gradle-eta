package com.typelead.gradle.utils;

public class NoSpec extends ExecutableSpec {

    private static final NoSpec INSTANCE = new NoSpec();

    public static NoSpec getInstance() {
        return INSTANCE;
    }

    private NoSpec() {}

    @Override
    public String toString() {
        return "NoSpec";
    }
}
