package com.typelead.gradle.utils;

@FunctionalInterface
public interface Function3<A, B, C, D> {
    D apply(A a, B b, C c);
}
