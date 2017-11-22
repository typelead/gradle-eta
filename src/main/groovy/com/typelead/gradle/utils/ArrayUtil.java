package com.typelead.gradle.utils;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public abstract class ArrayUtil {

  /**
   * Convert an array to a mutable List.
   * This is different than {@link Arrays#asList} which returns an unmodifiable list.
   */
  public static <A> List<A> toMutableList(A[] array) {
    return Arrays.stream(array).collect(Collectors.toList());
  }
}
