package com.typelead.gradle.utils;

import org.gradle.api.Nullable;
import org.gradle.internal.os.OperatingSystem;

/**
 * Supported operating systems and architectures.
 */
public enum Arch {
  X86_64_LINUX("x86_64-linux", ""),
  X86_64_OSX_64("x86_64-osx", ""),
  X86_64_WINDOWS_64("x86_64-windows", ".exe");

  /**
   * Name of the operating system and architecture.
   */
  public final String name;

  /**
   * Executable extension, if applicable; empty string otherwise.
   */
  public final String execExt;

  Arch(String name, String execExt) {
    this.name = name;
    this.execExt = execExt;
  }

  /**
   * Returns Right(Arch) if the OS is supported, Left(os) if otherwise.
   */
  @Nullable
  public static Either<String, Arch> current() {
    OperatingSystem os = OperatingSystem.current();
    if (os.isLinux()) return Either.right(X86_64_LINUX);
    else if (os.isMacOsX()) return Either.right(X86_64_OSX_64);
    else if (os.isWindows()) return Either.right(X86_64_WINDOWS_64);
    else return Either.left(os.getName());
  }
}
