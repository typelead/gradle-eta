package com.typelead.gradle.utils;

import org.gradle.api.logging.Logger;

public interface Log {
  default Logger logger() {
    return DefaultLog.LOG;
  }
}
