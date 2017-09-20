package com.typelead.gradle.utils;

import org.gradle.api.logging.Logger;
import org.gradle.api.logging.Logging;

public abstract class DefaultLog {
    public static final Logger LOG = Logging.getLogger(DefaultLog.class);
}
