package com.typelead.gradle.eta.api;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.FileOutputStream;
import java.io.OutputStream;

import org.gradle.api.GradleException;
import org.gradle.api.Project;

import static com.typelead.gradle.eta.plugins.EtaBasePlugin.*;

public class ProguardFiles {

    public static final String DEFAULT_PROGUARD_FILE = "eta-rules.pro";
    public static final String[] ALL_PROGUARD_FILES =
        new String[] { DEFAULT_PROGUARD_FILE };

    public static File getDefaultEtaProguardFile(final Project project) {
        return getEtaProguardFile(project, DEFAULT_PROGUARD_FILE);
    }

    public static File getEtaProguardFile(final Project project,
                                          final String proguardFile) {
        return project.getRootProject().getLayout().getBuildDirectory()
            .dir(ETA_INTERMEDIATES_DIRECTORY).get().file(proguardFile).getAsFile();
    }

    public static void createAll(final Project project) {
        try {
            for (String proguardFile : ALL_PROGUARD_FILES) {
                File outputFile = getEtaProguardFile(project, proguardFile);
                if (!outputFile.exists()) {
                    InputStream  in  = null;
                    OutputStream out = null;
                    try {
                        outputFile.getParentFile().mkdirs();
                        in  = ProguardFiles.class.getResourceAsStream(proguardFile);
                        if (in == null) {
                            throw new GradleException
                                ("Failed to find proguard file " + proguardFile +
                                 " in resources.");
                        }
                        out = new FileOutputStream(outputFile);
                        byte[] bytes = new byte[1024];
                        for (int len = 0;
                             (len = in.read(bytes)) != -1;
                             out.write(bytes, 0, len));
                        out.flush();
                    } finally {
                        if (in != null) {
                            in.close();
                        }
                        if (out != null) {
                            out.close();
                        }
                    }
                }
            }
        } catch (IOException ioe) {
            throw new GradleException("Failed to write Eta proguard files.", ioe);
        }
    }
}
