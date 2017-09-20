package com.typelead.gradle.eta.plugins

import org.gradle.api.GradleException
import org.gradle.testkit.runner.BuildResult
import org.gradle.testkit.runner.GradleRunner
import org.junit.Rule
import org.junit.rules.TemporaryFolder
import org.junit.rules.TestName
import spock.lang.Specification

import java.nio.file.Files
import java.nio.file.Paths

abstract class PluginSpec extends Specification {

    private static final String testDataPath = "src/test/resources/testData"

    @Rule final TemporaryFolder dir = new TemporaryFolder()
    @Rule final TestName name = new TestName()

    def setup() {
        copyTestData(name.methodName)
    }

    /**
     * Copies testData/$methodName files to current temp directory for testing.
     * If the testData/$methodName dir does not exist, use testData/default instead.
     */
    protected void copyTestData(String methodName) {
        def base = new File(testDataPath, methodName)
        if (!base.exists()) {
            base = new File(testDataPath, "default")
        }
        Files.walk(base.toPath()).iterator().each {
            if (it.toFile().isFile()) {
                def target = Paths.get(dir.root.getPath(), base.toPath().relativize(it).toString())
                def parent = target.parent.toFile()
                if (!parent.exists()) {
                    if (!parent.mkdirs()) {
                        throw new GradleException("Failed to mkdirs: $parent")
                    }
                }
                try {
                    Files.copy(it, target)
                } catch (IOException e) {
                    def parentExists = target.toFile().getParentFile().exists()
                    throw new GradleException("Failed to copy '$it' to '$target'; parentExists: $parentExists", e)
                }
            }
        }
    }

    protected BuildResult gradle(String... tasks) {
        tasks += ['--stacktrace']
        GradleRunner.create()
                .withProjectDir(dir.root)
                .withPluginClasspath()
                .withArguments(tasks)
                .build()
    }

    protected File cachesDir() {
        dir.root.parentFile.listFiles()
                .find { it.path.contains(".gradle-test-kit") }
                .with { new File(it, "caches").canonicalFile }
                .with { assert(it != null) ; it }
    }
}
