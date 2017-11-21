package com.typelead.gradle.eta.plugins

import spock.lang.Ignore

class EtaBasePluginTest extends PluginSpec {

    def download() {
        when:
        def result = gradle("printEtlasBinary")

        then:
        def etlasPath = "${cachesDir().path}/etlas/1.0.2.0/etlas"
        result.output.split('\n').contains(etlasPath)
    }

    def compile() {
        when:
        gradle("compileEta")

        then:
        exampleJarFile().exists()
    }

    def cleanCompile() {
        when:
        gradle("clean", "compileEta")

        then:
        exampleJarFile().exists()
    }

    def assemble() {
        when:
        gradle("assemble")

        then:
        exampleJarFile().exists()
    }

    def deps() {
        when:
        gradle("compileEta")

        then:
        exampleJarFile().exists()
    }

    def cleanEta() {
        when:
        gradle("build", "cleanEta")

        then:
        !exampleJarFile().exists()
    }

    def clean() {
        when:
        gradle("build", "clean")

        then:
        !exampleJarFile().exists()
    }

    def run() {
        when:
        def result = gradle("runEta")

        then:
        result.output.contains("Hello from Gradle Eta!")
    }

    def run2() {
        when:
        def result = gradle("foo", "bar")

        then:
        result.output.contains(":foo")
        result.output.contains("\nFoo!\n")
        result.output.contains(":bar")
        result.output.contains("\nBar!\n")
    }

    // TODO: Un-ignore this once `Data.Text.last: empty input` is fixed in etlas.
    @Ignore
    def testCompile() {
        when:
        gradle("installTestDepsEta", "testCompileEta")

        then:
        exampleTestJarFile().exists()
    }

    // TODO: Un-ignore this once `Data.Text.last: empty input` is fixed in etlas.
    @Ignore
    def test() {
        when:
        def result = gradle("installTestDepsEta", "testEta")

        then:
        result.output.contains("Test suite example-test: PASS")
    }

    private def exampleJarFile() {
        new File(dir.root, "build/etlas/dist/build/example/example.jar")
    }

    private def exampleTestJarFile() {
        new File(dir.root, "build/etlas/dist/build/example-test/example-test.jar")
    }
}
