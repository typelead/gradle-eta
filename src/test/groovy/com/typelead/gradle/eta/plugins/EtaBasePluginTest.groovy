package com.typelead.gradle.eta.plugins

class EtaBasePluginTest extends PluginSpec {

    def download() {
        when:
        def result = gradle("printEtlasBinary")

        then:
        def etlasPath = result.output.split('\n').find { it.contains(cachesDir().path) }
        new File(etlasPath) == new File(cachesDir(), "/etlas/1.0.2.0/etlas")
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

    def testCompile() {
        when:
        gradle("installTestDepsEta", "testCompileEta")

        then:
        exampleTestJarFile().exists()
    }

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
