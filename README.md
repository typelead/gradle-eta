# gradle-eta

[![Build Status](https://travis-ci.org/typelead/gradle-eta.svg?branch=master)](https://travis-ci.org/typelead/gradle-eta)

A gradle plugin for building [Eta](http://eta-lang.org/) projects via the
[Etlas](https://github.com/typelead/etlas) build tool.

**NOTE**: This is pre-release software and under active development.

## Quick Start

Here is an example `build.gradle` file -

```gradle
plugins {
    id 'com.typelead.eta'
}

apply plugin: 'com.typelead.eta'

eta {
    etlasVersion = '1.1.0.0'
}
```

See the `examples` and `src/test/testData` directories for more examples.

## Configuration

You can use the top-level `eta` extension for some basic global configurations -
* `String etlasVersion` - Version of Etlas to use for this project. The Etlas binary
    will automatically be downloaded and installed from the Etlas repository.
* `String etlasRepo`, defaults to standard Etlas repository. Generally, there is no
    reason to set this.
* `String etlasBinary` - Path to locally installed Etlas binary. It is recommended to
    use `etlasRepo` and `etlasVersion` instead, leaving this unconfigured. It will be
    dynamically set by Gradle if it was downloaded and installed.
* `boolean useSystemEtlas` - If specified, attempts to resolve the etlas binary
    on your system `PATH`

### Tasks

The Gradle plugin provides different task types depending on what you need.

## Build Tasks

The following tasks are available for compiling Eta sources -
* `cleanEta` - Cleans the etlas build via `etlas clean`
* `compileEta` - Compiles via `etlas build`
* `testDepsEta` - Installs test dependencies; you may need to run this first before
    running `testEta`
* `testEta` - Runs Eta test suites

Each of these tasks have the following configurations -
* `Boolean useSandbox`, defaults to `true`
* `String sandboxConfig`, defaults to `null`, using the Etlas default.
* `String defaultUserConfig`, defaults to `null`, using the Etlas default.
* `String etlasBinary`, defaults to the [global configuration](#configuration) in `eta` extension.
* `List<String> etlasFlags`, defaults to `[]`
* `List<String> buildFlags`, defaults to `[]`
* `String buildDir`, defaults to `build/etlas/dist`
* `List<String> components`, defaults to lib and exe components for `compileEta`,
    defaults to test components for `testEta`

## Run Tasks

The following run tasks are available for running Eta programs -
* `runEta`

You can build your own custom run task to run specific executables. This
is useful if your project defines more than one executable stanza in the
Cabal file -

```gradle

import com.typelead.gradle.eta.tasks.EtaRun

task foo(type: EtaRun) {
    component = "foo"
}

task bar(type: EtaRun) {
    component = "bar"
}
```

## Development Notes

You can run `gradle` with `-i` to enable info logging and see which tasks are being run.

To run the plugin locally,
you will need to build and deploy the gradle-eta plugin to your local m2 repo with -

```
% cd path/to/gradle-eta
% gradle clean assemble publishToMavenLocal
```

You will need the following in your `build.gradle` for it to resolve the plugin -

```
buildscript {
  repositories {
    mavenLocal()

    dependencies {
      classpath 'com.typelead:gradle-eta:0.0.1-SNAPSHOT'
    }
  }
}

```

See the `examples/local` project as a demonstration.


## Limitations

Currently, `etlas deps` can only be run for projects which have a `library`
component in their cabal file. This plugin relies one `etlas deps` to resolve
dependencies, so until `etlas deps` is updated to support non-library components,
ensure your project contains a `library` stanza in the cabal file.
