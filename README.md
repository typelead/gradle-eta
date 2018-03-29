# Gradle Plugin for Eta

[![Build Status](https://travis-ci.org/typelead/gradle-eta.svg?branch=master)](https://travis-ci.org/typelead/gradle-eta)
[![Gradle Plugin Portal](https://img.shields.io/badge/com.typelead.eta-0.5.2-green.svg?longCache=true&style=plastic)](https://plugins.gradle.org/plugin/com.typelead.eta)
[![Gradle Plugin Portal](https://img.shields.io/badge/com.typelead.eta.android-0.5.2-green.svg?longCache=true&style=plastic)](https://plugins.gradle.org/plugin/com.typelead.eta.android)
[![Gradle Plugin Portal](https://img.shields.io/badge/com.typelead.eta.base-0.5.2-green.svg?longCache=true&style=plastic)](https://plugins.gradle.org/plugin/com.typelead.eta.base)

A gradle plugin for building [Eta](http://eta-lang.org/) projects via the
[Etlas](https://github.com/typelead/etlas) build tool.

# Installing

_Note that Gradle 4.3+ is required_

```shell
git clone https://github.com/typelead/gradle-eta
cd gradle-eta
./gradlew pTML
```

Then, in your `build.gradle` you can add:


```gradle
buildscript {
  repositories {
    mavenLocal()

    dependencies {
      classpath 'com.typelead:gradle-eta:0.5.0'
    }
  }
}
```

## Quick Start

### Normal Library

```gradle
apply plugin: 'eta'

eta {
  useSystemEta = true
  useSystemEtlas = true
}
```

### Normal Executable
```gradle
apply plugin: 'eta'
apply plugin: 'application'

eta {
  useSystemEta = true
  useSystemEtlas = true
}
```

### Android Application

```gradle
apply plugin: 'eta-android'

eta {
  useSystemEta = true
  useSystemEtlas = true
}
```

See the `examples` and `src/test/testData` directories for more examples.

## Configuration

You can use the top-level `eta` extension for some basic global configurations.

NOTE: The `eta` extension is only available in the root project! It will not be 
available in subprojects of a multi-module build because the configuration applies to 
all projects in the build.

Properties:

* `String version` - Version of Eta to use. The corresponding will binary will 
    automatically be downloaded.
* `String etaPath` - Path to locally installed Eta binary. 
* `boolean useSystemEta` - If specified, attempts to resolve the eta binary on your system `PATH`.
* `String etlasVersion` - Version of Etlas to use for this project. The Etlas binary
    will automatically be downloaded and installed from the Etlas repository.
* `String etlasRepo`, defaults to standard Etlas repository. Generally, there is no
    reason to set this.
* `String etlasPath` - Path to locally installed Etlas binary. It is recommended to
    use `etlasRepo` and `etlasVersion` instead, leaving this unconfigured. It will be
    dynamically set by Gradle if it was downloaded and installed.
* `boolean useSystemEtlas` - If specified, attempts to resolve the etlas binary
    on your system `PATH`.
    
## Dependencies

You can add Eta dependencies (from Hackage or elsewhere) as follows:

```gradle
dependencies {
    compile eta('text:1.2.2.2')
}
```

The general format is `[package-name]:[version-or-version-range]` and you can specify dependencies just like you would for Java dependencies in Gradle.

[Ivy version range notation](http://ant.apache.org/ivy/history/latest-milestone/ivyfile/dependency.html) is supported.

### Tasks

The Eta Gradle Plugin adds the following tasks:

#### Root Project Tasks

* `:setupEnvironmentEta` - This task is attached to the root project and installs the necessary `eta` and `etlas` executables for your platform or uses the provided executables in the configuration.
* `:resolveDependenciesEta` - This task is attached to the root project and makes sure all the projects in the build use a consistent set of Eta dependencies.

#### Per-Project, Per-SourceSet Tasks

* `installDependencies<SourceSet>Eta` - This task installs the Eta dependencies into the Etlas global cache and injects the paths to all the dependency jars into the corresponding Gradle configurations. This task is incremental and will only do work on the first run and every time the dependencies change.
* `compile<SourceSet>Eta` - This task performs incremental compilation of the corresponding source set. This task depends on `compile<SourceSet>Java` and will have the output of that task in its classpath.

For the `main` source set, the tasks are `installDependenciesEta` and `compileEta`.

#### Conditional Tasks

If the `application` plugin is enabled as well, the `run` task will run the `main` function defined in `src/main/eta/Main.hs`.

## Standard Tasks

The standard Gradle tasks like `build`, `assemble`, so on will work as expected and 
will trigger compilation of the required Eta source sets.
