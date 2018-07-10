# Gradle Plugin for Eta

[![CircleCI](https://circleci.com/gh/typelead/gradle-eta/tree/master.svg?style=shield)](https://circleci.com/gh/typelead/gradle-eta/tree/master)
[![Gradle Plugin Portal](https://img.shields.io/badge/com.typelead.eta-0.7.5-green.svg?longCache=true&style=plastic)](https://plugins.gradle.org/plugin/com.typelead.eta)
[![Gradle Plugin Portal](https://img.shields.io/badge/com.typelead.eta.android-0.7.5-green.svg?longCache=true&style=plastic)](https://plugins.gradle.org/plugin/com.typelead.eta.android)
[![Gradle Plugin Portal](https://img.shields.io/badge/com.typelead.eta.base-0.7.5-green.svg?longCache=true&style=plastic)](https://plugins.gradle.org/plugin/com.typelead.eta.base)

A gradle plugin for building [Eta](http://eta-lang.org/) projects via the
[Etlas](https://github.com/typelead/etlas) build tool.

## Requirements

This plugin requires that Gradle 4.3+ is used.

## Using the Plugin

### Plugin DSL

#### Eta Base Plugin

This is used for configuring your Eta/Etlas versions for your entire project. You can only do such configuration in the root project.

NOTE: Both the Eta Plugin and the Eta Android Plugin apply the Eta Base Plugin by default, so if you import either one, it is not required to import this one!

```gradle
plugins {
    id 'com.typelead.eta.base' version '0.7.5'
}
```

#### Eta Plugin

This is used for standard JVM projects.

```gradle
plugins {
    id 'com.typelead.eta' version '0.7.5'
}
```

#### Eta Android Plugin

This is used for Android projects.

```gradle
plugins {
    id 'com.typelead.eta.android' version '0.7.5'
}
```

### Legacy Method

Before applying any of the plugins, you should add the following in the `build.gradle` for your root project.

```gradle
buildscript {
  repositories {
    maven {
      url 'https://plugins.gradle.org/m2/'
    }
  }
  dependencies {
    classpath 'gradle.plugin.com.typelead:gradle-eta:0.7.5'
  }
}
```

#### Eta Base Plugin

```gradle
apply plugin: 'eta-base'
```

#### Eta Plugin

```gradle
apply plugin: 'eta'
```

#### Eta Android Plugin

```gradle
apply plugin: 'eta-android'
```

## Building from Source

If you're interested in hacking on the plugin or trying out the latest version, you can install it from source.

### Source Installation

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
      classpath 'com.typelead:gradle-eta:latest.release'
    }
  }
}
```

## Quick Start

### Library

```gradle
eta {
  version = '0.7.2b1'
  etlasVersion = '1.3.0.0'
}
```

### Executable
```gradle
apply plugin: 'application'

eta {
  version = '0.7.2b1'
  etlasVersion = '1.3.0.0'
}
```

### Android Application

```gradle
eta {
  version = '0.7.2b1'
  etlasVersion = '1.3.0.0'
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

The general format is `eta([package-name]:[version-or-version-range])`.

[Ivy version range notation](http://ant.apache.org/ivy/history/latest-milestone/ivyfile/dependency.html) is supported.

## Tasks

The Eta Gradle Plugin adds the following tasks:

### Root Project Tasks

* `:setupEnvironmentEta` - This task is attached to the root project and installs the necessary `eta` and `etlas` executables for your platform or uses the provided executables in the configuration.
* `:resolveDependenciesEta` - This task is attached to the root project and makes sure all the projects in the build use a consistent set of Eta dependencies.

### Per-Project, Per-SourceSet Tasks

* `installDependencies<SourceSet>Eta` - This task installs the Eta dependencies into the Etlas global cache and injects the paths to all the dependency jars into the corresponding Gradle configurations. This task is incremental and will only do work on the first run and every time the dependencies change.
* `compile<SourceSet>Eta` - This task performs incremental compilation of the corresponding source set. This task depends on `compile<SourceSet>Java` and will have the output of that task in its classpath.
* `injectDependencies<SourceSet>Eta` - This task injects Eta dependencies into non-Eta projects when non-Eta projects depend on Eta projects. This task is an implementation detail and you don't have to worry about it.

For the `main` source set, the tasks are `installDependenciesEta` and `compileEta`.

### Conditional Tasks

If the `application` plugin is enabled as well, the `run` task will run the `main` function defined in `src/main/eta/Main.hs`.

### Standard Tasks

The standard Gradle tasks like `build`, `assemble`, so on will work as expected and will trigger compilation of the required Eta source sets.
