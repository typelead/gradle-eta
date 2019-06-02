#!/usr/bin/env sh

set -e

(cd examples/fatjar; ./gradlew wrapper --gradle-version $1)
(cd examples/multi; ./gradlew wrapper --gradle-version $1)
(cd examples/simple; ./gradlew wrapper --gradle-version $1)
(cd examples/test; ./gradlew wrapper --gradle-version $1)


