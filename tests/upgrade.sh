#!/usr/bin/env sh

set -e

(cd tests/fatjar; ./gradlew wrapper --gradle-version $1)
(cd tests/multi; ./gradlew wrapper --gradle-version $1)
(cd tests/simple; ./gradlew wrapper --gradle-version $1)
(cd tests/test; ./gradlew wrapper --gradle-version $1)
(cd tests/empty-code; ./gradlew wrapper --gradle-version $1)
(cd tests/git-dep; ./gradlew wrapper --gradle-version $1)


