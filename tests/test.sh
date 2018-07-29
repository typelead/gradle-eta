#!/usr/bin/env sh

set -e

(cd tests/fatjar; ./gradlew shadowJar -i --stacktrace)
(cd tests/multi; ./gradlew run -i --stacktrace)
(cd tests/simple; ./gradlew run -i --stacktrace)
(cd tests/test; ./gradlew test -i --stacktrace)
(cd tests/empty-code; ./gradlew run -i --stacktrace)


