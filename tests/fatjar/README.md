# FatJar Example

## Building the fatjar

This uses the [shadow](https://github.com/johnrengelman/shadow) plugin.

```
$ ./gradlew shadowJar
```

This will produce a file `build/libs/farjar-all.jar` which will contain all the dependencies in a single package, which you can then run with:

```
$ java -jar build/libs/fatjar-all.jar
```
