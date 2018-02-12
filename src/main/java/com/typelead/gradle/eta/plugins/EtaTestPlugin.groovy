package com.typelead.gradle.eta.plugins

class EtaTestPlugin {
    def methodMissing(String name, args) {
        // Intercept method that starts with find.
        if (name.startsWith("find")) {
            def result = list.find { it == name[4..-1] }
            // Add new method to class with metaClass.
            this.metaClass."$name" = {-> result + "[cache]" }
            result
        } else {
            throw new MissingMethodException(name, this.class, args)
        }
    }
}