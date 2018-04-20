# com.typelead.eta, com.typelead.eta.android, com.typelead.eta.base 0.5.8
- Throws exception if Gradle version is less than 4.3
- Properly feed dependencies for compileTestEta

# com.typelead.eta, com.typelead.eta.android, com.typelead.eta.base 0.5.7
- Fix bug with includeDirs property in which the user has to know the plugin internals
- Add `useEtaTest()` method that uses simple Eta-style testing

# com.typelead.eta, com.typelead.eta.android, com.typelead.eta.base 0.5.6
- Support configuration DSL for the sourceSet extension.

# com.typelead.eta, com.typelead.eta.android, com.typelead.eta.base 0.5.5

- Added new properties to EtaOptions:
	- List<String> installIncludes
	  Include files to install with the package installation
	- List<String> includeDirs
	  Directories for which to search include files
- Fixed sourceSet extension behavior to use conventions instead.

# com.typelead.eta, com.typelead.eta.android, com.typelead.eta.base 0.5.4

- Use friendly version when printing out error messages for binary installation.

# com.typelead.eta, com.typelead.eta.android, com.typelead.eta.base 0.5.3

- Added EtaOptions configuration for EtaCompile task:
  - String language
    - Either Haskell98 or Haskell2010
  - NamedDomainObjectContainer<String> extensions
    - Language extensions you want to enable
  - List<String> args
    - Arguments that you want to pass to the Eta compiler directly
  - List<String> cpp
    - Preprocessor arguments to send to the Eta compiler
- Added getDefaultEtaProguardFile() function to EtaPluginConvention that provides an out-of-the-box config file to use.
