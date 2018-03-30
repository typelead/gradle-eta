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
