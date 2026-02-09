# Code Style & Conventions

- Uses Scala 2.13 with modern language features ("-Xsource:3-cross")
- Imports are grouped and organized (java, scala, third-party, local)
- Case classes are preferred for data structures
- Uses `@transient` annotations for non-serializable fields in serializable classes
