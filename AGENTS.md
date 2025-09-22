# AGENTS.md

This file contains information for AI agents working on the prover-commons project.

## Project Overview

- **Language**: Scala 2.13
- **Build System**: Gradle with Kotlin DSL
- **Architecture**: Multi-module project with the following modules:
  - `core` - Core functionality
  - `infra` - Infrastructure utilities
  - `meta2` - Metaprogramming utilities
  - `spark` - Apache Spark integration
  - `abandoned` - Deprecated/experimental code
- **Main Package**: `ai.acyclic.prover.commons`
- **Testing**: Uses `testFixtures` for shared test utilities

## Frequently Used Commands

### Build & Compile
```bash
./gradlew build                # Build all modules
./gradlew clean                # Clean build directories
./gradlew compileScala         # Compile main Scala sources
./gradlew compileTestScala     # Compile test Scala sources
```

### Testing
```bash
./gradlew test                 # Run all tests
./gradlew check                # Run all checks (includes tests)
./gradlew testClasses          # Compile test classes
```

### Code Quality & Formatting
```bash
./gradlew scalafix             # Apply Scalafix rules
./gradlew checkScalafix        # Check Scalafix compliance (read-only)
./gradlew scalafixMain         # Apply Scalafix to main sources
./gradlew scalafixTest         # Apply Scalafix to test sources
```

### Module-Specific Commands
```bash
./gradlew :prover-commons:core:build     # Build core module
./gradlew :prover-commons:spark:test     # Test spark module
./gradlew :prover-commons:infra:scalafix # Fix infra module
```

## Code Style & Conventions

### Package Structure
- Base package: `ai.acyclic.prover.commons`
- Module-specific packages follow the pattern: `ai.acyclic.prover.commons.{module}`
- Example: Spark module uses `ai.acyclic.prover.commons.spark`

### Scala Conventions
- Uses Scala 2.13 with language features like `implicitConversions`
- Imports are grouped and organized (java, scala, third-party, local)
- Case classes are preferred for data structures
- Implicit conversions are used for DSL-like syntax
- Companion objects contain implicit definitions
- Uses `@transient` annotations for non-serializable fields in serializable classes

### File Organization
- Source files: `module/{module-name}/src/main/scala/ai/acyclic/prover/commons/{module}/`
- Test files: `module/{module-name}/src/test/scala/ai/acyclic/prover/commons/{module}/`
- Test fixtures: `module/{module-name}/src/testFixtures/scala/ai/acyclic/prover/commons/{module}/`

### Dependencies
- Apache Spark integration (spark-core)
- Custom multiverse utilities for equality and projections
- Hadoop IO for serialization
- Uses ClassTag for type information

## Testing Guidelines

- Test fixtures are shared across modules using the `testFixtures` source set
- Tests should be placed in appropriate module directories
- Use pattern: `module/{module}/src/test/scala/` for unit tests
- Shared test utilities go in `testFixtures` directories

## Development Workflow

1. Always run `./gradlew check` before committing to ensure code quality
2. Use `./gradlew scalafix` to automatically apply code style fixes
3. Build specific modules during development to save time: `./gradlew :{module}:build`
4. Run `./gradlew checkScalafix` in CI to verify code style compliance

## Module Dependencies

The project has internal dependencies between modules. When making changes:
- Core module provides base functionality
- Other modules (infra, spark, meta2) depend on core
- Check `buildDependents` tasks to understand impact of changes

## Configuration Files

- `.scalafix.conf` - Scalafix configuration for code style rules
- `.scalafmt.conf` - Scalafmt configuration for code formatting
- `build.gradle.kts` - Main build configuration
- `settings.gradle.kts` - Multi-module project settings
- `gradle.properties` - Gradle build properties
