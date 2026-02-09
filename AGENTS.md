# AGENTS.md — Critical instructions — READ THIS FIRST

This file contains information for AI agents working on the prover-commons project.

## Project Overview

- **Build System**: Gradle with Kotlin DSL
- **Architecture**: Multi-module project with the following modules:
    - `core` - Core functionality
    - `infra` - Infrastructure utilities
    - `meta2` - Metaprogramming utilities
    - `spark` - Apache Spark integration
    - `abandoned` - Deprecated/experimental code
- **Main Package**: `ai.acyclic.prover.commons`
- **Testing**: Uses `testFixtures` for shared test utilities

## Guardrails (violate only if user explicitly says "ignore AGENTS.md")

see [this](../buildSrc/.agent/guardrails.md)

## Frequently Used Commands

see [this](../buildSrc/.agent/common-commands.md)

## Code Style & Conventions

see [this](.agent/code-convention.md) and [this](../buildSrc/.agent/file-organization.md)

## Development Workflow

see [this](../buildSrc/.agent/development-workflow.md)
