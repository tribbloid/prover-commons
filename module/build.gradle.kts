val vs = versions()

allprojects {
    group = "ai.acyclic.prover-commons"
    // group overridden to avoid project name collision & cyclic dependency
}