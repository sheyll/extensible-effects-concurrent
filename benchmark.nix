{ withProfiling ? false
}:
(import ./default.nix { inherit withProfiling; }).extensible-effects-concurrent.components.benchmarks.extensible-effects-concurrent-bench

