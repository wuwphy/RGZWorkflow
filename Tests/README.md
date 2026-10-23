# Test tiers

Run the curated suites with `python3 Tools/run_tests.py --kernel /path/to/WolframKernel --suite all`.

- **core:** loading, exact independent tree inverse, session/result guards, model metadata and optional mass guards. No external packages required.
- **algebra:** coefficient reduction, spectral/numerator checkpoints and exact reconstruction helpers.
- **integration:** real serial FeynRules export followed by FeynCalc checks of 69 original propagator/connection entries, all 48 vertices and all 702 labelled permutations.
- **physics:** selected FeynCalc fixture regressions for scattering assembly, form factors, loop-order separation, early gauge algebra and MSbar guards/local identities. These are not a full updated-model amplitude recomputation.

The runner uses one fresh kernel per script and an isolated integration directory. `--suite physics` requires `--work` pointing to an isolated successful integration export. `--suite all` creates it automatically. Other scripts are preserved as manual audits; inspect their inputs/time limits before running. FeynHelpers-dependent tests are optional and not a default release gate.

Any `*Body.wl` is a helper, not an independent test entry. Some manual audits write `output/review`; that directory is intentionally not included in the source archive.

Independent optional horizon-model checks are included under `Experimental/FermionicHorizon`; run each in a fresh appropriate kernel. They are not part of the default-model suite or a full horizon S-matrix calculation.
