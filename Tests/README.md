# Essential checks

The retained checks cover package loading/session guards, the scalar audit adapter, notebook dependency handling, exact generic-D scalar identities, interrupted weighted assembly, certificate payload validation, paper-table integrity and Python filesystem safety. `MixedBackendSafety.wls` also tests assumption-independent pole splitting, scaleless-preservation policies in ordinary and accelerated reduction, and recursive validation of fresh and cached stage results. `PTContinuationSafety.wls` checks shifted-momentum acceleration and exact native-TID agreement, repeated-pole routing, PT environment changes and safe reuse, and invalid fresh/disk/memory result handling. Its PT upstream formula providers are explicit synthetic fixtures; it does not claim a full physical PT certification.

```sh
python3 Tools/run_tests.py --kernel /path/to/WolframKernel --suite core
python3 Tools/run_tests.py --kernel /path/to/WolframKernel --suite algebra
python3 Tests/ReleaseTools.py
```

Core checks require no external physics packages. Algebra checks require FeynCalc/FeynArts. The complete native model/graph calculation is exercised by Start Here and Diagnostics. Historical development tests are preserved in the local cleanup backup.

`GaugeCheckSafety.wls` checks invalid/foreign-context gauge inputs, fresh and cached coefficient validation, batch/memory cache rejection, principal-root and singular-denominator behavior, and independent tree-inverse/non-symmetric insertion identities. It checks the supplied residual expression; it does not independently enumerate a full diagram set.

`PTProvenanceSafety.wls` rejects invalid QCD prerequisite coefficients, exercises stage-input/result provenance and session restoration, prevents stale comparison reuse, and checks preserved FullMatrix NAV continuation versus explicit AAReplacement selection. NAV/formula packets in these tests are isolated synthetic fixtures, not physical certificates.

`FixedDenominatorSafety.wls` checks invalid fixed-reducer inputs, prepared-cache corruption, validated cache round trips, fixed versus direct reduction, current coefficient rows, and PT comparison source binding. Dispatch fixtures are synthetic and do not certify physical results.

`NAVPinchSafety.wls` tests invalid Ward inputs, supported and unsupported momentum routes, original-tensor reconstruction, double-longitudinal ordering, source-bound per-connection cache reuse, changed/corrupted/legacy checkpoints, and final packet consistency. It uses isolated synthetic tensors and temporary caches.

`AALoopSafety.wls` loads the AA backend independently and checks assumption-neutral general pole identities, shifted and repeated poles, invalid input/result propagation, gauge-symbol contexts, tadpole normalization, scaleless retention and batch reduction. All examples use temporary Work and synthetic expressions.

`NAVMatrixSafety.wls` tests matrix-source and mass binding, valid cache reuse, corrupted and legacy records, aggregate consistency, unknown fields, invalid attachments and attachment timeouts using synthetic vertices and temporary Work.

The NAV pinch follow-up extends `NAVPinchSafety.wls` to 62 checks, including assigned/emitted pole labels, shifted repeated poles, zero-mass/zero-gauge and coincident massive roots, colour powers and residual colour objects, and unknown fields.

`QQScatteringSafety.wls` uses an inert temporary raw-file fixture for task hashing and native simple integrals to verify convention isolation, source-bound part restoration, invalid M0 pairs and emitted pole reconstruction. It also independently derives the D-dimensional transverse/longitudinal residue coefficients, four-leg LSZ weight, elementary Ward contractions and physical measure. These local checks do not certify a full physical amplitude.

`ScatteringCheckpointSafety.wls` checks inert input capture, data and index binding, Module-local reuse, execution-budget independence, result hashes, invalid values, read/write timeouts and file preservation. It also verifies eight spectral simplification identities at 24 real and complex branch points in temporary Work.

The physics-first source organization adds direct assembly regressions to `QQScatteringSafety.wls`: the full graph sum, one mixed-self measure conversion, unchanged raw self source and output conventions. The general-gauge comparison body was also compared against the pre-organization loaded definition after normalizing sequence grouping.

The zero-temperature scope cleanup removes two Core.wlt tests for the deleted generic developer registry. Physics, loading, continuation and reconstruction tests remain. The two clean notebooks contain no calls to the removed optional interfaces.
