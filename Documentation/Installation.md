# Installation

## Runtime

The declared minimum is Wolfram Language 13.3. Other versions/platforms need their own validation. Use `Get[path/to/RGZWorkflow.wl]` for source loading, or the shipped kernel entry via a locally built paclet. A Wolfram license is not supplied. Do not load the old and new copies in the same kernel: both intentionally preserve the RGZWorkflow context and public API. Check `Evidence/validation.json` for the actual release-test environment.

Python 3.9+ runs the standard-library-only maintenance tools; Python is not needed for the physics package itself.

## External dependencies

Install FeynRules and FeynCalc with its FeynArts add-on from their upstream instructions (see [THIRD_PARTY](../THIRD_PARTY.md)). Do not copy dependencies into this source archive.

FeynRules can be on `$Path` under its normal application layout. Alternatively:

```sh
export RGZ_FEYNRULES_DIRECTORY=/path/to/FeynRules
```

or use the explicit Wolfram option:

```wl
RGZReport[RGZLoadFeynRules["Directory"->"/path/to/FeynRules"]];
```

An explicit `Directory` takes precedence over the environment variable. Setting `Global`FR$Parallel=False` before loading FeynRules selects serial operation for a first validation run. On hosts that need it, set `KMP_USE_SHM=0` for the kernel process; this is not required by the file format.

FeynCalc/FeynArts should be discoverable by `Needs["FeynCalc`"]`. `RGZLoadFeynCalc[]` requests the FeynArts add-on in a fresh non-FeynRules session. FeynHelpers with its Package-X interface is optional for supported analytic UV/IR evaluation; its tests/features are separately gated.

## Working data

Select a fresh directory with `RGZInitialize[work]`. The package creates `Data`, `Cache` and `Results` beneath it. Old sessions, mixed matrices and model exports are not copied into this release. Source/model fingerprints and the package version intentionally invalidate incompatible data. Retain old files only as historical records.

No dependency is downloaded or installed when the package is loaded.
