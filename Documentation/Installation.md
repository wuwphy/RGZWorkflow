# Installation

Use Wolfram Language 13.3 or later. The validated environment is Mathematica 13.3.1 on MacOSX ARM64 with FeynCalc 10.1.0 and FeynArts 3.12. Python 3.9+ is needed only for maintenance and ZIP creation.

Install FeynRules, FeynCalc with FeynArts, and FeynHelpers/Package-X externally for the supplied complete workflow. See [upstream attribution](../THIRD_PARTY.md). No dependencies are bundled or installed automatically.

FeynRules may be discoverable on Wolfram's `$Path`, commonly under `$UserBaseDirectory/Applications/FeynRules`. An explicit directory overrides the environment variable and automatic discovery:

```wl
RGZLoadFeynRules["Directory" -> "/path/to/FeynRules"]
```

Alternatively set `RGZ_FEYNRULES_DIRECTORY`. Set `Global`FR$Parallel=False` before the first FeynRules load for serial execution. Use a separate fresh kernel for FeynCalc.

Load the package through its root `RGZWorkflow.wl`; this also loads the required audit adapters in `Tools`. Do not load two copies of the package in the same kernel. Evaluate the package-loading cell separately from subsequent function calls.

`RGZInitialize[work]` selects a work directory containing `Data`, `Cache` and `Results`. Retain the existing local Work to continue this calculation. New installations should use a fresh Work. Source and model identities guard cache compatibility; path-bearing jobs cannot simply be relocated and treated as current.

The source ZIP contains no Work data or third-party installation. The cleanup backup remains local under `artifacts/`.
