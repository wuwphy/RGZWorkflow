# Current verification

The latest completed colour-corrected author run and the exact correspondence to release 1.1.2 are documented in [ColourCorrectedValidation.md](ColourCorrectedValidation.md). The original task certificate is retained unchanged. Release regression and completed physical computation are recorded separately.

# Current-source verification

The directory structure is frozen for this verification round. No integral,
projection or amplitude formula is changed by the preparation-boundary fixes.

A prepared QQ mixed insertion records `MixedSelfProof`, binding its transverse
matrix, task fingerprint, gauge, regulator mass, calculation conventions and
saved value. An old result without this binding must be prepared again; valid
intermediate checkpoints remain reusable. Empty vertex selections return
`NotChecked`, with zero rows.

In command-line kernels, the FeynCalc loader ignores only
`FrontEndObject::notavail` during package loading. The traced startup paths include Wolfram unit initialization and an external
`FCConfig.m` that enables frontend output formatting. Other loading messages
and errors retain their existing handling; notebook loading is unchanged.

`Tools/run_tests.py` writes `source.json` beside each regression report. It
contains SHA-256 hashes of every distributed file and a combined fingerprint.
The runner checks this snapshot again after the last test, records unrun tests
explicitly, and cannot report success if the source changed. Dependencies are
recorded in the run's environment report. Production `Work/` is not used.

The complete physical certification still requires the open checks described
in `OpenChecks.md`; a regression pass does not certify the full amplitude.
