# RGZWorkflow 1.1.0

Released package date: 2026-11-14. This source archive is prepared for publication; creating it does not publish a GitHub release or assign a DOI.

## What is included

The physics-oriented tree, one-loop, algebra, renormalization and validation modules; actual model companions; clean Start Here and Diagnostics notebooks; independent small examples; source tests and release tools; the author electronic reference; and compact historical/current calculation summaries.

Work caches, computed notebook outputs, raw amplitudes, PDFs, local review backups and third-party dependencies are excluded. The original working notebooks and published 1.0.0 ZIP are preserved outside this release directory.

## Validation

The completed local run passes the declared-domain certificate. All eight prefactors and 144 finite coefficients match the historical result and paper table exactly. The 31-function catalogue and the certificate's IR conditions/branch prescription agree. The compact evidence preserves the original artifact hashes and scope.

The calculation run used the corrected development source with version metadata 1.0.0. The only change to Kernel source for this archive is the release version string/comment; no raw certificate is relabelled. Isolated tests of the final 1.1.0 package are recorded separately; a full rerun under its changed version fingerprint is not claimed.

## Running

Use a fresh kernel and a new Work directory for this release. FeynRules and FeynCalc require separate kernels. Follow Start Here through fixed-gauge restoration, then Diagnostics. The Diagnostics entry explicitly recovers the fixed-gauge task and assembles its completed contributions before form factors. Stop on a failed prerequisite; a saved session is not itself evidence that all checks passed.

The self-preparation commands allow 14400 seconds in total. A frontend-only progress rendering issue can occur; it does not replace the returned calculation status. Preserve the declared IR conditions and analytic continuation with +i0.

## Citation

The DOI 10.5281/zenodo.23724368 belongs to 1.0.0. The new version has no DOI assigned here. The companion paper is arXiv:2610.38463.
