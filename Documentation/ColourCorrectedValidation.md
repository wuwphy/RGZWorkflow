# Colour-corrected calculation and release correspondence

The author completed the colour-corrected Start Here and Diagnostics run on 26 November 2026. The saved general-gauge comparison is `VerifiedSymbolicZero`: all 18 coefficient rows vanish relative to reference gauge 1. The fixed-gauge finite result is `CertifiedWithinDeclaredDomain`, with 24 certification checks true and no outstanding checks.

The weighted audit covers 256 records and 2890 graphs, with all weighted UV/IR transfers zero. Mass-region cancellation, raw mass-pole bounds and the completed mass/scaleless ledger pass. Finite assembly, scalar-pole consistency, finite scalar-representation equivalence and exact presentation transformations pass. `RGZQQSMatrix[]` returns the declared-domain certified result; the session was saved.

The result retains the original IR inequalities, nonexceptional invariants, positive renormalization scale and common Euclidean-to-Minkowski +i0 branch prescription. It is a one-loop, two-flavour, massless external qqprime result with M -> 0+. It is not an all-parameter or inclusive-cross-section statement, nor a full PT/background-field certificate.

## Evidence

- [Author run summary](../Evidence/colour-corrected-run-2026-11-26.json): original certificate SHA256, task/model fingerprints, all checks, IR conditions and branch prescription.
- [Independent payload check](../Evidence/colour-corrected-payload-check-2026-11-26.json): bare tree/loop, MSbar amplitude, gauge rows and finite-audit rows agree with the certificate payload hashes. Gauge-job linkage and validation-code hash also match.
- [Calculation/release correspondence](../Evidence/colour-corrected-source-correspondence.json): complete Kernel file hashes, exact differences, matching model companions and matching runtime helpers.

The raw certificate, notebooks with saved outputs and Work data remain unchanged in the author's calculation directory. The public JSON files are explicitly summaries, not a replacement certificate or a certificate for a different source fingerprint. The original certificate's `SourceFingerprint` is the bare-amplitude task fingerprint.

## What was run, and what changed for release

The completed computation used the working development source labelled 1.0.0, with the colour helper explicitly loaded in the FeynCalc kernel. The release is labelled 1.1.2 and automatically applies that same helper during FeynCalc loading. All other Kernel files and all model companions are byte-identical; the exact two-file Kernel diff is retained in the correspondence record. Physical calculation formulas are unchanged by packaging.

This documents the completed corrected calculation and the reviewed correspondence to 1.1.2. The full pipeline has not separately been executed from the version-labelled release tree. No certificate fingerprint or verification flag has been relabelled. Release-specific regression checks are recorded separately from the completed author calculation.

Historical exact agreement with the pre-correction paper table belongs to the earlier calculation. The finite amplitude changes after this colour correction; historical paper-table equality must not be reused as a claim about the corrected result.
