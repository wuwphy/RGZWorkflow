# RGZWorkflow 1.1.2 - 26 November 2026

- Correct the duplicated FeynCalc SUNF contraction rule in the current kernel during `RGZLoadFeynCalc[]`, including command-line use. Verify four colour identities before proceeding; preserve the installed dependency files.
- Distinguish the affected 10.1.0/10.2.1 release archives from the corrected official hotfix-stable branch. The upstream fix is commit 1c5d2d8; 1ee4800 adds regression tests. An ongoing corrected run need not update FeynCalc; keep its dependency environment fixed, then update and verify in a fresh kernel for a subsequent run. See Documentation/ColourCorrection.md.
- Record the resulting finite mixed-self-energy/amplitude correction, its tested scope and numerical examples in Documentation/ColourCorrection.md.
- Keep the release work directory named `Work/`; distribute no caches, sessions or computed outputs. Earlier results must be recomputed in an empty release Work directory.
- Remove Start Here section 8 (Author electronic reference), its historical introductory warning, and the corresponding reproduction-guide comparison section. Preserve historical data and public APIs.
- Preserve the ordinary QQ gauge difference: with symbolic alpha and regulator M, the native projection and actual external-leg-weighted mixed-matrix sum give u^T [DeltaPi(alpha) - DeltaPi(0)] u = 0. Individual gauge-dependent diagrams change, but their gauge dependence cancels in this sum; the other checked colour paths are unchanged. This is a source/integrand-level result. The corrected author notebook validation has now completed: 18 symbolic gauge rows are zero and the final declared-domain certificate has no outstanding checks. See Documentation/ColourCorrectedValidation.md for the original evidence and the reviewed release-source correspondence.
- Version DOI: https://doi.org/10.5281/zenodo.24564596. Historical version DOIs do not identify 1.1.2.

This release contains the current reorganized source tree, including the previously unreleased fixes documented below. Source-level regressions are distinct from a new full-amplitude certification.

## Self-preparation performance update - 26 November 2026

- Snapshot only the selected matrix entry. Original cache input identities and cached value hashes remain verified; the verified selected-input identity is retained for later reuse.
- Resume from completed mass-limit sums once their task/source provenance and values verify, including the original fixed-gauge packets behind copied sums. Otherwise continue the original preparation path.
- Distinguish selection, input validation, input hashing, cache reading and result checking in progress. Reset the cache status for each new checkpoint.
- Remove the nested Dynamic from self-preparation monitoring. The adapter is outside Kernel to preserve interrupted-task source fingerprints. Physics formulas and certification flags are unchanged.

In an existing stopped kernel, load `Tools/SelfPreparationResume.wl`, then repeat the same preparation command. No kernel restart or cache deletion is required. The new release loads the adapter automatically through `RGZWorkflow.wl`.

## Self-preparation timeout boundaries - 26 November 2026

- Propagate failures from completed mass-limit packet reads, copied-source reads and cached-value validation. A timeout is not treated as an absent cache and does not start matrix replay.
- Deduct elapsed preparation time before fallback and pass only the remaining total budget. The fallback call itself is bounded by that remaining time, including its setup.
- Add fault-injection regressions for timeout propagation, stopped status, no unintended matrix replay and shared total-budget behavior. Physical formulas and existing cache identities are unchanged.

## Previous-task snapshot format - 26 November 2026

- Read the original fixed-gauge task from the QQ field of the QQBeforeGauge workflow snapshot before validating copied mass-limit sums. This fixes a false fallback to matrix replay in real notebook sessions.
- The regression fixture now uses the actual wrapped workflow snapshot rather than supplying an unwrapped task. Cache validity checks and physics formulas are unchanged.

## Completed corrected calculation

The saved corrected author run passes all 24 certification checks, the 256-record/2890-graph weighted audit, mass/scaleless completion, finite reconstruction and scalar representation checks. The original certificate and its task fingerprints remain unchanged. The complete run used the development source label 1.0.0 with explicit colour-helper loading; 1.1.2 changes the version and automatically loads the identical helper. The full release-labelled pipeline was not separately rerun. See Documentation/ColourCorrectedValidation.md and Evidence/colour-corrected-source-correspondence.json.

## Relationship to version 1.1.1

Version 1.1.2 retains the FP cubic-sign and external-ordering comparison correction from [1.1.1](Release-1.1.1.md). That isolated correction leaves the inspected ordinary closed-FP one-loop QQ amplitude unchanged because both vertices in its ghost bubble reverse sign. The independent FeynCalc colour correction in 1.1.2 does change the finite amplitude. Neither statement supersedes the other's scope. See the README version comparison and ColourCorrection.md.

The subsequent reduced source revision excludes the new PT Ward notebook, effective-charge helper, example, regression and documentation. Existing 1.1.1 PT interfaces are retained. The GitHub package, source tag, validation attachment and checksums are updated together. The initial commit remains in Git history. The reduced distribution passes 31 process checks; the initial 32-check report is historical. Zenodo file replacement is handled separately and must be checked by checksum.
