# Reproducing the calculation

Version 1.1.2 uses an empty `Work/` in this release directory. Do not reuse earlier Work caches or session files. `RGZLoadFeynCalc[]` verifies the corrected colour contractions automatically. See [the colour correction](ColourCorrection.md).

The unreleased backend fixes dated 2026-11-06 change calculation-source fingerprints. Historical v1.0.0 certificates and caches remain evidence for that archived source, not for the patched tree. Use a fresh kernel and preferably a separate new Work directory to recompute and certify the corrected source. Explicit graph resumes reject missing or changed environment provenance. Changing the scaleless-preservation switch also selects a different integral/matrix environment; do not combine graph stages from the two conventions.

PT candidate tasks also reject live environment changes before skipping or reading completed stages. Reinitialize with `RGZPTStart[]` after changing the integral policy, assumptions or kinematics; only matching-environment results may be imported. Logarithmically divergent scaleless retention uses native TID, while supported shifted massive integrals can use scalar preprocessing with their original mass labels and routing. PT candidate checks remain distinct from full QQ certification.

The 2026-11-07 PT follow-up also binds completed stages to their original expressions and predecessor/result hashes. Old PT stages without these records must be recomputed. Part updates invalidate affected downstream stages and gauge comparisons; a saved comparison is exposed only for its matching current sum. `RGZPTContinueAll` preserves an existing FullMatrix NAV task and its validated packet, and fails if that evidence is unavailable. Select `"NAVMode" -> "AAReplacement"` explicitly to change an existing full-NAV task to that candidate scope; `"NAVPacket"`, `"Mode"`, `"ReferenceGauge"` and `"ComparisonGauge"` can also be supplied explicitly.

Direct gauge-check inputs must be finite, fully evaluated and use the unassigned ``RGZWorkflow`Engine`\[Alpha]`` gauge symbol. Foreign-context alpha symbols and unnormalized parameter placeholders are rejected; normalize them explicitly before calling the checker. Coefficient and resumable-batch caches use the validated v3 policy and do not inherit success from old status labels. A zero residual proves only the supplied expression within its stated conventions, not independent diagram completeness or a full S-matrix theorem.

Start with [Start Here](<../Examples/RGZWorkflow — Start Here.nb>) and continue with [Diagnostics](<../Examples/RGZWorkflow — Diagnostics.nb>). Both distributed notebooks contain inputs only. Historical evaluated copies remain in the local cleanup backup and Work/Review.

| Order | Required calculation |
| --- | --- |
| 1 | FeynRules: tree extraction, vertex export, extra-field quadratic action and action-vertex coverage |
| 2 | Fresh FeynCalc kernel: propagator, vertex and labelled-permutation comparisons |
| 3 | Complete mixed matrix and all AV/NAV/box contributions, including external residues |
| 4 | Pure one-loop D-dimensional form factors and symbolic general-gauge difference |
| 5 | Preserve the gauge comparison file; restore the fixed Landau/M0 task |
| 6 | UV/IR analysis, scaleless/PV ledgers, local MSbar counterterms and finite representation |
| 7 | Tadpole ledger, mass-region matching, raw pole bounds and graph-weighted coverage |
| 8 | Complete mass ledger, independent finite audit and source-linked certification |
| 9 | Scalar pole/finite audits, simplification, singlet/octet conversion and 144-coefficient row presentation |

Run each stage only after its prerequisites succeed. A timeout is incomplete evidence. Rerun the same command to reuse completed checkpoints. Weighted assembly also saves T/L components and source-bound prefixes. Its exact scalar shortcut retains full D dependence, compares with the saved source and falls back to the original replay when inapplicable.

The weighted notebook limits match the reference workflow: 144000 seconds overall, 12000 per graph and 600 per integral. These are ceilings, not predicted runtimes. Kernel and required adapter sources must stay compatible with the selected task; never edit verification flags.

The scope is distinct-flavour massless external quarks, two dynamical flavours, alpha=0 and M->0+, with nonzero nondegenerate internal masses and nonexceptional s,t,u=-s-t. Preserve the declared IR conditions, positive renormalization scale squared and common analytic continuation from the Euclidean domain with +i0. See [scope](ModelAndScope.md) and [renormalization](QuarkRenormalization.md).

## Independent reproduction status

Version 1.0.0 distributes source, input-only notebooks and a compact author-run summary. The author confirms that the local full workflow began with an empty cache; final artifacts were rechecked on 2026-10-28. The initial empty state has no separately recorded machine-verifiable log. Raw author caches are not distributed, and no third-party independent reproduction is claimed. To perform that validation, initialize a new empty Work directory and execute the sequence above without importing existing results. Retain the generated manifests, linked audit evidence, environment/version records, final certificate and exact paper-comparison report. A release-tool or loading test is not a substitute for this calculation.

The 2026-11-07 fixed-denominator update requires `FixedFiniteScalar/2` checkpoint validation and payload hashes. Older fixed checkpoints remain on disk but are recomputed. Prepared coefficient tables also carry a content hash. Fixed PT comparisons carry the same current-candidate identity as the standard PT comparator. These checks do not establish graph or pinch completeness.

NAV Ward records produced by the 2026-11-07 update carry the `NAVWard/3` policy, an input identity covering the current workflow and transverse source tensor, and a kernel payload hash. Both the original tensor transformation and the polynomial Ward decomposition must reconstruct exactly. Final packet restoration checks every connection against the current NAV matrix. Old unbound records cannot serve as current evidence; source-keyed caches are regenerated while historical calculation directories remain available. These checks do not independently establish physical pinch completeness.

The 2026-11-07 legacy AA update applies neutral assumptions and emitted-propagator reconstruction to `RGZLoopSplitFactor`. AA reduction and batch totals reject nested stopped/nonfinite values before any denominator-free polynomial terms are discarded. `RGZGaugeDifference` rejects invalid inputs and foreign gauge symbols. These checks cover the legacy AA entry points and do not certify graph completeness or a physical S matrix.

The 2026-11-07 NAV matrix update requires `NAVMatrix/2` connection records bound to their actual converted vertex, field/leg metadata and current workflow, with tensor payload hashes. Quark-line attachments have separate mass-dependent checkpoints and per-operation time limits. Final matrix packets are accepted only when their aggregate tensors, integrands, field sums and counts agree with the individual records and current source identities. Older unbound records are recomputed; historical source-keyed calculation directories remain available. This validates recorded consistency, not independent physical graph completeness.

The NAV Ward follow-up rejects assigned internal mass labels and reconstructs the actual emitted propagators as well as the abstract fractions. Coalescing candidates are deduplicated; repeated-pole orders still come from the original polynomial. When an external colour factor is present, exactly one common structure constant is accepted. Colour-free compatibility inputs remain supported. Ward policy 2 records require regeneration under policy 3.

The 2026-11-07 QQ update includes the current scaleless-retention flag, assumptions, TID options and scalar-product definitions in `qqJobFingerprint`. A convention change requires `RGZQQStart` under that convention. Reduced parts use `QQPart/1` provenance binding their actual original integrand (and deferred NAV source), physical loop measure, environment and result hash. Missing or mismatching part provenance is recomputed on reduction and rejected on amplitude assembly; it is not repaired by changing flags. Legacy raw directories remain historical records. Full graph completeness, general-gauge cancellation and mass-region evidence still require their independent calculations.

The scattering self-energy checkpoint update uses `QQSelfCheckpoint/2` records with an inert input snapshot identity and a value hash. Snapshotting resolves supplied data without running the expensive stage; changing a relevant list, index, association value or stage expression invalidates reuse. Assignment-bound local symbols are normalized across kernel runs; known M0/gauge time budgets do not alter physical identity. Input capture, cache reads, stage evaluation and atomic writes each have the requested per-operation time limit; the enclosing task retains its separate total time limit. Legacy/unbound or altered records are recomputed. IO failures return a stopped result and preserve the previous completed file.

## Assembly identity inside one QQ task

A task `Fingerprint` fixes the model/workflow task, but the reduced contributions can change while that task remains active. New bare-amplitude packets therefore record `AssemblyInputHash`, binding the tree graph, mixed self energy, six reduced parts, on-shell residue, gauge/regulator and loop conversion in a fixed order. The form-factor packet carries this identity onward.

Automatic amplitude input, including the default audit reader, checks both identities against the active completed task, for in-memory and disk packets. Recomputing any contribution requires `RGZQQAmplitude[]` again before automatic form factors; an old or unbound packet gives `QQAmplitudeInputsChanged`. This does not require regenerating the diagrams. An explicit File remains an independent historical import and is not thereby certified for the active task. Old files are retained, not silently rewritten or assigned a new identity.

## Self-preparation performance update - 26 November 2026

- Snapshot only the selected matrix entry. Original cache input identities and cached value hashes remain verified; the verified selected-input identity is retained for later reuse.
- Resume from completed mass-limit sums once their task/source provenance and values verify, including the original fixed-gauge packets behind copied sums. Otherwise continue the original preparation path.
- Distinguish selection, input validation, input hashing, cache reading and result checking in progress. Reset the cache status for each new checkpoint.
- Remove the nested Dynamic from self-preparation monitoring. The adapter is outside Kernel to preserve interrupted-task source fingerprints. Physics formulas and certification flags are unchanged.

In an existing stopped kernel, load `Tools/SelfPreparationResume.wl`, then repeat the same preparation command. No kernel restart or cache deletion is required. The new release loads the adapter automatically through `RGZWorkflow.wl`.
