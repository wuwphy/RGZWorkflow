# Remaining checks

The 26 November colour-corrected author calculation now closes its ordinary QQ general-gauge, mass/scaleless and finite/IR/branch checks within the declared domain. See [the completed validation](ColourCorrectedValidation.md). The original broader research list below is historical; it must not be read as the outstanding-check list of that completed certificate. Independent derivation of the full graph inventory, all-parameter extensions and PT/background-field completeness remain separate research tasks.

- An independent graph-by-graph correspondence for the complete mixed-field sum, including symmetry factors and ghost signs.
- General-gauge cancellation for the entire conventional amplitude, including the vertex and box remainders.
- Complete regulator-tadpole and small-mass bubble-region matching in the declared order of dimensional and mass limits.
- A fresh full finite-part, branch and IR-domain verification for the current calculation source.

The known local restoration-cache and graph-evaluation boundary findings have now been addressed as described below. These corrections do not complete the physics calculations above.

These are recorded findings, not assertions that a previous physical result necessarily encountered the failing paths. The historical archived release and the current source remain distinct. See [the calculation](Calculation.md) and [reproducibility](Reproducibility.md).

The local M0 restoration-cache findings were fixed on 2026-11-10. `Renormalization/MassRestoration.wl` now binds checkpoints to evaluated physical substitutions (including masses and the external invariant), verifies the saved value hash and rejects malformed/non-scalar master-coefficient rows and unresolved reductions. The linear restoration formula, generic D and principal-root prescription are retained. New version-2 restoration checkpoints are separate from the old unverified version-1 files, which are preserved. The targeted native regressions are in `Tests/MassRestorationSafety.wls`; this fix does not complete the physical checks above.

The QQ graph-evaluation boundary was corrected on 2026-11-10. The task, part reducer and amplitude assembly now require the same exact stored loop conversion, `1/(2 Pi)^4`. Reduced-part reads and both saves use the existing per-operation time budget. An outer abort or incomplete returned value ends in `Stopped`, and a valid reused reduced part remains labelled `Reused`. Native and fault-injection regressions are in `Tests/EvaluateGraphsSafety.wls`. Invalid or unsupported measure conventions are rejected; no new measure convention is inferred.

The runnable `Examples/QuarkBubble.wls` checks a single current-model sector, including the exact general-D trace formula, both reduction routes and cache continuation. Its success is deliberately narrower than full-matrix or full-amplitude certification.

The 2026-11-11 ghost/mixed examples add local independent tensor, projection and integral checks: all four U ghost classes for the AA bubble, the non-diagonal A-phi xi tadpole, and a separate transverse repeated-pole integral at a declared complex-mass point. This narrows the local coverage gap but does not close the complete graph inventory or the full gauge/IR/branch checks above.

New bare-amplitude packets bind the actual assembly inputs through `AssemblyInputHash`; automatic in-memory/disk reads reject a previous assembly from the same task after its components change. Historical explicit file imports remain separate.

The three preparation/display boundaries were resolved on 2026-11-11:

- Prepared mixed self energies carry `MixedSelfProof`, binding the actual matrix, task, gauge, regulator, calculation context and saved value. Reuse and task restoration validate this binding; unbound legacy results require preparation again.
- Empty vertex and ordering selections return `NotChecked` with zero rows.
- Headless FeynCalc loading isolates only `FrontEndObject::notavail` during startup. Traced causes include Wolfram unit initialization and an external FCConfig frontend-format setting. Notebook loading and other errors retain their handling.

The frozen-source regression in `Evidence/runs/20261111T063132-73484/` passed all 28 registered process checks, with no unrun entries and an unchanged source fingerprint. Three Python release-tool tests also passed. Separate fresh FeynRules/FeynCalc kernels checked the source-free ghost quadratic action and all four A-ghost vertices against the actual model rules; see [the examples](GhostAndMixedExamples.md). These results do not include a fresh complete mixed matrix or QQ amplitude.
