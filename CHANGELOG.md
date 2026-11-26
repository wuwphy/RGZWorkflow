# Changes

## RGZWorkflow 1.1.2 - 26 November 2026

- Correct the duplicated FeynCalc SUNF contraction rule in the current kernel during `RGZLoadFeynCalc[]`, including command-line use. Verify four colour identities before proceeding; preserve the installed dependency files.
- Distinguish the affected 10.1.0/10.2.1 release archives from the corrected official hotfix-stable branch. The upstream fix is commit 1c5d2d8; 1ee4800 adds regression tests. An ongoing corrected run need not update FeynCalc; keep its dependency environment fixed, then update and verify in a fresh kernel for a subsequent run. See Documentation/ColourCorrection.md.
- Record the resulting finite mixed-self-energy/amplitude correction, its tested scope and numerical examples in Documentation/ColourCorrection.md.
- Keep the release work directory named `Work/`; distribute no caches, sessions or computed outputs. Earlier results must be recomputed in an empty release Work directory.
- Remove Start Here section 8 (Author electronic reference), its historical introductory warning, and the corresponding reproduction-guide comparison section. Preserve historical data and public APIs.
- Preserve the ordinary QQ gauge difference: with symbolic alpha and regulator M, the native projection and actual external-leg-weighted mixed-matrix sum give u^T [DeltaPi(alpha) - DeltaPi(0)] u = 0. Individual gauge-dependent diagrams change, but their gauge dependence cancels in this sum; the other checked colour paths are unchanged. This is a source/integrand-level result. The corrected author run has completed: 18/18 symbolic gauge rows are zero and 24/24 declared-domain certification checks pass. The original evidence and release-source correspondence are documented in Documentation/ColourCorrectedValidation.md.
- Version DOI: https://doi.org/10.5281/zenodo.24564596. Historical version DOIs do not identify 1.1.2.

This release contains the current reorganized source tree, including the previously unreleased fixes documented below. Source-level regressions are distinct from a new full-amplitude certification.


## RGZWorkflow 1.1.1 - 23 November 2026

- Correct the ordered FP ghost–gluon FeynArts class sign to match native FeynRules; separate external ghost ordering normalization in the tree comparison. The previous two sign errors compensated in that comparison.
- Resolve the principal general-gauge ghost UV discrepancy without claiming a complete off-shell RGZ ST identity. Check all six external permutations and preserve other ghost-class comparison conventions.
- The inspected ordinary closed-FP one-loop QQ amplitude is unchanged because the FP bubble has two corrected cubic couplings. This statement concerns this isolated correction, not the subsequent 1.1.2 FeynCalc colour correction, open ghost chains or arbitrary source amplitudes.
- Preserve historical certificates; no complete amplitude was regenerated under the 1.1.1 source fingerprint. See Documentation/Release-1.1.1.md and Evidence/fp-sign-review-2026-11-23.json.
- Version DOI: https://doi.org/10.5281/zenodo.24470745.


## Unreleased shifted NAV denominator correction — 2026-11-13

- Restore expanded polynomials in a shifted loop square before QQ pole splitting, including RGZ quartic denominators and repeated poles; retain exact emitted-propagator reconstruction checks. Memoize repeated denominator restoration locally within one call.
- Preserve the actual pole-split failure as Cause in QQPoleSplitIncomplete, and stop failure reporting before indexing an interruption sentinel.
- Add shifted/repeated-pole reconstruction regressions. Existing completed artifacts are retained as historical source-bound records; they are not silently certified under changed source fingerprints.

## Unreleased preparation boundaries and frozen-source verification — 2026-11-11

- Bind prepared mixed self energies to the actual inputs and result; validate reuse and restored task packets instead of relying on SelfPrepared alone.
- Return NotChecked for empty vertex and ordering selections. Isolate only the unavailable-frontend display message during headless FeynCalc startup.
- Record distributed-source hashes, dependency versions, unrun checks and source stability in regression reports. All 28 registered process checks and three Python release-tool tests passed on the frozen snapshot.
- Add separate-kernel ghost-action/model examples: source-free quadratic inverses and four A-ghost vertices agree with the actual FeynArts rules. Full-amplitude recertification remains outstanding.

## Unreleased assembly identity and ghost/mixed examples — 2026-11-11

- Bind bare amplitudes to the actual completed tree, mixed self, six reduced contributions and on-shell residue with AssemblyInputHash; automatic amplitude reads reject stale same-task assemblies, including disk fallback and the default audit input. Explicit historical file import remains independent.
- Carry the assembly identity into form-factor keys and result metadata without changing amplitude formulas or public signatures.
- Add independently specified AA ghost-loop tensors and analytic integrals for all four U classes, preserving the vector-ghost Lorentz trace and the bundled spectator dimension.
- Add the genuine non-diagonal A-phi xi tadpole, its open-color and normalized-adjoint projections, its complex-pole integral, and a separately identified transverse repeated-pole unit check. No full-matrix or full-amplitude certification is inferred.
- Keep directory organization fixed. The subsequent preparation-boundary fixes are recorded above.

## Unreleased quark-bubble integrand cross-check — 2026-11-11

- Compare each generated flavour graph with an independently written closed-fermion-loop tensor before integration, including the minus sign, TF=1/2 and the common q,q-p routing.
- Check both the full Lorentz tensor and its T/L projected integrands before the existing general-D scalar formula, route-equality and continuation checks. Expose the hand tensor in the example output and explain the comparison in the walkthrough.
- Calculation-library formulas and public interfaces are unchanged; this extends the small physical example rather than reorganizing the package.

## Unreleased graph-boundary fixes and small physical example — 2026-11-10

- Require the declared exact QQ conversion 1/(2 Pi)^4 consistently at task validation, graph reduction and amplitude assembly. Reject zero, nonfinite and alternative normalizations without computing or saving a reduced part.
- Apply per-operation budgets to reduced-part IO and task saves, catch outer aborts, reject incomplete returned sentinels and preserve the Reused/Saved distinction in completed progress.
- Add a runnable A-A/QuarkBubble example and a mathematical walkthrough: actual two-flavour graphs, color/Dirac factors, general-D projections, both integral routes, scalar sector sum, AA matrix entry, one measure conversion and cache continuation.
- Separate shared loop primitives and two-point topologies from the standalone AA driver. Preserve the relocated definitions exactly; the conventional QQ loader no longer loads AA graph-generation functions.
- The earlier restoration-cache fix remains in place. These local corrections and examples do not promote historical results to current-source full certification.

## Unreleased M0 restoration-cache correction — 2026-11-10

- Bind linear restoration checkpoints to the actual mass/invariant substitutions and restoration definitions. Changing an assigned mass or scalar-product value no longer reuses a result from another physical environment.
- Check master/coefficient row structure before indexing, reject unresolved/nonfinite payloads and verify each saved value hash before reuse. A finite corrupted value is an invalid checkpoint, not a restored coefficient.
- Keep the linear physical substitution, optional bounded coefficient algebra, general D and principal-root prescription unchanged. Preserve old files while isolating new checked checkpoints under QQLinearM0Restore/2.
- Add native regressions for the reproduced stale-parameter and corrupt-value failures, malformed rows, valid reuse, external invariants and physical restoration identities.

## Unreleased gauge-coefficient responsibility split — 2026-11-10

- Keep ordinary radical/batch algebra in Algebra/GaugeCoefficients.wl and fixed-denominator mathematics in Algebra/CommonDenominator.wl, with explicit internal section and dependency descriptions.
- Move their checkpoint IO, progress and bounded diagnostics to Internal/GaugeRuntime.wl. Remove the fixed module's clearing/redefinition of ordinary batch-cache functions; fixed work calls its WXF IO directly.
- Preserve ordinary text writes and fixed WXF writes independently. Valid ordinary WXF records produced by the previous override remain readable without conversion or deletion.
- Retain public signatures and mathematical reduction definitions. Add module-reload and explicit cache-format regressions, and compare native residuals and cache reuse before/after in isolated work.

## Unreleased mixed-self-energy responsibility split — 2026-11-10

- Inventory the mixed backend by graph generation, algebra, integral reduction, task/cache and matrix assembly; record final owners and prerequisites in Documentation/MixedSelfEnergyStructure.md.
- Keep a sole RGZMXIntegralUnit dispatcher with independently named standard and accelerated routes. Route modules no longer clear or overwrite the entry point; repeated loading preserves explicit acceleration settings.
- Move mixed index/color/projector tools to Algebra/MixedProjection.wl, routed pole splitting to Algebra/MixedPoles.wl, standard/PV integration to Algebra/MixedIntegrals.wl and runtime/IO/progress to Internal/MixedRuntime.wl.
- Keep mixed graph generation, sector orchestration and matrix assembly in OneLoop/MixedSelfEnergy.wl. loadLoopEngine[] declares the full dependency order; the calculation file no longer loads its AA sibling implicitly.
- Preserve public signatures and mathematical formulas. Compare newly generated A-A quark-bubble projections, integrals, sums and cache recovery before/after on both routes in isolated work; this is a local refactor, not full recertification.

## Unreleased independent conventional QQ route — 2026-11-10

- Separate conventional QQ loading from candidate PT loading, including the former implicit candidate-kernel load in the AA backend. Shared AA attachments, SU(N) normalization and PV grouping live in Algebra/MixedProjection.wl.
- Move candidate kernels, NAV pinch extraction, task continuation and candidate/QQ comparison to Pinch/. Keep coefficient algorithms in Algebra/ and physical checks in Identities/.
- Give quark residues, mixed-insertion preparation, spectral-coordinate algebra and regulator restoration separate physical responsibilities. Existing public notebook commands are retained.
- Group the API into main calculation, validation/renormalization, result expression and detailed maintenance/research. Add module role/input/dependency headers and a dependency diagram.
- Preserve the mathematical definitions under source relocation; RGZPTColour is renamed internally to shared RGZLoopColour. Current-source fingerprints change, so historical results are retained and are not recertified by this reorganization.

## Unreleased integral-cache cleanup — 2026-11-10

- Remove an unused thaw parameter and obsolete temporary-symbol tracking from the tensor-integral cache. Restored values are already ordinary expressions; no temporary abbreviation definitions are created.
- Keep canonical index renaming, shared integral reuse, packet validation and native TID evaluation.

## Unreleased direct mixed-graph evaluation — 2026-11-10

- Put optional pre-scalar integral reorganization directly in the mixed graph runner. Remove load-time DownValues copying, the saved base runner and the forwarding acceleration wrapper.
- Retain the scalar formulas, activation conditions, native TID fallback and existing physical checks.

## Unreleased calculation cleanup — 2026-11-09

- Remove five uncalled predecessor helpers while retaining independent Ward and mass-limit reference formulas.
- Remove the disabled personal-upload shortcut and its unused VerifiedInput option from fixed-denominator reduction; both gauge branches still use the exact reducer.

## Unreleased zero-temperature scope cleanup — 2026-11-09

- Archive the unused thermal and standalone vacuum algorithms, their optional entry points and the one-time historical probe-sign migration outside the current source distribution.
- Remove the generic stage-registration registry and its two feature-specific tests; the physics workflow does not call it. Retain the physical tests and current-task continuation.
- Keep the useful propagator split and mass-restoration helpers in OneLoop/MixedMatrices.wl. The two notebooks and their calculation commands are unchanged.
- A complete pre-cleanup source backup is retained locally under artifacts/zero-temperature-scope-before-20261109.zip. Work data and the published v1.0.0 archive are unchanged.

## Unreleased physical chapter consolidation — 2026-11-09

- Consolidate gauge comparison, common-basis preparation and pre-integration Ward reduction in Identities/GaugeDependence.wl. Bring box projector contractions into the box Ward chapter.
- Put QCD reference coefficients, candidate pinch contributions and the calculated gluon-self-energy comparison in Identities/QCDParts.wl. The benchmark loads this chapter once after its gluon prerequisite.
- Combine master poles with UV/IR separation; combine mass regions, independent negative-power bounds and mass-limit assembly. Bring result consistency and its explicit mathematical checks into one chapter.
- Group model-content checks with Feynman-rule comparisons, and branch conventions with coefficient algebra. Calculation source chapters decrease from 67 to 55; the ordinary loader decreases from 41 to 31 entries.
- This is a source consolidation, with no new runtime framework, physics prescription or public command. Historical results and unresolved checks retain their original status.

## Unreleased physical calculation layout — 2026-11-09

- Replace the flat Kernel and generic Backend layout with Tree, OneLoop, Renormalization, Identities and Algebra chapters. Sessions and progress are confined to Internal; optional thermal/vacuum work is separate in Extensions.
- Give the calculation files physical names and update all direct loads, source references, notebook-independent tests and documentation links. Public function names and physics formulas are unchanged.
- Merge the four coefficient files into Algebra/Coefficients.wl, four PV files into Algebra/PassarinoVeltman.wl, and the two finite-coefficient presentation files into Algebra/FiniteCoefficients.wl. Preserve their definition order.
- Add a short Kernel reading map centered on the amplitude equation. No dependency-injection layer, new runtime framework or extra public workflow is introduced. Existing Work results and the published archive remain untouched.

## Unreleased physics-first source organization — 2026-11-08

- Put the conventional one-loop amplitude equation, external residues and the single mixed-self measure conversion in ScatteringAssembly.wl. Remove the layered recovery/measure redefinitions and the obsolete measure file.
- Collect mixed-self preparation, spectral simplification and M0 restoration in ScatteringSelfEnergy.wl. Call the algebra directly; progress monitoring no longer substitutes physical functions or selects the simplification algorithm.
- Expand ten load-time gauge-comparison rewrites into its ordinary definition. Keep its effective body unchanged apart from sequence grouping, and define public gauge/ward entry points once.
- Make certification aggregation an explicit named evidence calculation rather than a copied public definition. Remove obsolete public fallbacks and show the complete module order in one loader, without nested package-file loads.
- Add a physics reading guide, explain the main notebook stopping point and retain unresolved physics/implementation findings separately. Physical formulas, public commands, local results and the archived published source ZIP are preserved.

## Unreleased scattering checkpoint fixes — 2026-11-07

- Bind self-energy checkpoints to inert stage-input snapshots, current calculation context and source fingerprint; validate payload hashes before reuse. Recompute incompatible legacy or altered records.
- Preserve cache lookup without executing expensive stage algebra, normalize assignment-bound local names, and exclude the known M0/gauge execution-budget arguments from physical input identity.
- Time-limit input capture, reads, evaluation and atomic writes separately. IO timeouts stop without promoting a checkpoint or replacing its completed file. Spectral simplification formulas remain unchanged and are covered by exact and complex-branch regressions.

## Unreleased QQ scattering fixes — 2026-11-07

- Include the scaleless-integral convention, assumptions, TID options and scalar-product state in QQ task identities. Changed conventions require a fresh current task.
- Bind reduced-part files and in-memory results to the actual integrand, deferred NAV source, measure and calculation context. Recompute incompatible part files and reject unbound assembly inputs.
- Reject assigned QQ pole labels and verify actual emitted propagators after restoring physical masses under neutral assumptions.
- Validate M0 coefficient pairs before discarded-tadpole shortcuts, and reject invalid Laurent/split/reduction inputs. Physical residue and LSZ formulas are unchanged and are independently checked by the new regression script.

## Unreleased NAV pinch follow-up — 2026-11-07

- Reject assigned internal pole labels and verify actual emitted FAD denominators after restoring physical masses, under neutral assumptions.
- Deduplicate coalescing root candidates before determining multiplicity from the original denominator.
- Require a single common external structure constant when colour is present; reject residual internal/external colour objects. Preserve explicitly colour-factored compatibility inputs.
- Validate supported fields at the pinch builder and packet boundaries. Advance Ward validation to NAVWard/3; older policy records must be recomputed.

## Unreleased NAV matrix fixes — 2026-11-07

- Bind matrix connection checkpoints to current converted vertices and metadata, and validate tensor payload hashes before reuse. Reject unknown fields.
- Validate and time each quark-line attachment before summing or saving; preserve completed tensors and attachment checkpoints for continuation. Attachment identities include the quark mass.
- Validate final tensor, integrand, external-field and connection-count aggregates against their per-connection evidence. Restore final packets only for the current source identities and mass.

## Unreleased AA backend fixes — 2026-11-07

- Isolate the legacy AA pole splitter from session assumptions and verify actual emitted propagators after restoring pole labels.
- Reject nested invalid amplitudes and reduction values before denominator-free terms can be discarded; reject invalid per-graph and total AA results.
- Validate gauge-difference inputs, gauge-symbol contexts and results, with neutral assumptions for the comparison.

## Unreleased NAV pinch fixes — 2026-11-07

- Reject stopped/nonfinite Ward inputs, unsupported momentum routes and reserved coordinate collisions. Verify the original tensor after the coordinate transformation as well as the polynomial split.
- Bind each Ward checkpoint to its current source tensor and workflow, validate the kernel payload hash, and recompute incompatible records. Validate projections and final packet totals before saving.
- Validate restored final NAV packets against the current matrix connections, including their input identities. Older unbound evidence must be regenerated with the current source.

## Unreleased fixed-denominator fixes — 2026-11-07

- Reject nested stopped/nonfinite values and foreign gauge symbols before fixed reduction, completion and checkpoint writes.
- Validate checkpoint payload hashes and prepared coefficient contents; recompute incompatible legacy caches without deleting them.
- Use the current coefficient-cache context and validated row format. Bind fixed PT comparison reports to the current candidate and detect changes during comparison.

## Unreleased PT follow-up fixes — 2026-11-07

- Validate original QCD benchmark coefficients and physical payloads before subtraction, rejecting invalid values and incomplete placeholders.
- Bind every completed PT stage to its original kernel, representation, predecessor input, result and environment. Reject unprovenanced disk stages and inconsistent restored packets; invalidate dependent stages after part changes.
- Clear outdated gauge comparisons on part updates and bind reports to the current candidate sum, including self-energy data and NAV scope.
- Preserve FullMatrix NAV evidence in continuation. Missing evidence stops the call; an AAReplacement scope change requires explicit selection for an existing FullMatrix task. Continuation accepts explicit mode, reference, NAV packet and comparison-gauge options.
- These remain unreleased calculation-source changes. The archived v1.0.0 evidence does not certify this patched source.

## Unreleased backend fixes — 2026-11-06

- Isolate pole splitting from ambient assumptions and verify the actual emitted propagators after restoring general pole labels.
- Distinguish scaleless-preservation policies and assumptions in integral, graph and matrix cache environments. Retaining logarithmically divergent scaleless integrals bypasses the scalar fast path and uses TID.
- Validate nested stage values before saving or reusing checkpoints, invalidate dependent stages after a bad stored result, and reject graph caches without current environment provenance.
- Bind PT tasks and completed-result imports to the live source, model, integral policy, assumptions and kinematics; reject nested stopped values consistently in fresh, cached and assembled PT stages.
- Recover shifted quadratic propagators from native sums of Momentum objects. Preserve both mass labels and q+p/q-p routing in repeated-pole TID delegation, avoiding unnecessary fallback for the tested supported integrals.
- Reject nonfinite/stopped gauge inputs before equal-branch cancellation, and reject noncanonical gauge symbols explicitly. Validate physical coefficient payloads, status/value agreement and resumable batch results; isolate validated gauge caches under the v3 policy.
- These calculation-source changes require new current-source calculations and certification. The published v1.0.0 tag, DOI archive and author-run evidence remain historical snapshots; this patch is not a new fully certified release.

## 1.0.0

- Complete paper workflow: tree/model checks, mixed matrix, quark amplitudes, symbolic gauge comparison, UV/IR and ordered mass limits, linked certification and scalar presentation.
- Exact generic-D scalar audit acceleration and resumable weighted assembly.
- Verified author run: 256 records, 2890 graphs and all 24 certificate checks passed; all eight prefactors and 144 paper coefficients match exactly.
- Distribution cleanup retains the calculation source snapshot, model companions, two notebooks, required runtime adapters, concise documentation and essential checks. Historical material is archived locally; existing Work data are retained separately.

## Post-release documentation update — 2026-10-28

Record the author-confirmed full workflow from an empty cache and recheck final artifact hashes, six mass-evidence links, model/bare/gauge source compatibility and exact paper agreement. The initial empty state is author-confirmed; no third-party reproduction is claimed. The published v1.0.0 tag and original assets remain unchanged; this documentation update is available on main and in the release notes.

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

## Completed colour-corrected validation and release metadata - 26 November 2026

- Record the completed corrected author calculation, 18 zero gauge rows, complete mass/scaleless coverage and finite certification with no outstanding checks. Preserve original certificate/task identities.
- Record the exact two-file Kernel difference between the calculation snapshot and 1.1.2; formulas and model companions are unchanged by packaging.
- Restore historical 1.1.0/1.1.1 notes and the existing FP convention regression. Preserve website and copyable article/software citations.

## Distribution scope — 26 November 2026

- Exclude newly added PT/effective-charge research tools, examples, notebook and documentation from this ordinary QQ distribution. Existing PT interfaces from 1.1.1 are retained. Calculation formulas are unchanged.
- Correct the GitHub 1.1.2 package, tag, validation attachment and checksums together; retain the initial commit in Git history. The reduced distribution passes 31 checks; do not relabel the earlier 32-check report. Zenodo file replacement is a separate archival operation.
