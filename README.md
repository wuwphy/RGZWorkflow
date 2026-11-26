# RGZWorkflow 1.1.2

Wolfram Language source package for tree-level and one-loop quark scattering in the refined Gribov–Zwanziger (RGZ) framework at zero temperature. The implemented process is distinct-flavour scattering with massless external quarks, including the complete mixed-field self-energy insertion, vertex corrections, box diagrams and external-leg residues.

Companion paper: [*Gauge-parameter cancellation and finite form factors in one-loop quark scattering in the refined Gribov-Zwanziger framework* — arXiv:2610.38463](https://arxiv.org/abs/2610.38463).

[![Version 1.1.2 DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.24564596.svg)](https://doi.org/10.5281/zenodo.24564596) [![All versions DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.23724291.svg)](https://doi.org/10.5281/zenodo.23724291)

*If you use RGZWorkflow in your research, please cite the accompanying paper and the Zenodo DOI.* [Copy BibTeX](#citation).

**[Project website](https://rgzworkflow.pages.dev)** — physics overview, reproduction guide, results and citation information.

## Physical calculation

```text
actual RGZ action -> tree kernels, propagators and vertices
                  -> complete mixed-field one-loop self energies
                  -> quark vertices, boxes and external-leg residues
                  -> conventional qqprime amplitude and form factors
                  -> gauge, UV/IR, MSbar, mass-limit and finite-part checks
```

The calculation retains general D until the dimensional expansions. The final comparison concerns two dynamical flavours, massless external quarks, Landau gauge and M -> 0+, within the declared complex-mass and IR domain. The symbolic general-gauge comparison is a separate check. Branches are continued from the declared Euclidean domain with a common +i0 prescription; no all-parameter or inclusive-cross-section claim is made.

The [calculation guide](Documentation/Calculation.md) explains the physical sum and normalization; [model and scope](Documentation/ModelAndScope.md) states its assumptions. The [Kernel map](Kernel/README.md) follows the tree, one-loop, algebra, renormalization and identity chapters. The conventional scattering route is separate from the optional PT candidate workflow.

## Reproduce the calculation

Install the [external dependencies](Documentation/Installation.md), then use the two input-only notebooks:

1. **[Start Here](<Examples/RGZWorkflow — Start Here.nb>)**: export the actual model in a fresh FeynRules kernel; restart in a separate FeynCalc kernel for tree checks, mixed matrices, the bare amplitude and form factors. Continue with the independent general-gauge check and restore/save the fixed-gauge task.
2. **[Diagnostics](<Examples/RGZWorkflow — Diagnostics.nb>)**: recover the fixed-gauge task and assembled amplitude, then run UV/IR separation, MSbar, ordered mass-limit and weighted-coverage audits, finite reconstruction and conditional certification. Stop if a prerequisite fails.

Use a fresh kernel and a separate Work directory for a new release calculation. The [reproduction instructions](Documentation/Reproducibility.md) and [renormalization conventions](Documentation/QuarkRenormalization.md) give the order of operations, limit prescriptions and interpretation of the results.

For a small derivation before the full run, follow the [quark-bubble walkthrough](Documentation/QuarkBubbleWalkthrough.md) and [runnable example](Examples/QuarkBubble.wls). Independent [ghost and mixed-field examples](Documentation/GhostAndMixedExamples.md) cover further tensor, colour, projection and integral conventions.

## Citation

| Citation scope | DOI |
| --- | --- |
| RGZWorkflow, all versions (concept DOI) | [10.5281/zenodo.23724291](https://doi.org/10.5281/zenodo.23724291) |
| Version 1.1.2 | [10.5281/zenodo.24564596](https://doi.org/10.5281/zenodo.24564596) |
| Version 1.1.1 | [10.5281/zenodo.24470745](https://doi.org/10.5281/zenodo.24470745) |
| Version 1.1.0 used for a calculation | [10.5281/zenodo.24212586](https://doi.org/10.5281/zenodo.24212586) |

Use the version DOI for reproducible citations of a specific calculation; use the concept DOI when referring to the project across versions. The software BibTeX below identifies version 1.1.2 using its version-specific DOI. The 1.1.0 DOI above remains historical.

```bibtex
@article{Wu2026RGZ,
  author        = {Wu, Wan},
  title         = {Gauge-parameter cancellation and finite form factors in one-loop quark scattering in the refined Gribov-Zwanziger framework},
  journal       = {arXiv preprint arXiv:2610.38463},
  year          = {2026},
  eprint        = {2610.38463},
  archivePrefix = {arXiv},
  url           = {https://arxiv.org/abs/2610.38463}
}

@software{Wu2026RGZWorkflow,
  author    = {Wu, Wan},
  title     = {RGZWorkflow},
  version   = {1.1.2},
  year      = {2026},
  publisher = {Zenodo},
  doi       = {10.5281/zenodo.24564596},
  url       = {https://github.com/wuwphy/RGZWorkflow/releases/tag/v1.1.2}
}
```

## Corrections in 1.1.1 and 1.1.2

These are two separate sign corrections with different consequences for the ordinary one-loop QQ amplitude. Version 1.1.2 includes the 1.1.1 correction.

| Version | What was corrected | Effect on the QQ amplitude |
| --- | --- | --- |
| [1.1.1](Documentation/Release-1.1.1.md) | The ordered FP ghost–gluon FeynArts class coefficient (`+gs f` to `-gs f`) and the compensating external-ghost ordering normalization in the tree comparison. This was a model/comparison convention error, not the FeynCalc bug below. | The inspected ordinary closed-FP one-loop QQ contribution is unchanged: the AA ghost bubble contains two cubic vertices, so reversing both signs leaves its integrand unchanged. This does not extend to open ghost chains or arbitrary source amplitudes. |
| [1.1.2](Documentation/Release-1.1.2.md) | A duplicated pattern in FeynCalc `SUNSimplify` can give wrong signs in four-/five-structure-constant contractions. RGZWorkflow verifies and, if needed, corrects the rule in memory before calculation. | The affected mixed-self-energy contributions and the finite QQ amplitude change. The connected gauge difference is preserved; the corrected author run independently finds all 18 symbolic gauge rows zero. |

The 1.1.1 general-gauge ghost-sector UV check agrees with the native FeynRules rule after the correction; it is not a proof of every off-shell mixed RGZ Slavnov–Taylor identity. See [the FP convention review](Documentation/Release-1.1.1.md) for the counterterms, permutations and scope.

The tested FeynCalc **10.1.0 and 10.2.1 release archives** contain the duplicated rule. The official upstream code fix is [1c5d2d8](https://github.com/FeynCalc/feyncalc/commit/1c5d2d88e9bd08738732486e45b97240ceca808c); [1ee4800](https://github.com/FeynCalc/feyncalc/commit/1ee480093293a01a82b3e365d026808f1d4e1c17) adds regression tests. The checked `hotfix-stable` snapshot contains the fix. A version number alone is insufficient: verify the actual contractions. `RGZLoadFeynCalc[]` automatically calls [the helper](Tools/FeynCalcColourCorrection.wl); four identities must pass. Installed FeynCalc files are never edited. Keep dependencies fixed during an ongoing corrected run; update and verify in a fresh kernel for a subsequent run. Details and mathematical identities are in [ColourCorrection.md](Documentation/ColourCorrection.md).

**The finite amplitude changes, while the gauge difference remains zero.** Some individual gauge-dependent diagrams do change: their correction cancels only after the physical external-leg-weighted sum. This is not an assertion that every gauge-dependent diagram is unaffected, nor a full PT/background-field proof. The corrected author run passes **24/24 certification checks** within the declared IR conditions and branch prescription, with no outstanding checks. See [completed validation and source correspondence](Documentation/ColourCorrectedValidation.md).

The author confirms that the current results have now been recomputed and validated with **RGZWorkflow 1.1.2**, starting from an empty `Work/`. Cite the 1.1.2 version DOI for these results. The archived validation package attached to the release records the earlier corrected calculation and its exact source correspondence; it is retained unchanged and is not relabelled as evidence from this subsequent release-version run. Historical paper-table equality predates the finite correction and is not claimed for the corrected result.

### Other implementation changes in 1.1.2

- **Self-preparation and resumption:** [SelfPreparationResume.wl](Tools/SelfPreparationResume.wl) selects one checkpoint input before hashing, reuses verified completed mass-limit sums, and correctly reads the `QQBeforeGauge["QQ"]` snapshot. Read/validation failures propagate, and fallback shares the remaining total time budget. These changes avoid unnecessary matrix replay without changing physical formulas or declaring unfinished results complete.
- **Progress display:** self-preparation removes a nested `Dynamic` and reports selection, hashing, cache reads and result checks separately. The root loader loads the adapter automatically; existing stopped sessions can load the helper explicitly and resume.
- **Examples and research:** updated input-only Start Here and Diagnostics notebooks accompany the optional [PT Ward benchmark](<Examples/RGZWorkflow — PT Ward Benchmark.nb>) and [effective-charge example](Documentation/PTEffectiveCharge.md). The PT tools are explicitly loaded research tools; they do not certify PT completeness, RG invariance or a physical infrared freezing prediction.
- **Validation and packaging:** 32/32 process regressions and 3/3 Python release-tool tests pass. The release includes compact evidence and source correspondence; source-level regressions and full physical certification have distinct scopes. Public calculation interfaces are retained.

Use an **empty `Work/`** for a new release run. Do not copy earlier caches or `Session.wxf`; rerun Start Here sections 1–7 and Diagnostics. Keep FeynRules and FeynCalc in separate kernels. Start Here no longer contains section 8, the author-electronic-reference comparison; the historical table and reader remain available explicitly. See [the complete 1.1.2 notes](Documentation/Release-1.1.2.md) and [change history](CHANGELOG.md).

## Required files

| Location | Purpose |
| --- | --- |
| `RGZWorkflow.wl`, `Kernel/`, `PacletInfo.wl` | Entry point and calculation source snapshot |
| `Models/` | The matching `.fr`, `.gen` and `.mod` model companions |
| `Examples/` | Main calculation and diagnostics notebooks, optional PT benchmark and small physical examples |
| `Tools/` | Required audit adapters, notebook builder, comparison and validation tools |
| `Documentation/` | Installation, scope, main API and reproduction sequence |
| `Paper/` | Inert reference coefficient table and provenance |
| `Evidence/` | Compact record of the completed author's run |
| `Tests/` | Essential loading, audit, reconstruction and filesystem checks |

`Work/` contains local results and resumable caches. It is retained in the working project and excluded from the source ZIP. It is separate from the calculation source; no local results are deleted by the source reorganization.

## Use

Install the [dependencies](Documentation/Installation.md), then load the root entry point in its own input cell:

```wl
Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
```

Evaluate initialization and subsequent commands in later cells. The notebooks supply `project` and the complete sequence. Loading never starts a calculation.

See [reproduction](Documentation/Reproducibility.md), [renormalization](Documentation/QuarkRenormalization.md), [scope](Documentation/ModelAndScope.md) and [main API](Documentation/API.md).

## Validation and source ZIP

```sh
python3 Tests/ReleaseTools.py
python3 Tools/run_tests.py --kernel /path/to/WolframKernel --suite all
python3 Tools/check_release.py
python3 Tools/build_release.py
```

Checks use isolated work directories. `--suite core` needs no external physics packages; `--suite algebra` additionally needs FeynCalc/FeynArts. The release builder targets `artifacts/RGZWorkflow-1.1.2.zip`; preserve the published archive when packaging unreleased changes.

[GPL-3.0-or-later](LICENSE); full text in [COPYING](COPYING). External software is installed separately; see [attribution](THIRD_PARTY.md) and [citation](CITATION.cff).

## License and attribution

The author confirmed on 2026-10-28 that the included code, models and electronic reference table may be distributed under GPL-3.0-or-later, with necessary permissions obtained. This confirmation does not apply the code license to the manuscript or external software. Machine-readable citation metadata are in [CITATION.cff](CITATION.cff).
