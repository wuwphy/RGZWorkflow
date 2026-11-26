# RGZWorkflow 1.1.2

Wolfram Language source package for tree-level and one-loop quark scattering in the refined Gribov–Zwanziger (RGZ) framework at zero temperature. The implemented process is distinct-flavour scattering with massless external quarks, including the complete mixed-field self-energy insertion, vertex corrections, box diagrams and external-leg residues.

Companion paper: [*Gauge-parameter cancellation and finite form factors in one-loop quark scattering in the refined Gribov-Zwanziger framework* — arXiv:2610.38463](https://arxiv.org/abs/2610.38463).

[![Version 1.1.0 DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.24212586.svg)](https://doi.org/10.5281/zenodo.24212586) [![All versions DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.23724291.svg)](https://doi.org/10.5281/zenodo.23724291)

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
| Version 1.1.0 used for a calculation | [10.5281/zenodo.24212586](https://doi.org/10.5281/zenodo.24212586) |

Use the version DOI for reproducible citations of a specific calculation; use the concept DOI when referring to the project across versions. The software BibTeX below identifies version 1.1.2 and explicitly uses the all-versions DOI until Zenodo assigns its version DOI. The 1.1.0 DOI above remains historical.

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
  doi       = {10.5281/zenodo.23724291},
  url       = {https://github.com/wuwphy/RGZWorkflow/releases/tag/v1.1.2}
}
```

## Version 1.1.2: corrected colour contractions

FeynCalc release archives 10.1.0 and 10.2.1 have a duplicated SUNF contraction rule. The official hotfix-stable branch contains the fix; version numbers alone do not establish whether an installation contains it. This release verifies and, when needed, corrects the rule in the current kernel before calculation. The installed dependency is not edited.

**The finite amplitude changes, while the gauge difference remains zero.** The corrected author run has 18/18 symbolic gauge rows equal to zero, complete mass/scaleless coverage, UV closure, IR-pole cancellation within the declared conditions, and verified finite reconstruction. Final certification is `CertifiedWithinDeclaredDomain` with no outstanding checks. Read [the correction](Documentation/ColourCorrection.md), [completed validation and source correspondence](Documentation/ColourCorrectedValidation.md), and [release notes](Documentation/Release-1.1.2.md).

The original certificate remains bound to the actual calculation source. Version metadata and automatic loading of the identical colour helper distinguish the release tree; those differences are explicitly recorded. The full pipeline was not separately rerun from the release-labelled tree. Historical paper-table equality predates the finite correction and is not claimed for the corrected result.

Use an empty `Work/` for a new release run. Start Here no longer contains the author-electronic-reference section. Keep FeynRules and FeynCalc in separate kernels.

## Required files

| Location | Purpose |
| --- | --- |
| `RGZWorkflow.wl`, `Kernel/`, `PacletInfo.wl` | Entry point and calculation source snapshot |
| `Models/` | The matching `.fr`, `.gen` and `.mod` model companions |
| `Examples/` | Two clean input-only notebooks |
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
