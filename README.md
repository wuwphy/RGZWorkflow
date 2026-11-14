# RGZWorkflow 1.1.0

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.23724368.svg)](https://doi.org/10.5281/zenodo.23724368)

Wolfram Language source package for the zero-temperature tree and one-loop RGZ quark-scattering workflow.

The calculation is organized by physical content: [tree theory, one-loop amplitudes, integral algebra, renormalization and identities](Kernel/README.md). Start with [OneLoop/Amplitude.wl](Kernel/OneLoop/Amplitude.wl) for the full sum; [the calculation guide](Documentation/Calculation.md) explains its conventions. Cache and display code are separate in `Kernel/Internal/`.

For a short calculation first, follow the [two-quark-bubble derivation](Documentation/QuarkBubbleWalkthrough.md) and run [QuarkBubble.wls](Examples/QuarkBubble.wls). It generates two actual diagrams and follows them to a mixed-matrix entry, with both reduction routes and explicit loop normalization.

For an actual run, open [Start Here](<Examples/RGZWorkflow — Start Here.nb>). Sections 1-5 obtain the bare amplitude and its form factors. The subsequent general-gauge calculation is a separate check; [Diagnostics](<Examples/RGZWorkflow — Diagnostics.nb>) covers UV/IR, MSbar and detailed mass-limit audits. FeynRules and FeynCalc use separate kernels.

The main route is:

```text
actual model -> tree kernels and vertices -> mixed self energies
             -> full QQ graphs -> reduction -> bare amplitude
```

`RGZQQStart` -> `RGZQQPrepareSelf` -> six `RGZQQRunPart` calls -> `RGZQQAmplitude` is the scattering sequence once the model and mixed matrices are ready. It does not load the PT candidate engine. Read [validation and renormalization](Documentation/API.md#2-validation-and-renormalization), [result expressions](Documentation/API.md#3-result-expression), or [detailed audits and continuation](Documentation/API.md#4-development-detailed-audits-and-continuation) when needed.

The completed 2026-11-14 calculation passes the declared-domain certification and reproduces the historical result and paper table exactly: eight prefactors and 144 coefficients over 31 functions. See [the 1.1.0 release notes](Documentation/Release-1.1.0.md) and [verification evidence](Evidence/README.md). The release changes the validated development snapshot's version metadata; it does not relabel existing Work certificates as newly computed 1.1.0 certificates.

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

Checks use isolated work directories. `--suite core` needs no external physics packages; `--suite algebra` additionally needs FeynCalc/FeynArts. The release builder targets `artifacts/RGZWorkflow-1.1.0.zip`; preserve the published archive when packaging unreleased changes.

[GPL-3.0-or-later](LICENSE); full text in [COPYING](COPYING). External software is installed separately; see [attribution](THIRD_PARTY.md) and [citation](CITATION.cff).

## Article companion release

Cite version 1.1.0 using [CITATION.cff](CITATION.cff) and the [versioned release](https://github.com/wuwphy/RGZWorkflow/releases/tag/v1.1.0). The companion manuscript is titled *Gauge-parameter cancellation and finite form factors in one-loop quark scattering in the refined Gribov-Zwanziger framework*. The manuscript is available as [arXiv:2610.38463](https://arxiv.org/abs/2610.38463) (submitted 2026-10-28).

The author confirmed on 2026-10-28 that the included code, models and electronic reference table may be distributed under GPL-3.0-or-later, with necessary permissions obtained. This confirmation does not apply the code license to the manuscript or external software.

## Cite this release

Wu, Wan. *RGZWorkflow* (Version 1.1.0). https://github.com/wuwphy/RGZWorkflow/releases/tag/v1.1.0

The DOI shown above identifies the archived v1.0.0 release. No new version DOI is assigned in this package. The software archive is separate from the companion manuscript, [arXiv:2610.38463](https://arxiv.org/abs/2610.38463). Machine-readable citation metadata are in [CITATION.cff](CITATION.cff).
