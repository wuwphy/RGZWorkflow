# RGZWorkflow 1.0.0

Wolfram Language source package for the tree and one-loop RGZ quark-scattering workflow.

Open [Start Here](<Examples/RGZWorkflow — Start Here.nb>), then [Diagnostics](<Examples/RGZWorkflow — Diagnostics.nb>). Use separate FeynRules and FeynCalc kernels. In an existing FeynCalc session, follow Diagnostics section 1 and continue from section 3.

The current saved run passed all 24 certification checks within its declared domain. Its eight finite rows, 31 functions and 144 coefficients match the paper's electronic table exactly. See [verification](Evidence/README.md). An empty-cache independent rerun has not been performed.

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

`Work/` contains local results and resumable caches. It is retained in the working project and excluded from the source ZIP. Kernel and model bytes are unchanged by this cleanup, preserving existing source identities. Historical development files and the redundant local dependency copy are backed up under `artifacts/` and excluded from the distribution.

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

Checks use isolated work directories. `--suite core` needs no external physics packages; `--suite algebra` additionally needs FeynCalc/FeynArts. Build output is `artifacts/RGZWorkflow-1.0.0.zip`.

[GPL-3.0-or-later](LICENSE); full text in [COPYING](COPYING). External software is installed separately; see [attribution](THIRD_PARTY.md) and [citation](CITATION.cff).

## Article companion release

Cite version 1.0.0 using [CITATION.cff](CITATION.cff) and the [versioned release](https://github.com/wuwphy/RGZWorkflow/releases/tag/v1.0.0). The companion manuscript is titled *Gauge-parameter cancellation and finite form factors in one-loop quark scattering in the refined Gribov-Zwanziger framework*. No arXiv identifier or DOI has been assigned as of 2026-10-28. Manuscript identifiers will be added when available.

The author confirmed on 2026-10-28 that the included code, models and electronic reference table may be distributed under GPL-3.0-or-later, with necessary permissions obtained. This confirmation does not apply the code license to the manuscript or external software.
