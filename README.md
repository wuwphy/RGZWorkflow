# RGZWorkflow 0.3.0-dev.1

A GPL-3.0-or-later Wolfram Language development snapshot for auditable tree and one-loop RGZ calculations. Loading the package defines APIs and does not start a calculation. This source distribution contains no third-party libraries, production caches, precomputed amplitude or manuscript PDF.

**Development status:** the default local model contains the constraint Jacobian ghosts and two dynamical quark flavours (default masses zero). The complete updated-model amplitude and UV/IR closure have **not** been recomputed for this release. Older probe-model gauge-zero/finite-amplitude records are not evidence for this model. See [scope](Documentation/ModelAndScope.md) and [validation](Documentation/Validation.md).

## Start

1. Install Mathematica/Wolfram Engine 13.3 or later and the external dependencies described in [Installation](Documentation/Installation.md).
2. Open [Start Here](<Examples/RGZWorkflow — Start Here.nb>). Export FeynRules data, restart the kernel, then run FeynCalc/FeynArts. Work is written to the explicitly selected `Work` directory.
3. Keep FeynRules and FeynCalc in separate fresh kernels. Do not import sessions from the original working project.

```wl
Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
```

Evaluate subsequent calls in a new input cell:

```wl
RGZReport[RGZInitialize[FileNameJoin[{project,"Work"}]]];
RGZReport[RGZDependencies[]];
```

## Verify this snapshot

```sh
python3 Tools/check_release.py
python3 Tools/run_tests.py --kernel /path/to/WolframKernel --suite core
python3 Tools/run_tests.py --kernel /path/to/WolframKernel --suite all
```

The complete suite additionally needs FeynRules and FeynCalc/FeynArts; set `RGZ_FEYNRULES_DIRECTORY` if FeynRules is not on Wolfram's path. Tests use isolated temporary directories. Heavy production matrix/amplitude calculations are not part of the release suite. Python-only CI checks structure, links and packaging; it does not execute Mathematica.

## Layout

| Path | Contents |
|---|---|
| `Kernel/`, `RGZWorkflow.wl`, `PacletInfo.wl` | APIs, algorithms, loaders |
| `Models/RGZ_W2.6.*` | Default two-flavour RGZ model; filenames retained for compatibility |
| `Models/Experimental/` | Separate fermionic-horizon research model, not connected to the default workflow |
| `Examples/` | Clean input-only notebooks |
| `Tests/`, `Tools/` | Regression tests, scoped runner, structural and packaging tools |
| `Documentation/`, `Paper/`, `Evidence/` | Scope, conventions, reproducibility contract and release-test evidence |

`RGZSave[]` saves a versioned workflow packet, not the entire kernel. Graph/integral/coefficient checkpoints are saved separately with fingerprints. Failures and timeouts are not proofs of nonzero physics or completed results.

For source modifications see [Development](Documentation/Development.md). For paper/data release see [Reproducibility](Documentation/Reproducibility.md) and [Release checklist](Documentation/ReleaseChecklist.md). API usage strings are indexed in [API](Documentation/API.md).

## License and citation

[GPL-3.0-or-later](LICENSE); complete text in [COPYING](COPYING). Dependencies have separate terms and are not bundled: [THIRD_PARTY](THIRD_PARTY.md). Cite this exact snapshot and the physics/software references described in [CITATION.cff](CITATION.cff). No arXiv identifier or DOI is assigned in this snapshot.

[中文说明](README.zh-CN.md).
