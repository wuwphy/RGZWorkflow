# Tools

- `DiagnosticsCompatibility.wl`, `WeightedAuditResume.wl`: required runtime audit adapters, loaded by the root entry point. Their bytes are preserved to retain existing audit identities.
- `build_examples.wls`, `diagnostics_cells.json`: rebuild the two clean input-only notebooks. Back up evaluated notebooks before using the builder.
- `compare_paper_result.wls`: exact comparison of the computed scalar result with the inert paper table.
- `run_tests.py`: essential fresh-kernel checks in isolated work directories.
- `check_sources.wls`, `check_notebooks.wls`: source syntax and clean notebook validation.
- `check_release.py`, `update_manifest.py --write`, `build_release.py`: validate files and create the source ZIP. Work, artifacts and generated run logs are excluded.
