# Development

This is an independent copy of a working-tree snapshot, not a clean checkout of the recorded source commit. `SOURCE_SNAPSHOT.json` records the original hashes and that local source changes existed. Release-copy changes are limited to packaging/version metadata, external dependency discovery, portable tests/examples and publication documentation; algorithms have not been reorganized or deleted to make tests green.

## Interfaces

Keep the public `RGZWorkflow` context and kernel entry stable. Loader order is explicit in `Kernel/RGZWorkflow.wl`; some modules load additional siblings lazily. `Kernel/Backend` contains implementation adapters; `Kernel/Experimental` and experimental model companions are outside the supported paper baseline. API/data version is 0.3.0; this distribution label is 0.3.0-dev.1. Paclet versions use numeric syntax.

## Checks

Use `Tools/run_tests.py --suite core|algebra|integration|physics|all`. Every script runs in a fresh process. The integration suite exports actual FR data, then checks all default tree vertices/orders in FeynCalc. Physics regressions use fixtures copied from the isolated integration directory; never point `RGZ_TEST_WORK` at daily production data. Standalone manual/audit tests remain available but are not indiscriminately launched.

`--suite all` runs all selected suites. FeynHelpers/Package-X-specific tests are optional manual tests and are not included in the default gate. Tests that pass with synthetic packets validate algorithms/guards, not the full physical amplitude.

Logs/JSON go under `Evidence/runs` and are excluded from archives. Promoted, path-sanitized results are recorded in `Evidence/validation.json`. Build tools neither delete nor migrate Work data.

## Model and cache changes

Update companion files together and rerun native exports and comparisons. Model/source hashes are part of job keys; any schema/physics change requires a fresh work directory or explicit compatible migration. Never mask timeouts, force a residual to zero, fit nonlocal counterterms, or change a proof flag solely to pass a gate.

## Contributing

Use focused changes with stated physics assumptions and relevant tests. Keep new dependencies optional and document their provenance/licensing. Preserve symbolic D, correct Grassmann statistics, momentum routing and branch choices. Add failing regression evidence before fixing a substantive issue. Do not commit generated WXF, absolute paths, third-party installations or personal notebook outputs.
