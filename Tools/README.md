# Release tools

- `check_release.py`: read-only structural, link, source privacy, license, loader and manifest checks; Python standard library.
- `run_tests.py`: curated fresh-kernel test tiers and sanitized logs/JSON; explicit kernel executable required.
- `check_sources.wls`: syntax-only parsing of all shipped Wolfram/model/test sources.
- `check_notebooks.wls`: held input parsing, public API lookup and zero saved-output checks; no notebook cell evaluation.
- `update_manifest.py --write`: explicitly refresh hashes after reviewing intended source/evidence edits.
- `build_release.py`: read-only package collection into deterministic ZIP plus SHA256; excludes work/caches/run logs. Does not upload or initialize Git.

Do not run legacy audit scripts against production Work directories. See [Development](../Documentation/Development.md).
