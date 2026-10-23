# Validation

Machine-readable outcomes are in [validation.json](../Evidence/validation.json). Only executed checks with successful exit status are marked passed. The test runner separates core, algebra, native model integration and selected physics regressions. Source hashes for the actual shipped files are in RELEASE_MANIFEST.json.

The full updated two-flavour mixed matrix/amplitude, symbolic full gauge cancellation and complete UV/IR closure were not recomputed for this release. Existing historical probe calculations are excluded. Experimental horizon models have separate research status and are not certified by the default-model integration suite.

Python-only CI cannot execute proprietary Wolfram kernels or certify physics. Cross-platform validation and the paper's completed data remain separate publication requirements.
