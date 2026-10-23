# Reproducibility contract for the paper

The supplied manuscript is presently a five-page draft with author placeholders and unfinished sections. Its title and PDF SHA256 are recorded in `SOURCE_SNAPSHOT.json`; the PDF and unfinished article are not bundled or presented as final publication. No arXiv ID/DOI has been invented.

A reproducible paper release needs **both** frozen source and matching completed data. This archive currently supplies the source and explicitly listed regression evidence, not a precomputed full amplitude.

| Paper subject | Source / entry point | Required completed evidence |
|---|---|---|
| Action and conventions | Default Models companions | Exact action/field statistics, truncation, model SHA256 |
| Quadratic kernels and vertices | Start Here sections 1–2 | FR export, 69-entry scope, 48 vertices, 702 orders; separate missing free-field audits |
| Mixed one-loop self energy | `RGZMixedComputeAll`, `RGZMixedResult` | All matrix pairs and disjoint boson/ghost/quark sectors, completeness summary |
| Pure one-loop amplitude | `RGZQQAmplitude`, `RGZQQFormFactors` | Matching tree and LoopOrder=1 packets, exact reconstruction, coefficients |
| Gauge comparison | `RGZQQGaugeEarlyCheck` | Symbolic alpha, all coefficients, current model/fingerprint and prescriptions |
| UV/IR and MSbar | Dedicated diagnostics notebook | Raw-core/PV ledgers, local CT provenance, separated poles and closure flags |
| Mass dependence | Explicit parameter/branch limits | Surviving RGZ corrections, domain and degenerate-pole treatment |

## Data deposit

Deposit large WXF results in a separately versioned archive (for example a DOI-backed repository), not Git. Include a machine-readable manifest with release label, source archive SHA256, model SHA256, dependency versions, parameters, loop measure, branch/IR prescriptions, stage completion, expected row counts, raw/renormalized flags and each data-file SHA256. Make a small independent import/reconstruction check reproduce representative coefficients.

Do not relabel legacy probe-model results as results of the two-flavour model. Do not infer an IR-finite cross section merely from UV pole subtraction. Old Session files are not portable full-kernel snapshots.

## Freeze procedure

Run the release suites in isolated directories; populate validation evidence; complete the checklist. Update citation metadata with the actual authors, arXiv ID and data DOI when assigned. Tag the exact source revision and publish the generated archive plus its SHA256. Uploading to arXiv/GitHub/Zenodo is a separate action; this package creation does not publish anything.
