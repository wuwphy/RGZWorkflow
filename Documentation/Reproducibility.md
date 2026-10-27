# Reproducing the calculation

Start with [Start Here](<../Examples/RGZWorkflow — Start Here.nb>) and continue with [Diagnostics](<../Examples/RGZWorkflow — Diagnostics.nb>). Both distributed notebooks contain inputs only. Historical evaluated copies remain in the local cleanup backup and Work/Review.

| Order | Required calculation |
| --- | --- |
| 1 | FeynRules: tree extraction, vertex export, extra-field quadratic action and action-vertex coverage |
| 2 | Fresh FeynCalc kernel: propagator, vertex and labelled-permutation comparisons |
| 3 | Complete mixed matrix and all AV/NAV/box contributions, including external residues |
| 4 | Pure one-loop D-dimensional form factors and symbolic general-gauge difference |
| 5 | Preserve the gauge comparison file; restore the fixed Landau/M0 task |
| 6 | UV/IR analysis, scaleless/PV ledgers, local MSbar counterterms and finite representation |
| 7 | Tadpole ledger, mass-region matching, raw pole bounds and graph-weighted coverage |
| 8 | Complete mass ledger, independent finite audit and source-linked certification |
| 9 | Scalar pole/finite audits, simplification, singlet/octet conversion and 144-coefficient row presentation |

Run each stage only after its prerequisites succeed. A timeout is incomplete evidence. Rerun the same command to reuse completed checkpoints. Weighted assembly also saves T/L components and source-bound prefixes. Its exact scalar shortcut retains full D dependence, compares with the saved source and falls back to the original replay when inapplicable.

The weighted notebook limits match the reference workflow: 144000 seconds overall, 12000 per graph and 600 per integral. These are ceilings, not predicted runtimes. Kernel and required adapter sources must stay compatible with the selected task; never edit verification flags.

The scope is distinct-flavour massless external quarks, two dynamical flavours, alpha=0 and M->0+, with nonzero nondegenerate internal masses and nonexceptional s,t,u=-s-t. Preserve the declared IR conditions, positive renormalization scale squared and common analytic continuation from the Euclidean domain with +i0. See [scope](ModelAndScope.md) and [renormalization](QuarkRenormalization.md).

## Compare with the paper

`RGZPaperResult[]` reads the inert author table and checks its hash. It does not seed caches or certify current results. After computing the final scalar rows, run:

```sh
/path/to/WolframKernel -script Tools/compare_paper_result.wls
```

This compares eight prefactors and all 144 coefficients over the 31-function catalogue, using exact algebra. The report goes to `Evidence/runs/paper-reproduction/`. The current completed run matches exactly; its compact summary is in [verified-run.json](../Evidence/verified-run.json).

The paper PDFs are not bundled. Their hashes and conventions are in [paper provenance](../Paper/provenance.json). On 2026-10-28 the author confirmed completion of the local full workflow starting from an empty cache. Final evidence and exact paper agreement were rechecked against the current calculation source. Reuse of checkpoints within that calculation does not imply that the run started with pre-existing results.

## Independent reproduction status

Version 1.0.0 distributes source, input-only notebooks and a compact author-run summary. The author confirms that the local full workflow began with an empty cache; final artifacts were rechecked on 2026-10-28. The initial empty state has no separately recorded machine-verifiable log. Raw author caches are not distributed, and no third-party independent reproduction is claimed. To perform that validation, initialize a new empty Work directory and execute the sequence above without importing existing results. Retain the generated manifests, linked audit evidence, environment/version records, final certificate and exact paper-comparison report. A release-tool or loading test is not a substitute for this calculation.
