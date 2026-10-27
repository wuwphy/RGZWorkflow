# Paper companion

The supplied nine-page main paper and seven-page supplement specify the workflow described in [Reproducibility](../Documentation/Reproducibility.md). Their hashes and analytic conventions are recorded in [provenance.json](provenance.json).

The author's exact electronic table is preserved, with a distribution license comment, in [data/form-factors.wl](data/form-factors.wl). Load it with `RGZPaperResult[]` after loading the package. It contains eight colour-spin rows and 144 coefficients over 31 functions. Indices are zero based; the common loop normalization is already included in each row prefactor.

This is reference data, not evidence that the current source tree or Work directory has reproduced the paper. Live certification requires the full linked evidence chain. The PDFs are not bundled in the distribution.

The completed author run was compared separately against this reference; all eight prefactors and 144 coefficients match exactly. See [verification](../Evidence/README.md). The reference file itself remains inert and does not certify another calculation.
