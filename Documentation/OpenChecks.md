# Historical verification scope for 1.1.0

For the 1.1.1 FP correction and its scoped unchanged-amplitude conclusion, see [Release-1.1.1.md](Release-1.1.1.md). Historical verification below is not relabelled as a new-source full calculation.

The completed 2026-11-14 local run passes the declared-domain gauge, UV/IR, MSbar, ordered mass-limit, weighted-coverage and finite reconstruction checks. See [the compact evidence](../Evidence/validation-2026-11-14.json). The final finite coefficients agree exactly with the historical calculation and paper table.

The weighted source coverage check is not an independent hand-drawn graph-by-graph bijection for the whole model. The certificate does not extend beyond its stated complex-mass domain and analytic sheet, and does not certify an inclusive cross section or all-parameter limits. The ghost-action example checks the source-free quadratic kernels and four A-ghost vertices, not every ghost-xi interaction.

The preparation, restoration, pole-routing and reporting findings documented in the changelog are fixed. A cosmetic dynamic-progress rendering issue has been observed in the Mathematica frontend; the single-layer display change discussed during review was not applied to this frozen calculation source. The certification results do not depend on that panel.

The full local run predates the version-only change to 1.1.0. Historical raw artifacts keep their original source fingerprints. New calculations should use a fresh kernel and a separate Work directory.
