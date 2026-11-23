# RGZWorkflow 1.1.1

Release date: 2026-11-23.

## Correction

Align the FP ghost–gluon FeynArts class coefficient with an independent native FeynRules export. For the ordered fields `C[-U[1], U[1], V[1]]`, the first tensor basis vector is the antighost momentum. Matching this basis and cyclic colour order gives `-gs f`, whereas the previous model supplied `+gs f`.

The single external FP antighost/ghost/gluon tree comparison now removes its external-state ordering sign separately. Previously the opposite class sign and missing external normalization compensated, so the old tree test could pass. The comparison correction is restricted to this FP three-point field content; it is not a universal sign change for eta, vector ghosts or multiple ghost pairs. Public interfaces are unchanged.

An isolated principal FP/AAA general-gauge UV calculation previously differed from the conserved-current scalar-vertex requirement by `3 alpha CA/4`. Using the native class sign and a freshly normalized tree resolves that discrepancy. With `gs^2/(16 pi^2 epsilonUV)` stripped:

- `deltaZc = CA (3-alpha)/4`;
- `deltaZghostVertex = -CA alpha/2`;
- `deltaZg + deltaZA/2 = -CA (3+alpha)/4`.

This is a principal-sector UV result, not a complete off-shell mixed RGZ ST identity.

## Effect on the previously saved QQ amplitude

**This isolated FP cubic-sign correction leaves the previously saved ordinary one-loop QQ bare amplitude unchanged.** The quark probe vertices, boxes and quark external residues contain no FP field; NAV uses bosonic raw FR tensors. In all three inspected saved mixed-matrix manifests, the FP contribution occurs only in the AA ghost bubble, once per manifest. Its raw integrand has two identical FP cubic couplings; reversing both signs leaves the entire integrand, including its finite contribution, exactly unchanged. Nine cached-integrand checks pass across those instances. The QQ MSbar implementation uses fixed Landau-gauge counterterms and does not infer them from the general-gauge ghost-vertex diagnostic above.

This conclusion concerns the isolated cubic correction in the physical closed-FP sector (`rho=0`). It is not a general assurance for source amplitudes, open ghost chains, other future coupling corrections or arbitrary regulator extensions. Historical certificates and result hashes are retained unchanged. No complete amplitude was regenerated under the 1.1.1 source fingerprint. The release is not a new S-matrix, general Ward/ST or PT certification.

## Validation and use

The regression `Tests/FPGhostConvention.wls` compares the actual FR cubic with the corrected FA tree and checks all six external field permutations. It also verifies that the new comparison map leaves eta, vector-ghost and multiple-FP-pair inputs unchanged. The compact review record is in [the evidence summary](../Evidence/fp-sign-review-2026-11-23.json). The isolated release regression is recorded separately from the historical full computation.

Use a fresh kernel and a separate Work directory when computing with this release: the model/source fingerprint changes even though the tracked old QQ contribution is invariant. Do not relabel old caches or edit their verification flags.

## Citation

The all-versions DOI is [10.5281/zenodo.23724291](https://doi.org/10.5281/zenodo.23724291). A new version DOI must be obtained from Zenodo; the 1.1.0 DOI `10.5281/zenodo.24212586` must not be reassigned to 1.1.1.
