# Reading the calculation

The [Kernel calculation map](../Kernel/README.md) gives the directory structure and the reading order. Start with the physical sum, then follow the terms. The progress display and the cache are aids for long computations; they do not define an additional physical prescription.

## Process and conventions

The implemented external process is distinct-flavour, massless quark scattering,

$$q(k_1)+q'(k_2)\longrightarrow q(k_3)+q'(k_4),\qquad k_i^2=0.$$

The exchanged momentum is $p=k_3-k_1=k_2-k_4$, with $s=(k_1+k_2)^2$ and $t=p^2$. The action uses $D_\mu=\partial_\mu-i g_s A_\mu^aT^a$, giving the ordered quark vertex $+i g_s\gamma_\mu T^a$. The FeynArts external-fermion ordering contributes a common minus sign relative to the hand-ordered current product. Manual NAV and self-energy attachments use the same ordering.

The package stores **$i\mathcal M$** in the tree and graph expressions. The assembled packet provides both `iAmplitude` and `Amplitude`. Keep $D$ symbolic through the pole and finite-part expansions.

## The one-loop sum

[OneLoop/Amplitude.wl](../Kernel/OneLoop/Amplitude.wl) contains the complete conventional sum in one function:

$$i\mathcal M^{(1)}=i\mathcal M_{\rm self}+i\mathcal M_{\rm AV_1}+i\mathcal M_{\rm AV_2}+i\mathcal M_{\rm NAV_1}+i\mathcal M_{\rm NAV_2}+i\mathcal M_{\rm Box_1}+i\mathcal M_{\rm Box_2}+2\Sigma_V(0)\,i\mathcal M^{(0)}.$$

The four factors of $\sqrt{Z_q}$ give the coefficient $2\Sigma_V(0)$ at one loop. The mixed self-energy insertion is the complete matrix contraction $D^{(0)}_{Ai}\Pi_{ij}D^{(0)}_{jA}$, not just the AA entry of $\Pi$. Closed quark and ghost loops belong to this mixed self energy; they are not added again through the probe graphs. No separate PT correction is added to this conventional sum.

There are three normalization locations:

| Term | Stored normalization |
| --- | --- |
| Mixed self energy | Raw TID source; assembly multiplies by `1/(2 Pi)^4` once |
| Reduced AV, NAV and box parts | Already include that conversion |
| On-shell `SigmaV` | Already includes `g_s^2 CF/(16 Pi^2)` |

Do not multiply the whole assembled one-loop expression by the measure again. The dimensional finite-part conversion and MSbar subtraction are performed later.

## The main chapters

The calculation is now organized as chapters rather than separate files for each optimization:

| Chapter | Mathematical content |
| --- | --- |
| `OneLoop/Amplitude.wl` | The complete conventional one-loop sum |
| `OneLoop/QuarkSelfEnergy.wl` | Quark self-energy on-shell residue |
| `OneLoop/QQMixedSelf.wl` | Mixed-matrix attachment and scattering-current connection |
| `Algebra/SpectralCoordinates.wl` | Exact spectral-coordinate coefficient reduction |
| `Renormalization/QuarkMassLimit.wl`, `Renormalization/MassRestoration.wl` | Laurent orders and linear restoration of physical parameters |
| `Algebra/Coefficients.wl` | Spectral coordinates, exact rational reduction and branch conventions |
| `Algebra/PassarinoVeltman.wl` | Scalar/vector master identities and Gram relations |
| `Identities/GaugeDependence.wl` | The gauge difference, its common basis and Ward reduction before integration |
| `Identities/QCDParts.wl` | The common massless master and all candidate QCD contributions |
| `Renormalization/UVIR.wl` | Master poles and the full amplitude's pole separation |
| `Renormalization/MassLimit.wl` | Bubble regions, negative-power bounds and the assembled regulator limit |
| `Identities/Consistency.wl` | Matching the finite amplitude, gauge result and mass-limit conclusions |

## Follow one term through the source

Read these files in order:

1. [OneLoop/Amplitude.wl](../Kernel/OneLoop/Amplitude.wl): the equation above, external residues and normalization.
2. [OneLoop/QuarkScattering.wl](../Kernel/OneLoop/QuarkScattering.wl): select the process, model and gauge; create the conventional graph task.
3. [OneLoop/QQMixedSelf.wl](../Kernel/OneLoop/QQMixedSelf.wl): attach the mixed matrix and connect the external currents. Follow its calls to [Algebra/MixedProjection.wl](../Kernel/Algebra/MixedProjection.wl) for the external weights, [Algebra/SpectralCoordinates.wl](../Kernel/Algebra/SpectralCoordinates.wl) for coefficient algebra, and [Renormalization/MassRestoration.wl](../Kernel/Renormalization/MassRestoration.wl) for restoration after the stated M0 expansion.
4. [OneLoop/QuarkGraphs.wl](../Kernel/OneLoop/QuarkGraphs.wl): quark vertices, two AVs and two boxes, NAV attachments and pole masses. The D-dimensional residue formula is in [OneLoop/QuarkSelfEnergy.wl](../Kernel/OneLoop/QuarkSelfEnergy.wl).
5. [OneLoop/EvaluateGraphs.wl](../Kernel/OneLoop/EvaluateGraphs.wl): evaluate a chosen graph part through Ward preprocessing, partial fractions, TID, pole restoration and one measure conversion.
6. [Renormalization/MSbar.wl](../Kernel/Renormalization/MSbar.wl): local counterterms and finite renormalized coefficients. [QuarkRenormalization.md](QuarkRenormalization.md) states the limit and subtraction conventions.

For the full NAV field matrix, read [OneLoop/NonAbelianVertex.wl](../Kernel/OneLoop/NonAbelianVertex.wl). Its optional Ward pinch decomposition is separate in [Pinch/VertexWard.wl](../Kernel/Pinch/VertexWard.wl).

[Identities/GaugeDependence.wl](../Kernel/Identities/GaugeDependence.wl) contains the actual general-gauge comparison formula directly. Its coefficient reduction, PV preparation and branch normalization are ordinary named calls, not later rewrites of that function.

## Running and interpreting the result

[Start Here](<../Examples/RGZWorkflow — Start Here.nb>) follows the derivation: export the model in FeynRules; restart for FeynCalc; obtain the mixed matrices; calculate the six conventional parts; assemble the bare amplitude and its form factors. Stop after section 5 when that is your goal. The subsequent gauge calculation is an independent check. [Diagnostics](<../Examples/RGZWorkflow — Diagnostics.nb>) contains UV/IR, MSbar, mass-limit and other detailed audits.

A bare amplitude, a zero gauge difference and a finite representation are distinct mathematical results. In particular, cancellation in the supplied graph sum does not establish graph completeness. Dropped regulator tadpoles must be combined with small-mass bubble regions in the stated order of limits. Those questions remain explicit in the separate evidence calculations; the simplified source layout does not declare them solved. The [remaining checks](OpenChecks.md) also retain the preceding implementation findings.

The complete load order is visible at the end of [RGZWorkflow.wl](../Kernel/RGZWorkflow.wl). Numerical work remains in `Work/`; it is not part of the source distribution. Historical results are retained when the source changes, but do not become evidence for the changed calculation automatically.

The conventional route never loads the PT engine. Candidate kernels and their task driver are in `Kernel/Pinch/`; both routes use the external-leg and PV tools in `Kernel/Algebra/`. See the [dependency map](../Kernel/README.md).

