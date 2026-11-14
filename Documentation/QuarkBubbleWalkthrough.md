# Follow one quark bubble into a mixed-matrix entry

Run [Examples/QuarkBubble.wls](../Examples/QuarkBubble.wls) in a fresh FeynCalc/FeynArts kernel:

```sh
/path/to/WolframKernel -script Examples/QuarkBubble.wls
```

This small calculation uses the bundled FeynArts model and creates its own temporary work directory. It does not read or modify the production `Work/`. It needs no FeynRules session: it illustrates graph evaluation from the existing model companions, rather than independently certifying the action-to-model export. Follow Start Here for that separate check.

The output WXF contains the original FeynArts amplitudes, converted tensors, projections, split integrands, both scalar reductions and the resulting matrix entry. The script prints its location. These are actual newly computed expressions, not a copied reference result.

## 1. Specify the physical contribution

Choose the ordered external pair `A-A` and the single `QuarkBubble` sector:

```wl
RGZMXStart["FieldPair" -> {1,1}, "Category" -> {"QuarkBubble"},
  "reuseAACache" -> False]
```

The model has two dynamical massless quark flavours. Each supplies one closed fermion-loop diagram, so this calculation produces exactly two graphs, labelled by `FClass -> 1` and `FClass -> 2`. These are two species contributions, not an additional symmetry multiplier. The graph generator already includes the closed-fermion-loop sign and FeynArts combinatorial factors.

The functions shown in this guide use the canonical calculation context. The standalone script sets it up; it is not necessary to create copies of the physical symbols in the notebook global context.

## 2. Read the original tensor and its color factor

The Dirac trace uses the workflow convention $\operatorname{tr}1=4$, while Lorentz contractions remain D-dimensional. For one flavour, the numerator has the Dirac-trace structure

$$
\operatorname{tr}_D[\gamma_\mu\,\slashed q\,\gamma_\nu(\slashed q-\slashed p)]
=4\{q_\mu(q-p)_\nu+q_\nu(q-p)_\mu-g_{\mu\nu}\,q\cdot(q-p)\}.
$$

A translated/reflected loop routing gives the same integral. The propagators are massless, with denominators $q^2$ and $(q-p)^2$. Keep the numerator and denominator in the same routing.

The fundamental color trace is

$$\operatorname{tr}_c(t^a t^b)=T_F\delta^{ab},\qquad T_F=\tfrac12.$$

Summing the two flavours gives $N_fT_F=1$. No factor of $C_A$ is introduced. The script retains the external adjoint delta during graph reduction and removes it only when constructing the scalar AA matrix entry.

The independently written single-flavour integrand used in the script is

$$
\mathcal I^{ab}_{\mu\nu}(q,p)
=-g_s^2 T_F\delta^{ab}
\frac{\operatorname{tr}[\gamma_\mu\slashed q\gamma_\nu(\slashed q-\slashed p)]}
{(q^2+i0)((q-p)^2+i0)}.
$$

The minus sign is the closed-fermion-loop sign. No extra flavour factor is inserted into this one-flavour expression. The script compares it with each generated massless-flavour graph before integration: first the complete Lorentz tensor after the color trace, then its T and L projections. Both differences must vanish as rational functions of the loop invariants. This checks the actual numerator, loop sign and $T_F$ normalization in addition to the final integrated formula. The comparison uses the same $q,q-p$ routing in numerator and denominators; replacing only the denominator routing is not an identity.

`OriginalFAAmplitude` is the source diagram expression. After the explicit `Convert` stage, `Amplitude` is its FeynCalc tensor expression. Conversion is not integration.

## 3. Project at general D

Work at generic $p^2\ne0$ and $D\ne1$; an on-shell/scaleless limit is not being taken here. Define

$$P^T_{\mu\nu}=g_{\mu\nu}-\frac{p_\mu p_\nu}{p^2},\qquad
P^L_{\mu\nu}=\frac{p_\mu p_\nu}{p^2}.$$

For the unintegrated tensor $\mathcal I_{\mu\nu}$ the code forms

$$\mathcal I_T=\frac{P_T^{\mu\nu}\mathcal I_{\mu\nu}}{D-1},\qquad
\mathcal I_L=P_L^{\mu\nu}\mathcal I_{\mu\nu}.$$

The stored `Project` association contains these scalar integrands. They still depend on $q^2$ and $p\cdot q$. The longitudinal integrand need not vanish pointwise; its integrated coefficient vanishes after the integral relations are used. Do not set it to zero before integration.

## 4. Compare two reductions of exactly the same integrands

The `DecomposeT/L` stages prepare the propagators while preserving the numerator. For each saved split integrand the script runs:

```text
standard:    RGZMXIntegrate -> native TID -> RGZMXScalarReduce
accelerated: RGZMXIntegrate -> supported exact scalar identities
                            -> native TID fallback when needed
                            -> RGZMXScalarReduce
```

The existing `RGZMXSetAcceleration["Enabled" -> False|True]` selects the route. The two calculations use separate unit-cache directories.

Native TID/PV reduction can retain external notation such as `SPD[p,p]`, while the scalar route may return the internal `Pair[Momentum[p,D],Momentum[p,D]]`. The script first applies `FCI` to the difference, then checks exact rational cancellation. This changes notation, not the integral convention.

It compares both T and L for each of the two graphs. A timeout or failure stops the example; it does not count as equality.

## 5. Sum the sector and identify the matrix entry

`RGZMXTotal[]` sums the completed graph contributions. `RGZMixProjectPair[sector,{1,1},60]` extracts the normalized adjoint AA coefficient without expanding unrelated scalar coefficients.

Writing $s=p^2$, the two-flavour result in the stored raw-TID convention is

$$
\Pi^{q,\mathrm{raw}}_{T,AA}(s)
=-2i\pi^2 g_s^2\,\frac{D-2}{D-1}\,s\,B_0(s,0,0),
\qquad \Pi^{q,\mathrm{raw}}_{L,AA}(s)=0.
$$

The script checks this expression at symbolic D in addition to comparing the two algorithms. Keep D until its products with Laurent poles have been assembled.

This supplies the `(1,1)` entry in the transverse field order `{A,phi,phibar,beta,betabar}` and the `(1,1)` entry in the longitudinal field order. It is one field pair and one sector. The script does not fill uncomputed entries with zeros or declare a complete mixed matrix.

## 6. Apply the declared measure once

The graph source uses `PreFactor -> 1`; its TID normalization contains $i\pi^2$. In this package's stored convention the conversion is $1/(2\pi)^4$, hence

$$
\Pi^{q}_{T,AA}(s)
=-\frac{i g_s^2}{8\pi^2}\,\frac{D-2}{D-1}\,s\,B_0(s,0,0).
$$

The scalar master's dimensional/Laurent conventions remain those of the workflow. This example does not perform UV subtraction or choose a finite-part prescription.

For an insertion between tree AA lines, the code illustrates

$$d_{AA}^{(0)}(s)=-i\frac{s-\mu^2}{(s-m^2)(s-\mu^2)+m_G^4},\qquad
\delta D_{AA}^{q,T}=[d_{AA}^{(0)}]^2\Pi^q_{T,AA}.$$

The tree-line attachment is understood away from its propagator poles. There is no additional multiplication by i or graph symmetry factor. The complete mixed insertion would sum over all internal field labels; the complete QQ amplitude also needs vertices, boxes and external residues.

## 7. Reopen the cache

Finally, the script reopens the actual graph job and reruns a completed scalar stage. It must report `Complete; skipped`. This checks continuation of the same calculation, not a second independent physical derivation.

A successful run reports two flavours, agreement of each unintegrated tensor and its projections with the hand trace, equality of both routes, agreement with the integrated trace formula, a zero longitudinal coefficient and successful cache reuse. It does not certify the full S matrix.
