# Ghost and non-diagonal mixed examples

Run each script in its own fresh FeynCalc/FeynArts process:

```sh
/path/to/WolframKernel -script Examples/GhostBubble.wls
/path/to/WolframKernel -script Examples/MixedTadpole.wls
```

Both create temporary work and export their actual generated expressions and hand references. They use the bundled model companions, not a fresh FeynRules export. No production Work is read or modified. They complement the [quark-bubble example](QuarkBubbleWalkthrough.md); none certifies the full amplitude.

## 1. Four U-class ghost bubbles in AA

`GhostBubble.wls` generates the `A-A / GhostThreePointLoop` sector. Its four rows correspond to U[1] (FP), U[2] (omega), U[3] (zeta), and U[4] (Jacobian ghost). Each is compared with an independently specified tensor, before integration:

$$
\mathcal I^{ab}_{\mu\nu,j}
=-\kappa_j g_s^2 C_A\delta^{ab}
\frac{(q-p)_\mu q_\nu}{(q^2-a_j+i0)((q-p)^2-a_j+i0)}.
$$

The minus sign includes closed Grassmann-loop statistics. There is no additional diagram multiplier after generation. In the bundled model,

| Class | $a_j$ | $\kappa_j$ |
| --- | --- | --- |
| U[1], U[4] | $0$ | $1$ |
| U[2], U[3] | $\mu^2$ | $8D$ |

The two factors in $8D$ have different origins. The auxiliary ghosts are Lorentz vectors, giving a closed metric trace D. Their second color index is a spectator in the A coupling and has the eight values declared in the bundled SU(3) model. The active adjoint-color contraction separately gives $C_A$. The script leaves this active invariant as `SUNN`; the fixed spectator value 8 must not be advertised as a generic SU(N) result. Establishing a model with arbitrary index ranges is a separate check.

The source propagators are $i/q^2$ for scalar ghosts and $-i g_{\rho\sigma}/(q^2-\mu^2)$ for vector ghosts. The respective vertex signs and the closed-loop sign lead to the tensor above. The example compares the full converted tensor after color/Lorentz contraction, then both projections, for every class. U[1] and U[4] give identical AA bubbles here; that does not justify dropping either field from the complete model.

### Independent integral reference

Put $s=p^2\ne0$, $d_0=q^2-a$, $d_1=(q-p)^2-a$. The numerator identity is

$$2p\cdot q=d_0-d_1+s.$$

With the raw-TID conventions $I_A=i\pi^2 A_0(a)$ and $I_B=i\pi^2 B_0(s,a,a)$, translation invariance and polynomial scaleless integrals give

$$
\int\frac{q^2}{d_0d_1}=I_A+a I_B,\quad
\int\frac{p\cdot q}{d_0d_1}=\frac{s}{2}I_B,\quad
\int\frac{(p\cdot q)^2}{d_0d_1}=\frac{s}{2}I_A+\frac{s^2}{4}I_B.
$$

Applying $P_T/(D-1)$ and $P_L$ therefore predicts

$$
\Pi_{T,j}=-\frac{\kappa_j g_s^2 C_A i\pi^2}{D-1}
\left[\frac{A_0(a_j)}2+\left(a_j-\frac{s}4\right)B_0(s,a_j,a_j)\right],
$$

$$
\Pi_{L,j}=-\kappa_j g_s^2 C_A i\pi^2
\left[\frac{A_0(a_j)}2-\frac{s}4 B_0(s,a_j,a_j)\right].
$$

The script checks these formulas against the evaluated generated graphs. A ghost sector alone is not transverse: it does not demand a zero longitudinal coefficient. D, including the vector-ghost trace, remains symbolic. Multiply by the workflow's `1/(2 Pi)^4` once when using these raw entries in the physical normalization.

## 2. A-phi xi tadpole

`MixedTadpole.wls` generates the actual single graph in the ordered `A-phi / FourPointVertexLoop` sector. The quartic A-xi-xi-phi interaction closes its two xi legs. It is a non-diagonal contribution with both external auxiliary color indices retained before projection.

The hand construction uses the quartic coefficient $i g_s^2m_G^2/(4\sqrt N)$, its two equal xi-color terms, the identical-leg factor $1/2$, and

$$D_{\xi\xi}(q)=-\frac{i\alpha}{(q^2)^2+\alpha M^4}.$$

The surviving color network satisfies

$$\sum_{c,e,f} f^{ace}f^{bdf}f^{cfe}=-N f^{abd}.$$

Thus, with a denoting the A color and $(b,d)$ the open phi indices,

$$
\mathcal I^{a;bd}_{\mu\nu}
=-\frac{g_s^2m_G^2\alpha N}{4\sqrt N}
\frac{f^{abd}g_{\mu\nu}}{(q^2)^2+\alpha M^4}.
$$

The example compares this tensor with the generated graph and checks both projected integrands. These open phi indices do not produce an extra spectator factor 8. Projecting onto $X^a=f^{abd}\phi^{bd}/\sqrt N$ gives the normalized matrix-entry integrand

$$\mathcal I_{T,AX}=\mathcal I_{L,AX}
=-\frac{g_s^2m_G^2\alpha N}{4[(q^2)^2+\alpha M^4]}.$$

The corresponding entry is `(1,2)` in the transverse basis and `(1,5)` in the longitudinal basis. The other ordered pairs and sectors are uncomputed in this example.

### Nondegenerate complex poles

Keep $\alpha M^4\ne0$ and the package's principal-root prescription. Define $\ell_1=-i\sqrt{\alpha M^4}$, $\ell_2=i\sqrt{\alpha M^4}$. A direct partial fraction gives

$$
\Pi^{\xi,\mathrm{raw}}_{T,AX}=\Pi^{\xi,\mathrm{raw}}_{L,AX}
=-\frac{g_s^2m_G^2\alpha N}{4}\,i\pi^2
\frac{A_0(\ell_1)-A_0(\ell_2)}{\ell_1-\ell_2}.
$$

The script checks the generated result against this expression, not merely against a second implementation route. No $M\to0$ or $\alpha\to0$ limit is taken, and the complex A0 values are not evaluated numerically. This does not settle the full amplitude's contour, IR or ordered-limit questions.

### Separate transverse repeated-pole unit

The same script also checks an independently specified scalar integral with

$$\Delta(x)=(x-m^2)(x-\mu^2)+m_G^4=(x-a)(x-b).$$

It proves reconstruction of $1/\Delta(q^2)^2$ at symbolic masses, then compares its reduction at $a=5+3i$, $b=5-3i$ (`m=3, mu=1, mG=Sqrt[5]`) with

$$
\int\frac{d^Dq}{\Delta(q^2)^2}
=i\pi^2\left[
\frac{D-2}{2(a-b)^2}\left(\frac{A_0(a)}a+\frac{A_0(b)}b\right)
-\frac{2[A_0(a)-A_0(b)]}{(a-b)^3}\right].
$$

This follows by partial fractions and $J_2(a)=i\pi^2(D-2)A_0(a)/(2a)$. It tests the transverse RGZ complex-pole labels and repeated-pole coefficients with D retained. It is explicitly an additional integral-unit identity, not a second generated mixed tadpole or a proof at every mass degeneracy.

Both examples reopen their actual graph caches and verify that a completed scalar stage is skipped. This is continuation evidence for these cases, not full matrix or S-matrix certification.

## From the action to the ghost model rules

Use two fresh kernels, sharing only a new temporary work directory:

```sh
export RGZ_GHOST_RULE_WORK="$(mktemp -d /tmp/rgz-ghost-rules.XXXXXX)"
WolframKernel -script Examples/GhostActionRules.wls
WolframKernel -script Examples/GhostModelRules.wls
```

The first script loads the actual `.fr` through FeynRules, checks the ordered
quadratic ghost kernels against independent coefficients, and derives the four
antighost–ghost–A vertices. For the ordinary FP covariance the external BRST
source `rho` is explicitly set to zero. It does not discard a dynamical mixing
at nonzero source. Scalar ghosts have kernel `p^2`; auxiliary vector ghosts
have kernel `-(p^2-mu^2)` with open Lorentz and spectator indices.

The second script loads FeynCalc in a separate process. It creates the four
vertices using the bundled `.gen/.mod`, converts the actual FeynArts amplitudes,
and compares them with the freshly exported FeynRules vertices. It also checks
the model's scalar and auxiliary ordered propagator functions against the
quadratic-action inverses. Incoming/outgoing momenta and external Grassmann
ordering are handled by the existing vertex comparison; comparing bare class
coupling signs without these conventions is insufficient.

The two scripts save `ghost-action.wxf` and `ghost-model.wxf` in the temporary
work directory. Together with `GhostBubble.wls`, these checks connect the
source-free quadratic action and A-ghost vertices to all four AA ghost bubbles.
They do not check every ghost–xi interaction, certify the complete diagram set,
or establish a Slavnov–Taylor identity for the full amplitude.
