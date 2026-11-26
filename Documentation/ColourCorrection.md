# Colour-contraction correction in version 1.1.2

## Origin and mathematical correction

The tested official release archives of FeynCalc 10.1.0 and 10.2.1 contain two contraction rules with the same left-hand side in `SUN/SUNSimplify.m`. The second overwrites the first. It should match `sunf2[b_, c_SUNIndex, a_SUNIndex, d_]`, rather than repeat `sunf2[b_, a_SUNIndex, c_SUNIndex, d_]`.

With f2(b,a,c,d) = sum_x f(x,b,a) f(x,c,d), the contractions are

```text
f2(b,a,c,d) f(a,c,e) = -CA/2 f(b,d,e)
f2(b,c,a,d) f(a,c,e) = +CA/2 f(b,d,e).
```

This can produce incorrect signs for products of four or five structure constants. The report is tracked in [FeynCalc issue 368](https://github.com/FeynCalc/feyncalc/issues/368). Updating to 10.2.1 alone was not sufficient in the tested official source archive.

`RGZLoadFeynCalc[]` now calls the distributed correction helper before any calculation. It changes only the current kernel. Four colour identities are checked; an already correct dependency is left unchanged. The notebook repeats the checks explicitly. The helper passed 58 five-structure-constant regression cases on each tested version. Installed FeynCalc files are never modified.

## Official fix and when to update FeynCalc

As checked on 25 November 2026, the latest formal GitHub release is [10.2.1](https://github.com/FeynCalc/feyncalc/releases/tag/Release-10_2_1). The tested release archive still has the duplicated rule. This must be distinguished from the evolving [hotfix-stable source branch](https://github.com/FeynCalc/feyncalc/blob/hotfix-stable/FeynCalc/SUN/SUNSimplify.m), which now contains the corrected rule. A version string alone does not establish whether the fix is present.

The official code fix is [commit 1c5d2d8](https://github.com/FeynCalc/feyncalc/commit/1c5d2d88e9bd08738732486e45b97240ceca808c), dated 3 September 2026. It includes the same index-order correction used here. The following [commit 1ee4800](https://github.com/FeynCalc/feyncalc/commit/1ee480093293a01a82b3e365d026808f1d4e1c17), dated 9 September 2026 and titled "Merge PR #123: Fixed a bug in SUNSimplify affecting products of SUNF with one common index", adds regression tests. Thus the upstream fix predates this RGZWorkflow correction; issue 368 is the later report, not the origin of the upstream fix.

There is no need to replace FeynCalc during an ongoing corrected recalculation. Keep that dependency environment fixed until the run is completed and saved. With an affected installation, `RGZLoadFeynCalc[]` applies the distributed in-kernel correction; all four notebook checks must be `True`. This does not modify the installed files.

For a subsequent calculation, prefer an official source snapshot containing the upstream fix, such as the checked hotfix-stable branch. Record the exact commit as well as the version, restart the kernel and repeat the checks. On a correct unmodified installation, the helper returns `Correction -> "AlreadyCorrect"`. In the release notebook this status can also mean that `RGZLoadFeynCalc[]` has already applied the correction before the explicit check, so that status alone does not prove that the installation on disk was updated. Do not change dependencies or mix earlier caches midway through one validation run.

## Effect on the amplitude

An exact comparison of the earlier Landau-gauge source calculation found 32 affected contributions among 975 contributions in the 25 ordered transverse mixed-matrix entries. The affected entries are AA and A-auxiliary entries. The tested NAV attachments, Abelian vertices, boxes and quark-residue colour factors did not change. The weighted mixed-self-energy difference is nonzero: the earlier finite amplitude is not numerically unchanged.

For the tested nonzero spacelike transfers the correction itself is UV/IR finite. This does not assert that the entire bare amplitude is IR finite. At one fixed action point (Nc=3, Nf=2, alpha_s=0.303141545878229, m2=-0.8254322989767313 GeV2, mu2=1.9477145813064896 GeV2, mG4=2.0801942208358764 GeV4), the ratios DeltaM/Mtree are:

| Euclidean Q2 (GeV2) | DeltaM/Mtree |
| --- | --- |
| 0.25 | +0.475078798% |
| 0.75 | +0.177107235% |
| 1.00 | +0.119065322% |

These are finite differences at the stated action point, not universal shifts or a newly certified full-amplitude calculation. Independent factorized adaptive and Gaussian integrations agreed to 3.2e-15 GeV2 in the sampled kernel coefficient. The corrected author workflow has since completed its declared-domain certification; see [the completed calculation and source correspondence](ColourCorrectedValidation.md). The sampled numerical differences above are not a substitute for that full record.

## Gauge difference is preserved

The general-gauge source and integrand comparison confirms that the colour correction preserves the ordinary on-shell QQ gauge difference. The finite amplitude changes, while its difference between gauge choices is unchanged:

```text
Mnew(alpha) - Mnew(0) = Mold(alpha) - Mold(0).
```

This conclusion comes from the actual matrix assembly, not from assuming that each gauge-dependent diagram is unaffected. Among the 975 mixed-source graphs, 841 raw amplitudes contain alpha. The 32 previously identified Landau corrections are alpha free even before that gauge is selected. A further 128 auxiliary-auxiliary contributions change individually and contain alpha. All 128 were recomputed through the native transverse projection and normalized-adjoint projection, comparing against the saved old projected tensors, with symbolic alpha and regulator M.

With the actual transverse tree external-leg weights u, their weighted sum satisfies the exact identity

```text
u^T [DeltaPi(alpha) - DeltaPi(0)] u = 0.
```

The weighted correction is alpha independent; it need not itself be zero. No Landau substitution or prior regulator-mass limit was used to obtain this cancellation. This distinction matters: individual matrix entries and diagrams can change their gauge-dependent parts even though the connected amplitude's gauge difference is preserved.

All 73 non-Abelian vertex connections were also reconstructed at symbolic alpha and M; their tensors are unchanged, including the 40 containing alpha. Fresh FeynArts Abelian-vertex, box and quark-self-energy graphs retain symbolic alpha and M and have unchanged colour reductions. The transverse external-leg weights contain no alpha. Together these checks establish the stated preservation of the examined ordinary QQ source-level gauge difference. A compact record is in [the gauge-colour check](../Evidence/gauge-colour-check.json).

The corrected author notebook run is now complete: the independent general-gauge comparison has 18 zero rows, the mass/scaleless and finite checks pass, and final certification is `CertifiedWithinDeclaredDomain` with no outstanding checks. The checked payloads and the source correspondence to version 1.1.2 are documented in [the completed validation](ColourCorrectedValidation.md). The original certificate retains its original source/task fingerprints. This is not a separate full PT/background-field proof. Start Here retains its reproducible general-gauge check.

## Recalculation and historical references

Extract this source release into its own directory and start with an empty `Work/`. Restart between FeynRules and FeynCalc. Do not copy earlier caches or saved sessions. Run Start Here sections 1-7, then Diagnostics. A new kernel must load FeynCalc through the workflow again.

The author-electronic-reference section has been removed from Start Here, together with the corresponding paper-comparison subsection of the reproduction guide. Historical reference data and their public reader are retained for explicit comparisons. Old paper tables and archived verification records are not corrected-release results. No verification flags have been edited.
