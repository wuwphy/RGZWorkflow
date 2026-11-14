# API reading order

Start with [the calculation](Calculation.md) and [the dependency map](../Kernel/README.md). Public names and notebook commands are retained. The groups below distinguish the main calculation from checks, presentation and optional maintenance/research work. Options are documented by `?RGZFunctionName` and the two notebooks.

## 1. Main calculation: model to bare amplitude

| Step | Commands |
| --- | --- |
| Initialize a fresh kernel | `RGZInitialize`, `RGZLoadFeynRules` or `RGZLoadFeynCalc` |
| Extract the actual model | `RGZExtractTree`, `RGZExportVertices` |
| Compute mixed two-point functions | `RGZMixedStart`, `RGZMixedComputeAll`, `RGZMixedResult` |
| Prepare full conventional scattering | `RGZQQStart`, `RGZQQPrepareSelf` |
| Reduce the full vertices and boxes | `RGZQQRunPart` for AV1, AV2, NAV1, NAV2, Box1, Box2 |
| Assemble the bare amplitude | `RGZQQAmplitude` |

FeynRules and FeynCalc use separate kernels. The full NAV is supplied internally through `RGZNAVSum`. These commands do not start a PT candidate task. Follow [Start Here](<../Examples/RGZWorkflow — Start Here.nb>) for prerequisites and failure guards.

## 2. Validation and renormalization

| Question | Commands |
| --- | --- |
| Tree and model consistency | `RGZTreeBenchmark`, `RGZCheckPropagators`, `RGZCheckVertices`, `RGZCheckOrders`, `RGZExtraTreeAudit`, `RGZOneLoopCoverage`, `RGZCheckExtraPropagators`, `RGZCheckOneLoopCoverage` |
| BRST and Ward requirements | `RGZCheckIdentities`, `RGZIdentityRequirements` |
| Conventional gauge comparison | `RGZQQGaugeStart`, `RGZQQGaugePrepareSelf`, `RGZQQGaugeRunPart`, `RGZQQGaugeAssemble`, `RGZQQGaugeCheck`, `RGZQQGaugeRestore` |
| Pole separation and subtraction | `RGZQQUVIR`, `RGZQQMSbarCounterterms`, `RGZQQMSbar`, `RGZQQRenormalize` |
| Linked conclusions | `RGZQQFiniteAudit`, `RGZQQCertification`, `RGZQQSMatrix` |
| QCD comparison | `RGZQCDBenchmark` |

See [renormalization](QuarkRenormalization.md), [reproducibility](Reproducibility.md) and [remaining checks](OpenChecks.md). A bare amplitude, a zero residual and a conditional certified result are different outputs.

## 3. Result expression

| Representation | Commands |
| --- | --- |
| D-dimensional color and spin coefficients | `RGZQQFormFactors` |
| Scalar masters and basis checks | `RGZQQScalarFormFactors`, `RGZQQScalarPoleAudit`, `RGZQQScalarBasisAudit`, `RGZQQScalarFiniteAudit` |
| Compact finite coefficients | `RGZQQScalarFiniteSimplify`, `RGZQQScalarColorSimplify`, `RGZQQScalarFactorRows` |
| Author reference table | `RGZPaperResult` (reference only) |

These operations organize results; they do not create missing validation evidence.

## 4. Development, detailed audits and continuation

| Purpose | Commands |
| --- | --- |
| Inspect configuration and symbols | `RGZConfiguration`, `RGZDependencies`, `RGZSymbols`, `RGZSymbolAudit` |
| Inspect one graph or pair | `RGZMixedStartPair`, `RGZMixedComputePair`, `RGZRunGraph`, `RGZNAVSum`, `RGZSplit`, `RGZPoleMasses` |
| Detailed UV/IR and mass evidence | `RGZQQUVIRIntegral`, `RGZQQScalelessLedger`, `RGZQQPVUVIRLedger`, `RGZQQMixedUVIRLedger`, `RGZQQWeightedUVIRLedger`, `RGZQQMassLimitLedger`, `RGZQQRawMassPoleAudit`, `RGZQQMassRegionMatch`, `RGZQQCompleteMassLedger` |
| Protected integrand comparisons | `RGZQQGaugeEarlyPrepare`, `RGZQQGaugeEarlyCheck` |
| Continue and inspect | `RGZSave`, `RGZRestore`, `RGZQQRepairIncomplete`, `RGZResult`, `RGZStatus`, `RGZReport`, `RGZShow`, `RGZMonitor` |
| Optional PT candidate research | `RGZPinchKernels`, `RGZPTStart`, `RGZRunPart`, `RGZPinchResult`, `RGZGaugeCheck`, `RGZExportResidual`, `RGZMassLimit` |

The PT commands operate on a separate candidate calculation in `Kernel/Pinch/`. They share projection and integral tools with conventional QQ, and are not prerequisites for it. Detailed audit ordering is in [Diagnostics](<../Examples/RGZWorkflow — Diagnostics.nb>).

The working source is scoped to zero-temperature scattering. The unrelated thermal, standalone vacuum, historical probe-sign migration and generic stage registry interfaces were archived locally; the published v1.0.0 archive is unchanged.
