# Main workflow API

Complete usage messages are available through `?RGZFunctionName`. The source snapshot retains compatible diagnostic APIs; the paper workflow uses the following groups.

| Purpose | Entry points |
| --- | --- |
| Initialize and dependencies | `RGZInitialize`, `RGZConfiguration`, `RGZLoadFeynRules`, `RGZLoadFeynCalc`, `RGZLoadFeynHelpers` |
| Tree/model checks | `RGZTreeBenchmark`, `RGZExtractTree`, `RGZExportVertices`, `RGZExtraTreeAudit`, `RGZOneLoopCoverage`, `RGZCheckPropagators`, `RGZCheckVertices`, `RGZCheckOrders`, `RGZCheckExtraPropagators`, `RGZCheckOneLoopCoverage` |
| Mixed matrix | `RGZMixedStart`, `RGZMixedComputeAll`, `RGZMixedResult` |
| Quark amplitude | `RGZQQStart`, `RGZQQPrepareSelf`, `RGZQQRunPart`, `RGZQQAmplitude`, `RGZQQFormFactors` |
| General gauge | `RGZQQGaugeStart`, `RGZQQGaugePrepareSelf`, `RGZQQGaugeEarlyPrepare`, `RGZQQGaugeEarlyCheck`, `RGZQQGaugeRestore` |
| Subtraction | `RGZQQUVIR`, `RGZQQScalelessLedger`, `RGZQQPVUVIRLedger`, `RGZQQMSbarCounterterms`, `RGZQQMSbar` |
| Mass and graph evidence | `RGZQQMixedUVIRLedger`, `RGZQQMassLimitLedger`, `RGZQQMassRegionMatch`, `RGZQQRawMassPoleAudit`, `RGZQQWeightedUVIRLedger`, `RGZQQCompleteMassLedger` |
| Certification | `RGZQQFiniteAudit`, `RGZQQCertification`, `RGZQQSMatrix` |
| Scalar result | `RGZQQScalarFormFactors`, `RGZQQScalarPoleAudit`, `RGZQQScalarBasisAudit`, `RGZQQScalarFiniteAudit`, `RGZQQScalarFiniteSimplify`, `RGZQQScalarColorSimplify`, `RGZQQScalarFactorRows` |
| Results and continuation | `RGZResult`, `RGZReport`, `RGZShow`, `RGZStatus`, `RGZMonitor`, `RGZSave`, `RGZRestore` |
| Reference table | `RGZPaperResult` |

Use the notebooks for option values, dependency order and failure guards. See [reproduction](Reproducibility.md) and [renormalization](QuarkRenormalization.md).
