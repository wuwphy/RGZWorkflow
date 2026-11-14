# Mixed self-energy: function ownership and dependencies

This is the first deliverable of the local refactor. The inventory below describes the source before movement; line numbers refer to the retained `source-before.zip`. Public functions, field ordering, symmetry factors, loop measure and mathematical formulas are unchanged by the planned relocation.

## Final ownership

| Responsibility | Owner | Direct dependencies |
| --- | --- | --- |
| Mixed-field graph generation, per-sector orchestration and matrix assembly | `OneLoop/MixedSelfEnergy.wl` | Shared projection and integral functions; task/runtime functions |
| Index sums, color tensors and T/L projection | `Algebra/MixedProjection.wl` | AA loop conventions and FeynCalc; mixed field metadata at call time |
| Pole splitting and exact reconstruction | `Algebra/MixedPoles.wl` | AA pole definitions and progress callbacks |
| Standard integration, PV reduction and the sole integral dispatcher | `Algebra/MixedIntegrals.wl` | Native TID/PaVeReduce, runtime IO; optional explicit accelerated route |
| Scalar integral formulas and accelerated integral route | `Algebra/TwoPointIntegrals.wl` | Shared integral/runtime definitions; no redefinition of the dispatcher |
| Task records, checkpoint IO and progress/display | `Internal/MixedRuntime.wl` | Model/configuration and graph-stage functions at call time |

`loadLoopEngine[]` declares the complete load sequence. No calculation file loads another source file as a side effect. Some definitions refer to later modules, but calls occur only after the declared loader has completed.

## Existing repeated definitions

`RGZMXIntegralUnit` is defined in both the original mixed backend and `Algebra/TwoPointIntegrals.wl`. The latter clears and overwrites it, so loading selects the implementation. The refactored code retains distinct `RGZMXIntegralUnitStandard` and `RGZMXIntegralUnitAccelerated` functions and one explicit `RGZMXIntegralUnit` dispatcher. Neither route module replaces that dispatcher. Other names with multiple patterns in the same file (field-name conversion, basis components and message levels) are intentional overloads, not cross-file replacements.

## Original function inventory

| Function | Original line | Category | Final owner | Also defined elsewhere |
| --- | ---: | --- | --- | --- |
| `RGZMXSetDisplay` | 55 | Task and cache | `Internal/MixedRuntime.wl` | None |
| `RGZMXReport` | 58 | Task and cache | `Internal/MixedRuntime.wl` | None |
| `RGZMXProgressUpdate` | 61 | Task and cache | `Internal/MixedRuntime.wl` | None |
| `RGZMXProgressPanel` | 64 | Task and cache | `Internal/MixedRuntime.wl` | None |
| `RGZMXMonitor` | 94 | Task and cache | `Internal/MixedRuntime.wl` | None |
| `RGZMXInitStep` | 100 | Task and cache | `Internal/MixedRuntime.wl` | None |
| `RGZMXObjects` | 114 | Algebra | `Algebra/MixedProjection.wl` | None |
| `RGZMXTerms` | 116 | Algebra | `Algebra/MixedProjection.wl` | None |
| `RGZMXExternalSumQ` | 121 | Algebra | `Algebra/MixedProjection.wl` | None |
| `RGZMXIndexOccurrences` | 124 | Algebra | `Algebra/MixedProjection.wl` | None |
| `RGZMXIndexKind` | 132 | Algebra | `Algebra/MixedProjection.wl` | None |
| `RGZMXSpectatorIndexQ` | 145 | Algebra | `Algebra/MixedProjection.wl` | None |
| `RGZMXSpectatorDeltaQ` | 152 | Algebra | `Algebra/MixedProjection.wl` | None |
| `RGZMXSumDimension` | 156 | Algebra | `Algebra/MixedProjection.wl` | None |
| `RGZMXResolveSpectatorSums` | 161 | Algebra | `Algebra/MixedProjection.wl` | None |
| `RGZMXFourColourQ` | 202 | Algebra | `Algebra/MixedProjection.wl` | None |
| `RGZMXColourTensorQ` | 207 | Algebra | `Algebra/MixedProjection.wl` | None |
| `RGZMXBoundColourCount` | 215 | Algebra | `Algebra/MixedProjection.wl` | None |
| `RGZMXAdjointDimension` | 229 | Algebra | `Algebra/MixedProjection.wl` | None |
| `RGZMXNormalizeColourSums` | 245 | Algebra | `Algebra/MixedProjection.wl` | None |
| `RGZMXAuditSums` | 302 | Algebra | `Algebra/MixedProjection.wl` | None |
| `RGZMXFCOptions` | 348 | Graph generation | `OneLoop/MixedSelfEnergy.wl` | None |
| `RGZMXPrepareFA` | 368 | Graph generation | `OneLoop/MixedSelfEnergy.wl` | None |
| `RGZMXConvertOne` | 396 | Graph generation | `OneLoop/MixedSelfEnergy.wl` | None |
| `RGZMXConvert` | 416 | Graph generation | `OneLoop/MixedSelfEnergy.wl` | None |
| `RGZMXValidValueQ` | 431 | Task and cache | `Internal/MixedRuntime.wl` | None |
| `RGZMXStageNeeds` | 434 | Task and cache | `Internal/MixedRuntime.wl` | None |
| `RGZMXStageValueQ` | 437 | Task and cache | `Internal/MixedRuntime.wl` | None |
| `RGZMXStageContext` | 444 | Task and cache | `Internal/MixedRuntime.wl` | None |
| `RGZMXConvertedQ` | 446 | Task and cache | `Internal/MixedRuntime.wl` | None |
| `RGZMXMigrateRow` | 448 | Task and cache | `Internal/MixedRuntime.wl` | None |
| `RGZMXWrite` | 468 | Task and cache | `Internal/MixedRuntime.wl` | None |
| `RGZMXField` | 482 | Graph generation | `OneLoop/MixedSelfEnergy.wl` | None |
| `RGZMXAntiField` | 483 | Graph generation | `OneLoop/MixedSelfEnergy.wl` | None |
| `RGZMXVectorQ` | 484 | Graph generation | `OneLoop/MixedSelfEnergy.wl` | None |
| `RGZMXAuxQ` | 485 | Graph generation | `OneLoop/MixedSelfEnergy.wl` | None |
| `RGZMXLabel` | 486 | Graph generation | `OneLoop/MixedSelfEnergy.wl` | None |
| `RGZMXPairID` | 488 | Graph generation | `OneLoop/MixedSelfEnergy.wl` | None |
| `RGZMXLorentzNames` | 494 | Graph generation | `OneLoop/MixedSelfEnergy.wl` | None |
| `RGZMXUnfixedKinematicsQ` | 496 | Graph generation | `OneLoop/MixedSelfEnergy.wl` | None |
| `RGZMXPrepareIndices` | 502 | Graph generation | `Algebra/MixedProjection.wl` | None |
| `RGZMXLegacyAA` | 513 | Task and cache | `Internal/MixedRuntime.wl` | None |
| `RGZMXModelFingerprint` | 535 | Graph generation | `Internal/MixedRuntime.wl` | None |
| `RGZMXStart` | 539 | Graph generation | `Internal/MixedRuntime.wl` | None |
| `RGZMXResume` | 634 | Graph generation | `Internal/MixedRuntime.wl` | None |
| `RGZMXColourSign` | 664 | Algebra | `Algebra/MixedProjection.wl` | None |
| `RGZMXColourQuartet` | 669 | Algebra | `Algebra/MixedProjection.wl` | None |
| `RGZMXColourReduce` | 709 | Algebra | `Algebra/MixedProjection.wl` | None |
| `RGZMXProject` | 720 | Algebra | `Algebra/MixedProjection.wl` | None |
| `RGZMXIntegralUnits` | 746 | Integral reduction | `Algebra/MixedIntegrals.wl` | None |
| `RGZMXIntegralContext` | 757 | Integral reduction | `Algebra/MixedIntegrals.wl` | None |
| `RGZMXIntegralUnit` | 764 | Integral reduction | `Algebra/MixedIntegrals.wl` | `Algebra/TwoPointIntegrals.wl` |
| `RGZMXIntegrate` | 801 | Integral reduction | `Algebra/MixedIntegrals.wl` | None |
| `RGZMXPVObjectQ` | 831 | Integral reduction | `Algebra/MixedIntegrals.wl` | None |
| `RGZMXPVScalarQ` | 835 | Integral reduction | `Algebra/MixedIntegrals.wl` | None |
| `RGZMXPVUnit` | 839 | Integral reduction | `Algebra/MixedIntegrals.wl` | None |
| `RGZMXScalarReduce` | 871 | Integral reduction | `Algebra/MixedIntegrals.wl` | None |
| `RGZMXSplitStep` | 910 | Algebra | `Internal/MixedRuntime.wl` | None |
| `RGZMixSplitDiagnostics` | 917 | Algebra | `Internal/MixedRuntime.wl` | None |
| `RGZMXSplitFactor` | 924 | Algebra | `Algebra/MixedPoles.wl` | None |
| `RGZMXSplitTerm` | 981 | Algebra | `Algebra/MixedPoles.wl` | None |
| `RGZMXSplit` | 1024 | Algebra | `Algebra/MixedPoles.wl` | None |
| `RGZMXRun` | 1042 | Task and cache | `Internal/MixedRuntime.wl` | None |
| `RGZMXRunAll` | 1123 | Task and cache | `Internal/MixedRuntime.wl` | None |
| `RGZMXStatus` | 1129 | Task and cache | `Internal/MixedRuntime.wl` | None |
| `RGZMXTotal` | 1136 | Matrix assembly | `OneLoop/MixedSelfEnergy.wl` | None |
| `RGZMXShow` | 1155 | Task and cache | `Internal/MixedRuntime.wl` | None |
| `RGZMixBasis` | 1187 | Matrix assembly | `OneLoop/MixedSelfEnergy.wl` | None |
| `RGZMixSetDisplay` | 1190 | Task and cache | `Internal/MixedRuntime.wl` | None |
| `RGZMixMonitor` | 1192 | Task and cache | `Internal/MixedRuntime.wl` | None |
| `RGZMixValidatePair` | 1193 | Graph generation | `OneLoop/MixedSelfEnergy.wl` | None |
| `RGZMixPrepare` | 1196 | Task and cache | `Internal/MixedRuntime.wl` | None |
| `RGZMixStart` | 1219 | Task and cache | `Internal/MixedRuntime.wl` | None |
| `RGZMixRecordPath` | 1224 | Task and cache | `Internal/MixedRuntime.wl` | None |
| `RGZMixStore` | 1227 | Task and cache | `Internal/MixedRuntime.wl` | None |
| `RGZMixRead` | 1237 | Task and cache | `Internal/MixedRuntime.wl` | None |
| `RGZMixStartPair` | 1245 | Graph generation | `OneLoop/MixedSelfEnergy.wl` | None |
| `RGZMixRun` | 1253 | Matrix assembly | `Internal/MixedRuntime.wl` | None |
| `RGZMixShowGraph` | 1254 | Matrix assembly | `Internal/MixedRuntime.wl` | None |
| `RGZMixComputePair` | 1257 | Matrix assembly | `OneLoop/MixedSelfEnergy.wl` | None |
| `RGZMixComputeAll` | 1296 | Matrix assembly | `OneLoop/MixedSelfEnergy.wl` | None |
| `RGZMixStatus` | 1310 | Task and cache | `Internal/MixedRuntime.wl` | None |
| `RGZMixResultStep` | 1328 | Task and cache | `Internal/MixedRuntime.wl` | None |
| `RGZMixReadPair` | 1335 | Matrix assembly | `OneLoop/MixedSelfEnergy.wl` | None |
| `RGZMixTensor` | 1346 | Matrix assembly | `OneLoop/MixedSelfEnergy.wl` | None |
| `RGZMixColourFreeQ` | 1360 | Algebra | `Algebra/MixedProjection.wl` | None |
| `RGZMixColourPairs` | 1362 | Algebra | `Algebra/MixedProjection.wl` | None |
| `RGZMixAdjointColour` | 1384 | Algebra | `Algebra/MixedProjection.wl` | None |
| `RGZMixAdjoint` | 1407 | Algebra | `Algebra/MixedProjection.wl` | None |
| `RGZMixProjectedPair` | 1424 | Matrix assembly | `Internal/MixedRuntime.wl` | None |
| `RGZMixMatrix` | 1446 | Matrix assembly | `OneLoop/MixedSelfEnergy.wl` | None |
| `RGZMixResult` | 1464 | Matrix assembly | `OneLoop/MixedSelfEnergy.wl` | None |
| `RGZMixResultReport` | 1492 | Task and cache | `Internal/MixedRuntime.wl` | None |
| `RGZMixCompactData` | 1509 | Task and cache | `Internal/MixedRuntime.wl` | None |
| `RGZMixShow` | 1523 | Task and cache | `Internal/MixedRuntime.wl` | None |
| `RGZMixAssembleInsertion` | 1542 | Matrix assembly | `OneLoop/MixedSelfEnergy.wl` | None |

## Main flow after the refactor

```text
RGZMixComputePair
  -> RGZMXStart                 task preparation / cache resume
       -> RGZMXGenerateGraphs   mixed-field graph generation
  -> RGZMXRunAll                explicit graph stages
       -> conversion -> projection -> split -> integrate -> scalar reduction
  -> RGZMXTotal                 sum the sector
  -> RGZMixStore                record the completed sector

RGZMixResult
  -> RGZMixMatrix
       -> RGZMixReadPair        sum completed sectors
       -> RGZMixProjectedPair   reuse or compute the normalized adjoint projection
```

`RGZMXStart` keeps its existing signature but its persistence belongs to `Internal/MixedRuntime.wl`. The physical route/field selection is in `RGZMXGenerateGraphs`. Similarly, `RGZMixProjectedPair` retains its cache wrapper and delegates the unchanged projection formula to `RGZMixProjectPair`. The high-level sector loop and matrix assembly remain readable in `OneLoop/MixedSelfEnergy.wl`.

## Integral routes and loading

```text
RGZMXIntegralUnit
  Enabled -> False : RGZMXIntegralUnitStandard -> native TID
  Enabled -> True  : RGZMXIntegralUnitAccelerated
                      -> exact scalar identities, where supported
                      -> native TID fallback
```

The existing `RGZMixSetAcceleration["Enabled" -> False|True]` selects the route. Standard units keep the caller's time limit; accelerated units keep their existing per-kernel cap and scalar-cache upgrade behavior. Reloading a route module does not overwrite `RGZMXIntegralUnit`. Calling the loader again preserves explicitly selected settings.

`loadLoopEngine[]` loads, in order:

1. `Algebra/LoopPrimitives.wl`: shared loop conventions, poles and native tensor reduction.
2. `OneLoop/Topologies.wl`: shared two-point insertion topologies.
3. `Internal/MixedRuntime.wl`: task records, IO, stage execution and progress.
4. `Algebra/MixedProjection.wl`: index/color audits and projectors.
5. `Algebra/MixedPoles.wl`: exact routed pole splitting.
6. `Algebra/MixedIntegrals.wl`: standard integral/PV units and sole dispatcher.
7. `Algebra/TwoPointIntegrals.wl`: independent accelerated unit and scalar formulas.
8. `OneLoop/MixedSelfEnergy.wl`: mixed graphs, sector orchestration and matrix assembly.

The modules define functions without invoking a graph calculation. Forward references are resolved when a calculation is called after the loader completes. `Get` inside cache readers reads checkpoint data; no calculation module uses it to load a sibling source file.

## Behavior comparison

The comparison uses newly generated `A-A / QuarkBubble` graphs in isolated temporary work, once per route, before and after the refactor. Both calculations contain two quark graphs. For each route, every saved projection, T/L integral and scalar result, and the sector totals compare exactly with `SameQ`. Resuming the graph job succeeds; completed scalar stages are skipped; the completed sector is reused.

This local comparison does not claim full-matrix or full-amplitude recertification. Original production caches and published artifacts are retained. Existing checks cover both routes, scaleless policy, nested stopped values and current-source cache provenance; additional regressions check dispatcher stability on module reload and preservation of explicit route selection.

The standalone AA driver is now loaded only for its explicit graph calculation or QCD check. Shared loop primitives and topologies retain their original definitions; mixed/QQ setup does not define `RGZRunAA`.
