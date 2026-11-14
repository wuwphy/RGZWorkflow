# Gauge-coefficient reduction: ownership and call flow

This local refactor concerns the two coefficient algorithms and their direct runtime support. Public commands, pole/branch conventions and mathematical reduction formulas are retained.

| Responsibility | Owner |
| --- | --- |
| PV preparation, exact radical coefficients, ordinary batches and sparse merges | `Algebra/GaugeCoefficients.wl` |
| Fixed factored denominator, polynomial accumulation, exact factor cancellation and branch comparison | `Algebra/CommonDenominator.wl` |
| Progress, diagnostics, coefficient checkpoints and the two explicitly named batch-cache formats | `Internal/GaugeRuntime.wl` |

Before refactoring, `CommonDenominator.wl` clears and replaces `RGZGaugeBatchCacheRead/Write`, changing the ordinary algorithm's IO implementation merely by loading the fixed-denominator module. The final design gives ordinary batches their own text-cache calls and fixed-denominator work its own WXF-cache calls. Existing ordinary WXF batches written through the former override remain readable after payload validation; no existing files are converted or deleted.

## Original definitions

| Function | Original file | Original line | Final owner |
| --- | --- | ---: | --- |
| `RGZGaugeCoefficientResultQ` | `Algebra/GaugeCoefficients.wl` | 27 | `Algebra/GaugeCoefficients.wl` |
| `RGZGaugeRowQ` | `Algebra/GaugeCoefficients.wl` | 32 | `Algebra/GaugeCoefficients.wl` |
| `RGZGaugeCoefficientContext` | `Algebra/GaugeCoefficients.wl` | 36 | `Algebra/GaugeCoefficients.wl` |
| `RGZGaugeBatchPreparedQ` | `Algebra/GaugeCoefficients.wl` | 41 | `Algebra/GaugeCoefficients.wl` |
| `RGZGaugeStep` | `Algebra/GaugeCoefficients.wl` | 52 | `Internal/GaugeRuntime.wl` |
| `RGZGaugePrepare` | `Algebra/GaugeCoefficients.wl` | 64 | `Algebra/GaugeCoefficients.wl` |
| `RGZGaugeState` | `Algebra/GaugeCoefficients.wl` | 102 | `Algebra/GaugeCoefficients.wl` |
| `RGZGaugePoleSquares` | `Algebra/GaugeCoefficients.wl` | 112 | `Algebra/GaugeCoefficients.wl` |
| `RGZGaugeCoefficient` | `Algebra/GaugeCoefficients.wl` | 120 | `Algebra/GaugeCoefficients.wl` |
| `RGZGaugeCacheDirectory` | `Algebra/GaugeCoefficients.wl` | 171 | `Internal/GaugeRuntime.wl` |
| `RGZGaugeCacheRead` | `Algebra/GaugeCoefficients.wl` | 180 | `Internal/GaugeRuntime.wl` |
| `RGZGaugeCacheWrite` | `Algebra/GaugeCoefficients.wl` | 188 | `Internal/GaugeRuntime.wl` |
| `RGZGaugeProgressSummary` | `Algebra/GaugeCoefficients.wl` | 197 | `Internal/GaugeRuntime.wl` |
| `RGZGaugeStoppedInfo` | `Algebra/GaugeCoefficients.wl` | 203 | `Internal/GaugeRuntime.wl` |
| `RGZGaugeInspectStopped` | `Algebra/GaugeCoefficients.wl` | 210 | `Internal/GaugeRuntime.wl` |
| `RGZGaugeCompare` | `Algebra/GaugeCoefficients.wl` | 247 | `Algebra/GaugeCoefficients.wl` |
| `RGZGaugeSummary` | `Algebra/GaugeCoefficients.wl` | 352 | `Internal/GaugeRuntime.wl` |
| `RGZGaugeShow` | `Algebra/GaugeCoefficients.wl` | 367 | `Internal/GaugeRuntime.wl` |
| `RGZGaugeBatchChunks` | `Algebra/GaugeCoefficients.wl` | 380 | `Algebra/GaugeCoefficients.wl` |
| `RGZGaugeBatchPrepare` | `Algebra/GaugeCoefficients.wl` | 383 | `Algebra/GaugeCoefficients.wl` |
| `RGZGaugeBatchReduce` | `Algebra/GaugeCoefficients.wl` | 415 | `Algebra/GaugeCoefficients.wl` |
| `RGZGaugeBatchCacheRead` | `Algebra/GaugeCoefficients.wl` | 434 | `Internal/GaugeRuntime.wl` |
| `RGZGaugeBatchCacheWrite` | `Algebra/GaugeCoefficients.wl` | 443 | `Internal/GaugeRuntime.wl` |
| `RGZGaugeResumeStopped` | `Algebra/GaugeCoefficients.wl` | 458 | `Algebra/GaugeCoefficients.wl` |
| `RGZGaugeBatchSummary` | `Algebra/GaugeCoefficients.wl` | 636 | `Internal/GaugeRuntime.wl` |
| `RGZGaugeBatchRootReduce` | `Algebra/GaugeCoefficients.wl` | 651 | `Algebra/GaugeCoefficients.wl` |
| `RGZGaugeInspectBatch` | `Algebra/GaugeCoefficients.wl` | 657 | `Internal/GaugeRuntime.wl` |
| `RGZGaugeSparseGroup` | `Algebra/GaugeCoefficients.wl` | 678 | `Algebra/GaugeCoefficients.wl` |
| `RGZGaugeSparseFrom` | `Algebra/GaugeCoefficients.wl` | 682 | `Algebra/GaugeCoefficients.wl` |
| `RGZGaugeSparseTimes` | `Algebra/GaugeCoefficients.wl` | 697 | `Algebra/GaugeCoefficients.wl` |
| `RGZGaugeSparsePower` | `Algebra/GaugeCoefficients.wl` | 701 | `Algebra/GaugeCoefficients.wl` |
| `RGZGaugeSparseReduce` | `Algebra/GaugeCoefficients.wl` | 706 | `Algebra/GaugeCoefficients.wl` |
| `RGZGaugeSparseTo` | `Algebra/GaugeCoefficients.wl` | 718 | `Algebra/GaugeCoefficients.wl` |
| `RGZGaugeSparseTidy` | `Algebra/GaugeCoefficients.wl` | 723 | `Algebra/GaugeCoefficients.wl` |
| `RGZGaugeMergeContext` | `Algebra/GaugeCoefficients.wl` | 737 | `Algebra/GaugeCoefficients.wl` |
| `RGZGaugeSparseMergeCore` | `Algebra/GaugeCoefficients.wl` | 743 | `Algebra/GaugeCoefficients.wl` |
| `RGZGaugeBatchMerge` | `Algebra/GaugeCoefficients.wl` | 797 | `Algebra/GaugeCoefficients.wl` |
| `RGZGaugeTestStoppedMerge` | `Algebra/GaugeCoefficients.wl` | 810 | `Algebra/GaugeCoefficients.wl` |
| `RGZGaugeSparseSummary` | `Algebra/GaugeCoefficients.wl` | 838 | `Internal/GaugeRuntime.wl` |
| `RGZGaugeFixedCoordinates` | `Algebra/CommonDenominator.wl` | 24 | `Algebra/CommonDenominator.wl` |
| `RGZGaugeFixedPrepare` | `Algebra/CommonDenominator.wl` | 75 | `Algebra/CommonDenominator.wl` |
| `RGZGaugeFixedFinish` | `Algebra/CommonDenominator.wl` | 103 | `Algebra/CommonDenominator.wl` |
| `RGZGaugeFixedDenResume` | `Algebra/CommonDenominator.wl` | 129 | `Algebra/CommonDenominator.wl` |
| `RGZGaugeFixedSummary` | `Algebra/CommonDenominator.wl` | 232 | `Internal/GaugeRuntime.wl` |
| `RGZGaugeFixedBranch` | `Algebra/CommonDenominator.wl` | 251 | `Algebra/CommonDenominator.wl` |
| `RGZGaugeFixedPair` | `Algebra/CommonDenominator.wl` | 284 | `Algebra/CommonDenominator.wl` |
| `RGZGaugeFixedCompare` | `Algebra/CommonDenominator.wl` | 349 | `Algebra/CommonDenominator.wl` |
| `RGZGaugeFixedSetCache` | `Algebra/CommonDenominator.wl` | 454 | `Internal/GaugeRuntime.wl` |
| `RGZGaugeFixedPreparedQ` | `Algebra/CommonDenominator.wl` | 462 | `Algebra/CommonDenominator.wl` |
| `RGZGaugeFixedValidPacketQ` | `Algebra/CommonDenominator.wl` | 466 | `Internal/GaugeRuntime.wl` |
| `RGZGaugeFixedCacheRead` | `Algebra/CommonDenominator.wl` | 472 | `Internal/GaugeRuntime.wl` |
| `RGZGaugeFixedCacheFailure` | `Algebra/CommonDenominator.wl` | 489 | `Internal/GaugeRuntime.wl` |
| `RGZGaugeFixedCacheWrite` | `Algebra/CommonDenominator.wl` | 499 | `Internal/GaugeRuntime.wl` |
| `RGZGaugeBatchCacheRead` | `Algebra/CommonDenominator.wl` | 540 | `Internal/GaugeRuntime.wl` |
| `RGZGaugeBatchCacheWrite` | `Algebra/CommonDenominator.wl` | 541 | `Internal/GaugeRuntime.wl` |
| `RGZGaugeFixedSaveProgress` | `Algebra/CommonDenominator.wl` | 544 | `Internal/GaugeRuntime.wl` |
| `RGZGaugeFixedCacheInfo` | `Algebra/CommonDenominator.wl` | 559 | `Internal/GaugeRuntime.wl` |
| `RGZGaugeFixedTryFactor` | `Algebra/CommonDenominator.wl` | 576 | `Algebra/CommonDenominator.wl` |
| `RGZGaugeFixedFactorReduce` | `Algebra/CommonDenominator.wl` | 594 | `Algebra/CommonDenominator.wl` |
| `RGZGaugeFixedFinalInfo` | `Algebra/CommonDenominator.wl` | 645 | `Internal/GaugeRuntime.wl` |

## Algorithm relationship

```text
RGZGaugePrepare: shared physical PV keys and two short-label coefficients
    |
    +-- RGZGaugeCompare
    |     -> RGZGaugeCoefficient: direct exact radical reduction
    |     -> if stopped: RGZGaugeResumeStopped
    |          -> rational batches -> sparse pairwise merging
    |
    +-- RGZGaugeFixedCompare
          -> RGZGaugeFixedPair
               -> direct small-coefficient check when applicable
               -> otherwise reduce the two branches separately
                    -> squared-pole coordinates
                    -> one factored denominator
                    -> polynomial numerator groups
                    -> exact factor division
               -> compare after the reference-gauge substitution
```

The two methods are existing mathematical strategies. There is no new general algorithm-selection framework. The fixed method calls the ordinary exact coefficient check for its existing small-expression path; the ordinary method does not require the fixed-denominator module. Both ultimately record a coefficient residual and its actual zero/nonzero status.

## Explicit IO and loading

- Ordinary batching calls `RGZGaugeBatchCacheRead/Write`: it writes the existing `GaugeFiniteScalar/3` text packet and validates the ordinary batch/preparation shape.
- Fixed-denominator preparation, branch work and factor cancellation call `RGZGaugeFixedCacheRead/Write` directly: they keep the existing WXF packet, integrity hash and configured IO limits.
- Both cache implementations have one definition in `Internal/GaugeRuntime.wl`. Loading or reloading either algebra module never clears them.
- Ordinary reads can recover validated WXF scalar/preparation records written when the former fixed-module override was active. Existing files are read in place; no bulk cache migration occurs.
- Full coefficient rows still use the existing shared `RGZGaugeCacheRead/Write`, with their existing coefficient-context checks.

After the common loop tools and `Algebra/MixedProjection.wl`, load `Internal/GaugeRuntime.wl`, then `Algebra/GaugeCoefficients.wl`. Load `Algebra/CommonDenominator.wl` only for that method. The existing PT loader declares this order explicitly. Neither algebra file loads a sibling source file or replaces the other method's definitions.

## Scope and local verification

The mathematical definitions, public names and pole/branch prescriptions are retained. The source comparison finds 58 function symbols in both versions; after normalizing the now-explicit fixed-cache call names, 56 are unchanged. The two remaining changes are precisely the removal of the second batch-IO definitions and the reader for prior WXF ordinary batches.

Before/after isolated native checks compare ordinary independent/dependent/cancelling expressions, an ordinary stopped batch, a forced fixed-denominator residual and repeated cached results. They are local algebra checks, not evidence that a complete physical amplitude is gauge independent. The original graph/PV caches and the published archive are untouched.
