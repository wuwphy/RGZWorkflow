# Runnable examples

Start with the [quark-bubble walkthrough](../Documentation/QuarkBubbleWalkthrough.md) and its [runnable script](QuarkBubble.wls) for one field pair and sector. It creates temporary work and exposes every intermediate mathematical object.

Read [the calculation](../Documentation/Calculation.md) for the physical terms, conventions and source-reading order.

- [Start Here](<RGZWorkflow — Start Here.nb>): native export; fresh-kernel tree checks; mixed matrices; bare amplitude; pure-loop form factors; an optional current-model symbolic gauge difference. Sections 1-5 are the main bare-amplitude calculation.
- [Diagnostics](<RGZWorkflow — Diagnostics.nb>): scoped pole/MSbar, identity and progress diagnostics. No blanket verified-S-matrix claim.


Both notebooks contain input cells only. Repeated project loading marks the required session boundary, not permission to load FeynRules and FeynCalc together. Absolute local paths and old saved output are removed. Computation may take substantial time; timeouts preserve compatible completed checkpoints. See [installation](../Documentation/Installation.md) and [scope](../Documentation/ModelAndScope.md).

The [ghost and mixed walkthrough](../Documentation/GhostAndMixedExamples.md) accompanies [GhostBubble.wls](GhostBubble.wls) and [MixedTadpole.wls](MixedTadpole.wls). These add independent U-ghost, auxiliary-index, non-diagonal and pole-integral checks to the quark example.

[PT and Ward Benchmarks](<RGZWorkflow — PT Ward Benchmark.nb>) is a native notebook for the current PT/background-Ward research. Open it in Mathematica, start a fresh kernel, and evaluate the input cells in order.

It requires only Wolfram Language and this package: no FeynRules, FeynCalc, production `Work/`, or local research archive. It recomputes the package's explicit tree Schur benchmark, one-loop stationary hard-leg identities, the source derivative order, closed massless-sector finite coefficients, and two independent complex-pole triangle parameter integrals. Larger mixed-loop numbers are explicitly labelled `ReferenceSnapshot` and are **not** recomputed or certified by this notebook. The mixed three-point/PT correspondence remains unfinished. Results are saved in an isolated temporary work directory, with the WXF path and check summary displayed in the final section.

The release notebooks use `Work/`. Version 1.1.2 verifies and, when needed, corrects the known FeynCalc colour-contraction rule during `RGZLoadFeynCalc[]`. Use an empty release Work directory and do not copy caches or sessions from earlier versions. See [the correction](../Documentation/ColourCorrection.md).
