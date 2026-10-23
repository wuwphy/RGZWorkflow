# Model and verification scope

The default model is the inherited local RGZ implementation with constraint ghosts eta/etabar, two dynamical quark flavours, and the finite Stueckelberg interaction expansion encoded in `Models/RGZ_W2.6.fr`. The FR, mod and gen files form a single model; do not mix companions from different revisions. Default `QuarkMasses -> {0,0}` retains the supported massless external process.

## Current model

- Metric: Minkowski (+---); keep symbolic D during tensor/Dirac reduction.
- Quark minimal coupling: D_mu = partial_mu - i gs A_mu^a T^a; the ordered vertex convention is +i gs gamma_mu T^a.
- Ordinary FP ghosts, the two auxiliary ghost families, and the constraint ghost are distinct. Eta is already enumerated from the base model; never add an extra eta self-energy in postprocessing.
- Closed-quark enumeration is Nf=2. Quarks are not external-only probes in this model.
- `m`, `mu`, `mG` are RGZ parameters; `M` is the Stueckelberg infrared regulator and is not the renormalization scale.
- The transverse denominator is `(s-m^2)(s-mu^2)+mG^4`. Complex-pole branches, coincident poles and joint mass limits require the explicit prescriptions of the algorithms; a numeric point is not a general proof.

## Propagator encoding

Internal V/S carrier segments provide metric/identity factors, with one complete covariance insertion per physical internal line. U ghost and default F quark lines already contain their propagators; they must not receive a second copy. Split carrier segments do not represent extra physical propagators. Native FeynArts diagram symmetry and closed-fermion signs are retained.

The `69` propagator report covers 64 original bosonic matrix entries and five original FP/connection entries. It does not by itself check every eta, auxiliary-ghost or quark propagator. Full vertex comparison covers 48 interaction vertices; permutation comparison covers 702 labelled orders. `FA=FR` is consistency between implementations; only explicitly labelled three-way checks include a separate handwritten expression.

## Amplitude and limits

The supported external process is distinct-flavour, massless qq' -> qq', with tree, mixed self energy, both AVs, both boxes, both full NAVs and external residues. Pure `LoopOrder -> 1` form factors exclude the tree amplitude. An exact color-spin reconstruction does not prove minimality or independence of the basis.

`QuarkMasses -> {mq1,mq2}` controls base-model/closed-bubble masses and cache fingerprints. Massive external kinematics, residues and mass counterterms are not implemented; QQ start calls reject nonzero masses. Default-model MSbar operations are scoped to their stated Landau/M=0 prescription, with current model content and provenance required.

Gauge independence, UV renormalization and IR finiteness are separate claims. A zero pinch difference is not an independent functional ST proof. `RGZQQSMatrix[]` remains fail-closed when full proofs are missing. No new full updated-model gauge or finite-amplitude result is certified by this development release.

## Experimental extension

`Models/Experimental/FermionicHorizon` contains a separate two-flavour auxiliary-spinor extension based on arXiv:2107.08569v2. It has a finite four-particle FA expansion and its own covariance-insertion topology. Its normalization note distinguishes Eqs.(28)/(45) and Eq.(174). It is not enabled by the default model or loop enumeration; no release claim relies on its S-matrix or renormalization.
