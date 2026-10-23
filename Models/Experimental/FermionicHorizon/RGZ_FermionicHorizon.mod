(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Independent fermionic-horizon model. No production workflow is modified.
   FULL covariance insertions, not inverse kernels, occupy the two-point nodes.
   DO NOT insert the physical Q-lambda mixing a second time. *)
(* RGZ-BASE-CONTENT {"Schema":"RGZModelContent/1","JacobianGhostIncluded":true,"ClosedQuarkFlavors":2,"GhostClasses":[1,2,3,4],"ScalarGhostClasses":[1,4],"QuarkClasses":[1,2]} *)
(* Parent RGZ covariances plus full fermionic-horizon covariance blocks. *)
FR$ModelInformation = {
  ModelName -> "RGZ Fermionic Horizon",
  Authors -> {"Wan Wu"},
  Version -> "0.1-FH",
  Date -> "23. 10. 2026",
  Institutions -> {"Tsinghua University"},
  Description -> "Independent two-flavor fermionic-horizon RGZ; finite field expansion"
};

FR$ClassesTranslation = {};
FR$InteractionOrderPerturbativeExpansion = {};
FR$GoldstoneList = {};

IndexRange[Index[Global`Gluon]] =
  NoUnfold[Range[8]];

IndexRange[Index[Gribov]] =
  NoUnfold[Range[8]];

IndexRange[Index[Generation]] =
  Range[3];

IndexRange[Index[Global`Colour]] =
  NoUnfold[Range[3]];

IndexRange[Index[Global`Lorentz]] =
  Range[4];

IndexRange[Index[RGZWorkflow`Engine`FHSpin]]=NoUnfold[Range[4]];
M$ClassesDescription = {
 F[3] == {ClassName -> RGZWorkflow`Engine`LamF1, SelfConjugate -> False,
  Indices -> {Index[Global`Gluon],Index[Global`Colour]},Mass -> RGZWorkflow`Engine`wF1,
  QuantumNumbers -> {QFlavor1->1},PropagatorLabel -> "lamF1",PropagatorType -> Straight,PropagatorArrow -> Forward},
 S[4] == {ClassName -> RGZWorkflow`Engine`BosF1, SelfConjugate -> False,
  Indices -> {Index[RGZWorkflow`Engine`FHSpin],Index[Global`Gluon],Index[Global`Colour]},
  Mass -> RGZWorkflow`Engine`wF1,QuantumNumbers -> {QFlavor1->1},
  PropagatorLabel -> "bosF1",PropagatorType -> ScalarDash,PropagatorArrow -> Forward},
 F[4] == {ClassName -> RGZWorkflow`Engine`LamF2, SelfConjugate -> False,
  Indices -> {Index[Global`Gluon],Index[Global`Colour]},Mass -> RGZWorkflow`Engine`wF2,
  QuantumNumbers -> {QFlavor2->1},PropagatorLabel -> "lamF2",PropagatorType -> Straight,PropagatorArrow -> Forward},
 S[5] == {ClassName -> RGZWorkflow`Engine`BosF2, SelfConjugate -> False,
  Indices -> {Index[RGZWorkflow`Engine`FHSpin],Index[Global`Gluon],Index[Global`Colour]},
  Mass -> RGZWorkflow`Engine`wF2,QuantumNumbers -> {QFlavor2->1},
  PropagatorLabel -> "bosF2",PropagatorType -> ScalarDash,PropagatorArrow -> Forward},


  (* Two distinct Dirac quarks with optional masses; both are dynamical. *)
  F[1] == {ClassName -> Q1, SelfConjugate -> False,
    Indices -> {Index[Global`Colour]}, Mass -> RGZWorkflow`Engine`mQ1, Width -> 0,
    QuantumNumbers -> {QFlavor1 -> 1}, ParticleName -> "q1",
    AntiParticleName -> "q1~", PDG -> 1, PropagatorLabel -> "q1",
    PropagatorType -> Straight, PropagatorArrow -> Forward},
  F[2] == {ClassName -> Q2, SelfConjugate -> False,
    Indices -> {Index[Global`Colour]}, Mass -> RGZWorkflow`Engine`mQ2, Width -> 0,
    QuantumNumbers -> {QFlavor2 -> 1}, ParticleName -> "q2",
    AntiParticleName -> "q2~", PDG -> 2, PropagatorLabel -> "q2",
    PropagatorType -> Straight, PropagatorArrow -> Forward},
  (* BRST-singlet adjoint scalar ghosts for det[-partial.D(Ah)]. *)
  U[4] == {ClassName -> eta, SelfConjugate -> False,
    Indices -> {Index[Global`Gluon]}, Ghost -> A,
    QuantumNumbers -> {GhostNumber -> 1}, Mass -> 0, Width -> 0,
    ParticleName -> "eta", AntiParticleName -> "eta~", PDG -> 9000008,
    PropagatorLabel -> "eta", PropagatorType -> GhostDash,
    PropagatorArrow -> Forward},

  V[1] == {
    ClassName -> A,
    SelfConjugate -> True,
    Indices -> {Index[Global`Gluon]},
    Mass -> 0,
    PropagatorLabel -> "A",
    PropagatorType -> Cycles,
    PropagatorArrow -> None
  },

  V[2] == {
    ClassName -> \[Phi],
    SelfConjugate -> False,
    Indices -> {Index[Global`Gluon], Index[Gribov]},
    Mass -> 1,
    PropagatorLabel -> "\[Phi]",
    PropagatorType -> Sine,
    PropagatorArrow -> None
  },

  V[3] == {
    ClassName -> \[Beta],
    SelfConjugate -> False,
    Indices -> {Index[Global`Gluon], Index[Gribov]},
    Mass -> 1,
    PropagatorLabel -> "\[Beta]",
    PropagatorType -> Sine,
    PropagatorArrow -> None
  },

  U[1] == {
    ClassName -> c,
    SelfConjugate -> False,
    Indices -> {Index[Global`Gluon]},
    QuantumNumbers -> {GhostNumber},
    Mass -> 0,
    PropagatorLabel -> "c",
    PropagatorType -> GhostDash,
    PropagatorArrow -> Forward
  },

  U[2] == {
    ClassName -> \[Omega],
    SelfConjugate -> False,
    Indices -> {Index[Global`Gluon], Index[Gribov]},
    Mass -> RGZWorkflow`Engine`RGZAuxiliaryMass,
    Width -> 0,
    ParticleName -> "\[Omega]",
    AntiParticleName -> "\[Omega]~",
    PropagatorLabel -> "\[Omega]",
    PropagatorType -> GhostDash,
    PropagatorArrow -> Forward
  },

  U[3] == {
    ClassName -> \[Zeta],
    SelfConjugate -> False,
    Indices -> {Index[Global`Gluon], Index[Gribov]},
    Mass -> RGZWorkflow`Engine`RGZAuxiliaryMass,
    Width -> 0,
    ParticleName -> "\[Zeta]",
    AntiParticleName -> "\[Zeta]~",
    PropagatorLabel -> "\[Zeta]",
    PropagatorType -> GhostDash,
    PropagatorArrow -> Forward
  },

  S[1] == {
    ClassName -> \[Xi],
    SelfConjugate -> True,
    Indices -> {Index[Global`Gluon]},
    Mass -> 0,
    Width -> 0,
    ParticleName -> "\[Xi]",
    PropagatorLabel -> "\[Xi]",
    PropagatorType -> Sine,
    PropagatorArrow -> None
  },

  S[2] == {
    ClassName -> \[Tau],
    SelfConjugate -> True,
    Indices -> {Index[Global`Gluon]},
    Mass -> 0,
    Width -> 0,
    ParticleName -> "\[Tau]",
    PropagatorLabel -> "\[Tau]",
    PropagatorType -> Sine,
    PropagatorArrow -> None
  },

  S[3] == {
    ClassName -> b,
    SelfConjugate -> True,
    Indices -> {Index[Global`Gluon]},
    Mass -> 0,
    Width -> 0,
    ParticleName -> "b",
    PropagatorLabel -> "b",
    PropagatorType -> Sine,
    PropagatorArrow -> None
  }

};

GaugeXi[V[1]] = GaugeXi[G];
GaugeXi[U[1]] = GaugeXi[G];

M$CouplingMatrices = {

  C[-F[1,{c1}], F[1,{c2}], V[1,{a}]] ==
    I Global`FAGS Global`FASUNT[a,c1,c2] {{1,0},{1,0}},
  C[-F[2,{c1}], F[2,{c2}], V[1,{a}]] ==
    I Global`FAGS Global`FASUNT[a,c1,c2] {{1,0},{1,0}},
  C[-U[4,{g1}], U[4,{g2}], V[1,{g3}]] ==
    Global`FAGS Global`FASUNF[g1,g2,g3] {{1,0},{0,0},{0,0},{0,0}},
  C[-U[4,{g1}], U[4,{g2}], S[1,{g3}]] ==
    I Global`FAGS Global`FASUNF[g1,g2,g3] {{0,0},{0,0},{1,0},{0,0}},
  C[-U[4,{g1}], U[4,{g2}], S[1,{g3}], S[1,{g4}]] ==
    -I Global`FAGS^2/2 Global`FASUNF[g1,g2,g3,g4]
      {{0,0},{0,0},{0,0},{0,0},{1,0},{-1,0},{0,0},{0,0}},
  C[-U[4,{g1}], U[4,{g2}], V[1,{g3}], S[1,{g4}]] ==
    -Global`FAGS^2 Global`FASUNF[g1,g2,g3,g4] {{0,0},{0,0},{1,0},{0,0}},

  C[V[1, {g1}], V[1, {g2}]] ==
    {{0, -I*IndexDelta[g1, g2]}, {0, -I*IndexDelta[g1, g2]}, {0, 0}, {0, 0}, {0, 0}},

  C[V[1, {g1}], V[2, {g2, g3}]] ==
    {{0, 0}, {0, 0}, {0, 1*I*mG^2*Global`FASUNF[g1, g2, g3]/(2*Sqrt[N])}, {0, 0}, {0, 0}},

  C[V[2, {g1, g2}], V[1, {g3}]] ==
    {{0, 0}, {0, 0}, {0, 1*I*mG^2*Global`FASUNF[g3, g1, g2]/(2*Sqrt[N])}, {0, 0}, {0, 0}},

  C[V[1, {g1}], -V[2, {g2, g3}]] ==
    {{0, 0}, {0, 0}, {0, 1*I*mG^2*Global`FASUNF[g1, g2, g3]/(2*Sqrt[N])}, {0, 0}, {0, 0}},

  C[-V[2, {g1, g2}], V[1, {g3}]] ==
    {{0, 0}, {0, 0}, {0, 1*I*mG^2*Global`FASUNF[g3, g1, g2]/(2*Sqrt[N])}, {0, 0}, {0, 0}},

  C[V[1, {g1}], V[3, {g2, g3}]] ==
    {{0, 0}, {0, 0}, {0, -1*I*mG^2*Global`FASUNF[g1, g2, g3]/(2*Sqrt[N])}, {0, 0}, {0, 0}},

  C[V[3, {g1, g2}], V[1, {g3}]] ==
    {{0, 0}, {0, 0}, {0, -1*I*mG^2*Global`FASUNF[g3, g1, g2]/(2*Sqrt[N])}, {0, 0}, {0, 0}},

  C[V[1, {g1}], -V[3, {g2, g3}]] ==
    {{0, 0}, {0, 0}, {0, -1*I*mG^2*Global`FASUNF[g1, g2, g3]/(2*Sqrt[N])}, {0, 0}, {0, 0}},

  C[-V[3, {g1, g2}], V[1, {g3}]] ==
    {{0, 0}, {0, 0}, {0, -1*I*mG^2*Global`FASUNF[g3, g1, g2]/(2*Sqrt[N])}, {0, 0}, {0, 0}},

  C[V[2, {g1, g2}], V[2, {g3, g4}]] ==
    {{0, 0}, {0, 0}, {0, 0}, {0, -1*I*mG^4*Global`FASUNF[g1, g2, g3, g4]/(4*N)}, {0, 0}},

  C[V[2, {g1, g2}], -V[2, {g3, g4}]] ==
    {{0, 0}, {0, 0}, {0, 0}, {0, -1*I*mG^4*Global`FASUNF[g1, g2, g3, g4]/(4*N)}, {0, I*IndexDelta[g1, g3]*IndexDelta[g2, g4]}},

  C[V[2, {g1, g2}], V[3, {g3, g4}]] ==
    {{0, 0}, {0, 0}, {0, 0}, {0, 1*I*mG^4*Global`FASUNF[g1, g2, g3, g4]/(4*N)}, {0, 0}},

  C[V[2, {g1, g2}], -V[3, {g3, g4}]] ==
    {{0, 0}, {0, 0}, {0, 0}, {0, 1*I*mG^4*Global`FASUNF[g1, g2, g3, g4]/(4*N)}, {0, 0}},

  C[-V[2, {g1, g2}], V[2, {g3, g4}]] ==
    {{0, 0}, {0, 0}, {0, 0}, {0, -1*I*mG^4*Global`FASUNF[g1, g2, g3, g4]/(4*N)}, {0, I*IndexDelta[g1, g3]*IndexDelta[g2, g4]}},

  C[-V[2, {g1, g2}], -V[2, {g3, g4}]] ==
    {{0, 0}, {0, 0}, {0, 0}, {0, -1*I*mG^4*Global`FASUNF[g1, g2, g3, g4]/(4*N)}, {0, 0}},

  C[-V[2, {g1, g2}], V[3, {g3, g4}]] ==
    {{0, 0}, {0, 0}, {0, 0}, {0, 1*I*mG^4*Global`FASUNF[g1, g2, g3, g4]/(4*N)}, {0, 0}},

  C[-V[2, {g1, g2}], -V[3, {g3, g4}]] ==
    {{0, 0}, {0, 0}, {0, 0}, {0, 1*I*mG^4*Global`FASUNF[g1, g2, g3, g4]/(4*N)}, {0, 0}},

  C[V[3, {g1, g2}], V[2, {g3, g4}]] ==
    {{0, 0}, {0, 0}, {0, 0}, {0, 1*I*mG^4*Global`FASUNF[g1, g2, g3, g4]/(4*N)}, {0, 0}},

  C[V[3, {g1, g2}], -V[2, {g3, g4}]] ==
    {{0, 0}, {0, 0}, {0, 0}, {0, 1*I*mG^4*Global`FASUNF[g1, g2, g3, g4]/(4*N)}, {0, 0}},

  C[V[3, {g1, g2}], V[3, {g3, g4}]] ==
    {{0, 0}, {0, 0}, {0, 0}, {0, -1*I*mG^4*Global`FASUNF[g1, g2, g3, g4]/(4*N)}, {0, 0}},

  C[V[3, {g1, g2}], -V[3, {g3, g4}]] ==
    {{0, 0}, {0, 0}, {0, 0}, {0, -1*I*mG^4*Global`FASUNF[g1, g2, g3, g4]/(4*N)}, {0, I*IndexDelta[g1, g3]*IndexDelta[g2, g4]}},

  C[-V[3, {g1, g2}], V[2, {g3, g4}]] ==
    {{0, 0}, {0, 0}, {0, 0}, {0, 1*I*mG^4*Global`FASUNF[g1, g2, g3, g4]/(4*N)}, {0, 0}},

  C[-V[3, {g1, g2}], -V[2, {g3, g4}]] ==
    {{0, 0}, {0, 0}, {0, 0}, {0, 1*I*mG^4*Global`FASUNF[g1, g2, g3, g4]/(4*N)}, {0, 0}},

  C[-V[3, {g1, g2}], V[3, {g3, g4}]] ==
    {{0, 0}, {0, 0}, {0, 0}, {0, -1*I*mG^4*Global`FASUNF[g1, g2, g3, g4]/(4*N)}, {0, I*IndexDelta[g1, g3]*IndexDelta[g2, g4]}},

  C[-V[3, {g1, g2}], -V[3, {g3, g4}]] ==
    {{0, 0}, {0, 0}, {0, 0}, {0, -1*I*mG^4*Global`FASUNF[g1, g2, g3, g4]/(4*N)}, {0, 0}},

  C[V[1, {g1}], S[3, {g2}]] ==
    {{0, -IndexDelta[g1, g2]}, {0, 0}, {0, 0}, {0, 0}},

  C[S[3, {g1}], V[1, {g2}]] ==
    {{0, -IndexDelta[g1, g2]}, {0, 0}, {0, 0}, {0, 0}},

  C[V[1, {g1}], S[1, {g2}]] ==
    {{0, 0}, {0, -\[Alpha]*IndexDelta[g1, g2]}, {0, 0}, {0, 0}},

  C[S[1, {g1}], V[1, {g2}]] ==
    {{0, 0}, {0, -\[Alpha]*IndexDelta[g1, g2]}, {0, 0}, {0, 0}},

  C[V[1, {g1}], S[2, {g2}]] ==
    {{0, 0}, {0, 0}, {0, -\[Alpha]*M^4*IndexDelta[g1, g2]}, {0, 0}},

  C[S[2, {g1}], V[1, {g2}]] ==
    {{0, 0}, {0, 0}, {0, -\[Alpha]*M^4*IndexDelta[g1, g2]}, {0, 0}},

  C[S[2, {g1}], V[2, {g2, g3}]] ==
    {{0, 0}, {0, 0}, {0, 0}, {0, 1*mG^2*Global`FASUNF[g1, g2, g3]/(2*Sqrt[N])}},

  C[V[2, {g1, g2}], S[2, {g3}]] ==
    {{0, 0}, {0, 0}, {0, 0}, {0, 1*mG^2*Global`FASUNF[g3, g1, g2]/(2*Sqrt[N])}},

  C[S[2, {g1}], -V[2, {g2, g3}]] ==
    {{0, 0}, {0, 0}, {0, 0}, {0, 1*mG^2*Global`FASUNF[g1, g2, g3]/(2*Sqrt[N])}},

  C[-V[2, {g1, g2}], S[2, {g3}]] ==
    {{0, 0}, {0, 0}, {0, 0}, {0, 1*mG^2*Global`FASUNF[g3, g1, g2]/(2*Sqrt[N])}},

  C[S[2, {g1}], V[3, {g2, g3}]] ==
    {{0, 0}, {0, 0}, {0, 0}, {0, -1*mG^2*Global`FASUNF[g1, g2, g3]/(2*Sqrt[N])}},

  C[V[3, {g1, g2}], S[2, {g3}]] ==
    {{0, 0}, {0, 0}, {0, 0}, {0, -1*mG^2*Global`FASUNF[g3, g1, g2]/(2*Sqrt[N])}},

  C[S[2, {g1}], -V[3, {g2, g3}]] ==
    {{0, 0}, {0, 0}, {0, 0}, {0, -1*mG^2*Global`FASUNF[g1, g2, g3]/(2*Sqrt[N])}},

  C[-V[3, {g1, g2}], S[2, {g3}]] ==
    {{0, 0}, {0, 0}, {0, 0}, {0, -1*mG^2*Global`FASUNF[g3, g1, g2]/(2*Sqrt[N])}},

  C[S[3, {g1}], S[3, {g2}]] ==
    {{0, I*M^4*IndexDelta[g1, g2]}, {0, 0}, {0, 0}, {0, 0}},

  C[S[3, {g1}], S[1, {g2}]] ==
    {{0, 0}, {0, -I*IndexDelta[g1, g2]}, {0, 0}, {0, 0}},

  C[S[1, {g1}], S[3, {g2}]] ==
    {{0, 0}, {0, -I*IndexDelta[g1, g2]}, {0, 0}, {0, 0}},

  C[S[3, {g1}], S[2, {g2}]] ==
    {{0, -I*M^4*IndexDelta[g1, g2]}, {0, 0}, {0, 0}, {0, 0}},

  C[S[2, {g1}], S[3, {g2}]] ==
    {{0, -I*M^4*IndexDelta[g1, g2]}, {0, 0}, {0, 0}, {0, 0}},

  C[S[1, {g1}], S[1, {g2}]] ==
    {{0, -I*\[Alpha]*IndexDelta[g1, g2]}, {0, 0}, {0, 0}, {0, 0}},

  C[S[1, {g1}], S[2, {g2}]] ==
    {{0, 0}, {0, I*IndexDelta[g1, g2]}, {0, 0}, {0, 0}},

  C[S[2, {g1}], S[1, {g2}]] ==
    {{0, 0}, {0, I*IndexDelta[g1, g2]}, {0, 0}, {0, 0}},

  C[S[2, {g1}], S[2, {g2}]] ==
    {{0, I*M^4*IndexDelta[g1, g2]}, {0, 0}, {0, -I*m^2*IndexDelta[g1, g2]}, {0, I*mG^4*IndexDelta[g1, g2]}},

  C[U[1, {g1}], -U[1, {g2}]] ==
    {{0, 0}, {0, I*IndexDelta[g1, g2]}},

  C[-U[1, {g1}], S[3, {g2}]] ==
    {{0, I*\[Rho]*IndexDelta[g1, g2]}, {0, 0}, {0, 0}, {0, 0}},

  C[S[3, {g1}], -U[1, {g2}]] ==
    {{0, I*\[Rho]*IndexDelta[g1, g2]}, {0, 0}, {0, 0}, {0, 0}},

  C[-U[1, {g1}], S[2, {g2}]] ==
    {{0, -I*\[Rho]*IndexDelta[g1, g2]}, {0, 0}, {0, 0}, {0, 0}},

  C[S[2, {g1}], -U[1, {g2}]] ==
    {{0, -I*\[Rho]*IndexDelta[g1, g2]}, {0, 0}, {0, 0}, {0, 0}},

  C[-U[1, {g1}], S[1, {g2}]] ==
    {{0, 0}, {0, 0}, {0, I*\[Rho]*\[Alpha]*IndexDelta[g1, g2]}, {0, 0}},

  C[S[1, {g1}], -U[1, {g2}]] ==
    {{0, 0}, {0, 0}, {0, I*\[Rho]*\[Alpha]*IndexDelta[g1, g2]}, {0, 0}},

  C[-U[1, {g1}], V[1, {g2}]] ==
    {{0, 0}, {0, 0}, {0, \[Rho]*\[Alpha]*IndexDelta[g1, g2]}, {0, 0}},

  C[V[1, {g1}], -U[1, {g2}]] ==
    {{0, 0}, {0, 0}, {0, \[Rho]*\[Alpha]*IndexDelta[g1, g2]}, {0, 0}},

C[-U[1, {g1}], U[1, {g2}], V[1, {g3}]] == Global`FAGS*Global`FASUNF[g1, g2, g3] * {{1, dZgs1 + dZGG1/2}, {0, 0}, {0, 0}, {0, 0}},

C[V[1, {g1}], V[1, {g2}], V[1, {g3}]] == -Global`FAGS*Global`FASUNF[g1, g2, g3] * {{1, dZgs1 + 3*dZGG1/2}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}},

C[V[1, {g1}], V[1, {g2}], V[1, {g3}], V[1, {g4}]] == -I*Global`FAGS^2 * {( Global`FASUNF[g1, g3, g2, g4] - Global`FASUNF[g1, g4, g3, g2] ) * {1, 2*(dZgs1 + dZGG1)}, ( Global`FASUNF[g1, g2, g3, g4] + Global`FASUNF[g1, g4, g3, g2] ) * {1, 2*(dZgs1 + dZGG1)}, ( -Global`FASUNF[g1, g2, g3, g4] - Global`FASUNF[g1, g3, g2, g4] ) * {1, 2*(dZgs1 + dZGG1)}},

C[V[1, {g1}], V[3, {g2, g3}], -V[3, {g4, g5}]] == Global`FAGS*Global`FASUNF[g1, g2, g4]*IndexDelta[g3, g5] * {{0, 0}, {1, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}},

C[S[1, {g1}], V[3, {g2, g3}], -V[3, {g4, g5}]] == I*Global`FAGS*Global`FASUNF[g1, g2, g4]*IndexDelta[g3, g5] * {{0, 0}, {0, 0}, {1, 0}},

C[S[1, {g1}], S[1, {g2}], V[3, {g3, g4}], -V[3, {g5, g6}]] == I*Global`FAGS^2/2 * Global`FASUNF[g5, g3, g1, g2]*IndexDelta[g4, g6] * {{0, 0}, {0, 0}, {0, 0}, {1, 0}, {-1, 0}},

C[V[1, {g1}], V[3, {g2, g3}], -V[3, {g4, g5}], S[1, {g6}]] == Global`FAGS^2*Global`FASUNF[g1, g6, g4, g2]*IndexDelta[g3, g5] * {{1, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}},

C[V[1, {g1}], V[2, {g2, g3}], -V[2, {g4, g5}]] == Global`FAGS*Global`FASUNF[g1, g2, g4]*IndexDelta[g3, g5] * {{0, 0}, {1, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}},

C[S[1, {g1}], V[2, {g2, g3}], -V[2, {g4, g5}]] == I*Global`FAGS*Global`FASUNF[g1, g2, g4]*IndexDelta[g3, g5] * {{0, 0}, {0, 0}, {1, 0}},

C[S[1, {g1}], S[1, {g2}], V[2, {g3, g4}], -V[2, {g5, g6}]] == I*Global`FAGS^2/2 * Global`FASUNF[g5, g3, g1, g2]*IndexDelta[g4, g6] * {{0, 0}, {0, 0}, {0, 0}, {1, 0}, {-1, 0}},

C[V[1, {g1}], V[2, {g2, g3}], -V[2, {g4, g5}], S[1, {g6}]] == Global`FAGS^2*Global`FASUNF[g1, g6, g4, g2]*IndexDelta[g3, g5] * {{1, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}},

C[-U[3, {g1, g2}], U[3, {g3, g4}], V[1, {g5}]] == -Global`FAGS*Global`FASUNF[g5, g1, g3]*IndexDelta[g2, g4] * {{0, 0}, {0, 0}, {1, 0}, {0, 0}},

C[-U[3, {g1, g2}], U[3, {g3, g4}], S[1, {g5}]] == -I*Global`FAGS*Global`FASUNF[g5, g1, g3]*IndexDelta[g2, g4] * {{1, 0}, {0, 0}, {0, 0}, {0, 0}},

C[-U[3, {g1, g2}], U[3, {g3, g4}], S[1, {g5}], S[1, {g6}]] == I*Global`FAGS^2/2 * Global`FASUNF[g1, g3, g5, g6]*IndexDelta[g2, g4] * {{1, 0}, {-1, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}},

C[-U[3, {g1, g2}], U[3, {g3, g4}], V[1, {g5}], S[1, {g6}]] == Global`FAGS^2*Global`FASUNF[g1, g3, g5, g6]*IndexDelta[g2, g4] * {{1, 0}, {0, 0}, {0, 0}, {0, 0}},

C[-U[2, {g1, g2}], U[2, {g3, g4}], V[1, {g5}]] == -Global`FAGS*Global`FASUNF[g5, g1, g3]*IndexDelta[g2, g4] * {{0, 0}, {0, 0}, {1, 0}, {0, 0}},

C[-U[2, {g1, g2}], U[2, {g3, g4}], S[1, {g5}]] == -I*Global`FAGS*Global`FASUNF[g5, g1, g3]*IndexDelta[g2, g4] * {{1, 0}, {0, 0}, {0, 0}, {0, 0}},

C[-U[2, {g1, g2}], U[2, {g3, g4}], S[1, {g5}], S[1, {g6}]] == I*Global`FAGS^2/2 * Global`FASUNF[g1, g3, g5, g6]*IndexDelta[g2, g4] * {{1, 0}, {-1, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}},

C[-U[2, {g1, g2}], U[2, {g3, g4}], V[1, {g5}], S[1, {g6}]] == Global`FAGS^2*Global`FASUNF[g1, g3, g5, g6]*IndexDelta[g2, g4] * {{1, 0}, {0, 0}, {0, 0}, {0, 0}},

C[S[1, {g1}], S[1, {g2}], S[2, {g3}]] == -I*Global`FAGS*Global`FASUNF[g1, g3, g2]/2 * {{0, 0}, {1, 0}, {-1, 0}},

C[S[2, {g1}], V[1, {g2}], S[1, {g3}]] == Global`FAGS*Global`FASUNF[g1, g2, g3] * {{1, 0}, {0, 0}},

C[S[2, {g1}], S[1, {g2}], V[1, {g3}], S[1, {g4}]] == -Global`FAGS^2/2 * ( Global`FASUNF[g1, g2, g4, g3] + Global`FASUNF[g1, g4, g2, g3] ) * {{1, 0}, {0, 0}, {0, 0}},

C[S[1, {g1}], S[2, {g2}], V[1, {g3}], S[1, {g4}]] == -Global`FAGS^2/2 * ( Global`FASUNF[g2, g1, g4, g3] + Global`FASUNF[g2, g4, g1, g3] ) * {{0, 0}, {1, 0}, {0, 0}},

C[S[1, {g1}], S[1, {g2}], V[1, {g3}], S[2, {g4}]] == -Global`FAGS^2/2 * ( Global`FASUNF[g4, g1, g2, g3] + Global`FASUNF[g4, g2, g1, g3] ) * {{0, 0}, {0, 0}, {1, 0}},

C[S[2, {g1}], V[1, {g2}], S[1, {g3}], S[1, {g4}]] == -Global`FAGS^2/2 * ( Global`FASUNF[g1, g3, g4, g2] + Global`FASUNF[g1, g4, g3, g2] ) * {{1, 0}, {0, 0}, {0, 0}},

C[S[1, {g1}], V[1, {g2}], S[2, {g3}], S[1, {g4}]] == -Global`FAGS^2/2 * ( Global`FASUNF[g3, g1, g4, g2] + Global`FASUNF[g3, g4, g1, g2] ) * {{0, 0}, {1, 0}, {0, 0}},

C[S[1, {g1}], V[1, {g2}], S[1, {g3}], S[2, {g4}]] == -Global`FAGS^2/2 * ( Global`FASUNF[g4, g1, g3, g2] + Global`FASUNF[g4, g3, g1, g2] ) * {{0, 0}, {0, 0}, {1, 0}},

C[V[1, {g1}], S[2, {g2}], S[1, {g3}], S[1, {g4}]] == -Global`FAGS^2/2 * ( Global`FASUNF[g2, g3, g4, g1] + Global`FASUNF[g2, g4, g3, g1] ) * {{1, 0}, {0, 0}, {0, 0}},

C[V[1, {g1}], S[1, {g2}], S[2, {g3}], S[1, {g4}]] == -Global`FAGS^2/2 * ( Global`FASUNF[g3, g2, g4, g1] + Global`FASUNF[g3, g4, g2, g1] ) * {{0, 0}, {1, 0}, {0, 0}},

C[V[1, {g1}], S[1, {g2}], S[1, {g3}], S[2, {g4}]] == -Global`FAGS^2/2 * ( Global`FASUNF[g4, g2, g3, g1] + Global`FASUNF[g4, g3, g2, g1] ) * {{0, 0}, {0, 0}, {1, 0}},

C[S[2, {g1}], S[1, {g2}], S[1, {g3}], V[1, {g4}]] == -Global`FAGS^2/2 * ( Global`FASUNF[g1, g2, g3, g4] + Global`FASUNF[g1, g3, g2, g4] ) * {{1, 0}, {0, 0}, {0, 0}},

C[S[1, {g1}], S[2, {g2}], S[1, {g3}], V[1, {g4}]] == -Global`FAGS^2/2 * ( Global`FASUNF[g2, g1, g3, g4] + Global`FASUNF[g2, g3, g1, g4] ) * {{0, 0}, {1, 0}, {0, 0}},

C[S[1, {g1}], S[1, {g2}], S[2, {g3}], V[1, {g4}]] == -Global`FAGS^2/2 * ( Global`FASUNF[g3, g1, g2, g4] + Global`FASUNF[g3, g2, g1, g4] ) * {{0, 0}, {0, 0}, {1, 0}},

C[S[2, {g1}], S[1, {g2}], S[1, {g3}], S[1, {g4}]] == I*Global`FAGS^2/6 * {{ -Global`FASUNF[g2, g3, g1, g4] - Global`FASUNF[g2, g4, g1, g3], 0 }, { Global`FASUNF[g2, g3, g1, g4] + Global`FASUNF[g2, g1, g3, g4], 0 }, { Global`FASUNF[g2, g4, g1, g3] - Global`FASUNF[g2, g1, g3, g4], 0 }, {0, 0}, {0, 0}, {0, 0}},

C[S[1, {g1}], S[1, {g2}], S[1, {g3}], S[2, {g4}]] == I*Global`FAGS^2/6 * {{0, 0}, {0, 0}, { -Global`FASUNF[g1, g2, g4, g3] - Global`FASUNF[g1, g3, g4, g2], 0 }, {0, 0}, { Global`FASUNF[g1, g2, g4, g3] + Global`FASUNF[g1, g4, g2, g3], 0 }, { Global`FASUNF[g1, g3, g4, g2] - Global`FASUNF[g1, g4, g2, g3], 0 }},

C[S[1, {g1}], S[1, {g2}], V[3, {g3, g4}]] == Global`FAGS*mG^2/(4*Sqrt[N]) * Global`FASUNF[g4, g3, g1, g2] * {{1, 0}, {-1, 0}},

C[S[1, {g1}], S[1, {g2}], -V[3, {g3, g4}]] == Global`FAGS*mG^2/(4*Sqrt[N]) * Global`FASUNF[g4, g3, g1, g2] * {{1, 0}, {-1, 0}},

C[V[1, {g1}], S[1, {g2}], V[3, {g3, g4}]] == I*Global`FAGS*mG^2/(2*Sqrt[N]) * Global`FASUNF[g1, g2, g3, g4] * {{1, 0}, {0, 0}, {0, 0}},

C[V[1, {g1}], S[1, {g2}], -V[3, {g3, g4}]] == I*Global`FAGS*mG^2/(2*Sqrt[N]) * Global`FASUNF[g1, g2, g3, g4] * {{1, 0}, {0, 0}, {0, 0}},

C[V[1, {g1}], S[1, {g2}], S[1, {g3}], V[3, {g4, g5}]] == -I*Global`FAGS^2*mG^2/(4*Sqrt[N]) * IndexSum[ Global`FASUNF[g5, g4, g2, aa]*Global`FASUNF[g1, g3, aa] + Global`FASUNF[g5, g4, g3, aa]*Global`FASUNF[g1, g2, aa], {aa, 1, 8} ] * {{1, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}},

C[V[1, {g1}], S[1, {g2}], S[1, {g3}], -V[3, {g4, g5}]] == -I*Global`FAGS^2*mG^2/(4*Sqrt[N]) * IndexSum[ Global`FASUNF[g5, g4, g2, aa]*Global`FASUNF[g1, g3, aa] + Global`FASUNF[g5, g4, g3, aa]*Global`FASUNF[g1, g2, aa], {aa, 1, 8} ] * {{1, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}},

C[S[1, {g1}], S[1, {g2}], S[1, {g3}], V[3, {g4, g5}]] == -Global`FAGS^2*mG^2/(12*Sqrt[N]) * {{ IndexSum[ -Global`FASUNF[g5, g4, g3, aa]*Global`FASUNF[g1, g2, aa] - Global`FASUNF[g5, g4, g2, aa]*Global`FASUNF[g1, g3, aa], {aa, 1, 8} ], 0 }, { IndexSum[ Global`FASUNF[g5, g4, g3, aa]*Global`FASUNF[g1, g2, aa] - Global`FASUNF[g5, g4, g1, aa]*Global`FASUNF[g2, g3, aa], {aa, 1, 8} ], 0 }, { IndexSum[ Global`FASUNF[g5, g4, g2, aa]*Global`FASUNF[g1, g3, aa] + Global`FASUNF[g5, g4, g1, aa]*Global`FASUNF[g2, g3, aa], {aa, 1, 8} ], 0 }},

C[S[1, {g1}], S[1, {g2}], S[1, {g3}], -V[3, {g4, g5}]] == -Global`FAGS^2*mG^2/(12*Sqrt[N]) * {{ IndexSum[ -Global`FASUNF[g5, g4, g3, aa]*Global`FASUNF[g1, g2, aa] - Global`FASUNF[g5, g4, g2, aa]*Global`FASUNF[g1, g3, aa], {aa, 1, 8} ], 0 }, { IndexSum[ Global`FASUNF[g5, g4, g3, aa]*Global`FASUNF[g1, g2, aa] - Global`FASUNF[g5, g4, g1, aa]*Global`FASUNF[g2, g3, aa], {aa, 1, 8} ], 0 }, { IndexSum[ Global`FASUNF[g5, g4, g2, aa]*Global`FASUNF[g1, g3, aa] + Global`FASUNF[g5, g4, g1, aa]*Global`FASUNF[g2, g3, aa], {aa, 1, 8} ], 0 }},

C[S[1, {g1}], S[1, {g2}], V[2, {g3, g4}]] == Global`FAGS*mG^2/(4*Sqrt[N]) * Global`FASUNF[g4, g3, g1, g2] * {{-1, 0}, {1, 0}},

C[S[1, {g1}], S[1, {g2}], -V[2, {g3, g4}]] == Global`FAGS*mG^2/(4*Sqrt[N]) * Global`FASUNF[g4, g3, g1, g2] * {{-1, 0}, {1, 0}},

C[V[1, {g1}], S[1, {g2}], V[2, {g3, g4}]] == -I*Global`FAGS*mG^2/(2*Sqrt[N]) * Global`FASUNF[g1, g2, g3, g4] * {{1, 0}, {0, 0}, {0, 0}},

C[V[1, {g1}], S[1, {g2}], -V[2, {g3, g4}]] == -I*Global`FAGS*mG^2/(2*Sqrt[N]) * Global`FASUNF[g1, g2, g3, g4] * {{1, 0}, {0, 0}, {0, 0}},

C[V[1, {g1}], S[1, {g2}], S[1, {g3}], V[2, {g4, g5}]] == I*Global`FAGS^2*mG^2/(4*Sqrt[N]) * IndexSum[ Global`FASUNF[g5, g4, g2, aa]*Global`FASUNF[g1, g3, aa] + Global`FASUNF[g5, g4, g3, aa]*Global`FASUNF[g1, g2, aa], {aa, 1, 8} ] * {{1, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}},

C[V[1, {g1}], S[1, {g2}], S[1, {g3}], -V[2, {g4, g5}]] == I*Global`FAGS^2*mG^2/(4*Sqrt[N]) * IndexSum[ Global`FASUNF[g5, g4, g2, aa]*Global`FASUNF[g1, g3, aa] + Global`FASUNF[g5, g4, g3, aa]*Global`FASUNF[g1, g2, aa], {aa, 1, 8} ] * {{1, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}},

C[S[1, {g1}], S[1, {g2}], S[1, {g3}], V[2, {g4, g5}]] == Global`FAGS^2*mG^2/(12*Sqrt[N]) * {{ IndexSum[ -Global`FASUNF[g5, g4, g3, aa]*Global`FASUNF[g1, g2, aa] - Global`FASUNF[g5, g4, g2, aa]*Global`FASUNF[g1, g3, aa], {aa, 1, 8} ], 0 }, { IndexSum[ Global`FASUNF[g5, g4, g3, aa]*Global`FASUNF[g1, g2, aa] - Global`FASUNF[g5, g4, g1, aa]*Global`FASUNF[g2, g3, aa], {aa, 1, 8} ], 0 }, { IndexSum[ Global`FASUNF[g5, g4, g2, aa]*Global`FASUNF[g1, g3, aa] + Global`FASUNF[g5, g4, g1, aa]*Global`FASUNF[g2, g3, aa], {aa, 1, 8} ], 0 }},

C[S[1, {g1}], S[1, {g2}], S[1, {g3}], -V[2, {g4, g5}]] == Global`FAGS^2*mG^2/(12*Sqrt[N]) * {{ IndexSum[ -Global`FASUNF[g5, g4, g3, aa]*Global`FASUNF[g1, g2, aa] - Global`FASUNF[g5, g4, g2, aa]*Global`FASUNF[g1, g3, aa], {aa, 1, 8} ], 0 }, { IndexSum[ Global`FASUNF[g5, g4, g3, aa]*Global`FASUNF[g1, g2, aa] - Global`FASUNF[g5, g4, g1, aa]*Global`FASUNF[g2, g3, aa], {aa, 1, 8} ], 0 }, { IndexSum[ Global`FASUNF[g5, g4, g2, aa]*Global`FASUNF[g1, g3, aa] + Global`FASUNF[g5, g4, g1, aa]*Global`FASUNF[g2, g3, aa], {aa, 1, 8} ], 0 }},

C[V[1, {g1}], S[1, {g2}], S[1, {g3}]] == Global`FAGS*m^2/2*Global`FASUNF[g1, g2, g3] * {{-1, 0}, {1, 0}},

C[S[1, {g1}], S[1, {g2}], S[1, {g3}], S[1, {g4}]] == I*Global`FAGS^2*m^2/12 * {{ -Global`FASUNF[g1, g3, g2, g4] - Global`FASUNF[g1, g4, g2, g3], 0 }, { -Global`FASUNF[g1, g2, g3, g4] + Global`FASUNF[g1, g4, g2, g3], 0 }, { Global`FASUNF[g1, g2, g3, g4] + Global`FASUNF[g1, g3, g2, g4], 0 }, { Global`FASUNF[g1, g2, g3, g4] + Global`FASUNF[g1, g3, g2, g4], 0 }, { -Global`FASUNF[g1, g2, g3, g4] + Global`FASUNF[g1, g4, g2, g3], 0 }, { -Global`FASUNF[g1, g3, g2, g4] - Global`FASUNF[g1, g4, g2, g3], 0 }},

C[V[1, {g1}], S[1, {g2}], S[1, {g3}], S[1, {g4}]] == Global`FAGS^2*m^2/6 * {{ Global`FASUNF[g1, g4, g2, g3] + Global`FASUNF[g1, g3, g2, g4], 0 }, { -Global`FASUNF[g1, g4, g2, g3] + Global`FASUNF[g1, g2, g3, g4], 0 }, { -Global`FASUNF[g1, g3, g2, g4] - Global`FASUNF[g1, g2, g3, g4], 0 }}

};

M$LastModelRules = {};

(* Local MSbar counterterms are supplied by RGZQQMSbarCounterterms with
   Nf=2. The old RenConst[dZgs1]=-7 dZGG1/2 guess is not valid for this
   model. These tree-level FA rules do not define an on-shell CT model. *)

RGZFHEmbed[eq_,basis_Association] := Module[{key=RGZFHKey[eq[[1]]],old,new,coeff},
 old=Lookup[basis,key,Missing["NoGenericBasis"]];
 If[!ListQ[old],Return[Failure["FHGenericBasis",<|"Coupling"->eq[[1]]|>]]];
 new=RGZFHCombinedBasis[key];coeff=eq[[2]];
 If[Length[coeff]=!=Length[old],Return[Failure["FHCoefficientCount",<|"Coupling"->eq[[1]]|>]]];
 eq[[1]] == Table[With[{p=FirstPosition[old,b,Missing["Absent"],{1}]},
 If[MissingQ[p],{0,0},coeff[[First[p]]]]],{b,new}]];
RGZFHRawBaseMatrices=M$CouplingMatrices;
RGZFHOriginalMatrices=RGZFHEmbed[#,RGZFHBaseBasis]& /@ M$CouplingMatrices;
RGZFHNewMatrices={

C[ S[1, {e2x1, e2x2, e2x3}] , -S[1, {e3x1, e3x2, e3x3}] , V[1, {e1x2}] ] == {{-(Global`FAGS*Global`FASUNF[e1x2, e2x2, e3x2]*IndexDelta[e2x1, e3x1]*IndexDelta[e2x3, e3x3]), 0}, {0, 0}},

C[ S[1, {e1x1, e1x2, e1x3}] , -S[1, {e2x1, e2x2, e2x3}] , S[3, {e3x1}] ] == {{(-I)*Global`FAGS*Global`FASUNF[e1x2, e2x2, e3x1]*IndexDelta[e1x1, e2x1]*IndexDelta[e1x3, e2x3], 0}, {0, 0}, {0, 0}},

C[ S[2, {e2x1, e2x2, e2x3}] , -S[2, {e3x1, e3x2, e3x3}] , V[1, {e1x2}] ] == {{-(Global`FAGS*Global`FASUNF[e1x2, e2x2, e3x2]*IndexDelta[e2x1, e3x1]*IndexDelta[e2x3, e3x3]), 0}, {0, 0}},

C[ S[2, {e1x1, e1x2, e1x3}] , -S[2, {e2x1, e2x2, e2x3}] , S[3, {e3x1}] ] == {{(-I)*Global`FAGS*Global`FASUNF[e1x2, e2x2, e3x1]*IndexDelta[e1x1, e2x1]*IndexDelta[e1x3, e2x3], 0}, {0, 0}, {0, 0}},

C[ -F[1, {e1x2, e1x3}] , F[1, {e2x2, e2x3}] , V[1, {e3x2}] ] == {{Global`FAGS*Global`FASUNF[e1x2, e2x2, e3x2]*IndexDelta[e1x3, e2x3], 0}, {Global`FAGS*Global`FASUNF[e1x2, e2x2, e3x2]*IndexDelta[e1x3, e2x3], 0}, {0, 0}, {0, 0}},

C[ -F[1, {e1x2, e1x3}] , F[1, {e2x2, e2x3}] , S[3, {e3x1}] ] == {{I*Global`FAGS*Global`FASUNF[e1x2, e2x2, e3x1]*IndexDelta[e1x3, e2x3], 0}, {I*Global`FAGS*Global`FASUNF[e1x2, e2x2, e3x1]*IndexDelta[e1x3, e2x3], 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}},

C[ -F[2, {e1x2, e1x3}] , F[2, {e2x2, e2x3}] , V[1, {e3x2}] ] == {{Global`FAGS*Global`FASUNF[e1x2, e2x2, e3x2]*IndexDelta[e1x3, e2x3], 0}, {Global`FAGS*Global`FASUNF[e1x2, e2x2, e3x2]*IndexDelta[e1x3, e2x3], 0}, {0, 0}, {0, 0}},

C[ -F[2, {e1x2, e1x3}] , F[2, {e2x2, e2x3}] , S[3, {e3x1}] ] == {{I*Global`FAGS*Global`FASUNF[e1x2, e2x2, e3x1]*IndexDelta[e1x3, e2x3], 0}, {I*Global`FAGS*Global`FASUNF[e1x2, e2x2, e3x1]*IndexDelta[e1x3, e2x3], 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}},

C[ -F[1, {e1x2, e1x3}] , F[3, {e2x2}] , S[3, {e3x1}] ] == {{0, 0}, {0, 0}, {Global`FAGS*RGZWorkflow`Engine`hF1*IndexSum[Global`FASUNT[e1x2, e1x3, RGZWorkflow`Engine`Colour$1]*Global`FASUNT[e3x1, RGZWorkflow`Engine`Colour$1, e2x2], {RGZWorkflow`Engine`Colour$1, 1, 3}], 0}, {Global`FAGS*RGZWorkflow`Engine`hF1*IndexSum[Global`FASUNT[e1x2, e1x3, RGZWorkflow`Engine`Colour$1]*Global`FASUNT[e3x1, RGZWorkflow`Engine`Colour$1, e2x2], {RGZWorkflow`Engine`Colour$1, 1, 3}], 0}, {0, 0}, {0, 0}},

C[ -F[3, {e1x2}] , F[1, {e2x2, e2x3}] , S[3, {e3x1}] ] == {{0, 0}, {0, 0}, {-(Global`FAGS*RGZWorkflow`Engine`hF1*IndexSum[Global`FASUNT[e2x2, RGZWorkflow`Engine`Colour$1, e2x3]*Global`FASUNT[e3x1, e1x2, RGZWorkflow`Engine`Colour$1], {RGZWorkflow`Engine`Colour$1, 1, 3}]), 0}, {-(Global`FAGS*RGZWorkflow`Engine`hF1*IndexSum[Global`FASUNT[e2x2, RGZWorkflow`Engine`Colour$1, e2x3]*Global`FASUNT[e3x1, e1x2, RGZWorkflow`Engine`Colour$1], {RGZWorkflow`Engine`Colour$1, 1, 3}]), 0}, {0, 0}, {0, 0}},

C[ -F[2, {e1x2, e1x3}] , F[4, {e2x2}] , S[3, {e3x1}] ] == {{0, 0}, {0, 0}, {Global`FAGS*RGZWorkflow`Engine`hF2*IndexSum[Global`FASUNT[e1x2, e1x3, RGZWorkflow`Engine`Colour$1]*Global`FASUNT[e3x1, RGZWorkflow`Engine`Colour$1, e2x2], {RGZWorkflow`Engine`Colour$1, 1, 3}], 0}, {Global`FAGS*RGZWorkflow`Engine`hF2*IndexSum[Global`FASUNT[e1x2, e1x3, RGZWorkflow`Engine`Colour$1]*Global`FASUNT[e3x1, RGZWorkflow`Engine`Colour$1, e2x2], {RGZWorkflow`Engine`Colour$1, 1, 3}], 0}, {0, 0}, {0, 0}},

C[ -F[4, {e1x2}] , F[2, {e2x2, e2x3}] , S[3, {e3x1}] ] == {{0, 0}, {0, 0}, {-(Global`FAGS*RGZWorkflow`Engine`hF2*IndexSum[Global`FASUNT[e2x2, RGZWorkflow`Engine`Colour$1, e2x3]*Global`FASUNT[e3x1, e1x2, RGZWorkflow`Engine`Colour$1], {RGZWorkflow`Engine`Colour$1, 1, 3}]), 0}, {-(Global`FAGS*RGZWorkflow`Engine`hF2*IndexSum[Global`FASUNT[e2x2, RGZWorkflow`Engine`Colour$1, e2x3]*Global`FASUNT[e3x1, e1x2, RGZWorkflow`Engine`Colour$1], {RGZWorkflow`Engine`Colour$1, 1, 3}]), 0}, {0, 0}, {0, 0}},

C[ -F[1, {e1x2, e1x3}] , F[3, {e2x2}] , S[3, {e3x1}] , S[3, {e4x1}] ] == {{(-1/2*I)*Global`FAGS^2*RGZWorkflow`Engine`hF1*(IndexSum[IndexSum[Global`FASUNT[e1x2, e1x3, RGZWorkflow`Engine`Colour$1]*Global`FASUNT[e4x1, RGZWorkflow`Engine`Colour$1, RGZWorkflow`Engine`Colour$2], {RGZWorkflow`Engine`Colour$1, 1, 3}]*Global`FASUNT[e3x1, RGZWorkflow`Engine`Colour$2, e2x2], {RGZWorkflow`Engine`Colour$2, 1, 3}] + IndexSum[IndexSum[Global`FASUNT[e1x2, e1x3, RGZWorkflow`Engine`Colour$1]*Global`FASUNT[e3x1, RGZWorkflow`Engine`Colour$1, RGZWorkflow`Engine`Colour$2], {RGZWorkflow`Engine`Colour$1, 1, 3}]*Global`FASUNT[e4x1, RGZWorkflow`Engine`Colour$2, e2x2], {RGZWorkflow`Engine`Colour$2, 1, 3}]), 0}, {(-1/2*I)*Global`FAGS^2*RGZWorkflow`Engine`hF1*(IndexSum[IndexSum[Global`FASUNT[e1x2, e1x3, RGZWorkflow`Engine`Colour$1]*Global`FASUNT[e4x1, RGZWorkflow`Engine`Colour$1, RGZWorkflow`Engine`Colour$2], {RGZWorkflow`Engine`Colour$1, 1, 3}]*Global`FASUNT[e3x1, RGZWorkflow`Engine`Colour$2, e2x2], {RGZWorkflow`Engine`Colour$2, 1, 3}] + IndexSum[IndexSum[Global`FASUNT[e1x2, e1x3, RGZWorkflow`Engine`Colour$1]*Global`FASUNT[e3x1, RGZWorkflow`Engine`Colour$1, RGZWorkflow`Engine`Colour$2], {RGZWorkflow`Engine`Colour$1, 1, 3}]*Global`FASUNT[e4x1, RGZWorkflow`Engine`Colour$2, e2x2], {RGZWorkflow`Engine`Colour$2, 1, 3}]), 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}},

C[ -F[3, {e1x2}] , F[1, {e2x2, e2x3}] , S[3, {e3x1}] , S[3, {e4x1}] ] == {{(-1/2*I)*Global`FAGS^2*RGZWorkflow`Engine`hF1*(IndexSum[IndexSum[Global`FASUNT[e2x2, RGZWorkflow`Engine`Colour$1, e2x3]*Global`FASUNT[e4x1, RGZWorkflow`Engine`Colour$2, RGZWorkflow`Engine`Colour$1], {RGZWorkflow`Engine`Colour$1, 1, 3}]*Global`FASUNT[e3x1, e1x2, RGZWorkflow`Engine`Colour$2], {RGZWorkflow`Engine`Colour$2, 1, 3}] + IndexSum[IndexSum[Global`FASUNT[e2x2, RGZWorkflow`Engine`Colour$1, e2x3]*Global`FASUNT[e3x1, RGZWorkflow`Engine`Colour$2, RGZWorkflow`Engine`Colour$1], {RGZWorkflow`Engine`Colour$1, 1, 3}]*Global`FASUNT[e4x1, e1x2, RGZWorkflow`Engine`Colour$2], {RGZWorkflow`Engine`Colour$2, 1, 3}]), 0}, {(-1/2*I)*Global`FAGS^2*RGZWorkflow`Engine`hF1*(IndexSum[IndexSum[Global`FASUNT[e2x2, RGZWorkflow`Engine`Colour$1, e2x3]*Global`FASUNT[e4x1, RGZWorkflow`Engine`Colour$2, RGZWorkflow`Engine`Colour$1], {RGZWorkflow`Engine`Colour$1, 1, 3}]*Global`FASUNT[e3x1, e1x2, RGZWorkflow`Engine`Colour$2], {RGZWorkflow`Engine`Colour$2, 1, 3}] + IndexSum[IndexSum[Global`FASUNT[e2x2, RGZWorkflow`Engine`Colour$1, e2x3]*Global`FASUNT[e3x1, RGZWorkflow`Engine`Colour$2, RGZWorkflow`Engine`Colour$1], {RGZWorkflow`Engine`Colour$1, 1, 3}]*Global`FASUNT[e4x1, e1x2, RGZWorkflow`Engine`Colour$2], {RGZWorkflow`Engine`Colour$2, 1, 3}]), 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}},

C[ -F[2, {e1x2, e1x3}] , F[4, {e2x2}] , S[3, {e3x1}] , S[3, {e4x1}] ] == {{(-1/2*I)*Global`FAGS^2*RGZWorkflow`Engine`hF2*(IndexSum[IndexSum[Global`FASUNT[e1x2, e1x3, RGZWorkflow`Engine`Colour$1]*Global`FASUNT[e4x1, RGZWorkflow`Engine`Colour$1, RGZWorkflow`Engine`Colour$2], {RGZWorkflow`Engine`Colour$1, 1, 3}]*Global`FASUNT[e3x1, RGZWorkflow`Engine`Colour$2, e2x2], {RGZWorkflow`Engine`Colour$2, 1, 3}] + IndexSum[IndexSum[Global`FASUNT[e1x2, e1x3, RGZWorkflow`Engine`Colour$1]*Global`FASUNT[e3x1, RGZWorkflow`Engine`Colour$1, RGZWorkflow`Engine`Colour$2], {RGZWorkflow`Engine`Colour$1, 1, 3}]*Global`FASUNT[e4x1, RGZWorkflow`Engine`Colour$2, e2x2], {RGZWorkflow`Engine`Colour$2, 1, 3}]), 0}, {(-1/2*I)*Global`FAGS^2*RGZWorkflow`Engine`hF2*(IndexSum[IndexSum[Global`FASUNT[e1x2, e1x3, RGZWorkflow`Engine`Colour$1]*Global`FASUNT[e4x1, RGZWorkflow`Engine`Colour$1, RGZWorkflow`Engine`Colour$2], {RGZWorkflow`Engine`Colour$1, 1, 3}]*Global`FASUNT[e3x1, RGZWorkflow`Engine`Colour$2, e2x2], {RGZWorkflow`Engine`Colour$2, 1, 3}] + IndexSum[IndexSum[Global`FASUNT[e1x2, e1x3, RGZWorkflow`Engine`Colour$1]*Global`FASUNT[e3x1, RGZWorkflow`Engine`Colour$1, RGZWorkflow`Engine`Colour$2], {RGZWorkflow`Engine`Colour$1, 1, 3}]*Global`FASUNT[e4x1, RGZWorkflow`Engine`Colour$2, e2x2], {RGZWorkflow`Engine`Colour$2, 1, 3}]), 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}},

C[ -F[4, {e1x2}] , F[2, {e2x2, e2x3}] , S[3, {e3x1}] , S[3, {e4x1}] ] == {{(-1/2*I)*Global`FAGS^2*RGZWorkflow`Engine`hF2*(IndexSum[IndexSum[Global`FASUNT[e2x2, RGZWorkflow`Engine`Colour$1, e2x3]*Global`FASUNT[e4x1, RGZWorkflow`Engine`Colour$2, RGZWorkflow`Engine`Colour$1], {RGZWorkflow`Engine`Colour$1, 1, 3}]*Global`FASUNT[e3x1, e1x2, RGZWorkflow`Engine`Colour$2], {RGZWorkflow`Engine`Colour$2, 1, 3}] + IndexSum[IndexSum[Global`FASUNT[e2x2, RGZWorkflow`Engine`Colour$1, e2x3]*Global`FASUNT[e3x1, RGZWorkflow`Engine`Colour$2, RGZWorkflow`Engine`Colour$1], {RGZWorkflow`Engine`Colour$1, 1, 3}]*Global`FASUNT[e4x1, e1x2, RGZWorkflow`Engine`Colour$2], {RGZWorkflow`Engine`Colour$2, 1, 3}]), 0}, {(-1/2*I)*Global`FAGS^2*RGZWorkflow`Engine`hF2*(IndexSum[IndexSum[Global`FASUNT[e2x2, RGZWorkflow`Engine`Colour$1, e2x3]*Global`FASUNT[e4x1, RGZWorkflow`Engine`Colour$2, RGZWorkflow`Engine`Colour$1], {RGZWorkflow`Engine`Colour$1, 1, 3}]*Global`FASUNT[e3x1, e1x2, RGZWorkflow`Engine`Colour$2], {RGZWorkflow`Engine`Colour$2, 1, 3}] + IndexSum[IndexSum[Global`FASUNT[e2x2, RGZWorkflow`Engine`Colour$1, e2x3]*Global`FASUNT[e3x1, RGZWorkflow`Engine`Colour$2, RGZWorkflow`Engine`Colour$1], {RGZWorkflow`Engine`Colour$1, 1, 3}]*Global`FASUNT[e4x1, e1x2, RGZWorkflow`Engine`Colour$2], {RGZWorkflow`Engine`Colour$2, 1, 3}]), 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}},

C[ -F[1, {e1x2, e1x3}] , F[1, {e2x2, e2x3}] , S[3, {e3x1}] , S[3, {e4x1}] ] == {{0, 0}, {0, 0}, {(-1/2*I)*Global`FAGS^2*IndexDelta[e1x3, e2x3]*IndexSum[Global`FASUNF[e1x2, e2x2, RGZWorkflow`Engine`Gluon$1]*Global`FASUNF[e3x1, e4x1, RGZWorkflow`Engine`Gluon$1], {RGZWorkflow`Engine`Gluon$1, 1, 8}], 0}, {(-1/2*I)*Global`FAGS^2*IndexDelta[e1x3, e2x3]*IndexSum[Global`FASUNF[e1x2, e2x2, RGZWorkflow`Engine`Gluon$1]*Global`FASUNF[e3x1, e4x1, RGZWorkflow`Engine`Gluon$1], {RGZWorkflow`Engine`Gluon$1, 1, 8}], 0}, {(I/2)*Global`FAGS^2*IndexDelta[e1x3, e2x3]*IndexSum[Global`FASUNF[e1x2, e2x2, RGZWorkflow`Engine`Gluon$1]*Global`FASUNF[e3x1, e4x1, RGZWorkflow`Engine`Gluon$1], {RGZWorkflow`Engine`Gluon$1, 1, 8}], 0}, {(I/2)*Global`FAGS^2*IndexDelta[e1x3, e2x3]*IndexSum[Global`FASUNF[e1x2, e2x2, RGZWorkflow`Engine`Gluon$1]*Global`FASUNF[e3x1, e4x1, RGZWorkflow`Engine`Gluon$1], {RGZWorkflow`Engine`Gluon$1, 1, 8}], 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}},

C[ S[1, {e1x1, e1x2, e1x3}] , -S[1, {e2x1, e2x2, e2x3}] , S[3, {e3x1}] , S[3, {e4x1}] ] == {{(I/2)*Global`FAGS^2*IndexDelta[e1x1, e2x1]*IndexDelta[e1x3, e2x3]*IndexSum[Global`FASUNF[e1x2, e2x2, RGZWorkflow`Engine`Gluon$1]*Global`FASUNF[e3x1, e4x1, RGZWorkflow`Engine`Gluon$1], {RGZWorkflow`Engine`Gluon$1, 1, 8}], 0}, {(-1/2*I)*Global`FAGS^2*IndexDelta[e1x1, e2x1]*IndexDelta[e1x3, e2x3]*IndexSum[Global`FASUNF[e1x2, e2x2, RGZWorkflow`Engine`Gluon$1]*Global`FASUNF[e3x1, e4x1, RGZWorkflow`Engine`Gluon$1], {RGZWorkflow`Engine`Gluon$1, 1, 8}], 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}},

C[ -F[2, {e1x2, e1x3}] , F[2, {e2x2, e2x3}] , S[3, {e3x1}] , S[3, {e4x1}] ] == {{0, 0}, {0, 0}, {(-1/2*I)*Global`FAGS^2*IndexDelta[e1x3, e2x3]*IndexSum[Global`FASUNF[e1x2, e2x2, RGZWorkflow`Engine`Gluon$1]*Global`FASUNF[e3x1, e4x1, RGZWorkflow`Engine`Gluon$1], {RGZWorkflow`Engine`Gluon$1, 1, 8}], 0}, {(-1/2*I)*Global`FAGS^2*IndexDelta[e1x3, e2x3]*IndexSum[Global`FASUNF[e1x2, e2x2, RGZWorkflow`Engine`Gluon$1]*Global`FASUNF[e3x1, e4x1, RGZWorkflow`Engine`Gluon$1], {RGZWorkflow`Engine`Gluon$1, 1, 8}], 0}, {(I/2)*Global`FAGS^2*IndexDelta[e1x3, e2x3]*IndexSum[Global`FASUNF[e1x2, e2x2, RGZWorkflow`Engine`Gluon$1]*Global`FASUNF[e3x1, e4x1, RGZWorkflow`Engine`Gluon$1], {RGZWorkflow`Engine`Gluon$1, 1, 8}], 0}, {(I/2)*Global`FAGS^2*IndexDelta[e1x3, e2x3]*IndexSum[Global`FASUNF[e1x2, e2x2, RGZWorkflow`Engine`Gluon$1]*Global`FASUNF[e3x1, e4x1, RGZWorkflow`Engine`Gluon$1], {RGZWorkflow`Engine`Gluon$1, 1, 8}], 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}},

C[ S[2, {e1x1, e1x2, e1x3}] , -S[2, {e2x1, e2x2, e2x3}] , S[3, {e3x1}] , S[3, {e4x1}] ] == {{(I/2)*Global`FAGS^2*IndexDelta[e1x1, e2x1]*IndexDelta[e1x3, e2x3]*IndexSum[Global`FASUNF[e1x2, e2x2, RGZWorkflow`Engine`Gluon$1]*Global`FASUNF[e3x1, e4x1, RGZWorkflow`Engine`Gluon$1], {RGZWorkflow`Engine`Gluon$1, 1, 8}], 0}, {(-1/2*I)*Global`FAGS^2*IndexDelta[e1x1, e2x1]*IndexDelta[e1x3, e2x3]*IndexSum[Global`FASUNF[e1x2, e2x2, RGZWorkflow`Engine`Gluon$1]*Global`FASUNF[e3x1, e4x1, RGZWorkflow`Engine`Gluon$1], {RGZWorkflow`Engine`Gluon$1, 1, 8}], 0}, {0, 0}, {0, 0}, {0, 0}, {0, 0}},

C[ -F[1, {e1x2, e1x3}] , F[1, {e2x2, e2x3}] , S[3, {e4x1}] , V[1, {e3x2}] ] == {{-(Global`FAGS^2*IndexDelta[e1x3, e2x3]*IndexSum[Global`FASUNF[e1x2, e2x2, RGZWorkflow`Engine`Gluon$1]*Global`FASUNF[e3x2, e4x1, RGZWorkflow`Engine`Gluon$1], {RGZWorkflow`Engine`Gluon$1, 1, 8}]), 0}, {-(Global`FAGS^2*IndexDelta[e1x3, e2x3]*IndexSum[Global`FASUNF[e1x2, e2x2, RGZWorkflow`Engine`Gluon$1]*Global`FASUNF[e3x2, e4x1, RGZWorkflow`Engine`Gluon$1], {RGZWorkflow`Engine`Gluon$1, 1, 8}]), 0}, {0, 0}, {0, 0}},

C[ S[1, {e2x1, e2x2, e2x3}] , -S[1, {e3x1, e3x2, e3x3}] , S[3, {e4x1}] , V[1, {e1x2}] ] == {{Global`FAGS^2*IndexDelta[e2x1, e3x1]*IndexDelta[e2x3, e3x3]*IndexSum[Global`FASUNF[e1x2, e4x1, RGZWorkflow`Engine`Gluon$1]*Global`FASUNF[e2x2, e3x2, RGZWorkflow`Engine`Gluon$1], {RGZWorkflow`Engine`Gluon$1, 1, 8}], 0}, {0, 0}, {0, 0}},

C[ -F[2, {e1x2, e1x3}] , F[2, {e2x2, e2x3}] , S[3, {e4x1}] , V[1, {e3x2}] ] == {{-(Global`FAGS^2*IndexDelta[e1x3, e2x3]*IndexSum[Global`FASUNF[e1x2, e2x2, RGZWorkflow`Engine`Gluon$1]*Global`FASUNF[e3x2, e4x1, RGZWorkflow`Engine`Gluon$1], {RGZWorkflow`Engine`Gluon$1, 1, 8}]), 0}, {-(Global`FAGS^2*IndexDelta[e1x3, e2x3]*IndexSum[Global`FASUNF[e1x2, e2x2, RGZWorkflow`Engine`Gluon$1]*Global`FASUNF[e3x2, e4x1, RGZWorkflow`Engine`Gluon$1], {RGZWorkflow`Engine`Gluon$1, 1, 8}]), 0}, {0, 0}, {0, 0}},

C[ S[2, {e2x1, e2x2, e2x3}] , -S[2, {e3x1, e3x2, e3x3}] , S[3, {e4x1}] , V[1, {e1x2}] ] == {{Global`FAGS^2*IndexDelta[e2x1, e3x1]*IndexDelta[e2x3, e3x3]*IndexSum[Global`FASUNF[e1x2, e4x1, RGZWorkflow`Engine`Gluon$1]*Global`FASUNF[e2x2, e3x2, RGZWorkflow`Engine`Gluon$1], {RGZWorkflow`Engine`Gluon$1, 1, 8}], 0}, {0, 0}, {0, 0}}

};
(* Original FeynRules export order -> stable base-model field IDs. *)
RGZFHNewMatrices=RGZFHNewMatrices /. {
 F[j_Integer,r___] :> F[Lookup[<|1->3,2->4,3->1,4->2|>,j,j],r],
 S[j_Integer,r___] :> S[Lookup[<|1->4,2->5,3->1,4->2,5->3|>,j,j],r],
 U[j_Integer,r___] :> U[Lookup[<|1->4,2->1,3->2,4->3|>,j,j],r]};
RGZFHRawNewMatrices=RGZFHNewMatrices;
RGZFHNewMatrices=RGZFHEmbed[#,RGZFHAddedBasis]& /@ RGZFHNewMatrices;
M$CouplingMatrices=Join[RGZFHOriginalMatrices,RGZFHNewMatrices];
(* Full Schur inverse of [slash(k)-mq, h T; h T, -(k^2-w^2)].
   T^a T^a=CF. The unprojected lambda block includes the adjoint-
   fundamental identity; its orthogonal components are NOT discarded. *)
Do[With[{q=n,l=n+2,offset=7(n-1)},
 AppendTo[M$CouplingMatrices,C[-F[q,{ci}],F[q,{cj}]] ==
  Table[{0,If[MemberQ[offset+{1,2},j],IndexDelta[ci,cj],0]},{j,14}]];
 AppendTo[M$CouplingMatrices,C[-F[q,{ci}],F[l,{aa,cj}]] ==
  Table[{0,If[MemberQ[offset+{3,4},j],Global`FASUNT[aa,ci,cj],0]},{j,14}]];
 AppendTo[M$CouplingMatrices,C[-F[l,{aa,ci}],F[q,{cj}]] ==
  Table[{0,If[MemberQ[offset+{3,4},j],Global`FASUNT[aa,ci,cj],0]},{j,14}]];
 AppendTo[M$CouplingMatrices,C[-F[l,{aa,ci}],F[l,{bb,cj}]] ==
  Table[{0,Which[MemberQ[offset+{5,6},j],
   IndexSum[Global`FASUNT[aa,ci,ck] Global`FASUNT[bb,ck,cj],{ck,1,3}],
   j===offset+7,IndexDelta[aa,bb] IndexDelta[ci,cj],True,0]},{j,14}]];
 AppendTo[M$CouplingMatrices,C[-S[n+3,{sp,aa,ci}],S[n+3,{sq,bb,cj}]] ==
  Join[ConstantArray[{0,0},Length[RGZFHBaseBasis[RGZFHSSKey]]],
   Table[{0,If[j===n,IndexDelta[sp,sq] IndexDelta[aa,bb] IndexDelta[ci,cj],0]},{j,2}]]]],{n,2}];
(* Exported internal color tensors use the base model index convention. *)
M$CouplingMatrices=M$CouplingMatrices /. {
 RGZWorkflow`Engine`Gluon->Global`Gluon,RGZWorkflow`Engine`Colour->Global`Colour};
