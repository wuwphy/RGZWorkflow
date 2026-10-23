(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* RGZ-CONTENT {"Schema":"RGZModelContent/1","JacobianGhostIncluded":true,"ClosedQuarkFlavors":2,"GhostClasses":[1,2,3,4],"ScalarGhostClasses":[1,4],"QuarkClasses":[1,2]} *)
(* RGZ_W2.6.mod: direct Minkowski two-point rational functions. *)
FR$ModelInformation = {
  ModelName -> "QCD-RGZ Model",
  Authors -> {"Wan Wu"},
  Version -> "2.7",
  Date -> "23. 10. 2026",
  Institutions -> {"Tsinghua University"},
  Description -> "Local RGZ with eta and two massless dynamical quarks"
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

M$ClassesDescription = {

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
