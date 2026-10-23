(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Independent expected expressions transcribed from the reviewed PDF,
   equations (3.8), (4.2), (4.5), (5.4), (6.2), (7.5)-(7.6).
   This fixture does not call the production RGZBuildPinches implementation. *)
ClearAll[ATrgz, LTrgz, BTrgz, dBTrgz,
dBBTrgz, PTrgz, FourPiecesGF];
ATrgz[v_] := (SPD[v, v] - mu2)/
((SPD[v, v] - m2)*(SPD[v, v] - mu2) + mg4);
LTrgz[v_, a_] := a/(SPD[v, v]^2 + a*reg4);
BTrgz[v_, a_] := -ATrgz[v]/SPD[v, v] + LTrgz[v, a];
dBTrgz[v_, a_, a0_] := LTrgz[v, a] - LTrgz[v, a0];
dBBTrgz[v_, w_, a_, a0_] :=
BTrgz[v, a]*BTrgz[w, a] -
BTrgz[v, a0]*BTrgz[w, a0];
PTrgz[v_, i_, j_] := MTD[i, j] -
FVD[v, i]*FVD[v, j]/SPD[v, v];
FourPiecesGF[p_, k_, i_, j_, a_, a0_] :=
Module[{ell, sc, ap, al, dk, ck, pp, kk,
rho, sigma, box, nav, av, qse},
ell = k + p; sc = SPD[p, p];
ap = ATrgz[p]; al = ATrgz[ell];
dk = dBTrgz[k, a, a0];
ck = dBBTrgz[k, ell, a, a0];
pp = PTrgz[p, i, j];
kk = PTrgz[p, i, rho]*PTrgz[p, j, sigma]*
FVD[k, rho]*FVD[k, sigma];
box = SMP["g_s"]^2*CA*(pp*al*dk + kk*ck/2);
nav = SMP["g_s"]^2*CA*ap*(
-sc*kk*ck/2 + pp*(SPD[ell, ell] - sc)*al*dk
-kk*al*dk);
av = -SMP["g_s"]^2*(CA/2 - CF)*ap*pp*dk;
qse = -SMP["g_s"]^2*CF*ap*pp*dk/2;
FCI /@ <|"Box" -> box, "NAV_one" -> nav,
"AV_one" -> av, "QSE_one" -> qse|>
];

ClearAll[TotalGF, GammaPPair, FourPinch];
TotalGF[p_, k_, i_, j_, a_, a0_] :=
Module[{ell, sc, ap, al, dk, ck, pp, kk, rho, sigma},
ell = k + p; sc = SPD[p, p];
ap = ATrgz[p]; al = ATrgz[ell];
dk = dBTrgz[k, a, a0];
ck = dBBTrgz[k, ell, a, a0];
pp = PTrgz[p, i, j];
kk = PTrgz[p, i, rho]*PTrgz[p, j, sigma]*
FVD[k, rho]*FVD[k, sigma];
FCI[SMP["g_s"]^2*CA*(
pp*((1 + 2*ap*(SPD[ell, ell] - sc))*al - ap)*dk
+ kk*((1/2 - sc*ap)*ck - 2*ap*al*dk))]
];
GammaPPair[p_, k_, i_, j_] := FCI[
2*SMP["g_s"]^2*CA*ATrgz[p]*PTrgz[p, i, j]*
ATrgz[k]*ATrgz[k + p]];
FourPinch[p_, k_, i_, j_, a_] :=
Module[{ell, sc, ap, al, bk, bl, pp, kk, rho, sigma},
ell = k + p; sc = SPD[p, p];
ap = ATrgz[p]; al = ATrgz[ell];
bk = BTrgz[k, a]; bl = BTrgz[ell, a];
pp = PTrgz[p, i, j];
kk = PTrgz[p, i, rho]*PTrgz[p, j, sigma]*
FVD[k, rho]*FVD[k, sigma];
FCI[SMP["g_s"]^2*CA*(
pp*((1 + 2*ap*(SPD[ell, ell] - sc))*al - ap)*bk
+ kk*((1/2 - sc*ap)*bk*bl - 2*ap*al*bk))]
+ GammaPPair[p, k, i, j]
];
