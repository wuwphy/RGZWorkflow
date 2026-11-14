(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Role:
   Give the D-dimensional on-shell quark self-energy residue SigmaV(0).

   Inputs:
   The gauge parameter, regulator mass and physical transverse pole masses.

   Dependencies:
   FeynCalc scalar masters and common canonical model symbols. Defines a residue formula; does not attach mixed matrices or run a task.
*)


(* On-shell residue from the linear small-external-momentum expansion.
   Raw self-energy is -i Sigma. Keep D until UV/finite terms are separated. *)
rgzQQSigmaV0[gauge_,reg_] := Module[{r3,r4,a3,a4,l1,l2,trans,long},
 r3=(m^2+\[Mu]^2)/2+I Sqrt[mG^4-(m^2-\[Mu]^2)^2/4];r4=(m^2+\[Mu]^2)/2-I Sqrt[mG^4-(m^2-\[Mu]^2)^2/4];
 a3=(r3-\[Mu]^2)/(r3-r4);a4=(r4-\[Mu]^2)/(r4-r3);
 trans=-(D-1)(D-4)/D (a3 A0[r3]/r3+a4 A0[r4]/r4);
 long=If[TrueQ[gauge===0],0,
 If[TrueQ[reg===0],Return[Failure["MasslessLongitudinalResidueNeedsIRPrescription",<|"Action"->"Keep RegulatorMass symbolic and sum the complete amplitude before M0."|>]]];
 l1=-I Sqrt[gauge reg^4];l2=I Sqrt[gauge reg^4];-gauge (A0[l1]-A0[l2])/(l1-l2)];
 SMP["g_s"]^2 (SUNN^2-1)/(2 SUNN)/(16 Pi^2) (trans+long)];
