(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* ======================================================*)(*Full RGZ vacuum integral:explicit MSbar subtraction*)(* ======================================================*)ClearAll[vacFullL,vacDeltaL,vacDeltaCollectedL,vacUVCoefL,vacUVTermL,vacCTL,vacMSbarL,checkSubtractionL,checkReconstructionL];

(*------------------------------------------------------*)
(*1. Full one-loop integration*)
(*------------------------------------------------------*)

vacFullL=PaXEvaluateUVIRSplit[resultVacL,PaXImplicitPrefactor->1/(2 Pi)^D,PaXC0Expand->True,PaXD0Expand->True,FCVerbose->0];

Print["Step 1: Full unrenormalized integral"];

vacFullL//FCE


(*------------------------------------------------------*)
(*2. Express UV/IR singularities in Delta notation*)
(*------------------------------------------------------*)

vacDeltaL=FCHideEpsilon[Expand[vacFullL]];

vacDeltaCollectedL=Collect[vacDeltaL,{SMP["Delta_UV"],SMP["Delta_IR"]},Simplify];

Print["Step 2: UV/IR pole structure"];

vacDeltaCollectedL//FCE


(*------------------------------------------------------*)
(*3. Extract UV pole coefficient*)
(*------------------------------------------------------*)

vacUVCoefL=Simplify[Coefficient[Expand[vacDeltaL],SMP["Delta_UV"],1]];

Print["Step 3: UV pole coefficient"];

vacUVCoefL//FCE


(*------------------------------------------------------*)
(*4. Reconstruct full UV-divergent contribution*)
(*------------------------------------------------------*)

vacUVTermL=SMP["Delta_UV"] vacUVCoefL;

Print["Step 4: Full UV-divergent term"];

vacUVTermL//FCE


(*------------------------------------------------------*)
(*5. Explicit MSbar counterterm*)
(*------------------------------------------------------*)

vacCTL=-vacUVTermL;

Print["Step 5: Counterterm"];

vacCTL//FCE


(*------------------------------------------------------*)
(*6. Renormalized vacuum result*)
(*------------------------------------------------------*)

vacMSbarL=Simplify[Expand[vacDeltaL+vacCTL]];

Print["Step 6: MSbar-subtracted vacuum result"];

vacMSbarL//FCE


(*------------------------------------------------------*)
(*7. Check subtraction*)
(*------------------------------------------------------*)

checkSubtractionL=Simplify[Expand[vacMSbarL-(vacDeltaL/. SMP["Delta_UV"]->0)]];

Print["Subtraction check = ",checkSubtractionL];


(*------------------------------------------------------*)
(*8. Check reconstruction*)
(*------------------------------------------------------*)

checkReconstructionL=Simplify[Expand[vacDeltaL-(vacMSbarL+vacUVTermL)]];

Print["Reconstruction check = ",checkReconstructionL];


(*------------------------------------------------------*)
(*9. Check remaining singularities*)
(*------------------------------------------------------*)

Print["UV Delta remains: ",!FreeQ[vacMSbarL,SMP["Delta_UV"]]];

Print["Explicit UV pole remains: ",!FreeQ[vacMSbarL,EpsilonUV]];

Print["IR Delta remains: ",!FreeQ[vacMSbarL,SMP["Delta_IR"]]];

Print["Explicit IR pole remains: ",!FreeQ[vacMSbarL,EpsilonIR]];


(*------------------------------------------------------*)
(*10. Final result*)
(*------------------------------------------------------*)

vacMSbarL//FCE
