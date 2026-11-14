(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Role:
   Expose mixed-matrix calculations and load shared loop/quark definitions without loading PT candidates.

   Inputs:
   The checked tree/model data and the graph or task data named by the called function.

   Dependencies:
   Tree/ definitions and Algebra/ tools; Internal/ supplies configuration and persistence.
*)

(* Explicit adapter to the compatible RGZWorkflow`Engine` computational engine. *)
loadLoopEngine[] := engineEvaluation@Module[{r,name,configure},r=checksReady[];If[FailureQ[r],Return[r]];
 configure=!MemberQ[$loaded,"Algebra/TwoPointIntegrals.wl"];
 Do[r=engineGet[name];If[FailureQ[r],Return[r]],
  {name,{"Algebra/LoopPrimitives.wl","OneLoop/Topologies.wl","Internal/MixedRuntime.wl",
    "Algebra/MixedProjection.wl","Algebra/MixedPoles.wl",
    "Algebra/MixedIntegrals.wl","Algebra/TwoPointIntegrals.wl",
    "OneLoop/MixedSelfEnergy.wl"}}];
 If[configure,RGZWorkflow`Engine`RGZMixSetAcceleration["SingleKernelTimeLimit"->120,"PreprocessTimeLimit"->15]];
 RGZWorkflow`Engine`RGZMixSetDisplay[Switch[$config["Display"],"Quiet","Quiet","Verbose","Verbose",_,"Compact"]];Null
];
RGZMixedStart[] := engineEvaluation@Module[{r=loadLoopEngine[]},If[FailureQ[r],r,RGZWorkflow`Engine`RGZMixStart[]]];
Options[RGZMixedStartPair]={"Sectors"->All};
RGZMixedStartPair[a_,b_,seconds_:60,OptionsPattern[]] := engineEvaluation@Module[{r=loadLoopEngine[]},If[FailureQ[r],r,
 RGZWorkflow`Engine`RGZMixStartPair[a,b,"Category"->OptionValue["Sectors"],"SingleStepTimeLimit"->seconds,"reuseAACache"->False]]];
RGZRunGraph[n_Integer,stage_String,seconds_:120] := engineEvaluation@Module[{r=loadLoopEngine[]},If[FailureQ[r],Return[r]];
 If[!MemberQ[{"Convert","Project","DecomposeT","IntegrateT","ScalarT","DecomposeL","IntegrateL","ScalarL"},stage],Return[Failure["GraphStage",<||>]]];
 RGZWorkflow`Engine`RGZMXRun[n,stage,seconds]
];
Options[RGZMixedComputePair]={"Sectors"->All};
RGZMixedComputePair[a_,b_,seconds_:300,OptionsPattern[]] := engineEvaluation@Module[{r=loadLoopEngine[]},If[FailureQ[r],r,
 RGZWorkflow`Engine`RGZMixComputePair[a,b,seconds,"Category"->OptionValue["Sectors"],"reuseAACache"->False]]];
Options[RGZMixedComputeAll]={"Sectors"->All,"Pairs"->All};
RGZMixedComputeAll[seconds_:300,OptionsPattern[]] := engineEvaluation@Module[{r=loadLoopEngine[]},If[FailureQ[r],r,
 RGZWorkflow`Engine`RGZMixComputeAll[seconds,"FieldPair"->OptionValue["Pairs"],"Category"->OptionValue["Sectors"],"reuseAACache"->False]]];
RGZMixedResult[seconds_:120] := engineEvaluation@Module[{r=loadLoopEngine[]},If[FailureQ[r],Return[r]];
 r=RGZWorkflow`Engine`RGZMixResult[seconds];If[FailureQ[r],Return[r]];
 If[!validMixed[r],Return[Failure["InvalidMixedMatrices",<||>]]];
 RGZWorkflow`Engine`rgzMixed=r;storeResult["Mixed",r];<|"Status"->"Complete","Name"->"Mixed","Dimensions"->{{5,5},{8,8},{8,8}}|>
];
RGZStatus["Mixed"] := engineEvaluation@Module[{r=loadLoopEngine[]},If[FailureQ[r],r,RGZWorkflow`Engine`RGZMixStatus[]]];
(* Explicit propagator identities used in the zero-temperature calculation. *)
RGZSplit[expr_,loop_:RGZWorkflow`Engine`q,ext_:RGZWorkflow`Engine`p,seconds_:60] := engineEvaluation@Module[{r=loadLoopEngine[]},If[FailureQ[r],r,timed["PoleSplit",RGZWorkflow`Engine`RGZMXSplit[expr,loop,ext],seconds]]];
RGZPoleMasses[expr_] := engineEvaluation@Module[{r=loadLoopEngine[]},If[FailureQ[r],r,RGZWorkflow`Engine`RGZLoopPoleMasses[expr]]];

loadSharedLoopTools[] := engineEvaluation@Module[{r=loadLoopEngine[]},
 If[FailureQ[r],Return[r]];engineGet["Algebra/MixedProjection.wl"]];
loadQuarkEngine[] := engineEvaluation@Module[{r=loadSharedLoopTools[],name},
 If[FailureQ[r],Return[r]];
 Do[r=engineGet[name];If[FailureQ[r],Return[r]],
  {name,{"Renormalization/RegulatorLimit.wl","OneLoop/QuarkGraphs.wl",
    "OneLoop/QuarkSelfEnergy.wl","Renormalization/QuarkMassLimit.wl"}}];Null];
