(* SPDX-License-Identifier: GPL-3.0-or-later *)
(* Actual QQ setup must not implicitly load or initialize candidate PT work. *)
project=DirectoryName[DirectoryName[$InputFileName]];
Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
RGZInitialize[CreateDirectory[]];
If[!AssociationQ[RGZLoadFeynCalc[]],Exit[1]];
checks=<||>;rec[n_,v_]:=(AssociateTo[checks,n->TrueQ[v]];Print[n," ",TrueQ[v]]);
rec["RootLoadDoesNotLoadCandidates",FreeQ[RGZWorkflow`Private`$loaded,s_String/;StringStartsQ[s,"Pinch/"]]];
(* The public entry must reach the missing-matrix prerequisite with ordinary
   tools loaded, rather than requiring any PT/QCD candidate setup. *)
r=RGZQQStart[30];
rec["QQReachesItsOwnMatrixPrerequisite",MatchQ[r,Failure["MixedMatricesRequired",_]]];
rec["QQDoesNotLoadCandidates",FreeQ[RGZWorkflow`Private`$loaded,s_String/;StringStartsQ[s,"Pinch/"]]];
rec["QQDoesNotLoadStandaloneAADriver",!MemberQ[RGZWorkflow`Private`$loaded,"OneLoop/GluonSelfEnergy.wl"]&&DownValues[RGZWorkflow`Engine`RGZRunAA]==={}];
rec["CandidateKernelsRemainUndefined",DownValues[RGZWorkflow`Engine`RGZBuildPinches]==={}];
rec["CandidateTasksRemainUndefined",DownValues[RGZWorkflow`Engine`RGZPTRun]==={}&&OwnValues[RGZWorkflow`Engine`rgzPTJob]==={}];
shared={DownValues[RGZWorkflow`Engine`RGZGaugeStart],DownValues[RGZWorkflow`Engine`RGZGaugePVPairs],DownValues[RGZWorkflow`Engine`RGZLoopColour]};
rec["SharedConversionValidatorAvailable",Length[DownValues[RGZWorkflow`Engine`RGZLoopUnresolved]]>0];
rec["SharedToolsAvailable",AllTrue[shared,Length[#]>0&]];
rec["QuarkResidueAndMassToolsAvailable",Length[DownValues[RGZWorkflow`Engine`rgzQQSigmaV0]]>0&&Length[DownValues[RGZWorkflow`Engine`rgzQQM0Term]]>0];
r=RGZWorkflow`Private`loadPTEngine[];
rec["ExplicitCandidateLoadWorks",!FailureQ[r]&&Length[DownValues[RGZWorkflow`Engine`RGZBuildPinches]]>0&&Length[DownValues[RGZWorkflow`Engine`RGZPTRun]]>0];
rec["BothRoutesUseIdenticalSharedTools",shared==={DownValues[RGZWorkflow`Engine`RGZGaugeStart],DownValues[RGZWorkflow`Engine`RGZGaugePVPairs],DownValues[RGZWorkflow`Engine`RGZLoopColour]}];
rec["LoadingStillDoesNotStartPTTask",OwnValues[RGZWorkflow`Engine`rgzPTJob]==={}];
Print["SUMMARY ",checks];Exit[If[And@@Values[checks],0,1]];
