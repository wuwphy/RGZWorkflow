(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
project=DirectoryName[DirectoryName[$InputFileName]];
Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
dir=FileNameJoin[{$TemporaryDirectory,"rgz-restore-tests-"<>CreateUUID[]}];RGZInitialize[dir,"Display"->"Quiet"];
Quiet[RGZLoadFeynCalc[],FrontEndObject::notavail];
RGZWorkflow`Private`loadPTEngine[];
RGZWorkflow`Private`engineGet["RGZ_Residual39_M0_Diagnostic.wl"];
RGZWorkflow`Private`engineGet["RGZ_Quark_Amplitude.wl",False];
checks=<||>;record[n_,v_]:=(AssociateTo[checks,n->TrueQ[v]];Print[n," : ",TrueQ[v]]);
x=RGZWorkflow`Engine`qqM0X;y=RGZWorkflow`Engine`qqM0Y;s=RGZWorkflow`Engine`qqM0S;
h=RGZResidual39Diagnostic`Private`h;dim=RGZResidual39Diagnostic`Private`dim;
f=(h^2+dim/(x-y)) RGZResidual39Diagnostic`Private`a0[x]+h^4/(s+x) RGZResidual39Diagnostic`Private`b0[s,x,y]+5;
expected=FeynCalc`SMP["g_s"]^2 ((RGZWorkflow`Engine`\[Alpha]+D/(RGZWorkflow`Engine`m^2-RGZWorkflow`Engine`\[Mu]^2)) FeynCalc`A0[RGZWorkflow`Engine`m^2]+
 RGZWorkflow`Engine`\[Alpha]^2/(FeynCalc`SPD[RGZWorkflow`Engine`p,RGZWorkflow`Engine`p]+RGZWorkflow`Engine`m^2) FeynCalc`B0[FeynCalc`SPD[RGZWorkflow`Engine`p,RGZWorkflow`Engine`p],RGZWorkflow`Engine`m^2,RGZWorkflow`Engine`\[Mu]^2]+5);
zeroDifference[a_,b_]:=Module[{p=RGZWorkflow`Engine`RGZGaugePVPairs[a-b]},ListQ[p]&&And@@(TrueQ[Cancel[Together[#[[2]]]]===0]&/@p)];
r=RGZWorkflow`Private`qqRestoreProgress[f];
record["IndependentParameterAndDimensionRestoration",zeroDifference[r,expected]];
record["NoEvanescentDimensionDiscarded",!FreeQ[r,D]];
record["GaugeParameterNotSpecialized",!FreeQ[r,RGZWorkflow`Engine`\[Alpha]]];
record["InternalCoordinatesFullyRestored",FreeQ[r,h|dim|RGZWorkflow`Engine`qqM0X|RGZWorkflow`Engine`qqM0Y|RGZWorkflow`Engine`qqM0S]];
record["ZeroInputStaysZero",RGZWorkflow`Private`qqRestoreProgress[0]===0];
record["RationalRemainderCouplingAppliedOnce",RGZWorkflow`Private`qqRestoreProgress[5]===5 FeynCalc`SMP["g_s"]^2];
files=FileNames["master-*.wxf",RGZWorkflow`Private`cacheDirectory[],Infinity];hashes=FileHash[#,"SHA256"]&/@files;
r2=RGZWorkflow`Private`qqRestoreProgress[f];
record["RestoredMastersReusedWithoutRewrite",SameQ[r,r2]&&(FileHash[#,"SHA256"]&/@files)===hashes];
r3=Block[{RGZWorkflow`Private`$qqRestoreCoefficientSeconds=0.00000001},RGZWorkflow`Private`qqRestoreProgress[f+1]];
record["OptionalAlgebraTimeoutRetainsExactExpression",zeroDifference[r3,expected+FeynCalc`SMP["g_s"]^2]];
record["IncompleteInputRejected",FailureQ[RGZWorkflow`Private`qqRestoreProgress[TerminatedEvaluation["IterationLimit"]]]];
RGZWorkflow`Private`qqRestoreProgress[f];path=RGZResult["QQProgress"]["CheckpointFile"];p=Import[path,"WXF"];
AssociateTo[p,"Value"->TerminatedEvaluation["IterationLimit"]];Export[path,p,"WXF"];
record["InvalidCheckpointRejected",FailureQ[RGZWorkflow`Private`qqRestoreProgress[f]]];
record["MasterProgressCountsAreExplicit",RGZResult["QQProgress"]["Total"]===3];
Print[<|"Passed"->Count[Values[checks],True],"Failed"->Count[Values[checks],False]|>];Exit[If[And@@Values[checks],0,1]];
