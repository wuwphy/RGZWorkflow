(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
project=DirectoryName[DirectoryName[$InputFileName]];
Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
RGZInitialize[FileNameJoin[{project,"Work"}]];
Quiet[RGZLoadFeynCalc[],FrontEndObject::notavail];
summary=RGZNAVSum[120];Print[summary];
If[FailureQ[summary],Exit[1]];
nav=RGZResult["NAV"];
checks=<||>;record[name_,test_]:=(AssociateTo[checks,name->TrueQ[test]];Print[name," : ",checks[name]]);
record["FullConnections73",nav["ConnectionCount"]===73];
record["ConservedConnections44",nav["ConservedCurrentConnectionCount"]===44];
record["CentralEndpointCounts",Counts[First/@Lookup[nav["Connections"],"Fields"]]===<|"A"->16,"beta"->7,"betabar"->7,"xi"->26,"tau"->3,"phi"->7,"phibar"->7|>];
record["NoFailedExpressions",FreeQ[nav,$Failed|$TimedOut|_Failure]];
record["NoCentralLorentzIndices",FreeQ[{nav["Tensor"],nav["TransverseTensor"]},RGZWorkflow`Engine`rgzNAVL0|RGZWorkflow`Engine`rgzNAVL1|RGZWorkflow`Engine`rgzNAVL2]];
record["OnlyAAAAfterZeroMixingAndLandau",AllTrue[Select[nav["Connections"],#["Fields"]=!={"A","A","A"}&],TrueQ[Simplify[#["Tensor"]/.{RGZWorkflow`Engine`mG->0,RGZWorkflow`Engine`\[Alpha]->0}]===0]&]];
record["OnlyAAAInFullQCDLimitAtGeneralAlpha",AllTrue[Select[nav["Connections"],#["Fields"]=!={"A","A","A"}&],TrueQ[Simplify[#["Tensor"]/.{RGZWorkflow`Engine`mG->0,RGZWorkflow`Engine`m->0,RGZWorkflow`Engine`\[Mu]->0,RGZWorkflow`Engine`M->0}]===0]&]];
record["ScalarExchangeKilledByConservedCurrent",AllTrue[Select[nav["Connections"],MemberQ[{"xi","tau","b"},First[#["Fields"]]]&],#["TransverseTensor"]===0&]];
record["QuarkLineAttached",!FreeQ[nav["Integrand"],FeynCalc`GAD|FeynCalc`GSD]];
record["InternalQuarkLorentzIndicesContracted",FreeQ[nav["Integrand"],RGZWorkflow`Engine`rgzNAVRho|RGZWorkflow`Engine`rgzNAVSigma]];
record["InternalColourIndicesContracted",FreeQ[nav["Integrand"],RGZWorkflow`Engine`rgzNAVB|RGZWorkflow`Engine`rgzNAVC|RGZWorkflow`Engine`rgzNAVC0|RGZWorkflow`Engine`rgzNAVC1|RGZWorkflow`Engine`rgzNAVC2|RGZWorkflow`Engine`rgzNAVE0|RGZWorkflow`Engine`rgzNAVE1|RGZWorkflow`Engine`rgzNAVE2]];
Print[<|"Passed"->Count[Values[checks],True],"Failed"->Count[Values[checks],False]|>];
Export[FileNameJoin[{project,"output","review","NAVMatrix-summary.json"}],Join[summary,<|"Checks"->checks|>],"RawJSON"];
Exit[If[And@@Values[checks],0,1]];
