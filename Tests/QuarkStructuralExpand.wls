(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
project=DirectoryName[DirectoryName[$InputFileName]];
Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
RGZInitialize[FileNameJoin[{$TemporaryDirectory,"rgz-structural-"<>CreateUUID[]}],"Display"->"Quiet"];
Quiet[RGZLoadFeynCalc[],FrontEndObject::notavail];
checks=<||>;record[n_,v_]:=(AssociateTo[checks,n->TrueQ[v]];Print[n," : ",TrueQ[v]]);
colors=RGZWorkflow`Private`qqFFColors[];
chain=FeynCalc`FCI[FeynCalc`SpinorUBarD[RGZWorkflow`Engine`qqK3,0].
 (FeynCalc`GAD[testMu]+a FeynCalc`GSD[RGZWorkflow`Engine`qqK2]).
 FeynCalc`SpinorUD[RGZWorkflow`Engine`qqK1,0]];
x=(colors[[1]]+2 colors[[2]]) chain (a+b)^2 (FeynCalc`B0[t,m1,m2]+c);
parts=RGZWorkflow`Private`qqFFStructuralParts[x];
old=RGZWorkflow`Private`qqLinearizeChains[x];
record["IndependentOriginalLinearizerReconstruction",ListQ[parts]&&Expand[Total[Times@@@parts]-old]===0];
record["SpinAndColorSumsDistributed",Length[parts]===4];
record["ScalarPolynomialsRemainFactored",!FreeQ[parts[[All,3]],(a+b)^2]];
duplicateInput=colors[[1]] chain (a+b)+colors[[1]] chain (c+d);
duplicateParts=RGZWorkflow`Private`qqFFStructuralParts[duplicateInput];
record["EqualStructuresGroupedBeforeScalarRestoration",Length[duplicateParts]===2&&
 Expand[Total[Times@@@duplicateParts]-RGZWorkflow`Private`qqLinearizeChains[duplicateInput]]===0];
record["CoefficientSlotsDoNotEscape",FreeQ[parts,s_Symbol/;StringStartsQ[SymbolName[s],"slot$"]]];
big=(a+b)^40 (c+d)^40;
bigParts=RGZWorkflow`Private`qqFFStructuralParts[colors[[1]] chain big];
record["LargeScalarFactorsDoNotMultiplyStructuralTerms",Length[bigParts]===2&&LeafCount[bigParts]<300];
pure=RGZWorkflow`Private`qqFFStructuralParts[big+1/(a-b)];
record["PureScalarSumRemainsOneCoefficient",pure==={{1,1,big+1/(a-b)}}];
pv=FeynCalc`B0[t,m1+m2,m3];
terms=RGZWorkflow`Private`qqFFMasterTerms[big (1+pv)];
record["OnlyPVDependenceDistributed",Length[terms]===2&&LeafCount[terms]<100];
collected=RGZWorkflow`Private`qqFFCollectMasters[big (1+pv),False,1];
record["KnownPVAndRationalCoefficientsExact",Sort[collected]===Sort[{{1,big},{pv,big}}]];
record["PVArgumentsUntouched",MemberQ[collected[[All,1]],pv]];
record["MasterProductStillRejected",FailureQ[RGZWorkflow`Private`qqFFCollectMasters[pv FeynCalc`A0[m1],False,1]]];
record["MasterHiddenInNonlinearFunctionRejected",FailureQ[RGZWorkflow`Private`qqFFCollectMasters[Sin[pv],False,1]]];
entangled=RGZWorkflow`Private`qqFFStructuralParts[badHead[colors[[1]],chain]];
record["EntangledColorSpinStillRejected",FailureQ[entangled]];
Print[<|"Passed"->Count[Values[checks],True],"Failed"->Count[Values[checks],False]|>];
Exit[If[And@@Values[checks],0,1]];
