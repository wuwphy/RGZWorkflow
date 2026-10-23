(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
project=DirectoryName[DirectoryName[$InputFileName]];Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
checks=<||>;record[n_,v_]:=(AssociateTo[checks,n->TrueQ[v]];Print[n," : ",TrueQ[v]]);
delta=RGZWorkflow`Engine`mG^4-(RGZWorkflow`Engine`m^2-RGZWorkflow`Engine`\[Mu]^2)^2/4;
(* Deliberately uncollected cancelling sums inside the outer radical. *)
base=-(a-a-2 I Sqrt[delta])^2;
record["NestedDiscriminantRootExact",RGZWorkflow`Private`qqBranchCanonicalCoefficient[Sqrt[base]-2 Sqrt[delta]]===0];
record["NegativeHalfPowerExact",RGZWorkflow`Private`qqBranchCanonicalCoefficient[base^(-1/2)-1/(2 Sqrt[delta])]===0];
record["GenericSquareBranchPreserved",RGZWorkflow`Private`qqCanonicalSpectralRadicals[Sqrt[z^2]]===Sqrt[z^2]];
record["NegativeScalingNotExtracted",RGZWorkflow`Private`qqCanonicalSpectralRadicals[Sqrt[-delta]]===Sqrt[-delta]];
Do[point={RGZWorkflow`Engine`m->1,RGZWorkflow`Engine`\[Mu]->2,RGZWorkflow`Engine`mG->v};
 record["PrincipalBranchAt"<>ToString[v],Simplify[(Sqrt[base]-RGZWorkflow`Private`qqCanonicalSpectralRadicals[Sqrt[base]])/.point]===0],{v,{0,1,2,I,1+I}}];
p=<|"Rows"->{<|"Difference"->Sqrt[base]-2 Sqrt[delta],"Status"->"Uncancelled"|>},"AllCoefficientsZero"->False|>;
r=RGZWorkflow`Private`qqBranchCanonicalComparison[p];
record["ExactZeroComparisonRetagged",r["AllCoefficientsZero"]===True&&r["Status"]==="VerifiedSymbolicZero"];
r=RGZWorkflow`Private`qqBranchCanonicalComparison[<|"Rows"->{<|"Difference"->1|>}|>];
record["NonzeroComparisonNotOverridden",r["AllCoefficientsZero"]===False];
record["FailuresPreserved",RGZWorkflow`Private`qqBranchCanonicalComparison[Failure["Stop",<||>]]===Failure["Stop",<||>]];
record["ComparisonCanonicalizerInstalledOnce",Count[DownValues[RGZWorkflow`Private`qqGaugeCompare],HoldPattern[RGZWorkflow`Private`qqBranchCanonicalCoefficient[_]],Infinity]===1];
Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
record["ReloadDoesNotDuplicateCanonicalizer",Count[DownValues[RGZWorkflow`Private`qqGaugeCompare],HoldPattern[RGZWorkflow`Private`qqBranchCanonicalCoefficient[_]],Infinity]===1];
Print[<|"Passed"->Count[Values[checks],True],"Failed"->Count[Values[checks],False]|>];Exit[If[And@@Values[checks],0,1]];
