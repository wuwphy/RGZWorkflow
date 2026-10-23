(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
checks=<||>;record[n_,v_]:=(AssociateTo[checks,n->TrueQ[v]];Print[n," : ",checks[n]]);
base=4 mG^4-(m^2-\[Mu]^2)^2;
Do[a=RGZGaugeFixedCoordinates[base^k];b=RGZGaugeFixedCoordinates[Expand[base]^k];
 record["ExpandedDiscriminant:"<>ToString[k,InputForm],AssociationQ[a]&&AssociationQ[b]&&a["加项"]===b["加项"]],{k,{1/2,-1/2,3/2,-3/2}}];
base=mG^4-(m^2-\[Mu]^2)^2/4;
Do[a=RGZGaugeFixedCoordinates[base^k];b=RGZGaugeFixedCoordinates[Expand[base]^k];
 record["ExpandedGap:"<>ToString[k,InputForm],AssociationQ[a]&&AssociationQ[b]&&a["加项"]===b["加项"]],{k,{1/2,-1/2,3/2,-3/2}}];
record["UnknownRootStillRejected",FailureQ[RGZGaugeFixedCoordinates[Sqrt[1+m^2]]]];
record["OddPoleMassStillRejected",FailureQ[RGZGaugeFixedCoordinates[rgzLoopM1]]];
(* Actual current full NAV scalar, strip master integrals to inspect all
   coefficient summands without loading the multi-GB prepared session. *)
file=FileNameJoin[{RGZWorkflow`Private`cacheDirectory[],"RGZ_PT_candidate_cache_1e4cc54bda33","part_2_4.wl"}];
If[FileExistsQ[file],packet=Get[file];expr=packet["结果"];
 objects=DeleteDuplicates[Cases[expr,z_/;RGZMXPVObjectQ[z],{0,Infinity}]];
 raw=expr/.Thread[objects->1];coords=RGZGaugeFixedCoordinates[raw];
 record["ActualFullNAVAllCoefficientCoordinates",AssociationQ[coords]];
 If[AssociationQ[coords],record["ActualFullNAVNoRootsRemain",FreeQ[coords["加项"],Power[_,k_Rational]/;Denominator[k]=!=1]]];
 (* Exact numeric reconstruction with opposite signs of the discriminant;
    preserves principal sqrt branches, including a purely imaginary gap. *)
 branchRaw=Expand[4 mG^4-(m^2-\[Mu]^2)^2]^(-3/2)+2 Expand[4 mG^4-(m^2-\[Mu]^2)^2]^(1/2);
 branchCoordinates=RGZGaugeFixedCoordinates[branchRaw];
 Do[restored=Total[branchCoordinates["加项"]]/.branchCoordinates["还原规则"];restored=branchCoordinates["公共因子"]restored;
 substitutions={m->mm,\[Mu]->2,mG->3,M->1,\[Alpha]->2,SUNN->3,D->4,HoldPattern[Pair[Momentum[p,___],Momentum[p,___]]]->19,HoldPattern[SPD[p,p]]->19,HoldPattern[SP[p,p]]->19,SMP["g_s"]->1};
 err=Chop[N[restored/.substitutions,40]-N[branchRaw/.substitutions,40],10^-25];
 record["PrincipalBranchReconstruction:"<>ToString[mm],TrueQ[err===0]],{mm,{1,6}}]];
Print[<|"Passed"->Count[Values[checks],True],"Failed"->Count[Values[checks],False]|>];
Exit[If[And@@Values[checks],0,1]];
