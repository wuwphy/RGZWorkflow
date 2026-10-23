(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
(* Exact algebra only; deliberately outside the physical-job fingerprint.
   Completed checkpoints remain valid because neither amplitudes nor conventions change. *)
qqGroupedRational[input_] := Module[{x=RGZWorkflow`Engine`qqM0X,y=RGZWorkflow`Engine`qqM0Y,
 z=RGZWorkflow`Engine`qqM0Z,u,v,base,root,transform,terms,parts,groups,dens,nums,
 converted,variables,maxPowers=<||>,normalize,normalized,common,polys,result,back,tag=Unique["qqAlgebra"]},
 base=z-(x-y)^2/4;
 root[rad_,power_]:=Module[{ratio=Cancel[Together[rad/base]]},
 If[MatchQ[ratio,_Integer|_Rational]&&TrueQ[ratio>0],
 (Sqrt[ratio]*(v-u)/(2 I))^(2 power),rad^power]];
 transform[expr_]:=(expr/.HoldPattern[Power[rad_,power_Rational]]/;Denominator[power]===2:>root[rad,power])/.
 {x->u+v-y,z->(u-y)(v-y)};
 Catch[
 qqProgressOperation["Group identical denominator products"];
 terms=If[Head[input]===Plus,List@@input,{input}];
 parts=Function[term,Module[{f=If[Head[term]===Times,List@@term,{term}],negative},
 negative=Select[f,MatchQ[#,Power[_,(_Integer|_Rational)?Negative]]&];
 {negative,Times@@Complement[f,negative]}]] /@ terms;
 groups=GatherBy[parts,First];dens=groups[[All,1,1]];
 nums=Total[#[[All,2]]]& /@ groups;Clear[parts,groups,terms];
 qqProgressUpdate[<|"DenominatorGroups"->Length[dens]|>];
 qqProgressOperation["Convert and verify spectral-pole denominators"];
 converted=Factor[Together[transform[Times@@#]]]& /@ dens;
 back={u->(x+y)/2-I Sqrt[base],v->(x+y)/2+I Sqrt[base]};
 If[!And@@MapThread[TrueQ[Cancel[Together[(#1/.back)-Times@@#2]]===0]&,{converted,dens}],Throw[$Failed,tag]];
 variables=DeleteDuplicates[Cases[{converted,nums},a_Symbol/;!NumericQ[a]:>a,Infinity]];
 variables=DeleteDuplicates[Join[DeleteCases[variables,x|z],{u,v,y}]];
 normalize[den_]:=Module[{fac=If[Head[den]===Times,List@@den,{den}],unit=1,norm=1,bb,nn,lead},
 Do[If[NumericQ[f],unit*=f;Continue[]];
 {bb,nn}=If[MatchQ[f,Power[_,_Integer]],List@@f,{f,1}];
 If[!PolynomialQ[bb,variables]||!IntegerQ[nn]||nn<0,Throw[$Failed,tag]];
 lead=Last[First[CoefficientRules[bb,variables]]];If[!NumericQ[lead],Throw[$Failed,tag]];
 bb=Expand[bb/lead];unit*=lead^nn;norm*=bb^nn;
 AssociateTo[maxPowers,bb->Max[Lookup[maxPowers,bb,0],nn]],{f,fac}];{unit,norm}];
 normalized=normalize[Denominator[#]]& /@ converted;
 common=Times@@KeyValueMap[#1^#2&,maxPowers];
 qqProgressOperation["Combine polynomial numerator groups"];
 polys=Table[If[Mod[i-1,20]===0,qqProgressUpdate[<|"AlgebraGroupIndex"->i,"AlgebraGroupTotal"->Length[nums]|>]];Module[{n=Expand[transform[nums[[i]]]],multiplier},
 If[!PolynomialQ[n,variables],Throw[$Failed,tag]];
 multiplier=Cancel[common/normalized[[i,2]]];
 If[!PolynomialQ[multiplier,variables],Throw[$Failed,tag]];
 Expand[n Numerator[converted[[i]]]/normalized[[i,1]] multiplier]],{i,Length[nums]}];
 qqProgressOperation["Factor compact numerator"];
 result=Factor[Expand[Total[polys]]]/common;
 qqProgressOperation["Restore original mass coordinates"];
 result=Cancel[result/.back];
 If[!FreeQ[result,u|v],Throw[$Failed,tag]];
 qqProgressUpdate[<|"AlgebraGroupIndex"->Length[nums],"AlgebraGroupTotal"->Length[nums],"OutputLeaves"->LeafCount[result],"AlgebraMethod"->"GroupedSpectralPoles"|>];result,
 tag]
 ];
qqSimplifyCoefficient[input_] := Module[{r},
 r=qqGroupedRational[input];
 If[r=!=$Failed,Return[r]];
 qqProgressUpdate[<|"AlgebraMethod"->"OriginalRationalFallback"|>];
 qqProgressOperation["Together: common denominator (fallback)"];r=Together[input];
 qqProgressOperation["Cancel: rational cancellation"];r=Cancel[r];
 qqProgressOperation["Factor: factor coefficient"];Factor[r]];
