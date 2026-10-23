(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
project=DirectoryName[DirectoryName[$InputFileName]];
Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
RGZInitialize[FileNameJoin[{$TemporaryDirectory,"qq-pv-relations-"<>CreateUUID[]}],"Display"->"Quiet"];
Quiet[RGZLoadFeynCalc[],FrontEndObject::notavail];
checks=<||>;record[n_,v_]:=(AssociateTo[checks,n->TrueQ[v]];Print[n," : ",TrueQ[v]]);
canon=RGZWorkflow`Private`qqPVCanonical;
m=FeynCalc`PaVe[2,{u,t,s},{a,c,b},FeynCalc`PaVeAutoOrder->False];
canonicalTriangle=canon[m];
record["TriangleCanonicalizationChangesLabel",RGZWorkflow`Private`qqPVParse[canonicalTriangle][[1]]==={1}];
r=Block[{FeynCalc`$LimitTo4=False},FeynCalc`PaVeReduce[m,FeynCalc`BReduce->True,FeynCalc`PaVeAutoReduce->True]-FeynCalc`PaVeReduce[canonicalTriangle,FeynCalc`BReduce->True,FeynCalc`PaVeAutoReduce->True]];
record["GenericMassTriangleNativeCrossCheck",Cancel[Together[FeynCalc`PaVeOrder[r]]]===0];
record["CanonicalizationIdempotent",canon[canonicalTriangle]===canonicalTriangle];
record["ScalarBubbleAlias",canon[FeynCalc`PaVe[0,{s},{a,b}]]===FeynCalc`B0[s,a,b]];
record["ScalarTadpoleAlias",canon[FeynCalc`PaVe[0,{},{a}]]===FeynCalc`A0[a]];
record["ScalarTriangleAliasesMatch",canon[FeynCalc`PaVe[0,{u,t,s},{a,c,b},FeynCalc`PaVeAutoOrder->False]]===canon[FeynCalc`C0[u,t,s,a,c,b]]];
(* Independent Feynman-parameter polynomial and moment reconstruction,
   including the five-point family that native PaVeReduce cannot reduce. *)
parameter[v_,xx_]:=Module[{edges=RGZWorkflow`Private`qqPVEdges[Length[v[[3]]]],ind=v[[1]],n=Length[v[[3]]]},
 {Total[v[[3]] xx]-Total[MapThread[#1 xx[[#2[[1]]+1]] xx[[#2[[2]]+1]]&,{v[[2]],edges}]],
 Times@@Table[xx[[k+1]]^Count[ind,k],{k,1,n-1}]}];
Do[inv=Reverse[Array[Unique["s"]&,n(n-1)/2]];mass=Reverse[Array[Unique["m"]&,n]];
 orig={{0,0,1,n-1},inv,mass};raw=FeynCalc`PaVe[Sequence@@orig[[1]],inv,mass,FeynCalc`PaVeAutoOrder->False];
 dest=RGZWorkflow`Private`qqPVParse[canon[raw]];xx=Array[Unique["x"]&,n];p1=parameter[orig,xx];p2=parameter[dest,xx];
 perms=Prepend[#,1]& /@Permutations[Range[2,n]];
 good=AnyTrue[perms,Function[per,And@@(Expand[#]===0& /@(p1-(p2/.Thread[xx->xx[[per]]])) )]];
 record["ParameterPolynomialAndMomentsN"<>ToString[n],good],{n,3,5}];
m=FeynCalc`PaVe[0,0,{s},{a,b}];r=RGZWorkflow`Private`qqPVNativeReduce[m];
expected=((a-b+s) FeynCalc`A0[a]-(a-b-s)FeynCalc`A0[b]-(a^2-2a b+b^2-2a s-2b s+s^2)FeynCalc`B0[s,a,b])/(4(D-1)s);
r=r/.p_FeynCalc`PaVe:>canon[p];
record["MassiveB00GenericD",Cancel[Together[r-expected]]===0];
record["NativeReductionKeepsD",!FreeQ[r,D]];
record["NoLeakedKinematicSlots",FreeQ[r,sym_Symbol/;Context[sym]==="RGZWorkflow`PVSlots`"]];
pentagon=FeynCalc`PaVe[1,1,Range[10],{a,b,c,d,e},FeynCalc`PaVeAutoOrder->False];
record["NativePentagonExplicitlyRetained",RGZWorkflow`Private`qqPVNativeReduce[pentagon]===pentagon];
zero=RGZWorkflow`Private`qqPVZeroInvariant;
record["LightlikeMassiveB00",zero[FeynCalc`PaVe[0,0,{0},{0,a}]]===FeynCalc`A0[a]/D];
record["LightlikeMassiveB0000",zero[FeynCalc`PaVe[0,0,0,0,{0},{0,a}]]===a FeynCalc`A0[a]/(D(D+2))];
record["EqualMassLightlikeB00",Cancel[Together[zero[FeynCalc`PaVe[0,0,{0},{a,a}]]-FeynCalc`A0[a]/2]]===0];
masslessBubble=FeynCalc`PaVe[0,0,{0},{0,0}];
record["MasslessLightlikeIsScaleless",masslessBubble===0||zero[masslessBubble]===0];
record["UnsupportedTwoMassMomentRetained",zero[FeynCalc`PaVe[1,{0},{a,b}]]===$Failed];
record["MassiveScalarZeroMomentum",zero[FeynCalc`B0[0,0,a]]===FeynCalc`A0[a]/a];
Do[moment=FeynCalc`PaVe[Sequence@@ConstantArray[1,rr],{0},{0,a}];
 nu=Unique["nu"];xx=Unique["param"];
 betaRatio=(-1)^rr Integrate[xx^(nu+rr),{xx,0,1},Assumptions->nu>0]/Integrate[xx^nu,{xx,0,1},Assumptions->nu>0];
 paramResult=(betaRatio/.nu->D/2-2) FeynCalc`A0[a]/a;
 record["OneMassBetaMomentR"<>ToString[rr],Cancel[Together[paramResult-zero[moment]]]===0],{rr,1,2}];
record["OneMassExchangeWardMoment",Cancel[Together[zero[FeynCalc`PaVe[1,{0},{a,0}]]+zero[FeynCalc`PaVe[1,{0},{0,a}]]+FeynCalc`A0[a]/a]]===0];
record["InvalidReductionOptionRejected",FailureQ[RGZQQGaugeCheck[1,"ReduceTensorIntegrals"->3]]];
record["GaugeReductionIsOptIn",("ReduceTensorIntegrals"/.Options[RGZQQGaugeCheck])===False];
alpha=RGZWorkflow`Private`qqGaugeAlpha;
colour=First[RGZWorkflow`Private`qqFFColors[]];
spin=FeynCalc`FCI[FeynCalc`SpinorUBarD[RGZWorkflow`Engine`qqK3,0].FeynCalc`GAD[testMu].FeynCalc`SpinorUD[RGZWorkflow`Engine`qqK1,0]
 FeynCalc`SpinorUBarD[RGZWorkflow`Engine`qqK4,0].FeynCalc`GAD[testMu].FeynCalc`SpinorUD[RGZWorkflow`Engine`qqK2,0]];
tensorBubble=FeynCalc`PaVe[0,0,{s},{a,b}];
comparison=Block[{RGZWorkflow`Private`$qqGaugeNativePV=True},RGZWorkflow`Private`qqGaugeCompare[alpha colour spin (tensorBubble-expected),1,60,5,"NativePVBoundaryTest"]];
record["GaugeComparisonUsesOptionalNativeReduction",AssociationQ[comparison]&&TrueQ[comparison["AllCoefficientsZero"]]];
Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
record["ReloadRetainsOneOptionalGaugeWrapper",Count[DownValues[RGZQQGaugeCheck],HoldPattern[RGZWorkflow`Private`qqPVGaugeCheckOriginal[___]],Infinity]===1];
Print[<|"Passed"->Count[Values[checks],True],"Failed"->Count[Values[checks],False]|>];Exit[If[And@@Values[checks],0,1]];
