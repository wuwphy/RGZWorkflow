(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
project=DirectoryName[DirectoryName[$InputFileName]];
Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
RGZInitialize[FileNameJoin[{$TemporaryDirectory,"qq-pv-six-"<>CreateUUID[]}],"Display"->"Quiet"];
loaded=Quiet[RGZLoadFeynCalc[],FrontEndObject::notavail];If[FailureQ[loaded],Print[loaded];Exit[1]];
$MaxExtraPrecision=300;
checks=<||>;record[n_,v_]:=(AssociateTo[checks,n->TrueQ[v]];Print[n," : ",TrueQ[v]]);
edges=RGZWorkflow`Private`qqPVEdges[6];chords=Prepend[Array[k,5],0];
den=FeynCalc`ChangeDimension[FeynCalc`FCI[FeynCalc`FAD@@({loop+#,0}& /@chords)],D];
nativeIntegral=FeynCalc`ToPaVe[den,loop,FeynCalc`PaVeAutoOrder->False,FeynCalc`PaVeAutoReduce->False];
nativeLists=Cases[nativeIntegral,p_FeynCalc`PaVe:>RGZWorkflow`Private`qqPVParse[p][[2]],{0,Infinity}];
native=If[Length[nativeLists]===1,FeynCalc`FCI[First[nativeLists]],$Failed];
expected=FeynCalc`FCI[FeynCalc`ExpandScalarProduct[FeynCalc`SPD[chords[[#[[1]]+1]]-chords[[#[[2]]+1]]]]]& /@edges;
record["SixPointOrderMatchesNativeFeynCalc",native===expected];
record["EveryChordPairOccursOnce",Sort[edges]===Subsets[Range[0,5],{2}]];
a={0,1};b={2,3,4,5};inv=(If[MemberQ[a,#[[1]]]===MemberQ[a,#[[2]]],0,s]& /@edges);
Do[ind=Join[ConstantArray[0,2jj],ConstantArray[1,rr]];If[ind==={},ind={0}];
 master=FeynCalc`PaVe[Sequence@@ind,inv,ConstantArray[0,6],FeynCalc`PaVeAutoOrder->False,FeynCalc`PaVeAutoReduce->False];
 reduced=RGZWorkflow`Private`qqPVMasslessSingleScale[master];
 (* Independent normalized simplex beta integral divided by the bubble.
    c1=rr, alphaA=2+rr, alphaB=4, kappa=6-j-D/2. *)
 aa=2+rr;bb=4;kap=6-jj-D/2;
 reference=(-1)^(6+jj+rr)/2^jj (-s)^(jj-4) Gamma[kap]/Gamma[2-D/2] Factorial[rr]/(Gamma[aa] Gamma[bb])
  Gamma[aa-kap] Gamma[bb-kap] Gamma[D-2]/(Gamma[6+rr-2kap] Gamma[D/2-1]^2);
 numeric=N[(reduced/FeynCalc`B0[s,0,0]-reference)/.{D->13/7,s->-3},60];
 record["IndependentSixParameterBetaJ"<>ToString[jj]<>"R"<>ToString[rr],reduced=!=$Failed&&Abs[numeric]<10^-45];
 If[jj>0,gram=TimeConstrained[RGZWorkflow`Private`qqPVGramReduce[master,True],15,$TimedOut];
 record["SixPointTwinRecurrenceJ"<>ToString[jj]<>"R"<>ToString[rr],gram=!=$Failed&&gram=!=$TimedOut&&Cancel[Together[gram-reduced]]===0]],{jj,0,2},{rr,0,2}];
compactFixture=(1/(x-y)+1/(y-x)) FeynCalc`B0[s,0,0]+((x+y)^5/(x+y)^4) FeynCalc`D0[0,0,0,0,s,t,0,0,0,0];
compactResult=RGZWorkflow`Private`qqPVCompactCoefficients[compactFixture];
record["ScalarMasterCompactionIsExact",Expand[compactResult-(x+y) FeynCalc`D0[0,0,0,0,s,t,0,0,0,0]]===0];
record["SevenPointStillExplicitlyUnsupported",RGZWorkflow`Private`qqPVMasslessSingleScale[FeynCalc`PaVe[0,0,ConstantArray[0,21],ConstantArray[0,7],FeynCalc`PaVeAutoOrder->False,FeynCalc`PaVeAutoReduce->False]]===$Failed];
Print[<|"Passed"->Count[Values[checks],True],"Failed"->Count[Values[checks],False]|>];Exit[If[And@@Values[checks],0,1]];
