(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
project=DirectoryName[DirectoryName[$InputFileName]];Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
RGZInitialize[FileNameJoin[{$TemporaryDirectory,"rgz-complex-ir-"<>CreateUUID[]}],"Display"->"Quiet"];Quiet[RGZLoadFeynCalc[],FrontEndObject::notavail];RGZLoadFeynHelpers[];
checks=<||>;record[n_,v_]:=(AssociateTo[checks,n->TrueQ[v]];Print[n," : ",TrueQ[v]]);
cert[x_]:=RGZWorkflow`Private`qqIREndpointCertificate[x];e=FeynCalc`EpsilonIR;
z=2+3I;t=-7;
c=cert[FeynCalc`C0[0,0,t,z,0,z]];record["OneMasslessTriangleFinite",AssociationQ[c]&&c["IR"]===0];
c=cert[FeynCalc`D0[0,0,0,0,-5,t,0,z,0,Conjugate[z]]];record["AlternatingMassBoxFinite",AssociationQ[c]&&c["IR"]===0];
c=cert[FeynCalc`PaVe[1,{0,t,0},{0,z,0},FeynCalc`PaVeAutoReduce->False]];record["MassiveParameterMomentSuppressesCollinear",AssociationQ[c]&&c["IR"]===0&&c["IRMethod"]==="CollinearEndpointSuppressedByTensorMoment"];
c=cert[FeynCalc`PaVe[0,0,{0,t,0},{0,0,z},FeynCalc`PaVeAutoReduce->False]];record["MetricMomentSuppressesCollinear",AssociationQ[c]&&c["IR"]===0];
c=cert[FeynCalc`C0[0,0,t,0,0,z]];record["ScalarCollinearPoleNonzero",AssociationQ[c]&&c["IR"]=!=0];
expected=-(Log[z-t]-Log[z])/(t e);record["ScalarLogDifferenceExact",Expand[c["IR"]-expected]===0];
record["ConditionsPreservedForSymbolicMass",Length[cert[FeynCalc`C0[0,0,tt,0,0,mass]]["IRConditions"]]>0];
record["NoOverlappingPoleGuess",cert[FeynCalc`C0[0,0,tt,0,0,0]]===$Failed];
record["NoUnsupportedFourPointCollinearGuess",cert[FeynCalc`D0[0,0,-4,0,-3,-5,0,0,z,z]]===$Failed];
record["NonpositiveDomainRejected",cert[FeynCalc`C0[0,0,-7,0,0,-2+I]]===$Failed];
(* Check polynomial moments for both halves of the complex-mass plane.
   Numerical quadrature is an independent check, not the proof of the formula. *)
Do[A=SetPrecision[2+sign 3I,50];B=SetPrecision[7,50];
 exact=N[RGZWorkflow`Private`qqIRLinearMoment[a,b,A,B],40];
 num=NIntegrate[y^a(1-y)^b/(A+B y),{y,0,1},WorkingPrecision->40];
 record["ComplexMoment"<>ToString[{sign,a,b}],Abs[exact-num]<10^-30],{sign,{-1,1}},{a,0,2},{b,0,2}];
(* A zero linear slope must use the beta moment, not a 0/0 log formula. *)
record["ConstantSlopeLimit",RGZWorkflow`Private`qqIRLinearMoment[2,1,aa,0]===1/(12aa)];
real={FeynCalc`C0[0,0,-7,0,0,2],FeynCalc`PaVe[1,{0,-7,0},{0,0,2},FeynCalc`PaVeAutoReduce->False],FeynCalc`PaVe[2,{0,-7,0},{0,0,2},FeynCalc`PaVeAutoReduce->False],FeynCalc`PaVe[1,1,{0,-7,0},{0,0,2},FeynCalc`PaVeAutoReduce->False],FeynCalc`PaVe[1,2,{0,-7,0},{0,0,2},FeynCalc`PaVeAutoReduce->False],FeynCalc`PaVe[0,0,{0,-7,0},{0,0,2},FeynCalc`PaVeAutoReduce->False],FeynCalc`D0[0,0,0,0,-5,-7,0,2,0,3]};
Do[c=cert[real[[i]]];native=FeynCalc`PaXEvaluateIR[real[[i]],FeynCalc`FCVerbose->0];record["IndependentPackageX"<>ToString[i],AssociationQ[c]&&FullSimplify[c["IR"]-native]===0],{i,Length[real]}];
record["PublicMasterUsesCertificateForComplexMass",!MatchQ[RGZWorkflow`Private`qqUVIRMaster[FeynCalc`C0[0,0,-7,0,0,z]]["IR"],_Missing]];
bigPole=((x^2-y^2)/(x-y)-(x+y)) Log[z]+(a^2-b^2)/(a-b)/e;
compact=RGZWorkflow`Private`qqUVIRCompactPole[bigPole];
record["PoleLogCoefficientsCancelExactly",FreeQ[compact,Log]&&Cancel[compact-(a+b)/e]===0];
record["PoleCompactionPreservesNonzero",Cancel[Together[compact-bigPole]]===0];
(* Exercise the public analysis, including its conditional-domain metadata. *)
color=First[RGZWorkflow`Private`qqFFColors[]];
spin=FeynCalc`FCI[FeynCalc`SpinorUBarD[RGZWorkflow`Engine`qqK3,0].FeynCalc`GAD[testMu].FeynCalc`SpinorUD[RGZWorkflow`Engine`qqK1,0] FeynCalc`SpinorUBarD[RGZWorkflow`Engine`qqK4,0].FeynCalc`GAD[testMu].FeynCalc`SpinorUD[RGZWorkflow`Engine`qqK2,0]];
loop=color spin FeynCalc`C0[0,0,RGZWorkflow`Engine`qqT,0,0,mass];
RGZWorkflow`Private`storeResult["QQAmplitude",<|"Fingerprint"->"complex-ir-fixture","Amplitude"->loop,"OneLoop"->I loop,"Tree"->I color spin|>];
RGZQQFormFactors[60,"LoopOrder"->1,"CoefficientSeconds"->3];
r=RGZQQUVIR[60,"UsePackageX"->False,"CoefficientSeconds"->3];uv=RGZResult["QQUVIR"];
record["ConditionalIRCompleteWithoutPackageX",AssociationQ[r]&&uv["CompleteRepresentedIR"]&&uv["IRConditional"]];
record["ConditionalIRNotFullAmplitudeCertification",uv["UVIRSeparated"]===False&&Length[uv["IRValidityConditions"]]>0];
record["LogSlopeDegeneracyRecorded",MemberQ[cert[FeynCalc`C0[0,0,tt,0,0,mass]]["IRConditions"],Unequal[tt,0]|Unequal[-tt,0]]];
Print[<|"Passed"->Count[Values[checks],True],"Failed"->Count[Values[checks],False]|>];Exit[If[And@@Values[checks],0,1]];
