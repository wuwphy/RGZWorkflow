(* SPDX-License-Identifier: GPL-3.0-or-later
   Copyright (C) 2026 Wan Wu and RGZWorkflow contributors. *)
project=DirectoryName[DirectoryName[$InputFileName]];
Get[FileNameJoin[{project,"RGZWorkflow.wl"}]];
dir=FileNameJoin[{$TemporaryDirectory,"rgz-integral-cache-tests-"<>CreateUUID[]}];
RGZInitialize[dir,"Display"->"Quiet"];
Quiet[RGZLoadFeynCalc[],FrontEndObject::notavail];
RGZWorkflow`Private`loadPTEngine[];
RGZWorkflow`Private`engineGet["RGZ_Quark_Amplitude.wl",False];
checks=<||>;record[n_,v_]:=(AssociateTo[checks,n->TrueQ[v]];Print[n," : ",TrueQ[v]]);
abbrev[2]=testA+testB;abbrev[1]=3 HoldForm[abbrev[2]];
isolated=HoldForm[abbrev[1]] FeynCalc`FCI[FeynCalc`FVD[testP,testIndex]];
frozen=RGZWorkflow`Private`qqIntegralFreeze[isolated];expectedGraph=FeynCalc`FRH[isolated];
Clear[abbrev];
thawed=RGZWorkflow`Private`qqIntegralThaw[frozen,thawHead];
record["AbbreviationGraphIndependentOfOriginalDefinitions",FeynCalc`FRH[thawed]===expectedGraph];
record["FrozenGraphHasNoMutableHoldReferences",FreeQ[frozen,_HoldForm|abbrev]];
record["ThawedGraphHasNoHeldMutableReferences",FreeQ[thawed,_HoldForm]];
savedThaw=FileNameJoin[{dir,"thawed.wxf"}];Export[savedThaw,thawed,"WXF"];
Clear[thawHead];
record["SerializedThawSurvivesDefinitionCleanup",Import[savedThaw,"WXF"]===thawed];
record["HeldOutputRejectedAsCompletedAmplitude",RGZWorkflow`Private`qqFFInvalidQ[HoldForm[unknownAbbreviation[1]]]];
cycle=<|"Value"->RGZWorkflow`Private`qqIntegralAbbreviation[1],"Abbreviations"->{RGZWorkflow`Private`qqIntegralAbbreviation[1]}|>;
record["CyclicAbbreviationGraphRejected",RGZWorkflow`Private`qqIntegralThaw[cycle,Null]===$Failed];
Clear[thawHead];
kinematicHead[1]=kinematicS+kinematicT;
pvFrozen=RGZWorkflow`Private`qqIntegralFreeze[FeynCalc`B0[HoldForm[kinematicHead[1]],0,0]];
Clear[kinematicHead];pvThawed=RGZWorkflow`Private`qqIntegralThaw[pvFrozen,temporaryHead];Clear[temporaryHead];
Export[FileNameJoin[{dir,"pv-invariant.wxf"}],pvThawed,"WXF"];
record["PVInvariantSurvivesCacheScopeAndSerialization",Import[FileNameJoin[{dir,"pv-invariant.wxf"}],"WXF"]===FeynCalc`B0[kinematicS+kinematicT,0,0]];
q=RGZWorkflow`Engine`q;p=RGZWorkflow`Engine`qqK1-RGZWorkflow`Engine`qqK3;
job=<|"Directory"->dir,"Fingerprint"->"integral-cache-test"|>;
helperBefore=DownValues[FeynCalc`TID`Private`tidSingleIntegral];optsBefore=Options[FeynCalc`TID];
same[a_,b_]:=TrueQ[Cancel[Together[FeynCalc`PaVeReduce[Expand[a-b]]]]===0];
sample[mu_,nu_,mass_:2]:=FeynCalc`FCI[FeynCalc`FVD[q,mu] FeynCalc`FVD[q,nu] FeynCalc`FAD[{q,mass},{q-p,3}]];
x=sample[mu1,nu1];
Internal`InheritedBlock[{FeynCalc`TID},SetOptions[FeynCalc`TID,FeynCalc`FDS->False];
 expected=RGZWorkflow`Engine`rgzQQReduce[x];
 actual=RGZWorkflow`Private`qqWithTensorIntegralCache[job,RGZWorkflow`Engine`rgzQQReduce[x]];
 record["IndependentNativeTIDRankTwo",same[actual,expected]];
 files=FileNames["*.wxf",FileNameJoin[{dir,"tensor-integrals-v2"}]];hashes=FileHash[#,"SHA256"]&/@files;
 reused=RGZWorkflow`Private`qqWithTensorIntegralCache[job,RGZWorkflow`Engine`rgzQQReduce[x]];
 record["PersistentIntegralReuse",reused===actual&&Length[files]>0&&
  Lookup[RGZResult["QQProgress"],"TensorIntegralReused",0]>0&&(FileHash[#,"SHA256"]&/@files)===hashes];
 beforeHits=Lookup[RGZResult["QQProgress"],"TensorIntegralReused",0];
 renamed=RGZWorkflow`Private`qqWithTensorIntegralCache[job,RGZWorkflow`Engine`rgzQQReduce[sample[mu2,nu2]]];
 record["FreeIndicesRestoredAfterReuse",same[renamed,actual/.{mu1->mu2,nu1->nu2}]&&FreeQ[renamed,mu1|nu1]&&
  Lookup[RGZResult["QQProgress"],"TensorIntegralReused",0]>beforeHits];
 repeated=FeynCalc`FCI[FeynCalc`FVD[q,mu3] FeynCalc`FVD[q,nu3] FeynCalc`FAD[{q,2},{q,2},{q-p,3}]];
 expectedRepeated=RGZWorkflow`Engine`rgzQQReduce[repeated];
 actualRepeated=RGZWorkflow`Private`qqWithTensorIntegralCache[job,RGZWorkflow`Engine`rgzQQReduce[repeated]];
 record["RepeatedPoleNativeEquality",same[actualRepeated,expectedRepeated]];
 record["DNotSpecialized",!FreeQ[actual,D]];
 record["NoCacheIndicesEscape",FreeQ[actual,s_Symbol/;Context[s]==="RGZWorkflow`IntegralCache`"]];
 shifted=FeynCalc`FCI[FeynCalc`SPD[q,q] FeynCalc`FVD[q,mu5] FeynCalc`FAD[q,{q-p,3}]];
 shiftedExpected=RGZWorkflow`Engine`rgzQQReduce[shifted];
 shiftedActual=RGZWorkflow`Private`qqReduceAnchoredCore[shifted,10];
 record["CancellationReanchorsRemainingShiftedTadpole",same[shiftedActual,shiftedExpected]];
 contact=FeynCalc`FCI[FeynCalc`SPD[q,q] FeynCalc`FAD[{q,2}]];
 record["MassiveCancellationKeepsRemainderAndDropsContact",
  same[RGZWorkflow`Private`qqReduceAnchoredCore[contact,10],RGZWorkflow`Engine`rgzQQReduce[contact]]];
 contactZero=FeynCalc`FCI[FeynCalc`SPD[q,q] FeynCalc`FAD[q]];
 record["MasslessPolynomialContactIsZero",RGZWorkflow`Private`qqReduceAnchoredCore[contactZero,10]===0];
 repeatCancel=FeynCalc`FCI[FeynCalc`SPD[q,q]^2 FeynCalc`FAD[{q,2},{q,2},{q-p,3}]];
 repeatLowered=RGZWorkflow`Private`qqCancelRoutePowers[repeatCancel];
 record["RepeatedPoleCancellationReconstructsIntegrand",
  Cancel[Together[FeynCalc`ExpandScalarProduct[FeynCalc`FeynAmpDenominatorExplicit[repeatLowered-repeatCancel]]]]===0];
 (* The native unreduced rank-four zero-Gram C coefficients are a different
    redundant PV representation. Use the independent identity
    z^2/(z-4)^2 = 1 + 8/(z-4) + 16/(z-4)^2 instead. *)
 repeatReference=I Pi^2 (FeynCalc`A0[9]+8 FeynCalc`B0[RGZWorkflow`Engine`qqT,4,9]+
  16 FeynCalc`C0[0,RGZWorkflow`Engine`qqT,RGZWorkflow`Engine`qqT,4,4,9]);
 record["PostShiftReductionMatchesAnalyticRepeatedPoleIdentity",
  same[RGZWorkflow`Private`qqReduceAnchoredCore[repeatCancel,10],repeatReference]];
 (* Change the active kinematics while keeping the job label and input fixed. *)
 countBefore=Lookup[RGZResult["QQProgress"],"TensorIntegralComputed",0];
 changed=Internal`InheritedBlock[{FeynCalc`Pair,FeynCalc`SPD},
  FeynCalc`SPD[testP,testP]=17;
  z=FeynCalc`FCI[FeynCalc`FVD[q,mu4] FeynCalc`FAD[{q,2},{q-testP,3}]];
  RGZWorkflow`Private`qqWithTensorIntegralCache[job,RGZWorkflow`Engine`rgzQQReduce[z]]];
 countMiddle=Lookup[RGZResult["QQProgress"],"TensorIntegralComputed",0];
 changed2=Internal`InheritedBlock[{FeynCalc`Pair,FeynCalc`SPD},
  FeynCalc`SPD[testP,testP]=19;
  RGZWorkflow`Private`qqWithTensorIntegralCache[job,RGZWorkflow`Engine`rgzQQReduce[z]]];
 record["KinematicRulesArePartOfCacheIdentity",changed=!=changed2&&countMiddle>countBefore&&
  Lookup[RGZResult["QQProgress"],"TensorIntegralComputed",0]>countMiddle];
 (* A stale or interrupted packet is recomputed, never propagated into TID. *)
 Scan[Export[#,<|"Key"->0,"Value"->TerminatedEvaluation[IterationLimit]|>,"WXF"]&,files];
 repaired=RGZWorkflow`Private`qqWithTensorIntegralCache[job,RGZWorkflow`Engine`rgzQQReduce[x]];
 record["InterruptedIntegralPacketRecomputed",same[repaired,expected]&&FreeQ[repaired,_TerminatedEvaluation]];
];
driverInput=FeynCalc`FCI[FeynCalc`FVD[q,driverIndex] FeynCalc`FAD[{q,2},{q-p,3}]];
driverAnswer=RGZWorkflow`Private`qqReduceTensorGroups[driverInput,job,"HeldRepair",10];
driverFiles=FileNames["*.wxf",FileNameJoin[{dir,"HeldRepair-tensor"}]];
Do[packet=Import[file,"WXF"];Export[file,Join[packet,<|"Value"->HoldForm[missingTemporary[1]]|>],"WXF"],{file,driverFiles}];
driverRepaired=RGZWorkflow`Private`qqReduceTensorGroups[driverInput,job,"HeldRepair",10];
record["HeldCoreAutomaticallyRecomputed",!FailureQ[driverRepaired]&&same[driverRepaired,driverAnswer]&&FreeQ[driverRepaired,_HoldForm]];
record["InvalidCorePreservedForReview",Length[FileNames["*.invalid-*",FileNameJoin[{dir,"HeldRepair-tensor"}]]]===Length[driverFiles]&&Length[driverFiles]>0];
 record["FeynCalcDefinitionsRestored",DownValues[FeynCalc`TID`Private`tidSingleIntegral]===helperBefore];
 record["TIDOptionsRestored",Options[FeynCalc`TID]===optsBefore];
 CheckAbort[RGZWorkflow`Private`qqWithTensorIntegralCache[job,Abort[]],Null];
 record["FeynCalcDefinitionsRestoredAfterAbort",DownValues[FeynCalc`TID`Private`tidSingleIntegral]===helperBefore];
 record["InterruptedValuesRejected",!RGZWorkflow`Private`qqIntegralValueQ[TerminatedEvaluation[IterationLimit]]];
 Print[<|"Passed"->Count[Values[checks],True],"Failed"->Count[Values[checks],False]|>];
 If[!And@@Values[checks],Exit[1]];
